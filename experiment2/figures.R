library(ggplot2)
library(dplyr)
library(brms)
library(modelr)
library(ggthemes)

fit_exp2 <- readRDS("experiment2/results/fit1.rds")
combinations <- fit_exp2$data %>% 
  data_grid(viz, logit_p, p_lt0.05, p_eq0.05, cat_p, true_p) %>% 
  filter(interaction(logit_p, p_lt0.05, p_eq0.05, cat_p, true_p) %in% 
      unique(interaction(fit_exp2$data$logit_p, fit_exp2$data$p_lt0.05, 
                         fit_exp2$data$p_eq0.05, fit_exp2$data$cat_p, 
                         fit_exp2$data$true_p)))

f_mu <- fitted(fit_exp2, newdata = combinations, re_formula = NA)
f_zoi <- fitted(fit_exp2, newdata = combinations, re_formula = NA, dpar = "zoi")


dc <- combinations %>%
  filter(true_p == "0.04" | true_p == "0.06")
f_mu_exp2 <- fitted(fit_exp2, newdata = dc, re_formula = NA, summary = FALSE)

d <- data.frame(value = c(f_mu_exp2), 
                p = rep(dc$true_p, each = nrow(f_mu_exp2)),
                viz = rep(dc$viz, each = nrow(f_mu_exp2)),
                iter = 1:nrow(f_mu_exp2))

d %>% group_by(viz, iter) %>% 
  summarise(difference = value[p == "0.04"] - value[p == "0.06"]) %>%
  summarise(mean = mean(difference), sd = sd(difference),
            "2.5%" = quantile(difference, 0.025), 
            "97.5" = quantile(difference, 0.975))


f_df_mu_exp2 <- data.frame(
  p = plogis(combinations$logit_p), 
  viz = combinations$viz, 
  f_mu)


x_ticks <- c(0.001, 0.01, 0.04, 0.06, 0.1, 0.5, 0.8)
y_ticks <- c(0.05, seq(0.1, 0.9, by = 0.1), 0.95)

cols <- scales::brewer_pal(palette = "Set1")(5)[-1]

dodge <- 0.19
p1 <- f_df_mu_exp2 %>% 
  ggplot(aes(x = p, y = Estimate, colour = viz)) + 
  geom_line(position = position_dodge(dodge), size = 0.1) +
  geom_linerange(data = f_df_mu_exp2 %>% filter(p < 0.03 | p > 0.07),
    aes(ymin = Q2.5, ymax = Q97.5), 
    position = position_dodge(dodge), size = 0.1,
    show.legend = FALSE) + 
  geom_point(position = position_dodge(dodge), size = 0.7) +
  ylab("Confidence") + xlab("p-value") + 
  scale_color_manual("Representation", 
                       values = cols,
                       labels = c("CI", "Gradient CI", "Cont. Violin CI", "Disc. Violin CI")) + 
  scale_y_continuous(trans="logit", breaks = y_ticks, minor_breaks = NULL, labels = y_ticks) + 
  scale_x_continuous(trans="logit",
                     breaks = x_ticks, labels = x_ticks, minor_breaks = NULL) + 
  theme_classic() + 
  theme(legend.position = "bottom", 
        legend.margin = margin(t = -0.1, b = 0, unit = "cm"),
        legend.title = element_blank(),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1, vjust = 1), 
        axis.title.x = element_text(size = 14, margin = margin(t = -0.1, r = 0, b = -0.3, l = 0, unit = "cm")),
        axis.title.y = element_text(size = 14, margin = margin(t = 0, r = -0.1, b = 0, l = -0.1, unit = "cm")),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 14), strip.text.x = element_text(size = 10))  + 
  geom_rect(xmin = qlogis(0.03), xmax = qlogis(0.07), ymin = qlogis(0.2), ymax = qlogis(0.7), 
            color = "grey70", alpha = 0, linetype = "dashed", size = 0.1) + 
  guides(colour = guide_legend(override.aes = list(size = 1.5)))



p2 <- f_df_mu_exp2 %>% filter(p > 0.02 & p < 0.09) %>%
  ggplot(aes(x = p, y = Estimate, colour = viz)) + 
  geom_line(position = position_dodge(0.1), size = 0.1) +
  geom_linerange(
    aes(ymin = Q2.5, ymax = Q97.5), 
    position = position_dodge(0.1), size = 0.1,
    show.legend = FALSE) + 
  geom_point(position = position_dodge(0.1), size = 0.7) +
  ylab("Confidence") + xlab("p-value") + 
  scale_y_continuous(trans="logit", breaks = y_ticks,
                     minor_breaks = NULL, labels = y_ticks) + 
  scale_x_continuous(trans="logit",
                     breaks = c(0.04, 0.05, 0.06), 
                     labels = c(0.04, 0.05, 0.06), 
                     minor_breaks = NULL) + 
  scale_color_manual("Representation", 
                     values = cols,
                     labels = c("CI", "Gradient CI", "Cont. Violin CI", "Disc. Violin CI")) + 
  theme_classic() + 
  theme(legend.position = "none",  
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.background = element_blank(),
        plot.margin=unit(c(-4,-9,0,0), "mm"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12))

p <- p1 + coord_cartesian(xlim = c(0.001, 0.9), ylim = c(0.035, 0.95)) + 
  annotation_custom(
    ggplotGrob(p2), 
    xmin = qlogis(0.2), xmax = qlogis(0.9), ymin = qlogis(0.3), ymax = qlogis(0.95))


ggsave(p, filename = "exp2_confidence.pdf", 
       width = 2*8.5, height = 12, 
       unit = "cm", device = "pdf")
p
dev.off()


df_01 <- data.frame(
  p = plogis(combinations$logit_p), 
  viz = combinations$viz, 
  f_zoi)


y_ticks <- c(0.001, 0.01, seq(0.1,0.9,by=0.2))
df_01 %>% 
  ggplot(aes(x = p, y = Estimate, colour = viz)) +
  geom_linerange(aes(ymin = Q2.5, ymax = Q97.5),
    position = position_dodge(width=0.15)) + 
  geom_line(alpha=0.5, position = position_dodge(width=0.15))  + 
  ylab("Probability of all-or-none answer") + xlab("p-value") + 
  scale_fill_discrete("Representation", 
    labels = c("CI", "Gradient CI", "Cont. Violin CI", "Disc. Violin CI")) + 
  scale_colour_discrete("Representation", 
    labels = c("CI", "Gradient CI", "Cont. Violin CI", "Disc. Violin CI")) + 
  theme_bw() + 
  scale_y_continuous(trans = "logit",  breaks = y_ticks, labels = y_ticks, minor_breaks = NULL) + 
  scale_x_continuous(trans = "logit",
    breaks = x_ticks, labels = x_ticks, minor_breaks = NULL) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10), legend.position = "bottom",   
    axis.title.x = element_text(size = 12),
    axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12),
    legend.text=element_text(size = 10), strip.text.x = element_text(size = 10)) 


ggsave(filename = "012.pdf", width = 6, height = 4)

###
pp_check(fit, nsamples = 100)
pp_check(fit, type = "hist", nsamples = 11)
pp_check(fit, type = "stat_grouped", group = "true_p")
pp_check(fit, type = "stat_grouped", group = "viz")
bayes_R2(fit)
(res <- loo(fit, save_psis = TRUE))
plot(res)
yrep <- posterior_predict(fit)
library(bayesplot)
ppc_loo_pit_overlay(
  y = fit$data$confidence,
  yrep = yrep,
  lw = weights(res$psis_object)
)

ppc_intervals(y=fit$data$confidence, yrep = yrep, x = plogis(fit$data$logit_p))
ppc_loo_intervals(y=fit$data$confidence, yrep = yrep, x = plogis(fit$data$logit_p))