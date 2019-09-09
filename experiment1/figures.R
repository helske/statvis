library(modelr)
library(magrittr)
library(dplyr)
library(brms)
fit <- readRDS("fit5.rds")
combinations <- fit$data %>% 
  data_grid(viz, logit_p, p_lt0.05, p_eq0.05, cat_p, true_p) %>% 
  filter(interaction(logit_p, p_lt0.05, p_eq0.05, cat_p, true_p) %in% 
      unique(interaction(fit$data$logit_p, fit$data$p_lt0.05, fit$data$p_eq0.05, fit$data$cat_p, fit$data$true_p)))

f_mu <- fitted(fit, newdata = combinations, re_formula=NA)
f_zoi <- fitted(fit, newdata = combinations, re_formula=NA,dpar="zoi")
f_coi <- fitted(fit, newdata = combinations, re_formula=NA,dpar="coi")

f_df_mu <- data.frame(
  p = plogis(combinations$logit_p), 
  viz = combinations$viz, 
  f_mu)



x_ticks <- c(0.001, 0.01, 0.04, 0.05, 0.06, 0.1, 0.5, 0.8)
y_ticks <- c(0.05, seq(0.1, 0.9, by = 0.1), 0.95)

p1 <- f_df_mu %>% 
  ggplot(aes(x = p, y = Estimate, colour = viz)) + 
  geom_line(
    position = position_dodge(0.19)) +
  geom_linerange(
    aes(ymin = Q2.5, ymax = Q97.5), 
    position = position_dodge(0.19)) + 
  ylab("Confidence") + xlab("p-value") + 
  scale_color_discrete("Representation", 
    labels = c("p-value", "CI", "Gradient CI", "Violin CI")) + 
  scale_fill_discrete("Representation", 
    labels = c("p-value", "CI", "Gradient CI", "Violin CI")) + 
  theme_bw() + 
  scale_y_continuous(trans="logit", breaks = y_ticks, minor_breaks = NULL, labels = y_ticks) + 
  scale_x_continuous(trans="logit",
    breaks = x_ticks, labels = x_ticks, minor_breaks = NULL) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10), legend.position = "bottom",  
    axis.title.x = element_text(size = 12),
    axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12),
    legend.text=element_text(size = 10), strip.text.x = element_text(size = 10))  + 
  geom_rect(xmin=qlogis(0.04), xmax=qlogis(0.06), ymin=qlogis(0.25), ymax=qlogis(0.72), 
            color = "grey70", alpha=0, linetype="dashed")


p2 <- f_df_mu %>% filter(p > 0.02 & p < 0.09) %>%
  ggplot(aes(x = p, y = Estimate, colour = viz)) + 
  geom_line(position = position_dodge(0.025)) +
  geom_linerange(
    aes(ymin = Q2.5, ymax = Q97.5), 
    position = position_dodge(0.025)) + 
  ylab("Confidence") + xlab("p-value") + 
  theme_bw() + 
  scale_y_continuous(trans="logit", breaks = y_ticks2,# position = "right",
                     minor_breaks = NULL, labels = y_ticks2) + 
  scale_x_continuous(trans="logit",#position = "top",
                     breaks = x_ticks2, labels = x_ticks2, 
                     minor_breaks = NULL) + 
  theme(axis.text.x = element_text(size = 10), legend.position = "none",  
        axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12),
        strip.text.x = element_text(size = 10),
        plot.background = element_blank()) 

p1 + annotation_custom(
  ggplotGrob(p2), 
  xmin = qlogis(0.15), xmax = qlogis(0.85), ymin = qlogis(0.2), ymax = qlogis(0.95))

ggsave(filename = "curves1.pdf", width = 6, height = 4)

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
    labels = c("p-value", "CI", "Gradient CI", "Violin CI")) + 
  scale_colour_discrete("Representation", 
    labels = c("p-value", "CI", "Gradient CI", "Violin CI")) + 
  theme_bw() + 
  scale_y_continuous(trans = "logit",
                     breaks = y_ticks, labels = y_ticks, minor_breaks = NULL) + 
  scale_x_continuous(trans = "logit",
    breaks = x_ticks, labels = x_ticks, minor_breaks = NULL) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10), legend.position = "bottom",   
    axis.title.x = element_text(size = 12),
    axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12),
    legend.text=element_text(size = 10), strip.text.x = element_text(size = 10)) 
ggsave(file = "011.pdf", width = 6, height = 4)


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