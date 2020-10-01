library(modelr)
library(magrittr)
library(dplyr)
library(brms)
library(ggthemes)
library(ggplot2)

cols <- c("p-value" = "#D55E00", "CI" = "#0072B2", 
  "Gradient CI" = "#009E73", "Violin CI" = "#CC79A7",
  "Discrete Violin CI" = "#E69F00")
fit_exp1 <- readRDS("experiment1/results/fit9.rds")
fit_exp1

combinations_exp1 <- fit_exp1$data %>% 
  data_grid(viz, logit_p, p_lt0.05, p_eq0.05, cat_p, true_p) %>% 
  filter(interaction(logit_p, p_lt0.05, p_eq0.05, cat_p, true_p) %in% 
           unique(interaction(fit_exp1$data$logit_p, fit_exp1$data$p_lt0.05, 
                              fit_exp1$data$p_eq0.05, fit_exp1$data$cat_p, 
                              fit_exp1$data$true_p)))

f_mu_exp1 <- fitted(fit_exp1, 
  newdata = combinations_exp1, re_formula = NA)
f_zoi_exp1 <- fitted(fit_exp1, 
  newdata = combinations_exp1, re_formula = NA, dpar = "zoi")

f_df_mu_exp1 <- data.frame(
  p = plogis(combinations_exp1$logit_p), 
  viz = combinations_exp1$viz, 
  f_mu_exp1)





x_ticks <- c(0.001, 0.01, 0.04, 0.06, 0.1, 0.5, 0.8)
y_ticks <- c(0.05, seq(0.1, 0.9, by = 0.1), 0.95)

levels(f_df_mu_exp1$viz) <- c("p-value", "CI", "Gradient CI", "Violin CI")


dodge <- 0.19
p1 <- f_df_mu_exp1 %>% 
  ggplot(aes(x = p, y = Estimate, colour = viz)) + 
  geom_line(position = position_dodge(dodge), size = 0.1) +
  geom_linerange(data = f_df_mu_exp1 %>% filter(p < 0.03 | p > 0.07),
    aes(ymin = Q2.5, ymax = Q97.5), 
    position = position_dodge(dodge), size = 0.3,
    show.legend = FALSE) + 
  geom_point(position = position_dodge(dodge), size = 0.7, show.legend = FALSE) +
  ylab("Confidence") + xlab("p-value") + 
  scale_color_manual("Representation", 
    values = cols[1:4],
    labels = c("p-value", "CI", "Gradient CI", "Violin CI")) + 
  scale_y_continuous(trans="logit", breaks = y_ticks, minor_breaks = NULL, labels = y_ticks) + 
  scale_x_continuous(trans="logit",
    breaks = x_ticks, labels = x_ticks, minor_breaks = NULL) + 
  theme_classic() + 
  theme(legend.position = "bottom", 
    legend.margin = margin(t = -0.1, b = 0, unit = "cm"),
    legend.title = element_blank(),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1, vjust = 1), 
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14, 
      margin = margin(t = -0.1, r = 0, b = -0.3, l = 0, unit = "cm")),
    axis.title.y = element_text(size = 14, 
      margin = margin(t = 0, r = -0.1, b = 0, l = -0.1, unit = "cm")),
    legend.text = element_text(size = 14))  + 
  geom_rect(xmin = qlogis(0.03), xmax = qlogis(0.07), ymin = qlogis(0.31), ymax = qlogis(0.82), 
    color = "grey70", alpha = 0, linetype = "dashed", size = 0.1) + 
  guides(colour = guide_legend(override.aes = list(size = 1.5)))



p2 <- f_df_mu_exp1 %>% filter(p > 0.02 & p < 0.09) %>%
  ggplot(aes(x = p, y = Estimate, colour = viz)) + 
  geom_line(position = position_dodge(0.1), size = 0.1) +
  geom_linerange(
    aes(ymin = Q2.5, ymax = Q97.5), 
    position = position_dodge(0.1), size = 0.3,
    show.legend = FALSE) + 
  geom_point(position = position_dodge(0.1), size = 0.7) +
  ylab("Confidence") + xlab("p-value") + 
  scale_color_manual("Representation", 
    values =  cols[1:4],
    labels = c("p-value", "CI", "Gradient CI", "Violin CI")) + 
  scale_y_continuous(trans="logit", breaks = y_ticks,
    minor_breaks = NULL, labels = y_ticks) + 
  scale_x_continuous(trans="logit",
    breaks = c(0.04, 0.05, 0.06), 
    labels = c(0.04, 0.05, 0.06), 
    minor_breaks = NULL) + 
  theme_classic() + 
  theme(legend.position = "none",  
    axis.title.x = element_blank(), axis.title.y = element_blank(),
    plot.background = element_blank(),
    plot.margin=unit(c(-4,-9,0,0), "mm"),
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12)) 

p <- p1 + coord_cartesian(xlim = c(0.001, 0.9), ylim = c(0.045, 0.95)) + 
  annotation_custom(
    ggplotGrob(p2), 
    xmin = qlogis(0.2), xmax = qlogis(0.9), ymin = qlogis(0.3), ymax = qlogis(0.95))


ggsave(p, filename = "experiment1/results/exp1_confidence.pdf", 
  width = 2*8.5, height = 12.5, 
  unit = "cm", device = "pdf")


df_01_exp1 <- data.frame(
  p = plogis(combinations_exp1$logit_p), 
  viz = combinations_exp1$viz, 
  f_zoi_exp1)

y_ticks <- c(0, 0.01, seq(0.1,0.9,by=0.2))

p <- df_01_exp1 %>% 
  ggplot(aes(x = p, y = Estimate, colour = viz)) +
  geom_line(position = position_dodge(0.19), size = 0.1) +
  geom_linerange(
    aes(ymin = Q2.5, ymax = Q97.5), 
    position = position_dodge(0.19), size = 0.1,
    show.legend = FALSE) + 
  geom_point(position = position_dodge(0.19), size = 0.5) +
  ylab("Probability of all-or-none answer") + xlab("p-value") + 
  scale_fill_discrete("Representation", 
    labels = c("p-value", "CI", "Gradient CI", "Violin CI")) + 
  scale_colour_discrete("Representation", 
    labels = c("p-value", "CI", "Gradient CI", "Violin CI")) + 
  theme_bw() + 
  # scale_y_continuous(#trans = "logit",
  #                     breaks = y_ticks, labels = y_ticks, minor_breaks = NULL) + 
  scale_x_continuous(trans = "logit",
    breaks = x_ticks, labels = x_ticks, minor_breaks = NULL) + 
  theme_classic() + 
  theme(legend.position = "bottom", 
    legend.margin = margin(t = -0.1, b = 0, unit = "cm"),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10), 
    axis.title.x = element_text(size = 12, margin = margin(t = -0.1, r = 0, b = -0.3, l = 0, unit = "cm")),
    axis.title.y = element_text(size = 12, margin = margin(t = 0, r = 0, b = 0, l = -0.1, unit = "cm")),
    axis.text.y = element_text(size = 10),
    legend.text = element_text(size = 12), strip.text.x = element_text(size = 10))


ggsave(p, filename = "experiment1/results/exp1_extreme.pdf", 
  width = 2*8.5, height = 12, 
  unit = "cm", device = "pdf")


dc <- combinations_exp1 %>%
  filter(true_p == "0.04" | true_p == "0.06")
f_df_mu_exp1 <- fitted(fit_exp1, newdata = dc, re_formula = NA, summary = FALSE)

d <- data.frame(value = c(f_df_mu_exp1), 
  p = rep(dc$true_p, each = nrow(f_df_mu_exp1)),
  viz = rep(dc$viz, each = nrow(f_df_mu_exp1)),
  iter = 1:nrow(f_df_mu_exp1))

levels(d$viz) <- c("p-value", "CI", "Gradient CI", "Violin CI")


d %>% group_by(viz, iter) %>% 
  summarise(difference = value[p == "0.04"] - value[p == "0.06"]) %>%
  summarise(mean = mean(difference), sd = sd(difference),
    "2.5%" = quantile(difference, 0.025), 
    "97.5" = quantile(difference, 0.975))



p <- d %>% group_by(viz, iter) %>% 
  summarise(difference = value[p == "0.04"] - value[p == "0.06"]) %>% 
  ggplot(aes(x = difference, fill = viz, colour = viz)) + 
  geom_density(bw = 0.01, alpha = 0.6) +
  theme_classic() + 
  scale_fill_manual("Representation", 
    values = cols[1:4]) + 
  scale_colour_manual("Representation", 
    values = cols[1:4]) + 
  ylab("Posterior density") + 
  xlab("Cliff effect") +
  theme(legend.position = "bottom",  
    #plot.margin=unit(c(-4,-9,0,0), "mm"),
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12)) 

ggsave(p, filename = "experiment1/results/exp1_cliff.pdf", 
  width = 2*8.5, height = 6, 
  unit = "cm", device = "pdf")

postprob <- d %>% group_by(viz, iter) %>% 
  summarise(difference = value[p == "0.04"] - value[p == "0.06"]) %>%
  group_by(iter) %>% 
  mutate(p_vs_ci = difference[viz == "p-value"] - difference[viz == "CI"],
    p_vs_gradient = difference[viz == "p-value"] - difference[viz == "Gradient CI"],
    p_vs_violin = difference[viz == "p-value"] - difference[viz == "Violin CI"],
    ci_vs_gradient = difference[viz == "CI"] - difference[viz == "Gradient CI"],
    ci_vs_violin = difference[viz == "CI"] - difference[viz == "Violin CI"],
    gradient_vs_violin = difference[viz == "Gradient CI"] - difference[viz == "Violin CI"]) %>%
  ungroup() %>% summarise(
    "P(p > CI)" = mean(p_vs_ci > 0),
    "P(p > gradient)" = mean(p_vs_gradient > 0),
    "P(p > violin)" = mean(p_vs_violin > 0),
    "P(CI > gradient)" = mean(ci_vs_gradient > 0),
    "P(CI > violin)" = mean(ci_vs_violin > 0),
    "P(gradient > violin)" = mean(gradient_vs_violin > 0),
    "P(p > CI)" = mean(p_vs_ci > 0))
round(t(as.data.frame(postprob)), 2)
