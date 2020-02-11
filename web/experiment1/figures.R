library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(purrr)
library(scales)
path <- "web/experiment1/figures"

# 8 trials twice, with n=50, 200
reps <- 8
# target p-values of two-sided t-test (H0: mu = 0)
p <- c(0.001, 0.01, 0.04, 0.05, 0.06, 0.1, 0.5, 0.8)
sigma <- 3
n <- rep(c(50, 200), each = reps)
means <-  sigma / sqrt(n) * qt(1 - p / 2, df = n - 1)
2*(1-pt(means/(sigma/sqrt(n)), df = n - 1))

ci_data <- data.frame(trial = factor(1:reps), 
                      effect = means, 
                      lwr = means - qt(0.975, n - 1) * sigma / sqrt(n), 
                      upr = means + qt(0.975, n - 1) * sigma / sqrt(n),
                      p = p, df = n - 1, n = n,
                      se = sigma / sqrt(n))

p <- ggplot(data=ci_data, aes(x=factor(p), y=effect, colour=factor(n))) + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.25, 
                position = position_dodge(width=0.5)) + 
  geom_point(position = position_dodge(width=0.5), size = 0.7, show.legend = FALSE) + 
  xlab("p-value") + 
  ylab("Weight increase (kg)") + labs(colour = "Sample size") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30") + 
  scale_color_brewer(type = "qual") +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12), 
        legend.position = "bottom", 
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.text=element_text(size = 14), legend.title = element_text(size = 14))

ggsave("configuration.pdf" ,width = 6, height = 3, device = "pdf")

size <- 5
# Create confidence interval plots

ci_data %>% 
  group_by(trial, n) %>% 
  nest() %>% 
  mutate(plot = map2(data, trial, ~ ggplot(data=.x, aes(x=.y, y=effect)) + 
                       geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.25) + 
                       geom_point() + xlab(NULL) + ylab("Weight increase (kg)") +
                       geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30") + 
                       theme_bw() +
                       theme(axis.text.x = element_blank())),
         filename = paste0(trial, "_", n, ".png")) %>% 
  select(filename, plot) -> plots

pwalk(plots, ggsave, path = paste0(path, "/ci"), height = size, width = size)

labelfun <- Vectorize(function(effect, lwr, upr, p) {
  paste0("Mean weight increase ", round(effect, 3), 
         "kg, \n 95% CI: [", round(lwr, 3),"kg, ", round(upr, 3),
         "kg],\np = ", round(p, 3), " (2-sided t-test)")
})
# Create p-value "figures"

ci_data %>% 
  mutate(label = labelfun(effect, lwr, upr, p),
         ypos = 0) %>%
  group_by(trial, n) %>% 
  nest() %>% 
  mutate(
    plot = map2(data, trial, ~ ggplot(data=.x, aes(x=.y, y=ypos, label=label)) + 
                  xlab(NULL) + ylab(NULL) +
                  geom_text() +
                  theme_bw() + 
                  theme(axis.text.x = element_blank(),
                        axis.text.y = element_blank(),
                        panel.grid = element_blank())),
    filename = paste0(trial, "_", n, ".png")) %>% 
  select(filename, plot) -> plots
pwalk(plots, ggsave, path = paste0(path, "/pvalues"), height = size, width = size)


## Create modified t-violin plots
source("code/statstudent.R")
ci_levels <- c(seq(0.999, 0.95,by=-0.001))
k <- length(ci_levels)
ci_levels_f <- factor(ci_levels, levels = ci_levels)

# color scale
g <- scales::seq_gradient_pal("#e5f5f9", "#2ca25f")
summary_data <- ci_data %>%
  group_by(trial, n) %>%
  nest() %>% 
  mutate(data = map2(data, trial, ~ full_join(cbind(trial = .y, .x), 
                                              data.frame(
                                                trial = rep(.y, k), 
                                                level = ci_levels_f), by = "trial")))

summary_data %>%
  mutate(plot = map2(data, trial, ~ 
                       ggplot(data = .x, aes(x = .y)) + 
                       geom_student(aes(mean = effect, se = se, df = df, 
                                        level = level, fill = level), show.legend = FALSE) +
                       xlab(NULL) + ylab("Weight increase (kg)") +
                       scale_fill_manual(labels = paste0(100 * ci_levels, "%"), 
                                         values=g(seq(0,1,length = k))) + 
                       geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30") + 
                       theme_bw() +
                       theme(axis.text.x = element_blank())),
         filename = paste0(trial, "_", n, ".png")) %>% 
  select(filename, plot) -> plots
pwalk(plots, ggsave, path = paste0(path, "/violin"), height = size, width = size)

## Create gradient plots

summary_data %>%
  mutate(plot = map2(data, trial, ~ 
                       ggplot(data = .x, aes(x = .y)) + 
                       geom_student(aes(mean = effect, se = se, df = df, 
                                        level = level, fill = level), show.legend = FALSE, type = "box") +
                       xlab(NULL) + ylab("Weight increase (kg)") +
                       scale_fill_manual(labels = paste0(100 * ci_levels, "%"), 
                                         values=g(seq(0,1,length = k))) +
                       geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30") + 
                       theme_bw() +
                       theme(axis.text.x = element_blank())),
         filename = paste0(trial, "_", n, ".png")) %>% 
  select(filename, plot) -> plots
pwalk(plots, ggsave, path = paste0(path, "/gradient"), height = size, width = size)

## Example figures

set.seed(1)
x <- rnorm(100)
f <- t.test(x)
example_data <- data.frame(trial = factor(1), mean = f$estimate, 
                           p = f$p.value, lwr = f$conf.int[1], upr = f$conf.int[2], 
                           se = sd(x)/sqrt(100), df = 99)

p1 <- ggplot(data=example_data, aes(x=trial, y=mean)) + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.25) + 
  geom_point() + xlab(NULL) + ylab("Weight increase (kg)") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30") + 
  theme_bw() +
  theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank())
ggsave(p1, filename = paste0(path, "/examples/ci.png"), height = size, width = size)

example_data %>% 
  mutate(label = labelfun(mean, lwr, upr, p),
         ypos = 0) -> example_data

p2 <- ggplot(data=example_data, aes(x=trial, y=ypos, label=label)) + 
  xlab(NULL) + ylab(NULL) +
  geom_text() +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank())

ggsave(p2, filename = paste0(path, "/examples/pvalue.png"), height = size, width = size)

example_data <- data.frame(example_data, level = ci_levels_f)
p3 <- ggplot(data = example_data, aes(x = trial)) + 
  geom_student(aes(mean = mean, se = se, df = df, 
                   level = level, fill = level), show.legend = FALSE) + 
  scale_fill_manual(labels = paste0(100 * ci_levels, "%"), 
                    values=g(seq(0,1,length = k))) +
  xlab(NULL) + ylab("Weight increase (kg)") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30") + 
  theme_bw() + 
  theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank())

ggsave(p3, filename = paste0(path, "/examples/violin.png"), height = size, width = size)

p4 <- ggplot(data = example_data, aes(x = trial)) + 
  geom_student(aes(mean = mean, se = se, df = df, 
                   level = level, fill = level), show.legend = FALSE, type = "box") + 
  scale_fill_manual(labels = paste0(100 * ci_levels, "%"), 
                    values=g(seq(0,1,length = k))) +
  xlab(NULL) + ylab("Weight increase (kg)") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30") + 
  theme_bw() + 
  theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank())

ggsave(p4, filename = paste0(path, "/examples/gradient.png"), height = size, width = size)

