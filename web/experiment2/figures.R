library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(purrr)
library(scales)
path <- "web/experiment2/figures"

# 8 trials twice, with n=100
reps <- 8
# target p-values of two-sided t-test (H0: mu_1 != mu_2)
pvals <- c(0.001, 0.01, 0.04, 0.05, 0.06, 0.1, 0.5, 0.8)
n <- 50
sigma_x <- 1
sigma_y <- 1
se <- sqrt(sigma_x^2+sigma_y^2)/sqrt(n)
nu <- (n-1)*(sigma_x^2+sigma_y^2)^2/(sigma_x^4+sigma_y^4)

control_means <- rep(1, reps) #rnorm(reps, 1, sd = 0.1)
treatment_means <-  control_means + se * qt(1 - pvals/2, df = nu)

# test:
# res <- replicate(1e4, {
#   x <- rnorm(n, m1, sigma_x)
#   y <- rnorm(n, 0, sigma_y)
#   sigma_x <- sd(x)
#   sigma_y <- sd(x)
#   se <- sqrt(sigma_x^2+sigma_y^2)/sqrt(n)
#   nu <- (n-1)*(sigma_x^2+sigma_y^2)^2/(sigma_x^4+sigma_y^4)
#   y <- y - mean(y) + mean(x) + se * qt(1 - 0.025, df = nu)
#   t.test(y, x)$p.value
# })
# summary(res)

ci_data <- data.frame(
  trial = rep(factor(1:reps), 2),
  group = rep(c("control", "treatment"), each = reps),
  mean = c(control_means, treatment_means), 
  sigma = rep(c(sigma_x, sigma_y), each = reps),
  p = pvals, n = n, df = n - 1) %>% 
  mutate(
    se = sigma / sqrt(n),
    lwr = mean - qt(1-(1-0.95)/2, df) * se,
    upr = mean + qt(1-(1-0.95)/2, df) * se)

p <- ggplot(data=ci_data, aes(x=factor(p), y=mean, colour=group)) + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.25, 
                position = position_dodge(width=0.5)) + 
  geom_point(position = position_dodge(width=0.5)) + xlab("P-value") + 
  ylab("Weight decrease (kg)") + labs(colour = "Group") +
  theme_bw() + theme(legend.position="bottom")
p
size <- 5
p <- ggsave("experiment2/results/configuration2.pdf" ,width = 6, height = 3)


# Create confidence interval plots

ci_data %>% 
  group_by(trial, n) %>% 
  nest() %>% 
  mutate(plot = map2(data, trial, ~ 
                       ggplot(data=.x, aes(x=group, y=mean)) + 
                       geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.25) + 
                       geom_point() + xlab(NULL) + ylab("Weight decrease (kg)") +
                       geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30") + 
                       theme_bw() + theme(panel.grid.major.x = element_blank())),
         filename = paste0(trial, "_", n, ".png")) %>% 
  select(filename, plot) -> plots

pwalk(plots, ggsave, path = paste0(path, "/ci"), height = size, width = size)


## Create modified t-violin plots
source("code/statStudent.R")
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
                       ggplot(data = .x, aes(x = group, y = mean)) + 
                       geom_student(aes(mean = mean, se = se, df = df, 
                                        level = level, fill = level), show.legend = FALSE) +
                       xlab(NULL) + ylab("Weight decrease (kg)") +
                       scale_fill_manual(labels = paste0(100 * ci_levels, "%"), 
                                         values=g(seq(0,1,length = k))) + 
                       geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30") + 
                       theme_bw() + theme(panel.grid.major.x = element_blank())),
         filename = paste0(trial, "_", n, ".png")) %>% 
  select(filename, plot) -> plots
pwalk(plots, ggsave, path = paste0(path, "/violin"), height = size, width = size)


## Create gradient plots
summary_data %>%
  mutate(plot = map2(data, trial, ~ 
                       ggplot(data = .x, aes(x = group)) + 
                       geom_student(aes(mean = mean, se = se, df = df, 
                                        level = level, fill = level), type = "box", show.legend = FALSE) +
                       xlab(NULL) + ylab("Weight decrease (kg)") +
                       scale_fill_manual(labels = paste0(100 * ci_levels, "%"), 
                                         values=g(seq(0,1,length = k))) + 
                       geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30") + 
                       theme_bw() + theme(panel.grid.major.x = element_blank())),
         filename = paste0(trial, "_", n, ".png")) %>% 
  select(filename, plot) -> plots

pwalk(plots, ggsave, path = paste0(path, "/gradient"), height = size, width = size)

## violin2
ci_levels <- c(0.999, 0.95, 0.9, 0.85, 0.8)
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
                       ggplot(data = .x, aes(x = group)) + 
                       geom_student(aes(mean = mean, se = se, df = df, 
                                        level = level, fill = level)) +
                       xlab(NULL) + ylab("Weight decrease (kg)") +
                       scale_fill_manual("CI level", labels = paste0(100 * ci_levels, "%"), 
                                         values=g(seq(0,1,length = k))) + 
                       geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30") + 
                       theme_bw() + theme(panel.grid.major.x = element_blank())),
         filename = paste0(trial, "_", n, ".png")) %>% 
  select(filename, plot) -> plots

pwalk(plots, ggsave, path = paste0(path, "/violin2"), height = size, width = size)


## Example figures

n <- 100
set.seed(1)
x <- rnorm(n)
y <- rnorm(n)
f <- t.test(x, y)
example_data <- data.frame(
  group = c("treatment", "control"), 
  mean = f$estimate, 
  n = n,
  sigma = c(sd(x), sd(y)),  
  df = n - 1) %>% 
  mutate(
    se = sigma / sqrt(n),
    lwr = mean - qt(1-(1-0.95)/2, df) * se,
    upr = mean + qt(1-(1-0.95)/2, df) * se)

p1 <- ggplot(data=example_data, aes(x=group, y=mean)) + 
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.25) + 
  geom_point() +
  xlab(NULL) + ylab("Weight decrease (kg)") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30") + 
  theme_bw() + 
  theme(panel.grid.major.x = element_blank())
ggsave(p1, filename = paste0(path, "/examples/ci.png"), height = size, width = size)


ci_levels <- c(seq(0.999, 0.95,by=-0.001))
k <- length(ci_levels)
ci_levels_f <- factor(ci_levels, levels = ci_levels)

example_data1 <- data.frame(example_data, level = rep(ci_levels_f, each = 2))
p3 <- ggplot(data = example_data1, aes(x = group, y = mean)) + 
  geom_student(aes(mean = mean, se = se, df = df, 
                   level = level, fill = level), show.legend = FALSE) + 
  scale_fill_manual(labels = paste0(100 * ci_levels, "%"), 
                    values=g(seq(0,1,length = k))) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30") + 
  xlab(NULL) + ylab("Weight decrease (kg)") +
  theme_bw() + 
  theme(panel.grid.major.x = element_blank())

ggsave(p3, filename = paste0(path, "/examples/violin.png"), height = size, width = size)

p4 <- ggplot(data = example_data1, aes(x = group, y = mean)) + 
  geom_student(aes(mean = mean, se = se, df = df, 
                   level = level, fill = level), show.legend = FALSE, type = "box") + 
  scale_fill_manual(labels = paste0(100 * ci_levels, "%"), 
                    values=g(seq(0,1,length = k))) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30") + 
  xlab(NULL) + ylab("Weight decrease (kg)") +
  theme_bw() + 
  theme(panel.grid.major.x = element_blank())
ggsave(p4, filename = paste0(path, "/examples/gradient.png"), height = size, width = size)


ci_levels <- c(0.999,0.95,0.9,0.85,0.8)
k <- length(ci_levels)
ci_levels_f <- factor(ci_levels, levels = ci_levels)

example_data2 <- data.frame(example_data, level = rep(ci_levels_f, each = 2))

p2 <- ggplot(data = example_data2, aes(x = group, y = mean)) + 
  geom_student(aes(mean = mean, se = se, df = df, 
                   level = level, fill = level), draw_mean=TRUE) + 
  scale_fill_manual(labels = paste0(100 * ci_levels, "%"), 
                    values=g(seq(0,1,length = k))) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30") + 
  xlab(NULL) + ylab("Weight decrease (kg)") +
  theme_bw() + 
  theme(panel.grid.major.x = element_blank())

ggsave(p2, filename = paste0(path, "/examples/violin2.png"), height = size, width = size)

