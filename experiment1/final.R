library(brms)
library(rstan)
library(modelr)
library(ggplot2)
library(dplyr)
library(magrittr)
rstan_options(auto_write = TRUE)
Sys.setenv(LOCAL_CPPFLAGS = '-march=native')

data <- readRDS("data/exp1_data.rds")

data %>% group_by(id, viz) %>% 
  summarize(diff = confidence[true_p==0.06] - confidence[true_p==0.04])  %>% 
  group_by(viz) %>%
  summarise(
    mean = mean(diff), 
    sd = sd(diff), 
    se = sd(diff) / sqrt(length(diff)),
    lwr = quantile(diff, 0.025), 
    upr = quantile(diff, 0.975))

data %>% group_by(id, viz, expertise) %>% 
  mutate(extreme = confidence %in% c(0, 1))  %>% 
  group_by(viz, expertise) %>%
  summarise(
    mean = mean(extreme),
    sd = sd(extreme), 
    se = sd(extreme) / sqrt(length(extreme)),
    Q2.5 = mean - 2*se,
    Q97.5 = mean + 2*se) 

data %>% group_by(id, viz, n) %>% 
  summarize(diff = confidence[true_p==0.06] - confidence[true_p==0.04])  %>% 
  group_by(viz, n) %>%
  summarise(
    mean = mean(diff), 
    sd = sd(diff), 
    se = sd(diff) / sqrt(length(diff)),
    lwr = quantile(diff, 0.025), 
    upr = quantile(diff, 0.975))

data %>% group_by(id, viz, n) %>% 
  mutate(extreme = confidence %in% c(0, 1))  %>% 
  group_by(viz, n) %>%
  summarise(
    mean = mean(extreme),
    sd = sd(extreme), 
    se = sd(extreme) / sqrt(length(extreme)),
    Q2.5 = mean - 2*se,
    Q97.5 = mean + 2*se) 


data %>% filter(confidence %in% c(0,1)) %>% group_by(p) %>% 
  mutate(extreme = confidence == 1)  %>% 
  group_by(p) %>%
  summarise(
    mean = mean(extreme)) 

data %>% filter(confidence %in% c(0,1)) %>% group_by(p) %>% 
  mutate(extreme = confidence == 0)  %>% 
  group_by(p) %>%
  summarise(
    mean = mean(extreme)) 

data %>% group_by(id, viz) %>% 
  summarize(
    difference = confidence[true_p==0.06] - confidence[true_p==0.04]) %>%
  ggplot(aes(x=viz, y = difference)) + geom_violin() + 
  geom_point(position=position_jitter(0.1))



stan_funs <- "
 real logit_p_gaussian_lpdf(real y, real mu, real sigma,
                            real zoi, real coi) {
     if (y == 0) { 
       return bernoulli_lpmf(1 | zoi) + bernoulli_lpmf(0 | coi); 
     } else if (y == 1) {
       return bernoulli_lpmf(1 | zoi) + bernoulli_lpmf(1 | coi);
     } else { 
       return bernoulli_lpmf(0 | zoi) + normal_lpdf(logit(y) | mu, sigma);
     } 
                                    }
  real logit_p_gaussian_rng(real y, real mu, real sigma,
                            real zoi, real coi) {
    // 0 or 1
    int zero_one = bernoulli_rng(zoi);
    if (zero_one == 1) {
      // casting to real
      int one = bernoulli_rng(coi);
      if (one == 1) {
        return 1.0;
      } else {
        return 0.0;
      }
    } else {
      return inv_logit(normal_rng(mu, sigma));
    }
  }
"

log_lik_logit_p_gaussian <- function(i, draws) {
  mu <- draws$dpars$mu[, i]
  zoi <- draws$dpars$zoi[, i]
  coi <- draws$dpars$coi[, i]
  sigma <- draws$dpars$sigma
  y <- draws$data$Y[i]
  if (y == 0) { 
    dbinom(1, 1, zoi, TRUE) + dbinom(0, 1, coi, TRUE)
  } else if (y == 1) {
    dbinom(1, 1, zoi, TRUE) + dbinom(1, 1, coi, TRUE)
  } else { 
    dbinom(0, 1, zoi, TRUE) + dnorm(qlogis(y), mu, sigma, TRUE)
  } 
}


predict_logit_p_gaussian <- function(i, draws, ...) {
  mu <- draws$dpars$mu[, i]
  zoi <- draws$dpars$zoi[, i]
  coi <- draws$dpars$coi[, i]
  sigma <- draws$dpars$sigma
  zero_one <- rbinom(length(zoi), 1, zoi)
  ifelse(zero_one, rbinom(length(coi), 1, coi), plogis(rnorm(length(mu), mu, sigma)))
}

fitted_logit_p_gaussian <- function(draws) {
  mu <- draws$dpars$mu
  zoi <- draws$dpars$zoi
  coi <- draws$dpars$coi
  sigma <- draws$dpars$sigma
  # no analytical solution for the mean of logistic normal distribution, rely on simulation
  for (i in 1:ncol(mu)) {
    for(j in 1:nrow(mu)) {
      mu[j, i] <- mean(plogis(rnorm(1000, mu[j, i], sigma[j])))
    }
  }
  
  zoi * coi + (1 - zoi) * mu
}


logit_p_gaussian <- custom_family(
  "logit_p_gaussian", 
  dpars = c("mu", "sigma", "zoi", "coi"),
  links = c("identity", "log", "logit", "logit"),
  lb = c(NA, 0, 0, 0), ub = c(NA, NA, 1, 1),
  type = "real", 
  log_lik = log_lik_logit_p_gaussian,
  predict = predict_logit_p_gaussian,
  fitted = fitted_logit_p_gaussian)


data <- data %>% 
  mutate(
    logit_p = qlogis(p),
    p_lt0.05 = factor(p < 0.05, levels = c(TRUE, FALSE), labels = c("Yes", "No")),
    p_eq0.05 = factor(p == 0.05, levels = c(TRUE, FALSE), labels = c("Yes", "No")),
    cat_p = recode_factor(true_p, "0.06" = ">0.05", "0.1" = ">0.05", "0.5" = ">0.05", "0.8" = ">0.05", .ordered = TRUE))

# base model, allow varying main effects per individual
# no varying effect for p = 0.05 due to small number of observations
fit1 <- brm(bf(
  confidence ~ 
    viz * p_lt0.05 * logit_p + 
    viz * p_eq0.05 +
    (viz + p_lt0.05 + logit_p | id),
  zoi ~ 
    viz * true_p + (1 | id),
  coi ~ mo(cat_p)),
  data = data, #prior = priors,
  family = logit_p_gaussian,
  stanvars = stanvar(scode = stan_funs, block = "functions"),
  chains = 2, iter = 4000, init = 0,
  control = list(max_treedepth = 10, adapt_delta = 0.8),
  cores = 2, refresh = 10)

saveRDS(fit1, file="results/fit1.rds")

# add viz * expertise
fit2 <- brm(bf(
  confidence ~ 
    viz * expertise + 
    viz * p_lt0.05 * logit_p + 
    viz * p_eq0.05 +
    (viz + p_lt0.05 + logit_p | id),
  zoi ~ 
    viz * expertise + 
    viz * true_p + (1 | id),
  coi ~ mo(cat_p)),
  data = data, #prior = priors,
  family = logit_p_gaussian,
  stanvars = stanvar(scode = stan_funs, block = "functions"),
  chains = 2, iter = 4000, init = 0,
  control = list(max_treedepth = 10, adapt_delta = 0.8),
  cores = 2, refresh = 10)

saveRDS(fit2, file="results/fit2.rds")
# add varying viz to mu
fit3 <- brm(bf(
  confidence ~ 
    viz * expertise + 
    viz * p_lt0.05 * logit_p + 
    viz * p_eq0.05 +
    (viz + p_lt0.05 + logit_p | id),
  zoi ~ 
    viz * expertise + 
    viz * true_p + (viz | id),
  coi ~ mo(cat_p)),
  data = data, #prior = priors,
  family = logit_p_gaussian,
  stanvars = stanvar(scode = stan_funs, block = "functions"),
  chains = 2, iter = 4000, init = 0,
  control = list(max_treedepth = 10, adapt_delta = 0.8),
  cores = 2, refresh = 10)

saveRDS(fit3, file="results/fit3.rds")

# add varying interactions to mu
fit4 <- brm(bf(
  confidence ~ 
    viz * expertise + 
    viz * p_lt0.05 * logit_p + 
    viz * p_eq0.05 +
    (viz * p_lt0.05 + viz * logit_p + p_lt0.05 * logit_p | id),
  zoi ~ 
    viz * expertise + 
    viz * true_p + (viz | id),
  coi ~ mo(cat_p)),
  data = data, #prior = priors,
  family = logit_p_gaussian,
  stanvars = stanvar(scode = stan_funs, block = "functions"),
  chains = 2, iter = 4000, init = 0,
  control = list(max_treedepth = 10, adapt_delta = 0.8),
  cores = 2, refresh = 10)

saveRDS(fit4, file="results/fit4.rds")


# drop expertise
fit5 <- brm(bf(
  confidence ~ 
    viz * p_lt0.05 * logit_p + 
    viz * p_eq0.05 +
    (viz * p_lt0.05 + viz * logit_p + p_lt0.05 * logit_p | id),
  zoi ~ 
    viz * true_p + (viz | id),
  coi ~ mo(cat_p)),
  data = data, #prior = priors,
  family = logit_p_gaussian,
  stanvars = stanvar(scode = stan_funs, block = "functions"),
  chains = 2, iter = 4000, init = 0,
  control = list(max_treedepth = 10, adapt_delta = 0.8),
  cores = 2, refresh = 10)

saveRDS(fit5, file="results/fit5.rds")

# add three-way interactions to random effects
fit6 <- brm(bf(
  confidence ~ 
    viz * expertise + 
    viz * p_lt0.05 * logit_p + 
    viz * p_eq0.05 +
    (viz * logit_p * p_lt0.05 | id),
  zoi ~ 
    viz * expertise + 
    viz * true_p + (viz | id),
  coi ~ mo(cat_p)),
  data = data, #prior = priors,
  family = logit_p_gaussian,
  stanvars = stanvar(scode = stan_funs, block = "functions"),
  chains = 2, iter = 4000, init = 0,
  control = list(max_treedepth = 10, adapt_delta = 0.8),
  cores = 2, refresh = 10)

saveRDS(fit6, file="results/fit6.rds")

# add three-way interactions to random effects
fit7 <- brm(bf(
  confidence ~ 
    viz * p_lt0.05 * logit_p + 
    viz * p_eq0.05 +
    (viz * logit_p * p_lt0.05 | id),
  zoi ~ 
    viz * true_p + (viz | id),
  coi ~ mo(cat_p)),
  data = data, #prior = priors,
  family = logit_p_gaussian,
  stanvars = stanvar(scode = stan_funs, block = "functions"),
  chains = 2, iter = 4000, init = 0,
  control = list(max_treedepth = 10, adapt_delta = 0.8),
  cores = 2, refresh = 10)

saveRDS(fit7, file="results/fit7.rds")

# add expertise only for confidence
fit8 <- brm(bf(
  confidence ~ 
    viz * expertise +
    viz * p_lt0.05 * logit_p + 
    viz * p_eq0.05 +
    (viz * p_lt0.05 + viz * logit_p + p_lt0.05 * logit_p | id),
  zoi ~ 
    viz * true_p + (viz | id),
  coi ~ mo(cat_p)),
  data = data, #prior = priors,
  family = logit_p_gaussian,
  stanvars = stanvar(scode = stan_funs, block = "functions"),
  chains = 2, iter = 4000, init = 0,
  control = list(max_treedepth = 10, adapt_delta = 0.8),
  cores = 2, refresh = 10)

saveRDS(fit8, file="results/fit8.rds")
K <- 10
folds <- loo::kfold_split_grouped(K = K, x = fit1$data$id)
kfold1 <- kfold(fit1, folds = folds)
saveRDS(kfold1, file = "results/kfold1.rds")
kfold2 <- kfold(fit2, folds = folds)
saveRDS(kfold2, file = "results/kfold2.rds")
kfold3 <- kfold(fit3, folds = folds)
saveRDS(kfold3, file = "results/kfold3.rds")
kfold4 <- kfold(fit4, folds = folds)
saveRDS(kfold4, file = "results/kfold4.rds")
kfold5 <- kfold(fit5, folds = folds)
saveRDS(kfold5, file = "results/kfold5.rds")
kfold6 <- kfold(fit6, folds = folds)
saveRDS(kfold6, file = "results/kfold6.rds")
kfold7 <- kfold(fit7, folds = folds)
saveRDS(kfold7, file = "results/kfold7.rds")
kfold8 <- kfold(fit8, folds = folds)
saveRDS(kfold8, file = "results/kfold8.rds")

k1 <- readRDS("results/kfold1.rds")
k2 <- readRDS("results/kfold2.rds")
k3 <- readRDS("results/kfold3.rds")
k4 <- readRDS("results/kfold4.rds")
k5 <- readRDS("results/kfold5.rds")
k6 <- readRDS("results/kfold6.rds")
k7 <- readRDS("results/kfold7.rds")
k8 <- readRDS("results/kfold8.rds")
loo::compare(k1,k2,k3,k4,k5,k6,k7,k8)

#differences between models 4, 5, 7, and 8 are pretty small (within SE). choose simplest (model 5)