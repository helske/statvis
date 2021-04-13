# Script to reproduce Figure 4 of the paper

# List of packages for session
.packages = c("brms", "modelr", "ggplot2", "dplyr")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
invisible(lapply(.packages, require, character.only = TRUE))

# Functions used by the brms model

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
  mu <- brms:::get_dpar(draws, "mu", i = i)
  zoi <- brms:::get_dpar(draws, "zoi", i = i)
  coi <- brms:::get_dpar(draws, "coi", i = i)
  sigma <- brms:::get_dpar(draws, "sigma", i = i)
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
  mu <- brms:::get_dpar(draws, "mu", i = i)
  zoi <- brms:::get_dpar(draws, "zoi", i = i)
  coi <- brms:::get_dpar(draws, "coi", i = i)
  sigma <- brms:::get_dpar(draws, "sigma", i = i)
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

# Read the data and add some variables

# Data can be found at the Github repo in the same path as this script
data <- readRDS("exp1_data.rds")

data <- data %>% 
  mutate(
    logit_p = qlogis(p),
    p_lt0.05 = factor(p < 0.05, levels = c(TRUE, FALSE), labels = c("Yes", "No")),
    p_eq0.05 = factor(p == 0.05, levels = c(TRUE, FALSE), labels = c("Yes", "No")),
    cat_p = recode_factor(true_p, 
      "0.06" = ">0.05", "0.1" = ">0.05", "0.5" = ">0.05", "0.8" = ">0.05",
      .ordered = TRUE))

# Fit the model

fit_exp1 <- brm(bf(
  confidence ~ 
    viz * p_lt0.05 * logit_p + 
    viz * p_eq0.05 +
    (viz + p_lt0.05 * logit_p + p_eq0.05 | id),
  zoi ~ 
    viz * true_p + (viz | id),
  coi ~ mo(cat_p),
  sigma ~ viz + (1 | id)),
  data = data,
  family = logit_p_gaussian,
  stanvars = stanvar(scode = stan_funs, block = "functions"),
  chains = 4, cores = 4, iter = 2000, init = 0, seed = 123,
  save_warmup = FALSE, refresh = 10)

# Compute results for Figure 4 of the paper
comb_exp1 <- fit_exp1$data %>% 
  data_grid(viz, logit_p, p_lt0.05, p_eq0.05, cat_p, true_p) %>% 
  filter(interaction(logit_p, p_lt0.05, p_eq0.05, cat_p, true_p) %in% 
      unique(interaction( 
        fit_exp1$data$logit_p, fit_exp1$data$p_lt0.05, 
        fit_exp1$data$p_eq0.05, fit_exp1$data$cat_p, 
        fit_exp1$data$true_p)))

f_mu_exp1 <- posterior_epred(fit_exp1, newdata = comb_exp1, re_formula = NA)

d <- data.frame(value = c(f_mu_exp1), 
  p = rep(comb_exp1$true_p, each = nrow(f_mu_exp1)),
  viz = rep(comb_exp1$viz, each = nrow(f_mu_exp1)),
  iter = 1:nrow(f_mu_exp1))
levels(d$viz) <- c("Textual", "Classic CI", "Gradient CI", "Violin CI")

# Draw figure

cols <- c("Textual" = "#D55E00", "Classic CI" = "#0072B2", 
  "Gradient CI" = "#009E73", "Violin CI" = "#CC79A7")

p <- d %>% group_by(viz, iter) %>% 
  summarise(difference = value[p == "0.04"] - value[p == "0.06"]) %>% 
  ggplot(aes(x = difference, fill = viz, colour = viz)) + 
  geom_density(bw = 0.01, alpha = 0.6) +
  theme_classic() + 
  scale_fill_manual("Representation", values =  cols) + 
  scale_colour_manual("Representation", values =  cols) + 
  ylab("Posterior density") + 
  xlab("E[confidence(p=0.04) - confidence(p=0.06)]") +
  theme(legend.position = "bottom", 
    legend.title = element_blank(),
    axis.text.x = element_text(size = 12, hjust = 1, vjust = 1), 
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.text = element_text(size = 14)) 
p

ggsave(p, file = "replicability_stamp/fig4.png", device = "png", 
  width = 5, height = 5, dpi = 50)


