library(dplyr)
library(brms)
library(ggplot2)

data <- readRDS("experiment2/data/exp2_data.rds")
ranks <- readRDS("experiment2/data/exp2_rankdata.rds")
ranks <- distinct(inner_join(ranks, data[,c("id","viz","expertise")]))

fit1 <- brm(rank ~ viz * expertise + (1 | id), family=cumulative, data = ranks)
fit2 <- brm(rank ~ viz + (1 | id), family=cumulative, data = ranks)

fit1 <- add_criterion(fit1, "loo", reloo = TRUE)
fit2 <- add_criterion(fit2, "loo")

loo_compare(fit1, fit2)
# expertise doesn't really add much, use simpler model
fit <- fit2
saveRDS(fit, file="experiment2/results/ranking_result.rds")
effects <- marginal_effects(fit, effects = "viz", plot = FALSE, categorical = TRUE, 
                            reformula=NA)

p <- ggplot(effects[[1]], aes(x=viz, y = estimate__, colour = cats__)) + 
  geom_point(position=position_dodge(0.5)) + 
  geom_errorbar(width=0.25, aes(ymin=lower__, ymax = upper__),position=position_dodge(0.5)) + 
  theme_bw() + 
  ylab("Ranking probability") + xlab("Representation") +
 # scale_y_continuous(limits = 0:1) +
  scale_x_discrete(labels = c("CI", "Gradient CI", "Cont. Violin CI", "Disc. Violin CI")) +
  scale_color_discrete("Rank", 
                       labels = c("1 (best)", "2", "3", "4 (worst)")) + 
  theme(axis.text.x = element_text(size = 10), legend.position = "bottom", 
        axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 12),
        legend.text=element_text(size = 10), strip.text.x = element_text(size = 10))

ggsave(p, file="experiment2/results/ranks2.pdf", width = 6, height = 3)
