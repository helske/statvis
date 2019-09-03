library(dplyr)
library(magrittr)
setwd("experiment2")
path <- "data"
library(jsonlite)
answers <- list.files(path, pattern="answers", full.names = TRUE)

# fetch number of participants
(n <- length(answers))

# create a data frame for the results
data_raw <- data.frame(id = rep(1:n, each = 32), viz = NA, replication = NA, value = NA,
  expertise = NA, degree = NA)

# read in answers, stupid way will do for now
for(i in 1:n){
  x <- strsplit(fromJSON(answers[i]), ",")
  dem <- fromJSON(paste0(path,  "/demography", x[[1]][1], ".txt"))
  for(j in 1:32) {
    data_raw[32*(i-1) + j, c("id", "viz", "replication", "value")] <- x[[j]]
    data_raw[32*(i-1) + j, c("expertise", "degree")] <- dem[c("expertise", "level")]
  }
}
saveRDS(data_raw, file=paste0(path, "/data_raw.rds"))


table(data_raw$degree) / 32
data <- data_raw
data$expertise <- factor(data$expertise)

data$expertise <- recode_factor(data$expertise, 
  "Statistics" = "Stats/ML",
  "machine learning, statistics" = "Stats/ML",
  "infovis" = "VIS/HCI",
  "HCI and VIS" = "VIS/HCI",
  "HCI" = "VIS/HCI",
  "vis" = "VIS/HCI",
  "Vis and HCI" = "VIS/HCI", 
  "Visualisation" = "VIS/HCI",
  "Visualization" = "VIS/HCI",
  .default = "Other"
)
table(data$expertise)/32
data$expertise <- relevel(data$expertise, "Other")

# convert to factors and numeric
data$n <- factor(ifelse(as.numeric(data$id) %% 8 < 4, 50, 200))
data$id <- factor(data$id)
data$expertise <- relevel(data$expertise, "Other")
# convert to factors and numeric
data$id <- factor(data$id)
data$viz <- factor(data$viz, labels = c("CI", "Gradient", "Continuous Violin", "Discrete Violin"))
# make CI as reference class
data$viz <- relevel(data$viz, "CI")
data$replication <- as.numeric(data$replication)
data$value <- as.numeric(data$value)
# true p-values
p <- c(0.001, 0.01, 0.04, 0.05, 0.06, 0.1, 0.5, 0.8)

# add some variables
data <- data %>% mutate(
  p = p[replication], 
  confidence = (value-1)/99) %>% arrange(id, viz)

# for monotonic but non-linear effect on confidence
data$true_p <- factor(data$p)
outliers <- data %>% group_by(id, viz) %>% 
  summarize(
    mistake = min(confidence[p %in% c(0.001,0.01, 0.04)]) < max(confidence[p %in% c(0.1, 0.5, 0.8)])) %>% 
  filter(mistake)

data <- data %>% filter(!(interaction(id,viz) %in% interaction(outliers$id, outliers$viz)))

saveRDS(data, file=paste0(path,"/exp2_data.rds"))

# subjective rankings

files <- list.files(path, pattern="subjective", full.names = TRUE)

# fetch number of participants
(n <- length(files))

rankdata <- data.frame(id = rep(1:n, each=4),
  viz = factor(rep(c("violin2", "ci", "violin", "gradient")), levels=c("violin2", "ci", "violin", "gradient")),
  rank = factor(NA, levels=1:4))
feedback <- vector("list", n)

for(i in 1:n) {
  fb <- fromJSON(files[i])
  rankdata$id[4*(i-1) + 1:4] <- strsplit(strsplit(files[i], "subjective")[[1]], ".txt")[[2]]
  rankdata$rank[4*(i-1) + 1:4] <- factor(fb$rank)
  feedback[[i]] <- fb$feedback
}
rankdata$viz <- recode_factor(rankdata$viz, "violin2" = "Discrete Violin", "ci" = "CI", "gradient" = "Gradient", "violin" = "Continuous Violin")
# make CI as reference class
rankdata$viz <- relevel(rankdata$viz, "CI")

rankdata$rank <- factor(rankdata$rank, ordered = TRUE)
rankdata$id <- factor(rankdata$id, levels = levels(data$id))
saveRDS(rankdata, file = paste0(path, "/exp2_rankdata.rds"))
saveRDS(feedback, file = paste0(path, "/exp2_feedback.rds"))

