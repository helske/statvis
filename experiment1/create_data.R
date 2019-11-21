library(dplyr)
library(magrittr)
setwd("experiment1")
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
# remove person who didn't answer seriously on the demography part
data <- data_raw[data_raw$degree != "None",]

data$expertise <- factor(data$expertise)
data$expertise <- recode_factor(data$expertise, 
  "Statistics" = "Stats/ML",
  "statistics" = "Stats/ML",
  "statistics/machine learning" = "Stats/ML",
  "Analytics" = "Stats/ML",
  "Statistics/Medicine" = "Stats/ML",
  "Data science" = "Stats/ML",
  "Biostatistics" = "Stats/ML",
  "IT & Business Data Science" = "Stats/ML",
  "methods" = "Stats/ML",
  
  "interaction design and evaluation" = "VIS/HCI",
  "Human-Computer Interaction" = "VIS/HCI",
  "HCI" = "VIS/HCI",
  "Vis" = "VIS/HCI",
  "Visualization" = "VIS/HCI",
  "Data Visualization" = "VIS/HCI",
  "CS, Visualization, HCI" = "VIS/HCI",
  "Infovis" = "VIS/HCI",
  "Visualization / Computer Science" = "VIS/HCI",
  "AI" = "Stats/ML",
  "Virtual Reality" = "VIS/HCI",
  "Visualisation" = "VIS/HCI",
  "Neuroscience and Statistics" = "Stats/ML",
  "research in HCI" = "VIS/HCI",
  
  "Social science" = "Other",
  "Political science" = "Other",
  "sociology" = "Other",
  "Sociology" = "Other",
  "Analytical Sociology" = "Other",
  "Education research" = "Other",
  "Economics" = "Other", 
  "market research" = "Other",
  "Politics" = "Other",
  "Finance" = "Other",
  "Linguistics" = "Other",
  "Education Poliy" = "Other",
  "Political Science" = "Other",
  "Psychology" =  "Other",
  "psychology" =  "Other",
  "Animal science" = "Other",
  "Biology" = "Other",
  "Botany" = "Other",
  "ecology" = "Other",
  "Zoology" = "Other",
  "Physics" = "Other",
  "cognitive neuroscience" = "Other",
  "Neuroscience" = "Other",
  "neuroscience/motor control" = "Other",
  "Biomechanics" = "Other",
  "Neurocognitive Psychology" = "Other",
  "pharma" =  "Other",
  "Public health" = "Other",
  "neurobiology" = "Other",
  "medicine" = "Other",
  "Molcular Biology" = "Other",
  "Wind Energy" = "Other",
  "Mathematical Biology" = "Other",
  "segregation" = "Other",
  "Philosophy" = "Other",
  "Pain" = "Other",
  "genomics" = "Other",
  "organizational science" = "Other",
  "Psychometric" = "Other",
  "Medicine" = "Other",
  "Water engineering" = "Other",
  "Strategic Management" = "Other",
  "network analysis" = "Other",
  "CSS" = "Other",
  "Management"  = "Other",
  "Computer science" = "Other",
  "Computer Science" = "Other",
  "HCI, Visualization" = "VIS/HCI",
  "HCI/Visualization" = "VIS/HCI",
  "Computer vision" = "Stats/ML"
)
table(data$expertise)/32
data$expertise <- relevel(data$expertise, "Other")
# convert to factors and numeric
data$n <- factor(ifelse(as.numeric(data$id) %% 8 < 4, 50, 200))
data$id <- factor(data$id)
data$viz <- factor(data$viz, labels = c("CI", "gradient", "p", "violin"))
# make p-values as reference class
data$viz <- relevel(data$viz, "p")
data$replication <- as.numeric(data$replication)
data$value <- as.numeric(data$value)
# true p-values
p <- c(0.001, 0.01, 0.04, 0.05, 0.06, 0.1, 0.5, 0.8)

# add some variables
data <- data %>% mutate(
  p = p[replication], 
  confidence = (value-1)/99) %>% arrange(id, viz)

# define outlier as a person who had less confidence with p<=0.01 than with p>=0.5 
outliers <- data %>% group_by(id, viz) %>% 
  summarize(
    mistake = min(confidence[p %in% c(0.001,0.01, 0.04)]) < max(confidence[p %in% c(0.1, 0.5, 0.8)])) %>% 
  filter(mistake)

# check the outliers
unique(outliers$id) 
nrow(outliers) #42
library(ggplot2)
data %>% 
  filter((interaction(id,viz) %in% interaction(outliers$id, outliers$viz))) %>%
  mutate(logit_confidence = qlogis((confidence + 0.5) / 2),
    logit_p = qlogis(p)) %>%
  ggplot(aes(x = logit_p, y = logit_confidence, group = id, colour = id)) + 
  geom_line() + 
  geom_point() +
  facet_wrap(~ viz)

#data <- data %>% filter(!(interaction(id,viz) %in% interaction(outliers$id, outliers$viz)))

# for monotonic but non-linear effect on confidence
data$true_p <- factor(data$p)

saveRDS(data, file=paste0(path,"/exp1_data.rds"))


# subjective rankings

files <- list.files(path, pattern="subjective", full.names = TRUE)

# fetch number of participants
(n <- length(files))

rankdata <- data.frame(id = rep(1:n, each=4),
  viz = factor(rep(c("p", "ci", "violin", "gradient")), levels=c("p", "ci", "violin", "gradient")),
  rank = factor(NA, levels=1:4))
feedback <- vector("list", n)

for(i in 1:n) {
  fb <- fromJSON(files[i])
  rankdata$id[4*(i-1) + 1:4] <- strsplit(strsplit(files[i], "subjective")[[1]], ".txt")[[2]]
  rankdata$rank[4*(i-1) + 1:4] <- factor(fb$rank)
  feedback[[i]] <- fb$feedback
}
rankdata$viz <- recode_factor(rankdata$viz, "p" = "p", "ci" = "CI", "gradient" = "gradient", "violin" = "violin")
rankdata$rank <- factor(rankdata$rank, ordered = TRUE)
rankdata$id <- factor(rankdata$id, levels = levels(data$id))
saveRDS(rankdata, file = paste0(path, "/exp1_rankdata.rds"))
saveRDS(feedback, file = paste0(path, "/exp1_feedback.rds"))

