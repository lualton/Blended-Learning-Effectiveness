# Example Test Analysis
# rm(list=ls()) -------------------------------------------

# Packages
library(plyr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(scales)

setwd("/Users/lualt/OneDrive/...") # Workign Directory
data <- read.csv("Student Achievement Example.csv")

data$Gender[data$Gender == ""] <- NA

as.factor(data$Gender)
names(data) <- (c("Year", "Grade", "Gender",
                  "ReadSS", "ReadGE", "ReadNS", "ReadNPR", 
                  "MathSS", "MathGE","MathNS", "MathNPR", "Blended", "Cohort",
                  "Blyears", "LName", "FName", "Id"))
# "Last", "First","Combined", "Birth", "LName", "FName"

glimpse(data)

data <- dplyr::select(data, Year, Grade:Blyears, Id)

#View the data, mean and standard deviation
ddply(data, c("Year", "Grade"), summarise,
      reading = mean(ReadNPR, na.rm = TRUE),
      sd = sd(ReadNPR, na.rm = TRUE),
      math = mean(MathNPR, na.rm = TRUE),
      sd2 = sd(MathNPR, na.rm = TRUE),
      n = length(MathNPR))

# Plots to show national percentage rank in various grades
ggplot(data, aes(Year, MathNPR, group = Year)) +
  geom_boxplot() +
  facet_grid(. ~ Grade) +
  labs(title = "Natl. Perc. Rank by Grade in Math") +
  geom_vline(xintercept = 2013, linetype = 2, col = "red") +
  ylab("Score")

ggplot(data, aes(Year, ReadNPR, group = Year)) +
  geom_boxplot() +
  facet_grid(. ~ Grade) +
  labs(title = "Natl. Perc. Rank by Grade in Reading") +
  geom_vline(xintercept = 2013, linetype = 2, col = "red") +
  ylab("Score")

# T Test by Grade with Blended (0 or 1) as predictor ----------------------

GradeTtest <- function(grade){
  a <- t.test(data[which(data$Grade == grade & data$Blended == 0),]$ReadNPR,
              data[which(data$Grade == grade & data$Blended == 1),]$ReadNPR)
  b <- t.test(data[which(data$Grade == grade & data$Blended == 0),]$MathNPR,
              data[which(data$Grade == grade & data$Blended == 1),]$MathNPR)
  result <- c(a,b)
  return(result)
}

GradeTtest(3)
GradeTtest(5)
GradeTtest(7)

# Difference in achievement status ----------------------------------------
# Ordered by levels in grade 3

data.score <- data %>%
  dplyr::select(Year, Grade, Gender, Id, ReadNPR, MathNPR, Blended) %>%
  gather(key, value, ReadNPR, MathNPR)

ddply(data, c("Blended"), summarise,
      reading = mean(ReadNPR, na.rm = TRUE),
      sd = sd(ReadNPR, na.rm=TRUE),
      math = mean(MathNPR, na.rm = TRUE),
      sd2 = sd(MathNPR, na.rm = TRUE))

# Plots for showing National Percent Rank
ggplot(data.score, aes(Blended, value, col = key, width=1)) +
  geom_jitter(width = .4) +
  facet_grid(.~ Grade) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(breaks= c(0,1), expand = c(.3, 0), minor_breaks = NULL) +
  labs(title = "National Percent Rank of Pre and Post Blended") +
  ylab("Percent Rank") +
  xlab("Blended, 0-Pre, 1-Post") +
  theme(plot.margin = unit(c(0,4,0,4), "cm")) +
  annotate("text", x = .05, y = 75, label = "72") +
  annotate("text", x = .05, y = 67, label = "70") +
  annotate("text", x = 1, y = 63, label = "61") +
  annotate("text", x = 1, y = 55, label = "56")

ggplot(data.score, aes(Blended, value, group = Blended)) +
  geom_boxplot() +
  facet_grid(.~ Grade) +
  scale_x_continuous(breaks= c(0,1), expand = c(.3, 0), minor_breaks = NULL) +
  labs(title = "National Percent Rank of Pre and Post Blended") +
  ylab("Percent Rank") +
  xlab("Blended, 0-Pre, 1-Post") +
  theme(plot.margin = unit(c(0,4,0,4), "cm")) +
  stat_summary(aes(label=round(..y..,2)), fun.y=mean, geom="text", size=4,
               vjust = .2)

ggplot(data.score, aes(Year, value, group = Year)) +
  geom_boxplot() +
  facet_grid(. ~ Grade) +
  labs(title = "Natl. Perc. Rank by Grade") +
  geom_vline(xintercept = 2013, linetype = 2, col = "red") +
  ylab("Score") +
  stat_summary(aes(label=round(..y..,2)), 
               fun.y = mean, geom = "text", size = 2, vjust = -.2)

# Growth Data -----------------------------------------------------------

# Grouping by identity and arranging by year to find performance differences
data.growth <- data %>%
  group_by(Id) %>%
  arrange(Year) %>%
  mutate(ReadSS = ReadSS - lag(ReadSS)) %>%
  mutate(ReadGE = ReadGE - lag(ReadGE)) %>%
  mutate(ReadNS = ReadNS - lag(ReadNS)) %>%
  mutate(ReadNPR = ReadNPR - lag(ReadNPR)) %>%
  mutate(MathSS = MathSS - lag(MathSS)) %>%
  mutate(MathGE = MathGE - lag(MathGE)) %>%
  mutate(MathNS = MathNS - lag(MathNS)) %>%
  mutate(MathNPR = MathNPR - lag(MathNPR)) 

data.growth <- data.growth %>%
  filter(ReadSS != "NA") %>%
  dplyr::select(Year, Grade, ReadGE, MathGE, Blended, Cohort, Blyears, Id) 


# Modifying dataframe for different visualization, 
data.growthScore <- data.growth %>%
  gather(key, value, ReadGE, MathGE) %>%
  filter(Grade != 3)

# T Tests
GrowthTtest <- function(grade){
  a <- t.test(data[which(data$Grade == grade & data$Blended == 0),]$ReadGE,
              data[which(data$Grade == grade & data$Blended == 1),]$ReadGE)
  b <- t.test(data[which(data$Grade == grade & data$Blended == 0),]$MathGE,
              data[which(data$Grade == grade & data$Blended == 1),]$MathGE)
  result <- c(a,b)
  return(result)
}

GrowthTtest(5)
GrowthTtest(7)


# Plots for growth rate by Grade Equivalency
ggplot(data.growth, aes(Blended, ReadGE, group = Blended)) +
  geom_boxplot() +
  labs(title = "Growth Rates in Reading") +
  scale_x_continuous(breaks = c(0,1)) +
  ylab("Percent Rank Difference") +
  xlab("Blended, 0-Pre, 1-Post") +
  theme(plot.margin = unit(c(0,8,0,8), "cm")) +
  stat_summary(aes(label=round(..y..,2)), fun.y=mean, geom="text", size=4,
               vjust = .1)

ggplot(data.growth, aes(Blended, MathGE, group = Blended)) +
  geom_boxplot() +
  labs(title = "Growth Rates in Math") +
  scale_x_continuous(breaks = c(0,1)) +
  ylab("Grade Equivalent Increase") +
  xlab("Blended, 0-Pre, 1-Post") +
  theme(plot.margin = unit(c(0,8,0,8), "cm")) +
  stat_summary(aes(label=round(..y..,2)), fun.y=mean, geom="text", size=4,
               vjust = .1)

ggplot(data.growthScore, aes(Blyears, value, group = Blyears)) +
  geom_boxplot() +
  facet_grid(.~Grade) +
  labs(title = "Growth Rates in Reading") +
  ylab("Grade Equivalent Increase") +
  xlab("Blended, 0-Pre, 1-Post") +
  theme(plot.margin = unit(c(0,4,0,4), "cm")) +
  stat_summary(aes(label=round(..y..,2)), fun.y=mean, geom="text", size=4,
               vjust = .1)


