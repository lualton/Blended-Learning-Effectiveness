# -------------------------------------------------------
# DAT Data
# rm(list=ls())

library(plyr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(scales)
library(arm)

setwd("/Users/lualt/OneDrive/Work/CRPE/BOLD/Test Scores")

data <- read.csv("DAT Test Full.csv")

data$Gender[data$Gender == ""] <- NA

as.factor(data$Gender)
names(data) <- (c("Year", "Last", "First","Combined", "Birth", "Grade", "Gender",
                  "ReadSS", "ReadGE", "ReadNS", "ReadNPR", 
                  "MathSS", "MathGE","MathNS", "MathNPR", "Bold", "Cohort",
                  "Blyears", "LName", "FName", "Id"))

glimpse(data)
str(data)

data <- dplyr::select(data, Year, Grade:Blyears, Id)

ddply(data, c("Year", "Grade"), summarise,
      reading = mean(ReadNPR, na.rm = TRUE),
      sd = sd(ReadNPR, na.rm = TRUE),
      math = mean(MathNPR, na.rm = TRUE),
      sd2 = sd(MathNPR, na.rm = TRUE),
      n = length(MathNPR))


head(data[order(data$ReadGE,decreasing=T),],.05*nrow(data), 50)

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

ggplot(filter(data, Cohort != 0), aes(Year, MathNPR, group = Year)) +
  geom_boxplot() +
  facet_grid(. ~ Cohort) +
  geom_vline(xintercept = 2013, linetype = 2) +
  labs(title = "Natl. Perc. Rank by Cohort in Math") +
  ylab("Score")

ggplot(filter(data, Cohort != 0), aes(Year, ReadNPR, group = Year)) +
  geom_boxplot() +
  facet_grid(. ~ Cohort) +
  geom_vline(xintercept = 2013, linetype = 2) +
  labs(title = "Natl. Perc. Rank by Cohort in Reading") +
  ylab("Score")

# Grade Analysis and BOLD -------------------------------------------------

GradeTtest <- function(grade){
  a <- t.test(data[which(data$Grade == grade & data$Bold == 0),]$ReadNPR,
              data[which(data$Grade == grade & data$Bold == 1),]$ReadNPR)
  b <- t.test(data[which(data$Grade == grade & data$Bold == 0),]$MathNPR,
              data[which(data$Grade == grade & data$Bold == 1),]$MathNPR)
  result <- c(a,b)
  return(result)
}

# T Test by Grades
# First number is Reading, Second number is Math
GradeTtest(3)
GradeTtest(5)
GradeTtest(7)

t.test(data[which(data$Bold == 0),]$ReadNPR,
       data[which(data$Bold == 1),]$ReadNPR)

t.test(data[which(data$Bold == 0),]$MathNPR,
       data[which(data$Bold == 1),]$MathNPR)

npr.scores <- ddply(data, c("Grade", "Bold"), summarise,
                    reading = mean(ReadNPR, na.rm = TRUE),
                    sd = sd(ReadNPR, na.rm=TRUE),
                    math = mean(MathNPR, na.rm = TRUE),
                    sd2 = sd(MathNPR, na.rm = TRUE))

ggplot(npr.scores, aes(Year, reading)) +
  geom_path() +
  facet_grid(. ~ Grade)


npr.scores <- ddply(data, c("Bold"), summarise,
                    reading = mean(ReadNPR, na.rm = TRUE),
                    math = mean(MathNPR, na.rm = TRUE))

ggplot(npr.scores, aes(Year, reading)) +
  geom_path() +
  facet_grid(. ~ Grade)



# Difference in achievement status ----------------------------------------
# Ordered by levels in grade 3

cohort.four <- data %>%
  filter(Cohort == 4) %>%
  arrange(Year, desc(ReadGE))

data.score <- data %>%
  dplyr::select(Year, Grade, Gender, Id, ReadNPR, MathNPR, Bold) %>%
  gather(key, value, ReadNPR, MathNPR)

fit <- lm(ReadNPR ~ Bold, data = data)
display(fit)
summary(fit)

fit <- lm(MathNPR ~ Bold, data = data)
display(fit)

fit <- lm(value ~ Bold, data = data.score)
display(fit)


ddply(data, c("Bold"), summarise,
      reading = mean(ReadNPR, na.rm = TRUE),
      sd = sd(ReadNPR, na.rm=TRUE),
      math = mean(MathNPR, na.rm = TRUE),
      sd2 = sd(MathNPR, na.rm = TRUE))

ggplot(data.score, aes(Bold, value, col = key, width=1)) +
  geom_jitter(width = .4) +
  facet_grid(.~ Grade) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(breaks= c(0,1), expand = c(.3, 0), minor_breaks = NULL) +
  labs(title = "National Percent Rank of Pre and Post Bold") +
  ylab("Percent Rank") +
  xlab("Bold, 0-Pre, 1-Post") +
  theme(plot.margin = unit(c(0,4,0,4), "cm")) +
  annotate("text", x = .05, y = 75, label = "72") +
  annotate("text", x = .05, y = 67, label = "70") +
  annotate("text", x = 1, y = 63, label = "61") +
  annotate("text", x = 1, y = 55, label = "56")

ggplot(data.score, aes(Bold, value, group = Bold)) +
  geom_boxplot() +
  facet_grid(.~ Grade) +
  scale_x_continuous(breaks= c(0,1), expand = c(.3, 0), minor_breaks = NULL) +
  labs(title = "National Percent Rank of Pre and Post Bold") +
  ylab("Percent Rank") +
  xlab("Bold, 0-Pre, 1-Post") +
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
# Can we use stanford data to reveal some looks about the overall average
# and how it might compare here?


ddply(data, c("Year", "Grade"), summarise,
      reading = mean(ReadGE, na.rm = TRUE),
      sd = sd(ReadGE, na.rm = TRUE),
      math = mean(MathGE, na.rm = TRUE),
      sd2 = sd(MathGE, na.rm = TRUE),
      n = length(MathGE))


data.growth <- data %>%
  group_by(Id) %>%
  arrange(Year) %>%
  mutate(ReadSS = (ReadSS - lag(ReadSS)) %>%
           mutate(ReadGE = (ReadGE - lag(ReadGE))) %>%
           mutate(ReadNS = (ReadNS - lag(ReadNS))) %>%
           mutate(ReadNPR = (ReadNPR - lag(ReadNPR))) %>%
           mutate(MathSS = (MathSS - lag(MathSS)))) %>%
  mutate(MathGE = (MathGE - lag(MathGE))) %>%
  mutate(MathNS = (MathNS - lag(MathNS))) %>%
  mutate(MathNPR = (MathNPR - lag(MathNPR))) 

data.growth <- data %>%
  group_by(Id) %>%
  arrange(Year) %>%
  mutate(RGrowth = (ReadNPR - lag(ReadNPR))/lag(ReadNPR)) %>%
  mutate(MGrowth = (MathNPR - lag(MathNPR))/lag(MathNPR))

data.growth <- data.growth %>%
  filter(RGrowth != "NA") %>%
  dplyr::select(Year, Grade, ReadGE, MathGE, Bold, Cohort, Blyears, Id) 

data.growthScore <- gather(data.growth, key, value, ReadGE, MathGE)


# T Tests
GrowthTtest <- function(grade){
  a <- t.test(data[which(data$Grade == grade & data$Bold == 0),]$ReadGE,
              data[which(data$Grade == grade & data$Bold == 1),]$ReadGE)
  b <- t.test(data[which(data$Grade == grade & data$Bold == 0),]$MathGE,
              data[which(data$Grade == grade & data$Bold == 1),]$MathGE)
  result <- c(a,b)
  return(result)
}

GrowthTtest(5)
GrowthTtest(7)

fit <- lm(ReadGE ~ Bold, data = data.growth)
display(fit)
summary(fit)

ggplot(data.growth, aes(Bold, RGrowth, group = Bold)) +
  geom_boxplot() +
  labs(title = "Growth Rates in Reading") +
  scale_x_continuous(breaks = c(0,1)) +
  ylab("Percent Rank Difference") +
  xlab("Bold, 0-Pre, 1-Post") +
  theme(plot.margin = unit(c(0,8,0,8), "cm")) +
  stat_summary(aes(label=round(..y..,2)), fun.y=mean, geom="text", size=4,
               vjust = .1)

ggplot(data.growth, aes(Bold, MathGE, group = Bold)) +
  geom_boxplot() +
  labs(title = "Growth Rates in Math") +
  scale_x_continuous(breaks = c(0,1)) +
  ylab("Grade Equivalent Increase") +
  xlab("Bold, 0-Pre, 1-Post") +
  theme(plot.margin = unit(c(0,8,0,8), "cm")) +
  stat_summary(aes(label=round(..y..,2)), fun.y=mean, geom="text", size=4,
               vjust = .1)

ggplot(data.growthScore, aes(Blyears, value, group = Blyears)) +
  geom_boxplot() +
  facet_grid(.~Grade) +
  labs(title = "Growth Rates in Reading") +
  ylab("Grade Equivalent Increase") +
  xlab("Bold, 0-Pre, 1-Post") +
  theme(plot.margin = unit(c(0,4,0,4), "cm")) +
  stat_summary(aes(label=round(..y..,2)), fun.y=mean, geom="text", size=4,
               vjust = .1)

#summarise, stat, summary


ggplot(data, aes(Grade, ReadNPR, group = Grade, col = Cohort)) +
  geom_jitter(width = .4) +
  facet_grid(.~ Year) +
  scale_x_continuous(breaks=c(3,5,7))

ggplot(data, aes(Grade, ReadNPR, group = Grade, col = Cohort)) +
  geom_boxplot() +
  facet_grid(.~ Year) +
  scale_x_continuous(breaks=c(3,5,7))


ggplot(data, aes(ReadNPR)) +
  geom_density() +
  scale_x_continuous(breaks = c(0,25,50,75,100)) +
  facet_grid(.~ Cohort + Year) 

