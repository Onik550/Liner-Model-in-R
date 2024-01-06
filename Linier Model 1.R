setwd("C:/Users/rkoni/OneDrive/Desktop/R Programming")

library(dplyr)
library(ggplot2)
library(tidyverse)
library(patchwork)
library(caTools)


Realstate <- read.csv(file.choose(), header = T)



Realstate <- Realstate %>% 
  select(LotArea, YearBuilt, OverallCond, BsmtFullBath, FullBath, SalePrice, PavedDrive)

glimpse(Realstate)

Realstate$PavedDrive <- as.factor(Realstate$PavedDrive)


## Labeling Data
Realstate$PavedDrive <- factor(Realstate$PavedDrive, labels = c(0,1,2))

glimpse(Realstate)

## visualizing for examining relationships between dependent & Independent Variables

Realstate %>% 
  ggplot(aes(x = LotArea, y = SalePrice))+
  geom_point(col = "skyblue")+
  geom_smooth(method = lm, se = T, col = "pink")

Realstate %>% 
  ggplot(aes(x = FullBath, y = SalePrice))+
  geom_point(col = "orange")+
  geom_smooth(method = lm, se = T, col = "pink")

Realstate %>% 
  ggplot(aes(x = YearBuilt, y = SalePrice))+
  geom_point(col = "skyblue")+
  geom_smooth(method = lm, se = T, col = "pink")

Realstate %>% 
  ggplot(aes(x = PavedDrive, y = SalePrice))+
  geom_point(col = "skyblue")+
  geom_smooth(method = lm, se = T, col = "pink")

Realstate %>% 
  ggplot(aes(x= LotArea, y= SalePrice, col= factor(FullBath)))+
  geom_point()+
  geom_smooth(method = "lm", se= T)


## Spliting Data for Building the Model & Testing 

split_index <- sample.split(Realstate$SalePrice, SplitRatio = .65)

training <- subset(Realstate, split_index == T)
testing <- subset(Realstate, split_index == F)

nrow(training)
nrow(testing)

view(train)

## Model Building 

lmodel <- lm(SalePrice~., data = training)
result <- predict(lmodel, testing)
Comparing <- cbind(actual = testing$SalePrice, predicted = result)
Comparing <- as.data.frame(Comparing)
Error1 <- Comparing$actual - Comparing$predicted
Comparing <- cbind(Comparing, Error1)
view(Comparing)

##
mean(Comparing$Error1)  
##
summary(lmodel)








