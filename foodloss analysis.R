setwd("D:/WORK/Data Science/Google data analysis/8/Capstone Project 3")

library(tidyverse)
library(ggplot2)

#import data

data <- read.csv("Data.csv")

head(data)
View(data)

## lets do some data cleaning

## lets drop some unneeded columns

rem_col <- c("m49_code", "region", "cpc_code", "loss_percentage_original",
             "sample_size", "method_data_collection", "reference", 
             "reference.1", "reference.2", "loss_quantity", "treatment", "cause_of_loss")

newdata <- data %>% 
  select(- one_of(rem_col))

View(newdata)

## lets do some data explorations
#get column names
colnames(newdata)

dim(newdata)

str(newdata)

summary(newdata)

## lets find unique elements in the columns

unique(newdata$country) # we have 149 countries in this dataset

unique(newdata$commodity) # 195 commodities in this study

unique(newdata$year) # the study span from 2009 to 2021

unique(newdata$activity) #111 activities that leads to loss are mentioned in this study

unique(newdata$food_supply_stage) # 19 food supply stages where losses occur

## lets get min and max loss percentage

min(newdata$loss_percentage) #0.003%

max(newdata$loss_percentage) #65%

## lets check for duplicates
sum(duplicated(newdata)) # detected 390 duplicates

## lets drop duplicates

clean_data <- distinct(newdata)

View(clean_data)

## Also I want to remove aggregations that came with the dataset
aggr_country <- paste(c("Africa", "Australia and New Zealand", "China,Taiwan", "Northern Africa",
                  "Northern America", "Saint Kitts and Nevis", "South-Eastern Asia",
                  "Southern Asia", "Sub-Saharan Africa", "Western Africa", "Western Asia"), 
                  collapse = '|')

cleandata1 <- clean_data %>% 
  filter(!grepl(aggr_country, country))

View(cleandata1)


## data segmentation
## lets group by countries

df_countries <- cleandata1 %>% 
  group_by(country) %>% 
  summarise(mean_loss_percentage = mean(loss_percentage))

View(df_countries)

## group by year

df_year <- cleandata1 %>% 
  group_by(year) %>% 
  summarise(mean_loss_percentage = mean(loss_percentage))

View(df_year)

# group by coutry, year and get percentage yearly loss
df_counyear <- cleandata1 %>% 
  group_by(country, year) %>% 
  summarise(mean_loss_percentage = mean(loss_percentage))

View(df_counyear)

## commodity activity percentage loss
df_commdity <- cleandata1 %>% 
  group_by(country, commodity, year) %>% 
  summarise(average_loss = mean(loss_percentage))

View (df_commdity)


## groupby Supply stage
df_supplychain <- cleandata1 %>% 
  group_by(food_supply_stage) %>% 
  summarise(avarage_loss = mean(loss_percentage))

View(df_supplychain)

##groupby activities
df_activities <- cleandata1 %>% 
  group_by(activity) %>% 
  summarise(avarage_loss = mean(loss_percentage))

View(df_activities)




## Data Analysis

#Question 1: Which Country has the overall Highest Average Loss Percentage over the years?
df_countries[which.max(df_countries$mean_loss_percentage),]

#Question 2: Which Country has the overall Lowest Average Loss Percentage over the years?
df_countries[which.min(df_countries$mean_loss_percentage),]

#Question 3: What year do we have the overall highest average loss percentage?
df_year[which.max(df_year$mean_loss_percentage),]

#Question 4: What year do we have the overall lowest average loss percentage?
df_year[which.min(df_year$mean_loss_percentage),]

#Question 5: Which country has the highest average loss percentage and in what year?
df_counyear[which.max(df_counyear$mean_loss_percentage),]

#Question 6: Which country has the lowest average loss percentage and in what year?
df_counyear[which.min(df_counyear$mean_loss_percentage),]

#Question 7: Which Commodity is often wasted, from which country, and what year do we record the highest loss
df_commdity[which.max(df_commdity$average_loss),]

#Question 8: Which Commodity is the lowest wasted, from which country, and what year do we record the lowest loss
df_commdity[which.min(df_commdity$average_loss),]

#Question 9: Which food supply chain is food most mainly wasted
df_supplychain[which.max(df_supplychain$avarage_loss),]

#Question 10: Which food supply chain generates least waste
df_supplychain[which.min(df_supplychain$avarage_loss),]

#Question 11: Which food production activity generates the highest loss?
df_activities[which.max(df_activities$avarage_loss),]

#Question 12: Which food production activity generates the least loss?
df_activities[which.min(df_activities$avarage_loss),]





## some data visualization
#scatter plot of losses per year
ggplot(data = cleandata1, aes(x=year, y=loss_percentage)) +
  geom_point(aes(color=loss_percentage)) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(labels = as.character(cleandata1$year), breaks = cleandata1$year)


#barplot of average loss per year
ggplot(data = df_year) +
  geom_col(mapping = aes(x=year, y=mean_loss_percentage, fill= mean_loss_percentage)) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(labels = as.character(df_year$year), breaks = df_year$year)



#export data out for dashboard
write.csv(cleandata1, "foodlossclean.csv", row.names = FALSE)
                 