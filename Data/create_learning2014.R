# Victor Gonzalez
# 09/11/2021
# Data wrangling and analysis tasks and exercises

rm(list=ls())

library(tidyverse)

# Save the URL as a string for easier handling

url <- as.character("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt")

# Read the table and print the dimensions and structure

lrn14 <- read.table(url, header = T, sep = "\t")

dim(lrn14) # The data contains 183 observations of 60 variables

str(lrn14) # 59 of the variables are integer variables, while 1 (gender) is char

# Create the "attitude" column by dividing "Attitude" by 10 

lrn14 <- lrn14 %>% 
  mutate(attitude = Attitude/10)

# define questions related to deep, surface and strategic learning

deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")

# Take the selected columns, apply function rowMeans, and save in their own variables

lrn14$deep <- lrn14 %>% 
  select(one_of(deep_questions)) %>%
  rowMeans

lrn14$surf <- lrn14 %>% 
  select(one_of(surface_questions)) %>%
  rowMeans

lrn14$stra <- lrn14 %>% 
  select(one_of(strategic_questions)) %>%
  rowMeans

# Define and slect the columns to keep

keep_columns <- c("gender","Age","attitude", "deep", "stra", "surf", "Points")

learning2014 <- lrn14 %>% select(one_of(keep_columns))
  
# Rename the columns

colnames(learning2014)[2] <- "age"

colnames(learning2014)[7] <- "points"

# Select male students

male_students <- filter(learning2014, gender == "M")

# keep only points > 0

learning2014 <- learning2014 %>%
  filter(points > 0)

# set working directory and save the analysis dataset

setwd("C:/Users/ramosgon/OneDrive - University of Helsinki/Courses/OpenDataScience/IODS-project/Data")

write.csv(lrn14,"learning2014.csv", row.names = F)

# Checking that it reads.

learning2014_test <- read.table("learning2014.csv", header = T, sep = ",")








