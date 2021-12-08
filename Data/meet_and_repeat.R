# Victor Gonzalez
# 22/11/2021
# Analysis of longitudinal data
##

# Set working directory clean up memory and load tidyverse

setwd("C:/Users/ramosgon/OneDrive - University of Helsinki/Courses/OpenDataScience/IODS-project/Data")

rm(list=ls())
library(tidyverse)


# Read the data

bprs_wide <- read.csv("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt",
                 sep = " ",
                 stringsAsFactors = F)


rats_wide <- read.csv("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt",
                 sep = "\t",
                 stringsAsFactors = F)

# Explore the data sets and look at the variable summary

str(bprs_wide)
summary(bprs_wide)

str(rats_wide)
summary(rats_wide)

# Convert to long for and change categorical variables to factors

bprs_long <- bprs_wide %>%
  pivot_longer(cols = starts_with("week"),
               names_to = "week",
               names_prefix = "week",
               values_to = "BPRS") %>%
  mutate(across(c(treatment,subject, week), factor))

bprs_wide <- bprs_wide %>%
  mutate(across(c(treatment,subject), factor))

rats_long <- rats_wide %>%
  pivot_longer(cols = starts_with("WD"),
               names_to = "Time",
               names_prefix = "WD",
               values_to = "Weight") %>%
  mutate(across(c(ID, Group, Time), factor))

rats_wide <- rats_wide %>%
  mutate(across(c(ID, Group), factor))

# Look at the differences between long and wide sets

glimpse(bprs_long)
glimpse(bprs_wide)

str(bprs_long)
summary(bprs_long)

glimpse(rats_long)
glimpse(rats_wide)

str(rats_long)
summary(rats_long)

# Save the files

bprs_long %>% write.csv("bprs_long.csv", row.names = F)
bprs_wide %>% write.csv("bprs_wide.csv", row.names = F)

rats_long %>% write.csv("rats_long.csv", row.names = F)
rats_wide %>% write.csv("rats_wide.csv", row.names = F)
