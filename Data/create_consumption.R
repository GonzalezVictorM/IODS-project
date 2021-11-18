# Victor Gonzalez
# 016/11/2021
# Logistic Regression tasks and exercises
## Data from https://archive.ics.uci.edu/ml/datasets/Student+Performance
## This data approach student achievement in secondary education of two Portuguese schools. The data attributes include student grades, demographic, social and school related features) and it was collected by using school reports and questionnaires. Two datasets are provided regarding the performance in two distinct subjects: Mathematics (mat) and Portuguese language (por). In [Cortez and Silva, 2008], the two datasets were modeled under binary/five-level classification and regression tasks. Important note: the target attribute G3 has a strong correlation with attributes G2 and G1. This occurs because G3 is the final year grade (issued at the 3rd period), while G1 and G2 correspond to the 1st and 2nd period grades. It is more difficult to predict G3 without G2 and G1, but such prediction is much more useful (see paper source for more details).

# Set working directory clean up memory and load tidyverse

setwd("C:/Users/ramosgon/OneDrive - University of Helsinki/Courses/OpenDataScience/IODS-project/Data")

rm(list=ls())

library(tidyverse)

# Read the files and explore structure and dimensions

dat_por <- read.csv("student-por.csv", header = T, sep = ";")

dim(dat_por)
str(dat_por)

dat_math <- read.csv("student-mat.csv", header = T, sep = ";")

dim(dat_math)
str(dat_math)

# Define own id for both datasets

por_id <- dat_por %>% mutate(id=1000+row_number())

math_id <- dat_math %>% mutate(id=2000+row_number())

# Which columns vary in datasets

free_cols <- c("id","failures","paid","absences","G1","G2","G3")

# The rest of the columns are common identifiers used for joining the datasets

join_cols <- setdiff(colnames(por_id),free_cols)

# Use inner_join to extract the 370 students that appear in both

pormath_id <- inner_join(por_id, math_id, by = join_cols, suffix = c(".p",".m")) # add a .p or .m to the corresponding columns

# Check the Dimensions and structure

dim(pormath_id)
str(pormath_id)

# Create a vector with the new columns

mutate_cols <- paste(free_cols,".p", sep = "") %>%
  rbind(paste(free_cols,".m", sep = "")) %>% 
  .[1:14]

# Get the means or first value for each repeated column

pormath <- pormath_id %>%
  select(join_cols,mutate_cols) %>% # organize the columns
  mutate( # mutate the columns to get avg or first as well as getting the new variables
    failures = (failures.p + failures.m)/2,
    paid = first(paid.p,paid.m),
    absences = (absences.p + absences.m)/2,
    G1 = (G1.p + G1.m)/2,
    G2 = (G2.p + G2.m)/2,
    G3 = (G3.p + G3.m)/2,
    alc_use = (Dalc + Walc) / 2,
    high_use = alc_use > 2,
    cid=3000+row_number()
  )

glimpse(pormath)

# Write the csv

pormath %>% write.csv("pormath.csv", row.names = F)