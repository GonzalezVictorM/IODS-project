# Victor Gonzalez
# 22/11/2021
# Dimensionality reduction techniques
## The Human Development Index (HDI) is a summary measure of average 
## achievement in key dimensions of human development: a long and healthy life, 
## being knowledgeable and have a decent standard of living. g. The HDI is the 
## geometric mean of normalized indices for each of the three dimensions. The 
## Gender Inequality Index (GII) reflects gender-based disadvantage in three 
## dimensions—reproductive health, empowerment and the labor market—for as many 
## countries as data of reasonable quality allow. It shows the loss in potential 
## human development due to inequality between female and male achievements in 
## these dimensions. The following data sets consolidate the HDI and GII 
## indicators (17) for 195 countries. For more information on the topic, visit 
## the following links:
### http://hdr.undp.org/sites/default/files/hdr2015_technical_notes.pdf
### http://hdr.undp.org/en/content/human-development-index-hdi


# Set working directory clean up memory and load tidyverse

setwd("C:/Users/ramosgon/OneDrive - University of Helsinki/Courses/OpenDataScience/IODS-project/Data")

rm(list=ls())

# Read the data

hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)

gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

# Exploring the data sets

dim(hd)
str(hd)

dim(gii)
str(gii)

summary(hd)
summary(gii)

# Whenever you change variable names, always keep a table that relates them.

hdcols <- colnames(hd) %>%
  cbind(c("HDIrank","Country","hdi","LEaB","EYoE","MYoE","GNIpC","GNIminusHDIrank")) %>%
  as.data.frame()
colnames(hdcols) <- c("Old","New")

giicols <- colnames(gii) %>%
  cbind(c("GIIrank","Country","gii","MMR","ABR","PRiP","edu2F","edu2M","labF","labM")) %>%
  as.data.frame(col.names = c("Original", "New"))              
colnames(giicols) <- c("Old","New")

# Now we can change the column names

colnames(hd) <- hdcols$New
colnames(gii) <- giicols$New

# Calculate the secondary education and labour force participation ration  

gii <- gii %>%
  mutate(edu2Ratio = edu2F/edu2M, labRatio = labF/labM)

# Join the data keeping only countries in both tables.

human <- inner_join(hd, gii, by = "Country")

# Create the file.
human %>% write.csv("human.csv", row.names = F)