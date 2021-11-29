setwd("C:/Users/ramosgon/OneDrive - University of Helsinki/Courses/OpenDataScience/IODS-project/Data")

rm(list=ls())
par(mfrow = c(1,1))

## Cycle to test 10 random variable with logistic model

deltas <- matrix(nrow = 10,ncol=2)
deltas[,1] <- c(1:10)

randomvars <- colnames(pormath)[-49] %>%
  sample(size = 10)

for (i in 1:10) {
  
  factors <- randomvars[1:i] %>%
    paste0(collapse = " + ")
  
  design <- paste0("high_use ~ ", factors) %>%
    as.formula
  
  model <- glm(design, data = pormath, family = "binomial")
  
  cv <- pormath %>%
    cv.glm(cost = loss_func, glmfit = model, K = 10)
  
  deltas[i,2] <- cv$delta[1]
  
}

plot(deltas[,1], deltas[,2], type = "l")


## Human sandbox

setwd("C:/Users/ramosgon/OneDrive - University of Helsinki/Courses/OpenDataScience/IODS-project/Data")

rm(list=ls())
library(tidyverse)

human <- read.csv("human_base.csv", header = T, sep = ",")

human$GNI <- human$GNI %>%
  gsub(",","", x = .) %>%
  as.numeric

keep <- c("Country", "Edu2.FM", "Labo.FM", "Edu.Exp", "Life.Exp",
          "GNI", "Mat.Mor", "Ado.Birth", "Parli.F")

human <- human %>%
  select(one_of(keep))

data.frame(human, comp = complete.cases(human))

human_ <- human %>% 
  filter(complete.cases(human))

tail(human_, 10)

last <- nrow(human_) - 7

human_ <- human_[1:last, ]

rownames(human_) <- human_$Country

human <- human_[,-1]

human %>% write.csv("human.csv", row.names = F)

