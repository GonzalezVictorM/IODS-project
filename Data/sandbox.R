setwd("C:/Users/ramosgon/OneDrive - University of Helsinki/Courses/OpenDataScience/IODS-project/Data")

rm(list=ls())
par(mfrow = c(1,1))

install.packages("car")
library(car)


qqPlot(Boston$indus)

colnames(Boston)[-c(4,9)] %>% sapply(function(x){
  qqPlot(Boston[,x], main = x)
})









## Cycle to test 10 random variable 

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