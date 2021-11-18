setwd("C:/Users/ramosgon/OneDrive - University of Helsinki/Courses/OpenDataScience/IODS-project/Data")

rm(list=ls())

library(tidyverse)
library(ggplot2)
library(ggExtra)
library(GGally)
library(boot)
library(ggpubr)

pormath <- read.csv("pormath.csv", header = T, sep = ",")

dim(pormath)

str(pormath)

pormath_subset <- pormath %>% 
  select(sex, high_use, alc_use, age, health, famrel, famsup) %>%
  mutate(across(c(sex, health, famrel, famsup), factor))



pormath_subset %>%
  select(-sex) %>%
  ggpairs(mapping = aes(col = high_use, alpha = 0.3), 
          lower = list(combo = wrap("facethist", bins = 20)))

pormath_subset %>%
  select(-high_use) %>%
  ggpairs(mapping = aes(col = sex, alpha = 0.3), 
          lower = list(combo = wrap("facethist", bins = 20)))

pormath_subset %>%
  gather(key,value,-high_use) %>%
  ggplot(aes(value, fill= high_use)) +
  geom_bar(position = "dodge", alpha = 0.6) +
  facet_wrap("key", scales = "free") +
  theme_classic()

pormath_subset %>%
  gather(key,value,-sex) %>%
  ggplot(aes(value, fill= sex)) +
  geom_bar(position = "dodge", alpha = 0.6) +
  facet_wrap("key", scales = "free") +
  theme_classic()

pormath_subset %>%
  summary

pormath_subset %>%
  group_by(high_use, sex) %>%
  summarise(
    AvgAge = mean(age),
    sdAge = sd(age),
    AvgHealth = mean(as.numeric(health)),
    sdHealth= sd(as.numeric(health)),
    AvgFamRel = mean(as.numeric(famrel)),
    sdFamRel = sd(as.numeric(famrel)),
    AvgFamSup = mean(as.numeric(famsup)),
    sdFamSup = sd(as.numeric(famsup)))

pormath_subset %>%
  ggplot(aes(x = high_use, y = age)) +
  geom_boxplot(fill = "#69b3a2", alpha = 0.4) + 
  theme_classic() +
  scale_color_brewer(palette = "Pastel1")

pormath_subset %>%
  ggplot(aes(x = alc_use, y = age, group = alc_use)) +
  geom_boxplot(fill = "#69b3a2", alpha = 0.4) +
  theme_classic()

pormath_subset %>%
  ggplot(aes(x = high_use, x = famrel)) +
  geom_bar(fill = "#69b3a2", alpha = 0.4) + 
  theme_classic()

pormath_subset %>%
  ggplot(aes(x = alc_use, y = famrel, group = alc_use)) +
  geom_boxplot(fill = "#69b3a2", alpha = 0.4) + 
  theme_classic()

pormath_subset %>%
  ggplot(aes(x = high_use, y = health)) +
  geom_boxplot(fill = "#69b3a2", alpha = 0.4) + 
  theme_classic()

pormath_subset %>%
  ggplot(aes(x = alc_use, y = health, group = alc_use)) +
  geom_boxplot(fill = "#69b3a2", alpha = 0.4) + 
  theme_classic()

pormath_subset %>%
  ggplot(aes(x = high_use, y = G3)) +
  geom_violin(fill = "#69b3a2", width = 1, alpha = 0.5) +
  geom_boxplot(fill = "#69b3a2", width = 0.1, alpha = 0.2) +
  theme_classic()

pormath_subset %>%
  ggplot(aes(x = alc_use, y = G3, group = alc_use)) +
  geom_violin(fill = "#69b3a2", width = 0.7, alpha = 0.5) +
  geom_boxplot(fill = "#69b3a2", width = 0.1, alpha = 0.2) +
  theme_classic()

model1 <- glm(high_use ~ age + famrel + health + G3, 
              data = pormath_subset, 
              family = "binomial")

summary(model1)

model2 <- glm(high_use ~ age + famrel + G3, 
              data = pormath_subset, 
              family = "binomial")

summary(model2)

model3 <- glm(high_use ~ age + famrel + G3 - 1, 
              data = pormath_subset, 
              family = "binomial")

model <- model2

OR <- model %>% 
  coef %>% 
  exp

CI <- model %>% 
  confint %>% 
  exp

m_OR <- cbind(OR, CI)
m_OR

probs <- predict(model, type = "response")

pormath_predict <- pormath_subset %>%
  mutate(probability = probs) %>%
  mutate(prediction = probability > 0.5)

table(high_use = pormath_predict$high_use, 
      prediction = pormath_predict$prediction) %>%
  prop.table %>%
  addmargins()

g <- pormath_predict %>%
  ggplot(aes(x = probability, y = high_use, col = prediction)) + 
  geom_point()

loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}

loss_func(class = pormath_predict$high_use, prob = pormath_predict$probability)

cv <- pormath_predict %>%
  cv.glm(cost = loss_func, glmfit = model, K = 10)

cv$delta[1]




model1 <- glm(high_use ~ school + sex + age + G3, 
              data = pormath_subset, 
              family = "binomial")

summary(model1)



mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")




# second option

pormath <- read.csv("pormath.csv", header = T, sep = ",")

dim(pormath)

str(pormath)

pormath_subset <- pormath %>% 
  select(sex, high_use, alc_use, age, health, famrel, famsup)

pormath_subset_fct <- pormath %>% 
  select(sex, high_use, alc_use, age, health, famrel, famsup) %>%
  mutate(across(c(sex, health, famrel, famsup), factor))

pormath_subset_nmr <- pormath_subset_fct %>%
  mutate(across(c(health, famrel, famsup), as.numeric))

pormath_subset %>%
  select(-sex) %>%
  ggpairs(mapping = aes(col = high_use, alpha = 0.3), 
          lower = list(combo = wrap("facethist", bins = 20)))

pormath_subset %>%
  select(-high_use) %>%
  ggpairs(mapping = aes(col = sex, alpha = 0.3), 
          lower = list(combo = wrap("facethist", bins = 20)))

pormath_subset %>%
  summary

pormath_subset_fct %>%
  summary

pormath_subset_nmr %>%
  group_by(high_use, sex) %>%
  summarise(
    AvgAge = mean(age),
    sdAge = sd(age),
    AvgHealth = mean(health),
    sdHealth= sd(health),
    AvgFamRel = mean(famrel),
    sdFamRel = sd(famrel),
    AvgFamSup = mean(famsup),
    sdFamSup = sd(famsup))

pormath_subset_nmr %>%
  pivot_longer(cols = alc_use:famsup, names_to = "Attribute") %>%
  group_by(high_use, sex, Attribute) %>%
  summarise(Avg = mean(value), 
            se = sd(value)/sqrt(length(value))) %>%
  filter(Attribute != "alc_use") %>%
  ggplot(aes(x = sex, y = Avg, fill = high_use)) +
  geom_bar(position = position_dodge(), 
           stat = "identity", 
           alpha = 0.6) +
  geom_errorbar(aes(x = sex, ymin = Avg-se, ymax = Avg+se),
                position = position_dodge(width = 0.9),
                width = 0.6,
                alpha = 0.6)+
  facet_wrap("Attribute", scales = "free") +
  theme_classic()

g1 <- pormath_subset_nmr %>%
  ggplot(aes(x = high_use, y = age, fill = sex)) +
  geom_boxplot(alpha = 0.4) +
  facet_wrap("sex") +
  theme_classic()

g2 <- pormath_subset_nmr %>%
  ggplot(aes(x = alc_use, y = age, group = alc_use, fill = sex)) +
  geom_boxplot(alpha = 0.4) +
  facet_wrap("sex") +
  theme_classic()

g1 <- pormath_subset_nmr %>%
  ggplot(aes(x = high_use, y = age, fill = sex)) +
  geom_boxplot(alpha = 0.4) +
  facet_wrap("sex") +
  theme_classic()

g2 <- pormath_subset_nmr %>%
  ggplot(aes(x = alc_use, y = age, group = alc_use, fill = sex)) +
  geom_boxplot(alpha = 0.4) +
  facet_wrap("sex") +
  theme_classic()

g1 <- pormath %>%
  ggplot(aes(x = high_use, y = G3, fill = sex)) +
  geom_violin(width = 1, alpha = 0.5) +
  geom_boxplot(position = position_dodge(width =1),
               width = 0.1,
               alpha = 0.2) +
  facet_wrap("sex") +
  theme_classic()

g2 <- pormath %>%
  ggplot(aes(x = alc_use, y = G3, group = alc_use, fill = sex)) +
  geom_violin( width = 0.5, alpha = 0.5) +
  geom_boxplot( width = 0.1, alpha = 0.2) +
  facet_wrap("sex") +
  theme_classic()

g1 <- pormath_subset_fct %>%
  group_by(sex, famsup, high_use) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = famsup, y = count, fill = high_use )) +
    geom_bar(position = "dodge",stat = "identity",alpha = 0.4) +
    facet_wrap("sex") +
    theme_classic()

g2 <- pormath_subset_fct %>%
  group_by(sex, famsup) %>%
  summarise( se = sd(alc_use)/sqrt(length(alc_use)),
             alc_use = mean(alc_use)) %>%
  ggplot(aes(x = famsup, y = alc_use, fill = sex)) +
  geom_bar(position = "dodge",stat = "identity",alpha = 0.4) +
  geom_errorbar(aes(x = famsup, ymin = alc_use-se, ymax = alc_use+se),
                position = position_dodge(width = 0.9),
                width = 0.6,
                alpha = 0.6) +
  facet_wrap("sex") +
  theme_classic()

model1 <- glm(high_use ~ age + health + famrel + famsup, 
              data = pormath_subset, 
              family = "binomial")

summary(model1)

model2 <- glm(high_use ~ age + health + famrel + famsup - 1, 
              data = pormath_subset, 
              family = "binomial")

summary(model2)

model3 <- glm(high_use ~ age + health + famrel, 
              data = pormath_subset, 
              family = "binomial")

summary(model3)

anova(model1, model3, test = "LRT")

model4 <- glm(high_use ~ age + famrel, 
              data = pormath_subset, 
              family = "binomial")

summary(model4)

anova(model3, model4, test = "LRT")

model <- model3

OR <- model %>% 
  coef %>% 
  exp

CI <- model %>% 
  confint %>% 
  exp

m_OR <- cbind(OR, CI)
m_OR

probs <- predict(model, type = "response")

pormath_predict <- pormath_subset %>%
  mutate(probability = probs) %>%
  mutate(prediction = probability > 0.5)

table(high_use = pormath_predict$high_use, 
      prediction = pormath_predict$prediction) %>%
  prop.table %>%
  addmargins()

pormath_predict %>%
  ggplot(aes(x = probability, y = high_use, col = prediction)) + 
  geom_point()

loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}

loss_func(class = pormath_predict$high_use, prob = pormath_predict$probability)

cv <- pormath_predict %>%
  cv.glm(cost = loss_func, glmfit = model, K = 10)

cv$delta[1]
