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


## rats and pbrs sandbox

library(lme4)
library(dplyr)
library(ggplot2)
library(broom)

rats <- read.csv("rats_long.csv") %>%
  mutate(across(c(ID, Group), factor))

bprs <- read.csv("bprs_long.csv") %>%
  mutate(across(c(treatment, subject), factor))

str(rats)
summary(rats)

rats %>%
  ggplot(aes(x = Time, y = Weight, group = ID)) +
  geom_line(aes(col = Group)) +
  scale_x_continuous(name = "Time (days)", breaks = seq(0, 60, 10)) +
  scale_y_continuous(name = "Weight (grams)") +
  ggtitle("Rat's weight over time") +
  theme_bw()

rats <- rats %>%
  group_by(Time) %>%
  mutate(stdweight = (Weight - mean(Weight))/sd(Weight)) %>%
  ungroup()

rats %>%
  ggplot(aes(x = Time, y = stdweight, group = ID)) +
  geom_line(aes(col = Group)) +
  scale_x_continuous(name = "Time (days)", breaks = seq(0, 60, 10)) +
  scale_y_continuous(name = "Weight (grams)") +
  ggtitle("Rat's weight over time") +
  theme_bw()

ratsTS <- rats %>%
  group_by(Group, Time) %>%
  summarise(mean = mean(Weight), se = sd(Weight)/sqrt(n()) ) %>%
  ungroup()

glimpse(ratsTS)

ratsTS %>%
  ggplot(aes(x = Time, y = mean, col = Group, shape = Group)) +
  geom_line() +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width=0.5) +
  scale_y_continuous(name = "mean(bprs) +/- se(bprs)") +
  theme_bw()

rats_RegS  <- rats %>% 
  group_by(Group, ID) %>% 
  do(regTimeCoeff = tidy(lm(Weight ~ Time, data = .))$estimate[2]) %>%
  mutate(regTimeCoeff = unlist(regTimeCoeff))

glimpse(rats_RegS)

rats_RegS %>%
  ggplot(aes(x = Group, y = regTimeCoeff)) +
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape=23, size=2, fill = "red") +
  scale_y_continuous(name = "mean(Weight), days 1 to 64") +
  theme_bw()


ratsGS <- rats %>%
  filter(Time > 1) %>%
  group_by(Group, ID) %>%
  summarise(mean = mean(Weight)) %>%
  ungroup()

glimpse(ratsGS)

ratsGS %>%
  ggplot(aes(x = Group, y = mean)) +
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape=23, size=2, fill = "red") +
  scale_y_continuous(name = "mean(Weight), days 1 to 64") +
  theme_bw()

ratsGS1 <- ratsGS %>%
  filter((Group == 1 & mean > 250) | (Group == 2 & mean < 550) | (Group == 3 & mean > 500))

outlierIDs <- ratsGS %>%
  mutate(outlier = !((Group == 1 & mean > 250) | (Group == 2 & mean < 550) | (Group == 3 & mean > 500))) %>%
  filter(outlier) %>%
  .$ID

ratsGS1 %>%
  ggplot(aes(x = Group, y = mean)) +
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape=23, size=2, fill = "red") +
  scale_y_continuous(name = "mean(Weight), days 1 to 64") +
  theme_bw()

ratsnoO <- rats %>%
  filter(!(ID %in% outlierIDs))

ratsTS1 <- ratsnoO %>%
  group_by(Group, Time) %>%
  summarise(mean = mean(Weight), se = sd(Weight)/sqrt(n())) %>%
  ungroup()

glimpse(ratsTS1)

ratsTS1 %>%
  ggplot(aes(x = Time, y = mean, col = Group, shape = Group)) +
  geom_line() +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width=0.5) +
  scale_y_continuous(name = "mean(bprs) +/- se(bprs)") +
  theme_bw()

groups <- c(3,2,1)
stats <- sapply(groups, function(g){
  test <- t.test(mean ~ Group, data = filter(ratsGS, Group != g), var.equal = TRUE)
  return(c(test$p.val, unlist(test$conf.int))) 
})
colnames(stats) <- c("G 1 and 2", "G 1 and 3", "G 2 and 3")
rownames(stats) <- c("pval", "conf_int_low", "conf_int_high")
stats

baseline <- rats %>% 
  filter(Time == 1) %>%
  .$Weight

ratsGS2 <- ratsGS %>%
  mutate(baseline = baseline)

fit <- lm(mean ~ baseline + Group, data = ratsGS2)

anova(fit)


str(bprs)
summary(bprs)

bprs %>%
  ggplot(aes(x = week, y = BPRS)) +
  geom_line(aes(linetype = subject)) +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_wrap("treatment", labeller = label_both) +
  theme_bw() +
  theme(legend.position = "none")

bprs_reg <- lm(BPRS ~ week + treatment, data = bprs)

summary(bprs_reg)

bprs_ref <- lmer(BPRS ~ week + treatment + (1 | subject), data = bprs, REML = F)

summary(bprs_ref)

bprs_ref1 <- lmer(BPRS ~ week + treatment + (week | subject), data = bprs, REML = F)

summary(bprs_ref1)

anova(bprs_ref1,bprs_ref)

bprs_ref2 <- lmer(BPRS ~ week * treatment + (week | subject), data = bprs, REML = F)

summary(bprs_ref2)

anova(bprs_ref2, bprs_ref1)

bprs %>%
  ggplot(aes(x = week, y = BPRS)) +
  geom_line(aes(linetype = subject)) +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_wrap("treatment", labeller = label_both) +
  theme_bw() +
  theme(legend.position = "none")

Fitted <- fitted(bprs_ref2)

bprs <- bprs %>%
  mutate(Fitted)

bprs %>%
  ggplot(aes(x = week, y = Fitted)) +
  geom_line(aes(linetype = subject)) +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_wrap("treatment", labeller = label_both) +
  theme_bw() +
  theme(legend.position = "none")
