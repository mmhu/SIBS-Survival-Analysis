library(survival)
library(readxl)
library(tidyverse)

dial <- read_xlsx("dialysis2.xlsx", sheet = 1)

# infection is our "censored" var
# 1 indicates infection, 0 indicates no infection
# doesn't matter which is which

# dial <- dial %>% mutate(censored = ifelse(dial$infection == 0, 1, 0))

surv <- Surv(dial$time, dial$infection)

curve.gender <- survfit(surv ~ dial$gender)
plot(curve.gender, col = c(2, 3))
quantile(curve.gender) # look at median time to infection by gender
survdiff(surv ~ dial$gender) # test for difference of KM curves by gender

curve.dis <- survfit(surv ~ dial$dis_type)
plot(curve.dis, col = c(2, 3))
quantile(curve.dis)
survdiff(surv ~ dial$dis_type)

# Ensure that male and glomerulonephritis are the reference groups:
# dial$gender <- relevel(dial$gender, ref = 0)
# dial$dis_type <- relevel(dial$dis_type, ref = 1)

# The full proportional hazards regression model:
full <- coxph(surv ~ age + gender + dis_type, data = dial)

sch.resids <- residuals(full, type = "schoenfeld")

ranked <- cbind(dial$time[which(dial$infection == 1)], sch.resids,
                rank(dial$time[which(dial$infection == 1)]))

cor.test(ranked[, 2], ranked[, 5]) # age resids vs. ranked survival times
cor.test(ranked[, 3], ranked[, 5]) # gender resids vs. ranked survival times
cor.test(ranked[, 4], ranked[, 5]) # dis_type resids vs. ranked survival times

# Remove the variable with the largest p-value:
reduced <- coxph(surv ~ gender + dis_type, data = dial)
summary(reduced)

red.sch.resids <- residuals(reduced, type = "schoenfeld")

red.ranked <- cbind(dial$time[which(dial$infection == 1)], red.sch.resids,
                rank(dial$time[which(dial$infection == 1)]))
