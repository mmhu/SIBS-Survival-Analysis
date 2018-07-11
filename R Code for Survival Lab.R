##Install the 'survival package'
# install.packages("survival")

##Load the package
library(survival)

###Load data into R dataset called AIDS 
AIDS <- read.table("AIDS.DAT", col.names = c("Survival in months", "Censored", "AgeGroup"))

##Use the summary function to get an idea of what is contained in this dataset
summary(AIDS)

##Attach dataset 
attach(AIDS)

##Examine the distribution of the survival time
hist(Survival.in.months)

##Examine counts of censored obs and age groups
table(Censored)
table(AgeGroup)

##In order to use the package we need to create a variable that incorporates 
##time and censoring
surv.time <- Surv(AIDS$Survival.in.months, AIDS$Censored)

###Construct Kaplan-Meier Plots  
### The first item is a list and contains 14 outputs from the model                              
curve <- survfit(surv.time ~ AIDS$AgeGroup)  #survfit function calculates values for curve
plot(curve, col = c(2,3))           #then plot function makes the graph

##Examine some of the model output
summary(curve)   #Shows the risk of survival within each group and particular times
quantile(curve)  ##Shows the quantiles along with a confidence interval for each

##Conduct statistical test of difference in KM curves
survdiff(surv.time ~ AIDS$AgeGroup)

#Install package to ease import of Excel files
# install.packages("readxl")

##Load package to ease import of Excel files
library(readxl)

###Load aids2 dataset
AIDS2 <- read_xlsx("aids2.xlsx", sheet = 1)

##Fit a Cox Proportional Hazard Model with new dataset which includes gender variable
model1 <- coxph(surv.time ~ agecat + gender, data = AIDS2)

#obtain Shcoenfeld residuals for the PH model
sch.resids <- residuals(model1, type = "schoenfeld")

##Create a dataset that contains Schoenfeld residuals, survival times, and the ranks of those times
ranked <- cbind(AIDS$Survival.in.months[which(AIDS$Censored == 1)], sch.resids, 
                rank(AIDS$Survival.in.months[which(AIDS$Censored == 1)]))

##Test whether Ranks of survival times are corrrelated with Schoenfeld residuals
cor.test(ranked[, 2], ranked[, 4])
cor.test(ranked[, 3], ranked[, 4])
