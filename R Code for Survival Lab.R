##Install the 'survival package'
install.packages("survival")

##Load the package
library(survival)

###Load data into R dataset called AIDS 
AIDS=read.table("S:/course/SIBS/data sets/AIDS.DAT",col.names = c("Surivival(in months)",
                                                                  "Censored","AgeGroup"))

##Use the summary function to get an idea of what is contained in this dataset
summary(AIDS)

##Attach dataset 
attach(AIDS)

##Examine the distribution of the survival time
hist(Surivial.in.months.)

##Examine counts of censored obs and age groups
table(Censored)
table(AgeGroup)

##In order to use the package we need to create a variable that incorporates 
##time and censoring
Surv.time=Surv(Surivival.in.months.,Censored)

###Construct Kaplan-Meier Plots  
### The first item is a list and contains 14 outputs from the model                              
Curve=survfit(Surv.time~AgeGroup)  #survfit function calculates values for curve
plot(Curve,col = c(2,3))           #then plot function makes the graph

##Examine some of the model output
summary(Curve)   #Shows the risk of survival within each group and particular times
quantile(Curve)  ##Shows the quantiles along with a confidence interval for each

##Conduct statistical test of difference in KM curves
survdiff(Surv.time~AgeGroup)

#Install package to ease import of Excel files
install.packages("readxl")

##Load package to ease import of Excel files
library(readxl)

###Load aids2 dataset
AIDS2=read_xlsx("S:/course/SIBS/data sets/aids2.xlsx",sheetIndex = 1)

##Fit a Cox Proportional Hazard Model with new dataset which includes gender variable
Model1=coxph(Surv.time~AgeGroup+gender, data=AIDS2)


#obtain Shcoenfeld residuals for the PH model
SchResids=residuals(Model1,type="schoenfeld")

##Create a dataset that contains Schoenfeld residuals, survival times, and the ranks of those times
ranked=cbind(Surivival.in.months.[which(Censored==1)],SchResids,
             rank(Surivival.in.months.[which(Censored==1)]))

##Test whether Ranks of survival times are corrrelated with Schoenfeld residuals
cor.test(ranked[,2],ranked[,4])
cor.test(ranked[,3],ranked[,4])
