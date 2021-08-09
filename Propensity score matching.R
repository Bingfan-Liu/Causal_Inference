
#We will use data from Lalonde (1986), that aimed to evaluate the impact of  National Supported Work (NSW) Demonstration, which is a labor training program, on post-intervention income levels. Interest is in estimating the causal effect of this training program on income.


# load the packages tableone and Matching:
#install.packages("tableone")
#install.packages("Matching")
#install.packages("MatchIt")

library(tableone)
library(Matching)
library(MatchIt)

# load the lalonde data (which is in the MatchIt package):
data(lalonde)
head(lalonde)


#The data have n=614 subjects and 10 variables:

#  age age in years. 

#educ years of schooling. 

#black indicator variable for blacks. 

#hispan indicator variable for Hispanics. 

#married indicator variable for marital
#status. 

#nodegree indicator variable for high school
#diploma. 

#re74 real earnings in 1974. 

#re75 real earnings in 1975. 

#re78 real earnings in 1978. 

#treat an indicator variable for treatment
#status.

#The outcome is
#re78 – post-intervention income.

#The treatment is
#treat – which is equal to 1 if the subject received the labor training and equal
#to 0 otherwise.

#The potential confounding
#variables are: age, educ, black, hispan, married, nodegree, re74, re75.


### Task 1 Standardized differences

#Find the standardized differences for all of the
#confounding variables (pre-matching). What is the standardized difference for married (to nearest hundredth)?    

xvars <- colnames(lalonde)
xvars <- xvars[xvars!='treat']
table1<- CreateTableOne(vars=xvars,strata="treat", data=lalonde, test=FALSE) 
print(table1,smd=TRUE) 



### Task 2 Overall earning difference

#Calculate the raw (unadjusted) mean of real earnings in 1978 for treated subjects minus the mean of real earnings in 1978 for untreated subjects.    

treated_mean <- mean(lalonde[lalonde$treat==1,]$re78)
untreated_mean <- mean(lalonde[lalonde$treat==0,]$re78)
print(treated_mean-untreated_mean)




# Task 3: propensity score estimation

#Find the minimum and maximum values of the estimated propensity score

#First, fit a propensity score model. Use a logistic regression model, where the outcome is treatment. Include the 8 confounding variables in the model as predictors, with no interaction terms or non-linear terms (such as squared terms). Obtain the propensity score for each subject.


psmodel<-glm(treat~age+educ+race+married+nodegree+re74+re75,
             family=binomial(),data=lalonde) 

#show coefficients etc 
summary(psmodel) 

#create propensity score 
pscore<-psmodel$fitted.values 

# max and min of psc
summary(pscore)





### Task 4: Propensity score macthing


#Now carry out propensity score matching using the Match function. 

#Setting the seed will ensure that you end up with a matched data set that is the same as the one used to create the solutions.

#Use options to specify pair matching, without replacement, no caliper. 
#Match on the propensity score itself, not logit of the propensity score. Obtain the standardized differences for the matched data.

set.seed(931139)

psmatch<-Match(Tr=lalonde$treat,M=1,X=pscore,replace=FALSE) 

matched<-lalonde[unlist(psmatch[c("index.treated","index.control")]), ]

#get standardized differences 

matchedtab1<-CreateTableOne(vars=xvars, strata ="treat", 
                            data=matched, test = FALSE) 

print(matchedtab1, smd = TRUE) 




### Task 5: Redo matching using caliper


set.seed(931139)
psmatch<-Match(Tr=lalonde$treat,M=1,X=pscore,replace=FALSE, caliper = 0.1) 
matched<-lalonde[unlist(psmatch[c("index.treated","index.control")]), ]

#get standardized differences 

matchedtab2<-CreateTableOne(vars=xvars, strata ="treat", 
                            data=matched, test = FALSE) 

print(matchedtab2, smd = TRUE) 



### Task 6: 

#Use the matched data set (from propensity score matching with caliper=0.1) to carry out the outcome analysis. 

#For the matched data, what is the mean of real earnings in 1978 for treated subjects minus the mean of real earnings in 1978 for untreated subjects? 

treated_mean2 <- mean(matched[matched$treat==1,]$re78)
untreated_mean2 <- mean(matched[matched$treat==0,]$re78)
print(treated_mean2-untreated_mean2)


#### Task 7:

#Carry out a paired t-test for the effect of treatment on earnings. What are the values of the 95% confidence interval?

y_trt<-matched$re78[matched$treat==1] 

y_con<-matched$re78[matched$treat==0] 

#pairwise difference 
diffy<-y_trt-y_con 

#paired t-test 
t.test(diffy) 
