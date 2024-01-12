rm(list = ls())

#1. Import the data ====
setwd("D:/vu/terms/term 7/Probability and Statistics in Decision Modeling-I/End_Course_Hackathon")

leads_data= read.csv("Leads_cleaned", header = TRUE, sep= ",")

leads_data_original= leads_data
leads_data_original$X= NULL

#2. Understanding & Pre processing the data ====
View(leads_data)

#2a. Dimensions, structure & summary of data
dim(leads_data)
str(leads_data)
summary(leads_data)

#2b. Dealing with missing values
#check for number of 'NA' in data frame
sum(is.na(leads_data))

#3. Data Cleaning  ====

#Dropping the unwanted columns
leads_data$X= NULL
leads_data$Prospect.ID= NULL
leads_data$I.agree.to.pay.the.amount.through.cheque= NULL
leads_data$Get.updates.on.DM.Content= NULL
leads_data$Update.me.on.Supply.Chain.Content= NULL
leads_data$Receive.More.Updates.About.Our.Courses= NULL
leads_data$Do.Not.Call= NULL
leads_data$Lead.Number= NULL
leads_data$Magazine= NULL
leads_data$X.Education.Forums= NULL
leads_data$Newspaper= NULL
leads_data$Digital.Advertisement= NULL 
leads_data$Through.Recommendations= NULL
leads_data$What.matters.most.to.you.in.choosing.a.course= NULL 
leads_data$Search= NULL
leads_data$Newspaper.Article= NULL

#Subseting Numeric and Categorical columns to understand them better

#Numeric
leads_data_num_cols= c("TotalVisits" ,"Total.Time.Spent.on.Website","Page.Views.Per.Visit")
leads_data_num= subset(leads_data, select= (leads_data_num_cols))
str(leads_data_num)
summary(leads_data_num)

#Categorical
leads_data_cat= subset(leads_data, select = - c(TotalVisits ,Total.Time.Spent.on.Website,Page.Views.Per.Visit))
str(leads_data_cat)

leads_data_cat= as.data.frame(lapply(leads_data_cat, as.factor))
summary(leads_data_cat)

table(leads_data_cat$Lead.Source)
table(leads_data_cat$Last.Activity)
table(leads_data_cat$Country)
table(leads_data_cat$Specialization)
table(leads_data_cat$What.is.your.current.occupation)  
table(leads_data_cat$Tags)
table(leads_data_cat$City)
table(leads_data_cat$Last.Notable.Activity)


for (f in 1:nrow(leads_data)){
  
  #Lead Source (7)
  if((leads_data$Lead.Source[f]==c("bing"))
  | (leads_data$Lead.Source[f]==c("blog") )
  | (leads_data$Lead.Source[f]==c("Click2call") )
  | (leads_data$Lead.Source[f]==c("Live Chat") )
  | (leads_data$Lead.Source[f]==c("NC_EDM") )
  | (leads_data$Lead.Source[f]==c("Pay per Click Ads") )
  | (leads_data$Lead.Source[f]==c("Press_Release") )
  | (leads_data$Lead.Source[f]==c("Social Media") )
  | (leads_data$Lead.Source[f]==c("testone") )
  | (leads_data$Lead.Source[f]==c("WeLearn") )
  | (leads_data$Lead.Source[f]==c("welearnblog_Home") )
  | (leads_data$Lead.Source[f]==c("youtubechannel") )
  | (leads_data$Lead.Source[f]==c("Facebook") )
  ){ 
    leads_data$Lead.Source[f]= "Others"
  }
  if((leads_data$Lead.Source[f]==c("google"))
    |(leads_data$Lead.Source[f]==c("Google"))
    ){ 
    leads_data$Lead.Source[f]= "Google"
  }
  if((leads_data$Lead.Source[f]==c("Referral Sites"))
    |(leads_data$Lead.Source[f]==c("Welingak Website"))
    ){ 
    leads_data$Lead.Source[f]= "Referral Sites"
  }
}

for (f in 1:nrow(leads_data)){
  #Last Activity (8)
  if((leads_data$Last.Activity[f]==c("Approached upfront")) 
     | (leads_data$Last.Activity[f]==c("Email Marked Spam"))
     | (leads_data$Last.Activity[f]==c("Email Received"))
     | (leads_data$Last.Activity[f]==c("Had a Phone Conversation"))
     | (leads_data$Last.Activity[f]==c("Resubscribed to emails"))
     | (leads_data$Last.Activity[f]==c("Unsubscribed"))
     | (leads_data$Last.Activity[f]==c("View in browser link Clicked"))
     | (leads_data$Last.Activity[f]==c("Visited Booth in Tradeshow"))
     | (leads_data$Last.Activity[f]==c("Unreachable"))
     | (leads_data$Last.Activity[f]==c("Form Submitted on Website"))
     ){ 
     leads_data$Last.Activity[f]= "Others"
  }
}

for (f in 1:nrow(leads_data)){
  #Country (2)
  if(leads_data$Country[f]!=c("India") ){ 
    leads_data$Country[f]= "Other Countries"
  }
}
  
for (f in 1:nrow(leads_data)){ 
 #Specialization (11)
  if((leads_data$Specialization[f]==c("Finance Management"))
    | (leads_data$Specialization[f]==c("Healthcare Management"))
    | (leads_data$Specialization[f]==c("Hospitality Management"))  
    | (leads_data$Specialization[f]==c("Human Resource Management"))  
    | (leads_data$Specialization[f]==c("IT Projects Management"))  
    | (leads_data$Specialization[f]==c("Marketing Management"))
    | (leads_data$Specialization[f]==c("Operations Management"))
    | (leads_data$Specialization[f]==c("Retail Management"))
    | (leads_data$Specialization[f]==c("Supply Chain Management"))
    ){ 
     leads_data$Specialization[f]= "Management"
    }
}

for (f in 1:nrow(leads_data)){
  #What.is.your.current.occupation (4)
  if((leads_data$What.is.your.current.occupation[f]==c("Businessman"))
  | (leads_data$What.is.your.current.occupation[f]==c("Housewife"))
  | (leads_data$What.is.your.current.occupation[f]==c("Other"))
  | (leads_data$What.is.your.current.occupation[f]==c("Student"))
  ){ 
    leads_data$What.is.your.current.occupation[f]= "Other"
  }

}

for (f in 1:nrow(leads_data)){
  #Tags (5) 
  if((leads_data$Tags[f]==c("Diploma holder (Not Eligible)"))
     | (leads_data$Tags[f]==c("in touch with EINS") )
     | (leads_data$Tags[f]==c("invalid number") )
     | (leads_data$Tags[f]==c("Lost to Others") )
     | (leads_data$Tags[f]==c("number not provided") )
     | (leads_data$Tags[f]==c("opp hangup") )
     | (leads_data$Tags[f]==c("Recognition issue (DEC approval)") )
     | (leads_data$Tags[f]==c("University not recognized") )
     | (leads_data$Tags[f]==c("wrong number given") )
     | (leads_data$Tags[f]==c("Closed by Horizzon") )
     | (leads_data$Tags[f]==c("Lost to EINS") )
     | (leads_data$Tags[f]==c("switched off") )
  ){ 
    leads_data$Tags[f]= "Unreachable"
  }
  if((leads_data$Tags[f]==c("In confusion whether part time or DLP"))
     |(leads_data$Tags[f]==c("Shall take in the next coming month"))
     |(leads_data$Tags[f]==c("Still Thinking"))
     |(leads_data$Tags[f]==c("Want to take admission but has financial problems"))
  ){ 
    leads_data$Tags[f]= "NotSure"
  }
  if((leads_data$Tags[f]==c("Already a student"))
     |(leads_data$Tags[f]==c("Busy"))
     |(leads_data$Tags[f]==c("Graduation in progress"))
     |(leads_data$Tags[f]==c("Interested  in full time MBA"))
     |(leads_data$Tags[f]==c("Interested in other courses"))
     |(leads_data$Tags[f]==c("Not doing further education"))
     |(leads_data$Tags[f]==c("Interested in Next batch"))
     |(leads_data$Tags[f]==c("Lateral student"))
  ){ 
    leads_data$Tags[f]= "OtherIntrests"
  }
}

for (f in 1:nrow(leads_data)){
  #City (3)
  if((leads_data$City[f]==c("Other Cities of Maharashtra"))
     | (leads_data$City[f]==c("Thane & Outskirts"))
  ){ 
    leads_data$City[f]= "Other Cities of Maharashtra"
  }
  if((leads_data$City[f]==c("Other Cities"))
     |(leads_data$City[f]==c("Tier II Cities"))
     |(leads_data$City[f]==c("Other Metro Cities"))
  ){ 
    leads_data$City[f]= "Other Cities"
  }
}

for (f in 1:nrow(leads_data)){
  #Last.Notable.Activity (4)
  if((leads_data$Last.Notable.Activity[f]==c("Approached upfront"))
     | (leads_data$Last.Notable.Activity[f]==c("Email Marked Spam"))
     | (leads_data$Last.Notable.Activity[f]==c("Email Received"))
     | (leads_data$Last.Notable.Activity[f]==c("Form Submitted on Website"))
     | (leads_data$Last.Notable.Activity[f]==c("Had a Phone Conversation"))
     | (leads_data$Last.Notable.Activity[f]==c("Resubscribed to emails"))
     | (leads_data$Last.Notable.Activity[f]==c("Unreachable"))
     | (leads_data$Last.Notable.Activity[f]==c("Unsubscribed"))
     | (leads_data$Last.Notable.Activity[f]==c("View in browser link Clicked"))
     | (leads_data$Last.Notable.Activity[f]==c("Email Bounced"))
     | (leads_data$Last.Notable.Activity[f]==c("Email Link Clicked"))
     | (leads_data$Last.Notable.Activity[f]==c("Page Visited on Website"))
     | (leads_data$Last.Notable.Activity[f]==c("Olark Chat Conversation"))
  ){ 
    leads_data$Last.Notable.Activity[f]= "Others"
  }
}
 

#Converting all the factor variables to categorical
convert_tocateg = function(data, cols){
  for (col in cols){
    data[,col] = as.factor(data[,col])
  }
  return (data)
}

leads_data = convert_tocateg(leads_data, c("Lead.Source","Last.Activity",
                                          "Country","Specialization", 
                                          "What.is.your.current.occupation","Tags",
                                          "City", "Last.Notable.Activity",
                                          "Lead.Origin","Do.Not.Email",
                                          "Converted","Lead.Quality",
                                          "A.free.copy.of.Mastering.The.Interview"))

str(leads_data)
summary(leads_data)


#4. Split the data into train validation and test ====

library(caret)
set.seed(1111)

data= leads_data
attach(data)

# Finding the train rows & subset from data
train_rows = createDataPartition(Converted, p = 0.7, list = F)
train_data = data[train_rows, ]

# Find the validation rows & subset from the data
validationdata = data[-train_rows, ]

dim(data) #dimensions of original data
dim(train_data)[1] + dim(validationdata)[1]  #sum of rows in train,  & validation


#5. Build logistic regression model & find the predictions ====
log_reg = glm(Converted ~ ., data = train_data, family = "binomial")
summary(log_reg)
plot(log_reg$fitted.values)
#plot(log_reg$fitted.values, 1 / (1+exp(-log_reg$fitted.values)))
log_reg$fitted.values[1]

#Finding beta values
train_beta = predict(log_reg,train_data)
train_beta[1]
train_beta
plot(train_beta, 1 / (1+exp(-train_beta)))

#Finding probabilities
train_probabilities = predict(log_reg,train_data,type = "response")
train_probabilities[1]

#Finding predictions
#summary(data1$Converted)
levels(data$Converted) #understand the order of levels

train_prediction = ifelse((train_probabilities<0.5), "0", "1") #threshold chosen through trial and error
table(train_prediction)
table(train_data$Converted)

#Validation predictions
val_probabilities = predict(log_reg,validationdata,type = "response")
val_prediction = ifelse((val_probabilities<0.5), "0", "1")
table(val_prediction)
table(validationdata$Converted)

#6. Building confusion matrix ====
con_mat = table(train_prediction,train_actual = train_data$Converted)
val_con_mat = table(val_prediction,val_actual = validationdata$Converted)

# Finding metrics using inbuilt function
#library(caret) #req function available in caret package
confusionMatrix(con_mat)
confusionMatrix(con_mat, positive = "1")

# Finding validation metrics
confusionMatrix(val_con_mat, positive = "1")

#7. Identifying colinear variables using 'VIF' & important variables using 'Step-AIC' ====

# 'VIF' for colinear variables
library(car)

vif(log_reg)

# 'Step-AIC' for important variables

library(MASS)
m = stepAIC(log_reg) #using Step-AIC on logistic reg model
m$call #get the syntax for best variable model
summary(eval(m$call))
# Building logistic regression using variables from Step-AIC
log_reg_step_aic = glm(formula = Converted ~ Lead.Origin + Lead.Source + Do.Not.Email + 
                         TotalVisits + Total.Time.Spent.on.Website + Page.Views.Per.Visit + 
                         Last.Activity + Country + What.is.your.current.occupation + 
                         Tags + Lead.Quality + Last.Notable.Activity, family = "binomial", 
                       data = train_data)

summary(log_reg_step_aic)

#8. Finding train and validation pobabilities for the log+step-AIC model ====

# Finding probabilities
step_train_probailities = predict(log_reg_step_aic,train_data,type = "response")
step_val_probailities = predict(log_reg_step_aic,validationdata,type = "response")

# Finding predictions
step_train_predictions = ifelse((step_train_probailities<0.5), "0", "1")
step_val_predictions = ifelse((step_val_probailities<0.5), "0", "1")

# Finding confusion matrices
step_con_mat = table(step_train_predictions,train_actual = train_data$Converted)
step_val_con_mat = table(step_val_predictions,val_actual = validationdata$Converted)

# Finding metrics
confusionMatrix(step_con_mat,positive = "1")
confusionMatrix(step_val_con_mat,positive = "1")

#9. Using ROCR curves to find the best cut-off value ====

library(ROCR)

#Creating prediction object for ROCR
rocpreds = prediction(step_train_probailities, train_data$Converted)


# Extract performance measures (True Positive Rate and False Positive Rate) using the "performance()" function from the ROCR package
# The performance() function from the ROCR package helps us extract metrics such as True positive rate, False positive rate etc. from the prediction object, we created above.
# Two measures (y-axis = tpr, x-axis = fpr) are extracted
perf = performance(rocpreds, measure="tpr", x.measure="fpr")
slotNames(perf)

perf
# Plot the ROC curve using the extracted performance measures (TPR and FPR)
plot(perf, col = rainbow(10), colorize=T, print.cutoffs.at=seq(0,1,0.05))

# Extract the AUC score of the ROC curve and store it in a variable named "auc"
# Use the performance() function on the prediction object created above using the ROCR package, to extract the AUC score
perf_auc = performance(rocpreds,  measure="auc")

# Access the auc score from the performance object
auc = perf_auc@y.values[[1]]
auc

# For different threshold values identifying the tpr and fpr
cutoffs = data.frame(cut= perf@alpha.values[[1]], fpr= perf@x.values[[1]], 
                     tpr=perf@y.values[[1]])

# Sorting the data frame in the decreasing order based on tpr
cutoffs = cutoffs[order(cutoffs$tpr, cutoffs$fpr, decreasing=TRUE),]
head(cutoffs)
class(perf)

# Plotting the true positive rate and false negative rate based on the cutoff       
# increasing from 0.05-0.1
plot(perf, colorize = TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

## Choose a Cutoff Value



#10. Using best cutoff value, find new predictions & new metrics ====
# Finding predictions
new_step_train_predictions = ifelse((step_train_probailities<0.4), "0", "1")
new_step_val_predictions = ifelse((step_val_probailities<0.4), "0", "1")

# Finding confusion matrices
new_step_con_mat = table(new_step_train_predictions,train_actual = train_data$Converted)
new_step_val_con_mat = table(new_step_val_predictions,val_actual = validationdata$Converted)

# Finding metrics
confusionMatrix(new_step_con_mat,positive = "1")
confusionMatrix(new_step_val_con_mat,positive = "1")


#11. To get scores for the model ====

# Building logistic regression using variables from Step-AIC on the entire data
log_reg_step_aic_data = glm(formula = Converted ~ Lead.Origin + Lead.Source + Do.Not.Email + 
                         TotalVisits + Total.Time.Spent.on.Website + Page.Views.Per.Visit + 
                         Last.Activity + Country + What.is.your.current.occupation + 
                         Tags + Lead.Quality + Last.Notable.Activity, family = "binomial", 
                       data = data)

# Finding probabilities
step_data_probailities = predict(log_reg_step_aic_data,data,type = "response")

# Assigning scores 
#Lead Score= Probability of a person becoming ta customer of X Education*100
scores= round(step_data_probailities*100)

scores_data= leads_data_original
scores_data$scores= scores
View(scores_data)

View(scores_data[c("scores", "Converted")])

#The cut off we choose is 0.4 i.e. a Customer with a score greater than 40
#will be classified as a hot lead.

# Further, people with higher scores should be focused on more, 
#as chances of conversion for them are higher.
# Covering people with scores of 40 and more will make them 
#convert Customers almost 86% of the times.



############################################################################################
#12. Building  a model with naive Bayes ====
library("e1071")
library(caret)
model_nb<-naiveBayes(train_data$Converted~.,train_data)

# Response of the model
model_nb
# It lists all likelihoods for attributes -frequency tables
model_nb$tables
# Check for one variable Lead.Origin
model_nb$tables$Lead.Origin
# sanity check manual calculation of frequency table

# Prior Probabilities
# Conditional probabilities
a<-table(train_data$Lead.Origin,train_data$Converted)
a[,1]/table(train_data$Converted)[1]
a[,2]/table(train_data$Converted)[2]

# Predict the Converted("1"/"0") value on the train data 
train_preds<-predict(model_nb,train_data)
cnf<-table(train_data$Converted,train_preds)
acc<-sum(diag(cnf))/nrow(train_data)
print(acc)

# recall
cnf[2,2]/sum(cnf[2,])


#prediction on validation data
preds<-predict(model_nb,validationdata)
cnf<-table(validationdata$Converted,preds)
acc<-sum(diag(cnf))/nrow(validationdata)
print(acc)
# recall
cnf[2,2]/sum(cnf[2,])


#Alternate way of calculating confusion matrix
#Confusion Matrix
library(caret)
confusionMatrix(train_data$Converted,train_preds, positive="1")
confusionMatrix(validationdata$Converted,preds, positive="1")

#13. To get scores for the Naive Bayes model ====
library(naivebayes)
nb_model<-naive_bayes(data$Converted~.,data)

summary(nb_model)
# It lists all likelihoods for attributes -frequency tables
nb_model$tables

#Finding probabilities
p= predict(nb_model, data, type="prob")
head(cbind(round(p*100),data))

prob= round(p*100)

nb_scores_data= cbind(round(p*100),leads_data_original)
View(nb_scores_data)

View(nb_scores_data[c("0","1", "Converted")])

#After calculating the scores for 0 (not converted) and 1(converted) respectively,
# the level with a higher score is what the naive bayes model predicts.

#It works on a comparison basis using the independent probabilities of each occurrence 
#contributing to the target variable.



