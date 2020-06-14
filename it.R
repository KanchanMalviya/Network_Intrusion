
library(corrplot)
library(ggplot2)
library(caret)
set.seed(3690)
path1="F:\\project1\\IT Risk Management\\NetworkIntrusionTrainData.csv"
ittrain=read.csv((path1))
head(ittrain)
tail((ittrain))
View(ittrain)
colnames(ittrain)
nrow(ittrain)
ncol(ittrain)

####check for singularity
table(ittrain$land)
ittrain$land=NULL

table(ittrain$wrong_fragment)
ittrain$wrong_fragment=NULL

table(ittrain$urgent)
ittrain$urgent=NULL

table(ittrain$num_failed_logins)
ittrain$num_failed_logins=NULL

table(ittrain$root_shell)
ittrain$root_shell=NULL

table(ittrain$num_access_files)
ittrain$num_access_files=NULL

table(ittrain$num_outbound_cmds)
ittrain$num_outbound_cmds=NULL

table(ittrain$is_host_login)
ittrain$is_host_login=NULL

table(ittrain$is_guest_login)
ittrain$is_guest_login=NULL


#y var is class

ittrain$class=as.character(ittrain$class)
str(ittrain)
ittrain$class[ittrain$class=="normal"]=0
ittrain$class[ittrain$class=="anomaly"]=1
ittrain$class=as.factor(ittrain$class)



#num and fact
str(ittrain)
facts = names(ittrain)[sapply(ittrain,is.factor)]
nums = names(ittrain)[sapply(ittrain,is.numeric)]

print(facts)
print(nums)

num_data = ittrain[nums]
fact_data=ittrain[facts]


# EDA on numeric data
# 1) 0, outlier and missing value check
# 2) multicollinearity
# check for Nulls
# check for Nulls
col_name=colnames(num_data) [apply(num_data, 2, function(n) any(is.na(n)))]
print(col_name)
if(length(col_name) > 0) print("NULLs present") else print("No NULLs")

# check for Blanks
col_name = colnames(num_data) [apply(num_data, 2, function(n) any(n == ""))]
if(length(col_name) > 0) print("Blanks present") else print("No Blanks")

# check for 0
col_name = colnames(num_data) [apply(num_data, 2, function(n) any(n==0))]

if(length(col_name) > 0)
{
  print("Zeroes present in columns : ")
  print(col_name)
} else 
  print("No Zeroes")

###correlation
cor=cor(num_data)
corrplot(cor,method = "number",type = "lower")
#####
findCorrelation(cor,cutoff=0.9) 
View(cor)
######boxplot
boxplot(num_data$duration,horizontal = T)
summary(num_data$duration)

######relation between factors is calculated by chii square test
chisq.test(ittrain$service,ittrain$class)


nlevels(fact_data$protocol_type)
levels(fact_data$protocol_type)
nlevels(fact_data$flag)
nlevels(fact_data$service)
nlevels(fact_data$class)

####change the levels 
table(fact_data$service)
levels(ittrain$service)
nlevels((ittrain$service))

levels(ittrain$service)=c("ser_icmp","ser_icmp","ser_icmp","ser_icmp" , "ser_icmp",  
"ser_icmp" , "ser_icmp" , "ser_tcp" ,"ser_tcp"  ,"ser_icmp" ,"ser_icmp", "ser_icmp",
"ser_icmp"  , "ser_icmp", "other" ,    
"ser_tcp","ser_tcp","other","other","ser_tcp",   
"ser_tcp","ser_tcp", "ser_udp"  ,"ser_icmp" ,  "ser_icmp" ,  
"login" ,"login","login","login", "login", 
"ser_udp", "other","ser_udp", "ser_udp"  ,"ser_udp",
"ser_udp"    , "ser_udp", "ser_udp","ser_udp","other" ,
"other"  ,"ser_tcp",  "ser_tcp","other" ,"other" ,
"other"   , "login"  ,"login"       ,  "login"  ,  "ser_tcp",
"ser_tcp"     ,"ser_tcp","login", "login", "login",
"ser_tcp", "login",  "login" ,"login",  "login",
"other","other",   "other", "other", "other",
"other")




###model build
basemodel1=glm(class~.,binomial(link="logit"),data=ittrain)
summary(basemodel1)

#su_attempted,num_root,num_file_creations,num_shels,dst_host_srv_serroe_rate
#num_root and num_compresed,dst_host_srv_serroe_rate and dst_host_rerror_rate,rerror rate


###prediction
path="F:\\project1\\IT Risk Management\\NetworkIntrusionValidateData.csv"
val=read.csv(path)
nrow(val)
ncol(val)


table(val$land)
val$land=NULL
table(val$wrong_fragment)
val$wrong_fragment=NULL
table(val$urgent)
val$urgent=NULL
table(val$num_failed_logins)
val$num_failed_logins=NULL
table(val$root_shell)
val$root_shell=NULL
table(val$num_access_files)
val$num_access_files=NULL
table(val$num_outbound_cmds)
val$num_outbound_cmds=NULL
table(val$is_host_login)
val$is_host_login=NULL
table(val$is_guest_login)
val$is_guest_login=NULL

#y var is class
val$class=as.character(val$class)
str(val)
val$class[val$class=="normal"]=0
val$class[val$class=="anomaly"]=1
val$class=as.factor(val$class)

nlevels(val$service)
levels(val$service)=c("ser_icmp","ser_icmp","ser_icmp","ser_icmp" , "ser_icmp",  
                          "ser_icmp" , "ser_icmp" , "ser_tcp" ,"ser_tcp"  ,"ser_icmp" ,"ser_icmp", "ser_icmp",
                          "ser_icmp"  , "ser_icmp", "other" ,    
                          "ser_tcp","ser_tcp","other","other","ser_tcp",   
                          "ser_tcp","ser_tcp", "ser_udp"  ,"ser_icmp" ,  "ser_icmp" ,  
                          "login" ,"login","login","login", "login", 
                          "ser_udp", "other","ser_udp", "ser_udp"  ,"ser_udp",
                          "ser_udp"    , "ser_udp", "ser_udp","ser_udp","other" ,
                          "other"  ,"ser_tcp",  "ser_tcp","other" ,"other" ,
                          "other"   , "login"  ,"login"       ,  "login"  ,  "ser_tcp",
                          "ser_tcp"     ,"ser_tcp","login", "login", "login",
                          "ser_tcp", "login",  "login" ,"login",  "login",
                          "other","other",   "other", "other", "other",
                          "other")

basepredictions1 = predict(basemodel1, val, type="response")
print(basepredictions1[1:20])
print(basepredictions1)

table(val$class)

cutpoint = 0.5
predictions1 = ifelse(basepredictions1 <=0.5, 0,1)
print(predictions1[1:10])



# confusion matrix

library(caret)
confusionMatrix(as.factor(val$class), as.factor(predictions1), positive="1")
length(val$class)
library(ROCR)
preds1 = prediction(basepredictions1,val$class)
print(preds1)


# identifying the best cut-off by plotting the ROC curve
# 1) evaluations
# ------------------------------------
evals1 = performance(preds1, "acc")

evals1
plot(evals1)
abline(h=0.85,v=0.65) # play with these values
h

max_yval = which.max(slot(evals1, "y.values")[[1]])
max_acc = slot(evals1, "y.values")[[1]][max_yval]


max_cutoff = slot(evals1, "x.values")[[1]][max_yval]
print(paste("Best accuracy = ", round(max_acc,4), 
            " Best Cutoff = ", round(max_cutoff,4)))



# plot ROC
perf = performance(preds1, "tpr", "fpr")
plot(perf, colorize=T, main="ROC Curve", ylab = "Sensitivity", xlab = "1-Specificity")
abline(a=0, b=1)

# area under the curve (AUC)
auc = performance(preds1, "auc")
round(unlist(slot(auc, "y.values")),3)




















####################
#su_attempted,num_root,num_file_creations,num_shels,dst_host_srv_serroe_rate
#num_rootand num_compresed,dst_host_srv_serroe_rate and dst_host_rerror_rate,rerror rate


ittrain$num_compromised=NULL
ittrain$dst_host_srv_serror_rate=NULL
ittrain$rerror_rate=NULL
ittrain$su_attempted=NULL
ittrain$num_file_creations=NULL
ittrain$dst_host_serror_rate=NULL
###model build
colnames(ittrain)
basemodel2=glm(class~.,binomial(link="logit"),data=ittrain)
summary(basemodel2)

basepredictions2 = predict(basemodel2, val, type="response")
print(basepredictions2[1:20])

##prediction
val$num_compromised=NULL
val$dst_host_srv_serror_rate=NULL
val$rerror_rate=NULL
val$su_attempted=NULL
val$num_file_creations=NULL
val$dst_host_serror_rate=NULL
cutpoint = 0.5
predictions2 = ifelse(basepredictions2 <=0.5, 0,1)
print(predictions2[1:10])

# confusion matrix
library(caret)
confusionMatrix(as.factor(val$class), as.factor(predictions2), positive="1")












#######

path="F:\\project1\\IT Risk Management\\NetworkIntrusionTestData (1).csv"
test=read.csv(path)
nrow(test)
ncol(test)


table(test$land)
test$land=NULL
table(test$wrong_fragment)
test$wrong_fragment=NULL
table(test$urgent)
test$urgent=NULL
table(test$num_failed_logins)
test$num_failed_logins=NULL
table(test$root_shell)
test$root_shell=NULL
table(test$num_access_files)
test$num_access_files=NULL
table(test$num_outbound_cmds)
test$num_outbound_cmds=NULL
table(test$is_host_login)
test$is_host_login=NULL
table(test$is_guest_login)
test$is_guest_login=NULL


nlevels(test$service)
levels(test$service)=c("ser_icmp","ser_icmp","ser_icmp","ser_icmp" , "ser_icmp",  
                       "ser_icmp" , "ser_icmp" , "ser_tcp" ,"ser_tcp"  ,"ser_icmp" ,"ser_icmp", "ser_icmp",
                       "ser_icmp"  , "ser_icmp", "other" ,    
                       "ser_tcp","ser_tcp","other","other","ser_tcp",   
                       "ser_tcp","ser_tcp", "ser_udp"  ,"ser_icmp" ,  "ser_icmp" ,  
                       "login" ,"login","login","login", "login", 
                       "ser_udp", "other","ser_udp", "ser_udp"  ,"ser_udp",
                       "ser_udp"    , "ser_udp", "ser_udp","ser_udp","other" ,
                       "other"  ,"ser_tcp",  "ser_tcp","other" ,"other" ,
                       "other"   , "login"  ,"login"       ,  "login"  ,  "ser_tcp",
                       "ser_tcp"     ,"ser_tcp","login", "login", "login",
                       "ser_tcp", "login",  "login" ,"login",  "login",
                       "other","other",   "other", "other", "other",
                       "other")

basepredictions2 = predict(basemodel, test, type="response")
print(basepredictions2[1:20])


cutpoint = 0.5
predictions2 = ifelse(basepredictions2 <=0.5, 0,1)
print(predictions2[1:10])



# confusion matrix
library(caret)
length(predictions2)
length(ittrain$class)
confusionMatrix(as.factor(val$class), as.factor(predictions2), positive="1")

library(ROCR)
preds2 = prediction(basepredictions2,val$class)
print(preds2)


# identifying the best cut-off by plotting the ROC curve
# 1) evaluations
# ------------------------------------
evals2 = performance(preds2, "acc")

evals2
plot(evals2)
abline(h=0.85, v=0.65) # play with these values


max_yval = which.max(slot(evals2, "y.values")[[1]])
max_acc = slot(evals2, "y.values")[[1]][max_yval]


max_cutoff = slot(evals2, "x.values")[[1]][max_yval]
print(paste("Best accuracy = ", round(max_acc,4), 
            " Best Cutoff = ", round(max_cutoff,4)))
View(val)
View(test)








