#Sharma (2015)
library("rpart.plot")
library("RColorBrewer")
library("rattle")
library("rpart")
STAR_G1<-NULL
STAR_G1 <- read.csv("C:/Users/Aliba/Documents/STAR_G1_clean.csv")

##Single Tree on lang

#split the data 
set.seed(2)
training<-sample(1:nrow(STAR_G1), nrow(STAR_G1)/2) #creates subsample
testing<-STAR_G1[-training,]

#Training Set's Regression Tree
tree.STAR_G1<-rpart(lang~g1classtype+g1surban+tgender+trace+tyears+race+gender+freelu+birth,data=STAR_G1,subset=training,method="anova")
par(mfrow=c(1,1))
plot(tree.STAR_G1)
text(tree.STAR_G1)
title("Training Set's Regression Tree")

fancyRpartPlot(tree.STAR_G1)
title("Single Tree Model on Language")

#pruning 1
par(mfrow=c(1,2)) # two plots on one page
rsq.rpart(tree.STAR_G1) # visualize cross-validation results

#pruning 2
printcp(tree.STAR_G1)
par(mfrow=c(1,1))
plotcp(tree.STAR_G1)

#pruning makes no sense
#tree.STAR_G2_prune<-prune(tree.STAR_G2,cp=0.01) #get minimal cp from 'printcp(tree.STAR_G2)'
#fancyRpartPlot(tree.STAR_G2_prune)

#in sample goodness of fit & CATE
tree.STAR_G1_pr<-predict(tree.STAR_G1,newdata=STAR_G1[training,]) #predict tree
y_G1_tr<-STAR_G1[training, "lang"] #true values of y
mean((tree.STAR_G1_pr-y_G1_tr)^2) #MSE training

#out of sample goodness of fit
tree.STAR_G1_pr_test<-predict(tree.STAR_G1,newdata=STAR_G1[-training,]) #predict on test set
plot(tree.STAR_G1_pr_test)
y_G1_te<-STAR_G1[-training,"lang"] #y values of test set
mean((tree.STAR_G1_pr_test-y_G1_te)^2)

##Two Trees on lang

STAR_G1_B<-STAR_G1[!(STAR_G1$g1classtype=="0"),]
STAR_G1_S<-STAR_G1[!(STAR_G1$g1classtype=="1"),]

#STAR_G1_B
#split the data
set.seed(4)
training_B<-sample(1:nrow(STAR_G1_B), nrow(STAR_G1_B)/2) #creates subsample, 1:5
testing_B<-STAR_G1_B[-training_B,]

#Training Set's Regression Tree
tree.STAR_G1_B<-rpart(lang~g1surban+tgender+trace+tyears+race+gender+freelu+birth,data=STAR_G1_B,subset=training_B,method="anova")
par(mfrow=c(1,1))
plot(tree.STAR_G1_B)
text(tree.STAR_G1_B)
title("Training Set's Regression Tree")

fancyRpartPlot(tree.STAR_G1_B)

#pruning big class
par(mfrow=c(1,2)) # two plots on one page
rsq.rpart(tree.STAR_G1_B) # visualize cross-validation results

#STAR_G1_S
#split the data
set.seed(4)
training_S<-sample(1:nrow(STAR_G1_S), nrow(STAR_G1_S)/2) #creates subsample
testing_S<-STAR_G1_S[-training,]

#Training Set's Regression Tree
tree.STAR_G1_S<-rpart(lang~g1surban+tgender+trace+tyears+race+gender+freelu+birth,data=STAR_G1_S,subset=training_S,method="anova")
par(mfrow=c(1,1))
plot(tree.STAR_G1_S)
text(tree.STAR_G1_S)
title("Training Set's Regression Tree")

fancyRpartPlot(tree.STAR_G1_S)

#pruning small class
par(mfrow=c(1,2)) # two plots on one page
rsq.rpart(tree.STAR_G1_S) # visualize cross-validation results

#in sample goodness of fit & CATE
tree.STAR_G1_B_pr<-predict(tree.STAR_G1_B,newdata=STAR_G1_B[training_B,])
tree.STAR_G1_S_pr<-predict(tree.STAR_G1_S,newdata=STAR_G1_S[training_S,])
y_G1_B_tr<-STAR_G1_B[training_B, "lang"]
y_G1_S_tr<-STAR_G1_S[training_S, "lang"]
mean((tree.STAR_G1_B_pr +tree.STAR_G1_S_pr-y_G1_B_tr-y_G1_S_tr)^2)

#out of sample goodness of fit
tree.STAR_G1_B_pr_test<-predict(tree.STAR_G1_B,newdata=STAR_G1_B[-training_B,])
plot(tree.STAR_G1_B_pr_test)
tree.STAR_G1_S_pr_test<-predict(tree.STAR_G1_S,newdata=STAR_G1_S[-training_S,])
plot(tree.STAR_G1_S_pr_test)
y_G1_B_te<-STAR_G1_B[-training_B, "lang"]
y_G1_S_te<-STAR_G1_S[-training_S, "lang"]
mean((tree.STAR_G1_B_pr_test+tree.STAR_G1_S_pr_test-y_G1_B_te-y_G1_S_te)^2)
mean(tree.STAR_G1_S_pr_test)-mean(tree.STAR_G1_B_pr_test) #CATE

##Transformed Outcome Tree on word_2
#split the data 
set.seed(3)
training<-sample(1:nrow(STAR_G1), nrow(STAR_G1)/2) #creates subsample
testing<-STAR_G1[-training,]

#Training Set's Regression Tree
tree.STAR_G1<-rpart(word_2~g1surban+tgender+trace+tyears+race+gender+freelu+birth,data=STAR_G1,subset=training,method="anova")
par(mfrow=c(1,1))
plot(tree.STAR_G1)
text(tree.STAR_G1)
title("Training Set's Regression Tree")

fancyRpartPlot(tree.STAR_G1)

#pruning
par(mfrow=c(1,2)) # two plots on one page
rsq.rpart(tree.STAR_G1) # visualize cross-validation results

#pruning 2
printcp(tree.STAR_G1)
par(mfrow=c(1,1))
plotcp(tree.STAR_G1)
#pruning makes no sense
#tree.STAR_G1_prune<-prune(tree.STAR_G1,cp=0.01) #get minimal cp from 'printcp(tree.STAR_G1)'
#fancyRpartPlot(tree.STAR_G1_prune)

#in sample goodness of fit & CATE
tree.STAR_G1_pr<-predict(tree.STAR_G1,newdata=STAR_G1[training,])
y_G1_tr<-STAR_G1[training, "word_2"]
mean((tree.STAR_G1_pr-y_G1_tr)^2)

#out of sample goodness of fit
tree.STAR_G1_pr_test<-predict(tree.STAR_G1,newdata=STAR_G1[-training,])
plot(tree.STAR_G1_pr_test)
y_G1_te<-STAR_G1[-training,"word_2"]
mean((tree.STAR_G1_pr_test-y_G1_te)^2)
mean(tree.STAR_G1_pr_test) #CATE ?! 

#meanvar(tree.STAR_G1)

#Variance leaf 1
train<-STAR_G1[training,] #generate training dataset (non atomic)
ttree_1<-train[!(train$tyears>="11.5"),]
ttree_1<-train[!(train$g1surban<"3.5"),] #training set without students older than 1980
ttree_1t<-ttree_1[!(ttree_1$g1classtype>0),] #training set without big class
ttree_1c<-ttree_1[!(ttree_1$g1classtype<1),] #training set without small class
tvar_1<-var(ttree_1t$word_2)/nrow(ttree_1t)+var(ttree_1c$word_2)/nrow(ttree_1c) #variance formula
tvar_1

tree_1<-testing[!(testing$tyears>="11.5"),]
tree_1<-testing[!(testing$g1surban<"3.5"),] #same procedure on testing set, relevant!
tree_1t<-tree_1[!(tree_1$g1classtype>0),]
tree_1c<-tree_1[!(tree_1$g1classtype<1),]
var_1<-var(tree_1t$word_2)/nrow(tree_1t)+var(tree_1c$word_2)/nrow(tree_1c)
var_1

#Variance leaf 2
ttree_2<-train[!(train$tyears>="11.5"),]
ttree_2<-train[!(train$g1surban>="3.5"),] #training set without students older than 1980
ttree_2t<-ttree_2[!(ttree_2$g1classtype>0),] #training set without big class
ttree_2c<-ttree_2[!(ttree_2$g1classtype<1),] #training set without small class
tvar_2<-var(ttree_2t$word_2)/nrow(ttree_2t)+var(ttree_2c$word_2)/nrow(ttree_2c) #variance formula
tvar_2

tree_2<-testing[!(testing$tyears>="11.5"),]
tree_2<-testing[!(testing$g1surban>="3.5"),] #same procedure on testing set, relevant!
tree_2t<-tree_2[!(tree_2$g1classtype>0),]
tree_2c<-tree_2[!(tree_2$g1classtype<1),]
var_2<-var(tree_2t$word_2)/nrow(tree_2t)+var(tree_2c$word_2)/nrow(tree_2c)
var_2

#Variance leaf 3
ttree_3<-train[!(train$tyears<"11.5"),]
ttree_3<-train[!(train$trace>="1.5"),] #training set without students older than 1980
ttree_3<-train[!(train$g1surban>="3.5"),]
ttree_3t<-ttree_3[!(ttree_3$g1classtype>0),] #training set without big class
ttree_3c<-ttree_3[!(ttree_3$g1classtype<1),] #training set without small class
tvar_3<-var(ttree_3t$word_2)/nrow(ttree_3t)+var(ttree_3c$word_2)/nrow(ttree_2c) #variance formula
tvar_3

tree_3<-testing[!(testing$tyears<"11.5"),]
tree_3<-testing[!(testing$trace>="1.5"),] #same procedure on testing set, relevant!
tree_3<-testing[!(testing$g1surban>="3.5"),]
tree_3t<-tree_3[!(tree_3$g1classtype>0),]
tree_3c<-tree_3[!(tree_3$g1classtype<1),]
var_3<-var(tree_3t$word_2)/nrow(tree_3t)+var(tree_3c$word_2)/nrow(tree_3c)
var_3

#Variance leaf 4
ttree_4<-train[!(train$tyears<"11.5"),]
ttree_4<-train[!(train$trace>="1.5"),] #training set without students older than 1980
ttree_4<-train[!(train$g1surban<"3.5"),]
ttree_4<-train[!(train$tyears<"16.5"),]
ttree_4t<-ttree_4[!(ttree_4$g1classtype>0),] #training set without big class
ttree_4c<-ttree_4[!(ttree_4$g1classtype<1),] #training set without small class
tvar_4<-var(ttree_4t$word_2)/nrow(ttree_4t)+var(ttree_4c$word_2)/nrow(ttree_2c) #variance formula
tvar_4

tree_4<-testing[!(testing$tyears<"11.5"),]
tree_4<-testing[!(testing$trace>="1.5"),] #same procedure on testing set, relevant!
tree_4<-testing[!(testing$g1surban<"3.5"),]
tree_4<-testing[!(testing$tyears<"16.5"),]
tree_4t<-tree_4[!(tree_4$g1classtype>0),]
tree_4c<-tree_4[!(tree_4$g1classtype<1),]
var_4<-var(tree_4t$word_2)/nrow(tree_4t)+var(tree_4c$word_2)/nrow(tree_4c)
var_4

#Variance leaf 5
ttree_5<-train[!(train$tyears<"11.5"),]
ttree_5<-train[!(train$trace>="1.5"),] #training set without students older than 1980
ttree_5<-train[!(train$g1surban<"3.5"),]
ttree_5<-train[!(train$tyears>="16.5"),]
ttree_5t<-ttree_5[!(ttree_5$g1classtype>0),] #training set without big class
ttree_5c<-ttree_5[!(ttree_5$g1classtype<1),] #training set without small class
tvar_5<-var(ttree_5t$word_2)/nrow(ttree_5t)+var(ttree_5c$word_2)/nrow(ttree_2c) #variance formula
tvar_5

tree_5<-testing[!(testing$tyears<"11.5"),]
tree_5<-testing[!(testing$trace>="1.5"),] #same procedure on testing set, relevant!
tree_5<-testing[!(testing$g1surban<"3.5"),]
tree_5<-testing[!(testing$tyears>="16.5"),]
tree_5t<-tree_5[!(tree_5$g1classtype>0),]
tree_5c<-tree_5[!(tree_5$g1classtype<1),]
var_5<-var(tree_5t$word_2)/nrow(tree_5t)+var(tree_5c$word_2)/nrow(tree_5c)
var_5

#Variance leaf 6
ttree_6<-train[!(train$tyears<"11.5"),]
ttree_6<-train[!(train$trace<"1.5"),] #training set without students older than 1980
ttree_6t<-ttree_6[!(ttree_6$g1classtype>0),] #training set without big class
ttree_6c<-ttree_6[!(ttree_6$g1classtype<1),] #training set without small class
tvar_6<-var(ttree_6t$word_2)/nrow(ttree_6t)+var(ttree_6c$word_2)/nrow(ttree_2c) #variance formula
tvar_6

tree_6<-testing[!(testing$tyears<"11.5"),]
tree_6<-testing[!(testing$trace<"1.5"),] #same procedure on testing set, relevant!
tree_6t<-tree_6[!(tree_6$g1classtype>0),]
tree_6c<-tree_6[!(tree_6$g1classtype<1),]
var_6<-var(tree_6t$word_2)/nrow(tree_6t)+var(tree_6c$word_2)/nrow(tree_6c)
var_6



