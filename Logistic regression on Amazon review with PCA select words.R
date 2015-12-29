library(tm)
library(SnowballC)

#choose appropriate working directory where the file is present
setwd("D:/joka_acads/term5/term_paper")

#reading a csv file review,id,rating,class
data<-read.csv("amazon_reviews.csv") 

#reading the reivews
text_review<-data[1:1000,2]

#create corpus
text_translatable <- Corpus(VectorSource(text_review))  
text_translatable <- tm_map(text_translatable, stripWhitespace)  #<- to remove white spaces
text_translatable <- tm_map(text_translatable, content_transformer(tolower)) #<- to convert to lower case
text_translatable <- tm_map(text_translatable, removeWords, stopwords("english")) #<- to remove stop words
text_translatable <- tm_map(text_translatable, removePunctuation) #<- remove @,!,.....

#frequency  counter
text_translatable<-DocumentTermMatrix(text_translatable)
text_translatable <-as.matrix(text_translatable)             #<- covert to matrix
frequency <- colSums(text_translatable)
frequency <- sort(frequency, decreasing=TRUE)

#preparing data PCA
text_translatable<-t(text_translatable)
text_translatable<-cbind(text_translatable,Total=rowSums(text_translatable))
text_translatable<-data.frame(text_translatable)
text_translatable<-text_translatable[order(-text_translatable$Total),]
text_translatable<-t(text_translatable)
text_translatable <- head(text_translatable, -1) #remove the total row in last record

#choosing select words for PCA
text_translatable2<-NULL
colnames_words<-colnames(text_translatable)
poor_colno<-match("poor",colnames_words)
stuck_colno<-match("stuck",colnames_words)
return_colno<-match("return",colnames_words)
refund_colno<-match("refund",colnames_words)
dead_colno<-match("dead",colnames_words)
repair_colno<-match("repair",colnames_words)
great_colno<-match("great",colnames_words)
perfect_colno<-match("perfect",colnames_words)
text_translatable2<-cbind(text_translatable[,1],text_translatable[,poor_colno],text_translatable[,stuck_colno],text_translatable[,return_colno],text_translatable[,refund_colno],text_translatable[,dead_colno],text_translatable[,repair_colno],text_translatable[,great_colno],text_translatable[,perfect_colno])
colnames(text_translatable2) <- c("","poor","stuck","return","refund","dead","repair","great","perfect")
#write.csv(text_translatable2,"fit.csv")

#Fitting the PCA
fit<-prcomp(text_translatable2, scale.=TRUE)
summary(fit) 
# print variance accounted for 


plot(fit,type="lines") 
# scree plot 
fit$scale
fit$scores # the principal components
head(fit$rotation)
#biplot(fit)
fit$x


data<-read.csv("amazon_reviews.csv") #reading a csv file review,id,rating,class

#preparing data for regression
for_regression <- cbind(fit$x, log(data[,3]/(5-data[,3]),base=exp(1))) #<- adding ratings to the matrix
colnames(for_regression)[NCOL(for_regression)-1] <- "log_odds"

#Logit regression  
for_regression_2<-data.frame(for_regression)

#segregating training and testing data
train_data<-for_regression_2[1:800,]
test_data<-for_regression_2[801:900,]

#training data
logr<- glm (log_odds ~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8, data=train_data)

#Summary for regression
summary(logr)
#logr
#anova(logr)

#applying the model on test data
test_values<-predict(logr,newdata=test_data, type="response", se.fit=T) 
write.csv(test_values,"test_predictors.csv")


#test data accuracy
test_predictors<-read.csv("test_predictors.csv") 
for_accuracy_matrix <- cbind(test_predictors,1/(1+exp(-test_predictors$fit)),data[801:900,4],0)
colnames(for_accuracy_matrix)[5] <- "predicted_value"
colnames(for_accuracy_matrix)[6] <- "actual_class"
colnames(for_accuracy_matrix)[7] <- "predicted_class"

for_accuracy_matrix$predicted_class <- ifelse(for_accuracy_matrix$predicted_value <.495, 0, 1)

#confusion matrix
table(for_accuracy_matrix[, c("actual_class", "predicted_class")]) #accuracy matrix

#accuracy %
nrow(for_accuracy_matrix[for_accuracy_matrix$actual_class==for_accuracy_matrix$predicted_class,])/nrow(for_accuracy_matrix)*100

#appying the model on validation data
validate_data<-for_regression_2[901:1000,]
validate_values<-predict(logr,newdata=validate_data, type="response", se.fit=T) 
write.csv(validate_values,"validate_predictors.csv")
validate_predictors<-read.csv("validate_predictors.csv")
for_accuracy_matrix <- cbind(validate_predictors,1/(1+exp(-validate_predictors$fit)),data[901:1000,4],0)
colnames(for_accuracy_matrix)[5] <- "predicted_value"
colnames(for_accuracy_matrix)[6] <- "actual_class"
colnames(for_accuracy_matrix)[7] <- "predicted_class"
for_accuracy_matrix$predicted_class <- ifelse(for_accuracy_matrix$predicted_value <=0.495, 0, 1)

#confusion matrix
table(for_accuracy_matrix[, c("actual_class", "predicted_class")]) #accuracy matrix

#accuracy %
nrow(for_accuracy_matrix[for_accuracy_matrix$actual_class==for_accuracy_matrix$predicted_class,])/nrow(for_accuracy_matrix)*100
