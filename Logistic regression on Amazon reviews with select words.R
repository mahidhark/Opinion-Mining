library(tm)
library(SnowballC)
setwd("D:/joka_acads/term5/term_paper")
data<-read.csv("amazon_reviews.csv") #reading a csv file review,id,rating,class

text_reviews<-data[,2] #<- reading all the reviews to text_reviews

#create corpus
text_translatable <- Corpus(VectorSource(text_reviews))        
text_translatable <- tm_map(text_translatable, stripWhitespace)  #<- to remove white spaces
text_translatable <- tm_map(text_translatable, content_transformer(tolower)) #<- to convert to lower case
text_translatable <- tm_map(text_translatable, removeWords, stopwords("english")) #<- to remove stop words
text_translatable <- tm_map(text_translatable, removePunctuation) #<- remove @,!,.....
text_translatable <- tm_map(text_translatable, stemDocument)


#frequency  counter
text_translatable<-DocumentTermMatrix(text_translatable,control=list(minWordLength=1))
text_translatable <-as.matrix(text_translatable)             #<- covert to matrix

# number of times a key word occurs in each text 
frequency <- colSums(text_translatable)
frequency <- sort(frequency, decreasing=TRUE)


#preparing data for regression
for_regression <- cbind(text_translatable,data[,3]/5,data[,3]/(5-data[,3]), log(data[,3]/(5-data[,3]),base=exp(1))) #<- adding ratings to the matrix

#Logit regression  
for_regression<-data.frame(for_regression)


#segregating training and test data
train_data<-for_regression[1:3000,]
test_data<-for_regression[3001:4000,]

#training data
logr <- glm( for_regression[1:3000,16525] ~ poor+stuck+return+refund+dead+repair+great+perfect , data=train_data  )
#logr
summary(logr)
#anova(logr)

#predict classes for test data
test_values<-predict(logr,newdata=test_data, type="response", se.fit=T) 
write.csv(test_values,"test_predictors.csv")

#test data accuracy
test_predictors<-read.csv("test_predictors.csv") 
for_accuracy_matrix <- cbind(test_predictors,1/(1+exp(-test_predictors$fit)),data[3001:4000,4],0)
colnames(for_accuracy_matrix)[5] <- "predicted_value"
colnames(for_accuracy_matrix)[6] <- "actual_class"
colnames(for_accuracy_matrix)[7] <- "predicted_class"

for_accuracy_matrix$predicted_class <- ifelse(for_accuracy_matrix$predicted_value <=0.9, 0, 1)

#confusion matrix
table(for_accuracy_matrix[, c("actual_class", "predicted_class")])

#accuracy %
nrow(for_accuracy_matrix[for_accuracy_matrix$actual_class==for_accuracy_matrix$predicted_class,])/nrow(for_accuracy_matrix)*100


#validating the model
validate_data<-for_regression[4001:5015,]

#applying the model on validation data
validate_values<-predict(logr,newdata=validate_data, type="response", se.fit=T) 
write.csv(validate_values,"validate_predictors.csv")


#validation data accuracy
validate_predictors<-read.csv("validate_predictors.csv") 
for_accuracy_matrix <- cbind(validate_predictors,1/(1+exp(-validate_predictors$fit)),data[4001:5015,4],0)
colnames(for_accuracy_matrix)[5] <- "predicted_value"
colnames(for_accuracy_matrix)[6] <- "actual_class"
colnames(for_accuracy_matrix)[7] <- "predicted_class"

for_accuracy_matrix$predicted_class <- ifelse(for_accuracy_matrix$predicted_value <=0.9, 0, 1)

#confusion matrix
table(for_accuracy_matrix[, c("actual_class", "predicted_class")]) #accuracy matrix

#accuracy %
nrow(for_accuracy_matrix[for_accuracy_matrix$actual_class==for_accuracy_matrix$predicted_class,])/nrow(for_accuracy_matrix)*100

