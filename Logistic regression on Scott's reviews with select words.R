library(tm) #text mining library

setwd("C:/Users/sriteja/Documents/Termpaper/Sampledataset_2")#location where we have the data
data<-read.csv("scott.csv") #reading a csv file review,id,rating,class

text_reviews<-data[,1] #<- reading all the reviews to text_reviews. Reviews are written in first column


#text mining algorithm
#create corpus
text_translatable <- Corpus(VectorSource(text_reviews))        
text_translatable <- tm_map(text_translatable, stripWhitespace)  #<- to remove white spaces
text_translatable <- tm_map(text_translatable, content_transformer(tolower)) #<- to convert to lower case
text_translatable <- tm_map(text_translatable, removeWords, stopwords("english")) #<- to remove stop words
text_translatable <- tm_map(text_translatable, removePunctuation) #<- remove @,!,.....


#frequency  counter
# number of times a key word occurs in each text 
text_translatable<-DocumentTermMatrix(text_translatable,control=list(minWordLength=1))
text_translatable <-as.matrix(text_translatable)             #<- convert to matrix
frequency <- colSums(text_translatable)
frequency <- sort(frequency, decreasing=TRUE)
#write.csv(text_translatable2,"output_frequency.csv")


for_regression <- cbind(text_translatable,data[,3],data[,3]/(1-data[,3]), log(data[,3]/(1-data[,3]),base=exp(1))) #<- adding ratings to the matrix

#renaming the 3 added columns
colnames(for_regression)[NCOL(for_regression)-2] <- "rating"             #adding column name
colnames(for_regression)[NCOL(for_regression)-1] <- "odds"             #adding column name
colnames(for_regression)[NCOL(for_regression)] <- "log_odds"

#Logit regression  
for_regression_2<-data.frame(for_regression)

#Segregating the train and test data
train_data<-for_regression_2[1:300,]
test_data<-for_regression_2[301:600,]


#training the model
#change the words here to improve the model
logr <- glm( log_odds ~ gripping+mesmerizing+ riveting+spectacular+cool+awesome+ thrilling
             +bad+ cliched+ sucks+ boring+ stupid+ slow+least+unfortunately , data=train_data  )



summary(logr) # the last column indicates the p value of each variable
#logr                    
#anova(logr)


#running the model on test data
test_values<-predict(logr,newdata=test_data, type="response", se.fit=T) 
write.csv(test_values,"test_predictors.csv")
test_predictors<-read.csv("test_predictors.csv") 
#choose the row numbers in the next line based on test data
for_accuracy_matrix <- cbind(test_predictors,1/(1+exp(-test_predictors$fit)),data[301:600,4],0)
colnames(for_accuracy_matrix)[5] <- "predicted_value"
colnames(for_accuracy_matrix)[6] <- "actual_class"
colnames(for_accuracy_matrix)[7] <- "predicted_class"


#modify the predicted cutoff value for better results 
for_accuracy_matrix$predicted_class <- ifelse(for_accuracy_matrix$predicted_value <=0.63, 0,1)
table(for_accuracy_matrix[, c("actual_class", "predicted_class")]) #accuracy matrix consider improving specificity,sensitivity
#test data accuracy%
nrow(for_accuracy_matrix[for_accuracy_matrix$actual_class==for_accuracy_matrix$predicted_class,])/nrow(for_accuracy_matrix)*100
#60.3%





#Validation on the validation set
validate_data<-for_regression_2[601:900,]
validate_values<-predict(logr,newdata=validate_data, type="response", se.fit=T) 
write.csv(validate_values,"validate_predictors.csv")
validate_predictors<-read.csv("validate_predictors.csv") 
#choose the row numbers in the next line based on validation data
for_accuracy_matrix <- cbind(validate_predictors,1/(1+exp(-validate_predictors$fit)),data[601:900,4],0)
colnames(for_accuracy_matrix)[5] <- "predicted_value"
colnames(for_accuracy_matrix)[6] <- "actual_class"
colnames(for_accuracy_matrix)[7] <- "predicted_class"

#modify the predicted cutoff value for better results 
for_accuracy_matrix$predicted_class <- ifelse(for_accuracy_matrix$predicted_value <=0.63, 0,1)
table(for_accuracy_matrix[, c("actual_class", "predicted_class")])#accuracy matrix consider improving specificity,sensitivity
#validation accuracy
nrow(for_accuracy_matrix[for_accuracy_matrix$actual_class==for_accuracy_matrix$predicted_class,])/nrow(for_accuracy_matrix)*100
#65.9%

