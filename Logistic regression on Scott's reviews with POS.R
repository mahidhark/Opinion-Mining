library(tm) #text mining library

setwd("C:/Users/sriteja/Documents/Termpaper/Sampledataset_2")#location where we have the data
data<-read.csv("scott.csv") #reading a csv file review,id,rating,class

#NLP packages
#install.packages("openNLP")
#install.packages("NLP")
library(openNLP)
library(NLP)

#POS tagging function
tagPOS <-  function(x, ...) {
  s <- as.String(x)
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- annotate(s, word_token_annotator, a2)
  a3 <- annotate(s, Maxent_POS_Tag_Annotator(), a2)
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  list(POStagged = POStagged, POStags = POStags)
}


#pos Tagging for the input data, the data is in first column of data[]
#roughly takes 1 second to tag one review
#time consuming !!!
data_tag=data.frame()
for(i in 1:902)   
{data_tag[1,i]<-tagPOS(data[i,1])[1]   
#print(i)
}

data_tag<-t(data_tag) #transpose 
write.csv(data_tag,"data_tags.csv")
text_reviews<-read.csv("data_tags.csv")
text_reviews$X<-NULL
write.csv(text_reviews,"data_tags_check.csv")
data1<-read.csv("data_tags_check.csv")
text_reviews<-data1[,2]

#create corpus
text_translatable <- Corpus(VectorSource(text_reviews))  
text_translatable <- tm_map(text_translatable, stripWhitespace)  #<- to remove white spaces
text_translatable <- tm_map(text_translatable, content_transformer(tolower)) #<- to convert to lower case
text_translatable <- tm_map(text_translatable, removeWords, stopwords("english")) #<- to remove stop words


#frequency  counter
text_translatable1<-DocumentTermMatrix(text_translatable)
text_translatable2 <-as.matrix(text_translatable1)             #<- covert to matrix
frequency <- colSums(text_translatable2)
frequency <- sort(frequency, decreasing=TRUE)

#write.csv(text_translatable2,"output_frequency.csv")




# pulling adjectives and adverbs 
#/jj and /rb give tag for adjectives and adverbs
adj_occu<-cbind(text_translatable2[,colnames(text_translatable2)[grep("/jj",colnames(text_translatable2))]],text_translatable2[,colnames(text_translatable2)[grep("/rb",colnames(text_translatable2))]])
write.csv(adj_occu,"adjectives_occu.csv")
occurence2 <- colSums(adj_occu)
occurence2 <- sort(occurence2, decreasing=TRUE)


#data<-read.csv("scott.csv") #reading a csv file review,id,rating,class
for_regression <- cbind(adj_occu, log(data[,3]/(1-data[,3]),base=exp(1))) #<- adding ratings to the matrix
colnames(for_regression)[NCOL(for_regression)] <- "log_odds"




#Logit regression  
for_regression_2<-data.frame(for_regression)

#Segregating the train and test data
train_data<-for_regression_2[1:300,]
test_data<-for_regression_2[301:600,]

#training the model
#change the words here to improve the model
#use adj_occu.csv to run see the numbers 
logr<- glm (log_odds ~train_data[,2400]+train_data[,3504]+train_data[,5158]+train_data[,1258]+train_data[,649]+train_data[,5578]+train_data[,1870]+train_data[,673]+train_data[,1100]+train_data[,843]+train_data[,6676]+train_data[,5318]+train_data[,5048]+train_data[,5049]+train_data[,3151]+train_data[,7467]
            , data=train_data)
#gripping+mesmerizing+ riveting+spectacular+cool+awesome+ thrilling+excellent+exciting+bad+ cliched+ boring+ stupid+ slow +least+slowly+unfortunately

#summary(logr)# the last column indicates the p value of each variable

#running the model on test data
test_values<-predict(logr,newdata=test_data, type="response", se.fit=T) 
#choose the row numbers in the next line based on test data
for_accuracy_matrix <- cbind(test_predictors,1/(1+exp(-test_predictors$fit)),data[301:600,4],0)
colnames(for_accuracy_matrix)[5] <- "predicted_value"
colnames(for_accuracy_matrix)[6] <- "actual_class"
colnames(for_accuracy_matrix)[7] <- "predicted_class"

#modify the predicted cutoff value for better results 
for_accuracy_matrix$predicted_class <- ifelse(for_accuracy_matrix$predicted_value <=0.62, 0,1)
#confusion matrix
table(for_accuracy_matrix[, c("actual_class", "predicted_class")]) #accuracy matrix
#test data accuracy%
nrow(for_accuracy_matrix[for_accuracy_matrix$actual_class==for_accuracy_matrix$predicted_class,])/nrow(for_accuracy_matrix)*100




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
for_accuracy_matrix$predicted_class <- ifelse(for_accuracy_matrix$predicted_value <=0.62, 0,1)
table(for_accuracy_matrix[, c("actual_class", "predicted_class")]) #accuracy matrix
#validation accuracy
nrow(for_accuracy_matrix[for_accuracy_matrix$actual_class==for_accuracy_matrix$predicted_class,])/nrow(for_accuracy_matrix)*100
