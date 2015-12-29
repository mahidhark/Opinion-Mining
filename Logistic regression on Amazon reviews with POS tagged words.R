#with pca and pos tagging

library(tm)



setwd("C:/Users/sriteja/Documents/Termpaper/Amazon_dataset")
data<-read.csv("amazon_reviews.csv") #reading a csv file review,id,rating,class



#install.packages("openNLP")
#install.packages("NLP")
library(openNLP)
library(NLP)
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

data_tag=data.frame()
for(i in 1:1000)   
{data_tag[1,i]<-tagPOS(data[i,2])[1]   
print(i)
#print(data_tag[i])
}

data_tag<-t(data_tag) #transpose 
write.csv(data_tag,"data_tags.csv")
text_reviews<-read.csv("data_tags.csv")
text_reviews$X<-NULL
write.csv(text_reviews,"data_tags_check.csv")
data<-read.csv("data_tags_check.csv")
text_reviews<-data[,2]

#create corpus
text_translatable <- Corpus(VectorSource(text_reviews))  
text_translatable <- tm_map(text_translatable, stripWhitespace)  #<- to remove white spaces
text_translatable <- tm_map(text_translatable, content_transformer(tolower)) #<- to convert to lower case
text_translatable <- tm_map(text_translatable, removeWords, stopwords("english")) #<- to remove stop words
#text_translatable <- tm_map(text_translatable, removePunctuation) #<- remove @,!,.....
text_translatable1<-tm_map(text_translatable, stemDocument) #stem algorithm


#frequency  counter
text_translatable1<-DocumentTermMatrix(text_translatable)
text_translatable2 <-as.matrix(text_translatable1)             #<- covert to matrix
#text_translatable2             # number of times a key word occurs in each text 

frequency <- colSums(text_translatable2)
frequency <- sort(frequency, decreasing=TRUE)
frequency
#head(frequency,50)
#write.csv(text_translatable2,"output_frequency.csv")








#presence counter
text_translatable3<-text_translatable2
text_translatable3[text_translatable3 >0] <- 1
text_translatable3             #if a keyword is present or not
occurence <- colSums(text_translatable3)
occurence <- sort(occurence, decreasing=TRUE)
#head(occurence,20)
#write.csv(occurence,"output_presence.csv")


# pulling adjectives
adj_occu<-cbind(text_translatable3[,colnames(text_translatable2)[grep("/jj",colnames(text_translatable2))]],text_translatable2[,colnames(text_translatable2)[grep("/rb",colnames(text_translatable2))]])
write.csv(adj_occu,"adjectives_occu.csv")
occurence2 <- colSums(adj_occu)
occurence2 <- sort(occurence2, decreasing=TRUE)
occurence2
head(occurence2,20)
write.csv(occurence2,"output2_presence.csv")


write.csv(colnames(adj_occu),"output3_presence.csv")



data<-read.csv("amazon_reviews.csv") #reading a csv file review,id,rating,class

for_regression <- cbind(adj_occu, log(data[1:1000,3]/(5-data[1:1000,3]),base=exp(1))) #<- adding ratings to the matrix
NCOL(for_regression)
colnames(for_regression)[NCOL(for_regression)] <- "log_odds"
#tail(colnames(for_regression),20)
#head(for_regression)
#for_regression[,24880]

#write.csv(for_regression,"for_regression.csv")
write.csv(colnames(for_regression),"colnames.csv")


#Logit regression  
for_regression_2<-data.frame(for_regression)
#tail(colnames(for_regression_2))



train_data<-for_regression_2[1:300,]
test_data<-for_regression_2[301:600,]

#randomizing/ partitioning

#train_data[1,4]
#head(colnames(train_data))
#training data
logr<- glm (log_odds ~train_data[,1499]+train_data[,911]+train_data[,900]+train_data[,1849]+train_data[,2762]+train_data[,644]+train_data[,2417]+train_data[,1461]+train_data[,357]+train_data[,799]
            , data=train_data)

#poor+frustrating+freeze+terrible+unfortunately+dead+great+perfect+amazing+excellent

logr
summary(logr)
#anova(logr)


test_values<-predict(logr,newdata=test_data, type="response", se.fit=T) 
write.csv(test_values,"test_predictors.csv")


#test data accuracy
test_predictors<-read.csv("test_predictors.csv") 
for_accuracy_matrix <- cbind(test_predictors,1/(1+exp(-test_predictors$fit)),data[301:600,4],0)
colnames(for_accuracy_matrix)[5] <- "predicted_value"
colnames(for_accuracy_matrix)[6] <- "actual_class"
colnames(for_accuracy_matrix)[7] <- "predicted_class"

for_accuracy_matrix$predicted_class <- ifelse(for_accuracy_matrix$predicted_value <=0.94, 0,1)

head(for_accuracy_matrix)
table(for_accuracy_matrix[, c("actual_class", "predicted_class")]) #accuracy matrix

#accuracy %
nrow(for_accuracy_matrix[for_accuracy_matrix$actual_class==for_accuracy_matrix$predicted_class,])/nrow(for_accuracy_matrix)*100
#43.16%




validate_data<-for_regression_2[601:900,]
validate_values<-predict(logr,newdata=validate_data, type="response", se.fit=T) 
write.csv(validate_values,"validate_predictors.csv")


#validate data accuracy
validate_predictors<-read.csv("validate_predictors.csv") 
for_accuracy_matrix <- cbind(validate_predictors,1/(1+exp(-validate_predictors$fit)),data[601:900,4],0)
colnames(for_accuracy_matrix)[5] <- "predicted_value"
colnames(for_accuracy_matrix)[6] <- "actual_class"
colnames(for_accuracy_matrix)[7] <- "predicted_class"

for_accuracy_matrix$predicted_class <- ifelse(for_accuracy_matrix$predicted_value <=0.94, 0,1)

#head(for_accuracy_matrix)
table(for_accuracy_matrix[, c("actual_class", "predicted_class")]) #accuracy matrix

#accuracy %
nrow(for_accuracy_matrix[for_accuracy_matrix$actual_class==for_accuracy_matrix$predicted_class,])/nrow(for_accuracy_matrix)*100
#41%
