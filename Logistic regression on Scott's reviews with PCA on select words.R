library(tm)

setwd("C:/Users/sriteja/Documents/Termpaper/Sampledataset_2")#location where we have the data
data<-read.csv("scott.csv") #reading a csv file review,id,rating,class
head(data,1)  #display first row

text_review<-data[,1]

NROW(text_review)
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


text_translatable<-t(text_translatable)

text_translatable<-cbind(text_translatable,Total=rowSums(text_translatable))
#write.csv(text_translatable, "colnames.csv")
text_translatable<-data.frame(text_translatable)

text_translatable<-t(text_translatable)
text_translatable <- head(text_translatable, -1) #remove the total row in last record


colnames_words<-colnames(text_translatable)
write.csv(colnames_words, "colnames.csv")
gripping_colno<-match("gripping",colnames_words)
mesmerizing_colno<-match("mesmerizing",colnames_words)
riveting_colno<-match("riveting",colnames_words)
spectacular_colno<-match("spectacular",colnames_words)
cool_colno<-match("cool",colnames_words)
awesome_colno<-match("awesome",colnames_words)
thrilling_colno<-match("thrilling",colnames_words)
cliched_colno<-match("cliched",colnames_words)
sucks_colno<-match("sucks",colnames_words)
boring_colno<-match("boring",colnames_words)
stupid_colno<-match("stupid",colnames_words)
slow_colno<-match("slow",colnames_words)
least_colno<-match("least",colnames_words)
unfortunately_colno<-match("unfortunately",colnames_words)
text_translatable2<-cbind(text_translatable[,1],text_translatable[,gripping_colno],text_translatable[,mesmerizing_colno],text_translatable[,riveting_colno],text_translatable[,spectacular_colno],text_translatable[,cool_colno],text_translatable[,awesome_colno],text_translatable[,thrilling_colno],text_translatable[,cliched_colno], text_translatable[,sucks_colno], text_translatable[,boring_colno], text_translatable[,stupid_colno], text_translatable[,slow_colno], text_translatable[,least_colno], text_translatable[,unfortunately_colno])
NCOL(text_translatable2)
colnames(text_translatable2) <- c("","gripping","mesmerizing","riveting","spectacular","cool","awesome","thrilling","cliched","sucks","boring","stupid","slow","least","unfortunately")
write.csv(text_translatable2,"fit.csv")
fit<-prcomp(text_translatable2, scale.=TRUE)
summary(fit) 
# print variance accounted for 
#write.csv(fit$x,"PCA.csv")

plot(fit,type="lines") 
# scree plot 
fit$scale
fit$scores # the principal components
head(fit$rotation)
#biplot(fit)
fit$x




data<-read.csv("scott.csv") #reading a csv file review,id,rating,class

for_regression <- cbind(fit$x, log(data[,3]/(5-data[,3]),base=exp(1))) #<- adding ratings to the matrix
NCOL(for_regression)
colnames(for_regression)[NCOL(for_regression)] <- "log_odds"



#Logit regression  
for_regression_2<-data.frame(for_regression)
#tail(colnames(for_regression_2))



train_data<-for_regression_2[1:300,]
test_data<-for_regression_2[301:600,]

#training data
logr<- glm (log_odds ~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10, data=train_data)

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

for_accuracy_matrix$predicted_class <- ifelse(for_accuracy_matrix$predicted_value <=.11, 0, 1)
table(for_accuracy_matrix[, c("actual_class", "predicted_class")]) #accuracy matrix

#accuracy %
nrow(for_accuracy_matrix[for_accuracy_matrix$actual_class==for_accuracy_matrix$predicted_class,])/nrow(for_accuracy_matrix)*100
#31.7%

validate_data<-for_regression_2[601:900,]

validate_values<-predict(logr,newdata=validate_data, type="response", se.fit=T) 
write.csv(validate_values,"validate_predictors.csv")

validate_predictors<-read.csv("validate_predictors.csv") 
for_accuracy_matrix <- cbind(validate_predictors,1/(1+exp(-validate_predictors$fit)),data[601:900,4],0)
colnames(for_accuracy_matrix)[5] <- "predicted_value"
colnames(for_accuracy_matrix)[6] <- "actual_class"
colnames(for_accuracy_matrix)[7] <- "predicted_class"

for_accuracy_matrix$predicted_class <- ifelse(for_accuracy_matrix$predicted_value <=.11, 0, 1)

#head(for_accuracy_matrix)
table(for_accuracy_matrix[, c("actual_class", "predicted_class")]) #accuracy matrix

#accuracy %
nrow(for_accuracy_matrix[for_accuracy_matrix$actual_class==for_accuracy_matrix$predicted_class,])/nrow(for_accuracy_matrix)*100

