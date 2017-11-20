library(glmnet)
library(dplyr)
library(rpart)
library(e1071)
library(rms)
library(ggplot2)
library(lattice)
library(survival)

getTokens <- function(input)
{
  tokensbyslash = strsplit(input,"/")
  allTokens <- vector()
  for(i in tokensbyslash)
  {
    tokens=strsplit(i,"-")
  }
  tokensByDot <- vector()
  amp <- unlist(tokens)
  remove="com"
  temptokens = strsplit(amp,"[.]")
  der <- d[! d %in% remove ]
  return(allTokens)
}
Tl <- function()
  {
  all_url='/home/mifi/Desktop/R_project_detecting _malicious_url/original_dataset/data.csv'
  all_url_csv <- read.csv(all_url)
  all_url_data = as.data.frame(all_url_csv)
  all_url_data = as.vector(all_url_data)
  all_url_data <- sample((all_url_data))
                         for(d in all_url_data)
                         {
                           y=d[2]
                         }
                         for(d in all_url_data)
                         {
                           corpus=d[1]
                         }
                           vectorizer = TfidfVectorizer(tokenizer=getTokens)
                           X = vectorizer.fit_transform(corpus)
                           n_sample =NROW(X)
                         w=0.8
                         X_train = X[0:(w*n_sample),]
                         y_train = y[0:(w*n_sample)]
                         X_test = X[((w*n_sample)+1):n_sample,]
                         y_test = y[((w*n_sample)+1):n_sample]
set.seed(0)
##1.Logistic Regression
model_lambda <- cv.glmnet(as.matrix(X_train), as.factor(y_train),
                          nfolds = 10, alpha=1, family="binomial", type.measure="class")
pred <- as.numeric(predict(model_lambda, newx=as.matrix(X_test), type="class" , s=best_s))
Lg_sum=(y_test==pred)/NROW(pred)
print("Logistic Regression prediction score:",Lg_sum)
##2.Linear Regression
x <- cbind(X_train,y_train)
linear <- lm(y_train ~ ., data=x)
summary(linear)
#Output for linear regression is:
ln_predict = predict(linear,X_test)
print("Linear Regression Prediction score is:",ln_predict)
##3.Decision Tree
fit <- rpart(y_train ~ ., data = x,method="class")
summary(fit)
DT_predict = predict(fit,X_test)
print("Decision Tree Prediction Score:",DT_predict)
##4.SVM(Support Vector Machine )
fit1 <-svm(y_train ~ ., data = x)
summary(fit1)
Svm_prediction = predict(fit1,X_test)
print("SVM prediction score is:",Svm_prediction)
##5.Naive Bayes
fit2 <-naiveBayes(y_train ~ ., data = x)
summarise(fit2)
Naive_pred = predict(fit2,X_test)
print("Naive bayes Prediction Score is :",Naive_pred)
##6.KNN(K-Nearest Neighbours)
fit3 <-knn(y_train ~ ., data = x,k=5)
summary(fit3)
KNN_pred= predict(fit3,X_test)
print("KNN Score is:",KNN_pred)
##NOW plotting a Graph for all the prediction
p <- rbind(Lg_sum,ln_predict,DT_predict,Svm_prediction,Naive_pred,KNN_pred)
plot(p,varypred=TRUE,adj,subtitle=FALSE)


}  
Tl()


