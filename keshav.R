library(glmnet)
library(dplyr)
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
                         vectorizer =
                           ##vectorizer = TfidfVectorizer(tokenizer=getTokens)
                           ##X = vectorizer.fit_transform(corpus)
                           n_sample =NROW(X)
                         w=0.8
                         X_train = X[0:(w*n_sample),]
                         y_train = y[0:(w*n_sample)]
                         X_test = X[((w*n_sample)+1):n_sample,]
                         y_test = y[((w*n_sample)+1):n_sample]
set.seed(0)
model_lambda <- cv.glmnet(as.matrix(X_train), as.factor(y_train),
                          nfolds = 10, alpha=1, family="binomial", type.measure="class")
best_s  <- model_lambda$lambda.1se
pred <- as.numeric(predict(model_lambda, newx=as.matrix(X_test), type="class" , s=best_s))
print(best_s)
print(sum(y_test==pred)/NROW(pred))
}  


  

