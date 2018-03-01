library(rtweet)
library(data.table)
library(tm)
library(naivebayes)
library(caret)
##-----------------function defintions----------------
#convert dtm from frequency to  presence
label <- function(x) {
  y <- ifelse(x < (-0.4), 1,
              ifelse(x < (-0.1), 2,
                     ifelse(x < 0.1, 3,
                            ifelse(x < 0.4, 4, 5))))
  y <-
    factor(
      y,
      levels = c(1, 2, 3, 4, 5),
      labels = c("Vneg", "neg", "neu", "pos", "Vpos")
    )
  y
}
convert_count <- function(x) {
  y <- ifelse(x > 0, 1, 0)
  y <- factor(y, levels = c(0, 1), labels = c("No", "Yes"))
  y
}
##----------------main-------------------------
#create document term matrix from data set

data <-
  fread(
    input = "privacy_scored.csv",
    header = T,
    fill = T,
    col.names = c("text", "textblob", "vader"),
    blank.lines.skip = T
  )
data <- data[, vader := label(vader)]
data <- data[, vader := as.factor(vader)]
data <- data[, textblob := label(textblob)]
data <- data[, textblob := as.factor(textblob)]
corpus <- SimpleCorpus(VectorSource(data[, text]))
dtm <-
  DocumentTermMatrix(corpus, control = list(tokenize = scan_tokenizer, stopwords =
                                              T))
freq_terms <- findFreqTerms(dtm)[1:20]
corpus <- SimpleCorpus(VectorSource(data[, text]))
dtm <-
  DocumentTermMatrix(corpus,
                     control = list(
                       dictionary = freq_terms,
                       tokenize = scan_tokenizer,
                       stopwords = T
                     ))
#convert dtm to binnomial
dtm <- apply(dtm, 2, convert_count)
# train test creation

s <- rbinom(3881, 1, 0.70)
n <- numeric(0)
m <- numeric(0)
for (i in 1:length(s)) {
  if (s[i] == 1) {
    n <-  c(n, i)
  }
  else {
    m <-  c(m, i)
  }
}
train_NB <- dtm[n, ]
test_NB <- dtm[m, ]
# model and testing
tic("training")
classifier <- naive_bayes(train_NB, data[n, vader])
toc()
tic("prediction")
pred_test <- predict(object = classifier, newdata = test_NB)
fwrite(x = data.table(pred_test),"predicted_test_labels.csv")
pred_train <- predict(object = classifier, newdata = train_NB)
fwrite(data.table(pred_test),"predicted_train_labels.csv")
toc()
t_test <- confusionMatrix(data[m, vader] , pred_test)
t_train <- confusionMatrix(data[n, vader] , pred_train)
print(paste0(t_test$overall[1], ",", t_train$overall[1]))
##--------graph----------------------
#20
dt <- data.table(
  p = c(0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8),
  test_20 = c(0.342, 0.352, 0.335, 0.325, 0.34, 0.321, 0.347),
  train_20 = c(0.339, 0.325, 0.336, 0.344, 0.336, 0.342, 0.334),
  #50
  test_50 = c(0.34, 0.343, 0.322, 0.319, 0.321, 0.332, 0.363),
  train_50 = c(0.329, 0.335, 0.326, 0.328, 0.337, 0.334, 0.329)
)
dt <- melt(data = dt, id.vars = 1)
ggplot(dt, aes(x = p)) + geom_line(aes(y = value))

geom_line(aes(y = train_20), color = "blue", size = 1) +
  geom_line(aes(y = test_50),
            color = "red",
            linetype = "dashed",
            size = 1) +
  geom_line(aes(y = train_50), color = "red", size = 1) +
  coord_cartesian(ylim = c(0.2, 0.6)) + theme(text = element_text(size = 20)) +
  ggtitle("Naive Bayes accuracy") + xlab("Train/Test split") + ylab("Accuracy")