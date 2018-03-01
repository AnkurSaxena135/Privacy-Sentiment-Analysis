library(data.table)
library(ggplot2)
library(e1071)
library(tm)
library(qdap)
library(textstem)
library(wordcloud)
#------------------function definitions and variables------------------
#additional stop words
stops <- c("privacy","can","cybersecurity","will","protect","data","need","keep","now","get","just","like","know")
#generateWordCloud
generateWordcloud <- function(text,palette){
  corpus <- VCorpus(VectorSource(text))
  corpus <- lemmatize_words(corpus)
  dtm <- TermDocumentMatrix(corpus,control=list(tokenize=scan_tokenizer ,stopwords=T,weighting="weightBin",stopwords=T))
  #remove sparse terms
  dtm <- removeSparseTerms(dtm,sparse = 0.99)
  #wordcloud 
  wordFreq <- sort(rowSums(as.matrix(dtm)), decreasing = T)
  removeind <- names(wordFreq) %in% stops
  wordFreq <- wordFreq[!removeind]
  #print(wordFreq[1:10])
  pal <- brewer.pal(9, palette)[-(1:4)]
  wordcloud(words = names(wordFreq), freq = wordFreq,
            max.words = 60,scale = c(4,1),rot.per = 0.3, min.freq = 5,
            random.order = F, colors = pal)
}
#----------------main---------------------------------
#load labelled datasets
labelledTweet <- fread(input = "C:/Data/Ankur/Fall17/CSC555/R3/Privacy-Sentiment-Analysis/labelled_processed_tweets_textblob.csv",
                    header = F,col.names =c("tweet","score") )
#labels_unprocessed <- fread(input = "C:/Data/Ankur/Fall17/CSC555/R3/Privacy-Sentiment-Analysis/labelled_unprocessed_tweets.csv",
#                    header = F,col.names =c("tweet","score") )
#histogram plot
png(filename = "textblob_hist.png")
p <- ggplot(labelledTweet)+ geom_histogram(aes(score),bins = 30)
print(p)
dev.off()
#ggplot(labels_unprocessed)+ geom_histogram(aes(score),bins = 30)
neg_very <- labelledTweet[score< (-0.4),.(tweet,sentiment= "very negative")]
neg_some <- labelledTweet[score>=(-0.4) & score<(-0.1),.(tweet,sentiment= "somewhat negative")]
neu <- labelledTweet[score>=(-0.1) & score<0.1,.(tweet,sentiment= "neutral")]
pos_some <- labelledTweet[score<=0.4 & score>0.1,.(tweet,sentiment= "somewhat positive")]
pos_very <- labelledTweet[score>0.4,.(tweet,sentiment="very positive")]

png(filename="textblob_neg_very.png")
generateWordcloud(neg_very[,tweet],"GnBu")
dev.off()
png(filename="textblob_neg_some.png")
generateWordcloud(neg_some[,tweet],"OrRd")
dev.off()
png(filename="textblob_neu.png")
generateWordcloud(neu[,tweet],"RdPu")
dev.off()
png(filename="textblob_pos_some.png")
generateWordcloud(pos_some[,tweet],"OrRd")
dev.off()
png(filename="textblob_pos_very.png")
generateWordcloud(pos_very[,tweet],"GnBu")
dev.off()