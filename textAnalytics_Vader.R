library(data.table)
library(ggplot2)
library(tm)
library(qdap)
library(textstem)
library(wordcloud)
#------------------function definitions and variables------------------
#stop words
stops <- c("privacy","can","see","many","cybersecurity","will","protect","data","need","keep","now","get","just","like","know")
#generateWordCloud
generateWordcloud <- function(text,palette){
  corpus <- VCorpus(VectorSource(text))
  corpus <- lemmatizeCorpus(corpus)
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
            max.words = 60,scale = c(4,2),rot.per = 0.3, min.freq = 5,
            random.order = F, colors = pal)
}
#lemmatize and remove stop words corpus
lemmatizeCorpus <- function(data){
  data <- lemmatize_words(data)
  #data <- tm_map(data,removeWords,stops)
  return(data)
}
#----------------main---------------------------------
#load labelled datasets
labelledTweet <- fread(input = "C:/Data/Ankur/Fall17/CSC555/R3/Privacy-Sentiment-Analysis/privacy_scored.csv",
                    header = T,col.names =c("tweet","textblob","vader") )
#random <- fread(input = "random.csv",header = F,col.names = "tweet",sep = "\n",blank.lines.skip = T)
random_score <- fread(input = "random_scored.csv",header = T,na.strings = "NA",blank.lines.skip = T)
#random_score <- fread(input = "random_textblob.csv",header = F,col.names = "score",na.strings = "NA",sep = "\n",blank.lines.skip = T)
random <- random_score[,vader]
random <- random[sample(.N,3881)]
labelledTweet <- cbind(labelledTweet,random)
ggplot(labelledTweet)+ geom_freqpoly(aes(textblob),size=1,color="red",show.legend = T)+ 
  geom_freqpoly(aes(random),size=1,color="blue",show.legend = T)+
  theme(text = element_text(size = 20)) +ggtitle("Random vs Privacy tweets sentiments")+
  xlab("Polarity")
#histogram plot
#png(filename = "vader_hist.png")
rand <- sample(random[,score],3881)
ggplot(labelledTweet)+ geom_freqpoly(aes(x=vader),bins = 40,color="purple",size=1)+ 
  geom_freqpoly(aes(x=textblob),bins = 40,color="blue",size=1)+ 
  theme(text = element_text(size = 20)) +ggtitle("Textblob vs Vader distribution")+
  xlab("Polarity")
print(p)
dev.off()
#ggplot(labels_unprocessed)+ geom_histogram(aes(score),bins = 30)
neg_very <- labelledTweet[vader< (-0.4),.(tweet,vader,textblob,sentiment= "negative_extreme")]
neg_some <- labelledTweet[vader >=(-0.4) & vader<(-0.1),.(tweet,vader,textblob,sentiment= "negative_some")]
neu <- labelledTweet[vader>=(-0.1) & vader<0.1,.(tweet,vader,textblob,sentiment= "neutral")]
pos_some <- labelledTweet[vader<=0.4 & vader>0.1,.(tweet,vader,textblob,sentiment= "positive_some")]
pos_very <- labelledTweet[vader>0.4,.(tweet,vader,textblob,sentiment="positive_very")]
main <- rbind(neg_very,neg_some,neu,pos_some,pos_very)
t <- main[,.N,sentiment]
pal <- brewer.pal(9, "BuPu")[-(1:4)]
ggplot(t,aes(x=sentiment,y=N))+ geom_bar(stat="identity",fill=pal)+
  theme(text = element_text(size = 12)) +ggtitle("Sentiment group distribution")+
  xlab("Sentiment labels")
png(filename="vader_neg_very.png")
generateWordcloud(neg_very[,tweet],"RdOr")
dev.off()
png(filename="vader_neg_some.png")
generateWordcloud(neg_some[,tweet],"OrRd")
dev.off()
png(filename="vader_neu.png")
generateWordcloud(neu[,tweet],"RdPu")
dev.off()
png(filename="vader_pos_some.png")
generateWordcloud(pos_some[,tweet],"OrRd")
dev.off()
png(filename="vader_pos_very.png")
generateWordcloud(pos_very[,tweet],"GnBu")
dev.off()