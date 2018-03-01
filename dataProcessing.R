library(data.table)
library(tm)
library(textstem)
library(qdap)
library(wordcloud)
##----------function definitions--------------------
#remove patterns
removePatterns <- function(data){
  #non-ascii symbols
  t1 <- gsub("[^\x20-\x7E]", "", data)
  #urls
  t2 <- gsub("https?[^ ]+", "", t1)
  #mentions
  t3 <- gsub("@[^ ]+","",t2)
  #ampersands
  t4 <- gsub("&amp;"," ", t3)
  #not <-  n't
  t5 <- gsub("n't"," not",t4)
  #non alphabets
  t6 <- gsub("[^[:alpha:]']"," ", t5)
  return(t6)
}
#clean corpus
cleanCorpus <- function(data){
  data <- tm_map(data, content_transformer(tolower))
  data <- lemmatize_words(data)
  data <- tm_map(data, stripWhitespace)
  return(data)
}
#process and print dataset
processDataset <- function(dataset,filename){
  text <- removePatterns(dataset)
  corpus <- SimpleCorpus(VectorSource(text))
  corpus <- cleanCorpus(corpus)
  toLabelData <- data.table(text=get("content",corpus),stringsAsFactors = F)
  fwrite(x=toLabelData,file = filename,row.names = F,col.names = F,na = "NA")
  return(corpus)
  
}
#generate wordcloud w/o stopwords
generateWordcloud <- function(corpus,stops,max_words){
  dtm <- TermDocumentMatrix(corpus,control=list(tokenize=scan_tokenizer, stopwords=T))
  dtm <- removeSparseTerms(dtm,sparse = 0.99)
  wordFreq <- sort(rowSums(as.matrix(dtm)), decreasing = T)
  removeind <- names(wordFreq) %in% stops
  wordFreq <- wordFreq[!removeind]
  pal <- brewer.pal(9, "Paired")[-(1:4)]
  wordcloud(words = names(wordFreq), freq = wordFreq,
            max.words = max_words,scale = c(4,2),rot.per = 0.3, min.freq = 5,
            random.order = F, colors = pal)
  
}
##------------main program--------------------
#source: random tweets
randomTweets <- fread("C:/Data/Ankur/Fall17/CSC555/R3/Privacy-Sentiment-Analysis/random_raw.csv")
randomDataset <- randomTweets[,text]
r_corpus <- processDataset(randomDataset,'random.csv')
r_stops <- c("can","will","data","need","keep","now","get","just","like","know","much","many","new","got","one","want","see","dont","let")
generateWordcloud(r_corpus,r_stops,50)
#source: labelled dataset
csvs <- list.files("C:/Data/Ankur/Fall17/CSC555/R3/Privacy-Sentiment-Analysis/DataSet/Labelled")
dt <- data.table()
for(csv in csvs){
  dt <- rbind(dt,fread(input = paste0("./DataSet/Labelled/",csv),header = T,select = c(3,4,6),col.names = c("text","A","match")))
}
privacyDataset <- dt[A==1 & match==1,text]
p_corpus <- processDataset(privacyDataset,'privacy.csv')
p_stops <- c("can","will","data","privacy","protect","use","cybersecurity","security","need","keep","now","get","just","like","know","much","many","new","got","one","want","see","dont","let")
generateWordcloud(p_corpus,p_stops,50)
##-----------graphs-------------------------
#keywords
hashtags <- regmatches(privacyDataset,gregexpr("#(\\d|\\w)+",privacyDataset))
hashtags <- unlist(hashtags)
hashtags <- gsub("#","",hashtags)
hashtags <- tolower(hashtags)
hashtags <- data.table(hashtags)
num <- hashtags[,.N,hashtags][order(-N)]
names(num) <- c("hashtags","Count")
freq_tags <- num[3:28]
ggplot(freq_tags,aes(x= hashtags,y=Count,fill=Count))+geom_bar(stat="identity")+
  coord_flip()+theme(text = element_text(size=20))+ggtitle("Top 25 hashtags")
freq_tags <- num[29:50]
ggplot(freq_tags,aes(x= hashtags,y=Count,fill=Count))+geom_bar(stat="identity")+
  coord_flip()+theme(text = element_text(size=20))+ggtitle("Top 25-50 hashtags")
# kappa score
Cohkappa <- data.table(score=c(0,0.5501662429,0.9051053331,0.9586318621,0.8930272329,
           0.9574015901,0.8943017361,0.9380716011,0.9541356367,
           0.9407289505,0.8432550032,0.8772519636,0.8205634695,
           0.8014304136,0.9232207957,0.8334250933,0.828391293,0.8561016607,0.9524010595),
           ind=0:18)
Cohkappa <- Cohkappa[,avg:=cumsum(score)]
for(i in seq_along(1:19)) Cohkappa[i,avg:=avg/i]
ggplot(Cohkappa,aes(x=ind))+geom_step(aes(y=score),color="red",size=1)+geom_line(aes(y=avg),color="blue",size=1)+
  geom_abline(slope = 0,intercept = 0.8,show.legend = T)+
  ggtitle("Kappa's score through iterations")+xlab("Iterations")+ylab("Kappa score")+
  coord_cartesian(ylim=c(0,1))+  theme(text = element_text(size=20))
##----------------end---------------------------
