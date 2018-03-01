library(data.table)
library(tm)
library(tidytext)
library(caret)
library(topicmodels)
##----------------main----------------------
#source: labelled dataset
csvs <- list.files("C:/Data/Ankur/Fall17/CSC555/R3/Privacy-Sentiment-Analysis/DataSet/Labelled")
dt <- data.table()
for(csv in csvs){
  dt <- rbind(dt,fread(input = paste0("./DataSet/Labelled/",csv),header = T,select = c(1,3,4,6),col.names = c("time","text","A","match")))
}
privacyDataset <- dt[A==1 & match==1,.(time,text)]
privacyDataset[,time:=gsub("T[:0-9]+Z","",time)][,time:=gsub("2017-","",time)]
privacyDataset[,.N,time]
#12 nov
dt_12 <- privacyDataset[time=="11-12"]
corpus12 <- SimpleCorpus(VectorSource(dt_12[,text]))
dtm12 <-  DocumentTermMatrix(corpus, control = list(tokenize = scan_tokenizer, stopwords =T))
f <- freq_terms(corpus12,stopwords = Top200Words,at.least = 4,top = 10)
ldamodel <- LDA(dtm12,6)
ap_topics <- tidy(ldamodel, matrix = "beta")
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()













#13 nov
dt_13<- privacyDataset[time=="11-13"]
corpus13 <- SimpleCorpus(VectorSource(dt_13[,text]))
dtm13 <-  DocumentTermMatrix(corpus, control = list(tokenize = scan_tokenizer, stopwords =T))
f13 <- freq_terms(corpus13,stopwords = Top200Words,at.least = 4,top = 20)
dt_14 <- privacyDataset[time=="11-14"]
corpus14 <- SimpleCorpus(VectorSource(dt_14[,text]))
dtm14 <-  DocumentTermMatrix(corpus, control = list(tokenize = scan_tokenizer, stopwords =T))

dt_15 <- privacyDataset[time=="11-15"]
corpus15 <- SimpleCorpus(VectorSource(dt_15[,text]))
dtm15 <-  DocumentTermMatrix(corpus, control = list(tokenize = scan_tokenizer, stopwords =T))

dt_16 <- privacyDataset[time=="11-16"]
corpus16 <- SimpleCorpus(VectorSource(dt_16[,text]))
dtm16 <-  DocumentTermMatrix(corpus, control = list(tokenize = scan_tokenizer, stopwords =T))

dt_17 <- privacyDataset[time=="11-17"]
corpus17 <- SimpleCorpus(VectorSource(dt_17[,text]))
dtm17 <-  DocumentTermMatrix(corpus, control = list(tokenize = scan_tokenizer, stopwords =T))

dt_18 <- privacyDataset[time=="11-18"]
corpus18 <- SimpleCorpus(VectorSource(dt_18[,text]))
dtm18 <-  DocumentTermMatrix(corpus, control = list(tokenize = scan_tokenizer, stopwords =T))

dt_19 <- privacyDataset[time=="11-19"]
corpus19 <- SimpleCorpus(VectorSource(dt_19[,text]))
dtm19 <-  DocumentTermMatrix(corpus, control = list(tokenize = scan_tokenizer, stopwords =T))

dt_20 <- privacyDataset[time=="11-20"]
corpus20 <- SimpleCorpus(VectorSource(dt_20[,text]))
dtm20 <-  DocumentTermMatrix(corpus, control = list(tokenize = scan_tokenizer, stopwords =T))

dt_21 <- privacyDataset[time=="11-21"]
corpus21 <- SimpleCorpus(VectorSource(dt_21[,text]))
dtm21 <-  DocumentTermMatrix(corpus, control = list(tokenize = scan_tokenizer, stopwords =T))

dt_22 <- privacyDataset[time=="11-22"]
corpus22 <- SimpleCorpus(VectorSource(dt_22[,text]))
dtm22 <-  DocumentTermMatrix(corpus, control = list(tokenize = scan_tokenizer, stopwords =T))

dt_23 <- privacyDataset[time=="11-23"]
corpus23 <- SimpleCorpus(VectorSource(dt_23[,text]))
dtm23 <-  DocumentTermMatrix(corpus, control = list(tokenize = scan_tokenizer, stopwords =T))

f12 <- data.table(freq_terms(corpus12,stopwords = Top200Words,at.least = 4,top = 100))
f13 <- data.table(freq_terms(corpus13,stopwords = Top200Words,at.least = 4,top = 100))
f14 <- data.table(freq_terms(corpus14,stopwords = Top200Words,at.least = 4,top = 100))
f15 <- data.table(freq_terms(corpus15,stopwords = Top200Words,at.least = 4,top = 100))
f16 <- data.table(freq_terms(corpus16,stopwords = Top200Words,at.least = 4,top = 100))
f17 <- data.table(freq_terms(corpus17,stopwords = Top200Words,at.least = 4,top = 100))
f18 <- data.table(freq_terms(corpus18,stopwords = Top200Words,at.least = 4,top = 100))
f19 <- data.table(freq_terms(corpus19,stopwords = Top200Words,at.least = 4,top = 100))
f20 <- data.table(freq_terms(corpus20,stopwords = Top200Words,at.least = 4,top = 100))
f21 <- data.table(freq_terms(corpus21,stopwords = Top200Words,at.least = 4,top = 100))
f22 <- data.table(freq_terms(corpus22,stopwords = Top200Words,at.least = 4,top = 100))
f23 <- data.table(freq_terms(corpus23,stopwords = Top200Words,at.least = 4,top = 100))

trends <- fread("trends.csv")
names(trends) <- c("date","google","tweet","topic")
ggplot(trends,aes(x=date,color=topic))+geom_path(aes(y=google),size=1)+geom_path(aes(y=tweet),size=1)+
  xlab("date")+ylab("popularity")+ggtitle("Trend analysis")+
  facet_wrap(~topic,ncol=1)+
  theme(text = element_text(size=20))
