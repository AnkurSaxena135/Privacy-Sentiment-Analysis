corpus <- SimpleCorpus(VectorSource(test[,tweet]))
dtm <-
  DocumentTermMatrix(corpus, control = list(tokenize = scan_tokenizer, stopwords =
                                              T))
dtm <- apply(dtm, 2, convert_count)
pred_rand <- predict(object = classifier, newdata = dtm)
pred <- 1:length(pred_test)
for(x in 1:length(pred_test)){
  ##print(class(pred_rand[x]))
  #print(as.character(pred_rand[x]))
  pred[x]=switch(as.character(pred_test[x]),"Vneg"=1,"neg"=2,"neu"=3,"pos"=4,"Vpos"=5)
}
fwrite(data.table(pred),"pred_random.csv")
fwrite(data.table(pred),"pred_privacy.csv")