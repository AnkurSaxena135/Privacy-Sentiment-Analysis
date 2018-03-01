#libraries
library(data.table)
library(rtweet)
#search terms
keywords <- c("#privacymatters","#dataBreach","#identitytheft","#onlinesecurity","#freedomofInformation",
               "#cybersecurity","#cloudsecurity","#protectprivacy","#makeprivacygreatagain")
q <- paste(keywords, collapse = " OR ")
#last received tweet id
max_sid <- "933636963176136704"
for(i in seq(6400,8000,400)){
  rt <- search_tweets( q, n = 400, max_id =max_sid,
    retryonratelimit=T, include_rts = FALSE, lang= "en")    
  
  print("successfully received data from twitter!")
  dt <- data.table(rt)
  max_sid <- min(dt[,status_id])
  
  print("exporting data")
  fwrite(x = dt,file = paste("raw_",i,".csv",sep = ""))
  fwrite(x = dt[,.(created_at,screen_name,text)],file = paste("fine_",i,".csv",sep = ""))
  
  print("going to sleep....")
  Sys.sleep(1.5*60)
}
#search random tweets
keywords <- c("the","and")
q <- paste(keywords, collapse = " OR ")
rt <- search_tweets( q, n = 14000, retryonratelimit=T, include_rts = FALSE, lang= "en")    
dt <- data.table(rt)
fwrite(x = dt[,.(created_at,text)], file = "random_raw.csv")