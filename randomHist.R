random <- fread(input = "random.csv",header = F,col.names = "tweet",sep = "\n",blank.lines.skip = T)
random_score <- fread(input = "random_vader.csv",header = F,col.names = "score",na.strings = "NA",sep = "\n",blank.lines.skip = T)
random_score <- fread(input = "random_textblob.csv",header = F,col.names = "score",na.strings = "NA",sep = "\n",blank.lines.skip = T)
random <- cbind(random,random_score)
test <- random[sample(.N,3881)]
png(filename = "random_textblob_hist.png")
p <- ggplot(test)+ geom_freqpoly(aes(textblob),size=1,color="red",show.legend = T)+ 
                    geom_freqpoly(aes(vader),size=1,color="blue",show.legend = T)+
  theme(text = element_text(size = 20)) +ggtitle("RandomTextblob vs Vader distribution")+
  xlab("Polarity")
print(p)
dev.off()
png(filename = "random_textblob_hist.png")
p <- ggplot(test)+ geom_histogram(aes(vader),bins = 30)
print(p)
fwrite(x = random,file = "random_scored.csv",col.names = T)

random <- fread(input = "random_scored.csv",header = T,
                col.names = "tweet",blank.lines.skip = T)
