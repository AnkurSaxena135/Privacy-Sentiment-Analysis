import csv
import re
import pandas as pd
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer

df = pd.read_csv('results.csv')
csvFile=open('labelled_tweets.csv','w')
csvWriter= csv.writer(csvFile)
tweets_list = list(df.iloc[:])
# print len(tweets_list)

un_labelled = []
for tweet in tweets_list:
	tweet = re.sub(
    	"(@[A-Za-z0-9]+)|([^0-9A-Za-z \t])|(\w+:\/\/\S+)", " ", tweet)
	words = [w.lower() for w in tweet.strip().split() if len(w) >= 3]
	un_labelled.append(' '.join(words))
analyzer = SentimentIntensityAnalyzer()
for sentence in un_labelled:	
	vs = analyzer.polarity_scores(sentence)
	csvWriter.writerow([sentence, vs["compound"]])	
csvFile.close()