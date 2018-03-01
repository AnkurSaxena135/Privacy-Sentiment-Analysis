import csv
import re
import pandas as pd
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
import sys
from textblob import TextBlob

df = pd.read_csv('processed_tweets.csv')
processed_tweets_list = df['text'].tolist()

# un_labelled = []
# Preprocessing 
# for tweet in tweets_list:
# 	tweet = re.sub(
#     	"(@[A-Za-z0-9]+)|([^0-9A-Za-z \t])|(\w+:\/\/\S+)", " ", tweet)
# 	words = [w.lower() for w in tweet.strip().split() if len(w) >= 3]
# 	un_labelled.append(' '.join(words))


# Sentiment Analysis using VaderSentiment
# analyzer = SentimentIntensityAnalyzer()
# for sentence in un_labelled:	
# 	vs = analyzer.polarity_scores(sentence)
# 	print sentence, ",", vs["compound"]


# Sentiment analysis using TextBlob on processed tweets

for tweet in processed_tweets_list:

	blob = TextBlob(tweet.decode('utf-8'))
	print tweet, blob.sentiment.polarity
