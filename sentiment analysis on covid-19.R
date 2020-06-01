# required packages
library(twitteR)
library(SentimentAnalysis)
library(plyr)
library(RCurl)
library(httr)
library(tm)
library(wordcloud)
library(syuzhet)
library(ggplot2)
library(ROAuth)
library(wordcloud2)
library(RColorBrewer)
consumer_key <- ''
consumer_secret <- ''
access_token <- ''
access_secret <- ''
setup_twitter_oauth(consumer_key ,consumer_secret, access_token,  access_secret )
virus11 = searchTwitter("#MigrantLabourersDying", lang="en", n=10000, since="2020-03-18")
virus111 = strip_retweets(virus11)
tweets.df = twListToDF(virus111)
tweets.df$text=gsub("&amp", "", tweets.df$text)
tweets.df$text = gsub("&amp", "", tweets.df$text)
tweets.df$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets.df$text)
tweets.df$text = gsub("@\\w+", "", tweets.df$text)
tweets.df$text = gsub("[[:punct:]]", "", tweets.df$text)
tweets.df$text = gsub("[[:digit:]]", "", tweets.df$text)
tweets.df$text = gsub("http\\w+", "", tweets.df$text)
tweets.df$text = gsub("[ \t]{2,}", "", tweets.df$text)
tweets.df$text = gsub("^\\s+|\\s+$", "", tweets.df$text)
tweets.df$text <- iconv(tweets.df$text, "UTF-8", "ASCII", sub="")
# Emotions for each tweet using NRC dictionary
emotions <- get_nrc_sentiment(tweets.df$text)
emo_bar = colSums(emotions)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])
View(emo_sum)


#create plots

library(plotly)
p <- plot_ly(emo_sum, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="coronavirusoutbreak")
p
colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
fig1 <- plot_ly(emo_sum, labels = ~emotion, values =~count, type = 'pie',
                textposition = 'inside',
                textinfo = 'label+percent',
                insidetextfont = list(color = '#FFFFFF'),
                hoverinfo = 'text',
                text = ~paste('@', count),
                marker = list(colors = colors,
                              line = list(color = '#FFFFFF', width = 1)),
                #The 'pull' attribute can also be used to create space between the sectors
                showlegend = FALSE)
fig2 <- fig1 %>% layout(title = 'OUTBREAK',
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
fig2 <- fig1 %>% layout(title = 'migrant workers dying',
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
fig2


wordcloud_tweet = c(
  paste(tweets.df$text[emotions$anger
                       > 0], collapse=" "),
  paste(tweets.df$text[emotions$anticipation > 0], collapse=" "),
  paste(tweets.df$text[emotions$disgust > 0], collapse=" "),
  paste(tweets.df$text[emotions$fear > 0], collapse=" "),
  paste(tweets.df$text[emotions$joy > 0], collapse=" "),
  paste(tweets.df$text[emotions$sadness > 0], collapse=" "),
  paste(tweets.df$text[emotions$surprise > 0], collapse=" "),
  paste(tweets.df$text[emotions$trust > 0], collapse=" ")
)



# create corpus
corpus = Corpus(VectorSource(wordcloud_tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c(stopwords("english")))
corpus = tm_map(corpus, stemDocument)
tdm = TermDocumentMatrix(corpus)
# convert as matrix
#tdm = as.matrix(tdm)
#tdmnew <- tdm[nchar(rownames(tdm)) < 11,]
# column name binding
#colnames(tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
#colnames(tdmnew) <- colnames(tdm)
#comparison.cloud(tdmnew, random.order=FALSE,
#colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
#title.size=1, max.words=250, scale=c(2.5, 0.4),rot.per=0.4)
#tdm = TermDocumentMatrix(corpus)
matrix <- as.matrix(tdm)
words <- sort(rowSums(matrix),decreasing=TRUE)
df <- data.frame(word = names(words),freq=words)
set.seed(1234) # for reproducibility
wordcloud2(data=df, size=1.6, color='random-dark')
wordcloud2(data=df, size = 1,shape = 'pentagon')
letterCloud(data=df,'R',size=1)
wordcloud2(data=df, size = 1,shape = 'star')
wordcloud2(data=df, size = 2,
           color = "random-light", backgroundColor = "grey")
wordcloud2(data=df, size = 2, minRotation = -pi/2, maxRotation = -pi/2)
wordcloud2(data=df, size = 2, minRotation = -pi/6, maxRotation = -pi/6,
           rotateRatio = 1)
wordcloud2(data=df, size = 2, minRotation = -pi/2, maxRotation = -pi/2)
