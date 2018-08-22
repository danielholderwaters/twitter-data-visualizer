#This is my EDJ Analytics cover letter
#I would love to apply my expertise for your team
#this code finds and charts the most popular hashtags that the president
#copy and paste into Rstudio to run
#code written on a Windows computer

library(base64enc)
install.packages("httpuv")
library(httpuv)
install.packages("openssl")
library(openssl)
install.packages("httr")
library(httr)
install.packages("twitteR")
library(twitteR)
install.packages("rlang")
library("rlang")
install.packages("ggplot2")
library(ggplot2)

key <- "QYQXoN4JdRYu3phyiCe4u3uQm"
secret <- "QAhnzSUsmpQPZ0QlhHhHiA1kjwNSeQajJoN4bT17xky80OSaSP" 
access <- "906588095897849856-aqTnwHJgQDjuAE9zYmWnaOvxjCCjqEZ"
access_secret <- "jlKquDnPgEd73OViD7vs4b4MLxYjzusmF46bYn4FZZbo1"
setup_twitter_oauth(key, secret, access_token = access, access_secret = access_secret)
user <- getUser("realDonaldTrump") #any person's persona could be replaced here
ut <- userTimeline(user, n=500)
tweetlist <- twListToDF(ut)
txt <- tweetlist$text
find.hashtags <- function(vec){
    hash.pattern <- "#[[:alpha:]]+"
  have.hash <- grep(x = vec, pattern = hash.pattern)
  hash.matches <- gregexpr(pattern = hash.pattern,
                        text = vec[have.hash])
  extracted.hash <- regmatches(x = vec[have.hash], m = hash.matches)
    df <- data.frame(table(tolower(unlist(extracted.hash))))
  colnames(df) <- c("tag","freq")
  df <- df[order(df$freq,decreasing = TRUE),]
  return(df)
}
dat = head(find.hashtags(txt),50)
dat2 = transform(dat,tag = reorder(tag,freq))
p <- ggplot(dat2, aes(x = tag, y = freq)) + geom_bar( stat="identity", fill = "red")
p + coord_flip() + labs(title = "Trump's Most Used Hashtags")