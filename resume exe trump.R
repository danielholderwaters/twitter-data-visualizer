#This is my EDJ Analytics cover letter
#I would love to apply my expertise for your team
#this code finds and charts the most popular hashtags that the president
#copy and paste into Rstudio to run
#code written on a Windows computer

pack_check<-function(x){
  if (!x %in% installed.packages()) install.packages(x)
    paste(x, "was previously installed")
  else {
    paste(x, "is already installed")
  }
}
packages_ <- c("base64enc", "httpuv", "openssl", "httr", "twitteR", "rlang", "ggplot2")
sapply(packages_, pack_check)
lapply(packages_, require, character.only =TRUE)


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

find.reference <- function(vec){
  at.pattern <- "@[[:alpha:]]+"
  have.at <- grep(x = vec, pattern = at.pattern)
  at.matches <- gregexpr(pattern = at.pattern,
                           text = vec[have.at])
  extracted.at <- regmatches(x = vec[have.at], m = at.matches)
  df <- data.frame(table(tolower(unlist(extracted.at))))
  colnames(df) <- c("tag","freq")
  df <- df[order(df$freq,decreasing = TRUE),]
  return(df)
}

#printing chart of most used hashtags
dat = head(find.hashtags(txt),50)
dat2 = transform(dat,tag = reorder(tag,freq))
p <- ggplot(dat2, aes(x = tag, y = freq)) + geom_bar( stat="identity", fill = "red")
p + coord_flip() + labs(title = "Trump's Most Used Hashtags")

#printing chart of most used references (@'whoever')
dat = head(find.reference(txt),50)
dat2 = transform(dat,tag = reorder(tag,freq))
p <- ggplot(dat2, aes(x = tag, y = freq)) + geom_bar( stat="identity", fill = "red")
p + coord_flip() + labs(title = "Trump's Most Referenced")
