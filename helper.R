library(plyr)
library(dplyr)
library(igraph)
library(tm)
library(lazyeval)
library(caret)
library(SnowballC)
library(ROCR)
set.seed(666)

full <- read.table('data/full.tsv', sep='\t',stringsAsFactors = F, header=T)
shiny <- read.table('data/shiny.tsv', sep='\t',stringsAsFactors = F, header=T)

get.beaten.by <- function(name){
  losses <- get.losses(name)
  
  beatens.by <- data.frame(sort(table(losses$winner.name),decreasing=T)[2:5])
  names(beatens.by) <- 'count'
  return(beatens.by)
}

get.beatens <- function(name){
  wins <- get.wins(name)
  
  beatens <- data.frame(sort(table(wins$loser.name),decreasing=T)[2:5])
  names(beatens) <- 'count'
  return(beatens)
}

get.losses <- function(name){
  losses <- filter(full, loser.name == name)
}

get.wins <- function(name){
  wins <- filter(full, winner.name == name)
}

nlp.wins.f <- function(name){
  set.seed(666)
    wins<-get.wins(name)
    
    win.docs <- Corpus(VectorSource(paste(wins$loser.trademark.moves, wins$loser.finishers)))
    win.docs <- tm_map(win.docs,
                        content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')),
                        mc.cores=1)
    win.docs <- tm_map(win.docs, content_transformer(removeNumbers), lazy=T)
    win.docs <- tm_map(win.docs, content_transformer(tolower), lazy=T)
    win.docs <- tm_map(win.docs, content_transformer(removePunctuation), lazy=T)
    win.docs <- tm_map(win.docs, content_transformer(removeWords), stopwords("english"), lazy=T)
    win.docs <- tm_map(win.docs, content_transformer(stemDocument), lazy=T)
    win.docs <- tm_map(win.docs, content_transformer(stripWhitespace), lazy=T)
    win.dtm <- DocumentTermMatrix(win.docs)   
    freq <- colSums(as.matrix(win.dtm))   
    
    ord <- order(freq)   
    
    win.m <- as.matrix(win.dtm)   
    return(data.frame(win.m))
}

nlp.losses.f <- function(name){
  set.seed(666)
  losses <- get.losses(name)
    
    # create corpus
    lose.docs <- Corpus(VectorSource(paste(losses$winner.trademark.moves, losses$winner.finishers)))
    lose.docs <- tm_map(lose.docs,
                       content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')),
                       mc.cores=1)
    lose.docs <- tm_map(lose.docs, content_transformer(removeNumbers), lazy=T)
    lose.docs <- tm_map(lose.docs, content_transformer(tolower), lazy=T)
    lose.docs <- tm_map(lose.docs, content_transformer(removePunctuation), lazy=T)
    lose.docs <- tm_map(lose.docs, content_transformer(removeWords), stopwords("english"), lazy=T)
    lose.docs <- tm_map(lose.docs, content_transformer(stemDocument), lazy=T)
    lose.docs <- tm_map(lose.docs, content_transformer(stripWhitespace), lazy=T)
    lose.dtm <- DocumentTermMatrix(lose.docs)   
    freq <- colSums(as.matrix(lose.dtm))   
    
    ord <- order(freq)   
    
    lose.m <- as.matrix(lose.dtm)   
    return(data.frame(lose.m))
    # most frequent terms
    #freq[tail(ord,n=25)]
}


get.dtm <- function(name){
    df.win.dtm <- nlp.wins.f(name)
    df.lose.dtm <- nlp.losses.f(name)
    
    names<-intersect(names(df.win.dtm),names(df.lose.dtm))
    df.dtm <- rbind(select(df.win.dtm, one_of(names)),select(df.lose.dtm, one_of(names)))
    df.dtm$win.loss <- factor(c(rep(0,nrow(df.win.dtm)),rep(1,nrow(df.lose.dtm))))
    return(df.dtm)
}


trainIndex <- function(name) {
  set.seed(666)
  df.dtm <- get.dtm(name)
  return(createDataPartition(df.dtm$win.loss, p = .7, list = FALSE))
}

get.train <- function(name){
  df.dtm <- get.dtm(name)

  return(df.dtm[trainIndex(name),])
}

get.test <- function(name){
  df.dtm <- get.dtm(name)
  return(df.dtm[-trainIndex(name),])
}

get.accuracy <- function(name) {
    misClasificError <- mean(get.fitted.results(name) != get.test(name)$win.loss)
    return(paste('Accuracy:',1-misClasificError))
}

wrestler.names <-arrange(select(shiny,name),name)
