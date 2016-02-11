library(plyr)
library(dplyr)
library(igraph)
library(tm)
library(lazyeval)
library(caret)
library(SnowballC)
library(ROCR)
library(wordcloud)
set.seed(666)

full <- read.table('data/full.tsv', sep='\t',stringsAsFactors = F, header=T)
wrestler.names <-sort(as.character(filter(data.frame(table(c(full$winner.name, full$loser.name)), stringsAsFactors = F), Freq > 21)$Var1))

get.auc.graph <- function(fitted.results, test, id){
  pr <- prediction(fitted.results, test$win.loss)
  prf <- performance(pr, measure = "tpr", x.measure = "fpr")
  jpeg(filename=paste('viz/auc/',id,'.jpg',sep=''))
  plot(prf)
  dev.off()
}

get.area.under.curve <- function(fitted.results, test){
  pr <- prediction(fitted.results, test$win.loss)
  auc <- performance(pr, measure = "auc")
  auc <- auc@y.values[[1]]
  return(round(auc, 3))
}

get.beaten.by <- function(name){
  losses <- get.losses(name)
  
  beatens.by <- data.frame(sort(table(losses$winner.name),decreasing=T)[2:5])
  names(beatens.by) <- 'count'
  return(beatens.by)
}

get.beaten <- function(name) {
  wins <- get.wins(name)
  
  beatens <- data.frame(sort(table(wins$loser.name),decreasing=T)[2:5])
  names(beatens) <- 'count'
  return(beatens)
}

my.Logit <- function(train){ 
  set.seed(666)
  return(glm(win.loss~.,data=train,family=binomial(link='logit')))
}

get.ineffective.moves <- function(fit){
  coefs <- data.frame(summary(fit)$coef)
  coefs$name <- row.names(coefs)
  names(coefs) <- c('estimate','se','t.val','p.val', 'name')
  return(head(rbind(coefs[1,],arrange(coefs[2:nrow(coefs),],desc(estimate))),n=11)$name[2:11])
  
}

get.effective.moves <- function(fit){
  coefs <- data.frame(summary(fit)$coef)
  coefs$name <- row.names(coefs)
  names(coefs) <- c('estimate','se','t.val','p.val', 'name')
  return(rbind(coefs[1,],tail(arrange(coefs[2:nrow(coefs),],desc(estimate)),n=11))$name[2:11])
}

get.record <- function(name){
  return(round(mean(c(rep(1, nrow(filter(full, winner.name==name))), 
               rep(0, nrow(filter(full, loser.name == name))))),3))
}

graph.f <- function(name,id){
  muraco <- filter(full, winner.name == name | loser.name == name)
  
  net <- graph.data.frame(select(muraco, winner.name, loser.name), directed=T)
  x <- min(length(degree(net))-1, 21)
  
  top.opponents <-   row.names(data.frame(sort(degree(net),decreasing=T)[2:x]))
  net <- graph.data.frame(select(filter(muraco, winner.name %in% top.opponents | loser.name %in% top.opponents), winner.name, loser.name), directed=T)
  jpeg(filename=paste('viz/graph/',id,'.jpg',sep=''))
  plot(net, layout=layout.kamada.kawai, main=paste('Top Opponents for', name), asp=1)
  dev.off()
}

get.losses <- function(name){
  losses <- filter(full, loser.name == name)
}

get.wins <- function(name){
  wins <- filter(full, winner.name == name)
}

nlp.wins.f <- function(name, id){
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
  jpeg(filename = paste('viz/cloud/wins/', id, '.jpg', sep=''))
  
  out <- tryCatch({
    wordcloud(win.docs, max.words = 100, random.order = T)
  },error = function(cond){},
  finally = { dev.off()})
  
  return(data.frame(win.m))
}

nlp.losses.f <- function(name, id){
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
  jpeg(filename = paste('viz/cloud/losses/', id, '.jpg', sep=''))
  out <- tryCatch({
    wordcloud(lose.docs, max.words = 100, random.order = T)
  },error = function(cond){},
  finally = { dev.off()})
  
  return(data.frame(lose.m))
}


get.dtm <- function(w.name, id){
  df.win.dtm <- nlp.wins.f(w.name,id)
  df.lose.dtm <- nlp.losses.f(w.name, id)
  
  names<-intersect(names(df.win.dtm),names(df.lose.dtm))
  df.dtm <- rbind(select(df.win.dtm, one_of(names)),select(df.lose.dtm, one_of(names)))
  df.dtm$win.loss <- factor(c(rep(1,nrow(df.win.dtm)),rep(0,nrow(df.lose.dtm))))
  return(df.dtm)
}


trainIndex <- function(dtm) {
  set.seed(666)
  return(createDataPartition(dtm$win.loss, p = .7, list = FALSE))
}

get.fitted.results <- function(name){
  set.seed(666)
  myLogit <- my.Logit(name)
  fitted.results <- predict(myLogit,newdata=select(get.test(name), -win.loss),type='response')
  return(ifelse(fitted.results > 0.5,1,0))
}

get.accuracy <- function(fitted.results, test) {
  misClasificError <- mean(fitted.results != test$win.loss)
  return(round(1-misClasificError,3))
}

get.wrestler.info <- function(name){
 out <- tryCatch({
  set.seed(666)
    muraco.w <- select(filter(full, winner.name == name), loser.id, winner.name, winner.wgt, winner.hgt)
    muraco.l <- select(filter(full, loser.name == name), winner.id, loser.name, loser.wgt, loser.hgt)
    new.names <- c('id','name','wgt','hgt')
    names(muraco.w) <- new.names
    names(muraco.l) <- new.names
    muraco <- rbind(muraco.w,muraco.l)[1,]
    
    df.dtm <- get.dtm(name, muraco$id)
    ti <- trainIndex(df.dtm)
    train <- df.dtm[ti,]
    test <- df.dtm[-ti,]
    
    fit <- my.Logit(train)
    fitted.results <- predict(fit,newdata=select(test, -win.loss),type='response')
    fitted.results <- (ifelse(fitted.results > 0.5,1,0))
    
    df1 <- data.frame(matrix(table(fitted.results,test$win.loss), ncol=2, byrow=T))
    true.positives <- df1[2,2]
    false.positives <- df1[1,2]
    true.negatives <- df1[1,1]
    false.negatives <- df1[2,1]
    
    id <- muraco$id
    wgt <- muraco$wgt
    hgt <- muraco$hgt
    record <- get.record(name)
    ineffective.moves <- paste(get.ineffective.moves(fit),collapse=',')
    effective.moves <- paste(get.effective.moves(fit),collapse=',')
    accuracy <- get.accuracy(fitted.results, test)
    
    graph.f(name,id)
    auc <- get.area.under.curve(fitted.results,test)
    get.auc.graph(fitted.results,test,id)
    
    return(data.frame(id,name,wgt,hgt, record, accuracy, effective.moves, 
                      ineffective.moves, auc, true.positives, false.positives,
                      true.negatives, false.negatives))
  },error = function(cond){},
  finally = {message(paste('processed: ', name))}
  
  )
  return(out)
}

df <- as.data.frame(ldply(lapply(wrestler.names, get.wrestler.info)))
write.table(df, 'data/shiny.tsv', sep='\t', row.names = F)
#shiny <- read.table('data/shiny.tsv', sep='\t', stringsAsFactors = F)
