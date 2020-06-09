library(tm)
library(SnowballC) # Provides wordStem() for stemming.
library(RColorBrewer) # Generate palette of colours for plots.
library(ggplot2) # Plot word frequencies.
library(magrittr)
library(wordcloud) # Plot Word Cloud 


#preparing

reut21578 = system.file("texts", "crude", package = "tm")
reuters = VCorpus(DirSource(reut21578,
                            mode="binary"),
                            readerControl=
                              list(reader=readReut21578XMLasPlain)
                            )
dtm = DocumentTermMatrix(reuters)
tdm = t(dtm)

#already prepared


#plot histogram and return the top three most frequent terms
Do.1.2.iii <- function(){
  freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
  wf <- data.frame(word=names(freq),freq=freq)
  
  #plot histogram(term frequency > 10)
  subset(wf, freq>10)    %>%
    ggplot(aes(word, freq)) +
    geom_bar(stat="identity", fill="red", colour="yellow") +
    theme(axis.text.x=element_text(angle=45, hjust=1))
  ggsave("retuers-histogram-yyg.png")
  #record the first three terms
  theFirstThree = names(freq[1:3])
  theFirstThree
}

#plot word cloud
Do.1.2.iv = function(){
  png("retuers-wordcloud-yyg.png",res=200)
  freq = data.frame(sort(colSums(as.matrix(dtm)), decreasing=TRUE))
  wordcloud(rownames(freq), freq[,1], max.words=40, colors=brewer.pal(1, "Dark2"))
  dev.off()
}

#return TF TermDocumentMatrix
Do.1.2.v = function(){
  dtm_tf = DocumentTermMatrix(reuters,control = list(weighting = weightTf))
  t(dtm_tf)
}

#return TF-IDF TermDocumentMatrix
Do.1.2.vi = function(){
  dtm_tfidf = DocumentTermMatrix(reuters,control = list(weighting = weightTfIdf))
  t(dtm_tfidf)
}


#do some processing and compare the status before and after processing
Do.1.2.vii = function(){
  reuters = tm_map(reuters, content_transformer(tolower))
  reuters = tm_map(reuters, removeNumbers)
  reuters = tm_map(reuters, removePunctuation)
  reuters =  tm_map(reuters, stripWhitespace)
  dtm_filter = DocumentTermMatrix(reuters)
  print(t(dtm))
  print(t(dtm_filter))
}

#do bool query and return list of doc id
Do.1.2.viii = function(query="price AND oil"){
  querys = strsplit(query," AND ")[[1]]
  dtm = as.matrix(dtm)
  docnames = rownames(dtm)
  docs = c()
  for(i in 1:nrow(dtm)){
    if(all(dtm[i,querys] & rep(c(T),each=length(querys)))){
      docs = append(docs,docnames[i])
    }
  }
  docs
}


#using vertor model do query and return top three most relevant
Do.1.2.ix = function(query="price AND oil"){
  dtm_tfidf = DocumentTermMatrix(reuters,control = list(weighting = weightTfIdf))
  dtm_tfidf = as.matrix(dtm_tfidf)
  querys = strsplit(query," AND ")[[1]]
  query_vector = rep(0,each=ncol(dtm_tfidf))
  termnames = colnames(dtm_tfidf)
  for(query in querys){
    index = match(query,termnames)
    if(is.na(index)){next}
    query_vector[index] = 1
  }
  #normalize
  dtm_tfidf = dtm_tfidf/sqrt(rowSums(dtm_tfidf^2))
  result = dtm_tfidf %*% query_vector
  names(sort(result[,1],decreasing = T))[1:3]
}

#do BIM
Do.1.2.x = function(query="price AND oil"){
  dtm = as.matrix(dtm)
  querys = strsplit(query," AND ")[[1]]
  uts = c()
  pts = c()
  for(query in querys){
    #ut = dft/N
    ut = length(dtm[,query][dtm[,query]>0])/nrow(dtm)
    uts = append(uts,ut)
    #pt = 1/3+(2/3)ut
    pts = append(pts,1/3+(2/3)*ut)
  }
  names(uts) = querys
  names(pts) = querys
  RSVs = rep(0,each=nrow(dtm))
  for(i in 1:length(RSVs)){
    RSV = 0
    document = dtm[i,]
    for(query in querys){
      if(document[query]>0){
        #add 0.5 for smoothing
        RSV = RSV + log2(((pts[query]+0.5)*(1-uts[query]+0.5))/((uts[query]+0.5)*(1-pts[query]+0.5)))
      }
    }
    RSVs[i] = RSV
  }
  names(RSVs) = rownames(dtm)
  sort(RSVs,decreasing = T)
}


#running
Do.1.2.iii()
Do.1.2.iv()
#print(inspect(Do.1.2.v()[1,1:50]))
#print(inspect(Do.1.2.vi()[1,1:50]))
#Do.1.2.vii()
#Do.1.2.viii()
#Do.1.2.ix()
#Do.1.2.x()

