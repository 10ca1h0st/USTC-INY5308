library(stringr)
library(tm)
library(wordcloud)

emails <- list.files("maildir/", full.names=T, recursive=T)
emails <- emails[grep("/inbox", emails)]

texts = c()

for(emailname in emails){
  text = readLines(emailname,warn=F)
  text = paste(text,collapse="")
  texts = append(texts,text)
}
corpus = VCorpus(VectorSource(texts))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)

corpus <- tm_map(corpus, removeWords, 
                 c("email","from","sent","to","time","enron","day","contenttype","new","original",
                   "messageid","textplain","xfolder","subject",
                   #"xfrom","xto","xcc","xbcc","xorigin","xfilename",
                   "messagefrom","information","please",
                   stopwords('english'))
)
dtm = DocumentTermMatrix(corpus)
tdm = t(dtm)

#return dtm
Do.i = function(){
  dtm
}

#plot wordcloud
Do.ii = function(){
  png("enron-wordcloud-yyg.png",res=200)
  freq = data.frame(sort(colSums(as.matrix(dtm)), decreasing=TRUE))
  wordcloud(rownames(freq), freq[,1], max.words=40, colors=brewer.pal(1, "Dark2"))
  dev.off()
}

#return TF TermDocumentMatrix
Do.iii = function(){
  dtm_tf = DocumentTermMatrix(corpus,control = list(weighting = weightTf))
  t(dtm_tf)
}

#return TF-IDF TermDocumentMatrix
Do.iv = function(){
  dtm_tfidf = DocumentTermMatrix(corpus,control = list(weighting = weightTfIdf))
  t(dtm_tfidf)
}


#Do.i()
Do.ii()
#Do.iii()
#Do.iv()