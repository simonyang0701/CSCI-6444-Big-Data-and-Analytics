# Install "tm" package
#install.packages("tm")
library(tm)

# Try functions in Lecture 4
# Create a VCorpus
setwd("/Users/tianyuyang/Desktop/project2/txt/")
getwd()
SAT<-VCorpus(DirSource(".", ignore.case=TRUE, mode="text"))
SAT

# Inspect the VCorpus
inspect(SAT)

# VCorpus Structure
str(SAT)

# Extracting Documents
test1<-SAT[[1]]
test1
SATtdm <- TermDocumentMatrix(SAT)
SATtdm
# Inspecting the TDM
inspect(SATtdm[1:10,1:1])

# Document Term Frequency
test1tf <- termFreq(test1)
test1tf
test1df <-as.data.frame(test1tf)
test1df

# Corpus Management
SATlow <- tm_map(SAT, content_transformer(tolower))
SATlow
removeNumPunct <-function(x) gsub("[^[:alpha:][:space:]]*", "", x)
SATcl <- tm_map(SATlow,content_transformer(removeNumPunct))
SATcltdm <- TermDocumentMatrix(SATcl)
SATcltdm
inspect(SATcltdm[1:10,1:1])
myStopwords <- c(stopwords('english'))
myStopwords
# Removing stop words
SATstop <- tm_map(SATcl, removeWords, myStopwords)
inspect(SATstop[1:1])
# Remove sparse terms
SATtdm2<-TermDocumentMatrix(SATstop, control = list(wordLengths = c(1,Inf)))
SATtdm2
SATnosparse <-removeSparseTerms(SATtdm2,0.50)
SATnosparse

SATtdm3 <-TermDocumentMatrix(SATstop,control = list(wordlengths = c(1,Inf), weighting = weightBin))
SATtdm3
SATtdm4 <-TermDocumentMatrix(SATstop,control = list(wordlengths = c(1,Inf), weighting = weightTfIdf))
SATtdm4

# Finding Frequent Words
freq.terms <-findFreqTerms(SATtdm2,lowfreq = 5)
freq.terms
findAssocs(SATtdm2,"states",0.25)
freq.terms3 <-findFreqTerms(SATtdm3,lowfreq = 5)
freq.terms3
freq.terms4 <-findFreqTerms(SATtdm4,lowfreq = 5)
freq.terms4

# Term Frequency
term.freq <-rowSums(as.matrix(SATtdm2))
term.freq <-subset(term.freq,term.freq>=5)
df <-data.frame(term = names(term.freq), freq = term.freq)
term.freq
df

# Install "gglot2" package
#install.packages("ggplot2")
library("ggplot2")

# Plot graph
ggplot(df,aes(x=term,y=freq))+geom_bar(stat="identity")+xlab("Terms")+ylab("Count")+coord_flip()

# Clustering Terms
SATtdm2 <-removeSparseTerms(SATtdm,sparse = 0.50)
SATtdm2
distMatrix <-dist(scale(SATtdm))
distMatrix
test1 <-SATstop[[1]]
inspect(test1)

# Clustering Terms via Dendrogram
fit <-hclust(distMatrix,method = "ward.D2")
plot(fit)

# Word Cloud
word.freq <-sort(rowSums(m1),decreasing = T)
word.freq

#install.packages("wordcloud")
library("wordcloud")
pal <- brewer.pal(9,"BuGn")
pal <- pal[-(1:4)]
wordcloud(words = names(word.freq),freq = word.freq,min.freq = 3,random.order = F,colors = pal)

# Frequency Analysis
SATdtm <- DocumentTermMatrix(SATstop)
freq<-colSums(as.matrix(SATdtm))
SATdtm
length(freq)

ord <-order(freq,decreasing = TRUE)
freq[head(ord)]
freq[tail(ord)]

SATdtmr <-DocumentTermMatrix(SATstop,control = list(wordLengths=c(4,20)))
SATdtmr

freqr<-colSums(as.matrix(SATdtmr))
ordr <-order(freqr,decreasing = TRUE)
freq[head(ordr)]
freq[tail(ordr)]

# Install "quanteda" package
#install.packages("quanteda")
library("quanteda")

# Text Analysis
SAT1 <-SAT$content[1]
SAT1
SAT1txt <-SAT1[[1]]$content
SAT1txt
SAT1tokens<-tokens(SAT1txt)
SAT1tokens

# Install "syuzhet" package
#install.packages("syuzhet")
library(syuzhet)

# Text Analysis: Sentiment Analysis 
SAT1Sent <-syuzhet::get_nrc_sentiment(SAT1txt)
SAT1Sent
SAT1sdt<-rowSums(SAT1Sent)
SAT1sdt
SAT1rdt<-rowSums(SAT1Sent)
SAT1CDT<-colSums(SAT1Sent)
SAT1CDT

# Text Weighting 
SATdfm <-dfm(SAT1txt)
SATfrq <-docfreq(SATdfm)
SATfrq
SATWeights2 <-dfm_weight(SATdfm,scheme = "prop")
str(SATWeights2)
SATtfidf<-dfm_tfidf(SATdfm,scheme_tf = "count",scheme_df = "inverse")
SATtfidf@i
