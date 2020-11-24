setwd("C:/Users/Felix/Desktop/MeinCorpus")

wd<-"C:/Users/Felix/Desktop/MeinCorpus"
dir(wd)

library(tm)

docs <- Corpus(DirSource(wd))
docs
meta(docs)

writeLines(as.character(docs[[1]]))

getTransformations()

docs <- tm_map(docs,removePunctuation)
docs <- tm_map(docs, removeNumbers)
#docs <- tm_map(docs, PlainTextDocument)
writeLines(as.character(docs[[1]]))

for (j in seq(docs)) {
docs[[j]] <- gsub("/", " ", docs[[j]])
docs[[j]] <- gsub("@", " ", docs[[j]])
docs[[j]] <- gsub("–", " ", docs[[j]])
docs[[j]] <- gsub("’", " ", docs[[j]])
docs[[j]] <- gsub("“", " ", docs[[j]])
docs[[j]] <- gsub("…", " ", docs[[j]])
docs[[j]] <- gsub("‘", " ", docs[[j]])
docs[[j]] <- gsub(")", " ", docs[[j]])
docs[[j]] <- gsub("”", " ", docs[[j]])
}
#docs <- tm_map(docs, PlainTextDocument)
writeLines(as.character(docs[[1]]))

docs <- tm_map(docs, tolower)
#docs <- tm_map(docs, PlainTextDocument)
writeLines(as.character(docs[[1]]))

length(stopwords("english"))
stopwords("english")

docs <- tm_map(docs, removeWords, stopwords("English"))
#docs <- tm_map(docs, PlainTextDocument)
writeLines(as.character(docs[[1]]))

StW<-read.table("C:/Users/ninar/OneDrive/Documents/RESEARCH/Brandenburg/Classes/Class 2/StopWords.txt")
StW

StWW<-as.character(StW$V1)
StWW

docs <- tm_map(docs, removeWords, StWW)
#docs <- tm_map(docs, PlainTextDocument)
writeLines(as.character(docs[[1]]))

docs <- tm_map(docs, stripWhitespace)
#docs <- tm_map(docs, PlainTextDocument)
writeLines(as.character(docs[[1]]))

library(SnowballC)
stemDocument("modelling", language = "english")
stemDocument("modeller", language = "english")
stemDocument("models", language = "english")

for (j in seq(docs)) {
docs[[j]]<-stemDocument(docs[[j]], language = "english")
}
#docs <- tm_map(docs, PlainTextDocument)
writeLines(as.character(docs[[1]]))

dtm <- DocumentTermMatrix(docs)
dtm

inspect(dtm[1:6, 20:25])

m0 <- as.matrix(dtm)
write.csv(m0, file="C:/Users/Felix/Desktop/Vorlesung WS3/Sentiment Analysis/O.folder/LAb2/DocumentTermMatrix.csv")

freqr <- colSums(as.matrix(dtm))
length(freqr)
freq <- sort(freqr, decreasing=TRUE)
head(freq, 30)
mk<-min(head(freq, 30))
mk
wf=data.frame(word=names(freq),freq=freq)

library(ggplot2)
dev.off()

p <- ggplot(subset(wf, freq>mk), aes(x = reorder(word, -freq), y = freq))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

library(wordcloud)

set.seed(42)
wordcloud(names(freq),freq, min.freq=70)
dev.off()
set.seed(142)
wordcloud(names(freq), freq, max.words=100)
dev.off()
wordcloud(names(freq), freq, min.freq=50,colors=brewer.pal(6, "Dark2"))
dev.off()
set.seed(142)
wordcloud(words = names(freq), freq = freq, min.freq = 1,
max.words=200, random.order=FALSE, rot.per=0.35,
colors=brewer.pal(8, "Dark2"))


## Lab2


library(tm)
library(SnowballC)
library(ggplot2)
library(wordcloud)
library(cluster)
library(fpc)


MyData <-read.csv("C:/Users/Felix/Desktop/Vorlesung WS3/Sentiment Analysis/O.folder/LAb2/DocumentTermMatrix.csv",
 header = TRUE, #are there column names in 1st row?
 sep = ",", #what separates rows?
 strip.white = TRUE, #strip out extra white space in strings.
 fill = TRUE, #fill in rows that have unequal numbers of columns
 comment.char = "#", #character used for comments that should not be read in
 stringsAsFactors = FALSE #Another control for deciding whether characters should be converted to factor
 )

##Transform MyData into the Frame Matrix format

dtm1 = as.data.frame.matrix(MyData)

dtm1 [1:10,1:10]
dtm<-dtm1[,-1]
dtm
##Create the Rownames of the matrix as a File names. 

filenames <- list.files(getwd(),pattern="*.txt")
filenames <-c(filenames)
filenames
rownames(dtm)
rownames(dtm)<-filenames
dtm [1:10,1:10]

##cumulative frequency of Columns and Rows of Document-Term matrix.

freq <- sort(colSums(dtm), decreasing=TRUE)
freq 

freq1 <- sort(rowSums(dtm), decreasing=TRUE)
freq1

## Build the frequency Plot for the terms which frequency is grater then 80:

wf=data.frame(word=names(freq),freq=freq)
wf
p <- ggplot(subset(wf, freq>80), aes(x = reorder(word, -freq), y = freq))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

## remove a lot of the uninteresting or infrequent words(TF-IDF transformation).

tdm<- t(dtm) # t(dtm) – transpose matrix DTM into TDM

tf <- as.matrix(tdm) 
idf <- log(ncol(tf) / (rowSums(tf != 0)))
tf[1:5,1:3]
idf[1:5]

## matrice 
idf1 <- diag(idf)
idf1[1:5,1:5]

tf_idf <- crossprod(tf, idf1)
tf_idf

colnames(tf_idf) <- rownames(tf)
tf_idf

##  First WordClouds

freq <- colSums(as.matrix(tf_idf), na.rm=FALSE)
set.seed(42)
wordcloud(names(freq),freq, max.words=50)

# Second view of wordcloud
freq = data.frame(sort(colSums(as.matrix(tf_idf)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(3, "Dark2"))

##Tasks for Independent work:

head(freq)
head(freq1)

dtm[11:12,11:13]
dtm

# if you do not have the Filenames, do the following:
filenames <- list.files(getwd(),pattern="*.txt")
filenames <-c(filenames)
filenames
rownames(dtm)
rownames(dtm)<-filenames

d1 <- dist(dtm, method="euclidian")
# make the clustering
fit <- hclust(d=d1, method="complete")
fit
plot.new()
plot(fit, hang=-1, cex=2) 

groups <- cutree(fit, k=4) # "k" defines the number of clusters you are using
rect.hclust(fit, k=4, border="red") # draw dendogram with red borders around the 4 clusters 

##Kmeans dtm*********************************************************************************

d <- dist(dtm, method="euclidian")
kfit <- kmeans(d, 3)
kfit
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)

##********************************

##TF_IDF Matrix*;

# if you do not have the Filenames, do the following:
filenames <- list.files(getwd(),pattern="*.txt")
filenames <-c(filenames)
filenames
rownames(tf_idf)
rownames(tf_idf)<-filenames 

d1 <- dist(tf_idf, method="euclidian")
# make the clustering
fit <- hclust(d=d1, method="complete")
fit
plot.new()
plot(fit, hang=-1, cex=2)

groups <- cutree(fit, k=4) # "k" defines the number of clusters you are using
rect.hclust(fit, k=3, border="red") # draw dendogram with red borders around the 4 c




##TF_IDF Matrix decreased sparsity;

tf_idf <-as.DocumentTermMatrix(tf_idf,weighting = weightTf)
tf_idf1<-removeSparseTerms(tf_idf,0.7)
tf_idf1

## clstering 2

d1 <- dist(tf_idf1, method="euclidian")
# make the clustering
fit <- hclust(d=d1, method="complete")
fit
plot.new()
plot(fit, hang=-1, cex=2)

groups <- cutree(fit, k=4) # "k" defines the number of clusters you are using
rect.hclust(fit, k=3, border="red") # draw dendogram with red borders around the 4 c

##Kmeans tf_idf1 *********************************************************************************
d <- dist(tf_idf1, method="euclidian")
kfit <- kmeans(d, 3)
kfit
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)



##9. Perform the TERMS clustering based on 
##Transposed TF_IDF Matrix;

ttd_idf <- t(tf_idf) 
ttd_idf


d1 <- dist(ttd_idf, method="euclidian")
# make the clustering
fit <- hclust(d=d1, method="complete")
fit
plot.new()
plot(fit, hang=-1, cex=0.5)

groups <- cutree(fit, k=4) # "k" defines the number of clusters you are using
rect.hclust(fit, k=3, border="red") # draw dendogram with red borders around the 4 c



ttd_idf <-as.TermDocumentMatrix(ttd_idf,weighting = weightTf)
ttd_idf1<-removeSparseTerms(ttd_idf,0.35)
ttd_idf1

##Kmeans *********************************************************************************
d <- dist(ttd_idf1, method="euclidian")
kfit <- kmeans(d, 3)
kfit
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)

## 
d1 <- dist(ttd_idf1, method="euclidian")
# make the clustering
fit <- hclust(d=d1, method="complete")
fit
plot.new()
plot(fit, hang=-1, cex=0.5)

groups <- cutree(fit, k=4) # "k" defines the number of clusters you are using
rect.hclust(fit, k=3, border="red") # draw dendogram with red borders around the 4 c






## tdm

dtm = as.data.frame.matrix(MyData)
tdm<- t(dtm) 
tdm
library(fpc)
tdmr <-as.DocumentTermMatrix(tdm,weighting = weightTf)
dtmr<-removeSparseTerms(tdmr,0.35)
dtmr
d <- dist(dtmr, method="euclidian")
kfit <- kmeans(d, 3)
kfit
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)

## Transpose TF_IDF Matrix (ttd_idf)

d <- dist(ttd_idf, method="euclidian")
kfit <- kmeans(d, 3)
kfit
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)

## (without sparsity reducing)

library(fpc)
dtm = as.data.frame.matrix(MyData)
dtmr <-as.DocumentTermMatrix(dtm,weighting = weightTf)
dtmr1<-removeSparseTerms(tdmr,0.35)
dtmr1
d <- dist(dtmr1, method="euclidian")
kfit <- kmeans(d,4)
kfit
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)






##Build bigrams-based DTM Matrix:


docs_1 <- VCorpus(DirSource(wd))
docs_1
docs_1<- tm_map(docs_1,removePunctuation)
docs_1<- tm_map(docs_1, removeNumbers)
for (j in seq(docs_1)) {
 docs_1 [[j]] <- gsub("/", " ", docs_1[[j]])
 docs_1 [[j]] <- gsub("@", " ", docs_1[[j]])
 docs_1 [[j]] <- gsub("–", " ", docs_1[[j]])
 docs_1 [[j]] <- gsub("’", " ", docs_1[[j]])
 docs_1 [[j]] <- gsub("“", " ", docs_1[[j]])
 docs_1 [[j]] <- gsub("…", " ", docs_1[[j]])
 docs_1 [[j]] <- gsub("‘", " ", docs_1[[j]])
 docs_1 [[j]] <- gsub(")", " ", docs_1[[j]])
 docs_1 [[j]] <- gsub("”", " ", docs_1[[j]])
}
docs_1<- tm_map(docs_1, tolower)
docs_1<- tm_map(docs_1, removeWords, stopwords("English"))
StW<-read.table("C:/Users/Felix/Desktop/StopWords.txt")
StWW<-as.character(StW$V1)
StWW
docs_1<- tm_map(docs_1, removeWords, StWW)
docs_1<- tm_map(docs_1, stripWhitespace) 

for (j in seq(docs_1)) {
 docs[[j]]<-stemDocument(docs_1[[j]], language = "english")
}
docs_1<- tm_map(docs_1, PlainTextDocument)






NgramTokenizer = function(x) {
 unlist(lapply(ngrams(words(x), 2), paste, collapse = " "),
 use.names = FALSE)
}
dtm_n <- DocumentTermMatrix(docs_1, control = list(tokenize = NgramTokenizer))
dtm_n
filenames <- list.files(getwd(),pattern="*.txt")
filenames <-c(filenames)
rownames(dtm_n)<-filenames

#___________Calculating the Frequency______________________________
freq_n <- sort(colSums(as.matrix(dtm_n)), decreasing=TRUE)
head(freq_n, 15)
mk<-min(head(freq_n, 15))
tail(freq_n, 15)
m<-as.matrix(dtm_n)

#___________Building the Histogtram (zipf’s law)___________________
wf=data.frame(word=names(freq_n),freq=freq_n)
wf 

p <- ggplot(subset(wf, freq>=mk), aes(x = reorder(word, -freq), y = freq))
p <- p + geom_bar(stat="identity")+ ggtitle("Histogram of Bigrams for Opinions") +labs(x="Bigrams",y="Frequency")
p <- p + theme(axis.text.x=element_text(angle=90, hjust=1, size=16))
p


##  Bigram WordClouds

freq <- colSums(as.matrix(dtm_n), na.rm=FALSE)
set.seed(42)
wordcloud(names(freq),freq, max.words=100)

##bigram-based matrix Clustering

d <- dist(dtm_n, method="euclidian")
kfit <- kmeans(d,4)
kfit
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)




##Build 3grams-based DTM Matrix:

NgramTokenizer = function(x) {
 unlist(lapply(ngrams(words(x), 3), paste, collapse = " "),
 use.names = FALSE)
}
dtm_n <- DocumentTermMatrix(docs_1, control = list(tokenize = NgramTokenizer))
dtm_n
filenames <- list.files(getwd(),pattern="*.txt")
filenames <-c(filenames)
rownames(dtm_n)<-filenames

#___________Calculating the Frequency______________________________
freq_n <- sort(colSums(as.matrix(dtm_n)), decreasing=TRUE)
head(freq_n, 15)
mk<-min(head(freq_n, 15))
tail(freq_n, 15)
m<-as.matrix(dtm_n)

#___________Building the Histogtram (zipf’s law)___________________
wf=data.frame(word=names(freq_n),freq=freq_n)
wf 

p <- ggplot(subset(wf, freq>=mk), aes(x = reorder(word, -freq), y = freq))
p <- p + geom_bar(stat="identity")+ ggtitle("Histogram of 3grams for Opinions") +labs(x="Bigrams",y="Frequency")
p <- p + theme(axis.text.x=element_text(angle=90, hjust=1, size=16))
p



## Mall_customers

myData<- read.csv("C:/Users/Felix/Desktop/Mall_customers.csv",
header=TRUE,
sep=",",
strip.white = TRUE,
fill=TRUE,
comment.char="#",
stringsAsFactors=FALSE)
MyData
myData = as.matrix(MyData)
myData
myData



## Age and Income 
select <- c(3,4)
mydata <- myData[,select]
mydata

tail(mydata)
head(mydata)

library(cluster)
library(factoextra)
set.seed(100)
km.res <- kmeans(mydata,3, nstart=25)

fviz_cluster(km.res,data=mydata,palette="jco", ggtheme= theme_minimal())

res.hc <- hclust(dist(mydata),  method = "ward.D2")
fviz_dend(res.hc, cex = 0.5, k = 3, palette = "jco")





## Age and Spending 
select <- c(3,5)
mydata <- myData[,select]
mydata

tail(mydata)
head(mydata)

library(cluster)
library(factoextra)
set.seed(100)
km.res <- kmeans(mydata,3, nstart=25)

fviz_cluster(km.res,data=mydata,palette="jco", ggtheme= theme_minimal())

res.hc <- hclust(dist(mydata),  method = "ward.D2")
fviz_dend(res.hc, cex = 0.5, k = 3, palette = "jco")



## Age, income and Spending 
select <- c(3,4,5)
mydata <- myData[,select]
mydata

tail(mydata)
head(mydata)

library(cluster)
library(factoextra)
set.seed(100)
km.res <- kmeans(mydata,3, nstart=25)

fviz_cluster(km.res,data=mydata,palette="jco", ggtheme= theme_minimal())

res.hc <- hclust(dist(mydata),  method = "ward.D2")
fviz_dend(res.hc, cex = 0.5, k = 3, palette = "jco")









## Transpose
tmydata = t(mydata)
tmydata

res.hc <- hclust(dist(tmydata),  method = "ward.D2")
fviz_dend(res.hc, cex = 0.5, k = 3, palette = "jco")
## 
head(tmydata)
tail(tmydata)


## Gender,Age and Spending 
select <- c(2,3,5)
mydata2 <- myData[,select]
mydata2

set.seed(100)
km.res <- kmeans(mydata2,5, nstart=25)
km.res
fviz_cluster(km.res,data=mydata2,palette="jco", ggtheme= theme_minimal())

res.hc <- hclust(dist(mydata2),  method = "ward.D2")
fviz_dend(res.hc, cex = 0.5, k = 3, palette = "jco")

## Transpose
tmydata2 = t(mydata2)
tmydata2

res.hc <- hclust(dist(tmydata2),  method = "ward.D2")
fviz_dend(res.hc, cex = 0.5, k = 3, palette = "jco")
## 
head(tmydata2)
tail(tmydata2)
t1 <- head(tmydata2)

medien(t1)

##  Optimal number of clusters;

fviz_nbclust(mydata, kmeans, method = "gap_stat")


d <- dist(mydata, method="euclidian")
kfit <- kmeans(d, 3)
kfit
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=3, lines=0)

## Number of cluster

fviz_nbclust(mydata, kmeans, method = "gap_stat")

##############################
####### LABORATORY 3 #########
##############################

########################################################
#1- Hierarchical and k-means based on Cosine similarity
#######################################################
install.packages("igraph")
library(igraph)

MyData <-read.csv("C:/Users/Felix/Desktop/Vorlesung WS3/Sentiment Analysis/O.folder/LAb2/DocumentTermMatrix.csv",
 header = TRUE, #are there column names in 1st row?
 sep = ",", #what separates rows?
 strip.white = TRUE, #strip out extra white space in strings.
 fill = TRUE, #fill in rows that have unequal numbers of columns
 comment.char = "#", #character used for comments that should not be read in
 stringsAsFactors = FALSE #Another control for deciding whether characters should be converted to factor
 )

##Transform MyData into the Frame Matrix format

dtm1 = as.data.frame.matrix(MyData)

dtm1 [1:10,1:10]
dtm<-dtm1[,-1]
dtm
##Create the Rownames of the matrix as a File names. 

filenames <- list.files(getwd(),pattern="*.txt")
filenames <-c(filenames)
filenames
rownames(dtm)
rownames(dtm)<-filenames
dtm [1:10,1:10]

mm_s = as.matrix(dtm)
mm <- as.matrix(mm_s[1:10,])

##############################################
########## Cosine similarity##################


#function cosineSim compute cosine similarity between document vectors
#converting to distance matrix sets diagonal elements to 0

cosineSim <- function(x){
 as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}
cs <-cosineSim(mm)
cs

d1 <- dist(cs, method="euclidian")
fit <- hclust(d=d1, method="complete")
fit
plot.new()
plot(fit, hang=-1, cex=0.5, main="Cluster Dendrogram base on Cosine Similarity(k=2)")

groups <- cutree(fit, k=4) # "k" defines the number of clusters you are using
rect.hclust(fit, k=2, border="red") # draw dendogram with red borders around the 4 c

#b- K-means Clustering
library(cluster)


d <- dist(cs, method="euclidian")
kfit <- kmeans(d, 3)
kfit
clusplot(as.matrix(d), kfit$cluster, color=T, xlab="X-axis Terms", ylab="y-axis Terms", shade=T, lines=0,main="K-means base on Cosine Similarity(k=3)")
## xlab="X-axis Terms", ylab="y-axis Terms" used to rename the Label



################################
##21. Perform the Documents (and then Terms) clustering using Hierarchical and k-means algorithms
#one of the document-term matrices (dtm, dtm with reduced sparsity or tf-idf);

########################################################
## K-means Clustering base on tdm with reduce sparsity##
########################################################

## building Term Document Matrix as a transformed Document Term Matrix
tdm <-as.TermDocumentMatrix(t(dtm),weighting = weightTf)
tdm
tdm = removeSparseTerms(tdm , 0.20)

tdm 

library(cluster)
d <- dist(tdm, method="euclidian")
kfit <- kmeans(d,3)
kfit
clusplot(as.matrix(d), kfit$cluster, color=T, xlab="X-axis Terms", ylab="y-axis Terms", shade=T, labels=2, lines=0,main="K-means base on tdm with  removeSparseTerms(k=3)")

#####################################################################
## make the Hierarchical clustering base on tdm with reduce spasity##
#####################################################################

d1 <- dist(tdm, method="euclidian")
fit <- hclust(d=d1, method="complete")
fit
plot.new()
plot(fit, hang=-1, cex=0.5, main=" Cluster Dendrogram base on tdm with reduce spasity(k=3)")

groups <- cutree(fit, k=4) # "k" defines the number of clusters you are using
rect.hclust(fit, k=3, border="red") # draw dendogram with red borders around the 4 c


##one of the document-term matrices (dtm, dtm with reduced sparsity or tf-idf)
##############################################################
## make the Hierarchical clustering tdm with reduce sparsity##
##############################################################

d1 <- dist(tdm, method="euclidian")
fit <- hclust(d=d1, method="complete")
fit
plot.new()
plot(fit, hang=-1, cex=0.5 ,main="Cluster Dendrogram base tdm with reduced sparsity(k=3)")

groups <- cutree(fit, k=3) # "k" defines the number of clusters you are using
rect.hclust(fit, k=3, border="green") # draw dendogram with red borders around the 4 c

##############################################################
## make a K-means clustering dtm with reduce sparsity       ##
##############################################################


## space reduction
dtm = as.data.frame.matrix(MyData)
dtm
dtmr <-as.DocumentTermMatrix(dtm,weighting = weightTf)
dtm = removeSparseTerms(dtmr , 0.20)
dtm

## K-means Clustering##

library(cluster)
d <- dist(dtm, method="euclidian")

kfit <- kmeans(d,3)
kfit
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, xlab="X-axis Terms", ylab="y-axis Terms", lines=0,main="K-means base on dtm reduced sparsity(k=3)")

)

##############################################################
## An Hierarchical clustering dtm with reduce sparsity       ##
##############################################################


d1 <- dist(dtm, method="euclidian")
fit <- hclust(d=d1, method="complete")
fit
plot.new()
plot(fit, hang=-1, cex=0.5 ,main="Cluster Dendrogram base dtm with reduced sparsity(k=3)")

groups <- cutree(fit, k=3) # "k" defines the number of clusters you are using
rect.hclust(fit, k=3, border="red") # draw dendogram with red borders around the 4 c

##############################################################
## normalized document-term matrix      				##
##############################################################

## K-means Clustering
docs_dtm <- DocumentTermMatrix(docs,control = list(weighting = weightTf, normalize = TRUE))

docs_dtm_norm <- t(apply(docs_dtm, 1, function(x) x/sqrt(sum(x^2))))

docs_dtm_norm 


library(cluster)
d <- dist(docs_dtm_norm, method="euclidian")
kfit <- kmeans(d,2)
kfit
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, xlab="X-axis Terms", ylab="y-axis Terms", lines=0,main="K-means base normalized document-term matrix(k=2)")

)

## Hierarchical clustering

d1 <- dist(docs_dtm_norm, method="euclidian")
fit <- hclust(d=d1, method="complete")
fit
plot.new()
plot(fit, hang=-1, cex=0.5 ,main="Cluster Dendrogram base normalized document-term matrix(k=2)")

groups <- cutree(fit, k=2) # "k" defines the number of clusters you are using
rect.hclust(fit, k=2, border="red") # draw dendogram with red borders around the 4 c



##############################################################################
##2.2. Try to calculate Cosine similarity using of the R built-in functions.## 
Compare the results. *
##############################################################################


mm_s = as.matrix(dtm)
mm <- as.matrix(mm_s[1:10,])

#function cosineSim compute cosine similarity between document vectors
#converting to distance matrix sets diagonal elements to 0

cosineSim <- function(x){
 as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}

cs <-cosineSim(mm)
cs
write.csv(as.matrix(cs),file="C:/Users/Felix/Desktop/Vorlesung WS3/Sentiment Analysis/O.folder/LAb2/DocumentCosine.csv")

##
library(igraph)

dat <-read.csv(file="C:/Users/Felix/Desktop/Vorlesung WS3/Sentiment Analysis/O.folder/LAb2/DocumentCosine.csv",
 header = TRUE,
 sep = ",",
 colClasses = NA,
 na.string = "NA",
 skip = 0,
 strip.white = TRUE,
 fill = TRUE,
 comment.char = "#",
 stringsAsFactors = FALSE
 )
mm1 = as.data.frame.matrix(dat)
mm1=mm1[,-1]


filenames <- list.files(getwd(),pattern="*.txt")
filenames <-c(filenames[1:10])
filenames

#converting mm1 into matrix format
rownames(mm1)<-filenames
cs<-as.matrix(mm1)
cs

g=graph.adjacency(cs,mode="undirected",weighted=TRUE)
g
deg <- graph.strength(g,mode="all")
deg

#Checking the undirected weighted graph attributes
list.vertex.attributes(g)
list.edge.attributes(g)
V(g)$name
E(g)$weight


#######################################################################
##. Reveal the Communities existing in the Corpus within the Documents#
## (and then Terms) based on Cosine similarity#########################
#######################################################################


# Algorithm 1: edge betweenness (Newman-Girvan)
##############################################

ceb <- cluster_edge_betweenness(g)
plot(ceb, g,main="edge betweenness based on
Cosine similarity")
membership(ceb)

# Algorithm 2: based on propagating labels
##########################################

clp <- cluster_label_prop(g)
plot(clp , g,main="Propagating labels Algorithm based on
Cosine similarity")
membership(clp)

# Algorithm 3: based on greedy optimization of modularity
########################################################

cfg <- cluster_fast_greedy(as.undirected(g))
plot(cfg, as.undirected(g),main="Greedy optimization of modularity 
Algorithmus based on Cosine similarity")
membership(cfg)


##########################################################
#+++++++++++++++++++ LABORATORY 4 +++++++++++++++++++++++#


install.packages("topicmodels")
install.packages("lsa")
install.packages("scatterplot3d")

library(tm)
library(topicmodels)
library(lsa)
library(scatterplot3d)
library(ggplot2)



MyData <-read.csv("C:/Users/Felix/Desktop/Vorlesung WS3/Sentiment Analysis/O.folder/LAb2/DocumentTermMatrix.csv",
 header = TRUE, #are there column names in 1st row?
 sep = ",", #what separates rows?
 strip.white = TRUE, #strip out extra white space in strings.
 fill = TRUE, #fill in rows that have unequal numbers of columns
 comment.char = "#", #character used for comments that should not be read in
 stringsAsFactors = FALSE #Another control for deciding whether characters should be converted to factor
 )

##Transform MyData into the Frame Matrix format

dtm1 = as.data.frame.matrix(MyData)

dtm1 [1:10,1:10]
dtm<-dtm1[,-1]
dtm
##Create the Rownames of the matrix as a File names. 

filenames <- list.files(getwd(),pattern="*.txt")
filenames <-c(filenames)
filenames
rownames(dtm)
rownames(dtm)<-filenames
dtm [1:10,1:10]

freq <- colSums(as.matrix(dtm))
length(freq)
ord <- order(freq,decreasing=TRUE)
freq[ord]
write.csv(freq[ord],"C:/Users/Felix/Desktop/Vorlesung WS3/Sentiment Analysis/word_freq.csv")

#+++++++++++++TOPIC MODELLING USING LDA+++++++++++++++++++

	#----------- K = 2 Topic ----------------
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
k <- 2

ldaOut <-LDA(dtm, k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=
best, burnin = burnin, iter = iter, thin=thin))
str(ldaOut)


#--------most significant 6 terms in each topic --------

ldaOut.terms <- as.matrix(terms(ldaOut,6))
ldaOut.terms
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))

	#----------- K = 3 Topic ----------------
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
k <- 3

ldaOut <-LDA(dtm, k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=
best, burnin = burnin, iter = iter, thin=thin))
str(ldaOut)


#--------most significant 6 terms in each topic --------

ldaOut.terms <- as.matrix(terms(ldaOut,6))
ldaOut.terms
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))



	#----------- K = 4 Topic ----------------
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
k <- 4

ldaOut <-LDA(dtm, k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=
best, burnin = burnin, iter = iter, thin=thin))
str(ldaOut)

#--------most significant 6 terms in each topic --------

ldaOut.terms <- as.matrix(terms(ldaOut,6))
ldaOut.terms
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))

#--------------Latent Semantic Analysis----------

		#-Latent Semantic Analysis based on  dtm k=2###

lsaSpacedtm <- lsa(dtm,2)
lsaSpacedtm


		#-Latent Semantic Analysis based on  normalized document-term ##
 
xx<-rowSums(as.matrix(dtm))
xx
dtm_Norm<-dtm/xx
##Make LSA transformation K=2
lsaSpacedtmN <- lsa(dtm_Norm,2)
lsaSpacedtmN

		#-Latent Semantic Analysis based on  tf_idf k=2##

lsaSpacetf_ <- lsa(tf_idf,2)
lsaSpacetf_



###Analyze the results of LSA transformation – Dk, Tk and Sk Matrices##
#----------------------------------------------------------------------
lsaSpace$dk
head(lsaSpace$dk,10)
head(lsaSpace$tk,10)
head(lsaSpace$sk,10)

#--------- creation of a new Document in LSA----------

	     ####Based on Dtm_Norm #######

pointDtmN <- data.frame(x=lsaSpacedtmN$dk[,1], y=lsaSpacedtmN$dk[,2])
pointDtmN
           ##### Based on Dtm #######
pointDtm <- data.frame(x=lsaSpacedtm$dk[,1], y=lsaSpacedtm$dk[,2])
pointDtm
		
	     ##### Based on tf_idf #######
pointTf_ <- data.frame(x=lsaSpacetf_$dk[,1], y=lsaSpacetf_$dk[,2])
pointTf_

#---------Plot of Documents in Latent Semantic Space------------

	    ##### Based on Dtm_Norm #######
pDtmN<-ggplot(pointDtmN, aes(x = x, y = y))
pDtmN<-pDtmN + geom_point(data = pointDtmN, aes(x = x, y = y))
pDtmN<-pDtmN + geom_text(data = pointDtmN, aes(x = x, y = y, label=colnames(tf_idf)))
pDtmN
 
	      ##### Based on Dtm #######

pDtm<-ggplot(pointDtm, aes(x = x, y = y))
pDtm<-pDtm + geom_point(data = pointDtm, aes(x = x, y = y))
pDtm<-pDtm + geom_text(data = pointDtm, aes(x = x, y = y, label=colnames(tf_idf)))
pDtm

		 #- K-means clustering###

library(cluster)
d <- dist(lsaSpacedtmN$dk, method="euclidian")
kfit11 <- kmeans(d, 3)
kfit11 
clusplot(as.matrix(d), kfit11$cluster, color=T, shade=T,xlab="X-axis Terms", ylab="y-axis Terms", lines=0,main="K-means Clustering LSA (k=3)", cex.txt=0.7)

		#-Hierarchical clustering LSA###

d1 <- dist(lsaSpacedtmN$dk, method="euclidian")
fit <- hclust(d=d1, method="complete")
fit
plot.new()
plot(fit, hang=-1, cex=2,main="K-means Clustering LSA")

groups <- cutree(fit, k=3) # "k" defines the number of clusters you are using
rect.hclust(fit, k=3, border="red") # draw dendogram with red borders around the 4 c


#### Graph-based algorithms application##############################
#------------------------------------------                         #
                                                                    #
g=graph.adjacency(lsaSpacedtmN$dk,mode="undirected",weighted=TRUE)  #
g
deg <- graph.strength(g,mode="all")
deg
#Checking the undirected weighted graph attributes
list.vertex.attributes(g)
list.edge.attributes(g)
V(g)$name
E(g)$weight

# Algorithm 1: edge betweenness (Newman-Girvan)
#---------------------------------------------#

ceb <- cluster_edge_betweenness(g)
plot(ceb, g,main="edge betweenness based on
LSA")
membership(ceb)

# Algorithm 2: based on propagating labels
#----------------------------------------#

clp <- cluster_label_prop(g)
plot(clp , g,main="Propagating labels Algorithm based on
LSA")
membership(clp)

# Algorithm 3: based on greedy optimization of modularity
#-------------------------------------------------------#

cfg <- cluster_fast_greedy(as.undirected(g))
plot(cfg, as.undirected(g),main="Greedy optimization of modularity 
Algorithmus based on LSA")
membership(cfg)

