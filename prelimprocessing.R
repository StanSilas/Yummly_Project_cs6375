#cleaning train ingredients text file 
require(NLP)
require(tm)
require(SnowballC)
require(RColorBrewer)
require (wordcloud)
require(ggplot2)
require(tau)
getwd()

oz <- Corpus(DirSource("C:/Users/vivek/Desktop/Fall 2016/Machine Learning/Project/Project/prelim processing"))
#above link is to the ingrid.txt folder on my system.

inspect(oz[1]) #taking a look at the data
ozc<-oz #creatting a copy of the data
#oz <- tm_map(oz, tolower) #lowercase

#performing text transformations
oz <- tm_map(oz, content_transformer(tolower))
oz <- tm_map(oz, removePunctuation, preserve_intra_word_dashes = FALSE) # remove punctuation
oz <- tm_map(oz, removeWords, stopwords("english")) # remove stopwords
oz <- tm_map(oz, stemDocument) # reduce word forms to stems
oz <- Corpus(VectorSource(oz))

#creating a matrix of word vs count
oz.tdm.1 <- TermDocumentMatrix(oz) 

oz.tdm.1.c<-oz.tdm.1 #creating a copy

freqs <- (as.matrix(oz.tdm.1)) #converting TDM to matrix
freqs<-as.data.frame(freqs)  #converting matrix to data frame
freqs$ingredients<-rownames(freqs) #picking up row names of the data frame

colnames(freqs)<-c("occurence_count", "ingredients") #renaming data frame column names

freqs<-freqs[order(freqs$occurence_count),]  #sorting based on counts in descending
write.table(freqs, "frequencies_train.txt", sep="\t")



