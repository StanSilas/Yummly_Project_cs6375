#based on mansish saraswat code 
require(rjson, quietly = TRUE)
require(RJSONIO, quietly = TRUE)

require(jsonlite)
require(xgboost)
require(Matrix)
require(MatrixModels)

train <- fromJSON("train.json")
test <- fromJSON("test.json")

test$cuisine <- NA
combi <- rbind(train, test)

library(tm)
#create corpus
corpus <- Corpus(VectorSource(combi$ingredients))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, c(stopwords('english')))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument)

corpus <- tm_map(corpus, PlainTextDocument)
frequencies <- DocumentTermMatrix(corpus)
freq <- colSums(as.matrix(frequencies))
length(freq)
ord <- order(freq)
freq[head(ord)]
freq[tail(ord)]

head(table(freq),20)
tail(table(freq),20)

sparse <- removeSparseTerms(frequencies, 1 - 3/nrow(frequencies))
dim(sparse)
wf <- data.frame(word = names(freq), freq = freq)

head(wf)
require(ggplot2)

chart <- ggplot(subset(wf, freq >10000), aes(x = word, y = freq))
chart <- chart + geom_bar(stat = 'identity', color = 'black', fill = 'black')
chart <- chart + theme(axis.text.x=element_text(angle=60, hjust=1))
chart
require(wordcloud)

#find associated terms
# findAssocs(frequencies, c('salt','oil'), corlimit=0.30)

#word plot
wordcloud(names(freq), freq, max.words = 100, scale = c(4, .1), colors = brewer.pal(6, 'Dark2'))


#structural changes 
#create sparse as data frame #singular value / eigen value decomposition
newsparse <- as.data.frame(as.matrix(sparse))
dim(newsparse)

#check if all words are appropriate
colnames(newsparse) <- make.names(colnames(newsparse))

#check for the dominant dependent variable
table(train$cuisine)
#add cuisine
newsparse$cuisine <- as.factor(c(train$cuisine, rep('italian', nrow(test))))

#split data 
mytrain <- newsparse[1:nrow(train),]
mytest <- newsparse[-(1:nrow(train)),]


#xgboost
ctrain <- xgb.DMatrix(Matrix(data.matrix(mytrain[,!colnames(mytrain) %in% c('cuisine')])), label = as.numeric(mytrain$cuisine)-1)
dtest <- xgb.DMatrix(Matrix(data.matrix(mytest[,!colnames(mytest) %in% c('cuisine')]))) 
watchlist <- list(train = ctrain, test = dtest)


#parameter tuning and multiple learner creation

#train multiclass model using softmax
#first model
xgbmodel <- xgboost(data = ctrain, max.depth = 25, eta = 0.3, nround = 200, objective = "multi:softmax", num_class = 20, verbose = 1, watchlist = watchlist)

#second model
xgbmodel2 <- xgboost(data = ctrain, max.depth = 20, eta = 0.5, nrounds = 250, objective = "multi:softmax", num_class = 20, watchlist = watchlist)

#second model v2
xgbmodel22 <- xgboost(data = ctrain, max.depth = 20, eta = 0.2, nrounds = 250, objective = "multi:softmax", num_class = 20, watchlist = watchlist)


#third model
xgbmodel3 <- xgboost(data = ctrain, max.depth = 25, gamma = 2, min_child_weight = 2, eta = 0.1, nround = 250, objective = "multi:softmax", num_class = 20, verbose = 2,watchlist = watchlist)

#predictions
xgbmodel.predict <- predict(xgbmodel, newdata = data.matrix(mytest[, !colnames(mytest) %in% c('cuisine')]))
xgbmodel.predict.text <- levels(mytrain$cuisine)[xgbmodel.predict + 1]
#predict 2
xgbmodel.predict2 <- predict(xgbmodel2, newdata = data.matrix(mytest[, !colnames(mytest) %in% c('cuisine')])) 
xgbmodel.predict2.text <- levels(mytrain$cuisine)[xgbmodel.predict2 + 1]

##predict 22
xgbmodel.predict22 <- predict(xgbmodel22, newdata = data.matrix(mytest[, !colnames(mytest) %in% c('cuisine')])) 
xgbmodel.predict22.text <- levels(mytrain$cuisine)[xgbmodel.predict22 + 1]

#predict 3
xgbmodel.predict3 <- predict(xgbmodel3, newdata = data.matrix(mytest[, !colnames(mytest) %in% c('cuisine')])) 
xgbmodel.predict3.text <- levels(mytrain$cuisine)[xgbmodel.predict3 + 1]

#acc
submit_match1 <- cbind(as.data.frame(test$id), as.data.frame(xgbmodel.predict.text))
colnames(submit_match1) <- c('id','cuisine')
submit_match1 <- data.table(submit_match1, key = 'id')


#acc2
submit_match2 <- cbind(as.data.frame(test$id), as.data.frame(xgbmodel.predict2.text))
colnames(submit_match2) <- c('id','cuisine')
submit_match2 <- data.table(submit_match2, key = 'id')


#acc22
submit_match22 <- cbind(as.data.frame(test$id), as.data.frame(xgbmodel.predict22.text))
colnames(submit_match22) <- c('id','cuisine')
submit_match22 <- data.table(submit_match22, key = 'id')


#acc3
submit_match3 <- cbind(as.data.frame(test$id), as.data.frame(xgbmodel.predict3.text))
colnames(submit_match3) <- c('id','cuisine')
submit_match3 <- data.table(submit_match3, key = 'id')
###### #####
sum(diag(table(mytest$cuisine, xgbmodel.predict)))/nrow(mytest) 
sum(diag(table(mytest$cuisine, xgbmodel.predict2)))/nrow(mytest)
sum(diag(table(mytest$cuisine, xgbmodel.predict22)))/nrow(mytest)
sum(diag(table(mytest$cuisine, xgbmodel.predict3)))/nrow(mytest)
###########

#ensembling 
submit_match3$cuisine2 <- submit_match2$cuisine
submit_match3$cuisine22 <- submit_match22$cuisine
submit_match3$cuisine1 <- submit_match1$cuisine

####

Mode <- function(x) {
  u <- unique(x)
  u[which.max(tabulate(match(x, u)))]
}

x <- Mode(submit_match3[,c("cuisine","cuisine2","cuisine22","cuisine1")])
y <- apply(submit_match3,1,Mode)
###
final_submit <- data.frame(id= submit_match3$id, cuisine = y)
#view submission file
data.table(final_submit)
write.csv(final_submit, 'ensemble.csv', row.names = FALSE)
