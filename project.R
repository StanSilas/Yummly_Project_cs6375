#Project
require(rjson)
require(RJSONIO)
require(jsonlite)
require(NLP)
require(tm)
require(SnowballC) 
require(forecast)
require(ttutils)
require(stringdist)
library(plyr)
require(ggplot2)
train_unclean  <- fromJSON("C:/Users/vivek/Desktop/Fall 2016/Machine Learning/Project/Project/train/train.json", flatten = TRUE)

test_unclean<-fromJSON("C:/Users/vivek/Desktop/Fall 2016/Machine Learning/Project/Project/test/test.json", flatten = TRUE)

# str(train_unclean)
# head(str(train_unclean))
# str(test_unclean)

#visualising the data 
ggplot(data = train_unclean, aes(x = cuisine)) +
  geom_bar() +
  labs(title = "Cuisines", x = "Cuisine", y = "Number of Recipes")


#converting JSON into data frame

#train
unclean_train_df <- do.call(cbind, train_unclean) 
unclean_train_df<-as.data.frame(unclean_train_df)
View(unclean_train_df)
class(unclean_train_df)

#looking at the data, both not working
#qplot(unclean_train_df$cuisine,geom = "histogram")
#hist(unclean_train_df$cuisine)

#test
unclean_test_df<-do.call(cbind, test_unclean) 
unclean_test_df<-as.data.frame(unclean_test_df)
class(test_unclean)
View(test_unclean)

#understanding the training data distribution
sapply(train_unclean, function(x) length(unique(x)))
#understanding the test data distribution
sapply(test_unclean, function(x) length(unique(x)))

#graphical plot of train data

#ggplot(data = train_unclean, aes(x = cuisine))  + geom_histogram() 

#   labs(title = "Cuisines", x = "Cuisine", y = "Number of Recipes")

train_unclean$cuisine<-as.factor(train_unclean$cuisine) 


#extract list of ingredients for test and train
tr_ingredients<-unclean_train_df$ingredients 
tes_ingredients<-unclean_test_df$ingredients 

tr_ing_list<-(unlist(tr_ingredients))

tes_ing_list<-(unlist(tes_ingredients))



