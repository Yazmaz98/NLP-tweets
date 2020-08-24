#install.packages("rtweet")
#install.packages("reactable")
#install.packages("glue")
#install.packages("stringr")
#install.packages("httpuv")
#install.packages("dplyr")
#install.packages("purrr")
#install.packages("writexl")
#install.packages("wordcloud")
#install.packages("RColorBrewer")
#install.packages("wordcloud2")
#install.packages("igraph")
library(igraph)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(writexl)
library(rtweet)
library(dplyr)
library(boot)
library(tm)
library(SnowballC)

# -------------------------------------------PART 1----------------------------------------------
# ----------Analysing relationship between friends count and followers count from tweets---------
# 1
playStation_tweet_df <- search_tweets("#playstation", n = 1500, 
                          include_rts = FALSE, lang = "en")
playStation_tweet_df <- sample_n(playStation_tweet_df, 1000)
# writing the data to a file (excel file instead of csv file for visibility)
write_xlsx(playStation_tweet_df, "Download_1.Data.xlsx",col_names = TRUE)

# 2
x <- playStation_tweet_df$followers_count
y <- playStation_tweet_df$friends_count

# 3
xbar <- mean(x)
ybar <- mean(y)

# 4)proportion of the followers count/friends count that are higher than the average
px_hat <-(length(which(x>xbar))/length(x))
py_hat <-(length(which(y>ybar))/length(y))

# 5)a) bootstrap distribution 
foo <- function(data, indices){
  dt<-data[indices]
  mean(dt)
}
set.seed(12345) #for reproducibility and consistency of results
myBootstrap <- boot(x, foo, R=1000)

# 5)b) Plot a histogram of the bootstrap distribution of the followers counts

plot(myBootstrap,type="h")

# 5)c) Estimate a 97% confidence interval of the average of the followers count in the population

boot.ci(myBootstrap, conf=0.97) #(1893, 3122 )


upper=quantile(myBootstrap$t,0.975) #3094
lower=quantile(myBootstrap$t,0.005) #1862

#5)d) Estimate a 97% confidence interval of the average of the friends count in the population

set.seed(12345)
myBootstrap2 <- boot(y, foo, R=1000)
upper=quantile(myBootstrap2$t,0.975) 
lower=quantile(myBootstrap2$t,0.005)
boot.ci(myBootstrap2, conf=0.97)

#6 Use a 97% confidence to estimate the proportion of the users in the population who have higher 
#friends_count than the average count

foo2 <- function(data, indices){
  dt<-data[indices]
  avg = mean(dt)
  length(which(dt>avg))/length(dt)
}
set.seed(12345)
myBootstrap3 <- boot(y, foo2, R=1000)

# 7)a) grouping number of tweets according to followers count

x_tens <- length(x[which(x<100)])
x_hundreds <- length(x[which(x<1000 & x>=100)])
x_1thousands <- length(x[which(x<2000 & x>=1000)])
x_2thousands <- length(x[which(x<3000 & x>=2000)])
x_3thousands <- length(x[which(x<4000 & x>=3000)])
x_4thousands <- length(x[which(x<5000 & x>=4000)])
x_5thousands_or_more <- length(x[which(x>=5000)])

# 7)b) Similarly find the frequency of the friends count under each group, use same grouping in (a). 
# Show your grouping result in a table

y_tens <- length(y[which(y<100)])
y_hundreds <- length(y[which(y<1000 & y>=100)])
y_1thousands <- length(y[which(y<2000 & y>=1000)])
y_2thousands <- length(y[which(y<3000 & y>=2000)])
y_3thousands <- length(y[which(y<4000 & y>=3000)])
y_4thousands <- length(y[which(y<5000 & y>=4000)])
y_5thousands_or_more <- length(y[which(y>=5000)])

#table of x grouping
x_grouping = data.frame("Groups" = c("tens","hundreds", "1thousands", "2thousands",
                                     "3thousands", "4thousands", "5thousandsAndMore"), 
                        "Counts" = c(x_tens, x_hundreds, x_1thousands, x_2thousands,
                                     x_3thousands, x_4thousands, x_5thousands_or_more))
#table of y grouping
y_grouping = data.frame("Groups" = c("tens","hundreds", "1thousands", "2thousands",
                                     "3thousands", "4thousands", "5thousandsAndMore"), 
                        "Counts" = c(y_tens, y_hundreds, y_1thousands, y_2thousands,
                                     y_3thousands, y_4thousands, y_5thousands_or_more))

# 7)c)Find the expected counts under each group and use a suitable statistical test in R to test 
#the independence

#creating the table we saw in the lecture
data1 = rep(c("Followers","Friends"), c(1000,1000))
data2 = rep(c("tens","hundreds","one_thousands","two_thousands","three_thousands","four_thousands",
              "five_thousandsAndMore","tens","hundreds","one_thousands","two_thousands","three_thousands",
              "four_thousands","five_thousandsAndMore"),
            c(409,307,80,24,10,17,153,299,436,88,31,8,15,123))
chi_table = table(data1,data2)

#calculating the X2 statistic
p = rowSums(chi_table)
q = colSums(chi_table)
expectedRowProportion=p/sum(chi_table)
expectedRowProportion
expectedColProportion=q/sum(chi_table)
expectedColProportion
E=expectedRowProportion %o% expectedColProportion *sum(chi_table)
X2=sum((chi_table-E)^2/E)
X2

# 7)d) calculating the p-value
X2dist <- replicate(1000, {
  d1 = sample(data1)
  d2 = sample(data2)
  X = table(d1,d2)
  pi = rowSums(X)
  qi = colSums(X)
  expecRowProportion=pi/sum(X)
  expecColProportion=qi/sum(X)
  E=expecRowProportion %o% expecColProportion *sum(X)
  X2i=sum((X-E)^2/E)
  X2i
})
#hist(X2dist)
#The null hypothesis for this test is that there is no relationship between followers and friends count
p_value = mean(X2dist>X2) # p < 0.05 -> we reject H0 therefore the two variables are dependant
chisq.test(chi_table) #same result

# -------------------------------------------PART 2----------------------------------------------
#-----------------------------------Finding Themes in Tweets-------------------------------------

# 8) find unique users who have higher friends count than the average friends count in the sample
higherThanAvg <- unique(playStation_tweet_df$screen_name[y>ybar])

# 9) find unique users who have less than or equal to the average friends count in the sample
LessThanAvg <- unique(playStation_tweet_df$screen_name[y<=ybar])

# 10) find the tweets of those users identified in (8) and (9), combine them and save them in a 
# variable tweets
higher_tweets = playStation_tweet_df$text[y>ybar]
less_tweets = playStation_tweet_df$text[y<=ybar]
tweets = c(higher_tweets, less_tweets)

# 11) clean and preprocess "tweets" variable
corpus = Corpus(VectorSource(tweets))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, removeWords,stopwords("english"))
corpus = tm_map(corpus, stemDocument)

# 12) display the first two tweets before and after the cleaning/processing
#before
tweets[1]
tweets[2]
#after
corpus$content[1]
corpus$content[2]

# 13)
# creating the DocumentTermMatrix 
tweet.dtm = DocumentTermMatrix(corpus)
tweet.matrix = as.matrix(tweet.dtm)
dim(tweet.matrix)
# Weighting the Document Matrix
#TF-IDF Matrix 
#TFIDF = ln(fd,t +1)*ln(N/ft) #N is the number of documents, ft is the number of documents containing term t
N=nrow(tweet.matrix) #number of documents 
ft=colSums(tweet.matrix>0) #in how many documents term t appeared in
IDF=log(N/ft) #length is 5144
TF=log(tweet.matrix+1)
#Compute the weighted document term matrix tweet.weighted.matrix 
weighted.matrix=TF%*%diag(IDF) 
dim(weighted.matrix)# 1000*5144 where each colum represents a term

#how many documents were empty following the TFIDF process?
wordsPerDoc = rowSums(weighted.matrix)
numEmptyDocs = length(which(abs(wordsPerDoc)==0))

# 14) clustering using cosine distance
norm.tweet.matrix = diag(1/sqrt(rowSums(weighted.matrix^2))) %*% weighted.matrix
## then create the distance matrix
D =dist(norm.tweet.matrix, method = "euclidean")^2/2
#To visualise the clustering, we will use multidimensional 
#scaling to project the data into a 2d space
## perform MDS using 100 dimensions
mds.tweet.matrix <- cmdscale(D, k=100)
n = 15  
SSW = rep(0, n)
for (k in 1:n) {
  set.seed(40)
  K = kmeans(mds.tweet.matrix, k, nstart = 20)
  SSW[k] = K$tot.withinss
}

plot(1:n, SSW, type = "b") #the elbow is at 4

# 15)
#let's perform clustering with 4 clusters :
set.seed(40)
KmeansCluster = kmeans(mds.tweet.matrix, 4, nstart = 20)
#let's find the number of tweets in each cluster
KmeansCluster$size
# 16) 2-d visualisation 
mds2.tweet.matrix <- cmdscale(D, k=2)

plot(mds2.tweet.matrix, col = KmeansCluster$cluster)
# with different signs for tweets that have a higher friends count than average and those that have
# less friends count than average
isYHigherBool = logical(1000)
h = which(y>ybar)
isYHigherBool[h]=TRUE
mds2.tweet.matrix = cbind(mds2.tweet.matrix, isYHigherBool)
plot(mds2.tweet.matrix, col = KmeansCluster$cluster, pch = mds2.tweet.matrix[,3])

# 17) The clusters in black and green (bottom and far right) seem to contain only one category
# of friends count (compared to avg) while the 2 other clusters are mixed. Which is normal because we
#have 4 clusters. Maybe if we chose 2
# clusters only we'd obtain a cluster with those higher than average and another for those less.

#----------------------------------------------------------------------------------------------------
#let's perform clustering with 2 clusters :
set.seed(40)
KmeansCluster2 = kmeans(mds.tweet.matrix, 2, nstart = 20)
#let's find the number of tweets in each cluster
KmeansCluster2$size

# 16) 2-d visualisation 

mds2.tweet.matrix <- cmdscale(D, k=2)
plot(mds2.tweet.matrix, col = KmeansCluster2$cluster)
# with different signs for tweets that have a higher friends count than average and those that have
# less friends count than average
isYHigherBool = logical(1000)
h = which(y>ybar)
isYHigherBool[h]=TRUE
mds2.tweet.matrix = cbind(mds2.tweet.matrix, isYHigherBool)
plot(mds2.tweet.matrix, col = KmeansCluster2$cluster, pch = mds2.tweet.matrix[,3])
#----------------------------------------------------------------------------------------------------

# 18) Which cluster has the highest proportion of tweets that are greater than the average friends_count

# let's first start by binding the clusters values for each tweet to the y vector containing friends
# counts to compare the latter with their average
y_by_cluster = data.frame(y, KmeansCluster$cluster)
y_by_cluster %>%
  group_by(KmeansCluster.cluster) %>%
  summarise(mean(y>ybar))  
# ----> the cluster with highest proportion of tweets with friends count higher than average is 
# cluster 2 (lowest is cluster 3)

# 19) Display five samples of the tweets in clusters that you found in 18

# let's show 5 samples of tweets from cluster 2 
indexesCluster2 = which(y_by_cluster$KmeansCluster.cluster==2)
cluster2Tweets = playStation_tweet_df$text[indexesCluster2]
head(cluster2Tweets,5)

# 20)

# cluster with highest friends count proportion

## extract tweets vectors for cluster
clusterTweets = weighted.matrix[indexesCluster2,]
## combine the tweets into a mean tweet
clusterTermWeight = colMeans(clusterTweets)
## show the top 10 weighted words
sort(clusterTermWeight, decreasing = TRUE, index.return = TRUE)[1:10]
# the argument index return gives a list of the sorted weights we use it in the original tweets.matrix
# to see what these words actually are
high_popular_words = colnames(tweet.matrix)[c(319 ,314  ,36 ,48 ,321, 315 ,49 ,325  ,52 ,320)]

# cluster with lowest friends count proportion
indexesCluster3 = which(y_by_cluster$KmeansCluster.cluster==3)
clusterTweets2 = weighted.matrix[indexesCluster3,]
clusterTermWeight2 = colMeans(clusterTweets2)
sort(clusterTermWeight2, decreasing = TRUE, index.return = TRUE)[1:10]
low_popular_words = colnames(tweet.matrix)[c(28 ,23 ,24 ,27 ,30 ,70 ,29,21, 143 ,79)]

# 21) Use word cloud to display the themes of the clusters identified by question 20

# word cloud for cluster with highest friends count proportion
# fetching the matrix
matrixCluster2 = tweet.matrix[indexesCluster2,]
words <- sort(colSums(matrixCluster2),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

# the actual word cloud
set.seed(1234) # for reproducibility 
wordcloud(words = df$word, freq = df$freq, min.freq = 1,max.words=200, random.order=FALSE,
          rot.per=0.35,colors=brewer.pal(8, "Dark2"))

#wordcloud2(data=df, size=1.6, color='random-dark')
#wordcloud2(data=df, size = 0.7, shape = 'pentagon')

# word cloud for cluster with highest friends count proportion

# fetching the matrix
matrixCluster3 = tweet.matrix[indexesCluster3,]
words2 <- sort(colSums(matrixCluster3),decreasing=TRUE) 
df2 <- data.frame(word = names(words2),freq=words)

# the actual word cloud
set.seed(1234) # for reproducibility 
wordcloud(words = df2$word, freq = df2$freq, min.freq = 1,max.words=200, random.order=FALSE,
          rot.per=0.35,colors=brewer.pal(8, "Dark2"))

#wordcloud2(data=df2, size=1.6, color='random-dark')
#wordcloud2(data=df2, size = 0.7, shape = 'pentagon')

# 22) Use dendrogram to display the themes of the clusters identified by question 20

#First find only the terms that appear in at least 50 tweets to reduce the dimensions
#for visualization
frequent.words = which(colSums(tweet.matrix > 0) > 50)
length(frequent.words)
term.matrix = tweet.matrix[,frequent.words]
#We want to compute the cosine distance between each of the terms (not documents),
#so we must make sure that all term vectors (columns of matrix)
#have a norm of 1.
#Remember each column in term.matrix is a term and we want the column vectors
#to have unit length
norm.term.matrix = term.matrix %*% diag(1/sqrt(colSums(term.matrix^2)))
## preserve column names (terms associated with each column)
colnames(norm.term.matrix) = colnames(term.matrix)
#We then compute the Cosine distance between the columns of 
#the matrix 
#(IMPORTANT: Transposing the matrix is done since we want distances between terms
#for single linkage clustering and complete linkage clustering
t(norm.term.matrix)
D = dist(t(norm.term.matrix), method = "euclidean")^2/2


#then complete linkage clustering.
## hierarchical clustering 
h = hclust(D, method="complete") 
plot(h)

intersect(h$labels, high_popular_words)
intersect(h$labels, low_popular_words)

# 23)What is the conclusion regarding the themes from the analysis in this section? Interpret your 
# findings

############## explanation will be elaborated in report.

# -------------------------------------------PART 3----------------------------------------------
#---------------------------------------Building Networks----------------------------------------
# 24) Find the 10 most popular friends of the chosen Twitter handle
friends <- get_friends("PlayStation")
names(friends)
friendsInfo = lookup_users(friends$user_id)
friendsNames = friendsInfo$screen_name
friendsFollowersCount = friendsInfo$followers_count
friendsFollowersOrder = order(friendsFollowersCount, decreasing = TRUE)[1:10] 
top10popularFriends = friendsInfo[friendsFollowersOrder,]
top10popularFriendsNames = top10popularFriends$screen_name

# 25) Obtain a 2-degree egocentric graph centred at the chosen Twitter handle and plot the graph

user = lookup_users(c("PlayStation"))
user.name = rep(user$screen_name, nrow(top10popularFriends))  # repeat user's name    
el = cbind(user.name, top10popularFriendsNames)  # bind the columns to create a matrix
g = graph.edgelist(el)
plot(g)

# 26) Compute the closeness centrality score for each Twitter handle in your graph
closeness(g)

# 27) List the top 3 most central people in your graph according to the closeness centrality

g.centres=order(closeness(g), decreasing=TRUE)
#g[g.centres][,1]#names of the centres


# 28) Comment on your results

######## explanation will be elaborated in report.












