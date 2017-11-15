#Installing required packages
# readr
# dplyr
# magnittr
# tm
# Rcolorbrewer
# ggplot2
# stringi
# stringr
# wordcloud
# topicmodels
# lda

# Loading installed packages
# Including stopwords file from PC
words <- read.csv("words.csv",header = FALSE)
words <- as.data.frame(words)
words <- t(words)
words <- as.character(words)
# Reading Abstract file from Year 1961 - 1990
Abstract <- read_csv("C:/Users/hp/Downloads/Abstract.csv")
Abstract <- Abstract[,1:2]
Abstract <- Abstract %>% na.omit()
Abstract1 <- Abstract[,1]
scopuS1 <- Abstract1
# Converting into Corpus
start.time <- Sys.time()
abstract_1961 <- VCorpus(DataframeSource(scopuS1))
end.time <- Sys.time()
time_taken <- end.time - start.time
time_taken
f <- content_transformer(function(x,pattern,replacement)
  gsub(pattern,replacement,x))
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus,removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords,c(words,stopwords("en"),"taylor","francis","use","model","paper","provide","used","study","studied"))
  corpus <- tm_map(corpus,f,"ing\\b","")
  corpus <- tm_map(corpus,f,"s\\b","")
  corpus <- tm_map(corpus,f,"ed\\b","e")
  
  return(corpus)
}
# Cleaning Corpus
start.time <- Sys.time()
cleaned_scopus_1961 <- clean_corpus(abstract_1961)
end.time <- Sys.time()
time_taken2 <- end.time - start.time
time_taken2
# Making a Document Term Matrix for Text Mining
start.time <- Sys.time()
cleaned_scopus_1961_dtm <- DocumentTermMatrix(cleaned_scopus_1961)
end.time <- Sys.time()
time_taken3 <- end.time - start.time
time_taken3
inspect(cleaned_scopus_1961_dtm)
cleaned1961m <- as.matrix(cleaned_scopus_1961_dtm)
termsc <- colSums(cleaned1961m)
termsc <- sort(termsc,decreasing = TRUE)
cleaned1961f <- data.frame(term = names(termsc),num = termsc)
cleaned1961f <- cleaned1961f %>% na.omit()
# Plotting terms vs frequency graph
ggplot(head(cleaned1961f,50),aes(term,num))+
  geom_col(show.legend = FALSE)+
  coord_flip()
cleaned1961df <- as.data.frame(cleaned1961m)
# Creating an elbowchart to find appropriate number of clusters
ratio_ss <- rep(0,15)
start.time <- Sys.time()
for(k in 1:15){
  sc1 <- kmeans(cleaned1961df,centers = k,iter.max = 50)
  ratio_ss[k] <- sc1$tot.withinss/sc1$totss
  
}
plot(ratio_ss,type = "b",xlab = "k")
end.time <- Sys.time()
time_taken4 <- end.time - start.time
time_taken4
start.time <- Sys.time()
S1961 <- kmeans(cleaned1961df,centers = 5,iter.max = 50)
end.time <- Sys.time()
time_taken5 <- end.time - start.time
time_taken5
S1961$size
# Generating cluster center terms
for(i in 1:5){
  cat(paste("cluster",i,": ",sep = ""))
  b <- sort(S1961$centers[i, ],decreasing = TRUE)
  cat(names(b)[1:4],"\n")
  
}

# Finding the terms in each cluster
S11 <- cleaned1961f[S1961$cluster == 1,]
S12 <- cleaned1961f[S1961$cluster == 2,]
S13 <- cleaned1961f[S1961$cluster == 3,]
S14 <- cleaned1961f[S1961$cluster == 4,]
S15 <- cleaned1961f[S1961$cluster == 5,]
# Making wordcloud of each cluster and whole data
start.time <- Sys.time()
wordcloud(S11$term,S11$num,max.words=nrow(S11),colors=brewer.pal(8,"Dark2"),scale=c(3,0.5),random.order=F)
wordcloud(S12$term,S12$num,max.words=nrow(S12),colors=brewer.pal(8,"Dark2"),scale=c(3,0.5),random.order=F)
wordcloud(S13$term,S13$num,max.words=nrow(S13),colors=brewer.pal(8,"Dark2"),scale=c(3,0.5),random.order=F)
wordcloud(S14$term,S14$num,max.words=nrow(S14),colors=brewer.pal(8,"Dark2"),scale=c(3,0.5),random.order=F)
wordcloud(S15$term,S15$num,max.words=nrow(S15),colors=brewer.pal(8,"Dark2"),scale=c(3,0.5),random.order=F)
wordcloud(cleaned1961f$term,cleaned1961f$num,max.words= nrow(cleaned1961f),colors=brewer.pal(8,"Dark2"),scale=c(3,0.5),random.order=F)
end.time <- Sys.time()
time_taken6 <- end.time - start.time
time_taken6

# Assigning topics using LDA
start.time <- Sys.time()
lda <- LDA(cleaned_scopus_1961_dtm,k = 5,method = "Gibbs")
tpS1 <- topics(lda,1)
end.time <- Sys.time()
time_taken7 <- end.time - start.time
time_taken7

term <- terms(lda,10)
term <- apply(term,MARGIN = 2,paste,collapse = ", ") %>% print

# Overall Trend of documents
Year <- Abstract[,2]
A.df <- data.frame(
  document= 1:nrow(Abstract),
  Year = Year,
  Topic = tpS1
)
start.time <- Sys.time()
ggplot(A.df,aes(Year,fill = term[Topic]))+
  geom_bar(position = "dodge")
end.time <- Sys.time()
timetaken8 <- end.time - start.time
timetaken8
start.time <- Sys.time()
#For Topic 1,Yearwise Trend
index1 <- which(tpS1 == 1)
Year1 <- Abstract$year[index1]
A.df1 <- data.frame(
  document = index1,
  Year = Year1
)
A.df1 <- A.df1[order(A.df1$Year),]
A.df1 <- A.df1 %>% group_by(Year) %>% summarise(n())
colnames(A.df1) <- c("Year","Docs")
topic1 <- ggplot(A.df1,aes(Year,Docs))+
  geom_bar(stat = "identity",fill = "#FF3333") + geom_smooth(method = "lm",formula = y ~ splines::bs(x,3),se = FALSE)
print(topic1 + ggtitle("Topic 1 trend Year-Wise(1961-1990)") + labs(y = "No of Docs",x = "Year"))
#For Topic 2,Yearwise Trend
index2 <- which(tpS1 == 2)
Year2 <- Abstract$year[index2]
A.df2 <- data.frame(
  document= index2,
  Year = Year2
)
A.df2 <- A.df2[order(A.df2$Year),]
A.df2 <- A.df2 %>% group_by(Year) %>% summarise(n())
colnames(A.df2) <- c("Year","Docs")
topic2 <- ggplot(A.df2,aes(Year,Docs))+
  geom_bar(stat = "identity",fill = "#666666") + geom_smooth(method = "lm",formula = y ~ splines::bs(x,3),se = FALSE)
print(topic2 + ggtitle("Topic 2 trend Year-Wise(1961-1990)") + labs(y = "No of Docs",x = "Year"))
#For Topic 3,YearWise Trend
index3 <- which(tpS1 == 3)
Year3 <- Abstract$year[index3]
A.df3 <- data.frame(
  document= index3,
  Year = Year3
)
A.df3 <- A.df3[order(A.df3$Year),]
A.df3 <- A.df3 %>% group_by(Year) %>% summarise(n())
colnames(A.df3) <- c("Year","Docs")
topic3 <- ggplot(A.df3,aes(Year,Docs))+
  geom_bar(stat = "identity",fill = "#FF6666") + geom_smooth(method = "lm",formula = y ~ splines::bs(x,3),se = FALSE)
print(topic3 + ggtitle("Topic 3 trend Year-Wise(1961-1990)") + labs(y = "No of Docs",x = "Year"))
#For Topic 4,Yearwise Trend
index4 <- which(tpS1 == 4)
Year4 <- Abstract$year[index4]
A.df4 <- data.frame(
  document= index4,
  Year = Year4
)
A.df4 <- A.df4[order(A.df4$Year),]
A.df4 <- A.df4 %>% group_by(Year) %>% summarise(n())
colnames(A.df4) <- c("Year","Docs")
topic4 <- ggplot(A.df4,aes(Year,Docs))+
  geom_bar(stat = "identity",fill = "#003366") + geom_smooth(method = "lm",formula = y ~ splines::bs(x,3),se = FALSE)
print(topic4 + ggtitle("Topic 4 trend Year-Wise(1961-1990)") + labs(y = "No of Docs",x = "Year"))
#For Topic 5,Yearwise Trend
index5 <- which(tpS1 == 5)
Year5 <- Abstract$year[index5]
A.df5 <- data.frame(
  document= index5,
  Year = Year5
)
A.df5 <- A.df5[order(A.df5$Year),]
A.df5 <- A.df5 %>% group_by(Year) %>% summarise(n())
colnames(A.df5) <- c("Year","Docs")
topic5 <- ggplot(A.df5,aes(Year,Docs))+
  geom_bar(stat = "identity",fill = "#FF9933") + geom_smooth(method = "lm",formula = y ~ splines::bs(x,3),se = FALSE)
print(topic5 + ggtitle("Topic 5 trend Year-Wise(1961-1990)") + labs(y = "No of Docs",x = "Year"))
end.time <- Sys.time()
timetaken9 <- end.time - start.time
timetaken9


# For Abstracts from Year 1991-2010



Abstract2 <- read_csv("C:/Users/hp/Downloads/Abstract2.csv")
Abstract2 <- Abstract2[,1:2]
Abstract2 <- Abstract2 %>% na.omit()
Abstract21 <- Abstract2[,1]
scopuS2 <- Abstract21
# Creating a corpus
start.time <- Sys.time()
abstract_1991 <- VCorpus(DataframeSource(scopuS2))
end.time <- Sys.time()
timetaken10 <- end.time - start.time
# Cleaning corpus
start.time <- Sys.time()
cleaned_scopus_1991 <- clean_corpus(abstract_1991)
end.time <- Sys.time()
timetaken11 <- end.time - start.time
timetaken11
# Making a Document Term Matrix
start.time <- Sys.time()
cleaned_scopus_1991_dtm <- DocumentTermMatrix(cleaned_scopus_1991)
end.time <- Sys.time()
timetaken12 <- end.time - start.time
timetaken12
inspect(cleaned_scopus_1991_dtm)
cleaned1991m <- as.matrix(cleaned_scopus_1991_dtm)
termsc <- colSums(cleaned1991m)
termsc <- sort(termsc,decreasing = TRUE)
cleaned1991f <- data.frame(term = names(termsc),num = termsc)
cleaned1991f <- cleaned1991f %>% na.omit()
# Creating term frequency graph
ggplot(head(cleaned1991f,50),aes(term,num))+
  geom_col(show.legend = FALSE)+
  coord_flip()
cleaned1991df <- as.data.frame(cleaned1991m)
# Creating an elbowchart to find appropriate number of clusters
ratio_ss <- rep(0,15)
start.time <- Sys.time()
for(k in 1:15){
  sc2 <- kmeans(cleaned1991df,centers = k,iter.max = 20)
  ratio_ss[k] <- sc2$tot.withinss/sc2$totss
  
}
plot(ratio_ss,type = "b",xlab = "k")
end.time <- Sys.time()
timetaken13 <- end.time - start.time 
timetaken13
# Making appropriate number of clusters
start.time <- Sys.time()
S1991 <- kmeans(cleaned1991df,centers = 6,iter.max = 20)
end.time <- Sys.time()
timetaken14 <- end.time - start.time
S1991$size
# Generating Cluster Centers
for(i in 1:6){
  cat(paste("cluster",i,": ",sep = ""))
  b <- sort(S1991$centers[i, ],decreasing = TRUE)
  cat(names(b)[1:5],"\n")
  
}

# Finding terms in each cluster
S21 <- cleaned1991f[S1991$cluster == 1,]
S22 <- cleaned1991f[S1991$cluster == 2,]
S23 <- cleaned1991f[S1991$cluster == 3,]
S24 <- cleaned1991f[S1991$cluster == 4,]
S25 <- cleaned1991f[S1991$cluster == 5,]
S26 <- cleaned1991f[S1991$cluster == 6,]
# Creating wordclouds of each cluster and whole data
start.time <- Sys.time()
wordcloud(S21$term,S21$num,max.words=nrow(S21),colors=brewer.pal(8,"Dark2"),scale=c(3,0.5),random.order=F)
wordcloud(S22$term,S22$num,max.words=nrow(S22),colors=brewer.pal(8,"Dark2"),scale=c(3,0.5),random.order=F)
wordcloud(S23$term,S23$num,max.words=nrow(S23),colors=brewer.pal(8,"Dark2"),scale=c(3,0.5),random.order=F)
wordcloud(S24$term,S24$num,max.words=nrow(S24),colors=brewer.pal(8,"Dark2"),scale=c(3,0.5),random.order=F)
wordcloud(S25$term,S25$num,max.words=nrow(S25),colors=brewer.pal(8,"Dark2"),scale=c(3,0.5),random.order=F)
wordcloud(S26$term,S26$num,max.words=nrow(S26),colors=brewer.pal(8,"Dark2"),scale=c(3,0.5),random.order=F)
wordcloud(cleaned1991f$term,cleaned1991f$num,max.words= nrow(cleaned1991f),colors=brewer.pal(8,"Dark2"),scale=c(3,0.5),random.order=F)
end.time <- Sys.time()
timetaken15 <- end.time - start.time
timetaken15
# Assigning topics using LDA
start.time <- Sys.time()
lda2 <- LDA(cleaned_scopus_1991_dtm,k = 6,method = "Gibbs")
tpS2 <- topics(lda2,1)
end.time <-  Sys.time()
timetaken16 <- end.time - start.time
timetaken16
term <- terms(lda2,10)
term <- apply(term,MARGIN = 2,paste,collapse = ", ") %>% print

# Overall Trend of documents 
Year <- Abstract2[,2]
A2.df <- data.frame(
  document= 1:nrow(Abstract2),
  Year = Year,
  Topic = tpS2
)
start.time <- Sys.time()
ggplot(A2.df,aes(Year,fill = term[Topic]))+
  geom_bar(position = "dodge")
end.time <- Sys.time()
timetaken17 <- end.time - start.time
timetaken17
start.time <- Sys.time()
#For Topic 1,Yearwise Trend
index1 <- which(tpS2 == 1)
Year1 <- Abstract2$Year[index1]
A2.df1 <- data.frame(
  document= index1,
  Year = Year1
)
A2.df1 <- A2.df1[order(A2.df1$Year),]
A2.df1 <- A2.df1 %>% group_by(Year) %>% summarise(n())
colnames(A2.df1) <- c("Year","Docs")
topic1 <- ggplot(A2.df1,aes(Year,Docs))+
  geom_bar(stat = "identity",fill = "#FF3333") + geom_smooth(method = "lm",formula = y ~ splines::bs(x,3),se = FALSE)
print(topic1 + ggtitle("Topic 1 trend Year-Wise(1991-2010)") + labs(y = "No of Docs",x = "Year"))
#For Topic 2,Yearwise Trend
index2 <- which(tpS2 == 2)
Year2 <- Abstract2$Year[index2]
A2.df2 <- data.frame(
  document= index2,
  Year = Year2
)
A2.df2 <- A2.df2[order(A2.df2$Year),]
A2.df2 <- A2.df2 %>% group_by(Year) %>% summarise(n())
colnames(A2.df2) <- c("Year","Docs")
topic2 <- ggplot(A2.df2,aes(Year,Docs))+
  geom_bar(stat = "identity",fill = "#666666") + geom_smooth(method = "lm",formula = y ~ splines::bs(x,3),se = FALSE)
print(topic2 + ggtitle("Topic 2 trend Year-Wise(1991-2010)") + labs(y = "No of Docs",x = "Year"))
#For Topic 3,YearWise Trend
index3 <- which(tpS2 == 3)
Year3 <- Abstract2$Year[index3]
A2.df3 <- data.frame(
  document= index3,
  Year = Year3
)
A2.df3 <- A2.df3[order(A2.df3$Year),]
A2.df3 <- A2.df3 %>% group_by(Year) %>% summarise(n())
colnames(A2.df3) <- c("Year","Docs")
topic3 <- ggplot(A2.df3,aes(Year,Docs))+
  geom_bar(stat = "identity",fill = "#FF6666") + geom_smooth(method = "lm",formula = y ~ splines::bs(x,3),se = FALSE)
print(topic3 + ggtitle("Topic 3 trend Year-Wise(1991-2010)") + labs(y = "No of Docs",x = "Year"))
#For Topic 4,Yearwise Trend
index4 <- which(tpS2 == 4)
Year4 <- Abstract2$Year[index4]
A2.df4 <- data.frame(
  document= index4,
  Year = Year4
)
A2.df4 <- A2.df4[order(A2.df4$Year),]
A2.df4 <- A2.df4 %>% group_by(Year) %>% summarise(n())
colnames(A2.df4) <- c("Year","Docs")
topic4 <- ggplot(A2.df4,aes(Year,Docs))+
  geom_bar(stat = "identity",fill = "#003366") + geom_smooth(method = "lm",formula = y ~ splines::bs(x,3),se = FALSE)
print(topic4 + ggtitle("Topic 4 trend Year-Wise(1991-2010)") + labs(y = "No of Docs",x = "Year"))
#For Topic 5,Yearwise Trend
index5 <- which(tpS2 == 5)
Year5 <- Abstract2$Year[index5]
A2.df5 <- data.frame(
  document= index5,
  Year = Year5
)
A2.df5 <- A2.df5[order(A2.df5$Year),]
A2.df5 <- A2.df5 %>% group_by(Year) %>% summarise(n())
colnames(A2.df5) <- c("Year","Docs")
topic5 <- ggplot(A2.df5,aes(Year,Docs))+
  geom_bar(stat = "identity",fill = "#FF9933") + geom_smooth(method = "lm",formula = y ~ splines::bs(x,3),se = FALSE)
print(topic5 + ggtitle("Topic 5 trend Year-Wise(1991-2010)") + labs(y = "No of Docs",x = "Year"))
#For Topic 5,Yearwise Trend
index5 <- which(tpS2 == 5)
Year5 <- Abstract2$Year[index5]
A2.df5 <- data.frame(
  document= index5,
  Year = Year5
)
A2.df5 <- A2.df5[order(A2.df5$Year),]
A2.df5 <- A2.df5 %>% group_by(Year) %>% summarise(n())
colnames(A2.df5) <- c("Year","Docs")
topic5 <- ggplot(A2.df5,aes(Year,Docs))+
  geom_bar(stat = "identity",fill = "#FF9933") + geom_smooth(method = "lm",formula = y ~ splines::bs(x,3),se = FALSE)
print(topic5 + ggtitle("Topic 5 trend Year-Wise(1991-2010)") + labs(y = "No of Docs",x = "Year"))
#For Topic 6,Yearwise Trend
index6 <- which(tpS2 == 6)
Year6 <- Abstract2$Year[index6]
A2.df6 <- data.frame(
  document= index6,
  Year = Year6
)
A2.df6 <- A2.df6[order(A2.df6$Year),]
A2.df6 <- A2.df6 %>% group_by(Year) %>% summarise(n())
colnames(A2.df6) <- c("Year","Docs")
topic6 <- ggplot(A2.df6,aes(Year,Docs))+
  geom_bar(stat = "identity",fill = "#CC3333") + geom_smooth(method = "lm",formula = y ~ splines::bs(x,3),se = FALSE)
print(topic6 + ggtitle("Topic 6 trend Year-Wise(1991-2010)") + labs(y = "No of Docs",x = "Year"))
end.time <- Sys.time()
timetaken18 <- end.time - start.time
timetaken18

# For abstracts from Year 2011-2017
# Reading abstract file from PC
Abstract3 <- read_csv("C:/Users/hp/Downloads/Abstract3.csv")
Abstract3 <- Abstract3[,1:2]
Abstract3 <- Abstract3 %>% na.omit()
Abstract31 <- Abstract3[,1]

scopus3 <- Abstract31
# Creating a corpus
start.time <- Sys.time()
abstract_2011 <- VCorpus(DataframeSource(scopus3))
end.time <- Sys.time()
timetaken19 <- end.time - start.time
timetaken19
# Cleaning corpus
start.time <- Sys.time()
cleaned_scopus_2011 <- clean_corpus(abstract_2011)
end.time <- Sys.time()
timetaken20 <- end.time - start.time
timetaken20
# Making Document Term Matrix
start.time <- Sys.time()
cleaned_scopus_2011_dtm <- DocumentTermMatrix(cleaned_scopus_2011)
end.time <- Sys.time()
timetaken21 <- end.time - start.time
timetaken21
inspect(cleaned_scopus_2011_dtm)
cleaned2011m <- as.matrix(cleaned_scopus_2011_dtm)
termsc <- colSums(cleaned2011m)
termsc <- sort(termsc,decreasing = TRUE)
cleaned2011f <- data.frame(term = names(termsc),num = termsc)

cleaned2011f <- cleaned2011f %>% na.omit()
# plotting term frequency graph
ggplot(head(cleaned2011f,50),aes(term,num))+
  geom_col(show.legend = FALSE)+
  coord_flip()
cleaned2011df <- as.data.frame(cleaned2011m)
# Creating an elbowchart
ratio_ss <- rep(0,15)
start.time <- Sys.time()
for(k in 1:15){
  sc3 <- kmeans(cleaned2011df,centers = k,iter.max =20)
  ratio_ss[k] <- sc3$tot.withinss/sc3$totss
  
}
plot(ratio_ss,type = "b",xlab = "k")
end.time <- Sys.time()
timetaken22 <- end.time - start.time
timetaken22
start.time <- Sys.time()
#Making cluster according to elbowchart
S3011 <- kmeans(cleaned2011df,centers = 6,iter.max = 20)
end.time <- Sys.time()
timetaken23 <- end.time - start.time
timetaken23
S3011$size
# Generating Ckuster Center Terms
for(i in 1:6){
  cat(paste("cluster",i,": ",sep = ""))
  b <- sort(S3011$centers[i, ],decreasing = TRUE)
  cat(names(b)[1:5],"\n")
}
# Finding terms in each cluster
S31 <- cleaned2011f[S3011$cluster == 1,]
S32 <- cleaned2011f[S3011$cluster == 2,]
S33 <- cleaned2011f[S3011$cluster == 3,]
S34 <- cleaned2011f[S3011$cluster == 4,]
S35 <- cleaned2011f[S3011$cluster == 5,]
S36 <- cleaned2011f[S3011$cluster == 6,]
# Plotting wordclouds of each clusterand whole data
start.time <- Sys.time()
wordcloud(S31$term,S31$num,max.words=nrow(S31),colors=brewer.pal(8,"Dark2"),scale=c(3,0.5),random.order=F)
wordcloud(S32$term,S32$num,max.words=nrow(S32),colors=brewer.pal(8,"Dark2"),scale=c(3,0.5),random.order=F)
wordcloud(S33$term,S33$num,max.words=nrow(S33),colors=brewer.pal(8,"Dark2"),scale=c(3,0.5),random.order=F)
wordcloud(S34$term,S34$num,max.words=nrow(S34),colors=brewer.pal(8,"Dark2"),scale=c(3,0.5),random.order=F)
wordcloud(S35$term,S35$num,max.words=nrow(S35),colors=brewer.pal(8,"Dark2"),scale=c(3,0.5),random.order=F)
wordcloud(S36$term,S36$num,max.words=nrow(S36),colors=brewer.pal(8,"Dark2"),scale=c(3,0.5),random.order=F)
wordcloud(cleaned2011f$term,cleaned2011f$num,max.words= nrow(cleaned2011f),colors=brewer.pal(8,"Dark2"),scale=c(3,0.5),random.order=F)
end.time <- Sys.time()
timetaken24 <- end.time - start.time
timetaken24
# Assigning topics using LDA
start.time <- Sys.time()
lda3 <- LDA(cleaned_scopus_2011_dtm,k = 6,method = "Gibbs")
tpS3 <- topics(lda3,1)
end.time <- Sys.time()
timetaken25 <- end.time - start.time
timetaken25
term <- terms(lda3,10)
term <- apply(term,MARGIN = 2,paste,collapse = ", ") %>% print

# Overall Trend of documents 
Year <- Abstract3[,2]
A3.df <- data.frame(
  document= 1:nrow(Abstract3),
  Year = Year,
  Topic = tpS3
)
start.tme <- Sys.time()
ggplot(A3.df,aes(Year,fill = term[Topic]))+
  geom_bar(position = "dodge")
end.time <- Sys.time()
timetaken26 <- end.time - start.tme
timetaken26
start.time <- Sys.time()
#For Topic 1,Yearwise Trend
index1 <- which(tpS3 == 1)
Year1 <- Abstract3$Year[index1]
A3.df1 <- data.frame(
  document= index1,
  Year = Year1
)
A3.df1 <- A3.df1[order(A3.df1$Year),]
A3.df1 <- A3.df1 %>% group_by(Year) %>% summarise(n())
colnames(A3.df1) <- c("Year","Docs")
topic1 <- ggplot(A3.df1,aes(Year,Docs))+
  geom_bar(stat = "identity",fill = "#FF3333") + geom_smooth(method = "lm",formula = y ~ splines::bs(x,3),se = FALSE)
print(topic1 + ggtitle("Topic 1 trend Year-Wise(2011-2017)") + labs(y = "No of Docs",x = "Year"))
#For Topic 2,Yearwise Trend
index2 <- which(tpS3 == 2)
Year2 <- Abstract3$Year[index2]
A3.df2 <- data.frame(
  document= index2,
  Year = Year2
)
A3.df2 <- A3.df2[order(A3.df2$Year),]
A3.df2 <- A3.df2 %>% group_by(Year) %>% summarise(n())
colnames(A3.df2) <- c("Year","Docs")
topic2 <- ggplot(A3.df2,aes(Year,Docs))+
  geom_bar(stat = "identity",fill = "#666666") + geom_smooth(method = "lm",formula = y ~ splines::bs(x,3),se = FALSE)
print(topic2 + ggtitle("Topic 2 trend Year-Wise(2011-2017)") + labs(y = "No of Docs",x = "Year"))
#For Topic 3,YearWise Trend
index3 <- which(tpS3 == 3)
Year3 <- Abstract3$Year[index3]
A3.df3 <- data.frame(
  document= index3,
  Year = Year3
)
A3.df3 <- A3.df3[order(A3.df3$Year),]
A3.df3 <- A3.df3 %>% group_by(Year) %>% summarise(n())
colnames(A3.df3) <- c("Year","Docs")
topic3 <- ggplot(A3.df3,aes(Year,Docs))+
  geom_bar(stat = "identity",fill = "#FF6666") + geom_smooth(method = "lm",formula = y ~ splines::bs(x,3),se = FALSE)
print(topic3 + ggtitle("Topic 3 trend Year-Wise(2011-2017)") + labs(y = "No of Docs",x = "Year"))
#For Topic 4,Yearwise Trend
index4 <- which(tpS3 == 4)
Year4 <- Abstract3$Year[index4]
A3.df4 <- data.frame(
  document= index4,
  Year = Year4
)
A3.df4 <- A3.df4[order(A3.df4$Year),]
A3.df4 <- A3.df4 %>% group_by(Year) %>% summarise(n())
colnames(A3.df4) <- c("Year","Docs")
topic4 <- ggplot(A3.df4,aes(Year,Docs))+
  geom_bar(stat = "identity",fill = "#003366") + geom_smooth(method = "lm",formula = y ~ splines::bs(x,3),se = FALSE)
print(topic4 + ggtitle("Topic 4 trend Year-Wise(2011-2017)") + labs(y = "No of Docs",x = "Year"))
#For Topic 5,Yearwise Trend
index5 <- which(tpS3 == 5)
Year5 <- Abstract3$Year[index5]
A3.df5 <- data.frame(
  document= index5,
  Year = Year5
)
A3.df5 <- A3.df5[order(A3.df5$Year),]
A3.df5 <- A3.df5 %>% group_by(Year) %>% summarise(n())
colnames(A3.df5) <- c("Year","Docs")
topic5 <- ggplot(A3.df5,aes(Year,Docs))+
  geom_bar(stat = "identity",fill = "#FF9933") + geom_smooth(method = "lm",formula = y ~ splines::bs(x,3),se = FALSE)
print(topic5 + ggtitle("Topic 5 trend Year-Wise(2011-2017)") + labs(y = "No of Docs",x = "Year"))

#For Topic 6,Yearwise Trend
index6 <- which(tpS3 == 6)
Year6 <- Abstract3$Year[index6]
A3.df6 <- data.frame(
  document= index6,
  Year = Year6
)
A3.df6 <- A3.df6[order(A3.df6$Year),]
A3.df6 <- A3.df6 %>% group_by(Year) %>% summarise(n())
colnames(A3.df6) <- c("Year","Docs")
topic6 <- ggplot(A3.df6,aes(Year,Docs))+
  geom_bar(stat = "identity",fill = "#CC3333") + geom_smooth(method = "lm",formula = y ~ splines::bs(x,3),se = FALSE)
print(topic6 + ggtitle("Topic 6 trend Year-Wise(2011-2017)") + labs(y = "No of Docs",x = "Year"))
end.time <- Sys.time()
timetaken27 <- end.time - start.time
timetaken27


























