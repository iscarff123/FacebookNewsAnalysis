FB <- read.csv("FacebookData.csv")

View(FB)

library(lubridate)

FB$posted_at <- ymd_hms(FB$posted_at)

FB[23] <- year(FB$posted_at)
colnames(FB)[23] <- "year"


min(FB$posted_at)
max(FB$posted_at)

levels(FB$status_type)

library(plotly)

plot_ly(data = FB, labels = ~Source, type = 'pie') %>%
  layout(title = 'Data by News Source',
         xaxis = list(showgrid = F, zeroline = F, showticklabels = F))
 
plot_ly(data = FB, labels = ~year, type = 'pie', textposition = 'inside',
        textinfo = 'label+percent', text = ~paste('Year: ', year),
        showlegend = F) %>%
  layout(title = 'Data by Year',
         xaxis = list(showgrid = F, zeroline = F, showticklabels = F))


years <- c(2012:2016)

TotReact <- as.data.frame(matrix(data = NA, nrow = 15, ncol = 6))


colnames(TotReact) <- c("News",paste("Y",years,sep=""))

TotReact[,1] <- levels(FB$Source)

for (i in 1:length(levels(FB$Source))){
  
  for (j in 1:length(years)){
    
    if (any((subset(FB, (Source == levels(FB$Source)[i]), select = year)) == years[j])){
      
      TotReact[i,j+1] <- sum(subset(FB, (Source == levels(FB$Source)[i] & year == years[j]), 
                                  select = 
                                    c(likes_count,love_count,wow_count,haha_count,sad_count,
                                      thankful_count,
                                      angry_count)))
    }
    
    else{
      
      TotReact[i,j+1] <- 0
    }
    

  }
  
}



plot_ly(TotReact, x = ~News, y = ~Y2012, type = 'bar', name = '2012') %>%
  add_trace(y = ~Y2013, name = '2013') %>%
  add_trace(y = ~Y2014, name = '2014') %>%
  add_trace(y = ~Y2015, name = '2015') %>%
  add_trace(y = ~Y2016, name = '2016') %>%
  layout(yaxis = list(title = '# of Reactions'), barmode = 'group')




TotComm <- as.data.frame(matrix(data = NA, nrow = 15, ncol = 6))


colnames(TotComm) <- c("News",paste("Y",years,sep=""))

TotComm[,1] <- levels(FB$Source)


for (i in 1:length(levels(FB$Source))){
  
  for (j in 1:length(years)){
    
    if (any((subset(FB, (Source == levels(FB$Source)[i]), select = year)) == years[j])){
      
      TotComm[i,j+1] <- sum(subset(FB, (Source == levels(FB$Source)[i] & year == years[j]), 
                                    select = 
                                      c(comments_count)))
    }
    
    else{
      
      TotComm[i,j+1] <- 0
    }
    
    
  }
  
}

plot_ly(TotComm, x = ~News, y = ~Y2012, type = 'bar', name = '2012') %>%
  add_trace(y = ~Y2013, name = '2013') %>%
  add_trace(y = ~Y2014, name = '2014') %>%
  add_trace(y = ~Y2015, name = '2015') %>%
  add_trace(y = ~Y2016, name = '2016') %>%
  layout(yaxis = list(title = '# of Comments'), barmode = 'group')






TotShare <- as.data.frame(matrix(data = NA, nrow = 15, ncol = 6))


colnames(TotShare) <- c("News",paste("Y",years,sep=""))

TotShare[,1] <- levels(FB$Source)


for (i in 1:length(levels(FB$Source))){
  
  for (j in 1:length(years)){
    
    if (any((subset(FB, (Source == levels(FB$Source)[i]), select = year)) == years[j])){
      
      TotShare[i,j+1] <- sum(subset(FB, (Source == levels(FB$Source)[i] & year == years[j]), 
                                   select = 
                                     c(shares_count)))
    }
    
    else{
      
      TotShare[i,j+1] <- 0
    }
    
    
  }
  
}

plot_ly(TotShare, x = ~News, y = ~Y2012, type = 'bar', name = '2012') %>%
  add_trace(y = ~Y2013, name = '2013') %>%
  add_trace(y = ~Y2014, name = '2014') %>%
  add_trace(y = ~Y2015, name = '2015') %>%
  add_trace(y = ~Y2016, name = '2016') %>%
  layout(yaxis = list(title = '# of Shares'), barmode = 'group')





TotStories <- as.data.frame(matrix(data = NA, nrow = 15, ncol = 6))


colnames(TotStories) <- c("News",paste("Y",years,sep=""))

TotStories[,1] <- levels(FB$Source)


for (i in 1:length(levels(FB$Source))){
  
  for (j in 1:length(years)){
    
    if (any((subset(FB, (Source == levels(FB$Source)[i]), select = year)) == years[j])){
      
      TotStories[i,j+1] <- nrow(subset(FB, (Source == levels(FB$Source)[i] & year == years[j]), 
                                    select = 
                                      c(shares_count)))
    }
    
    else{
      
      TotStories[i,j+1] <- 0
    }
    
    
  }
  
}

plot_ly(TotStories, x = ~News, y = ~Y2012, type = 'bar', name = '2012') %>%
  add_trace(y = ~Y2013, name = '2013') %>%
  add_trace(y = ~Y2014, name = '2014') %>%
  add_trace(y = ~Y2015, name = '2015') %>%
  add_trace(y = ~Y2016, name = '2016') %>%
  layout(yaxis = list(title = '# of Stories'), barmode = 'group')




####################################################
###################################################
################################################

# Make box plots with log transformation

plot_ly(data = FB, y = ~log(likes_count + 1), color = ~Source, type = 'box') %>%
  layout(yaxis = list(title = "Log of (Likes + 1)"))

plot_ly(data = FB, y = ~log(comments_count + 1), color = ~Source, type = 'box') %>%
  layout(yaxis = list(title = "Log of (Comments + 1)"))

plot_ly(data = FB, y = ~log(shares_count + 1), color = ~Source, type = 'box') %>%
  layout(yaxis = list(title = "Log of (Shares + 1)"))

###################################################
#################################################
#################################################


# Text mining

library(tm)


FB$name <- as.character(FB$name)

### combine all names together

FB2 <- FB[FB$name != "NULL",]


### Separate by year
TopWordsPerYear <- list()


for (i in 1:length(years)){
  
  names_text <- paste(FB2[FB2$year == years[i], 4], collapse = " ")
 
  names_source <- VectorSource(names_text)
  names_corpus <- Corpus(names_source)
  
  names_corpus <- tm_map(names_corpus, content_transformer(tolower))
  names_corpus <- tm_map(names_corpus, removePunctuation)
  names_corpus <- tm_map(names_corpus, stripWhitespace)
  names_corpus <- tm_map(names_corpus, removeWords, stopwords("english"))
  
  names_dtm <- DocumentTermMatrix(names_corpus)
  names_dtm <- as.matrix(names_dtm)
  
  names_frequency <- colSums(names_dtm)
  names_frequency <- sort(names_frequency, decreasing = T)
  
  words <- names(names_frequency)
  
  wordsData <- as.data.frame(matrix(data = NA, nrow = length(words), ncol = 2))
  
  wordsData[,1] <- words
  wordsData[,2] <- names_frequency
  
  TopWordsPerYear[[i]] <- as.data.frame(wordsData)
  names(TopWordsPerYear)[i] <- paste("Year: ", years[i], sep = "")
}



#####
TopWordsPerSource <- list()

for (i in 1:length(levels(FB2$Source))){
  
  names_text <- paste(FB2[FB2$Source == levels(FB2$Source), 4], collapse = " ")
  
  names_source <- VectorSource(names_text)
  names_corpus <- Corpus(names_source)
  
  names_corpus <- tm_map(names_corpus, content_transformer(tolower))
  names_corpus <- tm_map(names_corpus, removePunctuation)
  names_corpus <- tm_map(names_corpus, stripWhitespace)
  names_corpus <- tm_map(names_corpus, removeWords, stopwords("english"))
  
  names_dtm <- DocumentTermMatrix(names_corpus)
  names_dtm <- as.matrix(names_dtm)
  
  names_frequency <- colSums(names_dtm)
  names_frequency <- sort(names_frequency, decreasing = T)
  
  words <- names(names_frequency)
  
  wordsData <- as.data.frame(matrix(data = NA, nrow = length(words), ncol = 2))
  
  wordsData[,1] <- words
  wordsData[,2] <- names_frequency
  
  TopWordsPerSource[[i]] <- as.data.frame(wordsData)
  names(TopWordsPerSource)[i] <- paste(levels(FB2$Source)[i], sep = "")
}



### Separate by Source and year

TopWordsPerSourceByYear <- list()
p <- 1

for (i in 1:length(levels(FB$Source))){
  
  for (j in 1:length(years)){
    
    if (any((subset(FB, (Source == levels(FB$Source)[i]), select = year)) == years[j])){
      
      tempData <- (subset(FB2, (Source == levels(FB2$Source)[i] & year == years[j])))
      
      
      names_text <- paste(tempData[tempData$year == years[j], 4], collapse = " ")
      
      names_source <- VectorSource(names_text)
      names_corpus <- Corpus(names_source)
      
      names_corpus <- tm_map(names_corpus, content_transformer(tolower))
      names_corpus <- tm_map(names_corpus, removePunctuation)
      names_corpus <- tm_map(names_corpus, stripWhitespace)
      names_corpus <- tm_map(names_corpus, removeWords, stopwords("english"))
      
      names_dtm <- DocumentTermMatrix(names_corpus)
      names_dtm <- as.matrix(names_dtm)
      
      names_frequency <- colSums(names_dtm)
      names_frequency <- sort(names_frequency, decreasing = T)
      
      words <- names(names_frequency)
      
      wordsData <- as.data.frame(matrix(data = NA, nrow = length(words), ncol = 2))
      
      wordsData[,1] <- words
      wordsData[,2] <- names_frequency
      
      TopWordsPerSourceByYear[[p]] <- as.data.frame(wordsData)
      names(TopWordsPerSourceByYear)[p] <- paste(levels(FB2$Source)[i], years[j], sep = "_")
      
      p <- p + 1
    }
    
    else{
      
      TopWordsPerSourceByYear[[p]] <- 0
      names(TopWordsPerSourceByYear)[p] <- paste(levels(FB2$Source)[i], years[j], sep = "_")
      
      p <- p + 1
    }
    
    
  }
  
}


### Make functions

SourceYearWordCloud <- function(NewsSource, Year){
  wordcloud(TopWordsPerSourceByYear[[paste(NewsSource,Year,sep = "_")]][1:100,1],
            TopWordsPerSourceByYear[[paste(NewsSource,Year,sep = "_")]][1:100,2],random.order = F,
            colors = c((rep("red", times = 20)), rep("black", times = 80)),
            ordered.colors = T)
  text(x=0.5, y=0.9, paste("Top 100 Words in News Stories by",NewsSource,"in",Year,sep = " "))
  
}

SourceWordCloud <- function(NewsSource){
  wordcloud(TopWordsPerSource[[paste(NewsSource)]][1:100,1],
            TopWordsPerSource[[paste(NewsSource)]][1:100,2],random.order = F,
            colors = c((rep("red", times = 20)), rep("black", times = 80)),
            ordered.colors = T)
  text(x=0.5, y=0.9, paste("Top 100 Words in News Stories by",NewsSource,sep = " "))
}

YearWordCloud <- function(Year){
  wordcloud(TopWordsPerYear[[paste("Year: ",Year, sep = "")]][1:100,1],
            TopWordsPerYear[[paste("Year: ",Year, sep = "")]][1:100,2],random.order = F,
            colors = c((rep("red", times = 20)), rep("black", times = 80)),
            ordered.colors = T)
  text(x=0.5, y=0.9, paste("Top 100 Words in News Stories in",Year,sep = " "))
  
  
}


SourceYearWordCloud("CNN",2012)
SourceWordCloud("FOX_NEWS")
YearWordCloud(2016)

for (i in 1:length(years)){
  try(YearWordCloud(years[i]))
  
  
}



###########################

names_text <- paste(FB2$name, collapse = " ")

### Source and corpus
names_source <- VectorSource(names_text)
names_corpus <- Corpus(names_source)

### Clean
names_corpus <- tm_map(names_corpus, content_transformer(tolower))
names_corpus <- tm_map(names_corpus, removePunctuation)
names_corpus <- tm_map(names_corpus, stripWhitespace)
names_corpus <- tm_map(names_corpus, removeWords, stopwords("english"))

### making a document-term matrix

names_dtm <- DocumentTermMatrix(names_corpus)
names_dtm <- as.matrix(names_dtm)


### Finding the most frequent terms
names_frequency <- colSums(names_dtm)
names_frequency <- sort(names_frequency, decreasing = T)

head(names_frequency, n = 10)

library(wordcloud)


words <- names(names_frequency)

wordcloud(words[1:100], names_frequency[1:100], random.order = F,
          colors = c((rep("red", times = 30)), rep("black", times = 70)),
          ordered.colors = T)
text(x=0.5, y=0.92, "Title of my first plot", cex = 1.7)




library(ggplot2)


r <- sample(1:nrow(FB), size = 0.25*nrow(FB))


FB3 <- FB[r,]
FB3$year <- as.factor(FB3$year)

ggplot(data = FB3, mapping = aes(x = log(comments_count), y = log(likes_count), color = year)) + facet_wrap(~Source) + geom_point(alpha = 0.3)





######################################################################
#####################################################################
#################################################################


FB <- read.csv("FacebookDataReduced.csv")

FB$name <- as.character(FB$name)
FB <- FB[FB$name != "null",]

summary(FB$likes_count)
summary(FB$comments_count)

library(tm)

textFreq <- function(Data){
  
  names_text <- paste(Data[3], collapse = " ")
  
  ### Source and corpus
  names_source <- VectorSource(names_text)
  names_corpus <- Corpus(names_source)
  
  ### Clean
  names_corpus <- tm_map(names_corpus, content_transformer(tolower))
  names_corpus <- tm_map(names_corpus, removePunctuation)
  names_corpus <- tm_map(names_corpus, stripWhitespace)
  names_corpus <- tm_map(names_corpus, removeWords, stopwords("english"))
  
  ### making a document-term matrix
  
  names_dtm <- DocumentTermMatrix(names_corpus)
  names_dtm <- as.matrix(names_dtm)
  
  
  ### Finding the most frequent terms
  names_frequency <- colSums(names_dtm)
  names_frequency <- sort(names_frequency, decreasing = T)
  
  words <- names(names_frequency)

  return(data.frame(words,names_frequency))
}

greaterThanMedian <- textFreq(FB2[FB2$likes_count >= median(FB2$likes_count),])



library(wordcloud)
plot.new()

wordcloud(greaterThanMedian$words[3:102], 
          greaterThanMedian$names_frequency[3:102], random.order = F,
          colors = c((rep("red", times = 30)), rep("black", times = 70)),
          ordered.colors = T)
text(x=0.5, y=0.91, "Top 100 Words where Story Likes are >= Median", cex = 1)



###########################################################################
###########################################################################
###########################################################################
###########################################################################

cnn <- FB[FB$Source == "CNN",]
CNN_Mass <- cnn[str_detect(cnn$name,"mass shooting"),]

fox <- FB[FB$Source == "FOX_NEWS",]
FOX_MASS <- fox[str_detect(fox$name,"mass shooting"),]


abc <- FB[FB$Source == "ABC_NEWS",]
ABC_MASS <- abc[str_detect(abc$name,"mass shooting"),]


huff <- FB[FB$Source == "HUFF_POST",]
HUFF_MASS <- huff[str_detect(huff$name,"mass shooting"),]


months <- month.name

timeline <- seq(as.Date("2012/01/01"), 
                        as.Date("2016/11/01"),
                        "month")


cnn_occ <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    cnn_occ[k] <- nrow(subset(CNN_Mass, (year == years[i] & month == months[j]), 
                               select = name))
    k = k + 1
  }
}




fox_occ <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    fox_occ[k] <- nrow(subset(FOX_MASS, (year == years[i] & month == months[j]), 
                              select = name))
    k = k + 1
  }
}




abc_occ <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    abc_occ[k] <- nrow(subset(ABC_MASS, (year == years[i] & month == months[j]), 
                              select = name))
    k = k + 1
  }
}




huff_occ <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    huff_occ[k] <- nrow(subset(HUFF_MASS, (year == years[i] & month == months[j]), 
                              select = name))
    k = k + 1
  }
}




abc_occ <- abc_occ[-60]
cnn_occ <- cnn_occ[-60]
huff_occ <- huff_occ[-60]
fox_occ <-  fox_occ[-60]


massShoot <- data.frame(abc_occ,cnn_occ,huff_occ,fox_occ, timeline)


ggplot(data = massShoot, mapping = aes(x = timeline, y = cnn_occ)) +
  geom_line(color = "red", size = 2, linetype = "solid") +
  geom_line(data = massShoot, aes(x = timeline, y = huff_occ), color = "green",
            inherit.aes = F, size = 2, linetype = "dotted") +
  geom_line(data = massShoot, aes(x = timeline, y = fox_occ), color = "blue", 
            inherit.aes = F, size = 2, linetype = "dotdash") +
  geom_line(data = massShoot, aes(x = timeline, y = abc_occ), color = "black", 
            inherit.aes = F, size = 2, linetype = "longdash") +
  scale_x_date(breaks = timeline, date_labels = "%Y-%m") + theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1), 
    axis.text.x = element_text(angle = 90))+labs(x = "Time", y = "Freq of Phrase 'Mass Shooting(s)'")+labs(title = "Frequency of Phrase \"Mass Shooting(s)\"", y = "Frequency")


g1 <- ggplot(data = massShoot, mapping = aes(x = timeline, y = cnn_occ)) +
  geom_line(color = "red", size = 2, linetype = "solid") + expand_limits(y = c(0,60)) +
  scale_x_date(breaks = timeline, date_labels = "%m-%y") + theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1), 
    axis.text.x = element_text(vjust = 0.25, 
        angle = 90)) +labs(title = "CNN", x = "Time", y = "Frequency of Phrase 'Mass Shooting(s)'")


g2 <- ggplot(data = massShoot, mapping = aes(x = timeline, y = abc_occ)) +
  geom_line(color = "black", size = 2, linetype = "solid")+ expand_limits(y = c(0,60)) +
  scale_x_date(breaks = timeline, date_labels = "%m-%y")+ theme(plot.subtitle = element_text(vjust = 1), 
                                                                  plot.caption = element_text(vjust = 1), 
                                                                  axis.text.x = element_text(vjust = 0.25, 
                                                                                             angle = 90)) +labs(title = "ABC NEWS", x = "Time", y = "Frequency of Phrase 'Mass Shooting(s)'")

g3 <- ggplot(data = massShoot, mapping = aes(x = timeline, y = fox_occ)) +
  geom_line(color = "blue", size = 2, linetype = "solid")+ expand_limits(y = c(0,60)) +
  scale_x_date(breaks = timeline, date_labels = "%m-%y") + theme(plot.subtitle = element_text(vjust = 1), 
                                                                    plot.caption = element_text(vjust = 1), 
                                                                    axis.text.x = element_text(vjust = 0.25, 
                                                                                               angle = 90)) +labs(title = "FOX NEWS", x = "Time", y = "Frequency of Phrase 'Mass Shooting(s)'")

g4 <- ggplot(data = massShoot, mapping = aes(x = timeline, y = huff_occ)) +
  geom_line(color = "green", size = 2, linetype = "solid")+ expand_limits(y = c(0,60)) +
  scale_x_date(breaks = timeline, date_labels = "%m-%y") + theme(plot.subtitle = element_text(vjust = 1), 
                                                                     plot.caption = element_text(vjust = 1), 
                                                                     axis.text.x = element_text(vjust = 0.25, 
                                                                                                angle = 90)) +labs(title = "HUFFINGTON POST", x = "Time", y = "Frequency of Phrase 'Mass Shooting(s)'")


library(ggpubr)

ggarrange(g1,g2,g3,g4, ncol = 2, nrow = 2)
