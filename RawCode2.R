
library(plotly)
library(ggplot2)
library(ggpubr)
library(tm)
library(lubridate)
library(wordcloud)
library(stringr)
library(data.table)
library(scales)
library(extrafont)
loadfonts()


FB2 <- read.csv("FacebookData.csv")

View(FB2)

FB2$posted_at <- ymd_hms(FB2$posted_at)

FB2[23] <- year(FB2$posted_at)
colnames(FB2)[23] <- "year"


plot_ly(data = FB2, labels = ~Source, type = 'pie') %>%
  layout(title = 'Data by News Source',
         xaxis = list(showgrid = F, zeroline = F, showticklabels = F))

plot_ly(data = FB2, labels = ~year, type = 'pie', textposition = 'inside',
        textinfo = 'label+percent', text = ~paste('Year: ', year),
        showlegend = F) %>%
  layout(title = 'Data by Year',
         xaxis = list(showgrid = F, zeroline = F, showticklabels = F))



ToT <- as.data.frame(matrix(data = NA, nrow = 15, ncol = 2))
colnames(ToT) <- c("News","Number of Actions")
ToT[,1] <- levels(FB2$Source)



for(i in 1:length(levels(FB2$Source))){
  ToT[i,2] <- sum(subset(FB2, (Source == levels(FB2$Source)[i]), 
                                select = 
                                  c(likes_count,love_count,wow_count,haha_count,sad_count,
                                    thankful_count,
                                    angry_count, likes_count,comments_count,
                                    shares_count)))
}

plot_ly(ToT, x = ~News, y = ~`Number of Actions`, type = 'bar', name = '2012')


TotReact <- as.data.frame(matrix(data = NA, nrow = 15, ncol = 6))


colnames(TotReact) <- c("News",paste("Y",years,sep=""))

TotReact[,1] <- levels(FB2$Source)

for (i in 1:length(levels(FB2$Source))){
  
  for (j in 1:length(years)){
    
    if (any((subset(FB2, (Source == levels(FB2$Source)[i]), select = year)) == years[j])){
      
      TotReact[i,j+1] <- sum(subset(FB2, (Source == levels(FB2$Source)[i] & year == years[j]), 
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

TotComm[,1] <- levels(FB2$Source)


for (i in 1:length(levels(FB2$Source))){
  
  for (j in 1:length(years)){
    
    if (any((subset(FB2, (Source == levels(FB2$Source)[i]), select = year)) == years[j])){
      
      TotComm[i,j+1] <- sum(subset(FB2, (Source == levels(FB2$Source)[i] & year == years[j]), 
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

TotShare[,1] <- levels(FB2$Source)


for (i in 1:length(levels(FB2$Source))){
  
  for (j in 1:length(years)){
    
    if (any((subset(FB2, (Source == levels(FB2$Source)[i]), select = year)) == years[j])){
      
      TotShare[i,j+1] <- sum(subset(FB2, (Source == levels(FB2$Source)[i] & year == years[j]), 
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

TotStories[,1] <- levels(FB2$Source)


for (i in 1:length(levels(FB2$Source))){
  
  for (j in 1:length(years)){
    
    if (any((subset(FB2, (Source == levels(FB2$Source)[i]), select = year)) == years[j])){
      
      TotStories[i,j+1] <- nrow(subset(FB2, (Source == levels(FB2$Source)[i] & year == years[j]), 
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


ggplot(data = FB2, mapping = aes(x = log(comments_count), 
                                 y = log(likes_count), color = as.factor(year))) +
  facet_wrap(~Source) + geom_point(alpha = 0.3) +
  labs(x = "Log(Comments)", y = "Log(Likes)", title = "Log(Comments) vs. Log(Likes)",
       subtitle = "Separated by Source") + scale_color_discrete(name = "Year")

####################################################
###################################################
################################################

# Make box plots with log transformation

plot_ly(data = FB2, y = ~log(likes_count + 1), color = ~Source, type = 'box') %>%
  layout(yaxis = list(title = "Log of (Likes + 1)"))

plot_ly(data = FB2, y = ~log(comments_count + 1), color = ~Source, type = 'box') %>%
  layout(yaxis = list(title = "Log of (Comments + 1)"))

plot_ly(data = FB2, y = ~log(shares_count + 1), color = ~Source, type = 'box') %>%
  layout(yaxis = list(title = "Log of (Shares + 1)"))




#######################################


FB <- read.csv("4Sources.csv")
FB$name <- as.character(FB$name)
FB <- FB[FB$name != "null",]

cnn <- FB[FB$Source == "CNN",]
abc <- FB[FB$Source == "ABC_NEWS",]
fox <- FB[FB$Source == "FOX_NEWS",]
huff <- FB[FB$Source == "HUFF_POST",]



years <- c(2012:2016)
months <- month.name



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



TopWordsPerYear <- list()


for (i in 1:length(years)){
  
  names_text <- paste(FB[FB$year == years[i], 3], collapse = " ")
  
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


YearWordCloud <- function(Year){
  wordcloud(TopWordsPerYear[[paste("Year: ",Year, sep = "")]][1:100,1],
            TopWordsPerYear[[paste("Year: ",Year, sep = "")]][1:100,2],random.order = F,
            colors = c((rep("red", times = 20)), rep("black", times = 80)),
            ordered.colors = T)
  text(x=0.5, y=0.9, paste("Top 100 Words in News Stories in",Year,sep = " "))
  
  
}



for (i in 1:length(years)){
  try(YearWordCloud(years[i]))
  
  
}


wordcloud(TopWordsPerYear[["Year: 2012"]][3:102,1],
          TopWordsPerYear[["Year: 2012"]][3:102,2],random.order = F,
          colors = c((rep("red", times = 20)), rep("black", times = 80)),
          ordered.colors = T)
text(x=0.5, y=0.97, paste("Top 100 Words in News Stories in 2012" ,sep = " "))


wordcloud(TopWordsPerYear[["Year: 2013"]][3:102,1],
          TopWordsPerYear[["Year: 2013"]][3:102,2],random.order = F,
          colors = c((rep("red", times = 20)), rep("black", times = 80)),
          ordered.colors = T)
text(x=0.5, y=0.95, paste("Top 100 Words in News Stories in 2013" ,sep = " "))



wordcloud(TopWordsPerYear[["Year: 2014"]][3:102,1],
          TopWordsPerYear[["Year: 2014"]][3:102,2],random.order = F,
          colors = c((rep("red", times = 20)), rep("black", times = 80)),
          ordered.colors = T)
text(x=0.5, y=0.966, paste("Top 100 Words in News Stories in 2014" ,sep = " "))


wordcloud(TopWordsPerYear[["Year: 2015"]][3:102,1],
          TopWordsPerYear[["Year: 2015"]][3:102,2],random.order = F,
          colors = c((rep("red", times = 20)), rep("black", times = 80)),
          ordered.colors = T)
text(x=0.5, y=0.97, paste("Top 100 Words in News Stories in 2015" ,sep = " "))


wordcloud(TopWordsPerYear[["Year: 2016"]][3:102,1],
          TopWordsPerYear[["Year: 2016"]][3:102,2],random.order = F,
          colors = c((rep("red", times = 20)), rep("black", times = 80)),
          ordered.colors = T)
text(x=0.5, y=0.98, paste("Top 100 Words in News Stories in 2016" ,sep = " "))


#############################################################
#############################################################
#############################################################
#############################################################
#############################################################
#############################################################
#############################################################
#############################################################

ggplot(data = FB, mapping = aes(x = log(comments_count), 
                                y = log(likes_count), color = as.factor(year))) +
  facet_wrap(~Source) + geom_point(alpha = 0.3) + 
  labs(x = "Log(Comments)", y = "Log(Likes)", title = "Log(Comments) vs. Log(Likes)",
       subtitle = "Separated by Source") + scale_color_discrete(name = "Year")




#####################Shooting Graph###############################################


cnn <- FB[FB$Source == "CNN",]
CNN_Mass <- cnn[str_detect(cnn$name,"shooting"),]

fox <- FB[FB$Source == "FOX_NEWS",]
FOX_MASS <- fox[str_detect(fox$name,"shooting"),]


abc <- FB[FB$Source == "ABC_NEWS",]
ABC_MASS <- abc[str_detect(abc$name,"shooting"),]


huff <- FB[FB$Source == "HUFF_POST",]
HUFF_MASS <- huff[str_detect(huff$name,"shooting"),]


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
mass_names <- c("Sandy Hook", "Aurora Movie Theater","WA Naval Yard",
                "Lafayette, LA/Chattanooga, TN", "San Bernardino",
                "Orlando", "Dallas Police","Townville Elementary/Cascade Mall",
                "Charleston Church")
mass_date <- as.Date(c("2012-12-01","2012-07-01","2013-09-01",
                       "2015-07-01","2015-12-01","2016-06-01",
                       "2016-07-01","2016-09-01","2015-06-01"))

mass_height <- massShoot[massShoot$timeline == mass_date[1],-5]

for (i in 2:length(mass_date)){
  
  mass_height <- rbind(mass_height, massShoot[massShoot$timeline == mass_date[i],-5])
  
}

mass_height_max <- apply(mass_height, 1, max)

mass_text <- data.frame(mass_names, mass_date, mass_height_max)

############################################

## Make x axis by year (year - month) for event. Do for all graphs

ggplot(massShoot, aes(x=timeline)) + 
  geom_line(aes(y = cnn_occ),color = "red", size = 2) + 
  geom_line(aes(y = huff_occ), color = "green", size = 2) + 
  geom_line(aes(y = fox_occ), color = "blue", size = 2) + 
  geom_line(aes(y = abc_occ), color = "yellow", size = 2) +
  scale_x_date(breaks = as.Date(c("2012-01-01","2012-07-01","2012-12-01",
                                  "2013-01-01","2013-09-01","2014-01-01",
                                  "2015-01-01","2015-06-01","2015-07-01",
                                  "2015-12-01","2016-01-01","2016-06-01",
                                  "2016-07-01","2016-09-01")), 
               date_labels = c("%Y","%Y-%b","%Y-%b","%Y","%Y-%b","%Y","%Y","%Y-%b","%Y-%b",
                               "%Y-%b","%Y","%Y-%b","%Y-%b","%Y-%b")) + 
  geom_text(data = mass_text, aes(label = mass_names,
                                  x = mass_date,
                                  y = mass_height_max + 1.5), inherit.aes = FALSE, 
            angle = 90, hjust = 0, size = 5, family = "Tahoma", fontface = "italic",
            color = "white") +
  expand_limits(y = c(0,90)) + 
  theme(plot.subtitle = element_text(vjust = 1),
        plot.caption = element_text(vjust = 1),
        axis.text.x = element_text(angle = 45)) +
  labs(title = "Number of News Stories With the Phrase 'Shooting(s)' in the Title", 
       y = "# of Stories", x = "Time (Months)") + 
  theme(axis.text.x = element_text(vjust = 0.3))+labs(subtitle = "Mass Shootings Listed") +
  theme(plot.title = element_text(size = 20)) +
  theme(panel.grid.minor = element_line(linetype = "blank")) +
  theme(axis.text = element_text(family = "Tahoma", size = 13)) + 
  theme(axis.title = element_text(size = 16, face = "bold")) + 
  theme(plot.subtitle = element_text(family = "Tahoma"), 
        axis.title = element_text(family = "Tahoma"), 
        axis.text = element_text(family = "Tahoma"), 
        axis.text.x = element_text(family = "Tahoma"), 
        axis.text.y = element_text(family = "Tahoma"), 
        plot.title = element_text(family = "Tahoma")) + 
  theme(plot.subtitle = element_text(family = "Tahoma", 
    colour = "white"), axis.line = element_line(colour = "white"), 
    axis.ticks = element_line(colour = "white"), 
    axis.title = element_text(family = "sans", 
        colour = "white"), axis.text = element_text(family = "Tahoma", 
        colour = "white"), axis.text.x = element_text(colour = "white"), 
    axis.text.y = element_text(colour = "white"), 
    plot.title = element_text(family = "Tahoma", 
        colour = "white"), panel.background = element_rect(fill = "black", 
        linetype = "solid"), plot.background = element_rect(fill = "black", 
        colour = NA, linetype = "solid")) +labs(colour = "white", fill = "white") + 
  theme(panel.grid.major = element_line(colour = "gray60")) + 
  theme(plot.subtitle = element_text(family = "sans", 
    size = 15), axis.text = element_text(family = "sans"), 
    plot.title = element_text(family = "sans")) + 
  theme(axis.text = element_text(hjust = 1))  + 
  theme(axis.ticks = element_line(size = 0.6), 
    axis.text.x = element_text(size = 16)) + 
  theme(axis.text.y = element_text(size = 16)) + 
  theme(axis.text.x = element_text(vjust = 1, 
    angle = 55))

#########################################


g1 <- ggplot(data = massShoot, mapping = aes(x = timeline, y = cnn_occ)) +
  geom_line(color = "red", size = 2, linetype = "solid") + expand_limits(y = c(0,60)) +
  scale_x_date(breaks = timeline, date_labels = "%y-%b") + 
  theme(plot.subtitle = element_text(vjust = 1),
        plot.caption = element_text(vjust = 1), 
        axis.text.x = element_text(vjust = 0.25,angle = 90)) +
  labs(title = "CNN", x = "Time", 
       y = "Frequency of Phrase 'Shooting(s)'") + 
  expand_limits(y=c(0,80)) + theme(panel.grid.minor = element_line(linetype = "blank"))

g2 <- ggplot(data = massShoot, mapping = aes(x = timeline, y = abc_occ)) +
  geom_line(color = "orange", size = 2, linetype = "solid")+ expand_limits(y = c(0,60)) +
  scale_x_date(breaks = timeline, date_labels = "%y-%b")+ 
  theme(plot.subtitle = element_text(vjust = 1),
        plot.caption = element_text(vjust = 1), 
        axis.text.x = element_text(vjust = 0.25,angle = 90)) +
  labs(title = "ABC NEWS", x = "Time", 
       y = "Frequency of Phrase 'Shooting(s)'") + 
  expand_limits(y=c(0,80)) + theme(panel.grid.minor = element_line(linetype = "blank"))

g3 <- ggplot(data = massShoot, mapping = aes(x = timeline, y = fox_occ)) +
  geom_line(color = "blue", size = 2, linetype = "solid")+ expand_limits(y = c(0,60)) +
  scale_x_date(breaks = timeline, date_labels = "%y-%b") + 
  theme(plot.subtitle = element_text(vjust = 1),
        plot.caption = element_text(vjust = 1), 
        axis.text.x = element_text(vjust = 0.25,angle = 90)) +
  labs(title = "FOX NEWS", x = "Time", 
       y = "Frequency of Phrase 'Shooting(s)'") + 
  expand_limits(y=c(0,80)) + theme(panel.grid.minor = element_line(linetype = "blank"))

g4 <- ggplot(data = massShoot, mapping = aes(x = timeline, y = huff_occ)) +
  geom_line(color = "green", size = 2, linetype = "solid")+ expand_limits(y = c(0,60)) +
  scale_x_date(breaks = timeline, date_labels = "%y-%b") + 
  theme(plot.subtitle = element_text(vjust = 1),
        plot.caption = element_text(vjust = 1), 
        axis.text.x = element_text(vjust = 0.25,angle = 90)) +
  labs(title = "HUFFINGTON POST", x = "Time", 
       y = "Frequency of Phrase 'Shooting(s)'") + 
  expand_limits(y=c(0,80)) + theme(panel.grid.minor = element_line(linetype = "blank"))


ggarrange(g1,g2,g3,g4, ncol = 2, nrow = 2)


###################################################################################
####################################################################################
################################################################################

CNN_hurr <- cnn[str_detect(cnn$name,"hurricane"),]

FOX_hurr <- fox[str_detect(fox$name,"hurricane"),]

ABC_hurr <- abc[str_detect(abc$name,"hurricane"),]

HUFF_hurr <- huff[str_detect(huff$name,"hurricane"),]


months <- month.name

timeline <- seq(as.Date("2012/01/01"), 
                as.Date("2016/11/01"),
                "month")


cnn_occ_hurr <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    cnn_occ_hurr[k] <- nrow(subset(CNN_hurr, (year == years[i] & month == months[j]), 
                              select = name))
    k = k + 1
  }
}




fox_occ_hurr <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    fox_occ_hurr[k] <- nrow(subset(FOX_hurr, (year == years[i] & month == months[j]), 
                              select = name))
    k = k + 1
  }
}




abc_occ_hurr <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    abc_occ_hurr[k] <- nrow(subset(ABC_hurr, (year == years[i] & month == months[j]), 
                              select = name))
    k = k + 1
  }
}




huff_occ_hurr <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    huff_occ_hurr[k] <- nrow(subset(HUFF_hurr, (year == years[i] & month == months[j]), 
                               select = name))
    k = k + 1
  }
}




abc_occ_hurr <- abc_occ_hurr[-60]
cnn_occ_hurr <- cnn_occ_hurr[-60]
huff_occ_hurr <- huff_occ_hurr[-60]
fox_occ_hurr <-  fox_occ_hurr[-60]

hurr <- data.frame(abc_occ_hurr,cnn_occ_hurr,huff_occ_hurr,fox_occ_hurr, timeline)


hurr_names <- c("Isaac (Cat. 1 - $3.11B)", "Sandy (Cat. 3 - $68.7B)",
                "Danny & Fred (Cat. 3 & 1 - $0 & $2.5M)",
                "Patricia (Cat. 5 - $462.8M)", 
                "Earl, Gaston, Hermine (Cat. 1,3,1 - $132M,$0,$550M)",
                "Matthew (Cat. 5 - $16.47B)")
hurr_date <- as.Date(c("2012-08-01","2012-10-01","2015-08-01","2015-10-01",
                       "2016-08-01","2016-10-01"))

hurr_height <- hurr[hurr$timeline == hurr_date[1],-5]

for (i in 2:length(hurr_date)){
  
  hurr_height <- rbind(hurr_height, hurr[hurr$timeline == hurr_date[i],-5])
  
}

hurr_height_max <- apply(hurr_height, 1, max)

hurr_text <- data.frame(hurr_names, hurr_date, hurr_height_max)


hurr_season <- data.frame(as.Date(c("2012/05/01","2013/05/01",
                                    "2014/05/01","2015/05/01",
                                    "2016/05/01")),
                                  as.Date(c("2012/12/01","2013/12/01",
                                            "2014/12/01","2015/12/01",
                                            "2016/11/01")))
colnames(hurr_season) <- c("start","end")




ggplot(data = hurr, mapping = aes(x = timeline, y = cnn_occ_hurr, color = "red")) +
  geom_rect(data = hurr_season, aes(xmin = start, xmax = end, fill = "orange"),
            ymin = -Inf, ymax = Inf, inherit.aes = FALSE, alpha = 0.3) +
  geom_line(size = 2, linetype = "solid") +
  geom_line(data = hurr, aes(x = timeline, y = huff_occ_hurr, color = "green"),
            inherit.aes = F, size = 2, linetype = "solid") +
  geom_line(data = hurr, aes(x = timeline, y = fox_occ_hurr, color = "blue"), 
            inherit.aes = F, size = 2, linetype = "solid") +
  geom_line(data = hurr, aes(x = timeline, y = abc_occ_hurr, color = "orange"), 
            inherit.aes = F, size = 2, linetype = "solid") +
  scale_x_date(breaks = timeline, date_labels = "%Y-%b") +
  theme(legend.position = "left") +
  scale_color_manual(name = "News Source", guide = 'legend',
                     labels = c("ABC NEWS","FOX NEWS","HUFF POST","CNN"),
                     values = c("orange","blue","green","red")) +
  scale_fill_identity(guide = 'legend', labels = "Hurricane Season") +
  geom_text(data = hurr_text, aes(label = hurr_names, x = hurr_date,
                                  y = hurr_height_max + 1.5),
            inherit.aes = FALSE, 
            angle = 90, hjust = 0, size = 4, family = "Tahoma", fontface = "bold.italic") +
  theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1), 
    axis.text.x = element_text(vjust = 0.3, 
        angle = 90)) + 
  expand_limits(y = c(0,145)) +
  theme(plot.title = element_text(size = 15)) +
  labs(title = "Frequency of the Phrase Hurricane(s)", 
    x = "Time", y = "Frequency") + theme(legend.title = element_text(size = 12), 
    legend.position = c(0.05, 0.54), legend.background = element_rect(fill = "gray88")) + 
  theme(plot.subtitle = element_text(size = 10)) +
  labs(subtitle = "Hurricane Name, Category, and Damage Listed") + 
  theme(panel.grid.minor = element_line(linetype = "blank")) +
  theme(axis.text = element_text(family = "Tahoma", 
    size = 13)) + theme(axis.title = element_text(size = 13, 
    face = "bold"))



###################################################################
###################################################################
###################################################################

CNN_tor <- cnn[str_detect(cnn$name,"tornado"),]

FOX_tor <- fox[str_detect(fox$name,"tornado"),]

ABC_tor <- abc[str_detect(abc$name,"tornado"),]

HUFF_tor <- huff[str_detect(huff$name,"tornado"),]


months <- month.name

timeline <- seq(as.Date("2012/01/01"), 
                as.Date("2016/11/01"),
                "month")


cnn_occ_tor <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    cnn_occ_tor[k] <- nrow(subset(CNN_tor, (year == years[i] & month == months[j]), 
                                   select = name))
    k = k + 1
  }
}




fox_occ_tor <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    fox_occ_tor[k] <- nrow(subset(FOX_tor, (year == years[i] & month == months[j]), 
                                   select = name))
    k = k + 1
  }
}




abc_occ_tor <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    abc_occ_tor[k] <- nrow(subset(ABC_tor, (year == years[i] & month == months[j]), 
                                   select = name))
    k = k + 1
  }
}




huff_occ_tor <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    huff_occ_tor[k] <- nrow(subset(HUFF_tor, (year == years[i] & month == months[j]), 
                                    select = name))
    k = k + 1
  }
}




abc_occ_tor <- abc_occ_tor[-60]
cnn_occ_tor <- cnn_occ_tor[-60]
huff_occ_tor <- huff_occ_tor[-60]
fox_occ_tor <-  fox_occ_tor[-60]

torn <- data.frame(abc_occ_tor,cnn_occ_tor,huff_occ_tor,fox_occ_tor, timeline)



torn_names <- c("150 Tornadoes, Max: EF4", "53 Tornadoes, Max: EF3",
                "227 Tornadoes, Max: EF5", "77 Tornadoes, Max: EF4",
                "128 Tornadoes, Max: EF4", "382 Tornadoes, Max: EF3",
                "83 Tornadoes, Max: EF4", "99 Tornadoes, Max: EF3",
                "84 Tornadoes, Max: EF2", "142 Tornadoes, Max: EF2",
                "218 Tornadoes, Max: EF4", "90 Tornadoes, Max: EF3")

torn_date <- as.Date(c("2012-03-01","2012-12-01","2013-05-01","2013-11-01",
                       "2014-04-01","2015-05-01","2015-12-01","2016-02-01",
                       "2016-03-01","2016-04-01","2016-05-01","2016-08-01"))

torn_height <- torn[torn$timeline == torn_date[1],-5]

for (i in 2:length(torn_date)){
  
  torn_height <- rbind(torn_height, torn[torn$timeline == torn_date[i],-5])
  
}

torn_height_max <- apply(torn_height, 1, max)

torn_text <- data.frame(torn_names, torn_date, torn_height_max)








ggplot(data = torn, mapping = aes(x = timeline, y = cnn_occ_tor, color = "red")) +
  geom_line(size = 2, linetype = "solid") +
  geom_line(data = torn, aes(x = timeline, y = huff_occ_tor, color = "green"),
            inherit.aes = F, size = 2, linetype = "solid") +
  geom_line(data = torn, aes(x = timeline, y = fox_occ_tor, color = "blue"), 
            inherit.aes = F, size = 2, linetype = "solid") +
  geom_line(data = torn, aes(x = timeline, y = abc_occ_tor, color = "orange"), 
            inherit.aes = F, size = 2, linetype = "solid") +
  scale_x_date(breaks = timeline, date_labels = "%Y-%b") +
  theme(legend.position = "left") +
  scale_color_manual(name = "News Source", guide = 'legend',
                     labels = c("ABC NEWS","FOX NEWS","HUFF POST","CNN"),
                     values = c("orange","blue","green","red")) +
  geom_text(data = torn_text, aes(label = torn_names, x = torn_date, 
                                  y = torn_height_max + 1.5),
            inherit.aes = FALSE, 
            angle = 90, hjust = 0, size = 4, family = "Tahoma", fontface = "bold.italic") +
  expand_limits(y = c(0,90)) +
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        axis.text.x = element_text(vjust = 0.3, 
                                   angle = 90)) +
  theme(legend.title = element_text(size = 12), 
        legend.position = c(0.07, 0.56), legend.background = element_rect(fill = "gray88")) +
  theme(plot.subtitle = element_text(size = 10), 
    plot.title = element_text(size = 14)) +
  labs(title = "Frequency of the Phrase 'Tornadoe(s)'", 
    x = "Time", y = "Frequency", subtitle = "Number of Tornadoes & Max Rating Listed") + 
  theme(panel.grid.minor = element_line(linetype = "blank")) +
  theme(axis.text = element_text(family = "Tahoma", 
                                 size = 13)) + 
  theme(axis.title = element_text(size = 13, face = "bold"))
  





###############################################################################
###############################################################################
###############################################################################

CNN_earth <- cnn[str_detect(cnn$name,"earthquake"),]

FOX_earth <- fox[str_detect(fox$name,"earthquake"),]

ABC_earth <- abc[str_detect(abc$name,"earthquake"),]

HUFF_earth <- huff[str_detect(huff$name,"earthquake"),]



months <- month.name

timeline <- seq(as.Date("2012/01/01"), 
                as.Date("2016/11/01"),
                "month")


cnn_occ_earth <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    cnn_occ_earth[k] <- nrow(subset(CNN_earth, (year == years[i] & month == months[j]), 
                                  select = name))
    k = k + 1
  }
}




fox_occ_earth <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    fox_occ_earth[k] <- nrow(subset(FOX_earth, (year == years[i] & month == months[j]), 
                                  select = name))
    k = k + 1
  }
}




abc_occ_earth <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    abc_occ_earth[k] <- nrow(subset(ABC_earth, (year == years[i] & month == months[j]), 
                                  select = name))
    k = k + 1
  }
}




huff_occ_earth <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    huff_occ_earth[k] <- nrow(subset(HUFF_earth, (year == years[i] & month == months[j]), 
                                   select = name))
    k = k + 1
  }
}




abc_occ_earth <- abc_occ_earth[-60]
cnn_occ_earth <- cnn_occ_earth[-60]
huff_occ_earth <- huff_occ_earth[-60]
fox_occ_earth <-  fox_occ_earth[-60]

earth <- data.frame(abc_occ_earth,cnn_occ_earth,huff_occ_earth,fox_occ_earth, timeline)

earth_names <- c("13 Earthquakes, Mag. 7.4","13 Earthquakes, Mag. 7.7",
                 "9 Earthquakes, Mag. 7.6", "14 Earthquakes, Mag. 7.8",
                 "11 Earthquakes, Mag. 7.3", "22 Earthquakes, Mag. 8.0",
                 "7 Earthquakes, Mag. 6.5", "16 Earthquakes, Mag. 7.7",
                 "12 Earthquakes, Mag. 7.3", "14 Earthquakes, Mag. 7.7",
                 "16 Earthquakes, Mag. 6.8", "11 Earthquakes, Mag. 7.1",
                 "15 Earthquakes, Mag. 7.8", "21 Earthquakes, Mag. 7.8",
                 "24 Earthquakes, Mag. 8.3", "10 Earthquakes, Mag. 6.4",
                 "22 Earthquakes, Mag. 7.8", "13 Earthquakes, Mag. 7.4",
                 "8 Earthquakes, Mag. 6.8")

earth_date <- as.Date(c("2012-03-01","2012-08-01","2012-09-01","2012-10-01",
                        "2012-12-01","2013-02-01","2013-03-01","2013-04-01",
                        "2013-07-01","2013-09-01","2014-03-01","2014-11-01",
                        "2015-04-01","2015-05-01","2015-09-01","2016-02-01",
                        "2016-04-01","2016-08-01","2016-10-01"))

earth_height <- earth[earth$timeline == earth_date[1],-5]

for (i in 2:length(earth_date)){
  
  earth_height <- rbind(earth_height, earth[earth$timeline == earth_date[i],-5])
  
}

earth_height_max <- apply(earth_height, 1, max)

earth_text <- data.frame(earth_names, earth_date, earth_height_max)



ggplot(data = earth, mapping = aes(x = timeline, y = cnn_occ_earth, color = "red")) +
  geom_line(size = 2, linetype = "solid") +
  geom_line(data = earth, aes(x = timeline, y = huff_occ_earth, color = "green"),
            inherit.aes = F, size = 2, linetype = "solid") +
  geom_line(data = earth, aes(x = timeline, y = fox_occ_earth, color = "blue"), 
            inherit.aes = F, size = 2, linetype = "solid") +
  geom_line(data = earth, aes(x = timeline, y = abc_occ_earth, color = "orange"), 
            inherit.aes = F, size = 2, linetype = "solid") +
  scale_x_date(breaks = timeline, date_labels = "%Y-%b") +
  theme(legend.position = "left") +
  scale_color_manual(name = "News Source", guide = 'legend',
                     labels = c("ABC NEWS","FOX NEWS","HUFF POST","CNN"),
                     values = c("orange","blue","green","red")) +
  geom_text(data = earth_text, aes(label = earth_names, x = earth_date,
                                   y = earth_height_max + 1.5),
            inherit.aes = FALSE, 
            angle = 90, hjust = 0, size = 4, family = "Tahoma", fontface = "bold.italic") +
  expand_limits(y = c(0,38)) +
  theme(legend.title = element_text(size = 12), 
        legend.position = c(0.07, 0.66), legend.background = element_rect(fill = "gray88")) +
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        axis.text.x = element_text(vjust = 0.3, 
                                   angle = 90)) + 
  theme(plot.subtitle = element_text(size = 10), 
    plot.title = element_text(size = 14)) +
  labs(title = "Frequency of the Phrase Earthquake(s)", 
    x = "Time", y = "Frequency", 
    subtitle = "Number of Major Earthquakes (6.0+ Moment Magnitude Scale) and Largest Magnitude Listed")


#################################################################################
#################################################################################
#################################################################################

CNN_climate1 <- cnn[str_detect(cnn$name,"climate change"),]
CNN_climate2 <- cnn[str_detect(cnn$name,"global warming"),]
CNN_climate <- rbind(CNN_climate1, CNN_climate2)

FOX_climate1 <- fox[str_detect(fox$name,"climate change"),]
FOX_climate2 <- fox[str_detect(fox$name,"global warming"),]
FOX_climate <- rbind(FOX_climate1, FOX_climate2)

ABC_climate1 <- abc[str_detect(abc$name,"climate change"),]
ABC_climate2 <- abc[str_detect(abc$name,"global warming"),]
ABC_climate <- rbind(ABC_climate1, ABC_climate2)

HUFF_climate1 <- huff[str_detect(huff$name,"climate change"),]
HUFF_climate2 <- huff[str_detect(huff$name,"global warming"),]
HUFF_climate <- rbind(HUFF_climate1, HUFF_climate2)


months <- month.name

timeline <- seq(as.Date("2012/01/01"), 
                as.Date("2016/11/01"),
                "month")


cnn_occ_climate <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    cnn_occ_climate[k] <- nrow(subset(CNN_climate, (year == years[i] & month == months[j]), 
                                    select = name))
    k = k + 1
  }
}




fox_occ_climate <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    fox_occ_climate[k] <- nrow(subset(FOX_climate, (year == years[i] & month == months[j]), 
                                    select = name))
    k = k + 1
  }
}




abc_occ_climate <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    abc_occ_climate[k] <- nrow(subset(ABC_climate, (year == years[i] & month == months[j]), 
                                    select = name))
    k = k + 1
  }
}




huff_occ_climate <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    huff_occ_climate[k] <- nrow(subset(HUFF_climate, (year == years[i] & month == months[j]), 
                                     select = name))
    k = k + 1
  }
}




abc_occ_climate <- abc_occ_climate[-60]
cnn_occ_climate <- cnn_occ_climate[-60]
huff_occ_climate <- huff_occ_climate[-60]
fox_occ_climate <-  fox_occ_climate[-60]

climate <- data.frame(abc_occ_climate,cnn_occ_climate,huff_occ_climate,fox_occ_climate, timeline)

climate_names <- c("UN Climate Change Conference","UN Climate Change Conference",
                   "Paris Agreement")

climate_date <- as.Date(c("2012-11-01","2015-11-01","2015-12-01"))

climate_height <- climate[climate$timeline == climate_date[1],-5]

for (i in 2:length(climate_date)){
  
  climate_height <- rbind(climate_height, climate[climate$timeline == climate_date[i],-5])
  
}

climate_height_max <- apply(climate_height, 1, max)

climate_text <- data.frame(climate_names, climate_date, climate_height_max)







ggplot(data = climate, mapping = aes(x = timeline, y = cnn_occ_climate, color = "red")) +
  geom_line(size = 2, linetype = "solid") +
  geom_line(data = climate, aes(x = timeline, y = huff_occ_climate, color = "green"),
            inherit.aes = F, size = 2, linetype = "solid") +
  geom_line(data = climate, aes(x = timeline, y = fox_occ_climate, color = "blue"), 
            inherit.aes = F, size = 2, linetype = "solid") +
  geom_line(data = climate, aes(x = timeline, y = abc_occ_climate, color = "orange"), 
            inherit.aes = F, size = 2, linetype = "solid") +
  scale_x_date(breaks = timeline, date_labels = "%Y-%b") +
  theme(legend.position = "left") +
  scale_color_manual(name = "News Source", guide = 'legend',
                     labels = c("ABC NEWS","FOX NEWS","HUFF POST","CNN"),
                     values = c("orange","blue","green","red")) +
  geom_text(data = climate_text, aes(label = climate_names, x = climate_date,
                                   y = climate_height_max + 0.5),
            inherit.aes = FALSE, 
            angle = 0, hjust = 1, size = 4, family = "Tahoma", fontface = "bold.italic") +
  theme(legend.title = element_text(size = 12), 
        legend.position = c(0.07, 0.66), 
        legend.background = element_rect(fill = "gray88")) +
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        axis.text.x = element_text(vjust = 0.3, 
                                   angle = 90)) + 
  theme(plot.subtitle = element_text(size = 10), 
    plot.title = element_text(size = 14)) +
  labs(title = "Frequency of the Phrases 'Climate Change' & 'Global Warming'", 
    x = "Time", y = "Frequency", subtitle = "Political Climate Change Events Listed") +
  theme(panel.grid.minor = element_line(linetype = "blank")) +
  theme(axis.text = element_text(family = "Tahoma", 
                                 size = 13)) + theme(axis.title = element_text(size = 13, 
                                                                               face = "bold"))


###############################################################################
###############################################################################
###############################################################################

CNN_flood <- cnn[str_detect(cnn$name,"flood"),]

FOX_flood <- fox[str_detect(fox$name,"flood"),]

ABC_flood <- abc[str_detect(abc$name,"flood"),]

HUFF_flood <- huff[str_detect(huff$name,"flood"),]



months <- month.name

timeline <- seq(as.Date("2012/01/01"), 
                as.Date("2016/11/01"),
                "month")


cnn_occ_flood <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    cnn_occ_flood[k] <- nrow(subset(CNN_flood, (year == years[i] & month == months[j]), 
                                      select = name))
    k = k + 1
  }
}




fox_occ_flood <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    fox_occ_flood[k] <- nrow(subset(FOX_flood, (year == years[i] & month == months[j]), 
                                      select = name))
    k = k + 1
  }
}




abc_occ_flood <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    abc_occ_flood[k] <- nrow(subset(ABC_flood, (year == years[i] & month == months[j]), 
                                      select = name))
    k = k + 1
  }
}




huff_occ_flood <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    huff_occ_flood[k] <- nrow(subset(HUFF_flood, (year == years[i] & month == months[j]), 
                                       select = name))
    k = k + 1
  }
}




abc_occ_flood <- abc_occ_flood[-60]
cnn_occ_flood <- cnn_occ_flood[-60]
huff_occ_flood <- huff_occ_flood[-60]
fox_occ_flood <-  fox_occ_flood[-60]

flood <- data.frame(abc_occ_flood,cnn_occ_flood,huff_occ_flood,fox_occ_flood, timeline)



flood_names <- c("Midwest Floods","Colorado Floods", "Southeastern Michigan Flood",
                 "Texas-Oklahoma Flood","Louisiana Flood","South Carolina Flood",
                 "Missouri Floods","Southern & Midwestern Floods", "Houston Floods",
                 "Oklahoma Floods","West Virginia Floods","Louisiana Floods",
                 "South Carolina Floods")

flood_date <- as.Date(c("2013-04-01","2013-09-01","2014-08-01","2015-05-01",
                        "2015-06-01","2015-10-01","2015-12-01","2016-03-01",
                        "2016-04-01","2016-05-01","2016-06-01","2016-08-01",
                        "2016-10-01"))

flood_height <- flood[flood$timeline == flood_date[1],-5]

for (i in 2:length(flood_date)){
  
  flood_height <- rbind(flood_height, flood[flood$timeline == flood_date[i],-5])
  
}

flood_height_max <- apply(flood_height, 1, max)

flood_text <- data.frame(flood_names, flood_date, flood_height_max)



ggplot(data = flood, mapping = aes(x = timeline, y = cnn_occ_flood, color = "red")) +
  geom_line(size = 2, linetype = "solid") +
  geom_line(data = flood, aes(x = timeline, y = huff_occ_flood, color = "green"),
            inherit.aes = F, size = 2, linetype = "solid") +
  geom_line(data = flood, aes(x = timeline, y = fox_occ_flood, color = "blue"), 
            inherit.aes = F, size = 2, linetype = "solid") +
  geom_line(data = flood, aes(x = timeline, y = abc_occ_flood, color = "orange"), 
            inherit.aes = F, size = 2, linetype = "solid") +
  scale_x_date(breaks = timeline, date_labels = "%Y-%b") +
  theme(legend.position = "left") +
  scale_color_manual(name = "News Source", guide = 'legend',
                     labels = c("ABC NEWS","FOX NEWS","HUFF POST","CNN"),
                     values = c("orange","blue","green","red")) +
  geom_text(data = flood_text, aes(label = flood_names, x = flood_date,
                                   y = flood_height_max + 1.6),inherit.aes = FALSE, 
            angle = 90, hjust = 0, vjust = -0.25, size = 4, family = "Tahoma", 
            fontface = "bold.italic") +
  expand_limits(y = c(0,40)) +
  theme(legend.title = element_text(size = 12), 
        legend.position = c(0.07, 0.66), 
        legend.background = element_rect(fill = "gray88")) +
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        axis.text.x = element_text(vjust = 0.3, 
                                   angle = 90)) + 
  theme(plot.subtitle = element_text(size = 10), 
    plot.title = element_text(size = 14)) +
  labs(title = "Frequencsy of the Phrase 'Flood(s)'", 
    x = "Time", y = "Frequency", subtitle = "Major Flooding Events Listed") +
  theme(panel.grid.minor = element_line(linetype = "blank")) +
  theme(axis.text = element_text(family = "Tahoma", 
                                 size = 13)) + theme(axis.title = element_text(size = 13, 
                                                                               face = "bold"))




#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################


#### LIKES

wordLikesMedian <- as.data.frame(matrix(NA, nrow = 4, ncol = 5))
newsNames <- c("ABC News","CNN","Fox News","Huffington Post")
levels(FB$Source)


wordLikesMedian[,1] <- newsNames
colnames(wordLikesMedian) <- c("Source","Hurricane","Tornado","Flood","Earthquake")

topics <- c("Hurricane","Tornado","Flood","Earthquake")



### For each news source
for (i in 1:length(levels(FB$Source))){
  
  mySubset <- FB[FB$Source == levels(FB$Source)[i],c(3,9)]
  
  ### For each word
  for (j in 2:(length(topics)+1)){
    
    topicSubset <- mySubset[str_detect(mySubset[,1], tolower(topics[j-1])),]
    
    wordLikesMedian[i,j] <- median(topicSubset[,2])
    
  }
}




p1 <- ggplot(data = melt(wordLikesMedian, id.vars = 'Source'), 
             aes(x = Source, y = value)) + 
  geom_bar(aes(fill = variable),
           position = position_dodge(width = 0.9), stat = "identity") + 
  scale_fill_manual(values = c("#DB1111","#D89402","#16A80F","#0537FF","#FF05E6")) + 
  theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1), 
    plot.title = element_text(size = 16)) +
  labs(title = "Likes", x = NULL, y = "Median Value", fill = "Phrase") 




#### LOVE

wordLoveMedian <- as.data.frame(matrix(NA, nrow = 4, ncol = 5))

wordLoveMedian[,1] <- newsNames
colnames(wordLoveMedian) <- c("Source","Hurricane","Tornado","Flood","Earthquake")


for (i in 1:length(levels(FB$Source))){
  
  mySubset <- FB[FB$Source == levels(FB$Source)[i] & FB$year == "2016",c(3,12)]
  
  ### For each word
  for (j in 2:(length(topics)+1)){
    
    topicSubset <- mySubset[str_detect(mySubset[,1], tolower(topics[j-1])),]
    
    wordLoveMedian[i,j] <- median(topicSubset[,2])
    
  }
}

p2 <- ggplot(data = melt(wordLoveMedian, id.vars = 'Source'), 
             aes(x = Source, y = value)) + 
  geom_bar(aes(fill = variable),
           position = position_dodge(width = 0.9), stat = "identity") + 
  scale_fill_manual(values = c("#DB1111","#D89402","#16A80F","#0537FF","#FF05E6")) + 
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        plot.title = element_text(size = 16)) +
  labs(title = "Loves", x = NULL, y = "Median Value", fill = "Phrase")





#### WOW

wordWowMedian <- as.data.frame(matrix(NA, nrow = 4, ncol = 5))

wordWowMedian[,1] <- newsNames
colnames(wordWowMedian) <- c("Source","Hurricane","Tornado","Flood","Earthquake")


for (i in 1:length(levels(FB$Source))){
  
  mySubset <- FB[FB$Source == levels(FB$Source)[i] & FB$year == "2016",c(3,13)]
  
  ### For each word
  for (j in 2:(length(topics)+1)){
    
    topicSubset <- mySubset[str_detect(mySubset[,1], tolower(topics[j-1])),]
    
    wordWowMedian[i,j] <- median(topicSubset[,2])
    
  }
}

p3 <- ggplot(data = melt(wordWowMedian, id.vars = 'Source'), 
             aes(x = Source, y = value)) + 
  geom_bar(aes(fill = variable),
           position = position_dodge(width = 0.9), stat = "identity") + 
  scale_fill_manual(values = c("#DB1111","#D89402","#16A80F","#0537FF","#FF05E6")) + 
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        plot.title = element_text(size = 16)) +
  labs(title = "Wows", x = NULL, y = "Median Value", fill = "Phrase")


#### HAHA

wordHahaMedian <- as.data.frame(matrix(NA, nrow = 4, ncol = 5))

wordHahaMedian[,1] <- newsNames
colnames(wordHahaMedian) <- c("Source","Hurricane","Tornado","Flood","Earthquake")


for (i in 1:length(levels(FB$Source))){
  
  mySubset <- FB[FB$Source == levels(FB$Source)[i] & FB$year == "2016",c(3,14)]
  
  ### For each word
  for (j in 2:(length(topics)+1)){
    
    topicSubset <- mySubset[str_detect(mySubset[,1], tolower(topics[j-1])),]
    
    wordHahaMedian[i,j] <- median(topicSubset[,2])
    
  }
}

p4 <- ggplot(data = melt(wordHahaMedian, id.vars = 'Source'), 
             aes(x = Source, y = value)) + 
  geom_bar(aes(fill = variable),
           position = position_dodge(width = 0.9), stat = "identity") + 
  scale_fill_manual(values = c("#DB1111","#D89402","#16A80F","#0537FF","#FF05E6")) + 
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        plot.title = element_text(size = 16)) +
  labs(title = "Hahas", x = NULL, y = "Median Value", fill = "Phrase")


#### SAD


wordSadMedian <- as.data.frame(matrix(NA, nrow = 4, ncol = 5))

wordSadMedian[,1] <- newsNames
colnames(wordSadMedian) <- c("Source","Hurricane","Tornado","Flood","Earthquake")


for (i in 1:length(levels(FB$Source))){
  
  mySubset <- FB[FB$Source == levels(FB$Source)[i] & FB$year == "2016",c(3,13)]
  
  ### For each word
  for (j in 2:(length(topics)+1)){
    
    topicSubset <- mySubset[str_detect(mySubset[,1], tolower(topics[j-1])),]
    
    wordSadMedian[i,j] <- median(topicSubset[,2])
    
  }
}

p5 <- ggplot(data = melt(wordSadMedian, id.vars = 'Source'), 
             aes(x = Source, y = value)) + 
  geom_bar(aes(fill = variable),
           position = position_dodge(width = 0.9), stat = "identity") + 
  scale_fill_manual(values = c("#DB1111","#D89402","#16A80F","#0537FF","#FF05E6")) + 
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        plot.title = element_text(size = 16)) +
  labs(title = "Sads", x = NULL, y = "Median Value", fill = "Phrase")


#### ANGRY


wordAngryMedian <- as.data.frame(matrix(NA, nrow = 4, ncol = 5))

wordAngryMedian[,1] <- newsNames
colnames(wordAngryMedian) <- c("Source","Hurricane","Tornado","Flood","Earthquake")


for (i in 1:length(levels(FB$Source))){
  
  mySubset <- FB[FB$Source == levels(FB$Source)[i] & FB$year == "2016",c(3,17)]
  
  ### For each word
  for (j in 2:(length(topics)+1)){
    
    topicSubset <- mySubset[str_detect(mySubset[,1], tolower(topics[j-1])),]
    
    wordAngryMedian[i,j] <- median(topicSubset[,2])
    
  }
}

p6 <- ggplot(data = melt(wordAngryMedian, id.vars = 'Source'), 
             aes(x = Source, y = value)) + 
  geom_bar(aes(fill = variable),
           position = position_dodge(width = 0.9), stat = "identity") + 
  scale_fill_manual(values = c("#DB1111","#D89402","#16A80F","#0537FF","#FF05E6")) + 
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        plot.title = element_text(size = 16)) +
  labs(title = "Angrys", x = NULL, y = "Median Value", fill = "Phrase")



#### COMMENTS

wordCommentMedian <- as.data.frame(matrix(NA, nrow = 4, ncol = 5))
newsNames <- c("ABC News","CNN","Fox News","Huffington Post")
levels(FB$Source)


wordCommentMedian[,1] <- newsNames
colnames(wordCommentMedian) <- c("Source","Hurricane","Tornado","Flood","Earthquake")



### For each news source
for (i in 1:length(levels(FB$Source))){
  
  mySubset <- FB[FB$Source == levels(FB$Source)[i],c(3,10)]
  
  ### For each word
  for (j in 2:(length(topics)+1)){
    
    topicSubset <- mySubset[str_detect(mySubset[,1], tolower(topics[j-1])),]
    
    wordCommentMedian[i,j] <- median(topicSubset[,2])
    
  }
}

p7 <- ggplot(data = melt(wordCommentMedian, id.vars = 'Source'), 
             aes(x = Source, y = value)) + 
  geom_bar(aes(fill = variable),
           position = position_dodge(width = 0.9), stat = "identity") + 
  scale_fill_manual(values = c("#DB1111","#D89402","#16A80F","#0537FF","#FF05E6")) + 
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        plot.title = element_text(size = 16)) +
  labs(title = "Comments", x = NULL, y = "Median Value", fill = "Phrase")






#### SHARES

wordShareMedian <- as.data.frame(matrix(NA, nrow = 4, ncol = 5))
newsNames <- c("ABC News","CNN","Fox News","Huffington Post")
levels(FB$Source)


wordShareMedian[,1] <- newsNames
colnames(wordShareMedian) <- c("Source","Hurricane","Tornado","Flood","Earthquake")





### For each news source
for (i in 1:length(levels(FB$Source))){
  
  mySubset <- FB[FB$Source == levels(FB$Source)[i],c(3,11)]
  
  ### For each word
  for (j in 2:(length(topics)+1)){
    
    topicSubset <- mySubset[str_detect(mySubset[,1], tolower(topics[j-1])),]
    
    wordShareMedian[i,j] <- median(topicSubset[,2])
    
  }
}

p8 <- ggplot(data = melt(wordShareMedian, id.vars = 'Source'), 
             aes(x = Source, y = value)) + 
  geom_bar(aes(fill = variable),
           position = position_dodge(width = 0.9), stat = "identity") + 
  scale_fill_manual(values = c("#DB1111","#D89402","#16A80F","#0537FF","#FF05E6")) + 
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        plot.title = element_text(size = 16)) +
  labs(title = "Shares", x = NULL, y = "Median Value", fill = "Phrase")




ggarrange(p1,p2,p3,p4,p5,p6,p7,p8, ncol = 2, nrow = 4)



################################################################################
################################################################################
###################################Amounts#############################################

years <- c(2012:2016)
months <- month.name

timeline <- seq(as.Date("2012/01/01"), 
                as.Date("2016/11/01"),
                "month")



cnn_nStory <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    
    if(nrow(subset(cnn, (year == years[i] & month == months[j]), 
                   select = name)) == 0){
      
      cnn_nStory[k] <- 0
      
      k = k + 1
      
    }
    else{
      cnn_nStory[k] <- nrow(subset(cnn, (year == years[i] & month == months[j]), 
                                  select = name))
      k = k + 1
    }
    
  }
}
cnn_nStory <- cnn_nStory[-60]



fox_nStory <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    
    if(nrow(subset(fox, (year == years[i] & month == months[j]), 
                   select = name)) == 0){
      
      fox_nStory[k] <- 0
      
      k = k + 1
      
    }
    else{
      fox_nStory[k] <- nrow(subset(fox, (year == years[i] & month == months[j]), 
                                  select = name))
      k = k + 1
    }
    
  }
}
fox_nStory <- fox_nStory[-60]




abc_nStory <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    
    if(nrow(subset(abc, (year == years[i] & month == months[j]), 
                   select = name)) == 0){
      
      abc_nStory[k] <- 0
      
      k = k + 1
      
    }
    else{
      abc_nStory[k] <- nrow(subset(abc, (year == years[i] & month == months[j]), 
                                  select = name))
      k = k + 1
    }
    
  }
}
abc_nStory <- abc_nStory[-60]




huff_nStory <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    
    if(nrow(subset(huff, (year == years[i] & month == months[j]), 
                   select = name)) == 0){
      
      huff_nStory[k] <- 0
      
      k = k + 1
      
    }
    else{
      huff_nStory[k] <- nrow(subset(huff, (year == years[i] & month == months[j]), 
                                   select = name))
      k = k + 1
    }
    
  }
}
huff_nStory <- huff_nStory[-60]


nStory <- data.frame(cnn_nStory,fox_nStory,huff_nStory,abc_nStory)


ggplot(data = nStory, mapping = aes(x = timeline)) +
  geom_line(aes(y = cnn_nStory), color = "red", size = 2) +
  geom_line(aes(y = fox_nStory), color = "blue", size = 2) +
  geom_line(aes(y = huff_nStory), color = "green", size = 2) +
  geom_line(aes(y = abc_nStory), color = "yellow", size = 2) +
  scale_x_date(breaks =  as.Date(c("2012-01-01",
                                   "2013-01-01",
                                   "2014-01-01",
                                   "2015-01-01",
                                   "2016-01-01")), date_labels = "%Y") +
  scale_y_continuous(labels = comma) +
  labs(y = "# of Stories", x = "Time (Months)") + 
  theme(axis.text.x = element_text(vjust = 0.3)) +
  theme(plot.title = element_text(size = 20)) +
  theme(panel.grid.minor = element_line(linetype = "blank")) +
  theme(axis.text = element_text(family = "Tahoma", size = 13)) + 
  theme(axis.title = element_text(size = 16, face = "bold")) + 
  theme(axis.title = element_text(family = "Tahoma"), 
        axis.text = element_text(family = "Tahoma"), 
        axis.text.x = element_text(family = "Tahoma"), 
        axis.text.y = element_text(family = "Tahoma"), 
        plot.title = element_text(family = "Tahoma")) + 
  theme(axis.line = element_line(colour = "white"), 
        axis.ticks = element_line(colour = "white"), 
        axis.title = element_text(family = "sans",colour = "white"), 
        axis.text = element_text(family = "Tahoma", colour = "white"), 
        axis.text.x = element_text(colour = "white"), 
        axis.text.y = element_text(colour = "white"), 
        plot.title = element_text(family = "Tahoma", colour = "white"),
        panel.background = element_rect(fill = "black",linetype = "solid"), 
        plot.background = element_rect(fill = "black", colour = NA, linetype = "solid")) +
  labs(colour = "white", fill = "white") + 
  theme(panel.grid.major = element_line(colour = "gray60")) + 
  theme(plot.subtitle = element_text(family = "sans",size = 15), 
        axis.text = element_text(family = "sans"), 
        plot.title = element_text(family = "sans")) + 
  theme(axis.text = element_text(hjust = 0.5)) +
  labs(title = "Number of News Stories Over Time") +
  theme(axis.ticks = element_line(size = 0.6), 
        axis.text.x = element_text(size = 16))
  



#########################################################################

cnn_nLikes <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    
    if(nrow(subset(cnn, (year == years[i] & month == months[j]), 
              select = likes_count)) == 0){

      cnn_nLikes[k] <- 0
      
      k = k + 1
      
    }
    else{
      cnn_nLikes[k] <- sum(subset(cnn, (year == years[i] & month == months[j]), 
                                  select = likes_count))
      k = k + 1
    }
    
  }
}
cnn_nLikes <- cnn_nLikes[-60]



fox_nLikes <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    
    if(nrow(subset(fox, (year == years[i] & month == months[j]), 
                   select = likes_count)) == 0){
      
      fox_nLikes[k] <- 0
      
      k = k + 1
      
    }
    else{
      fox_nLikes[k] <- sum(subset(fox, (year == years[i] & month == months[j]), 
                                  select = likes_count))
      k = k + 1
    }
    
  }
}
fox_nLikes <- fox_nLikes[-60]




abc_nLikes <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    
    if(nrow(subset(abc, (year == years[i] & month == months[j]), 
                   select = likes_count)) == 0){
      
      abc_nLikes[k] <- 0
      
      k = k + 1
      
    }
    else{
      abc_nLikes[k] <- sum(subset(abc, (year == years[i] & month == months[j]), 
                                  select = likes_count))
      k = k + 1
    }
    
  }
}
abc_nLikes <- abc_nLikes[-60]




huff_nLikes <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    
    if(nrow(subset(huff, (year == years[i] & month == months[j]), 
                   select = likes_count)) == 0){
      
      huff_nLikes[k] <- 0
      
      k = k + 1
      
    }
    else{
      huff_nLikes[k] <- sum(subset(huff, (year == years[i] & month == months[j]), 
                                  select = likes_count))
      k = k + 1
    }
    
  }
}
huff_nLikes <- huff_nLikes[-60]


nLikes <- data.frame(cnn_nLikes,fox_nLikes,huff_nLikes,abc_nLikes)



ggplot(data = nLikes, mapping = aes(x = timeline)) +
  geom_line(aes(y = cnn_nLikes), color = "red", size = 2) +
  geom_line(aes(y = fox_nLikes), color = "blue", size = 2) +
  geom_line(aes(y = huff_nLikes), color = "green", size = 2) +
  geom_line(aes(y = abc_nLikes), color = "yellow", size = 2) +
  scale_x_date(breaks = as.Date(c("2012-01-01",
                                  "2013-01-01",
                                  "2014-01-01",
                                  "2015-01-01",
                                  "2016-01-01")), date_labels = "%Y") +
  scale_y_continuous(breaks = seq(0,15000000, 1000000),
                     labels = comma) +
  labs(y = "# of Likes", x = "Time (Months)") + 
  theme(axis.text.x = element_text(vjust = 0.3)) +
  theme(plot.title = element_text(size = 20)) +
  theme(panel.grid.minor = element_line(linetype = "blank")) +
  theme(axis.text = element_text(family = "Tahoma", size = 13)) + 
  theme(axis.title = element_text(size = 16, face = "bold")) + 
  theme(axis.title = element_text(family = "Tahoma"), 
        axis.text = element_text(family = "Tahoma"), 
        axis.text.x = element_text(family = "Tahoma"), 
        axis.text.y = element_text(family = "Tahoma"), 
        plot.title = element_text(family = "Tahoma")) + 
  theme(axis.line = element_line(colour = "white"), 
        axis.ticks = element_line(colour = "white"), 
        axis.title = element_text(family = "sans",colour = "white"), 
        axis.text = element_text(family = "Tahoma", colour = "white"), 
        axis.text.x = element_text(colour = "white"), 
        axis.text.y = element_text(colour = "white"), 
        plot.title = element_text(family = "Tahoma", colour = "white"),
        panel.background = element_rect(fill = "black",linetype = "solid"), 
        plot.background = element_rect(fill = "black", colour = NA, linetype = "solid")) +
  labs(colour = "white", fill = "white") + 
  theme(panel.grid.major = element_line(colour = "gray60")) + 
  theme(plot.subtitle = element_text(family = "sans",size = 15), 
        axis.text = element_text(family = "sans"), 
        plot.title = element_text(family = "sans")) + 
  theme(axis.text = element_text(hjust = 0.5)) +
  labs(title = "Number of Likes Over Time") +
  theme(axis.ticks = element_line(size = 0.6), 
        axis.text.x = element_text(size = 16))






##################################################################################

cnn_nComment <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    
    if(nrow(subset(cnn, (year == years[i] & month == months[j]), 
                   select = comments_count)) == 0){
      
      cnn_nComment[k] <- 0
      
      k = k + 1
      
    }
    else{
      cnn_nComment[k] <- sum(subset(cnn, (year == years[i] & month == months[j]), 
                                  select = comments_count))
      k = k + 1
    }
    
  }
}
cnn_nComment <- cnn_nComment[-60]



fox_nComment <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    
    if(nrow(subset(fox, (year == years[i] & month == months[j]), 
                   select = comments_count)) == 0){
      
      fox_nComment[k] <- 0
      
      k = k + 1
      
    }
    else{
      fox_nComment[k] <- sum(subset(fox, (year == years[i] & month == months[j]), 
                                  select = comments_count))
      k = k + 1
    }
    
  }
}
fox_nComment <- fox_nComment[-60]




abc_nComment <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    
    if(nrow(subset(abc, (year == years[i] & month == months[j]), 
                   select = comments_count)) == 0){
      
      abc_nComment[k] <- 0
      
      k = k + 1
      
    }
    else{
      abc_nComment[k] <- sum(subset(abc, (year == years[i] & month == months[j]), 
                                  select = comments_count))
      k = k + 1
    }
    
  }
}
abc_nComment <- abc_nComment[-60]




huff_nComment <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    
    if(nrow(subset(huff, (year == years[i] & month == months[j]), 
                   select = comments_count)) == 0){
      
      huff_nComment[k] <- 0
      
      k = k + 1
      
    }
    else{
      huff_nComment[k] <- sum(subset(huff, (year == years[i] & month == months[j]), 
                                   select = comments_count))
      k = k + 1
    }
    
  }
}
huff_nComment <- huff_nComment[-60]


nComment <- data.frame(cnn_nComment,fox_nComment,huff_nComment,abc_nComment)


ggplot(data = nComment, mapping = aes(x = timeline)) +
  geom_line(aes(y = cnn_nComment), color = "red", size = 2) +
  geom_line(aes(y = fox_nComment), color = "blue", size = 2) +
  geom_line(aes(y = huff_nComment), color = "green", size = 2) +
  geom_line(aes(y = abc_nComment), color = "yellow", size = 2) +
  scale_x_date(breaks = timeline, date_labels = "%Y-%b") +
  scale_y_continuous(labels = comma) +
  theme(plot.subtitle = element_text(vjust = 1),
        plot.caption = element_text(vjust = 1),
        axis.text.x = element_text(angle = 90)) +
  labs(y = "# of Comments", x = "Time (Months)") + 
  theme(axis.text.x = element_text(vjust = 0.3)) +
  theme(plot.title = element_text(size = 20)) +
  theme(panel.grid.minor = element_line(linetype = "blank")) +
  theme(axis.text = element_text(family = "Tahoma", size = 13)) + 
  theme(axis.title = element_text(size = 16, face = "bold")) + 
  theme(axis.title = element_text(family = "Tahoma"), 
        axis.text = element_text(family = "Tahoma"), 
        axis.text.x = element_text(family = "Tahoma"), 
        axis.text.y = element_text(family = "Tahoma"), 
        plot.title = element_text(family = "Tahoma")) + 
  theme(axis.line = element_line(colour = "white"), 
        axis.ticks = element_line(colour = "white"), 
        axis.title = element_text(family = "sans",colour = "white"), 
        axis.text = element_text(family = "Tahoma", colour = "white"), 
        axis.text.x = element_text(colour = "white"), 
        axis.text.y = element_text(colour = "white"), 
        plot.title = element_text(family = "Tahoma", colour = "white"),
        panel.background = element_rect(fill = "black",linetype = "solid"), 
        plot.background = element_rect(fill = "black", colour = NA, linetype = "solid")) +
  labs(colour = "white", fill = "white") + 
  theme(panel.grid.major = element_line(colour = "gray60")) + 
  theme(plot.subtitle = element_text(family = "sans",size = 15), 
        axis.text = element_text(family = "sans"), 
        plot.title = element_text(family = "sans")) + 
  theme(axis.text = element_text(hjust = 0)) +
  labs(title = "Number of Comments Over Time")



##########################################################################################



cnn_nShare <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    
    if(nrow(subset(cnn, (year == years[i] & month == months[j]), 
                   select = shares_count)) == 0){
      
      cnn_nShare[k] <- 0
      
      k = k + 1
      
    }
    else{
      cnn_nShare[k] <- sum(subset(cnn, (year == years[i] & month == months[j]), 
                                    select = shares_count))
      k = k + 1
    }
    
  }
}
cnn_nShare <- cnn_nShare[-60]



fox_nShare <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    
    if(nrow(subset(fox, (year == years[i] & month == months[j]), 
                   select = shares_count)) == 0){
      
      fox_nShare[k] <- 0
      
      k = k + 1
      
    }
    else{
      fox_nShare[k] <- sum(subset(fox, (year == years[i] & month == months[j]), 
                                    select = shares_count))
      k = k + 1
    }
    
  }
}
fox_nShare <- fox_nShare[-60]




abc_nShare <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    
    if(nrow(subset(abc, (year == years[i] & month == months[j]), 
                   select = shares_count)) == 0){
      
      abc_nShare[k] <- 0
      
      k = k + 1
      
    }
    else{
      abc_nShare[k] <- sum(subset(abc, (year == years[i] & month == months[j]), 
                                    select = shares_count))
      k = k + 1
    }
    
  }
}
abc_nShare <- abc_nShare[-60]




huff_nShare <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    
    if(nrow(subset(huff, (year == years[i] & month == months[j]), 
                   select = shares_count)) == 0){
      
      huff_nShare[k] <- 0
      
      k = k + 1
      
    }
    else{
      huff_nShare[k] <- sum(subset(huff, (year == years[i] & month == months[j]), 
                                     select = shares_count))
      k = k + 1
    }
    
  }
}
huff_nShare <- huff_nShare[-60]


nShare <- data.frame(cnn_nShare,fox_nShare,huff_nShare,abc_nShare)


ggplot(data = nShare, mapping = aes(x = timeline)) +
  geom_line(aes(y = cnn_nShare), color = "red", size = 2) +
  geom_line(aes(y = fox_nShare), color = "blue", size = 2) +
  geom_line(aes(y = huff_nShare), color = "green", size = 2) +
  geom_line(aes(y = abc_nShare), color = "yellow", size = 2) +
  scale_x_date(breaks = as.Date(c("2012-01-01",
                                  "2013-01-01",
                                  "2014-01-01",
                                  "2015-01-01",
                                  "2016-01-01")), date_labels = "%Y") +
  scale_y_continuous(breaks = seq(0,7000000,1000000),labels = comma) +
  labs(y = "# of Shares", x = "Time (Months)") + 
  theme(axis.text.x = element_text(vjust = 0.3)) +
  theme(plot.title = element_text(size = 20)) +
  theme(panel.grid.minor = element_line(linetype = "blank")) +
  theme(axis.text = element_text(family = "Tahoma", size = 13)) + 
  theme(axis.title = element_text(size = 16, face = "bold")) + 
  theme(axis.title = element_text(family = "Tahoma"), 
        axis.text = element_text(family = "Tahoma"), 
        axis.text.x = element_text(family = "Tahoma"), 
        axis.text.y = element_text(family = "Tahoma"), 
        plot.title = element_text(family = "Tahoma")) + 
  theme(axis.line = element_line(colour = "white"), 
        axis.ticks = element_line(colour = "white"), 
        axis.title = element_text(family = "sans",colour = "white"), 
        axis.text = element_text(family = "Tahoma", colour = "white"), 
        axis.text.x = element_text(colour = "white"), 
        axis.text.y = element_text(colour = "white"), 
        plot.title = element_text(family = "Tahoma", colour = "white"),
        panel.background = element_rect(fill = "black",linetype = "solid"), 
        plot.background = element_rect(fill = "black", colour = NA, linetype = "solid")) +
  labs(colour = "white", fill = "white") + 
  theme(panel.grid.major = element_line(colour = "gray60")) + 
  theme(plot.subtitle = element_text(family = "sans",size = 15), 
        axis.text = element_text(family = "sans"), 
        plot.title = element_text(family = "sans")) + 
  theme(axis.text = element_text(hjust = 0.5)) +
  labs(title = "Number of Shares Over Time") +
  theme(axis.ticks = element_line(size = 0.6), 
        axis.text.x = element_text(size = 16))




##########################################################################################


############################NEW SHOOTING GRAPHS###########################################




shooting <- FB[str_detect(FB$name,"shooting"),]

cnn_shoot <- shooting[shooting$Source == "CNN",]
fox_shoot <- shooting[shooting$Source == "FOX_NEWS",]
abc_shoot <- shooting[shooting$Source == "ABC_NEWS",]
huff_shoot <- shooting[shooting$Source == "HUFF_POST",]


#########################################################################

cnn_shoot_nLikes <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    
    if(nrow(subset(cnn_shoot, (year == years[i] & month == months[j]), 
                   select = likes_count)) == 0){
      
      cnn_shoot_nLikes[k] <- 0
      
      k = k + 1
      
    }
    else{
      cnn_shoot_nLikes[k] <- sum(subset(cnn_shoot, (year == years[i] & month == months[j]), 
                                  select = likes_count))
      k = k + 1
    }
    
  }
}
cnn_shoot_nLikes <- cnn_shoot_nLikes[-60]



fox_shoot_nLikes <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    
    if(nrow(subset(fox_shoot, (year == years[i] & month == months[j]), 
                   select = likes_count)) == 0){
      
      fox_shoot_nLikes[k] <- 0
      
      k = k + 1
      
    }
    else{
      fox_shoot_nLikes[k] <- sum(subset(fox_shoot, (year == years[i] & month == months[j]), 
                                  select = likes_count))
      k = k + 1
    }
    
  }
}
fox_shoot_nLikes <- fox_shoot_nLikes[-60]




abc_shoot_nLikes <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    
    if(nrow(subset(abc_shoot, (year == years[i] & month == months[j]), 
                   select = likes_count)) == 0){
      
      abc_shoot_nLikes[k] <- 0
      
      k = k + 1
      
    }
    else{
      abc_shoot_nLikes[k] <- sum(subset(abc_shoot, (year == years[i] & month == months[j]), 
                                  select = likes_count))
      k = k + 1
    }
    
  }
}
abc_shoot_nLikes <- abc_shoot_nLikes[-60]




huff_shoot_nLikes <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    
    if(nrow(subset(huff_shoot, (year == years[i] & month == months[j]), 
                   select = likes_count)) == 0){
      
      huff_shoot_nLikes[k] <- 0
      
      k = k + 1
      
    }
    else{
      huff_shoot_nLikes[k] <- sum(subset(huff_shoot, (year == years[i] & month == months[j]), 
                                   select = likes_count))
      k = k + 1
    }
    
  }
}
huff_shoot_nLikes <- huff_shoot_nLikes[-60]


nLikes <- data.frame(cnn_shoot_nLikes,fox_shoot_nLikes,huff_shoot_nLikes,abc_shoot_nLikes)



sp2 <- ggplot(data = nLikes, mapping = aes(x = timeline)) +
  geom_line(aes(y = cnn_shoot_nLikes), color = "red", size = 2) +
  geom_line(aes(y = fox_shoot_nLikes), color = "blue", size = 2) +
  geom_line(aes(y = huff_shoot_nLikes), color = "green", size = 2) +
  geom_line(aes(y = abc_shoot_nLikes), color = "yellow", size = 2) +
  scale_x_date(breaks = as.Date(c("2012-01-01",
                                  "2013-01-01",
                                  "2014-01-01",
                                  "2015-01-01",
                                  "2016-01-01")), date_labels = "%Y") +
  scale_y_continuous(labels = comma) +
  labs(y = "# of Likes", x = "Time (Months)") + 
  theme(axis.text.x = element_text(vjust = 0.3)) +
  theme(plot.title = element_text(size = 20)) +
  theme(panel.grid.minor = element_line(linetype = "blank")) +
  theme(axis.text = element_text(family = "Tahoma", size = 13)) + 
  theme(axis.title = element_text(size = 16, face = "bold")) + 
  theme(axis.title = element_text(family = "Tahoma"), 
        axis.text = element_text(family = "Tahoma"), 
        axis.text.x = element_text(family = "Tahoma"), 
        axis.text.y = element_text(family = "Tahoma"), 
        plot.title = element_text(family = "Tahoma")) + 
  theme(axis.line = element_line(colour = "white"), 
        axis.ticks = element_line(colour = "white"), 
        axis.title = element_text(family = "sans",colour = "white"), 
        axis.text = element_text(family = "Tahoma", colour = "white"), 
        axis.text.x = element_text(colour = "white"), 
        axis.text.y = element_text(colour = "white"), 
        plot.title = element_text(family = "Tahoma", colour = "white"),
        panel.background = element_rect(fill = "black",linetype = "solid"), 
        plot.background = element_rect(fill = "black", colour = NA, linetype = "solid")) +
  labs(colour = "white", fill = "white") + 
  theme(panel.grid.major = element_line(colour = "gray60")) + 
  theme(plot.subtitle = element_text(family = "sans",size = 15), 
        axis.text = element_text(family = "sans"), 
        plot.title = element_text(family = "sans")) + 
  theme(axis.text = element_text(hjust = 0.5)) +
  labs(title = "Number of Likes Over Time") + 
  theme(plot.subtitle = element_text(family = "Tahoma", 
    colour = "white")) +
  labs(subtitle = "Titles of News Stories Contain the Phrase 'Shooting(s)'") +
  theme(axis.ticks = element_line(size = 0.6), 
        axis.text.x = element_text(size = 16))



plot(sp2)



##################################################################################

cnn_shoot_nComment <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    
    if(nrow(subset(cnn_shoot, (year == years[i] & month == months[j]), 
                   select = comments_count)) == 0){
      
      cnn_shoot_nComment[k] <- 0
      
      k = k + 1
      
    }
    else{
      cnn_shoot_nComment[k] <- sum(subset(cnn_shoot, (year == years[i] & month == months[j]), 
                                    select = comments_count))
      k = k + 1
    }
    
  }
}
cnn_shoot_nComment <- cnn_shoot_nComment[-60]



fox_shoot_nComment <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    
    if(nrow(subset(fox_shoot, (year == years[i] & month == months[j]), 
                   select = comments_count)) == 0){
      
      fox_shoot_nComment[k] <- 0
      
      k = k + 1
      
    }
    else{
      fox_shoot_nComment[k] <- sum(subset(fox_shoot, (year == years[i] & month == months[j]), 
                                    select = comments_count))
      k = k + 1
    }
    
  }
}
fox_shoot_nComment <- fox_shoot_nComment[-60]




abc_shoot_nComment <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    
    if(nrow(subset(abc_shoot, (year == years[i] & month == months[j]), 
                   select = comments_count)) == 0){
      
      abc_shoot_nComment[k] <- 0
      
      k = k + 1
      
    }
    else{
      abc_shoot_nComment[k] <- sum(subset(abc_shoot, (year == years[i] & month == months[j]), 
                                    select = comments_count))
      k = k + 1
    }
    
  }
}
abc_shoot_nComment <- abc_shoot_nComment[-60]




huff_shoot_nComment <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    
    if(nrow(subset(huff_shoot, (year == years[i] & month == months[j]), 
                   select = comments_count)) == 0){
      
      huff_shoot_nComment[k] <- 0
      
      k = k + 1
      
    }
    else{
      huff_shoot_nComment[k] <- sum(subset(huff_shoot, (year == years[i] & month == months[j]), 
                                     select = comments_count))
      k = k + 1
    }
    
  }
}
huff_shoot_nComment <- huff_shoot_nComment[-60]


nComment <- data.frame(cnn_shoot_nComment,fox_shoot_nComment,huff_shoot_nComment,abc_shoot_nComment)


sp3 <- ggplot(data = nComment, mapping = aes(x = timeline)) +
  geom_line(aes(y = cnn_shoot_nComment), color = "red", size = 2) +
  geom_line(aes(y = fox_shoot_nComment), color = "blue", size = 2) +
  geom_line(aes(y = huff_shoot_nComment), color = "green", size = 2) +
  geom_line(aes(y = abc_shoot_nComment), color = "yellow", size = 2) +
  scale_x_date(breaks = as.Date(c("2012-01-01",
                                  "2013-01-01",
                                  "2014-01-01",
                                  "2015-01-01",
                                  "2016-01-01")), date_labels = "%Y") +
  scale_y_continuous(labels = comma) +
  labs(y = "# of Comments", x = "Time (Months)") + 
  theme(axis.text.x = element_text(vjust = 0.3)) +
  theme(plot.title = element_text(size = 20)) +
  theme(panel.grid.minor = element_line(linetype = "blank")) +
  theme(axis.text = element_text(family = "Tahoma", size = 13)) + 
  theme(axis.title = element_text(size = 16, face = "bold")) + 
  theme(axis.title = element_text(family = "Tahoma"), 
        axis.text = element_text(family = "Tahoma"), 
        axis.text.x = element_text(family = "Tahoma"), 
        axis.text.y = element_text(family = "Tahoma"), 
        plot.title = element_text(family = "Tahoma")) + 
  theme(axis.line = element_line(colour = "white"), 
        axis.ticks = element_line(colour = "white"), 
        axis.title = element_text(family = "sans",colour = "white"), 
        axis.text = element_text(family = "Tahoma", colour = "white"), 
        axis.text.x = element_text(colour = "white"), 
        axis.text.y = element_text(colour = "white"), 
        plot.title = element_text(family = "Tahoma", colour = "white"),
        panel.background = element_rect(fill = "black",linetype = "solid"), 
        plot.background = element_rect(fill = "black", colour = NA, linetype = "solid")) +
  labs(colour = "white", fill = "white") + 
  theme(panel.grid.major = element_line(colour = "gray60")) + 
  theme(plot.subtitle = element_text(family = "sans",size = 15), 
        axis.text = element_text(family = "sans"), 
        plot.title = element_text(family = "sans")) + 
  theme(axis.text = element_text(hjust = 0.5)) +
  labs(title = "Number of Comments Over Time") + 
  theme(plot.subtitle = element_text(family = "Tahoma", 
                                     colour = "white")) +
  labs(subtitle = "Titles of News Stories Contain the Phrase 'Shooting(s)'") +
  theme(axis.ticks = element_line(size = 0.6), 
        axis.text.x = element_text(size = 16))

plot(sp3)



##########################################################################################



cnn_shoot_nShare <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    
    if(nrow(subset(cnn_shoot, (year == years[i] & month == months[j]), 
                   select = shares_count)) == 0){
      
      cnn_shoot_nShare[k] <- 0
      
      k = k + 1
      
    }
    else{
      cnn_shoot_nShare[k] <- sum(subset(cnn_shoot, (year == years[i] & month == months[j]), 
                                  select = shares_count))
      k = k + 1
    }
    
  }
}
cnn_shoot_nShare <- cnn_shoot_nShare[-60]



fox_shoot_nShare <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    
    if(nrow(subset(fox_shoot, (year == years[i] & month == months[j]), 
                   select = shares_count)) == 0){
      
      fox_shoot_nShare[k] <- 0
      
      k = k + 1
      
    }
    else{
      fox_shoot_nShare[k] <- sum(subset(fox_shoot, (year == years[i] & month == months[j]), 
                                  select = shares_count))
      k = k + 1
    }
    
  }
}
fox_shoot_nShare <- fox_shoot_nShare[-60]




abc_shoot_nShare <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    
    if(nrow(subset(abc_shoot, (year == years[i] & month == months[j]), 
                   select = shares_count)) == 0){
      
      abc_shoot_nShare[k] <- 0
      
      k = k + 1
      
    }
    else{
      abc_shoot_nShare[k] <- sum(subset(abc_shoot, (year == years[i] & month == months[j]), 
                                  select = shares_count))
      k = k + 1
    }
    
  }
}
abc_shoot_nShare <- abc_shoot_nShare[-60]




huff_shoot_nShare <- rep(NA, length = length(timeline))
k = 1
for (i in 1:length(years)){
  for (j in 1:length(months)){
    
    if(nrow(subset(huff_shoot, (year == years[i] & month == months[j]), 
                   select = shares_count)) == 0){
      
      huff_shoot_nShare[k] <- 0
      
      k = k + 1
      
    }
    else{
      huff_shoot_nShare[k] <- sum(subset(huff_shoot, (year == years[i] & month == months[j]), 
                                   select = shares_count))
      k = k + 1
    }
    
  }
}
huff_shoot_nShare <- huff_shoot_nShare[-60]


nShare <- data.frame(cnn_shoot_nShare,fox_shoot_nShare,huff_shoot_nShare,abc_shoot_nShare)


sp4 <- ggplot(data = nShare, mapping = aes(x = timeline)) +
  geom_line(aes(y = cnn_shoot_nShare), color = "red", size = 2) +
  geom_line(aes(y = fox_shoot_nShare), color = "blue", size = 2) +
  geom_line(aes(y = huff_shoot_nShare), color = "green", size = 2) +
  geom_line(aes(y = abc_shoot_nShare), color = "yellow", size = 2) +
  scale_x_date(breaks = timeline, date_labels = "%Y-%b") +
  scale_y_continuous(breaks = seq(0,600000,100000),
                     labels = comma) +
  theme(plot.subtitle = element_text(vjust = 1),
        plot.caption = element_text(vjust = 1),
        axis.text.x = element_text(angle = 90)) +
  labs(y = "# of Shares", x = "Time (Months)") + 
  theme(axis.text.x = element_text(vjust = 0.3)) +
  theme(plot.title = element_text(size = 20)) +
  theme(panel.grid.minor = element_line(linetype = "blank")) +
  theme(axis.text = element_text(family = "Tahoma", size = 13)) + 
  theme(axis.title = element_text(size = 16, face = "bold")) + 
  theme(axis.title = element_text(family = "Tahoma"), 
        axis.text = element_text(family = "Tahoma"), 
        axis.text.x = element_text(family = "Tahoma"), 
        axis.text.y = element_text(family = "Tahoma"), 
        plot.title = element_text(family = "Tahoma")) + 
  theme(axis.line = element_line(colour = "white"), 
        axis.ticks = element_line(colour = "white"), 
        axis.title = element_text(family = "sans",colour = "white"), 
        axis.text = element_text(family = "Tahoma", colour = "white"), 
        axis.text.x = element_text(colour = "white"), 
        axis.text.y = element_text(colour = "white"), 
        plot.title = element_text(family = "Tahoma", colour = "white"),
        panel.background = element_rect(fill = "black",linetype = "solid"), 
        plot.background = element_rect(fill = "black", colour = NA, linetype = "solid")) +
  labs(colour = "white", fill = "white") + 
  theme(panel.grid.major = element_line(colour = "gray60")) + 
  theme(plot.subtitle = element_text(family = "sans",size = 15), 
        axis.text = element_text(family = "sans"), 
        plot.title = element_text(family = "sans")) + 
  theme(axis.text = element_text(hjust = 0)) +
  labs(title = "Number of Shares Over Time") + 
  theme(plot.subtitle = element_text(family = "Tahoma", 
                                     colour = "white")) +
  labs(subtitle = "Titles of News Stories Contain the Phrase 'Shooting(s)'")
 
plot(sp4)




ggarrange(sp1,sp2,sp3,sp4, nrow = 2, ncol = 2)









