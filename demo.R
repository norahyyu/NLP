rm(list = ls())

library(tm)
library(NLP)
library(ggplot2)
library(zoo)
library(wordcloud)

setwd("~/Desktop/SocialMediaAnalytics/slowtrend")
docs<-Corpus(DirSource(c('fb2011','fb2012','fb2013','fb2014','fb2015'))) #build corpus

# function to calculate term frequency

termfreq<-function(term){
    dtm <- DocumentTermMatrix(docs,control=list(dictionary=term,
                                                tolower=T, 
                                                removePunctuation=T, 
                                                removeNumbers=T, 
                                                stripWhitespace=T,
                                                stopwords=stopwords("english")
                                                ))
    dtm <- removeSparseTerms(dtm,0.95)
    freq = rowSums( as.matrix(dtm) )
    freq <-  data.frame(freq)
    return(freq)
}



# function to generate dataframe for plotting
t<-list()
for (i in seq(2011,2015)){
    for (j in c(01,10,11,12,seq(2,9))){
        m <- month.abb[j]
        d <- paste(as.character(i),m,sep='-')
        t <- append(t,d)
    }
}
d <- unlist(t)
d2<- as.yearmon(d,'%Y-%b')

preparedf <- function(df){
    df['date'] <- as.Date(d2)
    df <- df[order(df$date),]
    return(df)
}

##function to calculate moving average
movingavg<- function(N,data){
    ma<-data.frame('MA'= c(rollmean(data,N)))
    blank<-data.frame('MA'=rep(NA,N-1))
    mafull<-rbind(blank,ma)
    data['MA']<-mafull
    return(data)
}


#food-related terms DTM
ingredients <- read.csv('foodingredient.txt',header=FALSE)
ingredients<- tolower(ingredients$V1)
dtm_full <- DocumentTermMatrix(docs,control=list(dictionary=ingredients,
                                            tolower=T, 
                                            removePunctuation=T, 
                                            removeNumbers=T, 
                                            stripWhitespace=T,
                                            stopwords=stopwords("english")))
dtm_full <- removeSparseTerms(dtm_full,0.99)

# top 20 popular food
freq_full = colSums(as.matrix(dtm_full) )
wordcloud(names(freq_full), freq_full, max.words=20)



############################validation######################
###########1.cauliflower rice trend 
test1 <- termfreq('cauliflower')
cauliflower <- preparedf(test1)

test2 <- termfreq('rice')
rice <- preparedf(test2)

#co-occurance of cauliflower and rice
ggplot(cauliflower,aes(x=date,y=freq,color='cauliflower'))+ 
    geom_line(stat='identity')+
    geom_line(data=rice, aes(x=date,y=freq,color='rice'))+
    ggtitle('Cauliflower Rice Trend')+
    scale_x_date(date_labels="%y/%m",date_breaks = "4 month")+
    labs(x = "Time", y = "Num of Post")+
    theme(plot.title = element_text(size = 20, face = "bold"))

# let's see the trend
marice<-movingavg(5,test2)
marice['date']<-rice['date']

maCauliflower<-movingavg(5,test1)
maCauliflower['date']<-cauliflower['date']

ggplot(marice,aes(x=date,y=freq,color='rice'))+ 
    geom_line(stat='identity')+
    geom_line(data=marice, aes(x=date,y=MA,color='moving average of rice'))+
    geom_line(data=maCauliflower,aes(x=date,y=freq,color='cauliflower'))+
    geom_line(data=maCauliflower, aes(x=date,y=MA,color='moving average of cauliflower'))+
    ggtitle('Cauliflower &  Rice Trend')+
    scale_x_date(date_labels="%y/%m",date_breaks = "5 month")+
    labs(x = "Time", y = "Num of Post")+
    theme(plot.title = element_text(size = 20, face = "bold"))

#cauliflower trend
ggplot(cauliflower,aes(x=date,y=freq,color='cauliflower'))+ 
    geom_line(stat='identity')+
    geom_line(data=maCauliflower, aes(x=date,y=MA,color='moving average of cauliflower'))+
    ggtitle(' Cauliflower Trend')+
    scale_x_date(date_labels="%y/%m",date_breaks = "5 month")+
    labs(x = "Time", y = "Num of Post")+
    theme(plot.title = element_text(size = 20, face = "bold"))



###########2.vegetable noodle trend 
test5 <- termfreq(c('noodle','pasta','spiralizer'))
test7<- termfreq('vegetable')
test8<- termfreq(c('zucchini','squash','cucumber'))

noodle <- preparedf(test5)
vegetable <- preparedf(test7)
veg<- preparedf(test8)


ggplot(noodle,aes(x=date,y=freq,color='noodle,pasta,spiralizer'))+ 
    geom_line(stat='identity')+
    geom_line(data=vegetable, aes(x=date,y=freq,color='vegetable') )+
    geom_line(data=veg, aes(x=date,y=freq,color='zucchini,squash,cucumber') )+
    ggtitle('Vegetable Noodle Trend')+
    scale_x_date(date_labels="%y/%m",date_breaks = "4 month")+
    labs(x = "Time", y = "Num of Post")+
    theme(plot.title = element_text(size = 20, face = "bold"))

#use moving average to see trend use N=5
sp<-termfreq('spiralizer')
spiralizer<-preparedf(sp)
maSp<-movingavg(5,sp)
maSp['date']<-spiralizer['date']

v<-termfreq('veggie')
veggie<-preparedf(v)
mav<-movingavg(5,v)
mav['date']<-veggie['date']

ggplot(spiralizer,aes(x=date,y=freq,color='spiralizer'))+ 
    geom_line(stat='identity')+
    geom_line(data=maSp, aes(x=date,y=MA,color='moving average of spiralizer'))+
    geom_line(data=veggie, aes(x=date,y=freq,color='veggie'))+
    geom_line(data=mav, aes(x=date,y=MA,color='moving average of veggie'))+
    ggtitle('Veggie & Spiralizer Trend')+
    scale_x_date(date_labels="%y/%m",date_breaks = "5 month")+
    labs(x = "Time", y = "Num of Post")+
    theme(plot.title = element_text(size = 20, face = "bold"))

#veggie is not accurate, use term spiralizer could better capture the trend
ggplot(spiralizer,aes(x=date,y=freq,color='spiralizer'))+ 
    geom_line(stat='identity')+
    geom_line(data=maSp, aes(x=date,y=MA,color='moving average of spiralizer'))+
    ggtitle(' Spiralizer Trend')+
    scale_x_date(date_labels="%y/%m",date_breaks = "5 month")+
    labs(x = "Time", y = "Num of Post")+
    theme(plot.title = element_text(size = 20, face = "bold"))


#pumpkin pie trend
test3 <- termfreq('pumpkin')
pumpkin <- preparedf(test3)

test4 <- termfreq('pie')
pie <- preparedf(test4)

ggplot(pumpkin,aes(x=date,y=freq,color='pumpkin'))+ 
    geom_line(stat='identity')+
    geom_line(data=pie, aes(x=date,y=freq,color='pie'))+
    ggtitle('Pumpkin Pie Trend')+
    scale_x_date(date_labels="%y/%m",date_breaks = "4 month")+
    labs(x = "Time", y = "Num of Post")+
    theme(plot.title = element_text(size = 20, face = "bold"))




