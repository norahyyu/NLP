rm(list = ls())

library(tm)
library(NLP)
library(ggplot2)
library(zoo)
library(wordcloud)
library(text2vec)

setwd("~/UR Dropbox/norah yu/SocialMediaAnalytics/slowtrend")
fileName =list('fb2011/','fb2012/','fb2013/','fb2014/','fb2015/')
folder <- "~/UR Dropbox/norah yu/SocialMediaAnalytics/slowtrend/"
  

#############co-occurance function##############
wordpro <- function(x){
    x<- gsub('http\\S+\\s*',"", x)
    x<- tolower(x)
    x<-removePunctuation(x)
    x<- removeNumbers(x)
    x<- stripWhitespace(x)
}

tcmyear <- function(data){
  
  it = itoken(ingre, preprocessor=wordpro, tokenizer=word_tokenizer)
  it2 = itoken(data, preprocessor=wordpro, tokenizer=word_tokenizer)
    vocab <- create_vocabulary(it)
    # vocab_vectorizer() creates an object defining how to transform list of tokens into vector space - i.e. how to map words to indices
    vectorizer <- vocab_vectorizer(vocab)
    tcm <- create_tcm(it2, vectorizer, skip_grams_window = 5L,skip_grams_window_context = "right")
    tcm <- as.matrix(tcm)
    return(tcm)
}

coorTrend <- function(target1,target2){
    timerange=c()
    coor=c()
    ppmi=c()
    lift=c()
    for (y in seq(2011, 2015)){
        folder2 <- paste0(folder,'fb',y,'/')
        file_list <- list.files(path=folder2, pattern="*.csv")
        for (f in file_list){
            print(f)
            text = readLines(paste0(folder2,f))
            tcm<- tcmyear(text)
            print(sum(tcm[target1,target2]))
            coor <-append(coor,sum(tcm[target1,target2]))
            
           
    
            m = tcm
            marginal = matrix(rowSums(m), dim(m)[1]) #create matrix n*1
            liftm = sum(m) * m / ( marginal %*% t(marginal) )   #sum(m),m- element in tcm
            print(liftm[target1[1],target2[1]])
            ppmim = log2( liftm*(liftm>1) + (liftm<=1) ) #matrix add element by element
            print(ppmim[target1[1],target2[1]])
            ppmi <- append(ppmi,ppmim[target1[1],target2[1]])
            lift <- append(lift,liftm[target1[1],target2[1]])
            f <- substring(f,regexpr('-',f)+1,regexpr('\\.',f)-1)
            timerange<- append(timerange,f)
        }
    }
    return(cbind(timerange,coor,lift,ppmi))
}

t<-c()
for (i in seq(2011,2015)){
    for (j in c(01,10,11,12,seq(2,9))){
        m <- month.abb[j]
        d <- paste(as.character(i),m,sep='-')
        t <- append(t,d)
    }
}
d <- as.yearmon(t,'%Y-%b')
preparedf <- function(df){
  df$coor <- as.numeric(as.character(df$coor))
    df$timerange <- as.Date(d)
    df <- df[order(df$timerange),]
    return(df)
}


prepareppmi <- function(df){
  df$ppmi <- as.numeric(as.character(df$ppmi))
  df$timerange <- as.Date(d)
  df <- df[order(df$timerange),]
  return(df)
}


# function to calculate term frequency
termfreq<-function(term){
    dtm <- DocumentTermMatrix(docs,control=list(dictionary=term,
                                                tolower=T, 
                                                removePunctuation=T, 
                                                removeNumbers=T, 
                                                stripWhitespace=T,
                                                stopwords=c(stopwords('spanish'),stopwords("english"))
    ))
    dtm <- removeSparseTerms(dtm,0.95)
    freq = rowSums( as.matrix(dtm) )
    freq <-  data.frame(freq)
    return(freq)
}



#food-related terms DTM
ingredients <- read.csv('foodingredient.txt',header=FALSE,stringsAsFactors = F)
ingredients<- tolower(ingredients$V1)
ingredients <- append(ingredients,c('veggie','vegetable','zucchini','spiralizer','toast'))
ingre <- ingredients
docs<-Corpus(DirSource(c('fb2011','fb2012','fb2013','fb2014','fb2015')))
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




################################################################################
###########################Cauliflower rice co-occurence#########################
################################################################################
test1 <- termfreq('cauliflower')
test1$timerange <- as.Date(d)
test1 <- test1[order(avo$timerange),]
#plor cauliflower 
ggplot(data=test1,aes(x=timerange,y=freq))+ 
  geom_line(stat='identity')+
  ggtitle('Cauliflower Trend')+
  scale_x_date(date_labels="%y/%m",date_breaks = "4 month")+
  labs(x = "Time", y = "Num of Post")+
  theme(plot.title = element_text(size = 20, face = "bold"))

caulirice <- coorTrend('cauliflower','rice')
plotcaulirice <- preparedf(data.frame(caulirice))

#co-occurance of cauliflower and rice
ggplot(data=plotcaulirice,aes(x=timerange,y=coor))+ 
    geom_line(stat='identity')+
    ggtitle('Cauliflower Rice Trend')+
    scale_x_date(date_labels="%y/%m",date_breaks = "4 month")+
    labs(x = "Time", y = "Num of Post")+
    theme(plot.title = element_text(size = 20, face = "bold"))


plotcauliriceppmi <- prepareppmi(data.frame(caulirice))
ggplot(data=plotcauliriceppmi,aes(x=timerange,y=ppmi))+ 
  geom_line(stat='identity')+
  ggtitle('PPMI of Cauliflower Rice')+
  scale_x_date(date_labels="%y/%m",date_breaks = "4 month")+
  labs(x = "Time", y = "PPMI")+
  theme(plot.title = element_text(size = 20, face = "bold"))




################################################################################
###########################Avocado Toast Cooccurance#########################
################################################################################

avo <- termfreq('avocado')
avo$timerange <- as.Date(d)
avo <- avo[order(avo$timerange),]
ggplot(avo,aes(x=timerange,y=freq))+ 
  geom_line()+
  ggtitle('Avocado Trend')+
  scale_x_date(date_labels="%y/%m",date_breaks = "4 month")+
  labs(x = "Time", y = "Num of Post")+
  theme(plot.title = element_text(size = 20, face = "bold"))


avocadotoast <- data.frame(coorTrend('avocado','toast'))
plotavocadotoast <- preparedf(avocadotoast)
ggplot(plotavocadotoast,aes(x=timerange,y=coor))+ 
  geom_line()+
  ggtitle('Avocado Toast Co-occurence')+
  scale_x_date(date_labels="%y/%m",date_breaks = "4 month")+
  labs(x = "Time", y = "Num of Post")+
  theme(plot.title = element_text(size = 20, face = "bold"))

plotavocadotoastppmi <- prepareppmi(avocadotoast)
ggplot(plotavocadotoastppmi,aes(x=timerange,y=ppmi))+ 
  geom_line(stat='identity')+
  ggtitle('PPMI of Avocado Toast')+
  scale_x_date(date_labels="%y/%m",date_breaks = "4 month")+
  labs(x = "Time", y = "PPMI")+
  theme(plot.title = element_text(size = 20, face = "bold"))

################################################################################
###########################Vegetable Noodle cooccurance#########################
################################################################################

vegenoodle <- coorTrend(c('vegetable','veggie'),c('noodle','pasta'))
plotvegenoodle <- preparedf(data.frame(vegenoodle))

ggplot(plotvegenoodle,aes(x=timerange,y=coor))+ 
  geom_line()+
  ggtitle('Vegetable Noodle Co-occurence')+
  scale_x_date(date_labels="%y/%m",date_breaks = "4 month")+
  labs(x = "Time", y = "Num of Post")+
  theme(plot.title = element_text(size = 20, face = "bold"))


plotvegenoodleppmi <- prepareppmi(data.frame(vegenoodle))
ggplot(plotvegenoodleppmi,aes(x=timerange,y=ppmi))+ 
  geom_line()+
  ggtitle('Vegetable Noodle Co-occurence')+
  scale_x_date(date_labels="%y/%m",date_breaks = "4 month")+
  labs(x = "Time", y = "ppmi")+
  theme(plot.title = element_text(size = 20, face = "bold"))


zucnoodle <- coorTrend('zucchini',c('noodle','pasta'))
plotzucnoodle <- preparedf(data.frame(zucnoodle))
ggplot(plotzucnoodle,aes(x=timerange,y=coor))+ 
  geom_line()+
  ggtitle('Zucchini Noodle Co-occurence')+
  scale_x_date(date_labels="%y/%m",date_breaks = "4 month")+
  labs(x = "Time", y = "Num of Post")+
  theme(plot.title = element_text(size = 20, face = "bold"))

plotzucnoodleppmi <- prepareppmi(data.frame(zucnoodle))
ggplot(plotzucnoodleppmi,aes(x=timerange,y=ppmi))+ 
  geom_line()+
  ggtitle('PPMI of Zucchini Noodle')+
  scale_x_date(date_labels="%y/%m",date_breaks = "4 month")+
  labs(x = "Time", y = "ppmi")+
  theme(plot.title = element_text(size = 20, face = "bold"))



################################################################################
###########################Pumpkin Pie cooccurance#########################
################################################################################


pumpkinpie <- data.frame(coorTrend('pumpkin','pie'))
plotpumpkinpie <- preparedf(pumpkinpie)
ggplot(plotpumpkinpie,aes(x=timerange,y=coor))+ 
  geom_line()+
  ggtitle('Pumpkin Pie Co-occurence')+
  scale_x_date(date_labels="%y/%m",date_breaks = "4 month")+
  labs(x = "Time", y = "Num of Post")+
  theme(plot.title = element_text(size = 20, face = "bold"))



plotpumpkinpieppmi <- prepareppmi(pumpkinpie)
ggplot(plotpumpkinpieppmi,aes(x=timerange,y=ppmi))+ 
  geom_line()+
  ggtitle('Pumpkin Pie Co-occurence')+
  scale_x_date(date_labels="%y/%m",date_breaks = "4 month")+
  labs(x = "Time", y = "PPMI")+
  theme(plot.title = element_text(size = 20, face = "bold"))

