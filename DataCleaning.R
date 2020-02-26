library(NLP)
library("tidyr")
library("stopwords")
library("stringr")
library("tm")
library("wordcloud")
library("lda")
library("caret")
library("dplyr")
library("data.table")
library("tidytext")
library("topicmodels")
library("tm")
library("MASS")
library("caTools")
library("ldatuning")

data("stop_words")
text <-fread("example.csv")
names(text)

sample = sample.split(text$Title,SplitRatio = 0.75)

sample

train1 =subset(text$Title,sample ==TRUE)

test1=subset(text$Title, sample==FALSE)


h<-tolower(train1)

h



stop.words<- c("xyz","abc")

c<- gsub(paste0(stop.words,collapse = "|"),"", h)

c



z<-gsub("[0-9]*","",c)

z



p<-gsub("[[:punct:]]","",z)

p



class(p)



r<-trimws(p)

r

s<-stemDocument(r)

s



Corpus <- Corpus(VectorSource(s))

DTM <- DocumentTermMatrix(Corpus)

unique_indexes <- unique(DTM$i)

DTM <- DTM[unique_indexes,]

lda <- LDA(DTM, k =2 , control = list(seed = 1234))



lda



# as.data.frame(lda)



summary(lda)



class(lda)

topics <- tidy(lda, matrix = "beta")

topics

library(ggplot2)

library(dplyr)



ap_top_terms <- topics %>%
  
  group_by(topic) %>%
  
  top_n(10, beta) %>%
  
  ungroup() %>%
  
  arrange(topic, -beta)



ap_top_terms %>%
  
  mutate(term = reorder(term, beta)) %>%
  
  ggplot(aes(term, beta, fill = factor(topic))) +
  
  geom_col(show.legend = FALSE) +
  
  facet_wrap(~ topic, scales = "free") +
  
  coord_flip()

a<-tolower(test1)

a



stop.words<- c("xyz","abc")

b<- gsub(paste0(stop.words,collapse = "|"),"", a)

b



j<-gsub("[0-9]*","",b)

j



op<-gsub("[[:punct:]]","",j)

op



class(p)



ro<-trimws(op)

ro



so<-stemDocument(ro)

so

class(so)



Corpus <- Corpus(VectorSource(so))

DTM1 <- DocumentTermMatrix(Corpus)

unique_indexes <- unique(DTM1$i)

DTM1 <- DTM1[unique_indexes,]

(train.topics <- topics(lda))



test.topics <- posterior(lda,DTM1)

nrow(DTM1)

nrow(DTM)

test.topics

(test.topics <- apply(test.topics$topics,1,which.max))


system.time({
  
  tunes <- FindTopicsNumber(
    
    dtm = DTM,
    
    topics = c(2:20),
    
    method = "Gibbs",
    
    control = list(seed = 12345),
    
    mc.cores = 4L,
    
    verbose = TRUE
    
  )
  
})



FindTopicsNumber_plot(tunes)