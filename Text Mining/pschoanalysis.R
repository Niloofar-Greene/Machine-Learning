#set working directory
setwd('/Users/niloo/Documents/GitHub/Machine-Learning/Text Mining/TextMiningFolder')
getwd()

#libraries
library(tm) 

# Creates corpus
docs <- Corpus(DirSource('/Users/niloo/Documents/GitHub/Machine-Learning/Text Mining/TextMiningFolder/new'))
inspect(docs)

#Inspect a particular document
writeLines(as.character(docs[[1]]))

#Start pre-processing (data cleaning removing punctuation, stop words, ...)
#In order to fix the problem of replacing a hyphen with nothing, we need to create a custom transformation function
# reminder : gsub(pattern="a|i", replacement="_", x=animal)
toSpace <- content_transformer( function(x, pattern) { return( gsub(pattern, " ", x) ) } ) # replaces all instances of character pattern by space when x is the source

docs <- tm_map(docs, content_transformer(gsub), pattern="three", replacement="3")
docs <- tm_map(docs, content_transformer(gsub), pattern="five", replacement="5")
docs <- tm_map(docs, content_transformer(gsub), pattern="seven", replacement="7")
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, " -")

writeLines(as.character(docs[[1]]))

docs <- tm_map(docs, toSpace, "â???¦")

#Remove punctuation
docs <- tm_map(docs, removePunctuation)

#Strip digits
docs <- tm_map(docs, removeNumbers)

#Transform to lower case
docs <- tm_map(docs, content_transformer(tolower))

#Remove stop words from standard stop word list
docs <- tm_map(docs, removeWords, stopwords("english") )# you specify that the language is english
docs <- tm_map(docs, removeWords, c("one","two", "others","also","may", 'often', "can", "thing", "lot", "first", "onto", "another"
                                    , "give", "best", "less","might", "rather", "just", "likely", "better", "worse"))

#Strip White Space
docs <- tm_map(docs, stripWhitespace)

#Create document-term matrix - converting dtm to mathematical matrix
dtm <- DocumentTermMatrix(docs)

#Inspect segment of document term matrix
inspect(dtm[1:5, 10:55])

#Collapse matrix by stemming over columns - this gets total counts (over all docs) for each term
freq <- colSums(as.matrix(dtm))

#Length should be total number of terms
length(freq)

#Create sort order
ord <- order(freq, decreasing = TRUE)

#Inspect most and least frequent occurring term
freq[head(ord)]
freq[tail(ord)]

#Remove very frequent or very rare words - removing the words that are not important
dtmr <- DocumentTermMatrix( docs
                            , control=list(wordLengths=c(4,6)) #, bounds = list(global = c(3,7)) )
)

freqr <- colSums(as.matrix(dtmr))
length(freqr)
ordr <- order(freqr, decreasing = TRUE)
freqr[head(ordr)]
freqr[tail(ordr)]

#list most frequent terms. Finds all terms that occur more than or equal to 5 times. Lower bound specified as second argument
findFreqTerms(dtmr, lowfreq=5)

#Correlation
findAssocs(dtmr, "score", 0.6)
findAssocs(dtmr, "text", 0.6)

#Histogram
wf <- data.frame(term=names(freqr), occurrances=freqr)

library(ggplot2)

p <- ggplot(subset(wf, freqr>3), aes(term, occurrances) )
p <- p+geom_bar(stat="identity")
p <- p+theme(axis.text.x=element_text(angle=45, hjust=1))
p

#Word Cloud
library(RColorBrewer)
library(wordcloud)

wordcloud(names(freqr), freqr, min.freq=70)
wordcloud(names(freqr), freqr, min.freq=70, colors=brewer.pal(6, "Dark2"))