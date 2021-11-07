#set working directory
setwd('/Users/niloo/Documents/GitHub/Machine-Learning/Text Mining/TextMiningFolder')
getwd()

## Importing doc into R
# ReadLines : Read some or all text lines from a connection --> puts a cotation before and after a text in each paragraph
# Collapse : Collapses a character vector of any length into a length 1 vector --> removes spaces and put a / before original cotaion mark in the text
text <- paste(readLines("doc4.txt"), collapse=" ")
text

## Clean Up
# Lower case
text <- tolower(text)
text
# Remove numbers
gsub(pattern = "\\d", " ", text)
text

#Remove Stop Words
library(tm)

stopwords()

text <- removeWords(text, stopwords())
text

text <- removeWords(text, c("one","two", "others","also","may", 'often', "can", "thing", "lot", "first", "onto"))
grepl(pattern="also", text)
text

#Remove punctuation : \\W looks for spaces and punctuations
grep(pattern="\\W", text)
text <- gsub(pattern="\\W", replace=" ", text) # removes ?  , . () \ ' !
text

# \\b means the string start with sth - \\b means the string ends with sth
text <- gsub(pattern="\\b[a-z]\\b{1}"," " ,text) # it finds words with length = 1, the word starts with any letter and ends with any letter
text

# remove multiple empty spaces and convert them to a single empty space
text <- stripWhitespace(text)
text

#remove numbers
text <- removeNumbers(text)

# library stringr
library(stringr)
library(wordcloud)
library(RColorBrewer)

# str_split and paste/coppalse
a <- c("Hello", "the", "reason", "of", "my", "life")
b <- paste(b, collapse = " ") # makes a vector out of  many vectors butting " " between each vector
str_split(b, pattern = "\\s+") # pattern = any number of spaces

text <- str_split(text, pattern = "\\s+")
class(text)
str(text)

text <- unlist(text)
class(text)
str(text)
text

poswords <- scan('/Users/niloo/Documents/GitHub/Machine-Learning/Text Mining/negative-words.txt', what='character', comment.char=';')
poswords

negwords <- scan('/Users/niloo/Documents/GitHub/Machine-Learning/Text Mining/negative-words.txt', what='character', comment.char=';')
negwords

# Clean up doc from poswords
# It return the location of the word in a vector if matched ptherwise returns an NA
match(text, poswords) 

nbr_pos <- sum(!is.na(match(text, poswords))) # 3 positive words
nbr_neg <- sum(!is.na(match(text, negwords))) # no negative words

sentiment_score = nbr_pos - nbr_neg

mean_pos <- mean(!is.na(match(text, poswords)))
mean_neg <- mean(!is.na(match(text, negwords)))

std_pos <- sd(!is.na(match(text, poswords)))
std_neg <- sd(!is.na(match(text, negwords)))

text

#length
length(text)

#vis
wordcloud(text, min.freq=60, random.order = FALSE, backgroundColor = "green", color=rainbow(3))
