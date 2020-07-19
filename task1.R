# Task 1: Getting and cleaning the data

# The goal of this task is to get familiar with the databases and do the necessary cleaning.
# After this exercise, you should understand what real data looks like and how much effort
# you need to put into cleaning the data. When you commence on developing a new language,
# the first thing is to understand the language and its peculiarities with respect to your
# target. You can learn to read, speak and write the language. Alternatively, you can study
# data and learn from existing information about the language through literature and the
# internet. At the very least, you need to understand how the language is written: writing
# script, existing input methods, some phonetic knowledge, etc.


# Tasks to accomplish
# 
# Tokenization - identifying appropriate tokens such as words, punctuation, and numbers.
    # Writing a function that takes a file as input and returns a tokenized version of it.
#
# Profanity filtering - removing profanity and other words you do not want to predict.


# Tips, tricks, and hints
# 
# Loading the data in. This dataset is fairly large. We emphasize that you don't necessarily
# need to load the entire dataset in to build your algorithms (see point 2 below). At least
# initially, you might want to use a smaller subset of the data. Reading in chunks or lines
# using R's readLines or scan functions can be useful. You can also loop over each line of
# text by embedding readLines within a for/while loop, but this may be slower than reading
# in large chunks at a time. Reading pieces of the file at a time will require the use of
# a file connection in R.

# Sampling. To reiterate, to build models you don't need to load in and use all of the data.
# Often relatively few randomly selected rows or chunks need to be included to get an
# accurate approximation to results that would be obtained using all the data. Remember
# your inference class and how a representative sample can be used to infer facts about
# a population. You might want to create a separate sub-sample dataset by reading in a
# random subset of the original data and writing it out to a separate file. That way, you
# can store the sample and not have to recreate it every time. You can use the rbinom
# function to "flip a biased coin" to determine whether you sample a line of text or not.



setwd("~/Desktop/Data Science course/10. Data Science Capstone/final/en_US")


# Sampling
# Get a training set from 50% of the data

set.seed(1234)
system.time(
    inTrain <- as.logical( rbinom(length(USblog), 1, .1) )
)
train <- USblog[inTrain]




# Question: which text analysis R package to use?

# Deciding on which text analysis package to use:
# https://github.com/lgreski/datasciencectacontent/blob/master/markdown/capstone-choosingATextPackage.md

library(tm)

# 3 tokenizer functions
Boost_tokenizer(train[1])

MC_tokenizer(train[1])
# removes punctuation

scan_tokenizer(train[1])

# system.time(
#     blogwords <- MC_tokenizer(train)
# )
# user  system elapsed 
# 127.552   1.679 131.909 


library(quanteda)

system.time(
blogtokens <- tokens(train, remove_punct=T, remove_numbers=T)
)
# user  system elapsed 
# 96.151   2.283  92.304 

# DECISION: use quanteda package



# Creating a Corpus

blogcorpus <- corpus(train)
summary(blogcorpus)
    # Types: nr distinct tokens
    # Tokens: nr tokens
    # Sentences: nr sentences



# uppercase / lowercase

# DECISION: turn to lowercase

train <- tolower(train)




# Tokenization

blogtokens <- tokens(train, remove_punct=T, remove_numbers=T)

# Since we want to predict next word in a text, neither punctuation, symbols nor numbers are
# helpful. Note that removing punctuation does not remove apostrophes from contractions,
# ie. it leaves isn't, it's untouched.




# Profanity filtering
# found list in: http://www.bannedwordlist.com/

con <- url("http://www.bannedwordlist.com/lists/swearWords.txt")
badwords <- readLines(con, warn=F)
close(con)

# removing bad words
blogtokens <- tokens_remove(blogtokens, badwords)



# Pre-processing and tokenization
# Takes a file as input and returns a tokenized version of it.

setwd("~/Desktop/Data Science course/10. Data Science Capstone/final/en_US")
library(quanteda)

tokenize <- function(filename=NULL, p=.6){
    # filename: name of a text file
    # p: percentage of original data in training data
    # 60% by default
    
    if (!is.null(filename)){
        # reading the file
        con <- file(filename)
        data <- readLines(con, skipNul = TRUE)
        close(con)
    }
    else{
        con <- file("en_US.blogs.txt")
        USblog <- readLines(con)
        close(con)
        
        con <- file("en_US.news.txt")
        USnews <- readLines(con)
        close(con)
        
        con <- file("en_US.twitter.txt")
        UStwit <- readLines(con, skipNul = TRUE)
        close(con)
        
        data <- c(USblog, USnews, UStwit)
    }
    
    print("gathering all files")
    print(Sys.time())
    
    # sampling
    set.seed(1234)
    inTrain <- rbinom(length(data), 1, p)
    train <- data[inTrain==1]
    
    print("sampling")
    print(Sys.time())
    
    # convert/remove non-ASCII elements
    train <- iconv(train, from="UTF-8", to="ASCII", sub="")
    
    print("remove nonASCII:")
    print(Sys.time())
    
    # convert to lowercase
    train <- tolower(train)
    
    print("convert to lowercase")
    print(Sys.time())
    
    # split into sentences
    train <- unlist(strsplit(train, "(?<=[[:punct:]])\\s(?=[a-z])", perl=T))
    
    print("split into sentences")
    print(Sys.time())
    
    # profanity filtering: remove sentences with profanity
    con <- file("~/Desktop/Data Science course/10. Data Science Capstone/Tasks/swearWords.txt")
    badwords <- readLines(con, warn=F)
    close(con)
    train <- train[ - grep(paste(badwords,collapse="|"), train) ]
    
    print("profanity filtering")
    print(Sys.time())
    
    # tokenize and remove punctuation, numbers, symbols, urls
    tokens <- tokens(train, remove_punct=TRUE, remove_numbers=TRUE,
                     remove_symbols=TRUE, remove_url=TRUE)
    tokens
}

Sys.time()

alltokens <- tokenize(p=.3)
# user  system elapsed 
# 110.220   3.134 111.025 
object.size(alltokens)
# 1.25 GB

Sys.time()





system.time( blogtokens <- tokenize("en_US.blogs.txt", .1) )
# user  system elapsed 
# 33.813   0.476  33.541 

system.time( newstokens <- tokenize("en_US.news.txt", .1) )
# user  system elapsed 
# 33.251   0.426  32.862 

system.time( twittokens <- tokenize("en_US.twitter.txt", .1) )
# user  system elapsed 
# 32.055   0.461  31.900 





# ----------------------------------------

# For tokenizing training and testing data in task 4

library(quanteda)
tokenize2 <- function(train){
    
    # convert/remove non-ASCII elements
    train <- iconv(train, from="UTF-8", to="ASCII", sub="")
    
    print("remove nonASCII:")
    print(Sys.time())
    
    # convert to lowercase
    train <- tolower(train)
    
    print("convert to lowercase")
    print(Sys.time())
    
    # split into sentences
    train <- unlist(strsplit(train, "(?<=[[:punct:]])\\s(?=[a-z])", perl=T))
    
    print("split into sentences")
    print(Sys.time())
    
    # profanity filtering: remove sentences with profanity
    con <- file("~/Desktop/Data Science course/10. Data Science Capstone/Tasks/swearWords.txt")
    badwords <- readLines(con, warn=F)
    close(con)
    train <- train[ - grep(paste(badwords,collapse="|"), train) ]
    
    print("profanity filtering")
    print(Sys.time())
    
    # tokenize and remove punctuation, numbers, symbols, urls
    tokens(train, remove_punct=TRUE, remove_numbers=TRUE,
           remove_symbols=TRUE, remove_url=TRUE)
}

alltokens <- tokenize2(train)
object.size(alltokens)
# 2083386976 bytes = 2.1GB





# --------------------------------------

# Previous versions:
# this version splits into sentences and removes profanity in another way
# but it's slower

tokenize3 <- function(filename=NULL, p=.6){
    # filename: name of a text file
    # p: percentage of original data in training data
    # 60% by default
    
    if (!is.null(filename)){
        # reading the file
        con <- file(filename)
        data <- readLines(con, skipNul = TRUE)
        close(con)
    }
    else{
        con <- file("en_US.blogs.txt")
        USblog <- readLines(con)
        close(con)
        
        con <- file("en_US.news.txt")
        USnews <- readLines(con)
        close(con)
        
        con <- file("en_US.twitter.txt")
        UStwit <- readLines(con, skipNul = TRUE)
        close(con)
        
        data <- c(USblog, USnews, UStwit)
    }
    
    # sampling
    set.seed(1234)
    inTrain <- rbinom(length(data), 1, p)
    train <- data[inTrain==1]
    
    # convert/remove non-ASCII elements
    train <- iconv(train, from="UTF-8", to="ASCII", sub="")
    
    # convert to lowercase
    train <- tolower(train)
    
    print(Sys.time())
    
    # profanity filtering: remove sentences with profanity
    con <- file("~/Desktop/Data Science course/10. Data Science Capstone/swearWords.txt")
    badwords <- readLines(con, warn=F)
    close(con)
    regex <- paste(badwords,collapse="|")
    
    train <- char_trim(train, what="sentences",
                       exclude_pattern = regex)
    
    print("profanity filtering")
    print(Sys.time())
    
    # tokenize and remove punctuation, numbers, symbols, urls
    tokens <- tokens(train, remove_punct=TRUE, remove_numbers=TRUE,
                     remove_symbols=TRUE, remove_url=TRUE)
    tokens
}

system.time( alltokens3 <- tokenize3(p=.3) )
# user  system elapsed 
# 172.240   5.997 174.775 




# profanity filtering
# con <- url("http://www.bannedwordlist.com/lists/swearWords.txt")
# badwords <- readLines(con, warn=F)
# close(con)
# blogtokens <- tokens_remove(blogtokens, badwords)