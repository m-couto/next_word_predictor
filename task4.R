# Task 4: Prediction Model
# 
# The goal of this exercise is to build and evaluate your first predictive model.
# You will use the n-gram and backoff models you built in previous tasks to build and
# evaluate your predictive model. The goal is to make the model efficient and accurate.
# 
# Tasks to accomplish
# 
# 1. Build a predictive model based on the previous data modeling steps - you may combine
# the models in any way you think is appropriate.
# 2. Evaluate the model for efficiency and accuracy - use timing software to evaluate the computational complexity of your model. Evaluate the model accuracy using different metrics like perplexity, accuracy at the first word, second word, and third word.
# 
# Questions to consider
# 
# 1. How does the model perform for different choices of the parameters and size of the model?
# 2. How much does the model slow down for the performance you gain?
# 3. Does perplexity correlate with the other measures of accuracy?
# 4. Can you reduce the size of the model (number of parameters) without reducing performance?


setwd("~/Desktop/Data Science course/10. Data Science Capstone/final/en_US")

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

rm(USblog, USnews, UStwit)



# splitting data into testing/train

set.seed(1234)
inTrain <- rbinom(length(data), 1, .3)
train <- data[inTrain==1]   # 30% data
testing <- data[inTrain==0] # 70% data

set.seed(3243)
trainsample <- sample(train, length(train)*3/10)  # 10% data
testsample <- sample(testing, length(testing)/10)   # 7% data



# tokenize
# (using fn from task1)

traintokens <- tokenize2(trainsample)
testtokens <- tokenize2(testsample)





# Accuracy in the training data



#############################################


# Benchmark:
# using benchmark.R file I got from Capstone Project github

df1 <- fread("~/Desktop/Data Science course/10. Data Science Capstone/ngrams/sorted_1_grams.csv",
             header=TRUE)

df2 <- fread("~/Desktop/Data Science course/10. Data Science Capstone/ngrams/sorted_2_grams.csv",
             header=TRUE)
df2 <- df2[df2$count>=2]

df3 <- fread("~/Desktop/Data Science course/10. Data Science Capstone/ngrams/sorted_3_grams.csv",
             header=TRUE)
df3 <- df3[df3$count>=2]

df4 <- fread("~/Desktop/Data Science course/10. Data Science Capstone/ngrams/sorted_4_grams.csv",
             header=TRUE)
df4 <- df4[df4$count>=2]

df5 <- fread("~/Desktop/Data Science course/10. Data Science Capstone/ngrams/sorted_5_grams.csv",
             header=TRUE)
df5 <- df5[df5$count>=2]

dflist <- list(df1, df2, df3, df4, df5)


preproc_bench <- function(input){
    # IMPORTANT: unlike task3, input can now be a character vector
    
    # convert/remove non-ASCII elements
    input <- iconv(input, from="UTF-8", to="ASCII", sub="")
    
    # convert to lowercase
    input <- tolower(input)
    
    # tokenize and remove punctuation, numbers, symbols, urls
    tokens <- tokens(input, remove_punct=TRUE, remove_numbers=TRUE,
                     remove_symbols=TRUE, remove_url=TRUE)
    lapply(tokens, function(x){ tail(x, 4) } )
}


BOprob <- function(words, string){
    # words: to be fitted after string
    # string: a preprocessed tokenized string of at most 4 words
    
    n = length(string)
    
    if (n==0){
        df <- dflist[[1]]
        N <- sum(df$count)      # total nr tokens
        
        sub <- data.frame(ngrams = words,
                          score = .4^4 * df$count[df$ngrams==words]/N)
        return( sub )
    }
    
    df <- dflist[[n]]
    dfnext <- dflist[[n+1]]
    
    # IMPORTANT: assuming the word/expression in string shows up in the ngrams files
    string <- paste(string, collapse=" ")
    stringcount <- df$count[df$ngrams == string]
    
    # index <- grep( paste("^", string, " ", sep=""), dfnext$ngrams )
    # sub <- data.frame( ngrams = dfnext$ngrams[index],
    #                    score = dfnext$count[index] / stringcount * 0.4^(4-n) )
    # return(sub)
    
    n_grams <- paste(string, words)
    sub <- dfnext %>% filter(ngrams %in% n_grams)
    sub$score <- sub$count / stringcount * 0.4^(4-n)
    return(sub[,c(1,3)])
}

# gives top 3 prediction words
predBO_bench <- function(input){
    # pre-processing
    input <- preproc_bench(input)
    
    for (string in input){
        n <- length(string)
        
        # vocabulary of all words in training data
        vocab <- dflist[[1]]$ngrams
        
        while (length(string)>0) {
            # load data frame of grams with size = size(string)
            ngramsdf <- dflist[[length(string)]]
            
            # if string is in this data frame
            if ( paste(string, collapse=" ") %in% ngramsdf$ngrams ){
                
                # we calculate the expressions with top scores
                sub <- BOprob(vocab, string)
                
                if (nrow(sub)>3){
                    res <- strsplit(as.character(sub$ngrams[1:3]), split=" ")
                    return( sapply(res, function(x){ tail(x,1) } ) )
                }
            }
            
            # if it is not in the dataframe and string has length 1,
            # we leave the loop
            if (length(string)==1){
                break
            }
            
            # if we still don't have a top next prediction
            # we chop off the string's last word
            string <- string[2:length(string)]
        }
        
        # we reach this step if we couldn't match a word after the string
        # in which case we suggest the single words in vocab with top score
        sub <- arrange( BOprob(vocab,c()) , desc(score))
        res <- strsplit(as.character(sub$ngrams[1:3]), split=" ")
        return( sapply(res, function(x){ tail(x,1) } )  )
    }
}


MLprob_faster <- function(words, string){
    # words: to be fitted after string
    # string: a preprocessed tokenized string of at most 4 words
    
    n = length(string)
    
    if (n==0){
        df <- dflist[[1]]
        N <- sum(df$count)      # total nr tokens
        
        sub <- data.frame(ngrams=words, score=df$count[df$ngrams==words]/N)
        return( sub )
    }
    
    df <- dflist[[n]]
    dfnext <- dflist[[n+1]]
    
    # IMPORTANT: assuming the word/expression in string shows up in the ngrams files
    string <- paste(string, collapse=" ")
    stringcount <- df$count[df$ngrams == string]
    
    n_grams <- paste(string, words)
    sub <- dfnext %>% filter(ngrams %in% n_grams)
    sub$score <- sub$count / stringcount
    return(sub[,c(1,3)])
}

predML_bench <- function(input, k=5){
    # pre-processing
    input <- preproc(input)
    
    for (string in input){
        # vocabulary of all words in training data
        vocab <- dflist[[1]]$ngrams
        
        while (length(string)>0) {
            # load data frame of grams with size = size(string)
            ngramsdf <- dflist[[length(string)]]
            
            # if string is in this data frame
            if ( paste(string, collapse=" ") %in% ngramsdf$ngrams ){
                # we calculate the words with top scores
                sub <- MLprob_faster(vocab, string)
                sub <- head( arrange(sub, desc(score)), k)
                res <- strsplit(as.character(sub$ngrams[1:3]), split=" ")
                return( sapply(res, function(x){ tail(x,1) } ) )
            }
            
            # if string has length 1, we leave the loop
            # otherwise, we decrease the string by 1 words
            ifelse( length(string)==1, break,
                    string <- string[2:length(string)] )
        }
        
        # we reach this step if we couldn't match a word after the string
        # in which case we suggest the single words in vocab with top score
        sub <- MLprob_faster(vocab, c())
        sub <- head( arrange(sub, desc(score)), k)
        res <- strsplit(as.character(sub$ngrams[1:3]), split=" ")
        return( sapply(res, function(x){ tail(x,1) } ) )
    }
}

# Using benchmark.R file, BO takes longer than ML but it's more accurate!

# DECISION: I will use BO algorithm!


