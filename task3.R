# Task 3: Modelling

# The goal here is to build your first simple model for the relationship between words.
# This is the first step in building a predictive text mining application.
# You will explore simple models and discover more complicated modeling techniques.
# 
# Tasks to accomplish
# 
# Build basic n-gram model - using the exploratory analysis you performed, build a
# basic n-gram model for predicting the next word based on the previous 1, 2, or 3 words.
# Build a model to handle unseen n-grams - in some cases people will want to type a
# combination of words that does not appear in the corpora. Build a model to handle cases
# where a particular n-gram isn't observed.
# 
# Questions to consider
# 
# How can you efficiently store an n-gram model (think Markov Chains)?
# How can you use the knowledge about word frequencies to make your model smaller and
#     more efficient?
# How many parameters do you need (i.e. how big is n in your n-gram model)?
# Can you think of simple ways to "smooth" the probabilities (think about giving all
#     n-grams a non-zero probability even if they aren't observed in the data) ?
# How do you evaluate whether your model is any good?
# How can you use backoff models to estimate the probability of unobserved n-grams?
# 
# Hints, tips, and tricks
# 
# As you develop your prediction model, two key aspects that you will have to keep
# in mind are the size and runtime of the algorithm. These are defined as:
#     
# Size: the amount of memory (physical RAM) required to run the model in R
# Runtime: The amount of time the algorithm takes to make a prediction given the acceptable
# input
# 
# Your goal for this prediction model is to minimize both the size and runtime of the model
# in order to provide a reasonable experience to the user.
# 
# Keep in mind that currently available predictive text models can run on mobile phones,
# which typically have limited memory and processing power compared to desktop computers.
# Therefore, you should consider very carefully (1) how much memory is being used by
# the objects in your workspace; and (2) how much time it is taking to run your model.
# Ultimately, your model will need to run in a Shiny app that runs on the shinyapps.io server.
# 
# Tips, tricks, and hints
# 
# Here are a few tools that may be of use to you as you work on their algorithm:
#     
# object.size(): this function reports the number of bytes that an R object occupies
#     in memory
# Rprof(): this function runs the profiler in R that can be used to determine where
#     bottlenecks in your function may exist. The profr package (available on CRAN)
#     provides some additional tools for visualizing and summarizing profiling data.
# gc(): this function runs the garbage collector to retrieve unused RAM for R.
#     In the process it tells you how much memory is currently being used by R.
#     
# There will likely be a tradeoff that you have to make in between size and runtime.
# For example, an algorithm that requires a lot of memory, may run faster, while a
# slower algorithm may require less memory. You will have to find the right balance
# between the two in order to provide a good experience to the user.



library(data.table)
library(dplyr)
library(quanteda)


# Pre-processing and tokenization:

preproc <- function(string){
    # get last 4 tokens in string
    
    # convert/remove non-ASCII elements
    string <- iconv(string, from="UTF-8", to="ASCII", sub="")
    
    # convert to lowercase
    string <- tolower(string)
    
    # tokenize and remove punctuation, numbers, symbols, urls
    tokens <- tokens(string, remove_punct=TRUE, remove_numbers=TRUE,
                     remove_symbols=TRUE, remove_url=TRUE)
    tokens <- tokens[[1]]
    
    # profanity filtering: remove sentences with profanity
    con <- file("~/Desktop/Data Science course/10. Data Science Capstone/Tasks/swearWords.txt")
    badwords <- readLines(con, warn=F)
    close(con)

    badwords <- paste(badwords, collapse="|")
    badwords <- paste("^(", badwords, ")$", sep="")
    if (length(grep(badwords, tokens)) != 0){
        stop("The text contains profanity")
        # string <- string[ - grep(paste(badwords,collapse="|"), string) ]
    }
    
    if (length(tokens)>4){
        tokens <- tokens[(length(tokens)-3) : length(tokens)]
    }
    tokens
}

# for (n in 1:5){
#     loc <- sprintf("~/Desktop/Data Science course/10. Data Science Capstone/ngrams/sorted_%d_grams.csv", n)
#     df <- fread(loc, header=TRUE)
#     
#     print( sprintf("sorted_%d_grams: %f MB", n,
#                    round(object.size(df)/10^6, 2) ) )
# }

# [1] "sorted_1_grams: 30.260000 MB"
# [1] "sorted_2_grams: 30.260000 MB"
# [1] "sorted_3_grams: 977.980000 MB"
# [1] "sorted_4_grams: 1335.660000 MB"
# [1] "sorted_5_grams: 1336.660000 MB"




# MLE for probabilities of words after an empty string or after another word
MLprob <- function(words, string){
    # words: to be fitted after string
    # string: a preprocessed tokenized string of at most 4 words
    
    n = length(string)
    
    if (n==0){
        # df <- fread("~/Desktop/Data Science course/10. Data Science Capstone/ngrams/sorted_1_grams.csv",
        #             header=TRUE)
        
        N <- sum(df$count)      # total nr tokens
        ifelse( any(df$ngrams==words),
                return(as.numeric( df[ngrams==words, 2]/N )),
                return(0) )
    }
    
    locations <- sprintf("~/Desktop/Data Science course/10. Data Science Capstone/ngrams/sorted_%d_grams.csv",
                         c(n, n+1))
    df <- fread(locations[1], header=TRUE)
    dfnext <- fread(locations[2], header=TRUE)
    
    # IMPORTANT: assuming the word/expression in string shows up in the ngrams files
    string <- paste(string, collapse=" ")
    stringcount <- as.numeric( df[df$ngrams == string, 2] )
    
    n_grams <- paste(string, words)
    sub <- dfnext %>% filter(ngrams %in% n_grams)
    sub$prob <- sub$count / stringcount
    return(sub)
}

# else{ if (n==1) {
#     df <- fread("~/Desktop/Data Science course/10. Data Science Capstone/ngrams/sorted_1_grams.csv",
#                 header=TRUE)
#     df2 <- fread("~/Desktop/Data Science course/10. Data Science Capstone/ngrams/sorted_2_grams.csv",
#                  header=TRUE)
#     
#     # IMPORTANT: assuming the word in string shows up in the vocabulary
#     stringcount <- as.numeric( df[df$ngrams == string, 2] )
#     
#     bigrams <- paste(string, words)
#     sub <- df2 %>% filter(ngrams %in% bigrams)
#     sub$prob <- sub$count / stringcount
#     return(sub)
# }
#     
# else{ if(n==2) {
#     df2 <- fread("~/Desktop/Data Science course/10. Data Science Capstone/ngrams/sorted_2_grams.csv",
#                  header=TRUE)
#     df3 <- fread("~/Desktop/Data Science course/10. Data Science Capstone/ngrams/sorted_3_grams.csv",
#                  header=TRUE)
#     
#     # IMPORTANT: assuming the expression in string shows up in the 2-grams file
#     string <- paste(string, collapse=" ")
#     stringcount <- as.numeric( df2[df2$ngrams == string, 2] )
#     
#     trigrams <- paste(string, words)
#     sub <- df3 %>% filter(ngrams %in% trigrams)
#     sub$prob <- sub$count / stringcount
#     return(sub)
# }
# 
# else{ if(n==3){
#     df3 <- fread("~/Desktop/Data Science course/10. Data Science Capstone/ngrams/sorted_3_grams.csv",
#                  header=TRUE)
#     df4 <- fread("~/Desktop/Data Science course/10. Data Science Capstone/ngrams/sorted_4_grams.csv",
#                  header=TRUE)
#     
#     # IMPORTANT: assuming the expression in string shows up in the 3-grams file
#     string <- paste(string, collapse=" ")
#     stringcount <- as.numeric( df3[df3$ngrams == string, 2] )
#     
#     fourgrams <- paste(string, words)
#     sub <- df4 %>% filter(ngrams %in% fourgrams)
#     sub$prob <- sub$count / stringcount
#     return(sub)
# }
# 
# else{ if(n==4){
#     df4 <- fread("~/Desktop/Data Science course/10. Data Science Capstone/ngrams/sorted_4_grams.csv",
#                  header=TRUE)
#     df5 <- fread("~/Desktop/Data Science course/10. Data Science Capstone/ngrams/sorted_5_grams.csv",
#                  header=TRUE)
#     
#     # IMPORTANT: assuming the expression in string shows up in the 4-grams file
#     string <- paste(string, collapse=" ")
#     stringcount <- as.numeric( df4[df4$ngrams == string, 2] )
#     
#     fivegrams <- paste(string, words)
#     sub <- df5 %>% filter(ngrams %in% fivegrams)
#     sub$prob <- sub$count / stringcount
#     return(sub)
#     
# } } } } }



# returns top k predictions for word after string
# IMPORTANT: Assumes the string appears in the ngrams files
toppred <- function(string, k=5){
    # pre-processing
    string <- preproc(string)
    
    df <- fread("~/Desktop/Data Science course/10. Data Science Capstone/ngrams/sorted_1_grams.csv",
                header=TRUE)
    vocab <- df$ngrams
    
    sub <- MLprob(vocab, string)
    head( arrange(sub, desc(prob)), k)
}


# Top 5 predictions for word after "the ..."
toppred("the")

# Top 5 predictions for word after "of the ..."
toppred("of the")

# Top 5 predictions for word after "in front of ..."
toppred("in front of")

# Top 5 predictions for word after "at the end of ..."
toppred("at the end of")



# Prediction model

pred <- function(string, k=5){
    # pre-processing
    string <- preproc(string)
    
    wordsdf <- fread("~/Desktop/Data Science course/10. Data Science Capstone/ngrams/sorted_1_grams.csv",
                header=TRUE)
    vocab <- wordsdf$ngrams
    
    while (length(string)>0) {
        loc <- sprintf("~/Desktop/Data Science course/10. Data Science Capstone/ngrams/sorted_%d_grams.csv",
                       length(string))
        ngramsdf <- fread(loc, header=TRUE)
        
        if ( paste(string, collapse=" ") %in% ngramsdf$ngrams){
            sub <- MLprob(vocab, string)
            return( head( arrange(sub, desc(prob)), k) )
        }
        else{
            ifelse( length(string)==1, break,
                    string <- string[2:length(string)] )
        }
    }
    
    sub <- MLprob(vocab, c())
    return( head( arrange(sub, desc(prob)), k) )
}

# Top 5 predictions for word after "the ..."
pred("the")

# Top 5 predictions for word after "of the ..."
pred("of the")

# Top 5 predictions for word after "in front of ..."
pred("in front of")

# Top 5 predictions for word after "at the end of ..."
pred("at the end of")



pred("indescribeable of the")
pred("böfsö dslkbfabaslf of the")

# The algorithm tries these expressions but eventually
# decreases them until only "of the", and then gives the predictions.





# REDUCING RUNTIME

# Here I find all ngrams starting with string, rather than trying all words
# after it as I do in toppred2
toppred2 <- function(string, k=5){
    # pre-processing
    string <- preproc(string)
    n <- length(string)
    
    loc <- sprintf("~/Desktop/Data Science course/10. Data Science Capstone/ngrams/sorted_%d_grams.csv", n+1)
    df <- fread(loc, header=TRUE)
    
    # IMPORTANT: assuming the word/expression in string shows up in the ngrams files
    string <- paste(string, collapse=" ")
    index <- grep(paste("^", string, " ", sep=""), df$ngrams)
    
    loc2 <- sprintf("~/Desktop/Data Science course/10. Data Science Capstone/ngrams/sorted_%d_grams.csv", n)
    df2 <- fread(loc2, header=TRUE)
    
    scores <- df$count[index] / df2$count[df2$ngrams==string]
    scoresdf <- data.frame(ngrams = df$ngrams[index], scores = scores)
    head( arrange(scoresdf, desc(scores)), k)
}

toppred2("the")
toppred2("of the")
toppred2("in front of")
toppred2("at the end of")

# Conclusion: no significance better performance!



# memory <- rep(0, 5)
# for (n in 1:5){
#     loc <- sprintf("~/Desktop/Data Science course/10. Data Science Capstone/ngrams/sorted_%d_grams.csv", n)
#     df <- fread(loc, header = TRUE)
#     df <- df[df$count >= 2]
#     memory[n] <- object.size(df)/10^6
# }
# memory
# Total memory was:
# count>=1: 4050 MB (30.25606  369.24243  977.97661 1335.66500 1336.65919)
# count>=2: 387 MB
# count>=3: 188 MB
# count>=4: 125 MB
# count>=5: 93 MB

# Decision: Focus on counts of at least 2, except for 1_grams where I keep them all.
# Total memory: 405 MB

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



# MODEL: prediction using maximum likelihood estimators

MLprob_faster <- function(words, string){
    # words: to be fitted after string
    # string: a preprocessed tokenized string of at most 4 words
    
    n <- length(string)
    
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

# Top predictions for next word
# PS. still assumer string is in ngrams files
toppred_faster <- function(string, k=5){
    # pre-processing
    string <- preproc(string)
    
    df <- dflist[[1]]
    vocab <- df$ngrams
    
    sub <- MLprob_faster(vocab, string)
    head( arrange(sub, desc(score)), k)
}

toppred_faster("the")
toppred_faster("of the")
toppred_faster("in front of")
toppred_faster("at the end of")


# top predictions for next word
pred_faster <- function(string, k=5){
    # pre-processing
    string <- preproc(string)
    
    # vocabulary of all words in training data
    vocab <- dflist[[1]]$ngrams
    
    # initialize result
    res <- data.frame(ngrams=character(), score=c())
    reswords <- c()     # candidates for next word
    
    while (length(string)>0) {
        # load data frame of grams with size = size(string)
        # if string is in this data frame
        if ( paste(string, collapse=" ") %in% dflist[[length(string)]]$ngrams ){
            
            # we calculate the words with top scores
            sub <- MLprob_faster(vocab, string)
            
            # get last word in each expression
            subwords <- sapply(strsplit( as.character(sub$ngrams), split=" "),
                               function(x){ x[length(x)] })
            
            # add to results expressions that end in a word not in res yet
            index <- which( !(subwords %in% reswords) )
            reswords <- c(reswords, subwords[index])
            res <- rbind(res, sub[index,])
            
            # if we got the top k, we output it
            if (nrow(res)>=k){
                return( head( arrange(res, desc(score)), k) )
            }
            
        }
        
        # if it is not in the dataframe
        # if string has length 1, we leave the loop
        # otherwise, we decrease the string by 1 words
        ifelse( length(string)==1, break,
                string <- string[2:length(string)] )
    }
    
    # we reach this step if we couldn't match a word after the string
    # in which case we suggest the single words in vocab with top score
    res <- rbind(res, MLprob_faster(vocab, c()))
    return( head( arrange(res, desc(score)) , k) )
}

pred_faster("the")
pred_faster("of the")
pred_faster("in front of")
pred_faster("at the end of")
pred_faster("indescribeable of the")
pred_faster("böfsö dslkbfabaslf of the")
pred_faster("böfsö dslkbfabaslf")





# MODEL: Prediction with stupid backoff

# probabilites with stupid backoff
BOprob <- function(words, string){
    # words: to be fitted after string
    # string: a preprocessed tokenized string of at most 4 words
    
    n <- length(string)
    
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

# top 5 next word predictions using stupid backoff
stupidBO <- function(string, k=5){
    # pre-processing
    string <- preproc(string)
    
    # vocabulary of all words in training data
    vocab <- dflist[[1]]$ngrams
    
    # initialize result
    res <- data.frame(ngrams=character(), score=c())
    reswords <- c()     # candidates for next word
    
    while (length(string)>0) {
        
        # load data frame of grams with size = size(string)
        # if string is in this data frame
        if ( paste(string, collapse=" ") %in% dflist[[length(string)]]$ngrams ){
            
            # we calculate the expressions with top scores
            sub <- BOprob(vocab, string)
            
            # get last word in each expression
            subwords <- sapply(strsplit( as.character(sub$ngrams), split=" "),
                               function(x){ x[length(x)] })
            
            # add to results expressions that end in a word not in res yet
            index <- which( !(subwords %in% reswords) )
            reswords <- c(reswords, subwords[index])
            res <- rbind(res, sub[index,])
            
            # if we got the top k, we output it
            if (nrow(res)>=k){
                return( head( arrange(res, desc(score)), k) )
            }
        }
        
        # if it is not in the dataframe and string has length 1,
        # we leave the loop
        # if string was in the data frame and we still don't have top k
        # or if it wasn't in the data frame but has length>=2,
        # we chop off the string's last word
        ifelse( length(string)==1, break,
                string <- string[2:length(string)] )
    }
    
    # we reach this step if we couldn't match a word after the string
    # in which case we suggest the single words in vocab with top score
    res <- rbind(res, BOprob(vocab, c()))
    return( head( arrange(res, desc(score)) , k) )
}

stupidBO("the")
stupidBO("of the")
stupidBO("in front of")
stupidBO("at the end of")
stupidBO("indescribeable of the")
stupidBO("böfsö dslkbfabaslf of the")
stupidBO("böfsö dslkbfabaslf")

# Note the difference in scores between:
pred_faster("indescribeable of the")
stupidBO("indescribeable of the")

# An example of not getting top 5 immediately
stupidBO("and where have you")


# top next word prediction with stupid backoff
pred_stupidBO <- function(string){
    # pre-processing
    string <- preproc(string)
    n <- length(string)
    
    # vocabulary of all words in training data
    vocab <- dflist[[1]]$ngrams
    
    while (length(string)>0) {
        
        # load data frame of grams with size = size(string)
        # if string is in this data frame
        if ( paste(string, collapse=" ") %in% dflist[[length(string)]]$ngrams ){
            
            # we calculate the expressions with top scores
            sub <- BOprob(vocab, string)
            
            if (nrow(sub)>0){
                return( sub[1,] )
            }
        }
        
        # if it is not in the dataframe and string has length 1,
        # we leave the loop
        # if we still don't have a top next prediction
        # we chop off the string's last word
        ifelse (length(string)==1, break,
                string <- string[2:length(string)] )
    }
    
    # we reach this step if we couldn't match a word after the string
    # in which case we suggest the single words in vocab with top score
    return( arrange( BOprob(vocab,c()) , desc(score))[1,] )
}

stupidBO("the")
pred_stupidBO("the")

stupidBO("of the")
pred_stupidBO("of the")

stupidBO("in front of")
pred_stupidBO("in front of")

stupidBO("at the end of")
pred_stupidBO("at the end of")

stupidBO("indescribeable of the")
pred_stupidBO("indescribeable of the")

stupidBO("böfsö dslkbfabaslf of the")
pred_stupidBO("böfsö dslkbfabaslf of the")

stupidBO("böfsö dslkbfabaslf")
pred_stupidBO("böfsö dslkbfabaslf")




# obtain next word instead of the whole expression
# string can now be a character vector (rather than a single string)
predword_stupidBO <- function(string){
    pred <- pred_stupidBO(string)$ngrams
    return( tail( strsplit(pred, split=" ")[[1]], 1) )
}









# I don't think I'll use this:
# Good-Turing count
GTcount <- function(string){
    loc <- sprintf("~/Desktop/Data Science course/10. Data Science Capstone/ngrams/sorted_1_grams.csv")
    df <- fread(loc, header=TRUE)
    
    vocab <- df$ngrams
    
    if (!(word %in% vocab)){
        return( sum(df$count==1) )
    }
    else{
        c <- df$count[vocab==word]
        return( (c+1) * sum( df$count==(c+1) ) / sum( df$count==c ) )
    }
}


