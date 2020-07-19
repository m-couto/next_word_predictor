
library(data.table)
library(dplyr)
library(quanteda)


# We only read the rows with counts of at least 2
# except the 1-grams or tokens (where we consider them all)
# Total memory: 387 MB
    # 12.20838 101.19293 144.73127  91.62938  37.63014

df1 <- fread("ngrams/sorted_1_grams.csv",
             header=TRUE)

df2 <- fread("ngrams/sorted_2_grams.csv",
             header=TRUE, nrow=1308428)
    # reduces from 4671556 to 1308428 rows

df3 <- fread("ngrams/sorted_3_grams.csv",
             header=TRUE, nrow=1762488)
    # reduces from 11431613 to 1762488 rows

df4 <- fread("ngrams/sorted_4_grams.csv",
             header=TRUE, nrow=1031341)
    # reduces from 14639142 to 1031341 rows

df5 <- fread("ngrams/sorted_5_grams.csv",
             header=TRUE, nrow=405641)
    # reduces from 14133760 to 405641 rows

dflist <- list(df1, df2, df3, df4, df5)

rm(df1, df2, df3, df4, df5)


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
    con <- file("swearWords.txt")
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


# probabilites with maximum likelihood
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

# top 5 next word predictions using max.lik.
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



library(shiny)

shinyServer(function(input, output) {
    
    # Maximum likelihood prediction table
    output$table1 <- renderTable({

        df1 <- pred_faster(input$string, input$slider)
        # to get only next word (rather than the whole expression)
        df1$ngrams <- sapply( strsplit(as.character(df1$ngrams), " "),
                              function(x){ tail(x,1) })

        df1
        
    },
    digits=8,
    # table title
    caption="Max. lik. prediction", caption.placement="top")
    
    
    # Stupid backoff prediction table
    output$table2 <- renderTable({
        
        df2 <- stupidBO(input$string, input$slider)
        # to get only next word (rather than the whole expression)
        df2$ngrams <- sapply( strsplit(as.character(df2$ngrams), " "),
                              function(x){ tail(x,1) })
        
        df2
    },
    digits = 8,
    # table title
    caption="Stupid backoff prediction", caption.placement="top")
    
    
    # Documentation output
    output$doc1 <- renderText({
        "Write some text in the empty box on the side panel and press the Submit button
        or press ENTER in your keyboard. You can also choose the number of next word
        predictions in the slider, the default being 5."
    })
    
    output$doc2 <- renderText({
        "The app returns two tables of predictions for the next words, as well as the
        score of that word, ie. the probability that that is the correct word.
        Each table uses a different statistical model for obtaining the prediction,
        the first uses the maximum likelihood model, the second the stupid backoff model."
    })
})
