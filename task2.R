# Taks 2: Exploratory Data Analysis


# The first step in building a predictive model for text is understanding the
# distribution and relationship between the words, tokens, and phrases in the text.
# The goal of this task is to understand the basic relationships you observe in
# the data and prepare to build your first linguistic models.


# Tasks to accomplish

# 1. Exploratory analysis - perform a thorough exploratory analysis of the data,
# understanding the distribution of words and relationship between the words in the corpora.
# 
# 2. Understand frequencies of words and word pairs - build figures and tables to
# understand variation in the frequencies of words and word pairs in the data.


# Questions to consider

# 1. Some words are more frequent than others - what are the distributions of word 
    # frequencies?
# 2. What are the frequencies of 2-grams and 3-grams in the dataset?
# 3. How many unique words do you need in a frequency sorted dictionary to cover 50%
    # of all word instances in the language? 90%?
# 4. How do you evaluate how many of the words come from foreign languages?
# 5. Can you think of a way to increase the coverage -- identifying words that may
    # not be in the corpora or using a smaller number of words in the dictionary
    # to cover the same number of phrases?



# Number of tokens/types

ntoken(blogtokens)          # per text
sum(ntoken(blogtokens))     # in the file
    # over 7million

ntype(blogtokens)           # per text

sum(ntype(blogtokens))      # in the file
    # over 5million
# NOTE: these are not the nr of unique words in file because
# it counts uniqueness per text in file!!!!
# To get this number, look at nr features in
dfm(blogtokens)



# How many unique words do you need in a frequency sorted dictionary to cover
# p% of all word instances in the language?
cover <- function(p){
    # matrix of frequency of words in each text
    dfmat <- dfm(blogtokens)
    
    # frequency sorted dictionary
    ordered <- topfeatures(dfmat, 151499)
        # nr unique words = nr feature in dfmat = 151499
    
    # cumulative frequency
    total <- sum(ordered)
    cumfreq <- cumsum(ordered) / total
    
    # nr unique words in sorted 
    sum(cumfreq <= p/100)
}

cover(50)
# 106 unique words from a freq-sorted dictionary to cover 50% of all words
cover(90)
# 6839 unique words from a freq-sorted dictionary to cover 50% of all words

# ATTENTION: once again, these values are for 10% of US_blog file.




# Creating a document-feature matrix
# Useful for finding most frequent tokens in blogs

dfmat_blog <- dfm(train,
                  remove = badwords,
                  remove_punct=TRUE, remove_numbers=TRUE)
# ATTENTION: there is the option of stemming here! stem=TRUE

dfmat_blog[, 1:10]


# wordcloud
set.seed(100)
textplot_wordcloud(dfmat_blog, min_count = 1000,
                   random_order = FALSE,
                   color = RColorBrewer::brewer.pal(8, "Dark2"))
    # min_count = minimum frequency of words in wordcloud




# most frequent words
top <- topfeatures(dfmat_blog, 40)
    # class: numeric vector with labelled entries by words


# plotting frequency of most frequent words
df <- data.frame(words = names(top), freq = unname(top))
df$words <- factor(df$words, levels=rev(names(top)) )
# we reverse order in factor levels to get horizontal barplot
# with longest bar on top

library(ggplot2)
ggplot(df, aes(x=words, y=freq)) +
    geom_bar(stat="identity", color="black", fill="red2") +
    coord_flip()

# theme(axis.text.x = element_text(angle=90))

barplot(df$freq ~ df$words, xlab="", ylab="Frequency",
        las=2)




# Most frequent 2-grams
twogram <- tokens_ngrams(blogtokens, n=2, concatenator = " ")
dfm_twogram <- dfm(twogram)
top_twogram <- topfeatures(dfm_twogram, n=50)


# Plotting
df2 <- data.frame(words=names(top_twogram), freq=unname(top_twogram))
df2$words <- factor(df2$words, levels=rev(names(top_twogram)))

ggplot(df2, aes(x=words, y=freq)) + 
    geom_bar(stat="identity", color="black", fill="red2") +
    coord_flip()

barplot(df2$freq ~ df2$words,
        xlab="", ylab="Frequency",
        las=2)



# Most frequent 3-grams
threegram <- tokens_ngrams(blogtokens, n=3, concatenator = " ")
dfm3 <- dfm(threegram)
top_tthreegram <- topfeatures(dfm(threegram), n=50)

sum(dfm3[, "vested interests vested"])
    # 251
index <- dfm3[, "vested interests vested"]
index <- as.numeric(index)

set.seed(1234)
inTrain <- rbinom(length(USblog), 1, .1)
train <- data[inTrain==1]
train[index!=0]
    # this text contains "Vested interests." repeated 251 times
    # this explains the appearance of "vested interests vested" in the
        # most frequent 5-grams plot




# sorted dfm by feature frequency (in decreasing order)
dfmat_sorted <- dfm_sort(dfm(blogtokens), margin="features")




# Generalization

# building ngrams

Sys.time()

twograms <- tokens_ngrams(alltokens, 2, concatenator = " ")
object.size(twograms)

Sys.time()

threegrams <- tokens_ngrams(alltokens, 3, concatenator = " ")
object.size(threegrams)

Sys.time()

fourgrams <- tokens_ngrams(alltokens, 4, concatenator = " ")
object.size(fourgrams)

Sys.time()

fivegrams <- tokens_ngrams(alltokens, 5, concatenator = " ")
object.size(fivegrams)

Sys.time()



# creating and sorting ngrams in decreasing order of frequency
# and writing them into csv files
library(data.table)
sortedngrams <- function(token, n){
    # tokens: tokens of the texts in corpus
        # PS. use tokenize from task1 to get tokens of 
    # n: size of n-grams
    
    # get a sorted vector of frequency of words
    if (n==1){
        sorted <- dfm_sort(dfm(token), margin="features")
    }
    else{
        ngrams <- tokens_ngrams(token, n, concatenator = " ")
        sorted <- dfm_sort(dfm(ngrams), margin="features")
    }
    freqvec <- colSums(sorted)
    
    # into dataframe
    df <- data.frame(ngrams=names(freqvec), count=unname(freqvec))
    # ADDED:
    print(object.size(df))
    
    # write into file
    location <- sprintf("~/Desktop/Data Science course/10. Data Science Capstone/ngrams/sorted_%d_grams.csv", n)
    fwrite(df, file = location)
}
# Note: fwrite fn from library(data.table) is faster than write.csv!


# Using 30% data

system.time( sortedngrams(alltokens, 1) )
# df size: 33.5 MB
# user  system elapsed 
# 11.545   1.544  14.078 

system.time( sortedngrams(alltokens, 2) )
# 406 MB
# user  system elapsed 
# 116.368  17.677 126.910 

system.time( sortedngrams(alltokens, 3) )
# 1.07 GB
# user  system elapsed 
# 272.180  39.192 330.912 

system.time( sortedngrams(alltokens, 4) )
# 1.45 GB
# user  system elapsed 
# 393.759 104.365 566.300 

system.time( sortedngrams(alltokens, 5) )
# df size: 1.45 MB
# user  system elapsed 
# 119.426   2.926 115.719 



# system.time( sortedngrams(blogtokens, 1) )
# # user  system elapsed 
# # 1.657   0.081   1.745 
# 
# system.time( sortedngrams(blogtokens, 2) )
# # user  system elapsed 
# # 16.390   0.258  15.239 
# 
# system.time( sortedngrams(blogtokens, 3) )
# # user  system elapsed 
# # 38.759   0.531  37.341 
# 
# system.time( sortedngrams(blogtokens, 4) )
# # user  system elapsed 
# # 44.481   0.573  42.784 
# 
# system.time( sortedngrams(blogtokens, 5) )
# # user  system elapsed 
# # 45.954   0.732  44.326 



# These files have many ngrams with very few frequency.
# What happens if we only consider ngrams with at least 5 entries.

for (n in 1:5){
    loc <- sprintf("~/Desktop/Data Science course/10. Data Science Capstone/ngrams/sorted_%d_grams.csv",
                   n)
    df <- read.csv(loc, header=TRUE)
    print( sprintf("sorted_%d_grams data frame decreases %f %%", n, 100-sum(df$count>5)/nrow(df)*100) )
}

library(data.table)
    # contains fread function for reading faster
    # another alternative is read_csv from library(readr)
res <- sapply(1:5, function(n){
    loc <- sprintf("~/Desktop/Data Science course/10. Data Science Capstone/ngrams/sorted_%d_grams.csv",
                   n)
    df <- fread(loc, header=TRUE)
    c(nrow(df), sum(df$count>5))
})

saved <- data.frame(Before = res[1,], After = res[2,],
                    Percentage = 100 - res[2,] / res[1,] * 100 )


# [1] "sorted_1_grams data frame decreases 80.771938 %"
# [1] "sorted_2_grams data frame decreases 92.924114 %"
# [1] "sorted_3_grams data frame decreases 97.770992 %"
# [1] "sorted_4_grams data frame decreases 99.498759 %"
# [1] "sorted_5_grams data frame decreases 99.909316 %"




# barplot of k most frequent n-grams in token
library(ggplot2)

ngramsplot <- function(token, n, k=length(token)){
    # tokens: tokens of the texts in corpus
        # PS. use tokenize from task1 to get tokens of 
    # n: size of n-grams
    # k: number of most freq n-grams
    
    # calculate the ngrams of each text in corpus
    ngrams <- tokens_ngrams(token, n, concatenator = " ")
    
    # k most frequent ngrams
    top_ngrams <- topfeatures(dfm(ngrams), k)
    
    # put into dataframe
    df <- data.frame(words=names(top_ngrams), count=unname(top_ngrams))
    df$words <- factor(df$words,
                       levels=rev(names(top_ngrams)))
    print(df)
    
    # title and x-label
    plottitle <- sprintf("Frequency of %d-grams", n)
    xlabel <- sprintf("%d-grams", n)
    
    # plot
    ggplot(df, aes(x=words, y=count)) +
        geom_bar(stat="identity", color="black", fill="red2") +
        coord_flip() +
        labs(title = plottitle, x=xlabel, y="count")
}

ngramsplot(alltokens, 1, 20)
ngramsplot(alltokens, 2, 20)
ngramsplot(alltokens, 3, 20)
ngramsplot(alltokens, 4, 20)
ngramsplot(alltokens, 5, 20)


system.time( ngramsplot(blogtokens, 1, 20) )
# user  system elapsed 
# 0.846   0.087   0.955 

system.time( ngramsplot(blogtokens, 2, 20) )
# user  system elapsed 
# 6.270   0.130   4.954 

system.time( ngramsplot(blogtokens, 3, 20) )
# user  system elapsed 
# 11.546   0.216   9.697 

system.time( ngramsplot(blogtokens, 4, 20) )
# user  system elapsed 
# 13.706   0.283  11.574 

system.time( ngramsplot(blogtokens, 5, 20) )
# user  system elapsed 
# 15.474   0.325  13.150 



ngramsplot(alltokens, 1, 20)
# user  system elapsed 
# 2.276   0.125   2.411 

ngramsplot(alltokens, 2, 20)
# user  system elapsed 
# 18.103   0.318  14.490 

ngramsplot(alltokens, 3, 20)
# user  system elapsed 
# 30.890   0.632  25.880 

ngramsplot(alltokens, 4, 20)
# user  system elapsed 
# 38.541   1.732  34.128 

ngramsplot(alltokens, 5, 20)
# user  system elapsed 
# 41.509   1.561  36.124 





# top 2-grams of the form "I ..."
example <- tokens_select(twogram,
                         pattern = "^i ", valuetype = "regex",
                         selection = "keep")
# NOTE: use valuetype to specify regular expression

top_example <- topfeatures(dfm(example), n=10)
top_example





# Pruning:


