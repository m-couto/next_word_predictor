# Task 0: Understanding the Problem

# Tasks to accomplish:
# 
# Obtaining the data - Can you download the data and load/manipulate it in R?
# Familiarizing yourself with NLP and text mining - Learn about the basics of natural
    # language processing and how it relates to the data science process you have learned
    # in the Data Science Specialization.


# Questions to consider
# 
# 1. What do the data look like?
# 2. Where do the data come from?
# 3. Can you think of any other data sources that might help you in this project?
# 4. What are the common steps in natural language processing?
# 5. What are some common issues in the analysis of text data?
# 6. What is the relationship between NLP and the concepts you have learned in the
    # Specialization?




setwd("~/Desktop/Data Science course/10. Data Science Capstone/final/en_US")

# 2. Where do the data come from?
# The data is from a corpus called HC Corpora. 
# (see more below)

con <- file("en_US.twitter.txt", "r")
readLines(con, 1) ## Read the first line of text
readLines(con, 1) ## Read the next line of text
readLines(con, 5) ## Read in the next 5 lines of text
close(con) ## It's important to close the connection when you are done.



# Reading the 3 files
con <- file("en_US.blogs.txt")
USblog <- readLines(con)
close(con)

con <- file("en_US.news.txt")
USnews <- readLines(con)
close(con)

con <- file("en_US.twitter.txt")
UStwit <- readLines(con, skipNul = TRUE)
close(con)
    # skipNul = TRUE, because R found 4 embedded nuls in this file

all <- c(USblog, USnews, UStwit)

# read.delim is slower than readLines
# USblog <- read.delim("en_US/en_US.blogs.txt", header=FALSE)



# summary
summary(USblog)
summary(USnews)
summary(UStwit)

head(USblog)
head(USnews)
head(UStwit)


# length
all <- list(blog = USblog, news=USnews, twit=UStwit)
nlines <- sapply(all, length)
# length(USblog)  # 899288
# length(USnews)  # 1010242
# length(UStwit)  # 2360148


# nr characters of longest line
nchar <- sapply(all, function(x){ max(nchar(x)) })
# blog  news  twit 
# 40833 11384   140 


# average nr char per line
meanchar <- sapply(all, function(x){ mean(nchar(x)) })
# blog      news      twit 
# 229.98695 201.16285  68.68054 


# nr words in file
library(ngram)
nwords <- sapply(all, wordcount)
# blog     news     twit 
# 37334131 34372530 30373583 

# system.time:
# user  system elapsed 
# 18.851   0.214  19.316 


# memory size
size <- sapply(all, object.size)
size <- as.character( round(size/10^6,1) )
size <- paste(size, "MB")

df <- data.frame(Number_lines = nlines, Number_words = nwords,
                 Average_characters = meanchar, Memory = size)
df




# 1. What do the data look like?
# The data contains lines of text, which I have loaded into character vectors.


# 3. Can you think of any other data sources that might help you in this project?
# Maybe:
    # Library databases: https://guides.library.uq.edu.au/research-techniques/text-mining-analysis/library-databases
    # Language corpora: https://guides.library.uq.edu.au/research-techniques/text-mining-analysis/language-corpora



# 4. What are the common steps in natural language processing?



# 5. What are some common issues in the analysis of text data?
    # Ambiguity in text makes NLP hard!
    # non-standard English: social media expressions, abbreviations (eg. @justinbieber, TY, BRB)
    # idioms = language expressions: eg. get cold feet
    # neologisms: unfriend, retweet, bromance.




# 6. What is the relationship between NLP and the concepts you have learned in the
    # Specialization?
# Solving NLP problems often requires: 
    # Machine learning algs
    # Probabilistic models



# More about HC Corpora:

# The corpora are collected from publicly available sources by a web crawler. The crawler
# checks for language, so as to mainly get texts consisting of the desired language*.
# 
# Each entry is tagged with it's date of publication. Where user comments are included
# they will be tagged with the date of the main entry.
# 
# Each entry is tagged with the type of entry, based on the type of website it is collected
# from (e.g. newspaper or personal blog) If possible, each entry is tagged with one or more
# subjects based on the title or keywords of the entry (e.g. if the entry comes from the
# sports section of a newspaper it will be tagged with "sports" subject).In many cases
# it's not feasible to tag the entries (for example, it's not really practical to tag each
# individual Twitter entry, though I've got some ideas which might be implemented in the
# future) or no subject is found by the automated process, in which case the entry is tagged
# with a '0'.
# 
# To save space, the subject and type is given as a numerical code.
# 
# Once the raw corpus has been collected, it is parsed further, to remove duplicate entries
# and split into individual lines. Approximately 50% of each entry is then deleted. Since you
# cannot fully recreate any entries, the entries are anonymised and this is a non-profit
# venture I believe that it would fall under Fair Use.
# 
# * You may still find lines of entirely different languages in the corpus. There are 2 main
# reasons for that:1. Similar languages. Some languages are very similar, and the automatic
# language checker could therefore erroneously accept the foreign language text.2.
# "Embedded" foreign languages. While a text may be mainly in the desired language there may
# be parts of it in another language. Since the text is then split up into individual lines,
# it is possible to see entire lines written in a foreign language.Whereas number 1 is just
# an out-and-out error, I think number 2 is actually desirable, as it will give a picture of
# when foreign language is used within the main language.
# 
# Content archived from heliohost.org on September 30, 2016 and retrieved via Wayback Machine
# on April 24, 2017.
# https://web-beta.archive.org/web/20160930083655/http://www.corpora.heliohost.org/aboutcorpus.html


