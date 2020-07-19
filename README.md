# Next Word Predictor

An NLP project on building an app for predicting the next word the user will write, a similar feature to the one in our phones when we're writing text messages.

In this project we used data from the HC Corpora corpus, which contains text data in four different languages: German, Finnish, Russian and English. We focus on the files corresponding to English, namely one data set with blog entries, another with news entries and a third with Twitter entries.

This repository contains a report on a first analysis of our data, namely:
1. data cleansing and pre-processing;
2. exploratory data analysis.

Then, we used language models for predicting the next word the user will use, more specifically we used maximum likelihood estimations and stupid backoff models.




## Explanation of files

The data for this project is available at: https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip.

The report on the exploratory analysis of our data is included in the following files:

- `milestone_report_1.Rmd`
- `milestone_report_1.html`
- `milestone_report_1_files` folder
  - Check html file at: https://m-couto.github.io/next_word_predictor/milestone_report_1.html.
  
Our entire analysis, from loading the data up until making predictiongs, is contained in the files named `task*.R`.



