# Next Word Predictor

An NLP project on building an app for predicting the next word the user will write, a similar feature to the one in our phones when we're writing text messages.
  - Check available app at: https://m-couto.shinyapps.io/PredictNextWord/.

In this project we used data from the HC Corpora corpus, more specifically text in English from blogs, news and Twitter.

This repository contains a report on a first analysis of our data, namely:
1. data cleansing and pre-processing;
2. exploratory data analysis.

Then, we used language models for predicting the next word the user will use, more specifically we used maximum likelihood estimations and stupid backoff models.

Our prediction models were built into an app, which is now available at: https://m-couto.shinyapps.io/PredictNextWord/. And we also built a presentation for pitching this app.


## Explanation of files

The data for this project is available at: https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip.

The report on the exploratory analysis of our data is included in the following files:

- `milestone_report_1.Rmd`
- `milestone_report_1.html`
- `milestone_report_1_files` folder
  - Check html file at: https://m-couto.github.io/next_word_predictor/milestone_report_1.html.
  
Our entire analysis, from loading the data up until making predictiongs, is contained in the files named `task*.R`.

- `swearWords.txt`: an additional file to the analysis, for removing profanity.

The app was built with the following files and it is available at: https://m-couto.shinyapps.io/PredictNextWord/.

- `server.R`, `ui.R`

The pitch presentation for this app lies in the following files:

- `project_pitch.html`, `project_pitch.Rmd`
- `input.png`, `output.png`: image attachments for the pitch presentation
  - Check html file at: https://m-couto.github.io/next_word_predictor/project_pitch.html.
