Coursera Data Science Capstone Project
========================================================
author: Joel Modisette
date: January 18, 2022
autosize: true

Word Prediction Project
========================================================

This project employed Natural Language Processing (NLP) to predict the next word in a sentence. 

The prediction draws from a corpus from Twitter, Blogs and News articles to determine the likelihood of a word in a sentence, given preceding 1-3 words. 

Project Milestones:

- Exploratory Analysis of Corpus and Basic NLP Concepts
- Predictive Model Development using Backoff Method
- Beta Deployment of R Shiny App Demonstrating this Method

How it works: Prediction Model for Next Word Completion
========================================================

1. Download, sample and clean the data corpus.
2. Decompose the Twitter, news and blogs into 2, 3 and 4 word n grams. 
3. Determine the frequency of occurrence of each n gram.
4. Given a 1 to 3 word sentence, use corresponding "highest likelihood" match:
   + Given 1 word, match to the first word of a bi-gram
   + Given 2 words, match first and second word to a tri-gram.
   + Given 3 words, match first, second and third word to a quad-gram.
   + Given more than 3 words, match the last three to a quad-gram.


Performance Issue Trade Offs:

+ Retaining the maximum information from corpus.
+ Providing the fastest reasonable response time in the R Shiny App.


R Shiny App Incorporating Next Word Completion
========================================================

Objective: Demonstrate Word Prediction for User Feedback

Constraints:
+ Easy text box for user input
+ Predictions dynamically rendered
+ Rapid response
+ Mobile Phone Friendly

***

![](screenshot.PNG)


Project Resources
========================================================


* Exploratory Analysis Progress Report

   https://rpubs.com/JoelModisette/Capstone10_EA
* R Shiny App Project Deliverable 

   http://jmodisette.shinyapps.io/**thingy**
* Github Repository with source code

   https://github.com/joelmodisette/Course-10_DataScienceCapstone
* Text Mining with R: A Tidy Approach

   http://tidytextmining.com

* [Coursera-SwiftKey Corpus Repo] (https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip](Coursera-SwiftKey Corpus Repository) 
