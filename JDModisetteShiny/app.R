#
# Joel Modisette
# Jan 14, 2021
# Coursera Capstone R Shiny App


library(shiny)
suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
})

source("helper.R")

ui <- fluidPage(
  
# Header info, this is simple app, so no need for tabs
  
    titlePanel(
        h1("Predicting the Next Word",
        h3("You provide word(s) and the next word is predicted."),
        h6("Coursera Capstone Project"),
        br()
    )),

    textInput(inputId = "user_input", 
                label = "Enter some words in the box below:",
                placeholder = "what is, how come, ..."),

    h3("Next Predicted Word:"),
    
    h3(textOutput(outputId = "anygram_output"), style="color:red"),
    br(), 
    br(), 
    em(h4("About this app:")),
    em(h4("Prediction using a Simple Back Off Model - frequency of occurrence.")),
    em(h4("Corpus drawn from Twitter, Blogs, and News articles.")),

)
# server logic 

server <- function(input, output) {
  
  output$anygram_output <- renderText({
    anygram_search(input$user_input)
  })
  
}

# application logic

shinyApp(ui = ui, server = server)
