#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  useShinyjs(),
  
  # Application title
  titlePanel("Next Word Predictor"),
  
  fluidRow(
        column(12,
               
                tags$code("The Next Word Predictor uses the Kneser-Ney Smoothing to predict the next word."),
                hr(),
                p("Start typing in words in the text input field. As soon as letters are typed in you will see 3 sugestions for word completion. If you enter a whitespace after a word you will see the most probable next words."),
               
                textInput("inText", label = h3("Enter your text"), value = "", width = '40%'),
                
                fluidRow(
                        column(2, uiOutput("prediction_1")),
                        column(2, uiOutput("prediction_2")),
                        column(2, uiOutput("prediction_3"))
                ),
                
                
                hr(),
                fluidRow(column(6, verbatimTextOutput("value")))
        )
  ),

  fluidRow(
        column(12,
                #fluidRow(column(12, verbatimTextOutput("fulltext")))
                tabsetPanel(
                       tabPanel("Full text", verbatimTextOutput("fulltext")), 
                       tabPanel("Statistics", 
                                plotOutput("statistics")
                        ), 
                       tabPanel("Instruction", 
                               h4("Instruction"),
                               p('It is very uncomfortable to type on mobile devices like cellphones or tablets. A smart and efficient way for text input is required. The core of this input system is a predictive language model and this shiny app is builded based on n-gram model with Kneser-Ney Smoothing.'),
                               h4("Usage"),
                               p("Start typing in words in the text input field. As soon as letters are typed in you will see 3 sugestions for word completion. If you enter a whitespace after a word you will see the most probable next words.")
                       )
                )
        )
  )
  

))
