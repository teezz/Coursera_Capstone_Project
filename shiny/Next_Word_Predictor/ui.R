#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Next Word Predictor"),
  
  fluidRow(
        column(8,
                textInput("text", label = h3("Enter your text"), value = ""),
                fluidRow(column(6, verbatimTextOutput("value"))),
                hr()
        )
  ),
  fluidRow(
        column(12,
                #fluidRow(column(12, verbatimTextOutput("fulltext")))
                tabsetPanel(
                       tabPanel("Full text", verbatimTextOutput("fulltext")), 
                       tabPanel("Statistics", plotOutput("statistics")), 
                       tabPanel("About", verbatimTextOutput("about"))
                )
        )
  )
  

))
