#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(quanteda)
source("predict_word.R")

# --------- Load the data ---------
load("data/dt.kneser_ney.RData")
load("data/mapping_table.RData")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        
        output$fulltext <- renderPrint({ input$text })

        #fluidRow(column(12, verbatimTextOutput("fulltext")))
        
        
        # You can access the value of the widget with input$text, e.g.
        
        observe({
                if (input$text != "") {
                        if (grepl("\\s$", input$text) == TRUE) {
                                ## if there is a whitespace at the end get tokens
                                results <- predictNextWord(dt, input$text)
                                
                        } else {
                                ## if there is no whitespace get letters and find supplement letters in most frequent unigrams
                                print(input$text)
                                results <- predictWord(input$text, unigram_levels)
                                print(results)
                        }
                        
                        output$value <- renderPrint({ results })
                }
        })
  
  
})
