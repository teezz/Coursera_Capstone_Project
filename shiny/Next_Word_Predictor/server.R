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
# --------- END ---------


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
        output$fulltext <- renderPrint({ input$text })

        #fluidRow(column(12, verbatimTextOutput("fulltext")))
        
        
        # You can access the value of the widget with input$text, e.g.
        my_clicks <- reactiveValues(data1 = NULL, data2 = NULL, data3 = NULL)
        
        observe({
                #predicted <- predict_data()
                browser()
                if (input$inText != "") {
                        if (grepl("\\s$", input$inText) == TRUE) {
                                ## if there is a whitespace at the end get tokens
                                predicted <- predictNextWord(dt, input$inText)
                                results_table <- predicted ## output with P and N-grams
                                output$value <- renderPrint({ results_table })

                                predicted <- predicted$next_word
                                
                                prediction1$data <- predicted[1]
                                
                        } else {
                                ## if there is no whitespace get letters and find supplement letters in most frequent unigrams
                                predicted <- predictWord(input$inText, unigram_levels)
                                my_clicks$data1 <- predicted[1]
                                my_clicks$data2 <- predicted[2]
                                my_clicks$data3 <- predicted[3]
                        }
                        
                        
                        ## Action buttons
                        output$prediction_1 <- renderUI({
                                actionButton("action1", label = predicted[1])
                        })
                        output$prediction_2 <- renderUI({
                                actionButton("action2", label = predicted[2])
                        })
                        output$prediction_3 <- renderUI({
                                actionButton("action3", label = predicted[3])
                        })
                
                }

        })

        
        
        
        
        
        observeEvent(input$action1, {
                
                # This will change the value of input$inText
                updateTextInput(session, "inText", value = paste(my_clicks$data1, ""))
        })
        

})
