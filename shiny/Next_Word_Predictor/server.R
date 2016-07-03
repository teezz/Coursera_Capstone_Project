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
        output$fulltext <- renderPrint({ input$inText })

        #fluidRow(column(12, verbatimTextOutput("fulltext")))
        prepareInString <- function(in_string) {
                if (grepl("\\s$", in_string) == TRUE) { # if there is a whitespace at the end
                        return(in_string)
                } else { # if there is no whitespace at the end
                        x <- segment(in_string, what="sentences")
                        x <- last(x)
                        x <- sapply(strsplit(x,split="\\. "),tail,1)
                        return(x)  
                }
                
        }
        
        # Capitalizing first letter
        .simpleCap <- function(x) {
                s <- strsplit(x, " ")[[1]]
                paste(toupper(substring(s, 1, 1)), substring(s, 2),
                      sep = "", collapse = " ")
        }
        
        # Create reactive values
        my_clicks <- reactiveValues(data1 = NULL, data2 = NULL, data3 = NULL)
        intext <- NULL
        
        observe({
                # Call reactive function
                predicted <- predict_data()
                
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
        })
        
        
        
        predict_data <- reactive({
                if (input$inText != "" | grepl("[.$]$", input$inText) == TRUE) {
                        
                        inText <- last(prepareInString(input$inText))
                        
                        if (grepl("\\s$", input$inText) == TRUE) {
                                ## if there is a whitespace at the end get tokens
                                predicted <- predictNextWord(dt, input$inText)
                                results_table <- predicted ## output with P and N-grams
                                output$value <- renderPrint({ results_table })
                                
                                predicted <- predicted$next_word
                                
                                # if ( length(prepareInString(input$inText)) == 1) {
                                #         predicted <- sapply(predicted, .simpleCap)
                                # }
                                
                                my_clicks$data1 <- predicted[1]
                                my_clicks$data2 <- predicted[2]
                                my_clicks$data3 <- predicted[3]
                                return(predicted)
                        } else {
                                predicted <- predictWord(input$inText, unigram_levels)
                                
                                #if (grepl("^[^ ].*", last(inText)) == TRUE) { # if there is no preceding whitespace get letters and find supplement letters in most frequent unigrams
                                
                                if (grepl("\\s.*$", inText) == FALSE) {
                                        predicted <- sapply(predicted, .simpleCap)
                                } else if (grepl(".\\. *$", inText) == TRUE) { # if there is a dot and whitespace before
                                        predicted <- sapply(predicted, .simpleCap)
                                } else {
                                        print("What the ...")
                                        predicted <- predicted
                                }
                        }

                        my_clicks$data1 <- predicted[1]
                        my_clicks$data2 <- predicted[2]
                        my_clicks$data3 <- predicted[3]
                        return(predicted)
                }
        })

        shinyjs::onclick("action1",
                if ( grepl("\\s.*$", prepareInString(input$inText)) == FALSE) {
                        updateTextInput(session, "inText", value = paste(my_clicks$data1, ""))
                } else {
                        pre_text <- sub(input$inText, pattern = " [[:alpha:]]*$", replacement = "")
                        updateTextInput(session, "inText", value = paste(pre_text, my_clicks$data1, "", sep = " ", collapse = ""))
                }
         )
        
        shinyjs::onclick("action2",
                         if ( grepl("\\s.*$", prepareInString(input$inText)) == FALSE) {
                                 updateTextInput(session, "inText", value = paste(my_clicks$data2, ""))
                         } else {
                                 pre_text <- sub(input$inText, pattern = " [[:alpha:]]*$", replacement = "")
                                 updateTextInput(session, "inText", value = paste(pre_text, my_clicks$data2, "", sep = " ", collapse = ""))
                         }
        )
        
        shinyjs::onclick("action3",
                         if ( grepl("\\s.*$", prepareInString(input$inText)) == FALSE) {
                                 updateTextInput(session, "inText", value = paste(my_clicks$data3, ""))
                         } else {
                                 pre_text <- sub(input$inText, pattern = " [[:alpha:]]*$", replacement = "")
                                 updateTextInput(session, "inText", value = paste(pre_text, my_clicks$data3, "", sep = " ", collapse = ""))
                         }
        )
        
})
