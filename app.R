#!/usr/bin/env Rscript
# -*- coding: utf-8 -*- 
# Created on Sat Oct 22 04:49:25 PM EDT 2022 
# author: Ryan Hildebrandt, github.com/ryancahildebrandt

# imports ----
{
    
    library(caret)
    library(randomForest)
    library(shiny)
    library(shinythemes)
    library(reactable)
    library(reactablefmtr)
    library(stringr)
    source("funcs.R")
}

default_df <- data.frame(text = c(stringr::fruit, stringr::sentences[1:100]))
default_df <- prep_df(default_df)
default_df$user[1:10] <- "sift"
default_df$user[170:180] <- "keep"

ui <- fluidPage(
    theme = shinytheme("slate"),
    
    sidebarLayout(
        sidebarPanel(
            titlePanel("SIFtR"),
            tags$h4("By Ryan Hildebrandt :: github.com/ryancahildebrandt"),
            tags$h5("Code @ github.com/ryancahildebrandt/siftr"),
            tags$h2("----"),
            fileInput("upload", "Select CSV", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
            actionButton("keep", "Keep Selection"),
            actionButton("sift", "Sift Selection"),
            actionButton("train", "Update & Train Random Forest"),
            textOutput("msg"),
            tags$h2("----"),
            tags$h5("Accuracy Metrics"),
            tableOutput("acc"),
            tags$h5("Confusion Matrix"),
            tableOutput("conf"),
            tags$h5("Mislabeled Datapoints"),
            tableOutput("miss")
            ),
        mainPanel(
            tags$h4("User Guide:"),
            tags$h5("- Example dataset loaded by default with random forest trained and labels applied, if you just want to test out the app"),
            tags$h5("- Load your own text file with one sentence per line, no header"),
            tags$h5("- If you're using your own data, make sure to apply some labels before you train the model. Select some datapoints with the checkboxes to the left of the data, and use the Keep/Sift Selection buttons to label the selected rows"),
            tags$h5("- Once you have some labels, use the Update & Train Random Forest button to train the classifier"),
            tags$h5("- To correct a model prediction, select one or more mislabeled datapoints and use the Update & Train Random Forest button again"),
            tags$h5("- The classifier will provide a predicted label ($model), confidence in a 'keep' label ($keep), and confidence in a 'sift' label ($sift), and sorts datapoints by the model's confidence in the 'sift' label"),
            tags$h5("- The table is sortable by column, and each column is searchable using the textboxes at the top of the table"),
            reactableOutput("df")
            )
    )
)

server <- function(input, output, session) {
    
    # default state
    selected <- reactive(getReactableState("df", "selected"))
    rv <- reactiveValues()
    
    withProgress(message = "loading default data", {
        rf <- train_rf(default_df)
        rv$df <- pred_rf(default_df, rf)
        
        output$df <- render_rt(rv$df)
        output$acc <- renderTable(rf$acc, digits = 4)
        output$conf <- renderTable(rf$conf, digits = 4)
        output$miss <- renderTable(rf$miss, digits = 4)
    })
    
    # user data
    observeEvent(input$upload, {
        withProgress(message = "processing uploaded data", {
            df <- read_csv(input$upload$datapath, col_names = "text")
            rv$df <- prep_df(df)
            updateReactable("df", data = rv$df[, c(1:5)])
        })
    })
    
    #initial labels
    observeEvent(input$keep, {
        rv$df <- initial_x(rv$df, selected(), "keep")
        updateReactable("df", data = rv$df[, c(1:5)])
        })
    
    observeEvent(input$sift, {
        rv$df <- initial_x(rv$df, selected(), "sift")
        updateReactable("df", data = rv$df[, c(1:5)])
        })
    
    # model update/train
    observeEvent(input$train, {
        withProgress(message = "retraining model", {
            rv$df <- update_x(rv$df, selected())
            
            if (all(is.na(rv$df$user))) {
                output$msg <- renderPrint("Please apply data labels before attempting to train model")
                } else {
                rf <- train_rf(rv$df)
                rv$df <- pred_rf(rv$df, rf)
                
                updateReactable("df", data = rv$df[, c(1:5)])
                output$acc <- renderTable(rf$acc, digits = 4)
                output$conf <- renderTable(rf$conf, digits = 4)
                output$miss <- renderTable(rf$miss, digits = 4)
                }
            })
        })
}

shinyApp(ui = ui, server = server)

