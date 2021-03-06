# Author: Joris Wiethase
# 
rm(list = ls(all=TRUE))  
library(shiny)
library(DT)
library(stringdist)
library(reshape2)
library(magrittr)
library(lubridate)
library(dplyr)
library(stringr)

# Make the user interface
ui <- shiny::fluidPage(theme = "bootstrap.css",
                       tags$head(
                         tags$style(HTML("
                                         .shiny-output-error-validation {
                                         color: red;
                                         }
                                         "))),
                       # Add a side panel for inputs
                       shiny::sidebarPanel(width = 2,
                                           shiny::fileInput(inputId = 'dataset', 
                                                            label = h4('Choose .csv file to upload'),
                                                            accept = c('.csv')
                                           ),
                                           helpText("Warning: Dataset has to include all of the following column names:"),
                                           hr(),
                                           helpText("'Band.ID' (Format: 'Size-Sequence')"),
                                           helpText("'Species'"),
                                           helpText("'Sex'"),
                                           helpText("'Age'"),
                                           helpText("'Date' (Format: ymd)"),
                                           helpText("'Recap (Format: Y/N)'"),
                                           hr(),
                                           shiny::selectInput(inputId = "errorSeek", 
                                                              label = h4("Choose error seeking option"),
                                                              choices = c('None',
                                                                          'NA in species or band number',
                                                                          'Species - band number discrepancies',
                                                                          'Sex - band number discrepancies',
                                                                          'Age - band number discrepancies',
                                                                          'Same-season recaptures', 
                                                                          'Band sequence discrepancies',
                                                                          'Unusual band size')),
                                           uiOutput("sliderBandDiff"),
                                           uiOutput("sliderBandSize"),
                                           hr(),
                                           selectInput("colID", "Select column for typo check", ' '),
                                           actionButton("go", "Check!", icon = icon("angle-double-right"), width = "auto"),
                                           downloadButton('downloadData', 'Download')
                                           
                       ),
                       mainPanel(DTOutput("table"), style = "height:900px; overflow-y: scroll;overflow-x: scroll;")
                         )

# Make the server functions
server <- function(input, output, session) {
  data <- reactive({
    req(input$dataset)
    data <- read.csv(input$dataset$datapath, na.strings=c("","NA", "-", " ")) 
    req.names <- c("Band.ID", "Species", "Date", "Recap")
    validate(
      need(all(req.names %in% colnames(data), TRUE) == TRUE,
           message = paste("\nError: Missing or miss-spelled column names.\nUnmatched columns:\n\n", paste(c(req.names[req.names %in% colnames(data) == FALSE]), collapse="\n"), sep="")
      )
    )
    
    data$Date <- ymd(data$Date)
    data$field.season <- year(data$Date)
    data$band_size <- stringr::str_pad(sapply(strsplit(as.character(data$Band.ID), split="-"), `[`, 1), 2, side = "left", pad = "0")
    data$band_sequence <- as.numeric(sapply(strsplit(as.character(data$Band.ID), split="-"), `[`, 2))
    data
  })
  
  observeEvent(data(), {
    updateSelectInput(session, "colID", choices=colnames(data()))
  })
  
  
  
  shiny::observeEvent({
    input$dataset
    input$errorSeek}, {
      observe({
        
        if(input$errorSeek == 'None'){
          output$table <-  renderDT({
            data()
          }, options = list(scrollX = TRUE, paging = TRUE), editable = TRUE)
        }
        
        if(input$errorSeek == 'NA in species or band number'){
          data <- data() %>% filter(is.na(Band.ID) | is.na(Species))
          output$table <-  renderDT({
            data
          }, options = list(scrollX = TRUE, paging = FALSE), editable = TRUE)
          output$downloadData <- downloadHandler(
            filename = "NA_spec_band.csv",
            content = function(file) {
              write.csv(data, file, row.names = FALSE)
            })
        }
        
        if(input$errorSeek == 'Species - band number discrepancies'){
          data <- data() %>% 
            filter(!is.na(Band.ID)) %>% 
            group_by(Band.ID) %>% dplyr::filter(length(unique(Species)) > 1) %>% 
            arrange(Band.ID) %>%
            dplyr::select(Band.ID, Species, everything())
          output$table <-  renderDT({
            data
          }, options = list(scrollX = TRUE, paging = FALSE), editable = TRUE)
          output$downloadData <- downloadHandler(
            filename = "spec_band_discrep.csv",
            content = function(file) {
              write.csv(data, file, row.names = FALSE)
            })
        }
        
        if(input$errorSeek == 'Sex - band number discrepancies'){
          data <- data() %>% 
            filter(!is.na(Band.ID)) %>% 
            group_by(Band.ID) %>% dplyr::filter(length(unique(Sex)) > 1) %>% 
            arrange(Band.ID) %>%
            dplyr::select(Band.ID, Sex, Date, everything())
          output$table <-  renderDT({
            data
          }, options = list(scrollX = TRUE, paging = FALSE), editable = TRUE)
          output$downloadData <- downloadHandler(
            filename = "sex_band_discrep.csv",
            content = function(file) {
              write.csv(data, file, row.names = FALSE)
            })
        }
        
        if(input$errorSeek == 'Age - band number discrepancies'){
          data <- data() %>% 
            filter(!is.na(Band.ID)) %>% 
            group_by(Band.ID) %>% dplyr::filter(length(unique(Age)) > 1) %>% 
            arrange(Band.ID) %>%
            dplyr::select(Band.ID, Age, Date, everything())
          output$table <-  renderDT({
            data
          }, options = list(scrollX = TRUE, paging = FALSE), editable = TRUE)
          output$downloadData <- downloadHandler(
            filename = "sex_band_discrep.csv",
            content = function(file) {
              write.csv(data, file, row.names = FALSE)
            })
        }
        
        if(input$errorSeek == 'Same-season recaptures'){
          data <- data() %>% 
            group_by(Band.ID, field.season, Species) %>% 
            filter(n() > 1, !is.na(Band.ID), !is.na(DateTime)) %>% 
            arrange(Band.ID, DateTime) %>% 
            select(Band.ID, field.season, DateTime, everything())
          
            output$table <-  renderDT({
              data
            }, options = list(scrollX = TRUE, paging = FALSE), editable = TRUE)
            output$downloadData <- downloadHandler(
              filename = "same_season_recap.csv",
              content = function(file) {
                write.csv(data, file, row.names = FALSE)
              })
        }
        
        if(input$errorSeek == 'Band sequence discrepancies'){
          seq_data <- reactive({
            data <- data() %>% filter(Recap == 'N') %>% group_by(band_size) %>% arrange(band_size, band_sequence) %>% 
              mutate(band_diff = band_sequence - lag(band_sequence)) 
            data$band_diff[is.na(data$band_diff)] <- 0
            data
          })
          output$sliderBandDiff <- renderUI({
            seq_data <- seq_data()
            sliderInput("inSlider", "Max. band difference", min=min(seq_data$band_diff), max=max(seq_data$band_diff), value=median(seq_data$band_diff))
          })
          observeEvent(input$inSlider, {
            data <-  seq_data() %>% 
              filter(band_diff > input$inSlider) %>% 
              dplyr::select(band_size, band_sequence,  band_diff, Species, everything()) %>% 
              arrange(band_size, band_sequence,  band_diff)
            output$table <-  renderDT({
              data
            }, options = list(scrollX = TRUE, paging = FALSE), editable = TRUE)
            output$downloadData <- downloadHandler(
              filename = "band_seq_discrep.csv",
              content = function(file) {
                write.csv(data, file, row.names = FALSE)
              })
          })
        }
        
        if(input$errorSeek == 'Unusual band size'){
          output$sliderBandSize <- renderUI({
            sliderInput("SliderSize", "Percentage threshold", min=1, max=100, value=c(1,10), dragRange = TRUE)
          })
          observeEvent(input$SliderSize, {
            data <- data() %>% 
              group_by(Species, band_size) %>% mutate(N=length(band_size)) %>% 
              group_by(Species) %>% 
              mutate(perc = round((100*N)/sum(unique(N)), digits = 1),
                     Majority_size = stringr::str_pad(band_size[N = max(N)], 2, side = "left", pad = "0")) %>% 
              filter(perc > input$SliderSize[1] & perc < input$SliderSize[2]) %>% 
              dplyr::select(Species, band_size, perc, Majority_size, everything()) %>% 
              arrange(Species, perc, Majority_size, band_size)
            output$table <-  renderDT({
              data
            }, options = list(scrollX = TRUE, paging = FALSE), editable = TRUE)
            output$downloadData <- downloadHandler(
              filename = "unusual_band_size.csv",
              content = function(file) {
                write.csv(data, file, row.names = FALSE)
              })
          })
        }
        
       
        
      })
      
      observeEvent(input$go, {
        output$table <-  renderDT({
          input$go
          isolate({
            data <- data()
            colID <- input$colID
            names(data)[names(data) == colID] <- 'colID'
            
            kpm <- stringdistmatrix(unique(data$colID), useNames="strings",method="lv")
            m = as.matrix(kpm)
            
            out <- melt(m) %>% distinct() %>% filter(value > 0 & value < 2)
            data <- merge(data, out, by.x = 'colID', by.y = 'Var1', all.x=TRUE) %>% filter(!is.na(value)) %>% 
              dplyr::select(colID, Var2, value, everything()) %>% rename(StringDiff = value)
            
          })
        }, options = list(scrollX = TRUE, paging = FALSE), editable = TRUE)
      })
    })
}

shiny::shinyApp(ui, server)
