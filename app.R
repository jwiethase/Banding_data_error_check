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
                                           helpText("'Date' (Format: dmy)"),
                                           helpText("'Recap (Format: Y/N)'"),
                                           hr(),
                                           shiny::selectInput(inputId = "errorSeek", 
                                                              label = h4("Choose error seeking option"),
                                                              choices = c('None',
                                                                          'NA in species or band number',
                                                                          'Species - band number discrepancies',
                                                                          'Same-season recaptures', 
                                                                          'Band sequence discrepancies',
                                                                          'Unusual band size')),
                                           uiOutput("slider"),
                                           hr(),
                                           selectInput("colID", "Select column for typo check", ' '),
                                           actionButton("go", "Check!", icon = icon("angle-double-right"), width = "auto")
                                           
                       ),
                       mainPanel(DTOutput("table"), style = "height:900px; overflow-y: scroll;overflow-x: scroll;")
)

# Make the server functions
server <- function(input, output, session) {
  data <- reactive({
    req(input$dataset)
    data <- read.csv(input$dataset$datapath) 
    names <- colnames(data)
    name.Recap <- names[grepl("Recap", names, ignore.case=TRUE) == TRUE]
    names(data)[names(data) == name.Recap] <- 'Recap'
    name.spec <- names[grepl("Species", names, ignore.case=TRUE) == TRUE]
    names(data)[names(data) == name.spec] <- 'Species'
    name.band <- names[grepl("Band", names, ignore.case=TRUE) == TRUE & grepl("ID", names, ignore.case=TRUE) == TRUE]
    names(data)[names(data) == name.band] <- 'Band.ID'
    
    data$band_size <- sapply(strsplit(as.character(data$Band.ID), split="-"), `[`, 1)
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
          output$table <-  renderDT({
            data() %>% filter(is.na(band_sequence), is.na(Species))
          }, options = list(scrollX = TRUE, paging = FALSE), editable = TRUE)
        }
        if(input$errorSeek == 'Species - band number discrepancies'){
          output$table <-  renderDT({
            data() %>% 
              group_by(Band.ID) %>% dplyr::filter(length(unique(Species)) > 1) %>% 
              arrange(Band.ID) %>%
              dplyr::select(Band.ID, Species, everything())
          }, options = list(scrollX = TRUE, paging = FALSE), editable = TRUE)
        }
        if(input$errorSeek == 'Same-season recaptures'){
          output$table <-  renderDT({
            data() %>% 
              mutate(Date = dmy(Date)) %>% 
              group_by(Band.ID) %>% arrange(Date) %>% mutate(days_diff = difftime(Date, lag(Date), 
                                                                                  units='days')) %>% 
              filter(days_diff < 300) %>% 
              arrange(days_diff) %>%
              dplyr::select(days_diff, Date, Species, Band.ID, everything())
            
          }, options = list(scrollX = TRUE, paging = FALSE), editable = TRUE)
        }
        if(input$errorSeek == 'Band sequence discrepancies'){
          seq_data <- reactive({
            data <- data() %>% filter(Recap == 'N') %>% group_by(band_size) %>% arrange(band_size, band_sequence) %>% mutate(band_diff = band_sequence - lag(band_sequence)) 
            data$band_diff[is.na(data$band_diff)] <- 0
            data
          })
          output$slider <- renderUI({
            seq_data <- seq_data()
            sliderInput("inSlider", "Band difference range", min=min(seq_data$band_diff), max=max(seq_data$band_diff), value=2)
          })
          observeEvent(input$inSlider, {
            output$table <-  renderDT({
              seq_data() %>% 
                filter(band_diff > input$Slider) %>% 
                dplyr::select(band_size, band_sequence,  band_diff, Species, everything()) %>% 
                arrange(band_size, band_sequence,  band_diff)
              data
            }, options = list(scrollX = TRUE, paging = FALSE), editable = TRUE)
            
          })

        }
        if(input$errorSeek == 'Unusual band size'){
          output$table <-  renderDT({
            data() %>% 
              group_by(Species, band_size) %>% mutate(N=length(band_size)) %>% 
              group_by(Species) %>% 
              mutate(perc = round((100*N)/sum(unique(N)), digits = 1)) %>% 
              filter(perc <= 10) %>% 
              dplyr::select(band_size, Species, perc, everything()) %>% 
              arrange(Species, perc, band_size)
          }, options = list(scrollX = TRUE, paging = FALSE), editable = TRUE)
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
