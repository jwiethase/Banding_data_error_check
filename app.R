# 
# 
rm(list = ls(all=TRUE))  
library(shiny)
library(DT)
library(stringdist)
library(reshape2)
library(magrittr)

# Make the user interface
ui <- shiny::fluidPage(theme = "bootstrap.css",
                           # Add a side panel for inputs
                           shiny::sidebarPanel(width = 2,
                                               shiny::fileInput(inputId = 'dataset', 
                                                                   label = h4('Choose .csv file to upload'),
                                                                   accept = c('.csv')
                                                  ),
                                                  helpText("Warning: Data set has to include all of the following columns:",
                                                           "band_size  band_sequence  species  Date(as ymd)  year"),
                                               shiny::selectInput(inputId = "errorSeek", 
                                                                  label = h4("Choose error seeking option"),
                                                                  choices = c('None',
                                                                              'NA in species or band number',
                                                                              'Species - band number discrepancies',
                                                                              'Same-season recaptures', 
                                                                              'Band sequence discrepancies',
                                                                              'Unusual band size')),
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
    if("X" %in% names(data) == TRUE){
      data <- data %>% select(-X)
    }
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
      })
      
      observe({
        if(input$errorSeek == 'NA in species or band number'){
          output$table <-  renderDT({
            data <- data()
            data <- data %>% filter(is.na(Band.ID), is.na(species))
          }, options = list(scrollX = TRUE, paging = FALSE), editable = TRUE)
        }
      })
      
      observe({
        if(input$errorSeek == 'Species - band number discrepancies'){
          output$table <-  renderDT({
            data <- data()
            data <- data %>% group_by(Band.ID) %>% dplyr::filter(length(unique(species)) > 1) %>% 
              arrange(Band.ID) %>%
              select(Band.ID, species, everything())
          }, options = list(scrollX = TRUE, paging = FALSE), editable = TRUE)
        }
      })
      
      observe({
        if(input$errorSeek == 'Same-season recaptures'){
          output$table <-  renderDT({
            data <- data()
            data <- data %>% 
              mutate(Date = ymd(Date), field.season=as.factor(year)) %>% 
              group_by(Band.ID) %>% arrange(Date) %>% mutate(days_diff = difftime(Date, lag(Date), 
                                                                                  units='days')) %>% 
              filter(days_diff < 300) %>% 
              arrange(days_diff) %>%
              select(days_diff, Date, species, Band.ID, everything())
          }, options = list(scrollX = TRUE, paging = FALSE), editable = TRUE)
        }
      })
      
      observe({
        if(input$errorSeek == 'Band sequence discrepancies'){
          output$table <-  renderDT({
            data <- data()
            data <- data %>% group_by(band_size) %>% arrange(band_size, band_sequence) %>% mutate(band_diff = band_sequence - lag(band_sequence)) %>% 
              filter(band_diff > 1) %>% 
              select(band_size, band_sequence,  band_diff, year, species, everything())
          }, options = list(scrollX = TRUE, paging = TRUE), editable = TRUE)
        }
      })

      observe({
        if(input$errorSeek == 'Unusual band size'){
          output$table <-  renderDT({
            data <- data()
            data <- data %>% group_by(species, band_size) %>% mutate(N=length(band_size)) %>% 
              group_by(species) %>% 
              mutate(perc = round((100*N)/sum(unique(N)), digits = 1)) %>% 
              filter(perc <= 10) %>% 
              select(band_size, species, perc, everything()) %>% 
              arrange(species, perc, band_size)
          }, options = list(scrollX = TRUE, paging = FALSE), editable = TRUE)
        }
      })
      
      observeEvent(data(), {
        updateSelectInput(session, "colID", choices=colnames(data()))
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
              select(colID, Var2, value, everything()) %>% rename(StringDiff = value)
            
            })
            }, options = list(scrollX = TRUE, paging = FALSE), editable = TRUE)
      })
      
      
      })
  }

shiny::shinyApp(ui, server)
