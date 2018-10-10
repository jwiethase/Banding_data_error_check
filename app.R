# 
# 
rm(list = ls(all=TRUE))  
library(shiny)
library(DT)

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
                                                                              'Band sequence discrepancies'))
                                               ),
                       mainPanel(DTOutput("table"))
                       )

# Make the server functions
server <- function(input, output) {
  data <- reactive({
    req(input$dataset)
    data <- read.csv(input$dataset$datapath)
  })
  
  shiny::observeEvent({
    input$dataset
    input$errorSeek}, {
      
      observe({
        if(input$errorSeek == 'None'){
          output$table <-  renderDT({
          data()
            },options = list(scrollX = TRUE),editable = TRUE)
        }
      })
      
      observe({
        if(input$errorSeek == 'NA in species or band number'){
          output$table <-  renderDT({
            data <- data()
            data <- data %>% filter(is.na(Band.ID), is.na(species))
          },options = list(scrollX = TRUE),editable = TRUE)
        }
      })
      
      observe({
        if(input$errorSeek == 'Species - band number discrepancies'){
          output$table <-  renderDT({
            data <- data()
            data <- data %>% group_by(Band.ID) %>% dplyr::filter(length(unique(species)) > 1) %>% 
              arrange(Band.ID) %>%
              select(Band.ID, species, everything())
          },options = list(scrollX = TRUE),editable = TRUE)
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
          },options = list(scrollX = TRUE),editable = TRUE)
        }
      })
      
      observe({
        if(input$errorSeek == 'Band sequence discrepancies'){
          output$table <-  renderDT({
            data <- data()
            data <- data %>% group_by(band_size) %>% arrange(band_sequence) %>% mutate(band_diff = band_sequence - dplyr::lag(band_sequence)) %>% 
              filter(band_sequence == boxplot(band_sequence)$out) %>% 
              arrange(band_diff) %>%
              select(band_diff, species, Band.ID, everything())
          },options = list(scrollX = TRUE),editable = TRUE)
        }
      })
      
      
      })
  }

shiny::shinyApp(ui, server)
