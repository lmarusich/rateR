# ToDo Items:
# - look into custom URLs
# - make options for enforcing whole numbers (vs. decimals etc.)
# - enforce label options
# - add output/saving functionality
# - functionality for multiple ratings per item?
# - in modular window, add something to display which row they're on
# - in rating tab, can we limit the clicking to the specific column?
# - in rating tab, add instructions like "click an item to rate it"
# - add "previous" button functionality
# - let users add notes to each rating too
library(shiny)
library(shinyjs)
library(DT)
library(tidyverse)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("rateR"),
  
  tabsetPanel(id = "tabs",
              tabPanel(
                title = "Data Import",
                sidebarLayout(
                  sidebarPanel(
                    title = "Import Options",
                    dataImportUI("import_ui", label = "Import Method:"),
                    br(),
                    disabled(actionButton("import_done",
                                          label = "Next"))
                  ),
                  mainPanel(
                    h4('Data Preview:'),
                    # tableOutput('mainPreview')
                    DT::dataTableOutput('preview1')
                  )
                )
              ),
              tabPanel(
                title = "Ratings Options",
                sidebarLayout(
                  sidebarPanel(
                    ratingOptionsUI("options_ui", label = "Ratings Options:"),
                    disabled(actionButton("options_done",
                                          label = "Next"))
                  ),
                  mainPanel(
                    h2('Dataset'),
                    DT::dataTableOutput('preview2')
                  )
                )
              ),
              tabPanel(
                title = "Rate!",
                h2('Click on a row to start rating'),
                DT::dataTableOutput('mainDT')
                # modalUI("test")
              ),
              tabPanel(
                title = "Save",
                htmlOutput("ratedDownload"),
                h1('save to google option')
              )
  )
)


server <- function(input, output, session) {
  
  initialdf <- reactiveValues(df_data = NULL)
  ratings <- reactiveValues(ratings = list())
  
  #call data import module
  inputData <- dataImport(id = "import_ui")
  
  #if user has imported data, 
  #enable "next" button, and call rating options module
  observe({
    inputData$name()
    req(inputData$imported())
    initialdf$df_data <- inputData$inputData()
    enable('import_done')
    ratingOptions(id = "options_ui", data = inputData)
  })
  
  ratingSpecs <- reactive({
    req(input$ratingType)
    req(input$selectedColumn)
    req(input$ratingName)
    return(list(
      selectedColumn = (input$selectedColumn),
      ratingName = (input$ratingName),
      ratingType = (input$ratingType),
      specified = input$specifyRatings,
      minNumRating = (input$minNumRating),
      maxNumRating = (input$maxNumRating),
      ratingLabels = unlist(strsplit(input$ratingLabels, split = "\n"))
    ))
  })
  
  #call rating modal module
  observeEvent(input$mainDT_rows_selected,{
    showModal(myModal(initialdf,ratingSpecs()$selectedColumn,input$mainDT_rows_selected, ratingSpecs()$ratingName))
  })
  
  event_trigger <- reactive({
    list(input$next_button, input$close_button)
  })
  
  observeEvent(ignoreInit = T, event_trigger(), {
    if(input$next_button==0 && input$close_button==0){
      return()
    }
    validInput <- T
    #enforcing the rating/label type
    if (ratingSpecs()$ratingType == "labels"){
      #check that the label is in the list
      if (ratingSpecs()$specified) {
        if (!(input$inputRating %in% ratingSpecs()$ratingLabels)){
          validInput = F
          showModal(myModal(initialdf, 
                            ratingSpecs()$selectedColumn,
                            input$mainDT_rows_selected, 
                            ratingSpecs()$ratingName, 
                            failed = TRUE, 
                            failMsg = paste0("Please enter one of the following labels: ",
                                             paste(ratingSpecs()$ratingLabels, collapse = ', '))))
          
        }
      }
      
    } else if (ratingSpecs()$ratingType == "numbers"){
      #first check that it's a number - if not, throw an error
      if (is.na(as.numeric(input$inputRating))){
        validInput = F
        showModal(myModal(initialdf, ratingSpecs()$selectedColumn,input$mainDT_rows_selected, ratingSpecs()$ratingName, failed = TRUE, failMsg = "Please enter a numeric rating"))
        
        #then check if min/max was specified, and if so, if their rating is in that range
      } else if (ratingSpecs()$specified){
        if ((as.numeric(input$inputRating) < ratingSpecs()$minNumRating) |
            (as.numeric(input$inputRating) > ratingSpecs()$maxNumRating)){
          #throw an error
          validInput = F
          showModal(myModal(initialdf, ratingSpecs()$selectedColumn,input$mainDT_rows_selected,
                            ratingSpecs()$ratingName,
                            failed = TRUE,
                            failMsg = paste0("Please enter a numeric rating between ",
                                             ratingSpecs()$minNumRating,
                                             " and ",
                                             ratingSpecs()$maxNumRating)))
        }
        
      }
    }
    if (validInput){
      #save the rating
      ratings$ratings[[input$mainDT_rows_selected]] <- input$inputRating
      
      #if they hit the next button, select the next row
      if (input$next_button){
        #go to next row
        selectRows(DT_proxy, selected = input$mainDT_rows_selected + 1) # selects the row of the next index
      }
      
      #if they hit the save/close button, close the modal window
      if (input$close_button){
        removeModal()
      }
    }
    
  })
  
  #generate preview table of data
  main_preview_table <- reactive({
    req(inputData$name())
    DT::datatable(
      inputData$inputData(),
      rownames = F,
      selection = 'none',
      options = list(dom = 'tp', ordering = F)
    )
  })
  
  #show preview table on first page (no interaction)
  output$preview1 <- DT::renderDataTable({
    main_preview_table()
  })
  
  #show preview table on second page (highlight selected column)
  output$preview2 <- DT::renderDataTable({
    main_preview_table() %>% DT::formatStyle(
      columns = ifelse(nchar(input$selectedColumn)>0, input$selectedColumn, F),
      background = 'yellow'
    )
  })
  
  #call addColumns function to add rownumber column and ratings/notes columns to data
  # modified_data <- reactive({
  observe({
    input$selectedColumn
    input$ratingType
    input$ratingName
    req(input$selectedColumn)
    req(input$ratingType)
    req(input$ratingName)
    
    temp <- addColumns(initialdf$df_data, input$ratingName, input$selectedColumn, ratings$ratings, input$ratingType)
    initialdf$df_data <- temp
  })
  
  #show new data on third tab
  output$mainDT <- DT::renderDataTable(
    initialdf$df_data,
    selection = 'single',
    rownames = F
  )
  
  DT_proxy <- dataTableProxy("mainDT")
  
  #if user selects a column and chooses a type of rating, enable "next" button
  observe({
    input$selectedColumn
    input$ratingType
    req(input$selectedColumn)
    req(input$ratingType)
    req(input$ratingName)
    enable("options_done")
  })
  
  #if user hits "next" on import tab, move to options tab
  observeEvent(input$import_done, {
    updateTabsetPanel(session, inputId = 'tabs', selected = "Ratings Options")
  })
  
  #if user hits "next" on options tab, move to rating tab
  observeEvent(input$options_done, {
    updateTabsetPanel(session, inputId = 'tabs', selected = "Rate!")
  })
  
  output$ratedDownload <- renderUI({
    req(inputData$name())
    tagList(
      h4("Download the rated data"),
      selectInput("downloadFormat",
                  label = "File format",
                  choices = list(
                    "csv" = ".csv",
                    "txt" = ".txt"
                  ),
                  selected = NULL),
      downloadButton("downloadData",
                     label = "Download Data")
    )
  })
  
  # Download button
  output$downloadData <- downloadHandler(
    filename = function(){
      paste0(inputData$name(), "-rated", input$downloadFormat)
    },
    content = function(file) {
      if(input$downloadFormat == '.csv') {
        write.csv(initialdf$df_data, file, row.names = F)
      } 
      if(input$downloadFormat == '.txt') {
        write.table(initialdf$df_data, file, row.names = F, sep = "\t")
      } 
    }
  )
  
  
  
}

shinyApp(ui = ui, server = server)
