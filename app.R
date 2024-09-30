# ToDo Items:
# - look into custom URLs
# - make options for enforcing whole numbers (vs. decimals etc.)
# - add output/saving functionality
# - functionality for multiple ratings per item?
# - in rating tab, can we limit the clicking to the specific column?

library(shiny)
library(shinyjs)
library(DT)
library(tidyverse)
library(datamods)

jscode <- HTML("$('body').on('shown.bs.modal', (x) =>
                    $(x.target).find('input[type=\"text\"]#inputRating:first').focus().select());
")

ui <- function(request){
  fluidPage(
    useShinyjs(),
    titlePanel("rateR"),
    tags$header(
      tags$script(type = "text/javascript", jscode)
    ),
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
                  title = "Data Filtering",
                  sidebarLayout(
                    sidebarPanel(
                      disabled(checkboxInput(
                        "filterBool",
                        label = "Filter the dataset?",
                        value = F
                      )),
                      conditionalPanel(
                        condition = "input.filterBool",
                        uiOutput("filteringCols")
                        
                      ),
                    conditionalPanel(
                      condition = "input.filterBool & input.filteringCols != ''",
                      filter_data_ui("filtering", max_height = "500px")
                    ),
                    disabled(actionButton("filter_done",
                                 label = "Next"))
                      
                    ),
                    mainPanel(
                      DT::dataTableOutput('filteredpreview')
                    )
                  )
                ),
                tabPanel(
                  title = "Ratings Options",
                  sidebarLayout(
                    sidebarPanel(
                      ratingOptionsUI("options_ui", label = "Ratings Options:"),
                      disabled(actionButton("options_done",
                                            label = "Next")),
                      disabled(bookmarkButton(id="bookmarkBtn", label = "Bookmark Rating Options"))
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
                  htmlOutput("ratedDownload")
                  # ,
                  # h1('save to google option')
                )
    )
  )
}

server <- function(input, output, session) {
  
  #bookmarking options
  bookmarkingWhitelist <- c("selectedColumn",
                            "ratingName",
                            "ratingType",
                            "specifyRatings",
                            "ratingLabels",
                            "minNumRating",
                            "maxNumRating")
  
  observeEvent(input$bookmarkBtn, {
    session$doBookmark()
  })
  
  observe({
    toExclude <- setdiff(names(input), bookmarkingWhitelist)
    setBookmarkExclude(toExclude)
  })
  
  initialdf <- reactiveValues(df_data = NULL)
  filtereddf <- reactiveValues(df_data = NULL)
  ratings <- reactiveValues(ratings = list(),
                            notes = list())
  
  ###################################################################
  ### Data importing
  ###################################################################
  #call data import module
  inputData <- dataImport(id = "import_ui")
  
  
  ###################################################################
  ### Data filtering
  ###################################################################
  
  #if user has imported data, 
  #enable "next" button, and enable filtering options
  observe({
    inputData$name()
    req(inputData$imported())
    initialdf$df_data <- inputData$inputData()
    enable('import_done')
    enable('filterBool')
    enable('filter_done')
  })
  
  output$filteringCols <- renderUI({
    selectInput(
      "filteringCols",
      label = "Filter by values of which columns?",
      choices  = c("",inputData$columns()),
      multiple = T
    )
  })
  
  res_filter <- filter_data_server(
    id = "filtering",
    data = reactive(initialdf$df_data),
    name = reactive("filtered_data"),
    vars = reactive(input$filteringCols),
    widget_char = "picker",
    widget_num = "slider",
    widget_date = "slider",
    label_na = "Include missing cells?"

  )

  output$filteredpreview <- DT::renderDataTable(
    res_filter$filtered(),
    rownames = F,
    selection = 'none'
  )
  

  ###################################################################
  ### Rating options
  ###################################################################
  
  #if user has imported data, 
  #enable "next" button, and call rating options module
  observe({
    inputData$name()
    req(inputData$imported())
    filtereddf$df_data <-  res_filter$filtered()
    # enable('import_done')
    ratingOptions(id = "options_ui", data = inputData)
  })
  
  ratingSpecs <- reactive({
    req(input$ratingType)
    req(input$`options_ui-selectedColumn`)
    req(input$ratingName)
    return(list(
      selectedColumn = (input$`options_ui-selectedColumn`),
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
    # browser()
    showModal(myModal(filtereddf,
                      ratingSpecs()$selectedColumn,
                      input$mainDT_rows_selected, 
                      ratingSpecs()$ratingName,
                      existingRating = filtereddf$df_data[[ratingSpecs()$ratingName]][input$mainDT_rows_selected],
                      existingNotes = filtereddf$df_data[[paste0(ratingSpecs()$ratingName, "_notes")]][input$mainDT_rows_selected]))
  })
  
  event_trigger <- reactive({
    list(input$prev_button, input$next_button, input$close_button)
  })
  
  observeEvent(ignoreInit = T, event_trigger(), {
    if(input$prev_button==0 && input$next_button==0 && input$close_button==0){
      return()
    }
    validInput <- T
    #enforcing the rating/label type
    if (ratingSpecs()$ratingType == "labels"){
      #check that the label is in the list
      if (ratingSpecs()$specified) {
        if (!(input$inputRating %in% ratingSpecs()$ratingLabels)){
          validInput = F
          showModal(myModal(filtereddf, 
                            ratingSpecs()$selectedColumn,
                            input$mainDT_rows_selected, 
                            ratingSpecs()$ratingName, 
                            existingRating = input$inputRating,
                            existingNotes = input$inputNotes,
                            failed = TRUE, 
                            failMsg = paste0("Please enter one of the following labels: ",
                                             paste(ratingSpecs()$ratingLabels, collapse = ', '))))
          
        }
      } else {
        if (nchar(input$inputRating)<1){
          validInput = F
          showModal(myModal(filtereddf, 
                            ratingSpecs()$selectedColumn,
                            input$mainDT_rows_selected, 
                            ratingSpecs()$ratingName, 
                            existingRating = input$inputRating,
                            existingNotes = input$inputNotes,
                            failed = TRUE, 
                            failMsg = paste0("Please enter a label: ",
                                             paste(ratingSpecs()$ratingLabels, collapse = ', '))))
        }
      }
      
    } else if (ratingSpecs()$ratingType == "numbers"){
      #first check that it's a number - if not, throw an error
      if (is.na(as.numeric(input$inputRating))){
        validInput = F
        showModal(myModal(filtereddf, 
                          ratingSpecs()$selectedColumn,
                          input$mainDT_rows_selected, 
                          ratingSpecs()$ratingName, 
                          existingRating = input$inputRating,
                          existingNotes = input$inputNotes,
                          failed = TRUE, failMsg = "Please enter a numeric rating"))
        
        #then check if min/max was specified, and if so, if their rating is in that range
      } else if (ratingSpecs()$specified){
        if ((as.numeric(input$inputRating) < ratingSpecs()$minNumRating) |
            (as.numeric(input$inputRating) > ratingSpecs()$maxNumRating)){
          #throw an error
          validInput = F
          showModal(myModal(filtereddf, ratingSpecs()$selectedColumn,input$mainDT_rows_selected,
                            ratingSpecs()$ratingName,
                            existingRating = input$inputRating,
                            existingNotes = input$inputNotes,
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
      #save the notes
      ratings$notes[[input$mainDT_rows_selected]] <- input$inputNotes
      
      #if they hit the previous button, select the previous row
      if (input$prev_button){
        
        temprow <- input$mainDT_rows_selected
        tempdone <- F
        
        # #if skipping empty cells, check if the cell to be rated is empty
        # if(input$skipEmptyCells){
        #   while (temprow > 1){
        #     temprow <- temprow - 1
        #     # browser()
        #     if(!is.na(initialdf$df_data[[ratingSpecs()$selectedColumn]][temprow])){
        #       break
        #     }
        #     
        #     #if the previous row is still NA, and it's the first row, just close the modal
        #     if(temprow == 1){
        #       tempdone <- T
        #       removeModal()
        #     }
        #   }
        # } else {
          temprow <- temprow - 1
        # }
        
        #go to previous row
        if(!tempdone){
          selectRows(DT_proxy, selected = temprow) # selects the row of the previous index
        }
        
      }
      
      #if they hit the next button, select the next row
      if (input$next_button){
        
        temprow <- input$mainDT_rows_selected
        tempdone <- F
        
        # #if skipping empty cells, check if the cell to be rated is empty
        # if(input$skipEmptyCells){
        #   while (temprow < dim(initialdf$df_data)[1]){
        #     temprow <- temprow + 1
        #     if(!is.na(initialdf$df_data[[ratingSpecs()$selectedColumn]][temprow])){
        #       break
        #     }
        #     
        #     #if the next row is still NA, and it's the last row, just close the modal
        #     if(temprow == dim(initialdf$df_data)[1]){
        #       tempdone <- T
        #       removeModal()
        #     }
        #   }
        # } else {
          temprow <- temprow + 1
        # }
        
        #go to next row
        if (!tempdone){
          selectRows(DT_proxy, selected = temprow) # selects the row of the next index
        }
        
      }
      
      #if they hit the save/close button, close the modal window
      if (input$close_button){
        removeModal()
      }
    }
    
  })
  
  #generate preview table of raw data
  main_preview_table <- reactive({
    req(inputData$name())
    DT::datatable(
      inputData$inputData(),
      rownames = F,
      selection = 'none',
      options = list(dom = 'tp', ordering = F)
    )
  })
  
  #generate preview table of filtered data
  filtered_preview_table <- reactive({
    req(inputData$name())
    DT::datatable(
      res_filter$filtered(),
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
    filtered_preview_table() %>% DT::formatStyle(
      columns = ifelse(nchar(input$`options_ui-selectedColumn`)>0, input$`options_ui-selectedColumn`, F),
      background = 'yellow'
    )
  })
  
  #call addColumns function to add rownumber column and ratings/notes columns to data
  observe({
    input$`options_ui-selectedColumn`
    input$ratingType
    input$ratingName
    req(input$`options_ui-selectedColumn`)
    req(input$ratingType)
    req(input$ratingName)
    
    temp <- addColumns(filtereddf$df_data, input$ratingName, input$`options_ui-selectedColumn`, ratings$ratings, ratings$notes, input$ratingType)
    filtereddf$df_data <- temp
  })
  
  #show new data on third tab
  output$mainDT <- DT::renderDataTable(
    filtereddf$df_data,
    selection = 'single',
    rownames = F
  )
  
  DT_proxy <- dataTableProxy("mainDT")
  
  #if user selects a column and chooses a type of rating, enable "next" button
  observe({
    input$`options_ui-selectedColumn`
    input$ratingType
    req(input$`options_ui-selectedColumn`)
    req(input$ratingType)
    req(input$ratingName)
    enable("options_done")
    enable("bookmarkBtn")
  })
  
  #if user hits "next" on import tab, move to filtering tab
  observeEvent(input$import_done, {
    updateTabsetPanel(session, inputId = 'tabs', selected = "Data Filtering")
  })
  
  #if user hits "next" on filter tab, move to options tab
  observeEvent(input$filter_done, {
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
      # browser()
      finaldf <- full_join(inputData$inputData(), filtereddf$df_data)
      
      if(input$downloadFormat == '.csv') {
        # browser()
        write.csv(finaldf, file, row.names = F)
      } 
      if(input$downloadFormat == '.txt') {
        write.table(finaldf, file, row.names = F, sep = "\t")
      } 
    }
  )
  
  
  
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")
