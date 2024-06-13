# ToDo Items:
# - look into custom URLs
# - enforce rating specifications
# - add ratings to data preview
# - add output/saving functionality
# - functionality for multiple ratings per item?
# - in modular window, add something to display which row they're on
# - in rating tab, can we limit the clicking to the specific column?
# - in rating tab, add instructions like "click an item to rate it"
library(shiny)
library(shinyjs)
library(DT)

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
                    h1('Dataset'),
                    DT::dataTableOutput('preview2')
                  )
                )
              ),
              tabPanel(
                title = "Rate!",
                DT::dataTableOutput('mainDT')
                # modalUI("test")
              ),
              tabPanel(
                title = "Save",
                h1('download option'),
                h1('save to google option')
              )
  )
)


server <- function(input, output, session) {
  
  initialdf <- reactiveValues(df_data = NULL)
  newdf <- reactiveValues(df_data = NULL)
  ratings <- reactiveValues(ratings = list())
  
  # observeEvent(input$file, {
  #   values$df_data <- read.csv(input$file$datapath)
  # })
  # 
  # observeEvent(input$Go, {
  #   temp <- values$df_data[-input$Delete, ]
  #   values$df_data <- temp
  #   
  # })
  
  #call data import module
  inputData <- dataImport(id = "import_ui")
  
  #if user has imported data, 
  #enable "next" button, and call rating options module
  observe({
    inputData$name()
    req(inputData$imported())
    initialdf$df_data <- inputData$inputData()
    # ratings$ratings <- character(length = nrow(initialdf$df_data))
    # browser()
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
      ratingLabels = (input$ratingLabels)
    ))
  })
  
  #call rating modal module
  observeEvent(input$mainDT_rows_selected,{
    # observe(
    input$mainDT_rows_selected
    # req(initialdf$df_data)
    
    # req(input$mainDT_rows_selected)
    
    showModal(myModal(ratingSpecs()$selectedColumn,input$mainDT_rows_selected, ratingSpecs()$ratingName))
    #   newdf$df_data <- modal(
    #     input,
    #               data = initialdf$df_data,
    #               reactive(input$mainDT_rows_selected),
    #               ratingSpecs = ratingSpecs())
    # # )
    # temp <- initialdf$df_data
    # browser()
    # temp[[input$ratingName]] <- newdf()$rating
    # browser()
  })
  
  event_trigger <- reactive({
    list(input$next_button, input$close_button)
  })
  observeEvent(ignoreInit = T, event_trigger(), {
    if(input$next_button==0 && input$close_button==0){
      return()
    }
    # browser()
    validInput <- T
    #enforcing the rating/label type
    if (ratingSpecs()$ratingType == "labels"){
      #check that the label is in the list
    } else if (ratingSpecs()$ratingType == "numbers"){
      #first check that it's a number - if not, throw an error
      if (is.na(as.numeric(input$inputRating))){
        validInput = F
        showModal(myModal(ratingSpecs()$selectedColumn,input$mainDT_rows_selected, ratingSpecs()$ratingName, failed = TRUE, failMsg = "Please enter a numeric rating"))
        
        #then check if min/max was specified, and if so, if their rating is in that range
      } else if (ratingSpecs()$specified){
        if ((input$inputRating < ratingSpecs()$minNumRating) |
            (input$inputRating > ratingSpecs()$maxNumRating)){
          #throw an error
          validInput = F
          showModal(myModal(ratingSpecs()$selectedColumn,rownum(),
                            ratingSpecs()$ratingName,
                            failed = TRUE,
                            failMsg = paste0("Please enter a numeric rating between ",
                                             ratingSpecs()$minNumRating,
                                             " and ",
                                             ratingSpecs()$maxNumRating)))
        }
      }
      
      if (validInput){
        #save the rating
        # temp <- initialdf$df_data
        # temp[[ratingSpecs()$ratingName]][input$mainDT_rows_selected] <- input$inputRating
        # browser()
        # initialdf$df_data <- temp
        ratings$ratings[[input$mainDT_rows_selected]] <- input$inputRating
        browser()
        # df$row[rownum()] <- rownum()
        # df$rating[rownum()] <- input$inputRating
        
        if (input$next_button){
          #go to next row
          selectRows(DT_proxy, selected = input$mainDT_rows_selected + 1) # selects the row of the next index

        }
        
        if (input$close_button){
          removeModal()
        }
      }
    }
  })
  
  
  myModal <- function(colname, rownum, ratingName, failed = FALSE, failMsg = "") {
    # browser()
    modalDialog(
      title = "Item Rating",
      sidebarLayout(
        sidebarPanel(
          h4(paste0(colname,":")),
          p(initialdf$df_data[[colname]][rownum]),
          width = 6
        ),
        
        mainPanel(
          
          div(tags$b(ifelse(failed, failMsg, ""), style = "color: red")), #customize this based on the rating type
          textInput(("inputRating"), 
                    label = paste0(ratingName, " Rating:")), 
          width = 6
        )
      ),
      footer = tagList(
        actionButton(("prev_button"), "Previous"),
        actionButton(("next_button"), "Next"),
        actionButton(("close_button"), "Save and Close")
      )
    )
  }
  
  
  
  # observeEvent(input$next_button, {
  #   browser() #update newdf inside the modal function?
  #   selectRows(DT_proxy, selected = input$mainDT_rows_selected + 1) # selects the row of the next index
  # })
  
  
  # #if user has entered a rating
  # #do something?
  # observeEvent(input$`test-inputRating`,{
  #   # enable('import_done')
  #   # ratingOptions(id = "options_ui", data = inputData)
  #   req(input$`test-inputRating`)
  #   # browser()
  # })
  
  # observeEvent(input$test-close_button,{
  #     browser()
  #   })
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
    # browser()
    temp <- addColumns(initialdf$df_data, input$ratingName, input$selectedColumn, ratings)
    # browser()
    # initialdf$df_data <- temp
    # req(ratings$ratings)
    # browser()
    # temp2 <-initialdf$df_data
    # temp2[[input$ratingName]] <- ratings
    # 
  })
  #   req(inputData$name())
  #   # browser()
  #   req(input$selectedColumn)
  #   req(input$ratingType)
  #   # browser()
  #   addColumns(inputData$inputData(), input$ratingName, input$selectedColumn)
  #   
  #   # req(newdf())
  #   # browser()
  #   # if (req(df)){
  #   #   browser()
  #   # }
  #   
  # })
  
  # rated_data <- reactive({
  #   browser()
  #   req(df)
  #   browser()
  # })
  
  #show new data on third tab
  output$mainDT <- DT::renderDataTable(
    # modified_data(),
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
    # ratingSpecs <- ratingOptions(id = "options_ui", data = inputData)
    
  })
  
  #if user hits "next" on options tab, move to rating tab
  observeEvent(input$options_done, {
    # ratingSpecs <- ratingOptions(id = "options_ui", data = inputData)
    
    updateTabsetPanel(session, inputId = 'tabs', selected = "Rate!")
  })
  
  
}

shinyApp(ui = ui, server = server)
