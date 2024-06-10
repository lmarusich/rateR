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
                DT::dataTableOutput('mainDT'),
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
  
  #call data import module
  inputData <- dataImport(id = "import_ui")
  
  #if user has imported data, 
  #enable "next" button, and call rating options module
  observeEvent(inputData$name(),{
    enable('import_done')
    ratingOptions(id = "options_ui", data = inputData)
  })
  
  #call rating modal module
  observeEvent(input$mainDT_rows_selected,{
    # req(input$mainDT_rows_selected)
    modal(id = "test", 
          data = modified_data(), 
          reactive(input$selectedColumn),
          reactive(input$mainDT_rows_selected),
          reactive(input$ratingType),
          reactive(input$specifyRatings))
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
  modified_data <- reactive({
    req(inputData$name())
    # browser()
    req(input$selectedColumn)
    req(input$ratingType)
    addColumns(inputData$inputData(), input$ratingName, input$selectedColumn)
  })
  
  #show new data on third tab
  output$mainDT <- DT::renderDataTable(
    modified_data(),
    selection = 'single',
    rownames = F
  )
  
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
  
  
}

shinyApp(ui = ui, server = server)
