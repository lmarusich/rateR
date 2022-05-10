library(shiny)

ui <- fluidPage(
  titlePanel("rateR"),
  
  tabsetPanel(id = "tabs",
              tabPanel(
                title = "Data Import",
                sidebarLayout(
                  sidebarPanel(
                    title = "Import Options",
                    dataImportUI("import_ui", label = "Import Method:"),
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
                  ),
                  mainPanel(
                    h1('Dataset'),
                    DT::dataTableOutput('preview2')
                  )
                )
              ),
              tabPanel(
                title = "Rate!",
              )
  )
)


server <- function(input, output, session) {
  # Your application server logic
  
  inputData <- dataImport(id = "import_ui")
  
  observeEvent(inputData$name(),{
    ratingOptions(id = "options_ui", data = inputData)
  })
  
  

  main_preview_table <- reactive({
    req(inputData$name())
    DT::datatable(
      inputData$inputData(),
      rownames = F,
      selection = 'none',
      options = list(dom = 'tp', ordering = F)
      )
  })
  
  output$preview1 <- DT::renderDataTable({
    main_preview_table()

  })
  
  output$preview2 <- DT::renderDataTable({
    main_preview_table() %>% DT::formatStyle(
      columns = ifelse(nchar(input$selectedColumn)>0, input$selectedColumn, F),
      background = 'yellow'
    )
  })

  # output$mainDT <- DT::renderDataTable({
  #   # We don't render the table without inputData.
  #   req(inputData$name())
  #   DT::datatable(inputData$inputData(), 
  #             selection = list(mode = "single", target = "cell"),
  #             options = list(dom = 'tp', ordering = F),
  #             rownames = F) %>% DT::formatStyle(
  #               columns = ifelse(nchar(input$selectedColumn)>0, input$selectedColumn, F),
  #               background = 'yellow'
  #             )
  #   
  # })
  
  observeEvent(input$selectedColumn, {
    # req(input$mainData_cells_selected)
    # showModal(modalDialog(
    #   title = "message",
    #   paste("This is a somewhat important message:",
    #         input$my_table_cells_selected[1],
    #         input$my_table_cells_selected[2]),
    #   easyClose = TRUE,
    #   footer = NULL))
  })
  
  observeEvent(input$mainData_cells_selected, {
    req(input$mainData_cells_selected)
    showModal(modalDialog(
      title = "message",
      paste("This is a somewhat important message:",
            input$my_table_cells_selected[1],
            input$my_table_cells_selected[2]),
      easyClose = TRUE,
      footer = NULL))
  })
  

  
  # output$generatedUI <- renderUI({
  #   req(inputData$name()) #conditions are the column names
  #   tagList(
  #     h4('test')
  #   )
  # })
  
}

shinyApp(ui = ui, server = server)
