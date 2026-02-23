dataImportUI <- function(id, label = "Import Method:") {
  
  ns <- NS(id)
  
  tagList(
    
    radioButtons(
      ns("importOptions"),
      label = label,
      choices = c("Upload file" = "upload"
                  # ,
                  # "Google Sheet" = "googlesheet"
                  ),
      selected = "upload"
    ),
    conditionalPanel(
      condition = "input.importOptions == 'upload'",
      ns=ns,
      p("Upload a .txt, .csv, or .xlsx file with one item per row."),
      uiOutput(ns('file1_ui')), ## instead of fileInput('file1', label = NULL),
    ),
    uiOutput(ns("sheets")),
    conditionalPanel(
      condition = "input.importOptions == 'googlesheet'",
      ns = ns,
      textInput("googleURL",
                label = "Paste link to Google Sheet here:")
    ),
    p("If the preview doesn't appear correctly on the right, edit the options below."),
    
      checkboxInput(ns("header"), "Header", TRUE),
      # checkboxInput(ns("excludeEmptyCols"), "Exclude empty columns?", TRUE),
    tags$details(
    tags$summary("More options", style = "display:revert;"),
      radioButtons(ns("sep"), "Column Delimiter",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      radioButtons(ns("decimalPoint"), "Decimal separator",
                   choices = c(Point = '.',
                               Comma = ','),
                   selected = '.'),
      radioButtons(ns("quote"), "Quote",
                   choices = c(None = "",
                               "Double Quote" = "\"",
                               "Single Quote" = "'"),
                   selected = "\"")
    # ,
      ),

    
    
    # #if there's a table displayed, show Rating options
    # conditionalPanel(
    #   condition = "input.importOptions == 'googlesheet'",
    #   textInput(ns("googleURL"),
    #             label = "Paste link to Google Sheet here:")
    # )
    
    
    # 
    # 
    # # column(12, #offset = 0,
    # h4("Data Upload"),
    # p("Upload a .txt or .csv file with the variables separated by columns."),
    # p("or"),
    # checkboxInput(ns("sampleData"), "Use sample data", FALSE),
    # conditionalPanel(
    #   condition = 'input.sampleData == true',
    #   ns = ns,
    #   selectInput(ns("whichsampleData"),
    #               label = "Sample dataset:",
    #               choices = list(
    #                 "bland1995",
    #                 "gilden2010",
    #                 "marusich2016",
    #                 "raz2005"
    #               ),
    #               multiple = FALSE)),
    # fileInput(ns("excelFile"),
    #           label = h5(label),
    #           multiple = FALSE,
    #           accept = c("text/csv",
    #                      "text/comma-separated-values,text/plain",
    #                      ".csv"),
    #           buttonLabel = "Browse...", #Unnecessary line, it's the default
    #           placeholder = "No file selected"), #Ditto
    #
    
  )
}
