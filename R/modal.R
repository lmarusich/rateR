modal <- function(id, data, itemCol, selectedRow){
  moduleServer(
    id,
    
    
    
    function(input, output, session) {
      ns <- session$ns
      
      myModal <- function(colname, rownum) {
        modalDialog(
          title = "Item Rating",
          sidebarLayout(
            sidebarPanel(
              h4(paste0(colname,":")),
              p(data[[colname]][rownum]),
              width = 6
            ),
            mainPanel(
              textInput("inputRating", label = "Rating:"),
              width = 6
            )
          ),
          footer = tagList(
            actionButton(ns("prev_button"), "Previous"),
            actionButton(ns("next_button"), "Next"),
            modalButton("Save and Close")
          )
        )
      }
      # Show modal dialog on start up
      rownum <- reactiveVal()
      lastrow <- dim(data)[1]
      rownum(selectedRow())
      cat(file=stderr(), rownum(), "\n")
      showModal(myModal(itemCol(),rownum()))
      
      observeEvent(input$next_button, {
        # removeModal()
        rownum(rownum() + 1)
        if (rownum() <= lastrow){
        # cat(file=stderr(), rownum, "\n")
        showModal(myModal(itemCol(),rownum()))
        }
        
      })

      
    }
  )
}

