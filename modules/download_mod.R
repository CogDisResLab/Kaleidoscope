# Gene Text Input Module UI function

download_btn_ui <- function(id, label = "Download") {
  
  ns <- NS(id)
  
  tagList(
    hidden(downloadButton(ns("down"), label))
  )
}


# Gene Text Input Module Server function

download_btn_server <- function(id,tbl, name, ...) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$down <- downloadHandler(
          filename = function() {
            paste(name, "_", Sys.time(), ".csv", sep = "")
          },
          content = function(file) {
            write.csv(tbl, file, row.names = FALSE)
          }
        )
      
      show("down")

      
    }
    
  )
  
}