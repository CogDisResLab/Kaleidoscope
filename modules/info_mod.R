info_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    actionBttn(ns("btn"), icon = icon("info"), label = NULL,
               style = "material-circle", 
               color = "primary", 
               size = "xs"
               )
  )
}


info_server <- function(id, ...) {
  moduleServer(
    id,
    function(input, output, session) {
      
      list(
        btn = reactive(input$btn)
      )
      
    }
  )
}