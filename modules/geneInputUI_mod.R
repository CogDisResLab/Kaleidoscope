# Gene Text Input Module UI function

geneInputUI <- function(id, label = "Enter gene target(s) (HGNC Symbols) separated by commas", placeholder = "e.g. AKT1, NRXN1") {

  ns <- NS(id)
  
  tagList(
    
    tagList(
      textInput(ns("gene_input"), label,
                placeholder = placeholder, value = ""),
      shinyFeedback::loadingButton(ns("btn"), "Submit", class = "btn btn-default action-button shiny-bound-input"),
      actionBttn(ns("info_btn"), icon = icon("info"), label = NULL,
                 style = "material-circle", 
                 color = "primary", 
                 size = "xs"
      )
    )
  )
}


# Gene Text Input Module Server function

geneOutput <- function(id, ...) {
  moduleServer(
    id,
    function(input, output, session) {
      
      list(
        genes = reactive(input$gene_input), 
        btn = reactive(input$btn),
        info_btn = reactive(input$info_btn)
      )
 
    }
    
  )
  
}



