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
    
    # fluidRow(
    #   column(12, textInput(ns("gene_input"), label,
    #                        placeholder = placeholder, value = "")),
    #   column(2,shinyFeedback::loadingButton(ns("btn"), "Submit", 
    #                                          class = "btn btn-default action-button shiny-bound-input")),
    #   column(1,div(actionBttn(ns("info_btn"), 
    #                            icon = icon("info"), 
    #                            label = "Info",
    #                            style = "unite", 
    #                            color = "primary", 
    #                            size = "xs"
    #   ), style = "padding-left = 20px;"))
      # shinyFeedback::loadingButton(ns("btn"), "Submit", 
      #                              class = "btn btn-default action-button shiny-bound-input"),
      # div(actionBttn(ns("info_btn"), 
      #                icon = icon("info"), 
      #                label = "Info",
      #                style = "unite", 
      #                color = "primary", 
      #                size = "xs"
      # ))
      # column(1, div(class ="features-icon", icon("", class = "icon-map"), h2("GWAS"))
      #        ),
      
      # column(2, div(actionBttn(ns("info_btn"), 
      #                          icon = icon("info"), 
      #                          label = "Info",
      #                      style = "unite", 
      #                      color = "primary", 
      #                      size = "xs"
      # ), style = "padding-top: 50px")
      # ),
      # column(10, shinyFeedback::loadingButton(ns("btn"), "Submit", 
      #                                         class = "btn btn-default action-button shiny-bound-input"))
    #)
  #textInput(ns("gene_input"), label,
  #          placeholder = placeholder, value = ""),
  #shinyFeedback::loadingButton(ns("btn"), "Submit", class = "btn btn-default action-button shiny-bound-input"),
  #actionButton(ns("btn2"), "Submit"),
  # actionBttn(ns("info_btn"), icon = icon("info"), label = NULL,
  #           style = "material-circle", 
  #           color = "primary", 
  #           size = "xs"
  # )
  #process_input_ui(ns("err_msg")),
  #textOutput(ns("gene_output")),
  #verbatimTextOutput(ns("gene_output_2"))
  )
}


# Gene Text Input Module Server function

geneOutput <- function(id, ...) {
  moduleServer(
    id,
    function(input, output, session) {
      
      #req(reactive(input$gene_input))
      
      list(
        genes = reactive(input$gene_input), 
        btn = reactive(input$btn),
        info_btn = reactive(input$info_btn)
      )
 
    }
    
  )
  
}



