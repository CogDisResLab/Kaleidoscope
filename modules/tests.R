library(shiny)
library(shinyWidgets)
#library(shinyjs)


mod1_ui <- function(id) {
  ns <- NS(id)
  tagList(
    #useShinyjs(),
    actionButton(ns("mod1_btn"), label = "button 1"),
  )
}

mod1_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      toggle("mod1_btn")
      
    }
  )
}

mod2_ui <- function(id) {
  ns <- NS(id)
  tagList(
    #useShinyjs(),
    pickerInput(inputId=ns("dbs2"),label="MDD",choices=c("hi", "there"),  multiple = F, selected = "hi"),
    #mod1_ui("from_mod1"),
    #actionButton(ns("mod2_btn"), label = "button 3"),
  )
}

mod2_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      #tab1_inputs <- mod1_server("from_mod1")
      observeEvent(input$mod2_btn, {
        #mod1_server("from_mod1")
        toggle(session$ns("from_mod1-mod1_btn"))
      })
    }
  )
}

ui <- fluidPage(
  # useShinyjs(),
  mod2_ui("home_page")
  
)
server <- function(input, output, session) {
  #mod2_server("home_page")
}
shinyApp(ui, server)