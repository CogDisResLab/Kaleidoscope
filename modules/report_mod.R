
# Gene Text Input Module UI function

report_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 12,  textInput(ns("genes"), "What's your gene?")),
      column(width = 12,  downloadButton(ns("dl"), label = "Generate Report"), hr()),
      column(width = 12,  switchInput(inputId = ns("brainseq_par"),size = "normal", label = "BrainSeq", value = T)),
      column(width = 12,  switchInput(inputId = ns("ilincs_par"),size = "normal", label = "iLINCS", value = T)),
      column(width = 12,  switchInput(inputId = ns("gwas_par"),size = "normal", label = "GWAS Catalog", value = T)),
      column(width = 12,  switchInput(inputId = ns("braincloud_par"),size = "normal", label = "BrainCloud", value = T)),
      column(width = 12,  switchInput(inputId = ns("string_par"),size = "normal", label = "STRING", value = T)),
      column(width = 12,  switchInput(inputId = ns("brainatlas_par"),size = "normal", label = "BrainAtlas", value = T)),
      column(width = 12,  switchInput(inputId = ns("gtex_par"),size = "normal", label = "GTEx", value = T))
      
    )
  )
  
}




# Gene Text Input Module Server function

report_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$dl <- downloadHandler(
        # Create a file name using the response ID
        filename = function() {
          paste0(Sys.time(), "-KS-Report.html")
        },
        content = function(file) {
          
          # process file in temp directory to avoid any user-permissions problems upon deployment
          temp_path1 <- file.path(tempdir(), "ks-report.Rmd")
          file.copy(paste0("www/", "ks-report.Rmd"), temp_path1, overwrite = TRUE)
          
          temp_path2 <- file.path(tempdir(), "ks_logo_new.svg")
          file.copy(paste0("www/", "assets/images/ks_logo_new.svg"), temp_path2, overwrite = TRUE)
          
          temp_path3 <- file.path(tempdir(), "gwas.jpeg")
          file.copy(paste0("www/", "assets/images/gwas.jpeg"), temp_path3, overwrite = TRUE)
          
          # Show a notification that report is generating
          report_id <- shiny::showNotification(
            "Generating report...",
            duration = NULL,
            closeButton = FALSE
          )
          on.exit(shiny::removeNotification(report_id), add = TRUE)
          
          # setup parameters to pass into Report to pass in the customer's name, for
          # example, create a list accessing the reactive value from Shiny:
          
          param_list <- list(gene = input$genes, 
                             brainrnaseq = input$brainseq_par,
                             ilincs = input$ilincs_par,
                             gwas = input$gwas_par,
                             braincloud = input$braincloud_par,
                             string = input$string_par,
                             brainatlas = input$brainatlas_par,
                             gtex = input$gtex_par
                             )
          
          # Knit the document in its own environment, isolating its code from the Shiny app code.
          rmarkdown::render(input = temp_path1,
                            #output_format = rmarkdown::html_document(),
                            output_file = file, # this lets the rendered report be accessed by download handler
                            params = param_list,
                            envir = new.env(parent = globalenv())
          )
        }
      )
      
    }
  )
}

