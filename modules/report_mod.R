
# Gene Text Input Module UI function

report_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    use_waiter(),
    use_hostess(), 
    
    fluidRow(
     column(width = 2, 
            textInput(ns("genes"), "Enter gene target(s) (HGNC Symbols) separated by commas", placeholder = "e.g. AKT1, NRXN1"),
            downloadButton(ns("dl"), label = "Generate Report")

            ),
     column(width = 10, 
            hostess_loader("load", text_color = "black",
                           center_page = TRUE, min = 15,
                           svg = "ks_logo_new.svg",  progress_type = "fill"),
            includeHTML("www/report_tab_ui.html")
     
  )
    
  )
  )

}

# Gene Text Input Module Server function

report_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      hostess <- Hostess$new("loader", infinite = TRUE, min = 20)
      
      output$dl <- downloadHandler(

        # Create a file name using the response ID
        filename = function() {
          paste0(Sys.time(), "-KS-Report.html")
        },
        content = function(file) {

          waiter_show(color = "#bdbaba", 
                      html = hostess$get_loader(
                        text_color = "black",
                        center_page = TRUE,
                        svg = "assets/images/ks_logo_new.svg", 
                        progress_type = "fill", 
                        fill_direction = sample(c("btt"), 1)
                        )
                      )
          
          hostess$start()

          input_genes <- process_gene_input(input$genes)
          
          if(any(identical(input_genes, character(0)), input_genes == "")) {
            shinyalert("Opps", "Enter a valid gene symbol", type = "error")
            
            hostess$close()
            waiter_hide()

          }
          
          else {

          temp_path1 <- file.path(tempdir(), "ks-report.Rmd")
          file.copy(paste0("www/", "ks-report.Rmd"), temp_path1, overwrite = TRUE)
          
          temp_path2 <- file.path(tempdir(), "ks_logo_new.svg")
          file.copy(paste0("www/", "assets/images/ks_logo_new.svg"), temp_path2, overwrite = TRUE)
          
          temp_path3 <- file.path(tempdir(), "string_embedded_network_v2.0.2.js")
          file.copy("www/string_embedded_network_v2.0.2.js", temp_path3, overwrite = TRUE)
          
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
          
          hostess$close()
          waiter_hide()
          
        }
        }
      )
      
    }
  )
}

