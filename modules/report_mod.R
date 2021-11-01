
# Gene Text Input Module UI function

report_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    #shinyFeedback::loadingButton(ns("dl"), "Generate Report", class = "btn btn-default action-button shiny-bound-input"),
    fluidRow(
     #tags$head(tags$script(src="ks_custom_functions.js")),
     column(width = 2, 
            textInput(ns("genes"), "Enter gene target(s) (HGNC Symbols) separated by commas", placeholder = "e.g. AKT1, NRXN1"),
            downloadButton(ns("dl"), label = "Generate Report")
            #mui_ratingInput("rate-me"),
            #shinymui::mui_checkboxInput("rate-me2"),
            ),
     column(width = 10, 
            includeHTML("www/report_tab_ui.html")
     
  )
    
  )
  )
  
  #hostess_loader("load", text_color = "black", center_page = TRUE)
  
}




# Gene Text Input Module Server function

report_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      #ns <- session$ns
      
      #hostess <- Hostess$new("loader", infinite = TRUE)
      #hostess$start()
      
      output$dl <- downloadHandler(

        # Create a file name using the response ID
        filename = function() {
          paste0(Sys.time(), "-KS-Report.html")
        },
        content = function(file) {
          #req(input$genes)

          # waiter_show( 
          #   html = spin_fading_circles() ,
          #   hostess_loader(
          #     "loader", 
          #     preset = "circle", 
          #     text_color = "black",
          #     class = "label-center",
          #     center_page = TRUE
          #   )
          # )
          # 
          # hostess <- Hostess$new("loader", infinite = TRUE)
          # hostess$start()
          
          input_genes <- process_gene_input(input$genes) 
          
          if(any(identical(input_genes, character(0)), input_genes == "")) {
            shinyalert("Opps", "Enter a valid gene symbol", type = "error")
          }
          
          else {
          
          
          
          # process file in temp directory to avoid any user-permissions problems upon deployment
          temp_path1 <- file.path(tempdir(), "ks-report.Rmd")
          file.copy(paste0("www/", "ks-report.Rmd"), temp_path1, overwrite = TRUE)
          
          temp_path2 <- file.path(tempdir(), "ks_logo_new.svg")
          file.copy(paste0("www/", "assets/images/ks_logo_new.svg"), temp_path2, overwrite = TRUE)
          
          temp_path3 <- file.path(tempdir(), "string_embedded_network_v2.0.2.js")
          file.copy("www/string_embedded_network_v2.0.2.js", temp_path3, overwrite = TRUE)
          # 
          # temp_path4 <- file.path(tempdir(), "footer_report.html")
          # file.copy("www/footer_report.html", temp_path4, overwrite = TRUE)
          
          # Show a notification that report is generating
          # report_id <- shiny::showNotification(
          #   "Generating report...",
          #   duration = NULL,
          #   closeButton = FALSE
          # )
          # on.exit(shiny::removeNotification(report_id), add = TRUE)
          
          #shinybusy::show_modal_spinner(text = "Generating report ...")
          
          report_id <- shiny::showModal(
            modalDialog(
              title = "Generating report ..."
            )
          )
          #on.exit(shiny::removeModal(report_id))
          
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
          
          
          #hostess$close()
          #waiter_hide()
          
          #shinybusy::remove_modal_spinner()
          
          removeModal()
        }
        }
      )
      
    }
  )
}

