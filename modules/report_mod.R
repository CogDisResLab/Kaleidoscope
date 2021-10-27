
# Gene Text Input Module UI function

report_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    #shinyFeedback::loadingButton(ns("dl"), "Generate Report", class = "btn btn-default action-button shiny-bound-input"),
    fluidRow(
     column(width = 2, 
            textInput(ns("genes"), "Enter gene target(s) (HGNC Symbols) separated by commas", placeholder = "e.g. AKT1, NRXN1"),
            downloadButton(ns("dl"), label = "Generate Report")
            ),
     column(width = 10, 
      HTML('<section class="module" id="services" style="padding: 0">
          <div class="container">
            <div class="row">
              <div class="col-sm-6 col-sm-offset-3">
                <div class="module-subtitle font-serif">Select Databases and Tools to Be Included in the Report: </div>
              </div>
            </div>
            <div class="row multi-columns-row">
              <div class="col-md-3 col-sm-6 col-xs-12">
               <!-- <a href="https://string-db.org/" target="_blank" rel="noopener noreferrer"> -->
                <div class="features-item">
                  <div class="features-icon"><span class="icon-genius"></span></div>
                  <h3 class="features-title font-alt">STRING</h3>'),
      checkboxInput(ns("string_par"), label = "", value = T),
      # actionBttn(ns("info_btn"), icon = icon("cog"), label = NULL,
      #                        style = "material-circle", 
      #                        color = "default", 
      #                        size = "xs"
 
                  HTML('
                </div>
                <!--</a> -->
              </div>
              <div class="col-md-3 col-sm-6 col-xs-12">
                <div class="features-item">
                  <div class="features-icon"><span class="icon-profile-male"></span></div>
                  <h3 class="features-title font-alt">GTEx</h3>'),
    checkboxInput(ns("gtex_par"), label = "", value = T),
    HTML('
                </div>
              </div>
              <div class="col-md-3 col-sm-6 col-xs-12">
                <div class="features-item">
                  <div class="features-icon"><span class="icon-linegraph"></span></div>
                  <h3 class="features-title font-alt">BrainCloud</h3>'),
    checkboxInput(ns("braincloud_par"), label = "", value = T),
    HTML('
                </div>
              </div>
              <div class="col-md-3 col-sm-6 col-xs-12">
                <div class="features-item">
                  <div class="features-icon"><span class="icon-lifesaver"></span></div>
                  <h3 class="features-title font-alt">iLINCS</h3>'),
    checkboxInput(ns("ilincs_par"), label = "", value = T),
    HTML('
                </div>
              </div>
              <div class="col-md-3 col-sm-6 col-xs-12">
                <div class="features-item">
                  <div class="features-icon"><span class="icon-bargraph"></span></div>
                  <h3 class="features-title font-alt">BRAIN RNA-Seq</h3>'),
    checkboxInput(ns("brainseq_par"), label = "", value = T),
    HTML('
                </div>
              </div>
              <div class="col-md-3 col-sm-6 col-xs-12">
                <div class="features-item">
                  <div class="features-icon"><span class="icon-map"></span></div>
                  <h3 class="features-title font-alt">GWAS Catalog</h3>'),
    checkboxInput(ns("gwas_par"), label = "", value = T),
    HTML('
                </div>
              </div>
              <div class="col-md-3 col-sm-6 col-xs-12">
                <div class="features-item" style="opacity: 0.3">
                  <div class="features-icon"><span class="icon-search"></span></div>
                  <h3 class="features-title font-alt">Lookup</h3>'),
    shinyjs::disabled(checkboxInput(ns("lookup_par"), label = "", value = F)),
    HTML('<p>not available yet</p>
                </div>
              </div>
              <div class="col-md-3 col-sm-6 col-xs-12">
                <div class="features-item">
                  <div class="features-icon"><span class="icon-aperture"></span></div>
                  <h3 class="features-title font-alt">BrainAtlas</h3>'),
    checkboxInput(ns("brainatlas_par"), label = "", value = T),
    HTML('
                </div>
              </div>
            </div>
          </div>
        </section>')
  )

      #column(width = 3,  switchInput(inputId = ns("brainseq_par"),size = "small", label = "BrainSeq", value = T)),
      #column(width = 3,  switchInput(inputId = ns("ilincs_par"),size = "small", label = "iLINCS", value = T)),
      #column(width = 3,  switchInput(inputId = ns("gwas_par"),size = "small", label = "GWAS", value = T)),
      #column(width = 3,  switchInput(inputId = ns("braincloud_par"),size = "small", label = "BrainCloud", value = T)),
      #(width = 3,  switchInput(inputId = ns("string_par"),size = "small", label = "STRING", value = T)),
      #column(width = 3,  switchInput(inputId = ns("brainatlas_par"),size = "small", label = "BrainAtlas", value = T)),
      #column(width = 3,  switchInput(inputId = ns("gtex_par"),size = "small", label = "GTEx", value = T))
      #column(width = 3, shinyjs::disabled(switchInput(inputId = ns("lookup_par"),size = "small", label = "Lookup", value = T))),
    
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
          
          # temp_path3 <- file.path(tempdir(), "style.css")
          # file.copy("www/style.css", temp_path3, overwrite = TRUE)
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

