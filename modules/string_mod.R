
# Gene Text Input Module UI function

string_ui <- function(id) {
  
  ns <- NS(id)
  
  sting_md <-
    bs_modal(
      id = ns("string_info"),
      title = "STRING",
      body = HTML("Hi There"),
      size = "medium"
    )
  
  tagList(
    fluidRow(
      column(width = 4,
             geneInputUI(ns("genes"), label = "Enter a single gene or protein target (HGNC Symbol)", placeholder = "e.g. NRXN1")
              
             ),
      column(width = 4, 
             sliderInput(ns("node_slider"), label = "Nodes", min = 1,
                         max = 100, value = 20, step = 1) %>% 
               shinyInput_label_embed(
                 htmltools::tags$a(shiny::icon(name = "info-circle"), href = "javascript:;") %>% 
                   bs_embed_popover(
                     title = "Number of Nodes",content  = "Max number of interactors (other proteins) to show", 
                     placement = "bottom", trigger = "focus"
                   )
               )
             
             ),
      column(width = 4, 
             sliderInput(ns("score_slider"), label = "Score", min = 0,
                         max = 999, value = 500, step = 10) %>% 
               shinyInput_label_embed(
                 htmltools::tags$a(shiny::icon(name = "info-circle"), href = "javascript:;") %>% 
                   bs_embed_popover(
                     title = "Score",content  = "Minimum required interaction score (High score means only showing high confidence associations)", 
                     placement = "bottom", trigger = "focus"
                   )
               )
      )
             ),hr(),
    fluidRow(
      column(width = 6,
             plotOutput(ns("string_img"), height = "700px"), 
             shinyjs::hidden(div(id = ns("string_info"),
                                 img(src = "stringInfo.png", width = "100%", hight = "500px")))
             ),
      column(width = 6,
             reactableOutput(ns("table")),
             verbatimTextOutput(ns("interactors_genes")),
             #uiOutput(ns("clip")),
             hidden(actionButton(ns("enrichr_btn"), "Submit to Enrichr")),
             div(id = ns("dn_btn_div"), download_btn_ui(ns("dn_btn")))
             #enrichr_ui(ns("en_btn"))
             #shinyjs::hidden(actionButton(ns("trnsNet"), "Copy")),
             #actionButton(ns("enrichrBtn1"), "Enrichr", icon = icon("dna"))
             
      )
            
             ),
    )

  
}


# Gene Text Input Module Server function

string_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      genes_ids <- geneOutput("genes")
      
      observeEvent(genes_ids$info_btn(),{
        shinyalert(title = NULL, text = string_info_html, html = T, closeOnEsc = T, closeOnClickOutside = T)
        
      })
      

      observeEvent(genes_ids$btn(), {
        
        shinyjs::hide("string_img")
        shinyjs::hide("table")
        shinyjs::hide("interactors_genes")
        shinyjs::hide("string_info")
        shinyjs::hide("enrichr_btn")
        shinyjs::hide("dn_btn_div")
        #shinyjs::hide("stringTab-enrichr_btn-btn")
        
        req(genes_ids$genes())
        
         string_output <- withProgress(message = "connecting to STRING ...", {
           ks_string(process_gene_input(genes_ids$genes(), type = "single"),score = input$score_slider, nodes = input$node_slider)
           })
         
         if(!is.null(string_output)) {
         
         output$string_img <- renderPlot({
           plot.new()
           rasterImage(string_output$image,0,0,1,1)
           })
         shinyjs::show("string_img") 
         shinyjs::show("string_info")  

         output$interactors_genes <- renderText(paste(string_output$genes, collapse = ","))
         #enrichr_server("enrichr_btn", string_output$genes)

         output$table <- renderReactable({

           reactable(select(string_output$table, -Description) %>% 
                       mutate_if(is.numeric, convert_na), 
                     searchable = TRUE,
                     striped = TRUE,
                     bordered = TRUE,
                     defaultColDef = colDef(
                       #width = 75,
                       align = "center",
                       #headerStyle = list(fontSize = '7.5px', align = "center"),
                       cell = function(value) {
                         color <- rating_color(value)
                         value <- ""
                         div(class = "spi-rating", style = list(background = color), value)
                       }
                     ),
                     
                     columns = list(
                       Protein = colDef(
                         cell = function(value) {
                           value = value
                         }
                       )
                       
                     ), 
                     
                     details = function(index) {
                       paste(string_output$table$Description[[index]])
                     }
           )
           
           })
         
         enrichrLink <- ks_getEnrichrLink(paste(string_output$genes, collapse = "\n"))
         onclick("enrichr_btn", runjs(paste0("window.open('",enrichrLink,"')")))
         
         download_btn_server(id = "dn_btn", 
                             tbl = string_output$table %>% 
                               mutate_if(is.numeric, convert_na),
                             name = "STRING_Table")
         
         #en_res <<- enrichr_server("en_btn",genes = string_output$genes)
         #paste0(en_res$enrichrLink)
         #onclick(en_res$btn, runjs(paste0("window.open('",en_res$enrichrLink,"')")))
         
         #enrichr_server("enrichr_btn",genes = string_output$genes)
         # output$clip <- renderUI({
         # rclipboard::rclipButton("clipbtn", "Copy", paste(string_output$genes, collapse = ","), icon("clipboard"), modal = T)
         # })
         
         shinyjs::show("table")
         shinyjs::show("interactors_genes")
         shinyjs::show("enrichr_btn")
         shinyjs::show("dn_btn_div")
         #shinyjs::show("stringTab-enrichr_btn-btn")
         }
         
         else {
           shinyalert("Opps", "Couldn't Find Your Gene/s", type = "error")
         }
         
         shinyFeedback::resetLoadingButton("genes-btn")

      })
    
    }
    
  )
  
}



ks_string <- function(gene, score, nodes, get_img = T, high_res = F) {
  
  
  # get network image
  if (get_img == T) {
    img_api_url <- paste0("http://version-11-0.string-db.org/api/",ifelse(high_res, "highres_image", "image"),"/network?identifier=", gene, 
                  "&required_score=",score,"&limit=",nodes,
                  "&species=9606&network_flavor=evidence")
    
    img_res <- httr::GET(img_api_url)
    
    if(img_res$status_code == 200) {
      img_plot <- httr::content(img_res)
      
    }
    
  }
  
  # get interactors 
  api_url <- paste0("http://version-11-0.string-db.org/api/json/interaction_partners?identifiers=",
                    gene,"&species=9606&required_score=",
                    score,"&limit=",nodes)
  
  table_res <- httr::GET(api_url)
  
  meta_tag <- F
  
  if(table_res$status < 300) {
    table_string <- jsonlite::fromJSON(httr::content(table_res))
    
    message("got interactors")
    
    IdList <- c(gene, dplyr::pull(table_string, preferredName_B))
    
    table_string <- dplyr::add_row(table_string, preferredName_B = gene) %>% 
      dplyr::select(-preferredName_A, -stringId_A,-stringId_B,-ncbiTaxonId) %>% 
      dplyr::rename(preferredName = preferredName_B)
    
    
    #  Get gene info
    intr_info_api_url <- paste0("http://version-11-0.string-db.org/api/json/resolve?identifiers=",
                                paste(IdList,collapse ="%0D"),
                                "&species=9606")
    
    table_info_res <- httr::GET(intr_info_api_url)
    
    if(table_info_res$status < 300) {
      table_string_meta <- jsonlite::fromJSON(httr::content(table_info_res))
      
      message("got meta info")
      
      table_string_meta <- dplyr::select(table_string_meta,preferredName, annotation)
      
      interactors <- dplyr::pull(table_string_meta, preferredName)
      
      table_string_meta %>% 
        dplyr::full_join(table_string) %>% 
        dplyr::select(preferredName, score, annotation, ends_with("score")) %>% 
        dplyr::arrange(desc(score)) %>% 
        dplyr::rename(Protein = preferredName, CombinedScore = score, Description = annotation,Neighborhood = nscore, 
                      `Gene Fusion` =  fscore, Phylogenetic = pscore, 
                      Database = dscore, Textmining = tscore, CoExpression = ascore,
                      Experimental = escore) %>% 
        dplyr::select(Protein, CombinedScore, Description, Experimental, Database, everything()) -> table_string_final
      
      meta_tag <- T
    }
    
    if(all(table_res$status < 300, meta_tag)) {
      
      return(list(
        table = table_string_final,
        genes = interactors,
        image = img_plot 
      ))
    }
    else {
      message("Can't find Target")
      return(NULL)
    }
    
  }
  
  
}

# functions for reactable style

make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}

rating_color <- make_color_pal(c("#e8e8e8","#969696","#050505"), bias = 1.3)

convert_na <- function(x, value = 1) {
  
  x <- ifelse(is.na(x), value, x)
  
  round(x,  4)
}



string_info_html <- HTML('<div class="features-icon"><span class="icon-genius"></span></div>
                  <h3 class="features-title font-alt">STRING</h3>
                  <p>Database of known and predicted protein-protein interactions. The interactions include direct (physical)
                  and indirect (functional) associations; they stem from computational prediction, from knowledge transfer between organisms, 
                  and from interactions aggregated from other (primary) databases.</p>
                         <h3 class = "features-title font-alt">Resources</h3>
                         <p>
                         <a href="https://pubmed.ncbi.nlm.nih.gov/30476243/" target="_blank" rel="noopener noreferrer">PMID: 30476243</a>
                         <br>
                         <a href="https://string-db.org/cgi/info" target="_blank" rel="noopener noreferrer">Website</a>
                         </p>
                         ')


    

