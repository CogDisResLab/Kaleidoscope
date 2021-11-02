
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
      tags$head(tags$script(src="string_embedded_network_v2.0.2.js")),
      column(width = 6,
             geneInputUI(ns("genes"), label = "Enter gene target(s) (HGNC Symbols) separated by commas", placeholder = "e.g. NRXN1")
              
             ),
      column(width = 3, 
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
      column(width = 3, 
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
      column(width = 12,
             div(
             plotOutput("stringEmbedded", height = "auto", width = "auto"),
             style="text-align: center;"
             )
             ),
      column(width = 4,
             shinyjs::hidden(div(id = ns("string_info"),
                                 img(src = "stringInfo.png", width = "100%", hight = "250px", style="padding-top: 40px;")))
             #enrichr_ui(ns("en_btn"))
             #shinyjs::hidden(actionButton(ns("trnsNet"), "Copy")),
             #actionButton(ns("enrichrBtn1"), "Enrichr", icon = icon("dna"))
             
      ),
      column(width = 8,
             reactableOutput(ns("table")),
             verbatimTextOutput(ns("interactors_genes")),
             #uiOutput(ns("clip")),
             hidden(actionButton(ns("enrichr_btn"), "Submit to Enrichr")),
             div(id = ns("dn_btn_div"), download_btn_ui(ns("dn_btn")))
             
      )
            
             ),
    )

  
}


# Gene Text Input Module Server function

string_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      w <- Waiter$new(c("stringEmbedded"), 
                      html = spin_loaders(37, color = "black"), 
                      color = transparent(.5)
      )
      
      genes_ids <- geneOutput("genes")
      
      observeEvent(genes_ids$info_btn(),{
        shinyalert(title = NULL, text = string_info_html, html = T, closeOnEsc = T, closeOnClickOutside = T)
        
      })
      

      observeEvent(genes_ids$btn(), {
        
        w$show()
        
        shinyjs::hide("stringEmbedded")
        shinyjs::hide("table")
        shinyjs::hide("interactors_genes")
        shinyjs::hide("string_info")
        shinyjs::hide("enrichr_btn")
        shinyjs::hide("dn_btn_div")
        #shinyjs::hide("stringTab-enrichr_btn-btn")
        
        #req(genes_ids$genes())
        
        genes <- process_gene_input(genes_ids$genes())
        
         string_output <- withProgress(message = "connecting to STRING ...", {
           
           if(length(genes)>1) {
             ks_string(genes,score = input$score_slider, nodes = input$node_slider, multi = T, get_img = F)
           }
           
           else {
             ks_string(genes,score = input$score_slider, nodes = input$node_slider, get_img = F)
           }
           
           
           })
         
         if(!is.null(string_output)) {
           
           print(length(genes))
           
           if(length(genes) == 1) {
             str_params <- list(
               species = "9606",
               identifiers = c(genes),
               network_flavor = "evidence",
               caller_identity = 'https://cdrl-ut.org/',
               add_white_nodes = input$node_slider,
               required_score = input$score_slider,
               single_par = T

             )
             session$sendCustomMessage("string_input", str_params)
           } 
           
           else {
             str_params <- list(
               species = "9606",
               identifiers = string_output$genes,
               network_flavor = "evidence",
               caller_identity = 'https://cdrl-ut.org/',
               single_par = F
             )
             

             session$sendCustomMessage("string_input", str_params)
           }
           
           
           
           
           
           #session$sendCustomMessage("string_input", str_params)
           
           
         shinyjs::show("stringEmbedded") 
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
                       html = T,
                       #headerStyle = list(fontSize = '7.5px', align = "center"),
                       cell = function(value) {
                         color <- rating_color(value)
                         value <- ""
                         div(class = "spi-rating", style = list(background = color), value)
                       }
                     ),
                     
                     columns = list(
                       Protein = colDef(
                         name = if(length(genes)>1) {"Interaction"} else {"Protein"},
                         width = if(length(genes)>1) {150} else {75},
                         cell = function(value) {
                           value = value
                         }
                       )
                       
                     ), 
                     
                     details = function(index) {
                       string_output$table$Description[[index]]
                     }
           )
           
           })
         
         enrichrLink <- ks_getEnrichrLink(paste(string_output$genes, collapse = "\n"))
         onclick("enrichr_btn", runjs(paste0("window.open('",enrichrLink,"')")))
         
         download_btn_server(id = "dn_btn", 
                             tbl = string_output$table %>% select(-Description) %>% 
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



ks_string <- function(gene, score, nodes, get_img = T, high_res = F, multi = F) {
  
  
  # get interactors 
  if(multi) {
    api_url <- paste0("http://version-11-0.string-db.org/api/json/network?identifiers=",
                      paste0(gene, collapse = "%0d"),"&species=9606&required_score=",
                      score)
    
  }
  
  else {
    api_url <- paste0("http://version-11-0.string-db.org/api/json/interaction_partners?identifiers=",
                      gene,"&species=9606&required_score=",
                      score,"&limit=",nodes)
  }
  
  
  table_res <- httr::GET(api_url)
  
  meta_tag <- F
  
  if(table_res$status < 300) {
    table_string <- jsonlite::fromJSON(httr::content(table_res))
    
    message("got interactors")
    
    #IdList <- c(gene, dplyr::pull(table_string, preferredName_B))
    
    if(identical(table_string, list())) {
      IdList <- gene
    }
    
    else {
      IdList <- c(gene[1], dplyr::pull(table_string, preferredName_B), dplyr::pull(table_string, preferredName_A)) %>% unique()
      
      if(multi) {
        table_string <- dplyr::select(table_string, -stringId_A,-stringId_B,-ncbiTaxonId)
      } else {
        table_string <- dplyr::add_row(table_string, preferredName_B = gene) %>% 
          dplyr::select(-preferredName_A, -stringId_A,-stringId_B,-ncbiTaxonId) %>% 
          dplyr::rename(preferredName = preferredName_B)
        
      }
    }
    
    
    
    
    
    #  Get gene info
    intr_info_api_url <- paste0("http://version-11-0.string-db.org/api/json/resolve?identifiers=",
                                paste(IdList,collapse ="%0D"),
                                "&species=9606")
    
    table_info_res <- httr::GET(intr_info_api_url)
    
    if(table_info_res$status < 300) {
      table_string_meta <- jsonlite::fromJSON(httr::content(table_info_res))
      
      message("got meta info")
      
      table_string_meta <- dplyr::select(table_string_meta,preferredName, annotation)
      
      interactors <- IdList
      
      if(identical(table_string, list())) {
        table_string_meta %>% 
          dplyr::select(preferredName, annotation) %>% 
          mutate(score = 0, nscore=0, fscore = 0, pscore = 0, dscore = 0, tscore = 0, ascore= 0, escore = 0) %>% 
          dplyr::rename(Protein = preferredName, CombinedScore = score, Description = annotation,Neighborhood = nscore, 
                        `Gene Fusion` =  fscore, Phylogenetic = pscore, 
                        Database = dscore, Textmining = tscore, CoExpression = ascore,
                        Experimental = escore) %>% 
          dplyr::select(Protein, CombinedScore, Description, Experimental, Database, everything()) -> table_string_final
      }
      
      else {
        if(multi) {
          left_join(table_string, table_string_meta, 
                    by = c("preferredName_A" = "preferredName")) %>% 
            left_join(table_string_meta, by = c("preferredName_B" = "preferredName")) %>% 
            rowwise() %>% 
            mutate(annotation = paste(paste0(preferredName_A,": ",annotation.x), paste0(preferredName_B,": ", 
                                                                                        annotation.y), sep = "<br><br>")) %>% 
            select(preferredName_A, preferredName_B, score, annotation, ends_with("score")) %>% 
            dplyr::arrange(desc(score)) %>% 
            dplyr::rename(Protein_A = preferredName_A, Protein_B = preferredName_B, 
                          CombinedScore = score, Description = annotation,Neighborhood = nscore, 
                          `Gene Fusion` =  fscore, Phylogenetic = pscore, 
                          Database = dscore, Textmining = tscore, CoExpression = ascore,
                          Experimental = escore) %>% 
            rowwise() %>% 
            mutate(Protein = paste0(Protein_A, "---", Protein_B)) %>% 
            dplyr::select(Protein, CombinedScore, 
                          Description, Experimental, Database, everything(), -Protein_A, -Protein_B) -> table_string_final
        }
        else {
          table_string_meta %>% 
            dplyr::full_join(table_string) %>% 
            dplyr::select(preferredName, score, annotation, ends_with("score")) %>% 
            dplyr::arrange(desc(score)) %>% 
            dplyr::rename(Protein = preferredName, CombinedScore = score, Description = annotation,Neighborhood = nscore, 
                          `Gene Fusion` =  fscore, Phylogenetic = pscore, 
                          Database = dscore, Textmining = tscore, CoExpression = ascore,
                          Experimental = escore) %>% 
            dplyr::select(Protein, CombinedScore, Description, Experimental, Database, everything()) %>% 
            dplyr::arrange(desc(CombinedScore)) -> table_string_final
        }
        
      }
      
      
      
      meta_tag <- T
    }
    
    if(all(table_res$status < 300, meta_tag)) {
      
      return(list(
        table = table_string_final,
        genes = interactors
        #image = img_plot 
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
                  <h1 class="features-title font-alt">STRING</h1>
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


    

