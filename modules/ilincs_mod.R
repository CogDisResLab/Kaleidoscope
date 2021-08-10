
# Gene Text Input Module UI function

iincs_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 12,  geneInputUI(ns("genes"))),
      column(width = 6, echarts4rOutput(ns("plot1"), height = "500")),
      column(width = 6, echarts4rOutput(ns("plot2"), height = "500")),
      column(width = 12, reactableOutput(ns("table")),
             div(id = ns("dn_btn_div"), download_btn_ui(ns("dn_btn")))
             )
    )
  )
  
}


# Gene Text Input Module Server function

ilincs_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      genes_ids <- geneOutput("genes")
      
      observeEvent(genes_ids$info_btn(),{
        shinyalert(title = NULL, text = ilincs_info_html, html = T, closeOnEsc = T, closeOnClickOutside = T)
        
      })
      
      observeEvent(genes_ids$btn(), {
        
        genes <- isolate(genes_ids$genes())
        #req(genes)
        
        shinyjs::hide("plot1")
        shinyjs::hide("plot2")
        shinyjs::hide("table")
        shinyjs::hide("dn_btn_div")
        
        genes <- process_gene_input(genes)
        
        lnc_res <- withProgress(message = "connecting to iLINCS ...", {
          ks_ilincs(genes = genes, knockdown = T, overexpression = T) 
        })
        
        if(!is.null(lnc_res)) {
        
          output$plot1 <- renderEcharts4r({
            
            lnc_res %>% 
              count(Gene, Type) %>% 
              group_by(Type) %>% 
              arrange(Type,n) %>% 
              e_charts(Gene, reorder = F, renderer="svg") %>% 
              e_bar(n, position = "right") %>% 
              e_flip_coords() %>% 
              #e_labels(position = "right") %>% 
              e_title("iLINCS Signatures", subtext = "Number of gene knockdown or over expression signatures") %>% 
              e_toolbox() %>%
              e_toolbox_feature(feature = c("saveAsImage", "dataView")) %>% 
              e_tooltip()
          })
          
          output$plot2 <- renderEcharts4r({
            
            lnc_res %>% 
              group_by(Type) %>% 
              mutate(
                Tissue = ifelse(is.na(Tissue), "NA", Tissue),
                Tissue = forcats::fct_lump(Tissue, 5)
              ) %>% 
              count(Tissue) %>% 
              e_charts(Tissue, timeline = TRUE, renderer="svg") %>% 
              e_pie(n, roseType = "radius") %>% 
              e_labels() %>% 
              e_title("iLINCS Signatures", subtext = "Number of signatures per tissue type (derived from cell line) - Showing only top 5, and the rest grouped under 'Other") %>% 
              e_toolbox() %>%
              e_toolbox_feature(feature = c("saveAsImage", "dataView")) %>% 
              e_tooltip()
            
          })
          
          output$table <- renderReactable({
            reactable(lnc_res, 
                      searchable = TRUE,
                      striped = TRUE,
                      bordered = TRUE,
                      filterable = TRUE,
                      columns = list(
                        Link = colDef(
                          cell = function(value) {
                            htmltools::tags$a(href = value, target = "_blank", "Link")
                          },
                          style = list(color = "blue")
                          ),
                        
                        Type = colDef(cell = function(value) {
                          class <- paste0("tag status-", tolower(gsub(" ", "", value)))
                          div(class = class, value)
                        })
                        
                        )
                      )
          })
          
          download_btn_server(id = "dn_btn", 
                              tbl = lnc_res,
                              name = "iLINCS_Table")
          
          shinyjs::show("plot1")
          shinyjs::show("plot2")
          shinyjs::show("table")
          shinyjs::show("dn_btn_div")
          
        }
        
        else {
          shinyalert("Opps", "Couldn't Find Your Gene/s", type = "error")
        }
        
        shinyFeedback::resetLoadingButton("genes-btn")
          
          
      })
      }
  )
  }


#KD = LIB_6, OE = LIB_11
lincs_api_call <- function(x, lib = c("LIB_6", "LIB_11")) {
  
  url <- paste0('http://www.ilincs.org/api/SignatureMeta?filter={"where":{"treatment":"', x,'","libraryid":"',lib,'"}}' )
  
  table_res <- httr::GET(url)
  
  if(table_res$status == 200) {
    
    table_res_cont <- httr::content(table_res)
    
    if(!identical(table_res_cont, list())) {
      
      table_res_cont %>% 
        purrr::map_df(unlist) %>% 
        select(signatureid,cellline, compound, tissue, libraryid)
      
    }
    
    else {NULL}
    
  }
  
  else {return(NULL)}
  
}

'http://www.ilincs.org/api/SignatureMeta?filter={"where":{"treatment":"sdfkjhkjh","libraryid":"LIB_6"}}'

ks_ilincs <- function(genes, knockdown = T, overexpression = F) {
  
  if(all(knockdown, overexpression)) {
    fin_res <- purrr::map_df(genes, lincs_api_call, lib = "LIB_6") %>% 
      rbind(purrr::map_df(genes,lincs_api_call, lib = "LIB_11"))
  }
  
  else if(knockdown) {
    fin_res <- purrr::map_df(genes, lincs_api_call, lib = "LIB_6")
  }
  
  else if(overexpression) {
    fin_res <- purrr::map_df(genes, lincs_api_call, lib = "LIB_11")
  }
  
  else {fin_res <- tibble()}
  
  if(nrow(fin_res) > 0) {
    
  fin_res %>% 
    rename(Gene = compound, Sig_ID = signatureid, CellLine = cellline,Tissue = tissue, Type = libraryid) %>% 
    mutate(CellLine = stringr::str_extract(CellLine, "[^.]+"),
           Tissue = stringr::str_to_title(Tissue),
           Type = ifelse(Type == "LIB_6", "Knockdown", "Over Expression"),
           Link = paste0("http://www.ilincs.org/ilincs/signature/", Sig_ID)) %>% 
      select(Sig_ID, Gene, everything())
  }
  
  else(NULL)

}

ilincs_info_html <- HTML('<div class="features-icon"><span class="icon-lifesaver"></span></div>
                  <h3 class="features-title font-alt">iLINCS</h3>
                  <p>The integrative web platform for analysis of LINCS data and LINCS L1000 signatures. Uses the L1000 assay which is a gene-expression profiling assay based on the direct measurement of a reduced representation 
of the transcriptome and computational inference of the portion of the transcriptome not explicitly measured under different 
perturbations (like genes knockdown, drugs treatments, gene overexpression .. etc).</p>
                         <h3 class = "features-title font-alt">Resources</h3>
                         <p>
                         <a href="https://www.biorxiv.org/content/10.1101/826271v1" target="_blank" rel="noopener noreferrer">Preprint</a>
                         <br>
                         <a href="http://www.ilincs.org/ilincs/" target="_blank" rel="noopener noreferrer">Website</a>
                         </p>
                         ')

