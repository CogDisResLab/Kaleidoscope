
enrichr_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    actionButton(ns("btn"), "Enrichr", icon = icon("dna"))
  )
  
  
}


enrichr_server <- function(id, genes) {
  moduleServer(
    id,
    function(input, output, session) {
      
      enrichrLink <- ks_getEnrichrLink(paste(genes, collapse = "\n"))
      #onclick(btn, runjs(paste0("window.open('",enrichrLink,"')")))
      
      list(
        enrichrLink = reactive(input$gene_input),
        btn = reactive(input$btn)
      )
      
    }
    
  )
  
}

# enrichr link
ks_getEnrichrLink <- function(x) {
  if(is_empty(x) || x == "") {
    paste0("https://amp.pharm.mssm.edu/Enrichr/")
  }
  else {
    r <- jsonlite::fromJSON(content(POST("http://amp.pharm.mssm.edu/Enrichr/addList", 
                                         body = list(list = x)), as = "text")) 
    Sys.sleep(1/2)
    paste0("https://amp.pharm.mssm.edu/Enrichr/enrich?dataset=", r$shortId)
  }
}
