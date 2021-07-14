process_input_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    htmlOutput(ns("err_msg"))
  )
}


# Gene Text Input Module Server function

process_input_server  <- function(id, ...) {
  moduleServer(
    id,
    function(input, output, session) {
      
      final_genes <- process_gene_input(...)
      
      if(identical(final_genes, character(0))) {
        status <- F
        msg <- "Not Found"
        
      }
      
      else {
        status <- T
        msg <- "Found"
      }
      
      output$err_msg <- renderText({ paste("<font color=\"#FF0000\"><b>",msg) })
      

      
      res <- list(
        genes = reactive(final_genes),
        status = status
      )
      
      return(res)
      
      
      
      
    }
    
  )
  
}

process_gene_input <- function(x, type = "multiple", toupper = T, sep = ",") {
  
  x <- gsub("\\s+", "", x)
  
  if(type == "multiple") {
    final_genes <- base::strsplit(
      ifelse(toupper, toupper(x), x),
      split = sep)[[1]]
  }
  
  else {final_genes <- ifelse(toupper, toupper(x), x)}
  
  return(final_genes)
  
}




