
# enrichr link
getEnrichrLink <- function(x) {
  if(is_empty(x) || x == "") {
    paste0("https://amp.pharm.mssm.edu/Enrichr/")
  }
  else {
    r <- jsonlite::fromJSON(content(POST("http://amp.pharm.mssm.edu/Enrichr/addList", 
                                         body = list(list = x)), as = "text")) 
    Sys.sleep(1)
    paste0("https://amp.pharm.mssm.edu/Enrichr/enrich?dataset=", r$shortId)
  }
}