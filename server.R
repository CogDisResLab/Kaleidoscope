server<- shinyServer(function(input, output, session){
  
  brainrnaseq_server("brainrnaseqTab")
  string_server("stringTab")
  ilincs_server("lincsTab")
  braincloud_server("braincloudTab")
  gwascatalog_server("gwascatalogTab")
  gtex_server("gtexTab")
  brainatlas_server("brainatlasTab")
  lookup_server("lookupTab")
  pharos_server("pharosTab")
  report_server("reportTab")
  
  
  
  session$onSessionEnded(stopApp)

})