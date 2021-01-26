
# uploading a dataset
addingds <- function(ds) {
  withProgress(message = 'adding dataset ...', value = 0.1, { 
    
    # tryCatch({
    #     
    #     
    # })
    
    if(!is.data.frame(ds)) {stop("Make sure it's a dataframe")}
    if(ncol(ds) != 4) {stop("Number of columns have to be 4")}
    if(!all(colnames(ds) %in% c("Gene_Symbol", "Log2FC", "P_Value", "DataSet"))) {
      stop("check columns names, they should be: Gene_Symbol, Log2FC, P_Value, DataSet")
    }
    
    if(!is.character(ds$Gene_Symbol)) {stop("check that Gene_Symbol column is as charcter column")}
    if(!is.character(ds$DataSet)) {stop("check that DataSet column is as charcter column")}
    if(!is.numeric(ds$Log2FC)) {stop("check that Log2FC column is as charcter column")}
    if(!is.numeric(ds$P_Value)) {stop("check that P_Value column is as charcter column")}
    
    tbl(my_db, "lookup_userdefined_meta") %>% 
      collect() %>% pull(DataSet) -> existingds
    
    incProgress(0.3)
    tbl(my_db, "lookup_new_meta") %>% 
      collect() %>% pull(DataSet) -> existingds2
    
    if(any(ds$DataSet %>% unique() %in% c(existingds, existingds2))) {
      stop("The DataSet name is not unique, please change it")}
    
    incProgress(0.4)
    ds %>% na.omit(Gene_Symbol) %>% 
      mutate(Gene_Symbol = toupper(Gene_Symbol), Group = "User-Defined_DS_sc") %>% 
      mutate(Fold_Change = 2^Log2FC) %>% 
      group_by(Gene_Symbol) %>% mutate(Fold_Change = EnvStats::geoMean(Fold_Change),
                                       P_Value = EnvStats::geoMean(P_Value)
      ) %>% ungroup() %>% distinct(Gene_Symbol, DataSet, .keep_all = T) %>% mutate( 
        Log2FC = log2(Fold_Change),
        Log2FCAbs = abs(Log2FC),
        Fold_Change = ifelse(Fold_Change >= 1, Fold_Change, -1/Fold_Change)
      ) %>% group_by(DataSet) %>% 
      mutate(ecdf = ecdf(Log2FC)(Log2FC)*2-1, ecdfPlot = ifelse(ecdf < 0, ecdf * -1, ecdf), 
             ecdfPlot = ifelse(Log2FC<0, ecdfPlot*-1,ecdfPlot)) %>% ungroup() %>% 
      select(-ecdf) %>% mutate(Dir = ifelse(Log2FC>=0, "Up", "Down")) -> ds_2
    
    incProgress(0.7)
    pre_sc_table <<- rbind(pre_sc_table, ds_2)
    
    message("Ds was added")
    incProgress(0.9)
  })
  shinyalert("Thanks", "Your dataset was uploaded successfully", type = "success")
  pre_sc_table
  #"success"
  
  
}