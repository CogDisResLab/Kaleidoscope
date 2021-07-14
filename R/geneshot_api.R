library(httr)
library(tidyverse)

# example search term
# geneShotTermIn <- gsub("\\s+", "+", "Pancreatic cancer") %>% toupper()
# 
# #api url
# geneShoturl <- paste0("http://amp.pharm.mssm.edu/geneshot/api/search/auto/", geneShotTermIn)
# 
# # api call
# geneShotOutput <- content(GET(geneShoturl))
# 
# # reformat api results
# if (length(geneShotOutput$gene_count) > 0) {
#   enframe(geneShotOutput$gene_count) %>% unnest(cols = "value") %>% 
#     group_by(name) %>% 
#     mutate(col=seq_along(name)) %>% ungroup() %>% 
#     spread(key=col, value=value) %>% rename(Gene = name, 
#                                             Publications = `1`,
#                                             Normalized = `2`
#     )  %>% unnest(cols = c("Publications", "Normalized")) -> geneShotTable
# }
# 
# # filter specific genes
# full_genes <- "FRK,BLK,HCK,ABL2,DDR1,LYN,EPHA8,FYN,LCK,TEC,EGFR,FLT4,INSR,EPHA2,PDGFRA,SRC,TNK2"  
# strsplit(full_genes, ",")[[1]] -> full_genes
# 
# geneShotTable %>% filter(Gene %in% full_genes) %>% 
#   rename(`Targeted Publications` = Publications, 
#          `Targeted Publications / Total Publications` = Normalized
#   ) 