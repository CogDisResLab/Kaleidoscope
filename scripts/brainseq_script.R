library(tidyverse)
library(pool)
library(dbplyr)
library(echarts4r)

all_mice_prop <- readRDS("modules/brainseq_avg_mice_prop_fixed.RDS")

my_db <- dbPool(
  RMySQL::MySQL(), 
  dbname = "ksdatabase",
  host = "cdrlshinyapps.cdkkdi6q6ptl.us-east-2.rds.amazonaws.com",
  username = "cdrl",
  password = "cdrl_ks"
)



ks_brainseq <- function(genes, db = my_db) {
  
  tbl(my_db,"barres_mouse") %>% 
    filter(GeneSymbol %in% genes) %>% 
    collect() %>% select(-row_names) %>% 
    mutate(Species = "Mice", FPKM = round(log10(FPKM + 1), 2)) -> mice_table_one
  
  tbl(my_db,"barres_human") %>% 
    filter(Gene %in% genes) %>% collect() %>% 
    select(-row_names) %>% 
    rename(GeneSymbol = Gene) %>% 
    mutate(Species = "Human" ,FPKM = round(log10(FPKM + 1), 2)) -> human_table_one
  
  rbind(mice_table_one,human_table_one)
  
}

# list of genes
# ex
genes <- c("AKT1", "AKT2", "AKT3")

bs_res <- ks_brainseq(genes = genes, db = my_db)

bs_res_mice <-filter(bs_res, Species == "Mice")


bs_res_mice %>% 
  group_by(CellType) %>% 
  summarise(FPKM = sum(FPKM)) %>% 
  mutate(total = sum(FPKM), prob = (FPKM/total)*100, 
         Avg = "Input") %>% 
  rbind(all_mice_prop) %>% select(CellType, Avg, prob) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  pivot_wider(names_from = Avg, values_from = prob) %>% 
  e_charts(CellType, renderer="svg") %>% 
  e_radar(Avg, max = 50) %>% 
  e_radar(Input, max = 50) %>% 
  e_title("Brain RNA-Seq in Mice", subtext = "Proportion of Gene Expression values Per Cell Type") %>%
  e_tooltip() %>% 
  e_toolbox() %>%
  e_toolbox_feature(feature = c("saveAsImage", "dataView")) %>% 
  e_legend(left = "left", top = "middle", orient = "vertical")


poolClose(my_db)




