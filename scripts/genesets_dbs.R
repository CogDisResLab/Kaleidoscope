library(tidyverse)
library(dbplyr)
library(pool)


my_db <- dbPool(
  RMySQL::MySQL(), 
  dbname = "ksdatabase",
  host = "cdrlshinyapps.cdkkdi6q6ptl.us-east-2.rds.amazonaws.com",
  username = "khaled",
  password = "20211411kKk$%^$"
)  

dbListTables(my_db)

tbl(my_db, "lookup_updated_annotated") %>% 
  collect() %>% 
  distinct(HGNC_Symbol, DataSet, .keep_all = T) -> table_res

table_res <- rbind(table_res, new_annotaed_ds)

nrow(table_res)


count(table_res, DataSet) -> counts_ds


# all
table_res %>% select(HGNC_Symbol, DataSet, Log2FCAbs) %>% 
  group_by(DataSet) %>% 
  top_n(1000, Log2FCAbs) %>%  
  mutate(rank = row_number(desc(Log2FCAbs))) %>% 
  filter(rank <= 1000) %>% 
  mutate(DataSet = paste0(DataSet, "_all")) -> top_1000_all

# upregulated
table_res %>% select(HGNC_Symbol, DataSet, Log2FC) %>% 
  filter(Log2FC > 0) %>% 
  group_by(DataSet) %>% 
  top_n(1000, Log2FC) %>%  
  mutate(rank = row_number(desc(Log2FC))) %>% 
  filter(rank <= 1000)  %>% 
  mutate(DataSet = paste0(DataSet, "_up")) -> top_1000_up

# downregulated
table_res %>% select(HGNC_Symbol, DataSet, Log2FC) %>% 
  filter(Log2FC < 0) %>% 
  group_by(DataSet) %>% 
  top_n(1000, desc(Log2FC)) %>%  
  mutate(rank = row_number(Log2FC)) %>% 
  filter(rank <= 1000) %>% 
  mutate(DataSet = paste0(DataSet, "_down")) -> top_1000_down


names(gensets_list) %>% nchar()

rbind(top_1000_all, top_1000_up, top_1000_down) %>% 
  ungroup() %>% 
  select(HGNC_Symbol, DataSet, rank) -> top_1000

saveRDS(top_1000, "data/genesets_table_top1000_June16_2021.RDS")

dbWriteTable(my_db, value = top_1000, name = "lookup_genesets",
             field.types = c(HGNC_Symbol = "character(100)", 
                             DataSet = "character(150)",
                             rank = "integer"
             ), 
             row.names = FALSE
) 

glimpse(top_1000)

c(paste0(found_datasets[1:2], "_all"), paste0(found_datasets[1:2], "_up"), paste0(found_datasets[1:2], "_down"))  -> ss

top_1000 %>% 
  filter(DataSet %in% ss) %>% 
  select(DataSet, HGNC_Symbol) %>% 
  group_by(DataSet) %>% 
  summarize(geneset=list(unique(HGNC_Symbol))) %>% 
  deframe() -> gensets_list


genes_test <- "DISC1,DRD4,BBS4,FEZ1,ANK3,FBXW7,APP,DCTN5,ZNF804A,DPYSL2,BBS1,GSK3B,PVALB,PCM1,TRAK1,OLIG2,PDE4B,NDEL1,ATF4,DGKH,CCDC88A,SRR,DTNBP1,DAO,ERBB4,KCNQ5,PDE4D,NPAS3,PTGER3,POX2,GAD1,RHOT1,PCNT,COMT,KALRN,DRD2,GRM3,TSNAX,FMR1,GRIN1,TRAF3IP1,PDE4A,ITSN1,GRB2,YWHAZ,SLC6A4,NDE1,GRIN2A,PAFAH1B1,DLG4,NRG1,NRXN1,FOXP2,ZNF365,IMMT,SLC12A5,AXIN1,RELN,DIXDC1,CCDC141,ATF5,RGS4,TNIK,BDNF,CDK5,GRIA1,PAFAH1B2,SLC12A2,ADCYAP1,GRIN2B,DAOA,SPTBN4,CACNA1C"

strsplit(genes_test, split = ",")[[1]] -> genes_test

hyp_obj <- hypeR::hypeR(genes_test, gensets_list, test="hypergeometric", background=23467, fdr=1)

hyp_obj %>% hyp_show()

hyp_obj %>% hyp_dots(top = 30)

hyp_obj$as.data.frame() %>% 
  head(10) %>% 
  e_chart(label) %>% 
  e_bar(fdr, legend = F)

hyp_obj %>% hyp_emap(similarity_cutoff = 0.1)


hyp_obj$as.data.frame() %>% view


# testing 

my_db <- dbPool(
  RMySQL::MySQL(), 
  dbname = "ksdatabase",
  host = "cdrlshinyapps.cdkkdi6q6ptl.us-east-2.rds.amazonaws.com",
  username = "cdrl",
  password = "cdrl_ks"
)

tbl(my_db, "lookup_updated_meta") %>%
  collect() -> ks_datasets

ks_datasets[ks_datasets$Group == "SCZ",]$DataSet -> scz_ds

tbl(my_db, "lookup_updated_annotated") %>% filter(DataSet %in% scz_ds) %>% 
  collect() %>% 
  distinct(HGNC_Symbol, DataSet, .keep_all = T) -> table_res


table_res %>% filter(DataSet == "gandalMicro") %>% View()

table_res %>% count(DataSet) %>% View()

# all
table_res %>% select(HGNC_Symbol, DataSet, Log2FCAbs) %>% 
  group_by(DataSet) %>% 
  top_n(1000, Log2FCAbs) %>%  
  mutate(rank = row_number(desc(Log2FCAbs))) %>% 
  filter(rank <= 1000) %>% 
  mutate(DataSet = paste0(DataSet, "_all")) -> scz_top_500_all

# upregulated
table_res %>% select(HGNC_Symbol, DataSet, Log2FC) %>% 
  filter(Log2FC > 0) %>% 
  group_by(DataSet) %>% 
  top_n(1000, Log2FC) %>%  
  mutate(rank = row_number(desc(Log2FC))) %>% 
  filter(rank <= 1000)  %>% 
  mutate(DataSet = paste0(DataSet, "_up")) -> scz_top_500_up

# downregulated
table_res %>% select(HGNC_Symbol, DataSet, Log2FC) %>% 
  filter(Log2FC < 0) %>% 
  group_by(DataSet) %>% 
  top_n(1000, desc(Log2FC)) %>%  
  mutate(rank = row_number(Log2FC)) %>% 
  filter(rank <= 1000) %>% 
  mutate(DataSet = paste0(DataSet, "_down")) -> scz_top_500_down


rbind(scz_top_500_down, scz_top_500_all, scz_top_500_up) -> scz_top_500

scz_top_500 %>% 
  select(DataSet, HGNC_Symbol) %>% 
  group_by(DataSet) %>% 
  summarize(geneset=list(unique(HGNC_Symbol))) %>% 
  deframe() -> hh


genes_test <- "DISC1,DRD4,BBS4,FEZ1,ANK3,FBXW7,APP,DCTN5,ZNF804A,DPYSL2,BBS1,GSK3B,PVALB,PCM1,TRAK1,OLIG2,PDE4B,NDEL1,ATF4,DGKH,CCDC88A,SRR,DTNBP1,DAO,ERBB4,KCNQ5,PDE4D,NPAS3,PTGER3,POX2,GAD1,RHOT1,PCNT,COMT,KALRN,DRD2,GRM3,TSNAX,FMR1,GRIN1,TRAF3IP1,PDE4A,ITSN1,GRB2,YWHAZ,SLC6A4,NDE1,GRIN2A,PAFAH1B1,DLG4,NRG1,NRXN1,FOXP2,ZNF365,IMMT,SLC12A5,AXIN1,RELN,DIXDC1,CCDC141,ATF5,RGS4,TNIK,BDNF,CDK5,GRIA1,PAFAH1B2,SLC12A2,ADCYAP1,GRIN2B,DAOA,SPTBN4,CACNA1C"

strsplit(genes_test, split = ",")[[1]] -> genes_test

hyp_obj <- hypeR::hypeR(genes_test, hh, test="hypergeometric", background=23467, fdr=1)

hyp_obj %>% hyp_show()

hyp_obj %>% hyp_dots()
hyp_obj %>% hyp_emap()
hyp_obj %>% hyp_hmap()

hh$Superficial_Neurons_all

hyp_obj$as.data.frame() %>% 
  filter(fdr < 1) %>% 
  e_chart(label) %>% 
  e_bar(fdr)
