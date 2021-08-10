### alias genes

library(tidyverse)
library(pool)
library(dbplyr)

my_db <- dbPool(
  RMySQL::MySQL(), 
  dbname = "ksdatabase",
  host = "cdrlshinyapps.cdkkdi6q6ptl.us-east-2.rds.amazonaws.com",
  username = "khaled",
  password = "20211411kKk$%^$"
)  



dbListTables(my_db)


tbl(my_db, "lookup_new")  %>% 
  collect()  -> d2

tbl(my_db, "lookup_userdefined_meta") %>% 
  collect() -> us_def_ds

tbl(my_db, "lookup_new_meta") %>% 
  collect() -> us_new_ds

poolClose(my_db)

# dataset meta
human_ds <- select(us_def_ds, -Module) %>% rbind(us_new_ds) %>%  
  filter(Group %in% c("SCZ", "MDD", "BPD", "MB", "AD", "Dop") | grepl("GSE29861", DataSet) | grepl("Readhead", DataSet) | 
           grepl("GSE101521", DataSet) | grepl("GSE42546", DataSet) | grepl("GSE42546", DataSet) | grepl("GSE45042", DataSet) |
           grepl("GSE56192", DataSet)
           ) %>% 
  filter(!DataSet %in% c("Benoit_Mouse_NAC", "Benoit_Mouse_PFC", "Disc_MDD_Proteomics", "DISC1_Prot")) %>% 
  mutate(Sp = "Human")

human_ds_prot <- select(us_def_ds, -Module) %>% rbind(us_new_ds) %>% 
  filter(DataSet %in% c("Disc_MDD_Proteomics", "DISC1_Prot"))

mice <- select(us_def_ds, -Module) %>% rbind(us_new_ds) %>% 
  filter(Group %in% c("Aging", "AP", "Other", "Ins") | grepl("GSE116813", DataSet) | grepl("Benoit_Mouse", DataSet) | grepl("GSE50873", DataSet), ! grepl("Readhead", DataSet), ! grepl("GSE66275", DataSet), ! grepl("GSE125325", DataSet),
         ! grepl("89873", DataSet), ! grepl("4031", DataSet), ! grepl("GSE100349", DataSet), DataSet != "OLA-52615-cerebellum"
         ) %>% 
  mutate(Sp = "Mouse")

rat <- select(us_def_ds, -Module) %>% rbind(us_new_ds) %>% 
  filter(Group %in% c("ATD") | grepl("GSE59495", DataSet) | grepl("GSE59906", DataSet) | grepl("GSE100349", DataSet) | 
           grepl("OLA-52615-cerebellum", DataSet) |
           grepl("GSE66275", DataSet) | grepl("GSE125325", DataSet) | grepl("89873", DataSet) | grepl("(4031)", DataSet)
         ) %>% 
  mutate(Sp = "Rat")

zebrafish <- select(us_def_ds, -Module) %>% rbind(us_new_ds) %>% 
  filter(grepl("GSE12214", DataSet)) %>% 
  mutate(Sp = "zebrafish")

other_sp <- select(us_def_ds, -Module) %>% rbind(us_new_ds) %>% 
  filter(grepl("GSE3326", DataSet)) %>% 
  mutate(Sp = "Other_sp")

nrow(human_ds) + nrow(mice) + nrow(rat) + nrow(zebrafish) + nrow(other_sp)
nrow(us_def_ds) + nrow(us_new_ds)

rbind(human_ds, mice, rat, zebrafish, other_sp) -> all_ds_meta

setdiff(us_def_ds$DataSet, all_ds_meta$DataSet)


# human genes

d2 %>% filter(DataSet %in% human_ds$DataSet) %>% 
  separate_rows(Gene_Symbol, sep = " /// ") %>% 
  separate_rows(Gene_Symbol, sep = "///") -> d2_human

d2_human$Gene_Symbol %>% unique() -> d2_human_genes

HGNChelper::getCurrentHumanMap() -> new_human_map

HGNChelper::checkGeneSymbols(d2_human_genes, map = new_human_map) -> human_gene_fetched


human_gene_fetched %>% 
  select(-Approved) %>% 
  rename(Gene_Symbol = x) %>% drop_na(Suggested.Symbol) %>% 
  separate_rows(Suggested.Symbol, sep = " /// ") -> human_gene_fetched_edited
  
d2_human %>% left_join(human_gene_fetched_edited) -> d2_human_annotated

d2_human_annotated %>% drop_na(Suggested.Symbol) -> d2_human_annotated

d2_human_annotated  %>% 
  distinct(Suggested.Symbol, DataSet, .keep_all = T) -> d2_human_annotated

# d2_human_annotated %>% 
#   select(-Gene_Symbol) %>% 
#   mutate(Log2FC = round(Log2FC, 2)) %>% 
#   pivot_wider(names_from = DataSet, values_from = Log2FC) %>% 
#   janitor::clean_names() -> d2_human_annotated__wideLog2FC

d2_human_annotated %>% 
  select(-Gene_Symbol) %>% 
  rename(HGNC_Symbol = Suggested.Symbol) -> d2_human_annotated


saveRDS(d2_human_annotated, "data/lookup_human_annotated_May23_21.RDS")

####

d2 %>% filter(DataSet %in% mice$DataSet) %>% 
  separate_rows(Gene_Symbol, sep = " /// ") %>% 
  separate_rows(Gene_Symbol, sep = "///") -> d2_mice

#d2_mice %>% filter(Gene_Symbol %in% rat_inmice) %>% view

d2_mice$Gene_Symbol %>% unique() -> d2_mice_genes


# from http://www.informatics.jax.org/batch
#write_delim(tibble(ID = d2_mice_genes), "data/mice_ids_input.txt", delim = "\t")

mice_gene_fetched_v2 <- read_delim("data/mice_ids_hgnc_output.txt", delim = "\t")

mice_gene_fetched_v2 %>% drop_na(`Input Type`) %>% 
  filter(`Input Type` %in% c("current symbol", "old symbol", "Genbank", "synonym", "RefSeq", "Ensembl Transcript", "MGI", "related synonym", 
                             "Ensembl Gene Model", "HomoloGene", "current name", "old name")) %>% 
  filter(Input != "ID") %>% 
  select(Gene_Symbol = Input, Symbol) -> mice_gene_fetched_v2


d2_mice %>% left_join(mice_gene_fetched_v2) -> d2_mice_annotated

d2_mice_annotated %>% drop_na(Symbol) -> d2_mice_annotated

d2_mice_annotated  %>% 
  distinct(Symbol, DataSet, .keep_all = T) -> d2_mice_annotated

# d2_mice_annotated %>% 
#   select(-Gene_Symbol) %>% 
#   mutate(Log2FC = round(Log2FC, 2)) %>% 
#   pivot_wider(names_from = DataSet, values_from = Log2FC) %>% 
#   janitor::clean_names() -> d2_mice_annotated__wideLog2FC

d2_mice_annotated %>% 
  select(-Gene_Symbol) %>% 
  rename(HGNC_Symbol = Symbol) -> d2_mice_annotated


saveRDS(d2_mice_annotated, "data/lookup_mice_annotated_May23_21.RDS")

# rats

d2 %>% filter(DataSet %in% rat$DataSet) %>% 
  separate_rows(Gene_Symbol, sep = " /// ") %>% 
  separate_rows(Gene_Symbol, sep = "///") -> d2_rat


d2_rat$Gene_Symbol %>% unique() -> d2_rat_genes


# from http://www.informatics.jax.org/batch
#write_delim(tibble(ID = d2_rat_genes), "data/rat_ids_input.txt", delim = "\t")

rat_gene_fetched_v2 <- read_delim("data/GENES.RAT.txt", delim = "\t")

rat_gene_fetched_v2 %>% select(SYMBOL, OLD_SYMBOL) %>% 
  mutate(OLD_SYMBOL = ifelse(is.na(OLD_SYMBOL), SYMBOL, OLD_SYMBOL)) %>% 
  separate_rows(OLD_SYMBOL, sep = ";") %>% 
  separate_rows(OLD_SYMBOL, sep = "/") %>% 
  separate_rows(OLD_SYMBOL, sep = ", ") -> rat_gene_fetched_v2

tibble(
  SYMBOL = rat_gene_fetched_v2$SYMBOL,
  OLD_SYMBOL = rat_gene_fetched_v2$SYMBOL
) %>% rbind(rat_gene_fetched_v2) %>% 
  distinct() -> rat_gene_fetched_v3


rat_gene_fetched_v3 %>% rename(Gene_Symbol = OLD_SYMBOL) %>% 
  mutate(Gene_Symbol = toupper(Gene_Symbol)) %>% 
  rbind(tibble(
    SYMBOL = "Cyp3a23-3a1",
    Gene_Symbol = "CYP3A23/3A1"
  )) -> rat_gene_fetched_v3

d2_rat %>% 
  mutate(Gene_Symbol = gsub("_PREDICTED", "", Gene_Symbol)) %>% 
  left_join(rat_gene_fetched_v3) -> d2_rat_annotated

d2_rat_annotated %>% filter(is.na(SYMBOL)) %>% View

d2_rat_annotated %>% drop_na(SYMBOL) -> d2_rat_annotated

d2_rat_annotated  %>% 
  distinct(SYMBOL, DataSet, .keep_all = T) -> d2_rat_annotated

# d2_rat_annotated %>% 
#   select(-Gene_Symbol) %>% 
#   mutate(Log2FC = round(Log2FC, 2)) %>% 
#   pivot_wider(names_from = DataSet, values_from = Log2FC) %>% 
#   janitor::clean_names() -> d2_rat_annotated__wideLog2FC

d2_rat_annotated %>% 
  select(-Gene_Symbol) %>% 
  rename(HGNC_Symbol = SYMBOL) -> d2_rat_annotated


saveRDS(d2_rat_annotated, "data/lookup_rat_annotated_May23_21.RDS")


# zebras

d2 %>% filter(DataSet %in% zebrafish$DataSet) %>% 
  separate_rows(Gene_Symbol, sep = " /// ") %>% 
  separate_rows(Gene_Symbol, sep = "///") -> d2_zebra


d2_zebra$Gene_Symbol %>% unique() -> d2_zebra_genes


# from http://www.informatics.jax.org/batch
#write_delim(tibble(ID = d2_rat_genes), "data/rat_ids_input.txt", delim = "\t")

zebra_gene_fetched_v2 <- read_delim("data/GENES.Zebrafish.txt", delim = "\t", col_names = F)

zebra_gene_fetched_v2 %>% select(3,4) %>% 
  rename(SYMBOL = 1, OLD_SYMBOL = 2) -> zebra_gene_fetched_v2

zebra_gene_fetched_v2 %>% select(SYMBOL, OLD_SYMBOL) %>% 
  mutate(OLD_SYMBOL = ifelse(is.na(OLD_SYMBOL), SYMBOL, OLD_SYMBOL))  -> zebra_gene_fetched_v2

tibble(
  SYMBOL = zebra_gene_fetched_v2$SYMBOL,
  OLD_SYMBOL = zebra_gene_fetched_v2$SYMBOL
) %>% rbind(zebra_gene_fetched_v2) %>% 
  distinct() -> zebra_gene_fetched_v3


zebra_gene_fetched_v3 %>% rename(Gene_Symbol = OLD_SYMBOL) %>% 
  mutate(Gene_Symbol = toupper(Gene_Symbol)) -> zebra_gene_fetched_v3

d2_zebra %>% 
  left_join(zebra_gene_fetched_v3) -> d2_zebra_annotated

d2_zebra_annotated %>% filter(is.na(SYMBOL)) %>% View

d2_zebra_annotated %>% drop_na(SYMBOL) -> d2_zebra_annotated

d2_zebra_annotated  %>% 
  distinct(SYMBOL, DataSet, .keep_all = T) -> d2_zebra_annotated



d2_zebra_annotated %>% 
  select(-Gene_Symbol) %>% 
  rename(HGNC_Symbol = SYMBOL) -> d2_zebra_annotated


saveRDS(d2_zebra_annotated, "data/lookup_zebra_annotated_May23_21.RDS")

# other species

d2 %>% filter(DataSet == "GSE3326_MOCKvsSARS_36h") %>% 
  rename(HGNC_Symbol = Gene_Symbol) -> d2_others_annotated

# combine new annotated datastes

new_annotaed_ds <- rbind(d2_human_annotated, d2_mice_annotated, d2_rat_annotated, d2_zebra_annotated, d2_others_annotated)

new_annotaed_ds %>% select(-Fold_Change, -row_names) %>% 
  mutate_at(c("Log2FC", "Log2FCAbs"), round, 2) %>% 
  mutate_at(c("ecdfPlot", "P_Value"), round, 5) %>% 
  select(HGNC_Symbol, everything()) %>% 
  drop_na(HGNC_Symbol) %>% 
  mutate(HGNC_Symbol = toupper(HGNC_Symbol)) -> new_annotaed_ds

saveRDS(new_annotaed_ds, "data/lookup_updated_annotated_May24_21.RDS")


### gene tables



genes_tbl <- rbind(
  mutate(human_gene_fetched_edited, Sp = "Human") %>% rename(HGNC_Symbol = Suggested.Symbol),
  mutate(mice_gene_fetched_v2, Sp = "Mouse") %>% rename(HGNC_Symbol = Symbol), 
  mutate(rat_gene_fetched_v3, Sp = "Rat") %>% rename(HGNC_Symbol = SYMBOL),
  mutate(zebra_gene_fetched_v3, Sp = "Zebrafish") %>% rename(HGNC_Symbol = SYMBOL)
) %>% mutate(HGNC_Symbol = toupper(HGNC_Symbol),
             Gene_Symbol = toupper(Gene_Symbol)
             )


genes_tbl %>% 
  group_by(Sp, HGNC_Symbol) %>% 
  mutate(Alias = paste0(unique(Gene_Symbol), collapse = ",")) %>% 
  slice(1) %>% 
  ungroup() -> genes_tbl_v2

genes_tbl_v2 %>% 
  select(-Gene_Symbol, HGNC_Symbol, Alias, Sp) ->  genes_tbl_v2

genes_in_dbs <- new_annotaed_ds$HGNC_Symbol %>% unique()
genes_in_ref <- genes_tbl_v2$HGNC_Symbol %>% unique()

genes_tbl_v2 %>% filter(HGNC_Symbol %in% genes_in_dbs) -> genes_tbl_v3

saveRDS(genes_tbl, "data/genes_tbl_v1_May24_21.RDS")
saveRDS(genes_tbl_v3, "data/genes_tbl_v3_May24_21.RDS")

# added official genes missing from Gene_Symbol
saveRDS(genes_tbl_v4, "data/genes_tbl_v4_May24_21.RDS")

## ds meta table

all_ds_meta %>% 
  select(-row_names) %>% 
  mutate(Sp = case_when(
    Sp == "zebrafish" ~ "Zebrafish",
    Sp == "Other_sp" ~ "Arabidopsis", 
    T ~ Sp)) -> all_ds_meta

saveRDS(all_ds_meta, "data/updated_ds_meta_tbl_May24_21.RDS")

## testing using dm lib

library(dm)

lookup_dm_no_keys <- dm(new_annotaed_ds, all_ds_meta, genes_tbl_v3)
lookup_dm_no_keys

dm_enum_pk_candidates(
  dm = lookup_dm_no_keys,
  table = all_ds_meta
)

dm_enum_pk_candidates(
  dm = lookup_dm_no_keys,
  table = genes_tbl_v3
)

lookup_dm_no_keys %>%
  dm_add_pk(table = genes_tbl_v2, columns = c(HGNC_Symbol, Sp)) %>%
  dm_draw()

# lookup_dm_only_pks <-
#   lookup_dm_no_keys %>%
#   dm_add_pk(table = all_ds_meta, columns = DataSet) %>%
#   dm_add_pk(table = genes_tbl_v3, columns = c(HGNC_Symbol, Sp)) %>%
#   dm_add_pk(table = new_annotaed_ds, columns = c(HGNC_Symbol, DataSet))

lookup_dm_pks_fks <-
  lookup_dm_no_keys %>%
  dm_add_pk(table = all_ds_meta, columns = DataSet) %>%
  dm_add_fk(table = new_annotaed_ds, columns = DataSet,all_ds_meta )



lookup_dm_pks_fks %>% 
  dm_draw()


# testing 

library(tidyverse)
library(dbplyr)

new_annotaed_ds <- readRDS("data/lookup_updated_annotated_May24_21.RDS")
all_ds_meta <- readRDS("data/updated_ds_meta_tbl_May24_21.RDS")
genes_tbl_v1 <- readRDS("data/genes_tbl_v1_May24_21.RDS")

genes_tbl_v1 %>% filter(HGNC_Symbol %in%  unique(new_annotaed_ds$HGNC_Symbol)) -> genes_tbl_v1


db <- memdb_frame(x = c(2, NA, 5, NA, 10), y = 1:5, 
                  z = c("fhd,dk", "dkh", "dhhd", "dk", "djhf,shd")
)
db %>% filter(x <= 5, z %in% c("dhhd", "fhd")) %>% show_query()
db %>% select(z) %>% filter(z %in% c("dhhd", "dk")) %>% show_query()


# upload to db 

library(dbplyr)

new_annotaed_ds <- readRDS("data/lookup_updated_annotated_May24_21.RDS")
all_ds_meta <- readRDS("data/updated_ds_meta_tbl_May24_21.RDS")
genes_tbl_v1 <- readRDS("data/genes_tbl_v1_May24_21.RDS")

genes_tbl_v1 %>% filter(HGNC_Symbol %in%  unique(new_annotaed_ds$HGNC_Symbol)) -> genes_tbl_v1

my_db <- DBI::dbConnect(
  RMySQL::MySQL(), 
  dbname = "ksdatabase",
  host = "cdrlshinyapps.cdkkdi6q6ptl.us-east-2.rds.amazonaws.com",
  username = "khaled",
  password = "20211411kKk$%^$"
) 

DBI::dbDisconnect(my_db)

DBI::dbListTables(my_db)

colnames(all_ds_meta)

copy_to(dest = my_db, 
        df = all_ds_meta, 
        name = "lookup_updated_meta",
        temporary = FALSE,
        overwrite = T,
        indexes = list(
          "DataSet"
        )
)

colnames(all_ds_meta)

dbWriteTable(my_db, value = all_ds_meta, name = "lookup_updated_meta",  overwrite = TRUE,
             field.types = c(DataSet = "character(100)", Group = "character(10)", Sp = "character(20)"), 
             row.names = FALSE
             ) 

colnames(genes_tbl_v4)
dbWriteTable(my_db, value = genes_tbl_v4, name = "lookup_updated_genes_ids", overwrite = TRUE,
             field.types = c(Gene_Symbol = "character(100)", HGNC_Symbol = "character(100)", Sp = "character(20)"), 
             row.names = FALSE
             ) 

colnames(new_annotaed_ds %>% select(-Group))

dbWriteTable(my_db, value = new_annotaed_ds %>% select(-Group), name = "lookup_updated_annotated",  overwrite = T,
             field.types = c(HGNC_Symbol = "character(100)", 
                             Log2FC = "DECIMAL(5,2)", 
                             P_Value = "DECIMAL(6,5)",
                             ecdfPlot = "DECIMAL(6,5)",
                             DataSet = "character(100)",
                             Log2FCAbs = "DECIMAL(5,2)",
                             Dir = "character(5)"
                             ), 
             row.names = FALSE
             ) 



#### other databases

my_db <- dbPool(
  RMySQL::MySQL(), 
  dbname = "ksdatabase",
  host = "cdrlshinyapps.cdkkdi6q6ptl.us-east-2.rds.amazonaws.com",
  username = "khaled",
  password = "20211411kKk$%^$"
)  


tbl(my_db, "barres_human") %>% 
  collect()  -> barres_human_tbl


barres_human_tbl %>% select(-row_names) %>% 
  group_by(CellType, Gene) %>% 
  summarise(FPKM = mean(FPKM, na.rm = FALSE)) %>% 
  ungroup() %>% 
  mutate_if(is.numeric, round, 2) -> barres_human_tbl


barres_human_tbl$Gene %>% unique() -> barres_human_genes

HGNChelper::getCurrentHumanMap() -> new_human_map

HGNChelper::checkGeneSymbols(barres_human_genes, map = new_human_map) -> barres_human_gene_fetched

barres_human_gene_fetched %>% filter(Approved == "FALSE") %>% View()

barres_human_genes %>% toupper() -> barres_human_genes_Up

tbl(my_db, "lookup_updated_genes_ids") %>% filter(Gene_Symbol %in% barres_human_genes_Up, Sp == "Human") %>% 
  collect() -> checked_genes


checked_genes$Gene_Symbol %>% unique() -> found_genes

barres_human_gene_fetched %>% filter(!grepl("^LOC",x)) %>% 
  mutate(Suggested.Symbol = ifelse(is.na(Suggested.Symbol), x, Suggested.Symbol)) %>% 
  select(-Approved) -> barres_human_gene_fetched

barres_human_gene_fetched %>% rename(Gene = x, HGNC_Symbol = Suggested.Symbol) -> barres_human_gene_fetched

barres_human_tbl %>% left_join(barres_human_gene_fetched) %>% 
  select(-Gene) %>% 
  mutate(HGNC_Symbol = toupper(HGNC_Symbol)) %>% 
  select(HGNC_Symbol, everything()) -> barres_human_tbl

barres_human_tbl %>% 
  group_by(CellType, HGNC_Symbol) %>% 
  summarise(FPKM = mean(FPKM, na.rm = FALSE)) %>% 
  ungroup() -> barres_human_tbl


# mice
tbl(my_db, "barres_mouse") %>% 
  collect() %>% select(-row_names)  -> barres_mouse_tbl

barres_mouse_tbl$GeneSymbol %>% unique() %>% toupper() -> barres_mouse_genes


tbl(my_db, "lookup_updated_genes_ids") %>% filter(Gene_Symbol %in% barres_mouse_genes, Sp == "Mouse") %>% 
  collect() -> checked_genes

barres_mouse_tbl

barres_mouse_tbl %>% rename(Gene_Symbol = GeneSymbol) %>% 
  left_join(select(checked_genes, -Sp)) -> barres_mouse_tbl

barres_mouse_tbl %>% 
  filter(!grepl("^LOC",Gene_Symbol)) %>% 
  mutate(HGNC_Symbol = ifelse(is.na(HGNC_Symbol), Gene_Symbol, HGNC_Symbol)) %>% 
  select(-Gene_Symbol) %>% 
  group_by(CellType, HGNC_Symbol) %>% 
  summarise(FPKM = mean(FPKM, na.rm = FALSE)) %>% 
  ungroup() %>% 
  mutate_if(is.numeric, round, 2) -> barres_mouse_tbl

barres_human_tbl <- select(barres_human_tbl, HGNC_Symbol, CellType, FPKM)
colnames(barres_human_tbl)

dbWriteTable(my_db, value = barres_human_tbl, name = "barres_human_updated",
             field.types = c(HGNC_Symbol = "character(100)", CellType = "character(25)", FPKM = "DECIMAL(8,2)"), 
             row.names = FALSE
) 

barres_mouse_tbl <- select(barres_mouse_tbl, HGNC_Symbol, CellType, FPKM)
colnames(barres_mouse_tbl)

dbWriteTable(my_db, value = barres_mouse_tbl, name = "barres_mouse_updated", 
             field.types = c(HGNC_Symbol = "character(100)", CellType = "character(25)", FPKM = "DECIMAL(8,2)"), 
             row.names = FALSE
) 
  

# missed datasets

found_datasets <- readRDS("data/found_datasets.RDS")


my_db <- dbPool(
  RMySQL::MySQL(), 
  dbname = "ksdatabase",
  host = "cdrlshinyapps.cdkkdi6q6ptl.us-east-2.rds.amazonaws.com",
  username = "cdrl",
  password = "cdrl_ks"
)

tbl(my_db, "lookup_updated_meta") %>%
  collect() -> ks_datasets


ks_datasets %>% filter(!DataSet %in% found_datasets) %>% View()


tbl(my_db, "lookup_updated_annotated") %>% 
  count(DataSet) %>% collect()  -> table_res


setdiff(ks_datasets$DataSet, table_res$DataSet) -> not_found

# AD

ks_datasets %>% 
  filter(DataSet %in% not_found) %>% 
  filter(Sp == "Human") %>% 
  filter(!DataSet %in% c("DISC1_Prot", "Disc_MDD_Proteomics")) %>% 
  pull(DataSet) -> missed_human_datasets

tbl(my_db, "lookup_userdefined")  %>% 
  filter(DataSet %in% missed_human_datasets) %>% 
  collect() -> d1

d1 %>% 
  separate_rows(Gene_Symbol, sep = " /// ") %>% 
  separate_rows(Gene_Symbol, sep = "///") -> d2_human

d2_human$Gene_Symbol %>% unique() -> d2_human_genes

HGNChelper::getCurrentHumanMap() -> new_human_map

HGNChelper::checkGeneSymbols(d2_human_genes, map = new_human_map) -> human_gene_fetched


human_gene_fetched %>% 
  select(-Approved) %>% 
  rename(Gene_Symbol = x) %>% drop_na(Suggested.Symbol) %>% 
  separate_rows(Suggested.Symbol, sep = " /// ") -> human_gene_fetched_edited

d2_human %>% left_join(human_gene_fetched_edited) -> d2_human_annotated

d2_human_annotated %>% drop_na(Suggested.Symbol) -> d2_human_annotated

d2_human_annotated  %>% 
  distinct(Suggested.Symbol, DataSet, .keep_all = T) -> d2_human_annotated


d2_human_annotated %>% 
  select(-Gene_Symbol) %>% 
  rename(HGNC_Symbol = Suggested.Symbol) -> d2_human_annotated

# mice missed

ks_datasets %>% 
  filter(DataSet %in% not_found) %>% 
  filter(Sp == "Mouse") %>% 
  pull(DataSet) -> missed_mouse_datasets

tbl(my_db, "lookup_userdefined")  %>% 
  filter(DataSet %in% missed_mouse_datasets) %>% 
  collect() -> d2

tbl(my_db, "lookup_new")  %>% 
  filter(DataSet %in% missed_mouse_datasets) %>% 
  collect() -> d3

d4 <- rbind(d2,d3)

d4$DataSet %>% unique()


d4 %>% 
  separate_rows(Gene_Symbol, sep = " /// ") %>% 
  separate_rows(Gene_Symbol, sep = "///") -> d2_mice

#d2_mice %>% filter(Gene_Symbol %in% rat_inmice) %>% view

d2_mice$Gene_Symbol %>% unique() -> d2_mice_genes


# from http://www.informatics.jax.org/batch
#write_delim(tibble(ID = d2_mice_genes), "data/mice_ids_input_missed_ds.txt", delim = "\t")

mice_gene_fetched_v2 <- read_delim("data/mice_ids_hgnc_output_missed_ds.txt", delim = "\t")

mice_gene_fetched_v2 %>% drop_na(`Input Type`) %>% 
  filter(`Input Type` %in% c("current symbol", "old symbol", "Genbank", "synonym", "RefSeq", "Ensembl Transcript", "MGI", "related synonym", 
                             "Ensembl Gene Model", "HomoloGene", "current name", "old name")) %>% 
  filter(Input != "ID") %>% 
  select(Gene_Symbol = Input, Symbol) -> mice_gene_fetched_v2


d2_mice %>% left_join(mice_gene_fetched_v2) -> d2_mice_annotated

d2_mice %>% filter(DataSet %in% c("Aging_Mice_Hippocampus_PR")) %>% View()

d2_mice_annotated %>% filter(!is.na(Symbol)) -> d2_mice_annotated

d2_mice_annotated  %>% 
  distinct(Symbol, DataSet, .keep_all = T) -> d2_mice_annotated


d2_mice %>% count(DataSet)
d2_mice_annotated %>% count(DataSet)
d4 %>% count(DataSet)


d2_mice_annotated %>% 
  select(-Gene_Symbol) %>% 
  rename(HGNC_Symbol = Symbol) -> d2_mice_annotated


## rats

ks_datasets %>% 
  filter(DataSet %in% not_found) %>% 
  filter(Sp == "Rat") %>% 
  pull(DataSet) -> missed_rat_datasets

tbl(my_db, "lookup_userdefined")  %>% 
  filter(DataSet %in% missed_rat_datasets) %>% 
  collect() -> d1


d1 %>% 
  separate_rows(Gene_Symbol, sep = " /// ") %>% 
  separate_rows(Gene_Symbol, sep = "///") -> d2_rat


d2_rat$Gene_Symbol %>% unique() -> d2_rat_genes


# from http://www.informatics.jax.org/batch
#write_delim(tibble(ID = d2_rat_genes), "data/rat_ids_input_missed_ds.txt", delim = "\t")

rat_gene_fetched_v2 <- read_delim("data/GENES.RAT.txt", delim = "\t")

rat_gene_fetched_v2 %>% select(SYMBOL, OLD_SYMBOL) %>% 
  mutate(OLD_SYMBOL = ifelse(is.na(OLD_SYMBOL), SYMBOL, OLD_SYMBOL)) %>% 
  separate_rows(OLD_SYMBOL, sep = ";") %>% 
  separate_rows(OLD_SYMBOL, sep = "/") %>% 
  separate_rows(OLD_SYMBOL, sep = ", ") -> rat_gene_fetched_v2

tibble(
  SYMBOL = rat_gene_fetched_v2$SYMBOL,
  OLD_SYMBOL = rat_gene_fetched_v2$SYMBOL
) %>% rbind(rat_gene_fetched_v2) %>% 
  distinct() -> rat_gene_fetched_v3


rat_gene_fetched_v3 %>% rename(Gene_Symbol = OLD_SYMBOL) %>% 
  mutate(Gene_Symbol = toupper(Gene_Symbol)) %>% 
  rbind(tibble(
    SYMBOL = "Cyp3a23-3a1",
    Gene_Symbol = "CYP3A23/3A1"
  )) -> rat_gene_fetched_v3

d2_rat %>% 
  mutate(Gene_Symbol = gsub("_PREDICTED", "", Gene_Symbol)) %>% 
  left_join(rat_gene_fetched_v3) -> d2_rat_annotated

d2_rat_annotated %>% filter(is.na(SYMBOL)) %>% View

d2_rat_annotated %>% drop_na(SYMBOL) -> d2_rat_annotated

d2_rat_annotated  %>% 
  distinct(SYMBOL, DataSet, .keep_all = T) -> d2_rat_annotated

# d2_rat_annotated %>% 
#   select(-Gene_Symbol) %>% 
#   mutate(Log2FC = round(Log2FC, 2)) %>% 
#   pivot_wider(names_from = DataSet, values_from = Log2FC) %>% 
#   janitor::clean_names() -> d2_rat_annotated__wideLog2FC

d2_rat_annotated %>% 
  select(-Gene_Symbol) %>% 
  rename(HGNC_Symbol = SYMBOL) -> d2_rat_annotated

####
new_annotaed_ds <- rbind(d2_human_annotated, d2_mice_annotated, d2_rat_annotated)

count(new_annotaed_ds, DataSet) %>% pull(DataSet) -> processed

setdiff(not_found, processed)

new_annotaed_ds %>% select(-Fold_Change, -row_names) %>% 
  mutate_at(c("Log2FC", "Log2FCAbs"), round, 2) %>% 
  mutate_at(c("ecdfPlot", "P_Value"), round, 5) %>% 
  select(HGNC_Symbol, everything()) %>% 
  drop_na(HGNC_Symbol) %>% 
  mutate(HGNC_Symbol = toupper(HGNC_Symbol)) -> new_annotaed_ds

new_annotaed_ds %>% select(-Group) -> new_annotaed_ds


colnames(new_annotaed_ds)

#saveRDS(new_annotaed_ds,"data/lookup_missed_ds_annotated_June15_21.RDS")

my_db <- dbPool(
  RMySQL::MySQL(), 
  dbname = "ksdatabase",
  host = "cdrlshinyapps.cdkkdi6q6ptl.us-east-2.rds.amazonaws.com",
  username = "khaled",
  password = "20211411kKk$%^$"
)  

dbWriteTable(my_db, value = new_annotaed_ds, name = "lookup_updated_annotated",  overwrite = F,append = T,
             field.types = c(HGNC_Symbol = "character(100)", 
                             Log2FC = "DECIMAL(5,2)", 
                             P_Value = "DECIMAL(6,5)",
                             ecdfPlot = "DECIMAL(6,5)",
                             DataSet = "character(100)",
                             Log2FCAbs = "DECIMAL(5,2)",
                             Dir = "character(5)"
             ), 
             row.names = FALSE
) 
