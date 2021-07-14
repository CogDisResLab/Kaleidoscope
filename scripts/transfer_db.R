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

my_db_new <- dbPool(
  RMySQL::MySQL(), 
  dbname = "ksdatabase_new",
  host = "cdrlshinyapps-2.cdkkdi6q6ptl.us-east-2.rds.amazonaws.com",
  username = "khaled",
  password = "20211411kKk$%^$"
) 

dbListTables(my_db)

# lookup_updated_meta
# lookup_genesets
# lookup_updated_annotated

# lookup
lookup_meta <- tbl(my_db, "lookup_updated_meta") %>% 
  collect() %>% mutate(
    Group = case_when(
      Group == "Microcysti" ~ "MCLR" ,
      Group == "Coronaviru" ~ "COVID",
      T ~ Group
    )
  )

dbWriteTable(my_db_new, value = lookup_meta, name = "lookup_updated_meta",  overwrite = TRUE,
             field.types = c(DataSet = "character(100)", Group = "character(20)", Sp = "character(20)"), 
             row.names = FALSE
) 

lookup_genesets <- tbl(my_db, "lookup_genesets") %>% 
  collect()

dbWriteTable(my_db_new, value = lookup_genesets, name = "lookup_genesets",
             field.types = c(HGNC_Symbol = "character(100)", 
                             DataSet = "character(150)",
                             rank = "integer"
             ), 
             row.names = FALSE
) 

# SELECT
# TABLE_NAME AS `Table`,
# ROUND((DATA_LENGTH + INDEX_LENGTH) / 1024 / 1024) AS `Size (MB)`
# FROM
# information_schema.TABLES
# WHERE
# # TABLE_SCHEMA= "ksdatabase"
# ORDER BY
# (DATA_LENGTH + INDEX_LENGTH)
# DESC;

# CREATE INDEX dataset_ids ON lookup_genesets (DataSet);

lookup <- tbl(my_db, "lookup_updated_annotated") %>% 
  collect()

dbWriteTable(my_db_new, value = lookup, name = "lookup_updated_annotated",  overwrite = T,
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

# CREATE INDEX gene_lookup_ids ON lookup_updated_annotated (DataSet, HGNC_Symbol);

# brainseq
brainseq_mouse <- tbl(my_db, "barres_mouse") %>% collect()
brainseq_human <- tbl(my_db, "barres_human") %>% collect()

brainseq_mouse_updated <- tbl(my_db, "barres_mouse_updated") %>% collect()

brainseq_mouse_updated %>% 
  mutate(CellType = case_when(
    CellType == "Myelinating Oligodendrocy" ~ "Myelinating Oligodendrocytes",
    CellType == "Newly Formed Oligodendroc" ~ "Newly Formed Oligodendrocyte",
    CellType == "Oligodendrocyte Precursor" ~ "Oligodendrocyte Precursor Cell",
    T ~ CellType
    
  )) -> brainseq_mouse_updated


dbWriteTable(my_db_new, value = brainseq_mouse_updated, name = "brainseq_mouse_updated",
             field.types = c(HGNC_Symbol = "character(100)", CellType = "character(50)", FPKM = "DECIMAL(8,2)"), 
             row.names = FALSE, overwrite = T
) 

# CREATE INDEX gene_mouseBarres_ids ON barres_mouse_updated (HGNC_Symbol);


brainseq_human_updated <- tbl(my_db, "barres_human_updated") %>% collect()

dbWriteTable(my_db_new, value = brainseq_human_updated, name = "brainseq_human_updated",
             field.types = c(HGNC_Symbol = "character(100)", CellType = "character(50)", FPKM = "DECIMAL(8,2)"), 
             row.names = FALSE, overwrite = T
)

# CREATE INDEX gene_humanBarres_ids ON barres_human_updated (HGNC_Symbol);


# barinatlas
brainatlas <- tbl(my_db, "brain_atlas_data") %>% collect()

brainatlas %>% select(gene, cluster, CritPassCountsPerCluster, CellType, CPM_mean) -> brainatlas

dbWriteTable(my_db_new, value = brainatlas, name = "brain_atlas_data",
             field.types = c(gene = "character(50)", cluster = "character(50)", 
                             CritPassCountsPerCluster = "integer",
                             CellType = "character(50)",
                             CPM_mean = "DECIMAL(8,2)"), 
             row.names = FALSE
)


# brain cloud
braincloud <- tbl(my_db, "brain_cloud") %>% collect()
braincloud <- select(braincloud, -row_names)

colnames(braincloud) -> ggg
c("character(50)", rep("DECIMAL(8,3)", 269), "character(50)") -> types_col

dbWriteTable(my_db_new, value = braincloud, name = "brain_cloud",
             field.types = types_col, 
             row.names = FALSE, 
)

# gwas 
gwas <- tbl(my_db, "gwas_data") %>% collect()


gwas %>% select(-row_names) %>% 
  mutate(INITIAL.SAMPLE.SIZE = iconv(INITIAL.SAMPLE.SIZE, 'utf-8', 'ascii', sub=' '),
         DISEASE.TRAIT = iconv(DISEASE.TRAIT, 'utf-8', 'ascii', sub=' '),
         STRONGEST.SNP.RISK.ALLELE = iconv(STRONGEST.SNP.RISK.ALLELE, 'utf-8', 'ascii', sub=' ')
         ) -> gwas


colnames(gwas) -> gwas_cols
gwas_types <- c(rep("character(50)", 1), rep("TEXT", 4), "DECIMAL(25,24)", "TEXT",  "character(50)")

names(gwas_types) <- gwas_cols

# need to be indexed
dbWriteTable(my_db_new, value = gwas, name = "gwas_data",
             row.names = FALSE,
             field.types = gwas_types, 
             overwrite = TRUE
             
)

# gtex

gtex <- tbl(my_db,"gtex_data") %>% collect()

gtex %>% select(Gene = Description, everything(), -row_names, -gene_id ) -> gtex

colnames(gtex) -> gtex_cols
gtex_types <- c("character(50)", rep("DECIMAL(8,3)", 53))
names(gtex_types) <- gtex_cols

dbWriteTable(my_db_new, value = gtex, name = "gtex_data",
             row.names = FALSE, field.types = gtex_types
)

poolClose(my_db)
poolClose(my_db_new)

