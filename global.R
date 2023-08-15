# Libs ----------

library(shiny)
library(reactable)
library(tidyverse)
library(echarts4r)
library(shinyWidgets)
library(pool)
library(shinyjs)
library(shinyalert)
library(png)
library(dbplyr)
library(bsplus)
library(waiter)
library(httr)
library(ghql)
library(jsonlite)
library(reactablefmtr)
library(NGLVieweR)
library(shinydashboard)
library(Hmisc)
library(corrplot)

# ggplot theme
theme_set(theme_bw())

# Modules -----------------------------------------------------------------

source("modules/geneInputUI_mod.R")
source("modules/brainrnaseq_mod.R")
source("modules/string_mod.R")
source("modules/process_input.R")
source("modules/ilincs_mod.R")
source("modules/braincloud_mod.R")
source("modules/gwascatalog_mod.R")
source("modules/gtex_mod.R")
source("modules/brainatlas_mod.R")
source("modules/lookup_mod.R")
source("modules/enrichr_mod.R")
source("modules/info_mod.R")
source("modules/download_mod.R")
source("modules/pharos_mod.R")
source("modules/report_mod.R")

# Data ----

all_human_prop <- readRDS("modules/brainseq_avg_human_prop_fixed.RDS")
all_mice_prop <- readRDS("modules/brainseq_avg_mice_prop_fixed.RDS")

ba_all_human_avg <- readRDS("modules/brainatlas_avg_all_genes.RDS")

#l1000_genes <- readRDS("modules/L1000_genes.RDS")

# DB connect ----

my_db <- dbPool(
  RMySQL::MySQL(), 
  dbname = "ksdatabase_new",
  host = Sys.getenv("ks_host_new"),
  username = Sys.getenv("ks_username_new"),
  password = Sys.getenv("ks_pass_new")
)


conn <- GraphqlClient$new(url = 'https://pharos-api.ncats.io/graphql')


onStop(function() {
  poolClose(my_db)
})

# Find datasets
tbl(my_db, "lookup_updated_meta") %>%
  collect() %>% 
  filter(!DataSet %in% c("DISC1_Prot", "Disc_MDD_Proteomics")) -> ks_datasets


# gsea_pathway <- gmtPathways("modules/Human_GO_bp_no_GO_iea_symbol.gmt")
# 
# names(gsea_pathway) <- names(gsea_pathway) %>% str_extract(., "(^[\\D\\w]+%[\\D\\w]+)%") %>% 
#   str_remove(., "%$")


#tell shiny to log all reactivity
#reactlog_enable()
# 
#shiny::reactlogShow()






