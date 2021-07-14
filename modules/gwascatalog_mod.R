# Gene Text Input Module UI function

gwascatalog_UI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 12,  geneInputUI(ns("genes"))),hr(),
      column(width = 6, echarts4rOutput(ns("plot1"), height = "500")),
      column(width = 6, echarts4rOutput(ns("plot2"), height = "500")),
      column(width = 6, echarts4rOutput(ns("plot3"), height = "500")),
      column(width = 6, echarts4rOutput(ns("plot4"), height = "500")),
      column(width = 12, reactableOutput(ns("table")), 
             div(id = ns("dn_btn_div"), download_btn_ui(ns("dn_btn")))
             )
    )
  )
  
}

# Gene Text Input Module Server function

gwascatalog_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      genes_ids <- geneOutput("genes")
      
      observeEvent(genes_ids$info_btn(),{
        shinyalert(title = NULL, text = gwas_info_html, html = T, closeOnEsc = T, closeOnClickOutside = T)
        
      })
      
      observeEvent(genes_ids$btn(), {
        
        genes <- isolate(genes_ids$genes())
        req(genes)
        

        shinyjs::hide("table1")
        shinyjs::hide("plot1")
        shinyjs::hide("plot2")
        shinyjs::hide("plot3")
        shinyjs::hide("plot4")
        shinyjs::hide("dn_btn_div")
        
        genes <- process_gene_input(genes)
        
        res <-  withProgress(message = "connecting to KS db ...", {
          ks_gwas(genes, my_db)
        })
          
        
        if(!is.null(res)) {
          
          output$table <- renderReactable({
            
            reactable(select(res, -Sample_Size), 
                      searchable = TRUE,
                      striped = TRUE,
                      bordered = TRUE,
                      
                      columns = list(
                        Link = colDef(
                          cell = function(value) {
                            htmltools::tags$a(href = paste0("https://", value), target = "_blank", "Link")
                          },
                          style = list(color = "blue")
                        )
                      ),
                      
                      details = function(index) {
                        paste(res$Sample_Size[[index]])
                      }
            )
            
          })
          
          res %>% 
            count(Trait, sort = T) %>% 
            head(10) %>% 
            arrange(n) -> res_procssed
          
          top_traits <- pull(res_procssed, Trait) %>% unique()

          res %>% 
            filter(Trait %in% top_traits) %>% 
            select(Gene, Type) %>% 
            group_by(Gene, Type) %>% 
            add_tally() %>% ungroup() %>% 
            rename(Target = Type, Source = Gene ) -> gw_test1
          
          res %>% 
            filter(Trait %in% top_traits) %>% 
            select(Type, Trait) %>% 
            group_by(Type, Trait) %>% 
            add_tally() %>% ungroup() %>% 
            rename(Target = Trait,
                   Source = Type) -> gw_test2
          
          res %>% 
            filter(Trait %in% top_traits) %>% 
            select(Gene, Trait) %>% 
            group_by(Gene, Trait) %>% 
            add_tally() %>% ungroup() %>% 
            rename(Target = Trait, Source = Gene ) -> gw_test3

          
          output$plot1 <- renderEcharts4r({
            res_procssed %>% 
              e_charts(Trait, renderer="svg") %>% 
              e_bar(n, legend = F) %>% 
              e_title("Number of Associated SNPs", subtext = "Only showing top 10 associated traits") %>% 
              e_flip_coords() %>% 
              e_grid(
                left = 200, # pixels
                top = "15%" # percentage = responsive
              ) %>% 
              e_y_axis(axisLabel = list(fontSize = 9)) %>% 
              e_mark_point("n", data = list(name = "Max",type = "max"), itemStyle = list(color = "red")) %>% 
              e_toolbox() %>%
              e_toolbox_feature(feature = c("saveAsImage", "dataView")) %>% 
              e_tooltip()
          })
          
          output$plot2 <- renderEcharts4r({
            res %>% 
              filter(Trait %in% top_traits) %>% 
              mutate(
                Type = ifelse(is.na(Type), "NA", Type),
                Type = forcats::fct_lump(Type, 5)
              ) %>% 
              count(Type) %>% 
              e_charts(Type,renderer="svg") %>% 
              e_pie(n, roseType = "radius") %>% 
              e_labels() %>% 
              e_title("SNPs Types", subtext = "Only showing top 10 associated traits") %>% 
              e_toolbox() %>%
              e_toolbox_feature(feature = c("saveAsImage", "dataView")) %>% 
              e_tooltip()
          })
          
          output$plot3 <- renderEcharts4r({
            rbind(gw_test1, gw_test2) %>% 
              e_charts(renderer="svg") %>% 
              e_sankey(Source, Target, n) %>% 
              e_toolbox() %>%
              e_toolbox_feature(feature = c("saveAsImage", "dataView")) %>% 
              e_tooltip()
          })
          
          output$plot4 <- renderEcharts4r({
            gw_test3 %>% 
              e_charts(renderer="svg") %>% 
              e_sankey(Source, Target, n) %>% 
              e_toolbox() %>%
              e_toolbox_feature(feature = c("saveAsImage", "dataView")) %>% 
              e_tooltip()
          })
          
          download_btn_server(id = "dn_btn", 
                              tbl = res,
                              name = "GWAS_Catalog_Table")
          
          
          shinyjs::show("table1")
          shinyjs::show("plot1")
          shinyjs::show("plot2")
          shinyjs::show("plot3")
          shinyjs::show("plot4")
          shinyjs::show("dn_btn_div")
          
        }
        
        else {
          shinyalert("Opps", "Couldn't Find Your Gene/s", type = "error")
        }
        
        shinyFeedback::resetLoadingButton("genes-btn")
        
        
        
        
        
        
      })
      
    }
    
  )
  
}

ks_gwas <- function(genes, db = my_db) {
  tbl(my_db,"gwas_data") %>% 
    filter(Gene %in% genes) %>% 
    collect() %>% 
    rename(Risk_Allele = `STRONGEST.SNP.RISK.ALLELE`, Trait = `DISEASE.TRAIT`, 
           Type = `CONTEXT`, Sample_Size = `INITIAL.SAMPLE.SIZE`, Link = LINK
           ) -> table_res
  
  ifelse(nrow(table_res) > 0, return(table_res), return(NULL))
  
}

gwas_info_html <- HTML('<div class="features-icon"><span class="icon-map"></span></div>
                  <h3 class="features-title font-alt">GWAS Catalog</h3>
                  <p>A curated collection of all published genome-wide association studies that currently contains 3841 publications and 126603 
genetic variant - phenotype associations</p>
                         <h3 class = "features-title font-alt">Resources</h3>
                         <p>
                         <a href="https://pubmed.ncbi.nlm.nih.gov/30445434/" target="_blank" rel="noopener noreferrer">PMID: 30445434</a>
                         <br>
                         <a href="https://www.ebi.ac.uk/gwas/" target="_blank" rel="noopener noreferrer">Website</a>
                         </p>
                         ')

# tests
# library(gwasrapidd)
# 
# g_test <- get_variants(gene_name = "AKT1")
# 
# g_test2 <- get_associations()
# 
# g_test3 <- GET(url = "https://www.ebi.ac.uk/gwas/rest/api/singleNucleotidePolymorphisms/search/findByGene?geneName=AKT1")
# 
# g_test3 <- jsonlite::fromJSON(content(g_test3, as = "text"))
# 
# 
# g_test3[["_embedded"]][["singleNucleotidePolymorphisms"]][["rsId"]] %>% unique()
# 
# 
# 
# https://www.ebi.ac.uk/gwas/rest/api/associations/search/findByRsId?rsId=rs11802094
# 
# ?geneName=DISC1
# 
# 
# tbl(my_db, "gwas_data") %>% tally()


# ks_gwas(c("AKT1", "AKT2", "AKT3")) -> gw_test
# 
# 
# gw_test %>% 
#   count(Trait, sort = T) %>% 
#   head(10) %>% 
#   arrange(n) %>% 
#   e_charts(Trait) %>% 
#   e_bar(n, legend = F) %>% 
#   e_flip_coords() %>% 
#   e_mark_point("n", data = list(name = "Max",type = "max"), itemStyle = list(color = "red"))
# 
# 
# gw_test %>% 
#   mutate(
#     Type = ifelse(is.na(Type), "NA", Type),
#     Type = forcats::fct_lump(Type, 5)
#   ) %>% 
#   count(Type) %>% 
#   e_charts(Type,renderer="svg") %>% 
#   e_pie(n, roseType = "radius") %>% 
#   e_labels() %>% 
#   e_title("iLINCS Signatures", subtext = "Number of signatures per tissue type (derived from cell line) - Showing only top 5, and the rest grouped under 'Other") %>% 
#   e_toolbox() %>%
#   e_toolbox_feature(feature = "saveAsImage") %>% 
#   e_show_loading()
# 
# 
# gw_test %>% 
#   select(Gene, Type) %>% 
#   group_by(Gene, Type) %>% 
#   add_tally() %>% ungroup() %>% 
#   rename(Target = Type, Source = Gene ) -> gw_test1
# 
# 
# gw_test %>% 
#   select(Type, Trait) %>% 
#   group_by(Type, Trait) %>% 
#   add_tally() %>% ungroup() %>% 
#   rename(Target = Trait,
#          Source = Type) -> gw_test2
# 
# 
# 
# gw_test %>% 
#   select(Gene, Trait) %>% 
#   group_by(Gene, Trait) %>% 
#   add_tally() %>% ungroup() %>% 
#   rename(Target = Trait, Source = Gene ) -> gw_test3
# 
# 
# 
# rbind(gw_test1, gw_test2) %>% mutate(Links = "Indirect") %>% 
#   rbind(mutate(gw_test3, Links = "Direct")) %>% 
#   group_by(Links) %>% 
#   e_charts(timeline = T) %>% 
#   e_sankey(Source, Target, n)

# tbl(my_db,"gwas_data") %>% 
#   head(1) %>% collect() -> djjd

