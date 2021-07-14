
brainatlas_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 12,  geneInputUI(ns("genes")), hr()),
      column(width = 6, echarts4rOutput(ns("plot1"), height = "500")),
      column(width = 6, echarts4rOutput(ns("plot2"), height = "500")),
      column(width = 12, hidden(switchInput(ns("switch1"), "log", value = T, size = "small"))),
      column(width = 12, echarts4rOutput(ns("plot3"), height = "500")),
    ),
    fluidRow(
      column(width = 11, plotly::plotlyOutput(ns("plot4"), height = "800")),
      column(width = 1, 
             
             hidden(noUiSliderInput(ns("hm_slider2"),label = "Col Text Size", min = 1, max = 10, value = 5, step = 0.5,
                                    orientation = "vertical", height = "150", color = "#428bca",direction = "rtl")
             ),
             
             hidden(noUiSliderInput(ns("hm_slider3"),label = "Row Text Size", min = 1, max = 10, value = 5, step = 0.5,
                                    orientation = "vertical", height = "150", color = "#428bca",direction = "rtl")
             ),
             hidden(switchInput(ns("switch2"), "scale", value = T, size = "small"))
      ),
      column(width = 6, reactableOutput(ns("table")), 
            div(id = ns("dn_btn_div"), download_btn_ui(ns("dn_btn"))),
             ),
      column(width = 6, echarts4rOutput(ns("plot5")))
    )
  )
  
}


brainatlas_server <- function(id, ...) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      w <- Waiter$new(ns(c("plot1", "plot2", "plot3", "plot4", "table")), 
                      html = spin_loaders(37, color = "black"), 
                      color = transparent(.5)
      )
      
      genes_ids <- geneOutput("genes")
      
      observeEvent(genes_ids$info_btn(),{
        shinyalert(title = NULL, text = brainatlas_info_html, html = T, closeOnEsc = T, closeOnClickOutside = T)
        
      })
      
      observeEvent(genes_ids$btn(), {
        
        genes <- isolate(genes_ids$genes())
        req(genes)
        
        w$show()
        
        shinyjs::hide("plot1")
        shinyjs::hide("plot2")
        shinyjs::hide("plot3")
        shinyjs::hide("plot4")
        shinyjs::hide("plot5")
        shinyjs::hide("switch1")
        shinyjs::hide("switch2")
        shinyjs::hide("table")
        shinyjs::hide("hm_slider2")
        shinyjs::hide("hm_slider3")
        shinyjs::hide("dn_btn_div")
        
        genes <- process_gene_input(genes)
        
        ba_res <- withProgress(message = "connecting to BrainAtlas ...", {
          ks_brainatlas(genes = genes, db = my_db) 
        })
        
        if(!is.null(ba_res)) {
          
          
          output$plot1 <- renderEcharts4r({
            
            ba_res %>% 
              {if(input$switch1)  mutate(.,CPM_mean = log_transfomed(CPM_mean))  else .} %>% 
              group_by(CellType) %>% 
              e_charts(renderer="svg") %>% 
              e_boxplot(CPM_mean) %>% 
              e_toolbox() %>%
              e_toolbox_feature(feature = c("saveAsImage", "dataView")) %>% 
              e_tooltip() %>% 
              e_show_loading(color = "black")

          })
          
          output$plot2 <- renderEcharts4r({
            ba_res %>% 
              {if(input$switch1)  mutate(.,CPM_mean = log_transfomed(CPM_mean))  else .} %>% 
              group_by(gene) %>% 
              e_charts(x = CellType,renderer="svg", timeline = T) %>% 
              e_bar(CPM_mean, legend = F) %>% 
              e_toolbox() %>%
              e_toolbox_feature(feature = c("saveAsImage", "dataView")) %>% 
              e_tooltip()  %>% 
              e_show_loading(color = "black")
              
          })
          
          output$plot3 <- renderEcharts4r({
            
            ba_res %>% 
              {if(input$switch1)  mutate(.,CPM_mean = log_transfomed(CPM_mean))  else .} %>% 
              {if(length(genes) == 1) {
                arrange(.,desc(CPM_mean)) %>% 
                  group_by(CellType) %>% 
                  e_chart(.,cluster,renderer="svg", reorder = F) %>% 
                  e_bar(e = .,CPM_mean, stack = "grp", legend = T) %>% 
                  e_legend(selectedMode = F)
              } else {
                group_by(.,cluster) %>% 
                  arrange(.,cluster, desc(CPM_mean)) %>%
                  e_chart(.,renderer="svg", reorder = F) %>% 
                  e_boxplot(e = .,CPM_mean, outlier = F)
              } } %>% 
              e_x_axis(type = 'category', 
                       axisLabel = list(interval=0, rotate = 45, fontSize = 6)) %>% 
              e_toolbox() %>%
              e_toolbox_feature(feature = c("saveAsImage", "dataView")) %>% 
              e_tooltip()  %>% 
              e_show_loading(color = "black")

            
          })
          
          output$plot4 <- plotly::renderPlotly({
            
            
            
            
            ba_res %>% select(-CellType) %>% 
              {if(input$switch1)  mutate(.,CPM_mean = log_transfomed(CPM_mean))  else .} %>% 
              pivot_wider(names_from = cluster, values_from = CPM_mean) %>% 
              column_to_rownames("gene") -> mm
            
            mm <- mm[apply(mm, 1, var) != 0, ] 
            
            
            heatmaply::heatmaply(
              mm, scale = ifelse(input$switch2, "row", "none"), 
              fontsize_col = input$hm_slider2,
              fontsize_row = input$hm_slider3,
              Rowv = ifelse(length(ba_res$gene %>% unique()) > 1, T, F )
              
            )
            
          })
          
          output$table <- renderReactable({
            reactable(ba_res,
                      searchable = TRUE,
                      striped = TRUE,
                      bordered = TRUE,
                      filterable = TRUE
            )
          })
          
          # output$dn_btn <- downloadHandler(
          #   filename = function() {
          #     paste("BrainAtlas_Table", "_", Sys.time(), ".csv", sep = "")
          #   },
          #   content = function(file) {
          #     write.csv(ba_res, file, row.names = FALSE)
          #   }
          # )
          
          #genes_ids$info_btn()
          
          download_btn_server(id = "dn_btn", tbl = ba_res, name = "BrainAtlas_Table")
          #output$dn_btn <- dn_btn_res$x()
          
          output$plot5 <- renderEcharts4r({
            ba_res %>% 
              group_by(CellType) %>% 
              summarise(FPKM = mean(CPM_mean)) %>% 
              mutate(total = sum(FPKM), mean_prob = (FPKM/total)*100, 
                     Avg = "Input") %>% 
              select(CellType, mean_prob, Avg) %>% 
              rbind(ba_all_human_avg) %>% select(CellType, Avg, mean_prob) %>% 
              mutate_if(is.numeric, round, 2) %>% 
              pivot_wider(names_from = Avg, values_from = mean_prob) %>% 
              e_charts(CellType, renderer="svg") %>% 
              e_radar(Avg, max = 50) %>% 
              e_radar(Input, max = 50) %>% 
              e_title("BrainAtlas", subtext = "Proportion of Gene Expression values Per Cell Type") %>%
              e_tooltip() %>% 
              e_toolbox() %>%
              e_toolbox_feature(feature = c("saveAsImage", "dataView")) %>% 
              e_legend(bottom = 0) %>% 
              e_show_loading(color = "black")
          })
          
          shinyjs::show("plot1")
          shinyjs::show("plot2")
          shinyjs::show("plot3")
          shinyjs::show("plot4")
          shinyjs::show("hm_slider2")
          shinyjs::show("hm_slider3")
          shinyjs::show("switch1")
          shinyjs::show("switch2")
          shinyjs::show("table")
          shinyjs::show("plot5")
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


ks_brainatlas <- function(genes, db = my_db) {
  
  tbl(my_db, "brain_atlas_data") %>% filter(gene %in% genes) %>% 
    select(gene, cluster, CellType, CPM_mean) %>%
    collect() %>% mutate_if(is.numeric, round, 2) -> table_res
  
  if(nrow(table_res) > 0) {
    return(table_res)
  }
  
  else {
    return(NULL)
  }
  
  
}

brainatlas_info_html <- HTML('<div class="features-icon"><span class="icon-aperture"></span></div>
                  <h3 class="features-title font-alt">BrainAtlas</h3>
                  <p>Gene expression of nuclei derived from frozen human brain specimens to survey cell type diversity. Anatomical 
specificity is achieved by microdissecting tissue from defined brain areas. Currently this the tab contains: RNA-Seq 
data created from intact nuclei derived from frozen human brain specimens, to survey cell type diversity in the human
middle temporal gyrus (MTG). In total, 15,928 nuclei from 8 human tissue donors ranging in age from 24-66 years were 
analyzed. Analysis of these transcriptional profiles reveals approximately 75 transcriptionally distinct cell types, 
subdivided into 45 inhibitory neuron types, 24 excitatory neuron types, and 6 non-neuronal types.</p>
                         <h3 class = "features-title font-alt">Resources</h3>
                         <p>
                         <a href="https://pubmed.ncbi.nlm.nih.gov/22996553/" target="_blank" rel="noopener noreferrer">PMID: 22996553</a>
                         <br>
                         <a href="https://portal.brain-map.org/atlases-and-data/rnaseq" target="_blank" rel="noopener noreferrer">Website</a>
                         </p>
                         ')

# tests

# tbl(my_db, "brain_atlas_data") %>% filter(gene %in% c("AKT1", "AKT2")) %>% 
#   select(-row_names) %>% collect() -> ba_test
# 
# ba_test %>% 
#   group_by(CellType) %>% 
#   e_charts() %>% 
#   e_boxplot(CPM_mean)
# 
# 
# 

# ba_test %>% 
#   group_by(gene) %>% 
#   mutate(color = case_when(
#       CellType == "Inh" ~ "#ee6666",
#       CellType == "Exc" ~ "#3ba272",
#       CellType == "Astro" ~ "#5470c6",
#       CellType == "Endo" ~ "#73c0de",
#       CellType == "Micro" ~ "#fac858",
#       CellType == "Oligo" ~ "#fc8452",
#       CellType == "OPC" ~ "#9a60b4",
#       T ~ "black"
#     )) %>% 
#   e_charts(CellType, timeline = T) %>% 
#   e_bar(CPM_mean, legend = F) %>% 
#   e_add("itemStyle", color)

# ba_test %>% 
#   group_by(cluster) %>% 
#   e_charts() %>% 
#   e_boxplot(CPM_mean)
# 
# ba_test %>% select(gene, cluster, CPM_mean) %>% 
#   pivot_wider(names_from = cluster, values_from = CPM_mean) %>% 
#   column_to_rownames("gene") -> mm
# 
# 
# heatmaply::heatmaply(
#   mm, scale = "row", fontsize_col = 7.5
#   
# )

# ba_test %>% filter(gene == "AKT1") %>% 
#   mutate(color = case_when(
#   CellType == "Inh" ~ "red",
#   CellType == "Exc" ~ "green",
#   T ~ "black"
# )) %>%
#   mutate(CellType = as_factor(CellType)) %>%
#   #group_by(CellType) %>%
#   e_charts(x = CellType) %>%
#   e_bar(CPM_mean) %>% 
#   e_add("itemStyle", color)

# df <- data.frame(
#   year = c(
#     rep(2016, 25),
#     rep(2017, 25),
#     rep(2018, 25),
#     rep(2019, 25)
#   ),
#   x = rep(letters[1:4],25),
#   y = rnorm(100),
#   grp = c(
#     rep("A", 50),
#     rep("B", 50)
#   )
# ) 
# df %>% mutate(color=ifelse(x=="a","blue", 
#                            ifelse(x=="b","yellow",
#                                   ifelse(x=="c", "red","green")))) %>%
#   group_by(year) %>% 
#   e_charts(x, timeline = TRUE) %>% 
#   e_scatter(y, symbol_size = 20, legend = F) %>%
#   e_add("itemStyle", color)
# 
# 
# ba_test %>% mutate(color = case_when(
#   CellType == "Inh" ~ "red",
#   CellType == "Exc" ~ "green",
#   T ~ "black"
# )) %>% 
#   mutate(CellType = as_factor(CellType)) %>% 
#   group_by(CellType) %>% 
#   e_charts() %>% 
#   e_boxplot(CPM_mean) %>%
#   e_color(sample(colors(), 7))




# tbl(my_db, "brain_atlas_data") %>% collect() -> bs_all_human
# 
# colnames(bs_all_human)
# 
# head(bs_all_human)
# 
# bs_all_human %>% select(-row_names, -gene_name) %>%
#   distinct(gene, cluster, .keep_all = T) -> bs_all_human
# 
# bs_all_human %>%
#   group_by(gene, CellType) %>%
#   summarise(FPKM = mean(CPM_mean)) %>%
#   mutate(total = sum(FPKM), prob = (FPKM/total)*100,
#          Avg = "Avg"
#          ) -> all_human_avg
# 
# all_human_avg %>% 
#   ungroup() %>% 
#   group_by(CellType) %>% 
#   summarise(mean_prob = mean(prob, na.rm = T)) %>% 
#   ungroup() %>% mutate(Avg = "Avg") -> all_human_avg_fin
# 
# 
# saveRDS(all_human_avg_fin,"modules/brainatlas_avg_all_genes.RDS")
# 
# 
# 
# ks_brainatlas(c("NRXN1")) -> bs_res
# 
# 
# bs_res %>% 
#   group_by(CellType) %>% 
#   summarise(FPKM = mean(CPM_mean)) %>% 
#   mutate(total = sum(FPKM), mean_prob = (FPKM/total)*100, 
#          Avg = "Input") %>% 
#   select(CellType, mean_prob, Avg) %>% 
#   rbind(all_human_avg_fin) %>% select(CellType, Avg, mean_prob) %>% 
#   mutate_if(is.numeric, round, 2) %>% 
#   pivot_wider(names_from = Avg, values_from = mean_prob) %>% 
#   e_charts(CellType, renderer="svg") %>% 
#   e_radar(Avg, max = 70) %>% 
#   e_radar(Input, max = 70) %>% 
#   e_title("Brain RNA-Seq in Human", subtext = "Proportion of Gene Expression values Per Cell Type") %>%
#   e_tooltip() %>% 
#   e_toolbox() %>%
#   e_toolbox_feature(feature = c("saveAsImage", "dataView")) %>% 
#   e_legend(left = "left", top = "middle", orient = "vertical")







