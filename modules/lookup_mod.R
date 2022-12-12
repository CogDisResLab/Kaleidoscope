lookup_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(width = 12,  geneInputUI(ns("genes")),
             #actionButton(ns("use_l000"),label = "L1000 Genes"),
             hr()), 
      #shinydashboard::box(status = "info", width = 12,
      column(width = 3,
             pickerInput(inputId=ns("dbs1"),label="Schizophrenia",choices="",multiple = TRUE,options = list(`actions-box` = TRUE, `selected-text-format` = "count > 1")),
             pickerInput(inputId=ns("dbs2"),label="MDD",choices="",  multiple = T,options = list(`actions-box` = TRUE,`selected-text-format` = "count > 1")),
             pickerInput(inputId=ns("dbs13"),label="Asthma",choices="", multiple = TRUE,options = list(`actions-box` = TRUE,`selected-text-format` = "count > 1")),
             pickerInput(inputId=ns("dbs3"),label="Dopamine",choices="",  multiple = TRUE,options = list(`actions-box` = TRUE,`selected-text-format` = "count > 1"))
             ),
      column(width = 3,
             pickerInput(inputId=ns("dbs4"),label="Antipsychotics",choices="", multiple = TRUE,options = list(`actions-box` = TRUE,`selected-text-format` = "count > 1")),
             pickerInput(inputId=ns("dbs5"),label="Antidepressants",choices="",  multiple = TRUE,options = list(`actions-box` = TRUE,`selected-text-format` = "count > 1")),
             pickerInput(inputId=ns("dbs14"),label="PTSD",choices="", multiple = TRUE,options = list(`actions-box` = TRUE,`selected-text-format` = "count > 1")),
             pickerInput(inputId=ns("dbs6"),label="Alzheimer's Disease",choices="", multiple = TRUE,options = list(`actions-box` = TRUE,`selected-text-format` = "count > 1"))
      ),
      column(width = 3,
             pickerInput(inputId=ns("dbs7"),label="Insulin Signaling Inhibition",choices="",  multiple = TRUE,options = list(`actions-box` = TRUE,`selected-text-format` = "count > 1")),
             pickerInput(inputId=ns("dbs8"),label="Bipolar Disorder",choices="", multiple = TRUE,options = list(`actions-box` = TRUE,`selected-text-format` = "count > 1")),
             pickerInput(inputId=ns("dbs15"),label="Ketosis",choices="", multiple = TRUE,options = list(`actions-box` = TRUE,`selected-text-format` = "count > 1")),
             pickerInput(inputId=ns("dbs9"),label="Aging",choices="",  multiple = TRUE,options = list(`actions-box` = TRUE,`selected-text-format` = "count > 1"))
      ),
      column(width = 3,
             pickerInput(inputId=ns("dbs10"),label="Microcystin",choices="", multiple = TRUE,options = list(`actions-box` = TRUE,`selected-text-format` = "count > 1")),
             pickerInput(inputId=ns("dbs11"),label="Renal DBs",choices="", multiple = TRUE,options = list(`actions-box` = TRUE,`selected-text-format` = "count > 1")),
             pickerInput(inputId=ns("dbs12"),label="Myositis",choices="", multiple = TRUE,options = list(`actions-box` = TRUE,`selected-text-format` = "count > 1")),
             pickerInput(inputId=ns("dbs16"),label="Other",choices="", multiple = TRUE,options = list(`actions-box` = TRUE,`selected-text-format` = "count > 1"))
      #)
      ),
      ),
      #column(width = 12,
      #hidden(div(ns("lookup_tabset_div"),
      tabsetPanel(id = ns("lookup_tabset"),
                  
                  tabPanel("Summary",
                           
                           fluidRow(style="padding: 0 15px;",
                             column(width = 12, reactableOutput(ns("table")), hr(),
                                    div(id = ns("dn_btn_div"), download_btn_ui(ns("dn_btn")), download_btn_ui(ns("dn_btn2"), label = "Download with pvalues"))
                                    ),
                             column(width = 12, hr()),
                             column(width = 5,
                                    echarts4rOutput(ns("plot1"), height = "700")
                             ),
                             column(width = 1,
                                    hidden(noUiSliderInput(ns("slider1"), label = "LFC Threshold", min = 0, max = 2, value = 0.1, step = 0.05,
                                                           orientation = "vertical", height = "200", color = "#428bca",
                                                           direction = "rtl"
                                    ))
                             ),
                             column(width = 6,
                                    echarts4rOutput(ns("plot2"), height = "700")
                             ),
                             #div(ns("box_1"),
                             #shinydashboard::box(id = ns("box_1"),collapsible = T, width = 12,
                                                 column(width = 6, plotOutput(ns("plot3"), height = "500")),
                                                 column(width = 6, echarts4rOutput(ns("plot4"), height = "500"))
                             #)
                           )
                           
                           ),
                  tabPanel("Heatmap",
                           fluidRow(
                           column(width = 11, plotly::plotlyOutput(ns("heatmap_plot"), height = "800")),
                           column(width = 1, 
                                  hidden(noUiSliderInput(ns("hm_slider1"),label = "Adjust Scale", min = 0.1, max = 5, value = 1.5, step = 0.1,
                                                         orientation = "vertical", height = "150", color = "#428bca",direction = "rtl")
                                  ),
                                  hidden(noUiSliderInput(ns("hm_slider2"),label = "Col Text Size", min = 1, max = 10, value = 5, step = 0.5,
                                                         orientation = "vertical", height = "150", color = "#428bca",direction = "rtl")
                                  ),
                                  hidden(noUiSliderInput(ns("hm_slider3"),label = "Row Text Size", min = 1, max = 10, value = 5, step = 0.5,
                                                         orientation = "vertical", height = "150", color = "#428bca",direction = "rtl")
                                  )
                           )
                           )
                           )
                  # tabPanel("Enrichment",
                  #          fluidRow(
                  #            column(width = 3, offset = 1, align="center", hidden(sliderInput(ns("slider2"), label = "Top Geneset", min = 1, max = 50, value = 20, step = 1))),
                  #            column(width = 3, offset = 3, align="center", hidden(sliderInput(ns("slider3"), label = "Similarity Cutoff", min = 0.1, max = 1, value = 0.2, step = 0.1))),
                  #            column(width = 6,
                  #                   plotOutput(ns("plot5"), height = "500")
                  #            ),
                  #            column(width = 6, 
                  #                   visNetwork::visNetworkOutput(ns("geneset_network"), height = "500")
                  #            ),
                  #            column(width = 12, reactableOutput(ns("genset_table")))
                  #          )
                  #          )
                  
                  ),
             )

    )

  }


lookup_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      updatePickerInput(session, "dbs1", choices = filter(ks_datasets, Group == "SCZ") %>% pull(DataSet))
      updatePickerInput(session, "dbs2", choices = filter(ks_datasets, Group == "MDD") %>% pull(DataSet))
      updatePickerInput(session, "dbs3", choices = filter(ks_datasets, Group == "Dop") %>% pull(DataSet))
      updatePickerInput(session, "dbs4", choices = filter(ks_datasets, Group == "AP") %>% pull(DataSet))
      updatePickerInput(session, "dbs5", choices = filter(ks_datasets, Group == "ATD") %>% pull(DataSet))
      updatePickerInput(session, "dbs6", choices = filter(ks_datasets, Group == "AD") %>% pull(DataSet))
      updatePickerInput(session, "dbs7", choices = filter(ks_datasets, Group == "Ins") %>% pull(DataSet))
      updatePickerInput(session, "dbs8", choices = filter(ks_datasets, Group == "BPD") %>% pull(DataSet))
      updatePickerInput(session, "dbs9", choices = filter(ks_datasets, Group == "Aging") %>% pull(DataSet))
      updatePickerInput(session, "dbs10", choices = filter(ks_datasets, Group == "MCLR") %>% pull(DataSet))
      updatePickerInput(session, "dbs11", choices = filter(ks_datasets, Group == "KID") %>% pull(DataSet))
      updatePickerInput(session, "dbs12", choices = filter(ks_datasets, Group == "MYO") %>% pull(DataSet))
      updatePickerInput(session, "dbs13", choices = filter(ks_datasets, Group == "AST") %>% pull(DataSet))
      updatePickerInput(session, "dbs14", choices = filter(ks_datasets, Group == "PTSD") %>% pull(DataSet))
      updatePickerInput(session, "dbs15", choices = filter(ks_datasets, Group == "Ketosis") %>% pull(DataSet))
      updatePickerInput(session, "dbs16", choices = filter(ks_datasets, Group %in% c("COVID", "Other", "MB")) %>% pull(DataSet))

      ns <- session$ns
      w <- Waiter$new(ns(c("plot1", "plot2", "heatmap_plot")), 
                      html = spin_loaders(37, color = "black"), 
                      color = transparent(.5)
      )
      
      w_net <- Waiter$new(ns(c("geneset_network", "plot5")), 
                          html = spin_loaders(37, color = "black"), 
                          color = transparent(.5)
      )
      
      
      
      genes_ids <- geneOutput("genes")
      
      # observeEvent(input$use_l000, {
      #   updateTextInput(session,id = "lookupTab-genes-gene_input",value = paste(l1000_genes, collapse = ","))
      #   #genes_ids$genes() <- paste(l1000_genes, collapse = ",")
      # })
      
      observeEvent(genes_ids$info_btn(),{
        shinyalert(title = NULL, text = lookup_info_html, html = T, closeOnEsc = T, closeOnClickOutside = T)

      })

      observeEvent(genes_ids$btn(), {
        
        shinyjs::hide("lookup_tabset_div")
        shinyjs::hide("table")
        shinyjs::hide("dn_btn_div")
        shinyjs::hide("plot1")
        shinyjs::hide("plot2")
        shinyjs::hide("plot3")
        shinyjs::hide("plot4")
        shinyjs::hide("plot5")
        #shinyjs::hide("box_1")
        shinyjs::hide("box_2")
        shinyjs::hide("heatmap_plot")
        shinyjs::hide("geneset_network")
        shinyjs::hide("genset_table")
        shinyjs::hide("slider1")
        shinyjs::hide("slider2")
        shinyjs::hide("slider3")
        shinyjs::hide("hm_slider1")
        shinyjs::hide("hm_slider2")
        shinyjs::hide("hm_slider3")
        

        genes_raw <- isolate(genes_ids$genes())
        #req(genes)

        datasets_selected <- c(input$dbs1,input$dbs2, input$dbs3, input$dbs4, input$dbs5, input$dbs6, input$dbs7, 
                               input$dbs8, input$dbs9, input$dbs10, input$dbs11, input$dbs12, input$dbs13, input$dbs14, input$dbs15, input$dbs16)
        #req(datasets_selected)
        
        if(!any(all(trimws(genes_raw) == "") | is.null(datasets_selected))){
          w$show()
          
          genes <- process_gene_input(genes_raw)
          
          if (identical(genes, character(0))) {
            genes <- ""
          } else {}
          
          #genesets_ds <- c(paste0(datasets_selected, "_all"), paste0(datasets_selected, "_up"), paste0(datasets_selected, "_down"))
          
          
          look_res <<- withProgress(message = "connecting to KS database ...", {
            
            list(
              lookup_df = ks_lookup(genes = genes, datasets = datasets_selected)
              #geneset_df = ks_lookup_geneset(genes = genes, datasets = genesets_ds)
            )
            
          })
          
          
          
          
          if(!is.null(look_res$lookup_df)) {
            
            output$table <- renderReactable({
              look_res$lookup_df %>% select(HGNC_Symbol, Log2FC, DataSet) %>%
                pivot_wider(names_from = DataSet, values_from = Log2FC) %>%
                reactable(searchable = TRUE,
                          striped = TRUE,
                          bordered = TRUE)
            })
            
            download_btn_server(id = "dn_btn", 
                                look_res$lookup_df %>% select(HGNC_Symbol, Log2FC, DataSet) %>%
                                  pivot_wider(names_from = DataSet, values_from = Log2FC), 
                                name = "Lookup_Table")
            
            download_btn_server(id = "dn_btn2",
                                look_res$lookup_df %>% 
                                  mutate(Values = paste0(Log2FC, ", p=", P_Value)) %>% 
                                  select(HGNC_Symbol, DataSet, Values) %>% 
                                  pivot_wider(names_from = DataSet, values_from = Values),
                                name = "Lookup_Table_with_pvalues")
            
            
            
            
            
            output$plot1 <- renderEcharts4r({
              
              look_res$lookup_df %>%
                group_by(HGNC_Symbol) %>%
                summarise(Upregulated = sum(Log2FC > input$slider1),
                          Downregulated = sum(Log2FC< (input$slider1 * -1)) * -1) %>%
                pivot_longer(Upregulated:Downregulated, names_to = "Type") %>%
                group_by(Type) %>%
                e_charts(x = HGNC_Symbol) %>%
                e_bar(value, stack = "grp") %>%
                e_flip_coords() %>%
                e_grid(
                  left = 100, # pixels
                  top = "15%" # percentage = responsive
                ) %>%
                e_title(subtext = "Number of times a gene found to be upregulated or downregulated based on the selected cutoff") %>%
                e_toolbox() %>%
                e_toolbox_feature(feature = c("saveAsImage", "dataView")) %>%
                e_tooltip() %>% 
                e_legend(top = 35)
              
            })
            
            output$plot2 <- renderEcharts4r({
              
              look_res$lookup_df %>%
                group_by(DataSet) %>%
                summarise(Upregulated = sum(Log2FC > input$slider1),
                          Downregulated = sum(Log2FC< (input$slider1 * -1)) * -1) %>%
                pivot_longer(Upregulated:Downregulated, names_to = "Type") %>%
                group_by(Type) %>%
                e_charts(x = DataSet) %>%
                e_bar(value, stack = "grp") %>%
                e_flip_coords() %>%
                e_grid(
                  left = 250, # pixels
                  top = "15%" # percentage = responsive
                ) %>%
                e_title(subtext = "Number of genes found to be upregulated or downregulated based on the selected cutoff in each dataset") %>%
                e_toolbox() %>%
                e_toolbox_feature(feature = c("saveAsImage", "dataView")) %>%
                e_tooltip() %>% 
                e_legend(top = 35)
              
            })
            
            
            output$plot3 <- renderPlot({
              look_res$lookup_df %>% 
                group_by(HGNC_Symbol) %>%
                mutate(mean_LFC=median(Log2FC)) %>%
                ungroup() %>% 
                ggplot(aes(x = reorder(HGNC_Symbol, Log2FC, median), y = Log2FC, fill = mean_LFC)) +
                ggdist::stat_halfeye(adjust = .9,width = .7,justification = -.2,.width = 0,point_colour = NA) +
                geom_boxplot(width = .3,outlier.color = NA) +
                gghalves::geom_half_point(side = "l", range_scale = .4, alpha = .3, size = 1) +
                scale_fill_gradient2(low="blue",high = "red", mid = "orange", midpoint = 0) +
                coord_flip() +
                theme(legend.position = "none") +
                labs(x = "", title = "Differential Gene Expression Patterns Across all Selected Datasets")
            })
            
            
            output$plot4 <- renderEcharts4r({
              
              look_res$lookup_df %>%
                group_by(HGNC_Symbol) %>%
                summarise(Upregulated = sum(Log2FC> input$slider1),
                          Downregulated = sum(Log2FC< (input$slider1 * -1)),
                          Unchnaged = sum(Log2FC > (input$slider1 * -1) && Log2FC < input$slider1)) %>%
                pivot_longer(Upregulated:Unchnaged, names_to = "Type") %>%
                group_by(Type) %>%
                summarise(n = sum(value)) %>%
                e_charts(Type) %>%
                e_pie(n, radius = c("40%", "60%"), itemStyle = list(borderRadius = 10, borderWidth = 2),
                      label = list(formatter = "{b}: {@n} ({d}%)")
                ) %>%
                e_title(subtext = "Percentage of input genes found to be upregulated or downregulated based on the selected cutoff") %>% 
                e_toolbox() %>%
                e_toolbox_feature(feature = c("saveAsImage", "dataView")) %>%
                e_tooltip() %>% 
                e_legend(bottom = 0)
              
            })
            
            output$heatmap_plot <- plotly::renderPlotly({
              
              look_res$lookup_df %>% select(HGNC_Symbol, Log2FC, DataSet) %>%
                pivot_wider(names_from = DataSet, values_from = Log2FC) %>%
                column_to_rownames("HGNC_Symbol") -> mm
              
              
              labelmatrix<-matrix(paste0("LFC: ",as.numeric(unlist(mm))),nrow=nrow(mm),ncol=ncol(mm))
              
              heatmaply::heatmaply(
                matrix_rescale(mm, min = (input$hm_slider1 * -1), max = input$hm_slider1), scale = "none", 
                Rowv = ifelse(nrow(mm) > 1, T, F),
                fontsize_col = input$hm_slider2,
                fontsize_row = input$hm_slider3,
                scale_fill_gradient_fun = scale_fill_gradient2(
                  low = "blue", 
                  mid = "white",
                  high = "red", 
                  midpoint = 0, 
                  limits = c((input$hm_slider1 * -1), input$hm_slider1)),
                label_names = c("Gene", "DataSet", "Scaled Value"),
                custom_hovertext = labelmatrix
              )  %>% 
                plotly::config(
                  toImageButtonOptions = list(
                    format = "svg",
                    filename = "lookup_hm"
                  )
                )
              
              
              
              
            })
            
            
            # output$geneset_network <- visNetwork::renderVisNetwork({
            #   w_net$show()
            #   
            #   on.exit({
            #     w_net$hide()
            #   })
            #   look_res$geneset_df %>% hypeR::hyp_emap(similarity_cutoff = input$slider3, title = "Enrichment Network (Nodes = genesets, edges= similarity between genesets, color= enrichment significance - FDR ) - zoom in to reveal the names")
            #   
            # })
            # 
            # 
            # 
            # output$plot5 <- renderPlot({
            #   
            #   w_net$show()
            #   
            #   on.exit({
            #     w_net$hide()
            #   })
            #   
            #   look_res$geneset_df %>% hypeR::hyp_dots(top = input$slider2, title = "Gene Set Enrichment Analysis (FDR)") + 
            #     labs(y = "")
            #   
            # })
            # 
            # output$genset_table <- renderReactable({
            #   
            #   look_res$geneset_df$as.data.frame() %>% 
            #     select(-background) %>% as_tibble() %>%
            #     rename(Geneset = label, Geneset_Size = geneset, `P-Value` = pval, 
            #            Input_Size = signature, FDR = fdr, 
            #            Overlap = overlap, Hits = hits) %>% 
            #     reactable::reactable(searchable = TRUE,
            #                          striped = TRUE,
            #                          bordered = TRUE)
            #   
            # })
            
            
            
        
            
            shinyjs::show("lookup_tabset_div")
            shinyjs::show("table")
            
            shinyjs::show("plot1")
            shinyjs::show("plot2")
            shinyjs::show("plot3")
            shinyjs::show("plot4")
            shinyjs::show("plot5")
            shinyjs::show("dn_btn_div")
            #shinyjs::show("box_1")
            shinyjs::show("box_2")
            shinyjs::show("heatmap_plot")
            shinyjs::show("geneset_network")
            shinyjs::show("genset_table")
            shinyjs::show("slider1")
            shinyjs::show("slider2")
            shinyjs::show("slider3")
            shinyjs::show("hm_slider1")
            shinyjs::show("hm_slider2")
            shinyjs::show("hm_slider3")
            
          
            
          }
          
          else {
            shinyalert("Opps", "Couldn't Find Your Gene/s", type = "error")
            
          }
          
          
        }
        else {
          
          if(all(trimws(genes_raw) == "")){
            shinyalert("Opps", "Please Enter a Gene Symbol", type = "error")
          }
          else {
            shinyalert("Opps", "Please Select at Least One Dataset", type = "error")
          }

        }
        
        shinyFeedback::resetLoadingButton("genes-btn")
        
        
        
        


      })
    }
  )
}


ks_lookup <- function(genes, datasets, db = my_db) {
  
  suppressWarnings({
    tbl(my_db, "lookup_updated_annotated") %>% filter(HGNC_Symbol %in% genes, DataSet %in% datasets) %>% 
      collect() %>% 
      distinct(HGNC_Symbol, DataSet, .keep_all = T) -> table_res
  })
  
  if(nrow(table_res) > 0) {
    return(table_res)
  }
  
  else {
    return(NULL)
  }
  
}


# ks_lookup_geneset <- function(genes, datasets, db = my_db){
#   suppressWarnings({
#     tbl(my_db, "lookup_genesets") %>% filter(DataSet %in% datasets) %>% 
#       collect()  -> table_res
#     
#     table_res %>% 
#       select(DataSet, HGNC_Symbol) %>% 
#       group_by(DataSet) %>% 
#       summarize(geneset=list(unique(HGNC_Symbol))) %>% 
#       deframe() -> gensets_list
#     
#     table_res <- hypeR::hypeR(genes, gensets_list, test="hypergeometric", background=23467, fdr=1)
#     
#   })
#   
#   if(T) {
#     return(table_res)
#   }
#   
#   else {
#     return(NULL)
#   }
# }


lookup_info_html <- HTML('<div class="features-icon"><span class="icon-search"></span></div>
                  <h3 class="features-title font-alt">Lookup</h3>
                  <p>Differential gene expression signatures across different diseases and drug treatments. Disease/Drug based modules of previously published studies (RNAseq and microarray) curated and harmonized to be used as 
lookup replication studies.</p>
                         <h3 class = "features-title font-alt">Resources</h3>
                         <p>
                         <a href="https://www.ncbi.nlm.nih.gov/geo/" target="_blank" rel="noopener noreferrer">GEO</a>
                         </p>
                         ')


# BuYlRd <- function(x) rgb(colorRamp(c("#7fb7d7", "#ffffbf", "#fc8d59"))(x), maxColorValue = 255)
# 
# look_res %>% select(HGNC_Symbol, Log2FC, DataSet) %>% 
#   mutate(Log2FC = ifelse(is.na(Log2FC), 0, Log2FC)) %>% 
#   pivot_wider(names_from = DataSet, values_from = Log2FC, values_fill = 0) %>% 
#   reactable(searchable = TRUE,
#             striped = TRUE,
#             bordered = TRUE,
#             defaultColDef = colDef(
#               style = function(value) {
#                 if (!is.numeric(value)) return()
#                 color <- BuYlRd(value)
#                 list(background = color)
#               },
#               format = colFormat(digits = 1),
#               minWidth = 50
#             )
#             
#             )
# 
# BuYlRd(1)


matrix_rescale<-function(matrix, min = -2, max = 2){
  transformedmatrix <- matrix
  transformedmatrix[is.na(transformedmatrix)] = 0
  transformedmatrix <- transformedmatrix[apply(transformedmatrix, 1, var) != 0,,drop = F] 
  transformedmatrix[transformedmatrix>max] = max 
  transformedmatrix[transformedmatrix<min] = min 
  return(transformedmatrix)
}



# look_res$lookup_df %>%
#   group_by(HGNC_Symbol) %>%
#   summarise(Upregulated = sum(Log2FC > 0.2),
#             Downregulated = sum(Log2FC< (02 * -1))) %>%
#   mutate(Downregulated = -4) %>% 
#   #pivot_longer(Upregulated:Downregulated, names_to = "Type") %>%
#   #group_by(Type) %>%
#   e_charts(x = HGNC_Symbol) %>%
#   e_bar(Upregulated, stack = "grp") %>%
#   e_bar(Downregulated, stack = "grp2", x_index = 1) %>%
#   e_x_axis(index = 1, inverse = T, formatter = abs("value")) %>% 
#   e_x_axis(index = 0, inverse = F) %>% 
#   e_flip_coords() %>%
#   e_grid(
#     left = 100, # pixels
#     top = "15%" # percentage = responsive
#   ) %>%
#   #e_title("iLINCS Signatures", subtext = "Number of gene knockdown or over expression signatures") %>%
#   e_toolbox() %>%
#   e_toolbox_feature(feature = c("saveAsImage", "dataView")) %>%
#   e_tooltip()

