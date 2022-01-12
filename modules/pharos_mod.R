pharos_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 12,  geneInputUI(ns("genes"), label = "Enter a gene target (HGNC Symbol)")),
    ),hr(),
    fluidRow(
      column(width = 6,
             uiOutput(ns("gene_card_ui"))
      ),
      column(width = 6, 
             echarts4rOutput(ns("idg_level")),
             uiOutput(ns("link_cards"))
      ),
      #div("div_1",class="divider-w mt-10 mb-10"),
    ),div(id = ns("div_1"),class="divider-w mt-10 mb-20"),
    fluidRow(
      column(width = 6, echarts4rOutput(ns("pub_plot"))),
      column(width = 6, echarts4rOutput(ns("disease_plot")))
    ),hidden(div(id = ns("div_2"),class="divider-w mt-10 mb-20")),
    fluidRow(
      hidden(div(id = ns("path_title"), h2(class = "module-title font-alt", style = "margin-bottom: 10px;","Pathways"))),
      #div(class = "titan-caption font-alt mb-30 titan-title-size-1","Pathways"),
      column(width = 4, 
             #div(class = "tag status-knockdown", h2("Reactome")),
             reactableOutput(ns("reactome_tbl"))),
      column(width = 4, reactableOutput(ns("kegg_tbl"))),
      column(width = 4, reactableOutput(ns("wiki_tbl"))),
    ),hidden(div(id = ns("div_3"),class="divider-w mt-10 mb-20")),
    fluidRow(
      column(width = 6, echarts4rOutput(ns("tinx_plot"))),
      column(width = 6, reactableOutput(ns("tinx_tbl")))
    ),hidden(div(id = ns("div_4"),class="divider-w mt-10 mb-20")),
    fluidRow(
      column(width = 12, 
             uiOutput(ns("drugs"))
             #reactableOutput(ns("drug_tbl"))
             )
    ),#hr(),
    fluidRow(
      column(width = 12,align="center",
             div(id = ns("dn_btn_div"), download_btn_ui(ns("dn_btn"), label = "Download Table")),
             
      )
    ),
  )
}


pharos_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      w <- Waiter$new(ns(c("idg_level", "gene_card_ui")), 
                      html = spin_loaders(37, color = "black"), 
                      color = transparent(.5)
      )
      
      genes_ids <- geneOutput("genes")
      
      observeEvent(genes_ids$info_btn(),{
        shinyalert(title = NULL, text = idg_lvl_info2, html = T, closeOnEsc = T, closeOnClickOutside = T, size = "l")
        
      })
      
      observeEvent(input$idg_info_btn,{
        shinyalert(title = NULL, text = brainseq_info_html, html = T, closeOnEsc = T, closeOnClickOutside = T)
        
      })
      
      observeEvent(genes_ids$btn(), {
        
        #shinyjs::hide("gene_card_ui")
        #shinyjs::hide("idg_level")
        shinyjs::hide("link_cards")
        shinyjs::hide("pub_plot")
        shinyjs::hide("disease_plot")
        shinyjs::hide("path_title")
        shinyjs::hide("reactome_tbl")
        shinyjs::hide("kegg_tbl")
        shinyjs::hide("wiki_tbl")
        shinyjs::hide("tinx_plot")
        shinyjs::hide("tinx_tbl")
        shinyjs::hide("drugs")
        shinyjs::hide("div_1")
        shinyjs::hide("div_2")
        shinyjs::hide("div_3")
        shinyjs::hide("div_4")
        
        
        genes <- isolate(genes_ids$genes())
        
        w$show()
        
        genes <- process_gene_input(genes)[1]

        pharos_res <- withProgress(message = "connecting to Pharos API ...", {
          ks_pharos(gene = list(gene = genes), conn = conn) 
        })
        
        if(!is.null(pharos_res)) {
          uni_id <- gsub("\\s+", "", pharos_res$info$uniprot)
          db_str <- tempfile(pattern = uni_id, fileext = ".pdb")
          download.file(paste0("https://pharos-alphafold.ncats.io/models/AF-",uni_id,"-F1-model_v1.pdb"), 
                        destfile = db_str
          )
          
          output$gene_card_ui <-  renderUI({
            
            gene_card(
              pharos_res$info, 
              NGLVieweR(db_str) %>%
                stageParameters(backgroundColor = "white", zoomSpeed = 1) %>%
                addRepresentation("cartoon",
                                  param = list(name = "cartoon", colorScheme = "residueindex")
                ) %>%
                setSpin()
              
            )
          })
          
          
          output$idg_level <- renderEcharts4r({
            
            idg_l <- pharos_res$info$tdl
            
            col_idg <- case_when(
              idg_l == "Tdark" ~ "505050",
              idg_l == "Tbio" ~ "#FFBF00",
              idg_l == "Tchem" ~ "#6495ED",
              idg_l == "Tclin" ~ "#50C878",
              T ~ "white"
            )
            
            e_charts(renderer="svg") %>% 
              e_gauge(value = case_when(
                idg_l == "Tdark" ~ 12.5,
                idg_l == "Tbio" ~ 37.5,
                idg_l == "Tchem" ~ 62.5,
                idg_l == "Tclin" ~ 87.5,
                T ~ 0
              ),
              name = idg_l,
              startAngle= 180,
              endAngle= 0,
              pointer = list(
                show= F
              ),
              title = list(offsetCenter = c("0%", "-30%"), borderColor= 'black',
                           borderRadius= 50, fontSize=30, width=120,backgroundColor = col_idg,
                           height= 35,
                           borderWidth= 1),
              progress = list(
                show = T,
                overlap= F,
                roundCap= T,
                clip= F,
                itemStyle=list(
                  color = col_idg,
                  borderWidth= 1,
                  borderColor= '#464646'
                )
              ),
              axisTick=list(
                show= F
              ),
              splitNumber = 4,
              axisLabel=list(
                show= F
              ),
              splitLine=list(
                show= T,
                distance= 0,
                length= 10
              ),
              detail = list(show=F)
              ) %>% 
              e_title("IDG Development Level", left = "center") %>% 
              e_toolbox(
                show = T,
                itemSize = 22,
                right = "20%",
                feature = list(
                  myInfo = list(
                    show = T,
                    title= 'info',
                    icon= "image://https://img.icons8.com/color/48/000000/info--v1.png",
                    onclick = 
                      JS(paste0("function (){swal({ html:true, title: '', text:'",str_squish(gsub("\n", "",idg_lvl_info2)),"', customClass: 'alert-size-l sweet-alert'});}"))
                  )
                )
              )  %>% e_toolbox_feature(feature = c("saveAsImage"))
            
          })
          
          # map(split(db_links, db_links$sourceName), link_card2)
          # 
          pharos_res$affiliate_links %>%
            filter(sourceName %in% c("ARCHS4", "GENEVA", "GlyGen")) -> db_links
          
          link_list <- split(db_links, db_links$sourceName)
          
          output$link_cards <- renderUI(
            list(
              div(style = "margin-top: -30px;",
              div(h2(class = "module-title font-alt", style = "margin-bottom: 10px;","External Links")),
              div(style = "display:flex; justify-content:space-between;",
                  purrr::map(link_list, link_card2)
                  
                  )
            )
              #purrr::map(link_list, link_card2)
            )
          )
          
          # output$link_cards <- renderUI({
          #   div(
          #     div(style = "display:flex; flex-wrap: wrap; justify-content:space-between;",
          #         purrr::map(1:4, link_card)
          #         )
          #   )
          #   #purrr::map(1:3, link_card)
          # })
          
          output$reactome_tbl <- renderReactable({
            reactable(pharos_res$pathways %>% 
                        filter(type == "Reactome") %>% 
                        select(type, name, url), 
                      #searchable = TRUE,
                      defaultPageSize = 5,
                      striped = TRUE,
                      bordered = TRUE,
                      
                      columns = list(
                        url = colDef(
                          name = "Link",
                          align = "center",
                          width = 75,
                          cell = function(value) {
                            htmltools::tags$a(href = value, target = "_blank", icon("","icon-expand", verify_fa = FALSE))
                          }
                        ),
                        type = colDef(
                          name = "Database",
                          align = "center",
                          width = 125,
                          cell = function(value) {
                            div(class = "tag status-reactome", value)
                        }),
                        name = colDef(
                          name = "Pathawy"
                        )
                      )
            )
          })
          
          output$kegg_tbl <- renderReactable({
            reactable(pharos_res$pathways %>% 
                        filter(type == "KEGG") %>% 
                        select(type, name, url)
                        , 
                      defaultPageSize = 5,
                      #searchable = TRUE,
                      striped = TRUE,
                      bordered = TRUE,
                      
                      columns = list(
                        url = colDef(
                          name = "Link",
                          align = "center",
                          width = 75,
                          cell = function(value) {
                            htmltools::tags$a(href = value, target = "_blank", icon("","icon-expand", verify_fa = FALSE))
                          }
                        ),
                        type = colDef(
                          name = "Database",
                          align = "center",
                          width = 125,
                          cell = function(value) {
                            div(class = "tag status-knockdown", value)
                        }),
                        name = colDef(
                          name = "Pathawy"
                        )
                      )
            )
          })
          
          output$wiki_tbl <- renderReactable({
            reactable(pharos_res$pathways %>% 
                        filter(type == "WikiPathways") %>% 
                        select(type, name, url), 
                      #searchable = TRUE,
                      defaultPageSize = 5,
                      striped = TRUE,
                      bordered = TRUE,
                      
                      columns = list(
                        url = colDef(
                          name = "Link",
                          align = "center",
                          width = 75,
                          cell = function(value) {
                            htmltools::tags$a(href = value, target = "_blank", icon("","icon-expand", verify_fa = FALSE))
                          }
                        ),
                        type = colDef(
                          name = "Database",
                          align = "center",
                          width = 125,
                          cell = function(value) {
                            div(class = "tag status-overexpression", value)
                        }),
                        name = colDef(
                          name = "Pathawy"
                        )
                      )
            )
          })
          
          output$pub_plot <- renderEcharts4r({
            rbind(mutate(pharos_res$pubmedScores, Type = "PubMed"), 
                  mutate(pharos_res$pubTatorScores, Type = "PubTator")
            ) %>% 
              {if(nrow(pharos_res$patentCounts)>0) {
                full_join(.,mutate(pharos_res$patentCounts, Type = "Patent"))
              } else .} %>% 
              #full_join(mutate(res2$patentCounts, Type = "Patent")) %>% 
              filter(year >= 2000, year < 2019) %>% 
              group_by(Type) %>% 
              e_charts(year, renderer="svg") %>% 
              e_line(score) %>% 
              {if(nrow(pharos_res$patentCounts)>0) {e_line(.,count, y_index = 1)} else .} %>% 
              e_x_axis(type= "category") %>% 
              e_y_axis(name = "Score", nameLocation = 'center', nameGap = 40) %>% 
              e_legend(selected = list(
                PubMed = T,
                PubTator = T, 
                Patent = F
              )) %>% 
              e_title("Publication Statistics", 
                      subtext = "Statistics about the mentions of this target in literature, extracted via text mining") %>% 
              e_tooltip() %>% 
              e_toolbox(right = "10%") %>%
              e_toolbox_feature(feature = c("saveAsImage", "dataView"))
            
            
          })
          
          if(nrow(pharos_res$diseases) > 0) {
            output$disease_plot <- renderEcharts4r({
              
              pharos_res$diseases %>% 
                mutate(name = tools::toTitleCase(tolower(name))) %>% 
                arrange(associationCount) %>% 
                e_charts(name, renderer="svg") %>% 
                e_bar(associationCount, legend = F) %>% 
                e_flip_coords() %>% 
                e_grid(left = 200) %>% 
                e_y_axis(axisLabel = list(fontSize = 9)) %>% 
                e_x_axis(type="log", name = "Associations", nameLocation = 'center', nameGap = 40,
                         nameTextStyle = list(verticalAlign = "bottom")
                ) %>% 
                e_title("Disease Associations", 
                        subtext = "Top diseases associated with this target, compiled by several resources (DisGeNET, eRAM, Monarch, Expression Atlas, ... etc)") %>% 
                e_tooltip() %>% 
                e_toolbox(right = "10%") %>%
                e_toolbox_feature(feature = c("saveAsImage", "dataView")) 
                
                # e_toolbox(
                #   show = T,
                #   feature = list(
                #     saveAsImage = list(
                #       show = T
                #     )
                #     # myInfo = list(
                #     #   show = T,
                #     #   title= 'info',
                #     #   icon= "image://https://img.icons8.com/color/48/000000/info--v1.png",
                #     #   onclick = 
                #     #     JS(paste0("function (){swal({ html:true, title:'<i>IDG</i>', text:'",str_squish(gsub("\n", "",idg_lvl_info2)),"', customClass: 'sweet-alert  alert-size-l showSweetAlert visible'});}"))
                #     # )
                #   )
                # )
              
              
            })
            
            shinyjs::show("disease_plot")
          }
          
          if(nrow(pharos_res$tinx) > 0) {
            pharos_res$tinx %>% 
            filter(!is.na(disease.name)) %>% 
            select(Disease = disease.name, 
                   Importance = score,
                   Novelty = novelty
            ) %>% 
            mutate_if(is.numeric, round, 5) -> pharos_res$tinx
          
            output$tinx_plot <- renderEcharts4r({
            
            pharos_res$tinx %>% 
              #filter(!is.na(disease.name)) %>% 
              group_by(Disease) %>% 
              e_charts(Novelty, renderer="svg") %>% 
              e_scatter(Importance, symbol_size = 5, legend = F, color = "blue") %>% 
              e_x_axis(type = "log", name = "Novelty") %>% 
              e_y_axis(type = "log", name = "Importance", nameLocation = "center",nameGap = 40,) %>% 
              e_title("Target-Disease Importance and Novelty (Tin-x)", 
                      subtext = "Associations between diseases and targets using natural language processing techniques") %>%
              e_tooltip() %>% 
              e_toolbox(right = "10%") %>%
              e_toolbox_feature(feature = c("saveAsImage", "dataView"))
            
            
            })
            
            output$tinx_tbl <- renderReactable({
              
              reactable(pharos_res$tinx, 
                        searchable = TRUE,
                        striped = TRUE,
                        bordered = TRUE,
                        defaultPageSize = 7,
                        columns = list(
                          Importance = colDef(
                            width = 100,
                            style = color_scales(pharos_res$tinx)
                          ),
                          Novelty = colDef(
                            width = 100,
                            style = color_scales(pharos_res$tinx)
                          ),
                          Disease = colDef(
                            minWidth = 250
                          )
                        )
              )
              
            })
            
            shinyjs::show("tinx_plot")
            shinyjs::show("tinx_tbl")
        }
          
          
          
          col_pal <- c("#CD113B", "#FF7600", "#FFA900")
          
            if(nrow(pharos_res$ligands_dx) > 0 | nrow(pharos_res$ligands_sm) > 0) {
              
              if(nrow(pharos_res$ligands_dx) > 0 & nrow(pharos_res$ligands_sm) > 0) {
                pharos_res$ligands <- rbind(pharos_res$ligands_dx, pharos_res$ligands_sm)
              } else if (nrow(pharos_res$ligands_sm) > 0) {
                pharos_res$ligands <- pharos_res$ligands_sm
              } else  {
                pharos_res$ligands <- pharos_res$ligands_dx
              }
              
              pharos_res$ligands <- pharos_res$ligands %>%
                arrange(desc(value)) %>%
                distinct(name, .keep_all = T) %>%
                mutate(isdrug = ifelse(isdrug == T, "Drug", "SM")) %>% 
                head(6) %>% 
                rowwise() %>%
                mutate(strcture = ifelse(!is.na(smiles), paste0("https://pharos-ligand.ncats.io/indexer/render?structure=",
                                         URLencode(smiles, reserved = T), "&size=200"
                ), NA)) %>%
                select(name, strcture,type, value, reference, ligid, isdrug, targetCount)
              
              drug_list <- split(pharos_res$ligands, pharos_res$ligands$name)
              
              # reactable(pharos_res$ligands,
              #           searchable = TRUE,
              #           striped = TRUE,
              #           bordered = TRUE,
              #           
              #           columns = list(
              #             strcture = colDef(cell = embed_img(height = 200,width = 200)),
              #             value = colDef(
              #               cell = color_tiles(pharos_res$ligands, colors = col_pal)
              #             )
              #           )
              # )
              
              output$drugs <- renderUI(
                list(
                  div(h2(class = "module-title font-alt",style = "margin-bottom: 20px;", "Active Ligands")),
                  purrr::map(drug_list, drug_card)
                )
              )
              
              shinyjs::show("drugs")

            }

          # else {
          #   output$drugs <- renderUI(HTML(""))
          # }
            #HTML(paste(purrr::map_chr(drug_list, drug_card), collapse = " "))

          
          # output$drug_tbl <- renderReactable({
          #   if(nrow(pharos_res$ligands_dx) > 0 | nrow(pharos_res$ligands_sm) > 0) {
          # 
          #     if(nrow(pharos_res$ligands_dx) > 0 & nrow(pharos_res$ligands_sm) > 0) {
          #       pharos_res$ligands <- rbind(pharos_res$ligands_dx, pharos_res$ligands_sm)
          #     } else if (nrow(pharos_res$ligands_sm) > 0) {
          #       pharos_res$ligands <- pharos_res$ligands_sm
          #     } else  {
          #       pharos_res$ligands <- pharos_res$ligands_dx
          #     }
          # 
          #     pharos_res$ligands <- pharos_res$ligands %>%
          #       arrange(desc(value)) %>%
          #       distinct(name, .keep_all = T) %>%
          #       rowwise() %>%
          #       mutate(strcture = paste0("https://pharos-ligand.ncats.io/indexer/render?structure=",
          #                                URLencode(smiles, reserved = T), "&size=200"
          #       )) %>%
          #       select(name, strcture,type, value, reference)
          # 
          #     reactable(pharos_res$ligands,
          #         searchable = TRUE,
          #         striped = TRUE,
          #         bordered = TRUE,
          # 
          #         columns = list(
          #           strcture = colDef(cell = embed_img(height = 200,width = 200)),
          #           value = colDef(
          #             cell = color_tiles(pharos_res$ligands, colors = col_pal)
          #           )
          #         )
          #     )
          #   }
          # 
          # 
          # 
          # 
          # })

          #shinyjs::show("gene_card_ui")
          #shinyjs::show("idg_info_btn")
          #shinyjs::show("idg_level")
          shinyjs::show("link_cards")
          shinyjs::show("links")
          shinyjs::show("pub_plot")
          shinyjs::show("path_title")
          shinyjs::show("reactome_tbl")
          shinyjs::show("kegg_tbl")
          shinyjs::show("wiki_tbl")
          shinyjs::show("div_1")
          shinyjs::show("div_2")
          shinyjs::show("div_3")
          shinyjs::show("div_4")


        }
        
        else{
          shinyalert("Opps", "Couldn't Find Your Gene/s", type = "error")
          w$hide()
        }
        shinyFeedback::resetLoadingButton("genes-btn")
      })
    }
  )
}


convert_nulls <- function(x) {
  if(is.null(x)) {
    "NULL"
  } else {x}
}

ks_pharos <- function(gene, conn) {

  query <- '
query targetDetails($gene: String!){
  target(q:{sym: $gene}) {
    name
    tdl
    fam
    sym
    uniprot
    novelty
    affiliate_links {
      sourceName
      description
      url
    }
    dto{
      name
      parent{
        name
      }
    }
    diseases {
      name
      associationCount
      datasource_count
    }
    pubTatorScores{
      year
      score
    }
    pubmedScores {
      year
      score
    }
    pubs {
      pmid
      title
      journal
      date
      abstract
    }
    patentCounts{
      year
      count
    }
    pathways(getTopForEachType:true top:10 type:["Reactome", "KEGG", "WikiPathways"]){
      pwid
      name
      type
      url
    }
    locsigs{
      location
      signal
    }
    affiliate_links{
      sourceName
      description
      url
    }
    tinx (top:300) {
      novelty
      score
      disease {
        name
      }
    }
    ligands_sm: ligands(top:10 isdrug:false) {
      ligid
      smiles
      isdrug
      name
      description
      targetCount
      activities(all:false) {
        type
        value
        moa
        reference
      }
      
    }
    ligands_dx: ligands(top:10 isdrug:true) {
      ligid
      smiles
      isdrug
      name
      description
      targetCount
      activities(all:false) {
        type
        value
        moa
        reference
      }
      
    }
  }
  }'
  
  new <- Query$new()$query('link', query)
  
  res <- conn$exec(new$link, variables = gene) %>%
    fromJSON(flatten = T)
  if(!is.null(res$data$target)) {
    
    procssed_list <- list(
      info = res$data$target[1:6] %>% map(convert_nulls) %>% as.data.frame()
    ) %>% append(res$data$target[7:length(res$data$target)])
    
    if(identical(procssed_list$ligands_sm, list())) {
      procssed_list$ligands_sm <- tibble()
    } else {
      procssed_list$ligands_sm <- map2_df(procssed_list$ligands_sm$activities, procssed_list$ligands_sm$name, add_ligid) %>% 
        left_join(select(procssed_list$ligands_sm, -activities))
    }
    
    if(identical(procssed_list$ligands_dx, list())) {
      procssed_list$ligands_dx <- tibble()
    } else {
      procssed_list$ligands_dx <- map2_df(procssed_list$ligands_dx$activities, procssed_list$ligands_dx$name, add_ligid) %>% 
        left_join(select(procssed_list$ligands_dx, -activities))
    }
    
    
    procssed_list <- map(procssed_list, empty_list_to_tibble)
    
    # if(identical(procssed_list$patentCounts, list())) {
    #   procssed_list$patentCounts <- tibble()
    # }
    # 
    procssed_list

    
  }
  else {
    NULL
  }
  
}

#res2 <- ks_pharos(list(gene = "IGHE"), conn = conn)

empty_list_to_tibble <- function(x) {
  if(identical(x, list())){
    tibble()
  }
  else {
    x
  }
}

add_ligid <- function(x, y) {
  l <- nrow(x)
  x$name <- rep(y, l)
  x
}

#<div class="price-table font-alt">

drug_card <- function(x) {
  
  div(class = "col-sm-6 col-md-4 col-lg-2",
     
    div(class = "shop-item", 
        a(href=paste0("https://pharos.nih.gov/ligands/", x$ligid), target="_blank", 
        div(
          h1(class = "shop-item-title",
             style = "font-size: 1.5rem; font-weight: bold; color: #337ab7; text-decoration: underline;", stringr::str_trunc(x$name, width = 20)),
          hr(class="divider-w mt-10 mb-20")
        ),
        div(class = "shop-item-image",
            img(src = ifelse(is.na(x$strcture), "https://pharos-ligand.ncats.io/indexer/render?structure=c&size=200", x$strcture), alt = x$name)
            ),
        div(style = "display:flex; justify-content:space-between;",
            div(style = "padding: 15px;",
                h4(class="shop-item-title",style = "display:inline;", paste0(x$type, ": ")),
                h4(class="shop-item-title tag status-knockdown",style = "display:inline;", round(x$value, 2))
            ),
            # div(class="btn btn-round btn-xs", style = "padding: 15px;",
            #     a(href=paste0("https://pharos.nih.gov/ligands/", x$ligid), target="_blank",
            #       span(icon("link"))
            #       )
            #     ),
            div(style = "padding: 15px 0px;",
                h4(class="shop-item-title",style = "display:inline;", "Targets: "),
                h4(class="shop-item-title tag status-overexpression",style = "display:inline;", x$targetCount)
            ),
            div(style = "padding: 15px;",
                h4(class="shop-item-title",style = "display:inline;", "Type: "),
                h4(class = ifelse(x$isdrug =="Drug","shop-item-title tag status-reactome", "shop-item-title tag status-reactome"),style = "display:inline;", x$isdrug)
            )
        )
    )
    )
  )

}

gene_card <- function(x, af_div) {
  
  #ns <- NS(id)
  
  #div(class = "col-sm-3 col-md-2 col-lg-2",
      div(class = "shop-item", style= "min-width: 100%",
          div(
            h1(class = "shop-item-title", style = "font-size: 2rem; font-weight: bold;", x$sym),
            hr(class="divider-w mt-10 mb-10")
          ),
          #NGLVieweROutput(outputId  = ns("af_str")),
          renderNGLVieweR(af_div),
          h4(class = "shop-item-title", style = "font-size: 2rem;", x$name),
          a(target="_blank" ,rel="noopener noreferrer",
            style="color: #337ab7; text-decoration: underline;",
            href=paste0("https://www.uniprot.org/uniprot/", x$uniprot), x$uniprot),
          div(style = "display:flex; justify-content:space-between;",
              div(style = "padding: 15px;",
                h4(class="shop-item-title",style = "display:inline;", "Family: "),
                h4(class="shop-item-title tag status-overexpression",style = "padding:5px; display:inline;", x$fam)
                ),
              div(style = "padding: 15px;",
                h4(class="shop-item-title",style = "display:inline;", "Novelty: "),
                h4(class="shop-item-title tag status-reactome",style = "display:inline;", x$novelty)
              )
              )
      )
  #)
  
}



idg_lvl_info2 <- '
  
          <div class="container">
            <div class="row">
              <div class="col-sm-6 col-sm-offset-3">
              <img src="https://commonfund.nih.gov/sites/default/files/IDG_LOGO_UPRIGHT_3.png" width="50%" height="50%">
                <div class="module-subtitle font-serif">The National Institutes of Health (NIH) Common Fund launched the Illuminating the Druggable Genome (IDG) Program in 2014. The overall goal of the IDG Program is to catalyze research in areas of biology that are currently understudied but that have high potential to impact human health.</div>
              </div>
            </div>
            <div class="row multi-columns-row">
              <div class="col-md-3 col-sm-6 col-xs-12">
                <div class="features-item">
                  <img src="assets/images/tdark.webp" width= 200px height= 200px>
                  <p>These are targets about which virtually nothing is known. They do not have known drug or small molecule activities & satisfy two or more of the following criteria:</p>
    <br>
    <ul>
    <li>Pubmed score: (req: < 5)</li>
    <li>Gene RIFs: (req: <= 3)</li>
    <li> Antibodies: (req: <= 50)</li>
    </ul>
                </div>
              </div>
              <div class="col-md-3 col-sm-6 col-xs-12">
                <div class="features-item">
                  <img src="assets/images/tbio.webp" width= 200px height= 200px>
                  <p>These are targets about which virtually nothing is known. They do not have known drug or small molecule activities & satisfy two or more of the following criteria:</p>
    <br>
    <ul>
    <li>Pubmed score: (req: >= 5)</li>
    <li>Gene RIFs: (req: > 3)</li>
    <li> Antibodies: (req: > 50)</li>
    </ul>
                </div>
              </div>
              <div class="col-md-3 col-sm-6 col-xs-12">
                <div class="features-item">
                  <img src="assets/images/tchem.webp" width= 200px height= 200px>
                  
                  <p>Target has at least one ChEMBL compound with an activity cutoff of < 30 nM & satisfies the preceding conditions</p>
                </div>
              </div>
              
              <div class="col-md-3 col-sm-6 col-xs-12">
                <div class="features-item">
                  <img src="assets/images/tclin.webp" width= 200px height= 200px>
                  <p>Target has at least one approved drug & satisfies the preceding conditions</p>
                </div>
              </div>
              
            
            </div>
          </div>

'

link_card2 <- function(x) {
  
  div(class = "col-sm-6 col-md-3 col-lg-3", style = "min-width: 30%",
      a(target="_blank" ,rel="noopener noreferrer",
           href=x$url,
        div(class = "shop-item",
          div(
            h1(class = "shop-item-title", x$sourceName),
            hr(class="divider-w mt-10 mb-10")
          ),
          div(class="shop-item-image", 
              img(style="object-fit:scale-down; padding:10px;", src = case_when(
                x$sourceName == "ARCHS4" ~ "assets/images/archs_logo_sm_3.webp",
                x$sourceName == "GENEVA" ~ "assets/images/geneva_logo_sm_3.webp",
                x$sourceName == "GlyGen" ~ "assets/images/glygen_logo_sm_3.webp",
                T ~ "assets/images/na_drug.webp"
              )))
          )
      )
  )
  
}


