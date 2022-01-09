shinyUI(
  navbarPage(title = "Kaleidoscope",
             collapsible = T,
                   theme = "style.css",
                   id = "tb_sets",
                   #position = "fixed-top",
                   header = "",
                   footer = hr(),
             #inverse = T,
             
                   # Home tab ----
                   tabPanel("Home", icon = icon("home"),
                            tags$head(tags$link(rel="apple-touch-icon",sizes="180x180", href="assets/favicon/apple-touch-icon.png")),
                            tags$head(tags$link(rel="icon",type="image/png", sizes="32x32", href="assets/favicon/favicon-32x32.png")),
                            tags$head(tags$link(rel="icon",type="image/png", sizes="16x16", href="assets/favicon/favicon-16x16.png")),
                            useShinyjs(),
                            useShinyalert(),
                            use_bs_popover(),
                            use_bs_tooltip(),
                            useShinydashboard(),
                            use_waiter(),
                            value = "home",
                            includeHTML("www/index.html")
                   ),

                   # App Tab ----
                   tabPanel("App", icon = icon("th", lib = "glyphicon"),
                            value = "app",
                            navbarPage(title = "",
                                       collapsible = T,
                                       #inverse = T,
                                       #position = "fixed-top",

                                       # BrainSeq ----
                                       tabPanel("BrainRNA-Seq",icon = icon("", class = "icon-bargraph", verify_fa = FALSE),
                                                brainrnaseq_ui("brainrnaseqTab"),
                                                
                                                # css and GA tags ----
                                                fluidRow(tags$style(type = "text/css", ".navbar {margin-bottom: 0;}"),
                                                         tags$style(type = "text/css", ".container-fluid {padding-left:5px;
                                                                    padding-right:5px;}"),
                    #tags$head(tags$style(".shiny-notification {position: fixed; top: 60% ;left: 50%")),
                                                         tags$head(HTML(
                                                           "<!-- Global site tag (gtag.js) - Google Analytics -->
                                                           <script async src='https://www.googletagmanager.com/gtag/js?id=UA-149864972-1'></script>
                                                           <script>
                                                           window.dataLayer = window.dataLayer || [];
                                                           function gtag(){dataLayer.push(arguments);}
                                                           gtag('js', new Date());
                                                           gtag('config', 'UA-149864972-1');
                                                           </script>"
                                                           
                                                         ))
                                                         )
                                                ),
                                       # String ----
                                       tabPanel("STRING", icon = icon("", class = "icon-genius", verify_fa = FALSE),
                                                string_ui("stringTab")
                                                ),
                                       # iLINCS ----
                                       tabPanel("iLINCS", icon = icon("", class = "icon-lifesaver", verify_fa = FALSE),
                                                iincs_ui("lincsTab")
                                                ),
                                       # Lookup ----
                                       tabPanel("Lookup", icon = icon("", class = "icon-search", verify_fa = FALSE),
                                                lookup_ui("lookupTab")
                                                ),
                                       # BrainCloud ----
                                       tabPanel("BrainCloud", icon = icon("", class = "icon-linegraph", verify_fa = FALSE),
                                                braincloud_ui("braincloudTab")
                                                ),
                                       # GTEx ----
                                       tabPanel("GTEx", icon = icon("", class = "icon-profile-male", verify_fa = FALSE),
                                                gtex_ui("gtexTab")
                                                ),
                                       # BrainAtlas ----
                                       tabPanel("BrainAtlas", icon = icon("", class = "icon-aperture", verify_fa = FALSE),
                                                brainatlas_ui("brainatlasTab")
                                                ),
                                       # GWAS ----
                                       tabPanel("GWAS", icon = icon("", class = "icon-map", verify_fa = FALSE),
                                                gwascatalog_UI("gwascatalogTab")
                                                ),
                                       tabPanel("IDG", icon = icon("", class = "icon-lightbulb", verify_fa = FALSE),
                                                pharos_ui("pharosTab")
                                       ),
                    
                                       tabPanel("Report", icon = icon("", class = "icon-browser", verify_fa = FALSE),
                                                report_ui("reportTab")
                                       )

                                       
                                       )
                            )
             )
  )
