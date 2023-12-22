library(shiny)
library(bslib)
library(htmltools)
library(ggplot2)
library(dplyr)
library(DT)
library(shinyjqui)
library(FateMapper)
library(shinyWidgets)
library(plotly)
options(shiny.maxRequestSize=1024*1024^2)


coretable <- readxl::read_xlsx('scLTdb summary.xlsx')


ui <-
    navbarPage(
    includeCSS("www/style.css"),
    title = NULL,
    bg = "#325880",
    id = "inTabset",
    theme = bslib::bs_theme(),
    ###Home-----
    tabPanel(title = "Home",
              icon = icon('home',lib="glyphicon"),
              div(class = 'home',
              fluidPage(
                  card(
                      card_header("Introduction",class = "Introduction"),
                      status = "primary",
                      width = 12,
                      height = NULL,
                      card_body(
                          uiOutput("HOME_output_text")
                      )

                  ),

                  layout_column_wrap(
                      width = 1/3,
                      height = NULL,
                      card(
                          class = 'species',
                          card_header("Species"),
                          height = NULL,
                          style = "font-size: 20px;margin: 0 auto;text-align:center;",
                          card_image(
                              file = "www/pie_species.png"
                          )

                      ),
                      card(
                          class = 'tissue',
                          card_header("Tissue"),
                          height = NULL,
                          style = "font-size: 20px;margin: 0 auto;text-align:center;background:#FFFFFF;",
                          card_image(
                              file = "www/pie_tissue.png"
                          )

                      ),
                      card(
                          card_header("Technology"),
                          height = NULL,
                          card_image(
                              file = "www/pie_tech.png"
                          )
                      )
                  )
                  ,
                  tags$br(),
                  div(style = 'display: flex; justify-content: space-around;',
                      card(
                        card_image(file = "www/intro_img.png"),
                        style = 'background:#FFFFFF; flex: 2; margin: 10px;'
                      ),
                      card(
                        card_image(file = "www/basic_statistic.png"),
                        style = 'background:#FFFFFF; flex: 1; margin: 10px;'
                      )
                  ),
                  style = "font-size:150%;width:80%;"
              )
              )
        ),
     ###Search-----
    tabPanel(title = "Search",
               icon = icon('search',lib = 'glyphicon'),
               fluidPage(
                   div(
                   card(
                       card_header("Dataset overview"),
                       div(DT::dataTableOutput('coretable',width = "100%"))
                   ),
                   card(
                       actionButton("go_to_panel", "Expore dataset....",class = 'jump')
                   )

                    ),style = "font-size:120%;width:90%;")
               ),


    ###Results-----
    tabPanel(title = "Results",
             value =  "Results",
              icon = icon('chart-line',lib="font-awesome"),
              fluidPage(
                  layout_column_wrap(
                      width = 1/2,
                      height = NULL,
                      navset_card_tab(
                          title = textInput('input_barcode',NULL,value = 'Please input barcode...'),
                          id = "umap", height = "800px",
                          full_screen = TRUE,
                          #static umap
                          # nav_panel("raw_uamp",
                          #          div(imageOutput("show_umap1"),style = "margin-left: auto; margin-right: auto;background:#FFFFFF;")
                          # ),
                          #dynamic umap
                          div(uiOutput("show_umap"),style = "margin-left: auto; margin-right: auto;")
                      ),
                      navset_card_tab(
                          title = "1231231",
                          id = "tabset1", height = "800px",
                          full_screen = TRUE,
                          nav_panel("Barcode_count",
                                   div(imageOutput("barcode_count_plot"),style = "margin-left: auto; margin-right: auto;")
                          ),
                          nav_panel("cell_type_similarity",
                                   div(imageOutput("cell_type_similarity_plot"),style = "margin-left: auto; margin-right: auto;")
                          ),
                          nav_panel('fate_bias',
                                   div(class = 'fate_bias_table',style = "font-size:150%",
                                       dataTableOutput('look_fate_bias',width = "100%",)
                                   )
                          ),
                          nav_panel("lineage tree",
                                   div(imageOutput("lineage_tree_plot"),style = "margin-left: auto; margin-right: auto;")
                          )

                      )
                  ),
                  style = "font-size:100%;width:80%;"
              )

              ),

    ###Online tools-----
    navbarMenu(title = "Online tools",
              icon = icon('cloud'),
              tabPanel("Tools 1",


                    layout_column_wrap(
                       width = 1/2,
                       card(
                            card_header("header"),
                            "This is it."
                            ),
                       navset_card_tab(
                         title =   fluidRow(
                           class = "vertical-center-row",  # 添加自定义的类名
                           column(5,
                                  tags$label(
                                    "Please choose 2 datasets:",
                                    style = 'margin-left: 20px;margin-top: 2px;font-size: 22px;'
                                  )

                           ),
                           column(7,
                                  div(class = "double_choose",
                                      selectizeInput(
                                        "Compare_dataset",
                                        label = NULL,
                                        selected = c('Biddy_2018_Nature','Hurley_2020_CSC'),
                                        choices = coretable$Dataset,
                                        multiple = TRUE,
                                        width = 500,
                                        options = list(maxItems = 2)
                                      )
                                  )

                           ),
                           card(
                             plotOutput("selected_option")
                           ),
                           full_screen = TRUE
                         )

                       )
                       )
                    ),

              tabPanel("Tools 2",
                       layout_column_wrap(
                         width = 1/2,
                         card(
                           card_header("header"),
                           "This is it."
                         ),
                       navset_card_tab(
                         title = div(class = 'upload_meta',
                                     fileInput("upload_metadata", NULL,buttonLabel = "Upload...", width = 250,multiple = FALSE,accept = c(".csv"),
                                     )),
                         id = "tools2", height = "800px",
                         full_screen = TRUE,
                         nav_panel("clone profile",
                                   div(
                                     plotlyOutput('cloneprofile_tools')
                                   )
                         ),
                         nav_panel("cell type similarity",
                                   div(
                                     plotlyOutput('cell_type_fate_similartiy_tools')
                                   )

                         ),
                         nav_panel("Clone fate bias"

                         ),
                         nav_panel("Clone embedding")
                       )

                       )
                    )
              ),


    tabPanel(title = "Tutorials",
              icon = icon('bookmark',lib = 'glyphicon'),
              p("Second tab content.")
             ),

    tabPanel(title = "Download",
              icon = icon('download',lib = 'glyphicon'),
              p("Third tab content")),
    tabPanel(title = "Contact",
              icon =  icon('envelope',lib = 'glyphicon'),
             fluidPage(
               card(
                 card_header("Contact us",class = "Introduction"),
                 status = "primary",
                 width = 12,
                 height = NULL,
                 card_body(
                   uiOutput("Contact_text")
                 )

               ))
             ),
    nav_spacer(),
    nav_item(tags$a(shiny::icon("github"), "FateMapper", href = "https://jiang-junyao.github.io/FateMapper/", target = "_blank"))
)
