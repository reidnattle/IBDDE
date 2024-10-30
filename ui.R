
source("global.R")
thematic_on(font = "auto")

link_git <- tags$a(shiny::icon("github"), 
                   href = "https://github.com/reidnattle/IBDDE", 
                   target = "_blank")


# Define UI 
ui <- page_navbar(

  title = "{ IBDDE }",
  
  theme = bs_theme(version = 5, preset = "cerulean") %>% 
    bs_add_variables("bslib-value-box-horizontal-break-point" = "1px"),

  nav_panel("Songs and Standings",
            
            
            page_fluid(
              
              tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),

              layout_sidebar(
                sidebar = sidebar(
                  width = 230,
                  fillable = FALSE,
                  id = "GLOBAL_SIDEBAR",
                  open = "always",
                  img(src = "30.png", width = "100%"),
                  

                  pickerInput("ROUND_SELECT",
                              "Pick a Round",
                              choices = ROUND_SELECT_CHOICES,
                              selected = "All Rounds",
                              options = pickerOptions(actionsBox = TRUE,
                                                      dropupAuto = FALSE,
                                                      style = "btn-outline-primary")
                              ),
                  pickerInput("PICKER_SELECT",
                              "Pick a Picker",
                              choices = PICKER_SELECT_CHOICES,
                              selected = "All Pickers",
                              options = pickerOptions(actionsBox = TRUE,
                                                      dropupAuto = FALSE,
                                                      style = "btn-outline-primary")
                  ),
                  
                  card(card_body(plotOutput("VOTES_DIST_PLOT", height = 240),
                                 #class = "p-0"
                                 ),
                       full_screen = TRUE
                  ),
                  
                  actionBttn("P1_INFO",
                             "click for info",
                             style = "simple",
                             color = "royal",
                             size = "sm",
                             icon = icon("robot")
                  ) %>% 
                    popover(
                      "Filter the plots and table by selecting categories in the menus above. The plots may also be filtered by clicking on a row in the table.",
                      title = "about interactivity",
                    ),
                  
                  ),
                
                layout_column_wrap(
                  value_box(
                    title = "runtime",
                    value = textOutput("RUNTIME"),
                    showcase = bsicons::bs_icon("clock-fill",
                                                size = "0.70em"
                                                ),

                    fill = FALSE,
                    height = "90px",
                    theme = "bg-info"
                  ),
                  
                  value_box(
                    title = "average runtime",
                    value = textOutput("AVG_RUNTIME"),
                    showcase = bsicons::bs_icon("clock",
                                                size = "0.70em"),
                    fill = FALSE,
                    height = "90px",
                    theme = "teal",

                  ),
                  
                  value_box(
                    title = "explicit songs",
                    value = textOutput("PCT_EXPLICIT"),
                    showcase = bsicons::bs_icon("emoji-grimace-fill",
                                                size = "0.70em"
                    ),
                    fill = FALSE,
                    height = "90px",
                    theme = "green",

                  )
                ),
                
                layout_column_wrap(
                  width = NULL,
                  style = css(grid_template_columns = "3fr 2fr"),
                  
                  card(
                    sidebar = "GLOBAL_SIDEBAR",
                    full_screen = TRUE,
                    
                      card_body(
                        min_height = 500,
                        DTOutput(
                          "SONGS_DT", height = 500), 
                        height = "100%")
                  ),
                  
                  card(
                    full_screen = TRUE,
                    card_header(
                      radioGroupButtons(
                        inputId = "VOTE_PLOT_OPT",
                        choices = c("Standings", "Allocations"),
                        individual = TRUE
                      )
                    ),
                    
                    card_body(
                      #class = "p-0",
                      
                      conditionalPanel(
                        condition = "input.VOTE_PLOT_OPT == 'Standings'",
                        plotOutput("STANDINGS_PLOT")
                      ),
                      conditionalPanel(
                        condition = "input.VOTE_PLOT_OPT == 'Allocations'",
                        plotOutput("VOTE_SUM_PLOT"),
                      )
                      # layout_column_wrap(
                      # width = "200px",
                      # #title = "Votes",

                      # 
                      # navset_card_tab(
                      #   title = "Votes Summary",
                      #   nav_panel("Standings", 
                      #             plotOutput("STANDINGS_PLOT")
                      #   ),
                      #   nav_panel("Allocations",
                      #             plotOutput("VOTE_SUM_PLOT"))
                      # )
                    )
                    
                  )
                )
              )
            )
  ),
  ###################################################################################################
  ########################################   PAGE 2 UI   ############################################ 
  ###################################################################################################
  nav_menu("Song Data",
    nav_panel("Continuous variables",
            
            page_fluid(
              tags$style(HTML(".dataTables_wrapper .dataTables_filter input{
                      width: 50px;}"
              )),
              tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),

              layout_sidebar(
                sidebar = sidebar(
                  width = 230,
                  fillable = FALSE,
                  id = "GLOBAL_SIDEBAR",
                  open = "always",
                  img(src = "30.png", width = "100%"),
                  
                  #hr(style = "margin-bottom: 0"),
                  
                  awesomeRadio("GROUP_SELECT",
                              "Group plots by:",
                              choices = c("Round", "Picker")
                  ),
                  pickerInput("PARAM_SELECT",
                              label = popover(
                                id = "PARAM_POP",
                                trigger = list(
                                  "Pick a Variable",
                                  bsicons::bs_icon("info-circle")
                                               ),
                                "Tooltip message"
                                ),
                              choices = VAR_SELECT_CHOICES,
                              choicesOpt = list(
                                style = rep_len("style = font-weight: bold;", 10)),
                 
                              options = pickerOptions(dropupAuto = FALSE,
                                                      style = "btn-outline-primary")
                              ),
                  
               card(
                    card_body(
                      plotOutput("PARAM_DIST_PLOT", height = 160),
                      class = "p-0"
                      ),
                    full_screen = TRUE
                  ),
                  
                  sliderInput("HISTO_SLIDE",
                              "Number of bins:",
                              min = 5,
                              max = 30,
                              value = 15
                              ),
               
               actionBttn("P1_INFO",
                          "  click for info",
                          style = "simple",
                          color = "royal",
                          size = "sm",
                          icon = icon("robot")
               ) %>% 
                 popover(
                   "Click and drag over points in the box plots to filter the data table to the right.",
                   title = "about interactivity",
                 ),
               
                ),
                
                layout_column_wrap(
                  
                  
                  value_box(
                            title = textOutput("MIN_TITLE"),
                            value = textOutput("MIN_VALUE"),
                            class = "p-0",
                            showcase = bsicons::bs_icon("arrow-down",
                                                        size = "0.70em"),
                            fill = FALSE,
                            height = "90px",
                            theme = "purple",
                  
                  ),
                  
                  value_box(
                    title = textOutput("MAX_TITLE"),
                     value = textOutput("MAX_VALUE"),
                    class = "p-0",
                    showcase = bsicons::bs_icon("arrow-up",
                                                size = "0.70em"
                    ),

                    fill = FALSE,
                    height = "90px",
                    theme = "indigo"
                  ),
                  
                  value_box(
                     title = textOutput("MEDIAN_TITLE"),
                     value = textOutput("MEDIAN_VALUE"),
                     class = "p-0",
                    showcase = bsicons::bs_icon("arrows-collapse",
                                                size = "0.70em"
                    ),
                    fill = FALSE,
                    height = "90px",
                    theme = "teal",
                      showcase_layout = showcase_left_center(
                        width = 0.25,
                      )
                  )
                ),
                
                layout_column_wrap(
                  width = NULL,
                  style = css(grid_template_columns = "3fr 2fr"),
                  
                  card(height = "550px",
                    #sidebar = "GLOBAL_SIDEBAR",
                    full_screen = TRUE,
                    
                    card_header(
                      radioGroupButtons(
                        inputId = "BOX_JOY_OPT",
                        choices = c("Box Plot", "Joy Plot"),
                        individual = TRUE
                      )
                    ),
                    
                    card_body(
                      height = "100%",
                      conditionalPanel(
                        condition = "input.BOX_JOY_OPT == 'Box Plot'",
                        plotOutput("BOXPLOTS", brush = "PLOT_BRUSH", height = "100%")
                      ),
                      conditionalPanel(
                        condition = "input.BOX_JOY_OPT == 'Joy Plot'",
                        plotOutput("DENSE_HISTO_PLOT", height = "100%")
                      )
                      #class = "p-0",
                      # layout_column_wrap(
                      # width = "200px",
                      # navset_card_tab(
                      #   nav_panel("Box Plots", 
                      #               plotOutput("BOXPLOTS", brush = "PLOT_BRUSH", height = "100%"),
                      #    ),
                      #   nav_panel("Joy Plots",
                      #             plotOutput("DENSE_HISTO_PLOT", height = "100%"))
                      # )
                    )
                    ),
                    
                  
                  card(height = "550px",
                    full_screen = TRUE,
                    #div(
                      card_body(
                        height = "100%",
                        #class = "p-0",
                        min_height = 500,
                        DTOutput(
                          "SONGS_DT2",
                          #width = "96%",
                          height = "100%")),
                      )
                    )
                  )
                )
              
            ),
  
  ###################################################################################################
  ########################################   PAGE 3 UI   ############################################ 
  ###################################################################################################
  nav_panel("Categorical variables",
            
            page_fluid(
              tags$style(HTML(".dataTables_wrapper .dataTables_filter input{
                      width: 50px;}"
              )),
              tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
              
              layout_sidebar(
                sidebar = sidebar(
                  width = 230,
                  fillable = FALSE,
                  id = "GLOBAL_SIDEBAR",
                  open = "always",
                  img(src = "30.png", width = "100%"),
                  
                  #hr(style = "margin-bottom: 0"),
                  
                  awesomeRadio("GROUP_SELECT_CAT",
                               "Group plots by:",
                               choices = c("Round", "Picker")
                  ),
                  pickerInput("PARAM_SELECT_CAT",
                              label = popover(
                                id = "PARAM_POP_CAT",
                                trigger = list(
                                  "Pick a Variable",
                                  bsicons::bs_icon("info-circle")
                                ),
                                "Tooltip message"
                              ),
                              choices = VAR_SELECT_CHOICES_CAT,
                              selected = "key",
                              choicesOpt = list(
                                style = rep_len("style = font-weight: bold;", 10)),
                              
                              options = pickerOptions(dropupAuto = FALSE,
                                                      style = "btn-outline-primary")
                  ),
                  
                  card(
                    card_body(
                      plotOutput("PARAM_FREQ_PLOT", height = 200),
                      #class = "p-0"
                    ),
                    full_screen = TRUE
                  ),
                  
                  actionBttn("P1_INFO",
                             "  click for info",
                             style = "simple",
                             color = "royal",
                             size = "sm",
                             icon = icon("robot")
                  ) %>% 
                    popover(
                      "Filter the data table to the right by searching or selecting variables in the containers beneath the column headings.",
                      title = "about interactivity",
                    ),
                  
                ),
                
                layout_column_wrap(
                  
                  
                  value_box(
                    title = textOutput("COMMON_TITLE"),
                    value = textOutput("COMMON_VALUE"),
                    class = "p-0",
                    showcase = bsicons::bs_icon("arrow-up",
                                                size = "0.70em"),
                    fill = FALSE,
                    height = "90px",
                    theme = "green",
                    
                  ),
                  
                  value_box(
                    title = textOutput("RARE_TITLE"),
                    value = textOutput("RARE_VALUE"),
                    class = "p-0",
                    showcase = bsicons::bs_icon("arrow-down",
                                                size = "0.70em"
                    ),
                    
                    fill = FALSE,
                    height = "90px",
                    theme = "blue"
                  )
                ),
                
                layout_column_wrap(
                  width = NULL,
                  style = css(grid_template_columns = "3fr 2fr"),
                  
                  card(
                    sidebar = "GLOBAL_SIDEBAR",
                    full_screen = TRUE,
                    
                    card_header(
                      radioGroupButtons(
                        inputId = "PLOT_TAB_OPT",
                        choices = c("Stacked Bar Plot", "Frequency Table"),
                        individual = TRUE
                      )
                    ),
                    
                    card_body(
                      #class = "p-0",
                      conditionalPanel(
                        condition = "input.PLOT_TAB_OPT == 'Stacked Bar Plot'",
                        plotOutput("CATBAR_PLOT", height = "100%")
                      ),
                      conditionalPanel(
                        condition = "input.PLOT_TAB_OPT == 'Frequency Table' && input.GROUP_SELECT_CAT == 'Round'",
                        div(DTOutput("CAT_FREQ_TAB_ROUND", height = "100%"), style = "font-size:80%"),
                      ),
                      conditionalPanel(
                        condition = "input.PLOT_TAB_OPT == 'Frequency Table' && input.GROUP_SELECT_CAT == 'Picker'",
                        div(DTOutput("CAT_FREQ_TAB_PICKER", height = "100%"), style = "font-size:80%"),
                      )
                      
                      )
                    
                  ),
                  
                  card(
                    full_screen = TRUE,
                    #div(
                    card_body(
                      #class = "p-0",
                      min_height = 500,
                      DTOutput(
                        "SONGS_DT3",
                        width = "96%",
                        height = "100%"), 
                      height = "100%"),
                  )
                )
              )
            )
            
  )),
  
  ###################################################################################################
  ########################################   PAGE 4 UI   ############################################ 
  ###################################################################################################
  
  nav_panel("Votes",
            
            h3("tbd")
            
            ),
  
  nav_spacer(),
  nav_item(h4("Itty Bitty Ditties [data explorer]")),
  nav_item(link_git),
  nav_item(input_dark_mode(id = "DARK_MODE", mode = "dark")), 
)