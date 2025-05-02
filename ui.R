
source("global.R")
#thematic_on(font = "auto")

link_git <- tags$a(shiny::icon("github"), 
                   href = "https://github.com/reidnattle/IBDDE", 
                   target = "_blank")


# Define UI 
ui <- page_navbar(
  
  title = "{ IBDDE }",
  
  theme = bs_add_variables(
    bs_theme(
      #base_font = bslib::font_google("News Cycle"),
      #heading_font = bslib::font_google("News Cycle"),
      preset = "zephyr",
      info = "#008cba"
      #008cba
      #info = "#5A4FCF"
    ),
    "bslib-value-box-horizontal-break-point" = "1px"
    #"navbar-light-brand-color" = "darkgreen"
  ),
  bg = "#008cba",
  inverse = TRUE,
  nav_panel("Overview",
            
            
            page_fluid(
              
              tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
              
              layout_sidebar(
                sidebar = sidebar(
                  img(src = "30.png", width = "100%"),
                  width = 230,
                  fillable = FALSE,
                  id = "GLOBAL_SIDEBAR",
                  open = "always",
                  
                  card( 
                    #class = c("text-black"),
                    
                    card_header(
                      "Filter Tables & Plots",
                                class = c("bg-teal", "text-black", "font-weight-bold"),
                    ),
                    
                    card_body(
                      
                      pickerInput("ROUND_SELECT",
                                  "Pick a Round",
                                  choices = ROUND_SELECT_CHOICES,
                                  selected = "All Rounds",
                                  options = list(actionsBox = TRUE,
                                                 dropupAuto = FALSE,
                                                 style = c("btn-info"),
                                                 container = 'body'
                                                 
                                                 #style = "btn-sm"
                                  )
                      ),
                      pickerInput("PICKER_SELECT",
                                  "Pick a Picker",
                                  choices = PICKER_SELECT_CHOICES,
                                  selected = "All Pickers",
                                  options = list(actionsBox = TRUE,
                                                 dropupAuto = FALSE,
                                                 style = "btn-info",
                                                 container = 'body'
                                                 
                                                 #style = "btn-sm"
                                  )
                      ))),
                  
                  #hr(),
                  
                  card(card_body(plotOutput("VOTES_DIST_PLOT", height = 240),
                                 class = "p-0"
                  ),
                  full_screen = TRUE
                  )
                  
                ),
                
                layout_column_wrap(
                  value_box(
                    title = "total runtime",
                    value = textOutput("RUNTIME"),
                    showcase = bsicons::bs_icon("hourglass-bottom",
                                                size = "0.70em"
                    ),
                    
                    fill = FALSE,
                    height = "90px",
                    theme = "cyan"#,
                    #class = "text-dark"
                  ),
                  
                  value_box(
                    title = "average runtime",
                    value = textOutput("AVG_RUNTIME"),
                    #class = c("text-dark", "p-0"),
                    showcase = bsicons::bs_icon("stopwatch",
                                                size = "0.70em"),
                    fill = FALSE,
                    height = "90px",
                    theme = "teal",
                    
                  ),
                  
                  value_box(
                    title = "explicit songs",
                    value = textOutput("PCT_EXPLICIT"),
                    showcase = bsicons::bs_icon("explicit",
                                                size = "0.70em"
                    ),
                    fill = FALSE,
                    height = "90px",
                    theme = "purple",
                    
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
                        individual = TRUE,
                        size = "sm",
                        status = "btn-outline"
                      )
                    ),
                    
                    card_body(
                      #class = "p-0",
                      
                      conditionalPanel(
                        condition = "input.VOTE_PLOT_OPT == 'Standings'",
                        plotOutput("STANDINGS_PLOT", height = 450), 
                        height = "100%"
                      ),
                      conditionalPanel(
                        condition = "input.VOTE_PLOT_OPT == 'Allocations'",
                        plotOutput("VOTE_SUM_PLOT", height = 450), 
                        height = "100%"
                      )
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
                           card( 
                             # class = "bg-info",
                             card_header(
                               class = c("bg-teal", "text-black", "font-weight-bold"),
                               
                               "Select & Group Data"
                             ),
                             
                             card_body(awesomeRadio("GROUP_SELECT",
                                                    "Group plot by:",
                                                    choices = c("Round", "Picker"),
                                                    status = "primary"
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
                                         
                                         options = list(dropupAuto = FALSE,
                                                        style = "btn-info",
                                                        container = 'body'
                                         )
                             ))),
                           
                           card(
                             card_header(
                              
                                 h6("Full playlist summary"),
                                 hr(),
                              
                               radioGroupButtons(
                                 inputId = "HISTO_DENSE_OPT",
                                 choices = c("Histogram", "Density"),
                                 individual = TRUE,
                                 size = "sm",
                                 status = "btn-outline"
                               )
                               
                             ),
                             card_body(
                               plotOutput("PARAM_DIST_PLOT", height = 160),
                               class = "p-0"
                             ),
                             full_screen = TRUE,
                             
                             conditionalPanel(
                               condition = "input.HISTO_DENSE_OPT == 'Density'",
                               hr(),
                               br()
                             ),
                             
                             conditionalPanel(
                               
                               condition = "input.HISTO_DENSE_OPT == 'Histogram'",
                               
                               sliderInput("HISTO_SLIDE",
                                           "Number of bins:",
                                           min = 5,
                                           max = 30,
                                           value = 15
                               ))
                             
                           )
                           
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
                             #class = c("text-dark", "p-0"),
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
                           style = css(grid_template_columns = "3.3fr 1.7fr"),
                           
                           card(min_height = "630px",
                                #sidebar = "GLOBAL_SIDEBAR",
                                full_screen = TRUE,
                                
                                card_header(
                                  radioGroupButtons(
                                    inputId = "BOX_JOY_OPT",
                                    choices = c("Box Plot", "Joy Plot"),
                                    individual = TRUE,
                                    size = "sm",
                                    status = "btn-outline"
                                  ),
                                  
                                  popover(
                                    bsicons::bs_icon("robot", class = "ms-auto"), 
                                    "Click and drag over points in the box plots to filter the data table to the right.",
                                    title = "about interactivity"),
                                  class = "d-flex align-items-center gap-1"
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
                           card(
                             #class = "bg-info",
                             card_header("Select & Group Data",
                                         class = c("bg-teal", "text-black", "font-weight-bold"),
                             ),
                             
                             awesomeRadio("GROUP_SELECT_CAT",
                                          "Group plot by:",
                                          choices = c("Round", "Picker"),
                                          status = "primary"
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
                                         
                                         options = list(dropupAuto = FALSE,
                                                        style = "btn-info",
                                                        container = 'body'
                                                        
                                                        #         style = "btn-sm"
                                         )
                             )),
                           
                           
                           card(
                             
                             card_header(
                               h6("Full playlist summary"),
                               hr(),
                               radioGroupButtons(
                                 inputId = "TREE_BAR_OPT",
                                 choices = c("Bar Plot", "Treemap", "Frequency Table"),
                                 individual = TRUE,
                                 size = "sm",
                                 status = "btn-outline"
                               )
                             ),
                             
                             card_body(
                               class = "p-0",
                               height = 250,
                               conditionalPanel(
                                 condition = "input.TREE_BAR_OPT == 'Bar Plot'",
                                 plotOutput("CAT_FULL_BAR", height = "100%")
                               ),
                               conditionalPanel(
                                 condition = "input.TREE_BAR_OPT == 'Treemap'",
                                 plotOutput("TREEMAP_PLOT", height = "100%")
                               ),
                               conditionalPanel(
                                 condition = "input.TREE_BAR_OPT == 'Frequency Table'",
                                 DTOutput("FREQ_TAB_FULL", height = "100%")
                               )  
                             ),
                             full_screen = TRUE
                           ),
                           
                           # card(
                           #   card_body(
                           #     plotOutput("TREEMAP_PLOT", height = 200),
                           #     class = "p-0"
                           #   ),
                           #   full_screen = TRUE
                           # ),
                           
                         ),
                         
                         layout_column_wrap(
                           
                           
                           value_box(
                             title = textOutput("COMMON_TITLE"),
                             value = textOutput("COMMON_VALUE"),
                             #class = c("text-dark", "p-0"),
                             showcase = bsicons::bs_icon("arrow-up",
                                                         size = "0.70em"),
                             fill = FALSE,
                             height = "90px",
                             theme = "teal",
                             
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
                             theme = "cyan"
                           )
                         ),
                         
                         layout_column_wrap(
                           width = NULL,
                           style = css(grid_template_columns = "3.3fr 1.7fr"),
                           
                           card(
                             min_height = "630px",
                             #height = "550px",
                             sidebar = "GLOBAL_SIDEBAR",
                             full_screen = TRUE,
                             
                             card_header(
                               radioGroupButtons(
                                 inputId = "PLOT_TAB_OPT",
                                 choices = c("Stacked Bar Plot", "Frequency Table"),
                                 individual = TRUE,
                                 size = "sm",
                                 status = "btn-outline"
                               ),
                               
                               popover(
                                 bsicons::bs_icon("robot", class = "ms-auto"), 
                                 "Filter the data table to the right by searching or selecting variables in the containers beneath the column headings.",
                                 title = "about interactivity"),
                               
                               conditionalPanel(
                                 condition = "input.PLOT_TAB_OPT == 'Stacked Bar Plot'",
                                 popover(
                                   bsicons::bs_icon("gear"), 
                                   awesomeRadio(
                                     inputId = "PROP_COUNT_OPT",
                                     label = "Configure Y to display:",
                                     choices = c("proportion of songs", "count of songs")
                                   ),
                                   title = "plot controls")),
                               
                               class = "d-flex align-items-center gap-1"
                               
                             ),
                             
                             card_body(
                               #class = "p-0",
                               conditionalPanel(
                                 condition = "input.PLOT_TAB_OPT == 'Stacked Bar Plot'",
                                 plotOutput("CATBAR_PLOT", height = "100%")
                               ),
                               conditionalPanel(
                                 condition = "input.PLOT_TAB_OPT == 'Frequency Table' && input.GROUP_SELECT_CAT == 'Round'",
                                 div(DTOutput("CAT_FREQ_TAB_ROUND", height = "100%"), 
                                     #style = "font-size:90%"
                                 ),
                               ),
                               conditionalPanel(
                                 condition = "input.PLOT_TAB_OPT == 'Frequency Table' && input.GROUP_SELECT_CAT == 'Picker'",
                                 div(DTOutput("CAT_FREQ_TAB_PICKER", height = "100%"), 
                                     #style = "font-size:90%"
                                 ),
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
  nav_menu("Standings and Votes",
  nav_panel("Standings",
            page_fluid(
              
              layout_sidebar(
                sidebar = sidebar(
                  #img(src = "30.png", width = "100%"),
                  width = 300,
                  fillable = FALSE,
                  id = "GLOBAL_SIDEBAR",
                  open = "always",
                  
                  card( 
                    #class = c("text-black"),
                    
                    card_header(
                      "Filter Outputs",
                      class = c("bg-teal", "text-black", "font-weight-bold"),
                    ),
                    
                    card_body(
                      
                      pickerInput("PICKER_SELECT_2",
                                  label = "Pick Pickers",
                                  choices = PICKER_SELECT_CHOICES_2,
                                  selected = PICKER_SELECT_CHOICES_2,
                                  multiple = TRUE,
                                  #keepAlwaysOpen = TRUE,
                                  #optionsCount = 12,
                                  #optionHeight = "30px",
                                  #showValueAsTags = TRUE
                                  #bigger = TRUE
                                  #outline = TRUE,
                                  #icon = icon("check"),
                                  #inline = TRUE
                                  options = pickerOptions(actionsBox = TRUE,
                                                          #dropupAuto = FALSE,
                                                          style = c("btn-info"),
                                                          container = 'body',
                                                          selectedTextFormat = 'count > 4'
                                  )
                      ),
                      
                      # numericRangeInput(
                      #   "STANDINGS_RANGE",
                      #   "Zoom to Rounds",
                      #   min = 1,
                      #   max = 13,
                      #   value = c(1:13)
                      # )
                    )
                  ),
                  
                  card(
                    full_screen = TRUE,
                    card_body(DTOutput("STANDINGS_TAB", height = "100%"))
                  )
                  
                ),
                
                card(
                  full_screen = TRUE,
                  
                  card_header(
                    radioGroupButtons(
                      inputId = "STAND_PLOT_OPT",
                      choices = c("Standings", "Cummulative Scores"),
                      individual = TRUE,
                      size = "sm",
                      status = "btn-outline"
                    ),
                    
                    conditionalPanel(
                      condition = "input.STAND_PLOT_OPT == 'Standings'",
                      popover(
                      bsicons::bs_icon("robot", class = "ms-auto"), 
                      "Click on points in the plot to filter the data table in the sidebar.",
                      title = "about interactivity")
                     ),
                    
                    conditionalPanel(
                      condition = "input.STAND_PLOT_OPT == 'Cummulative Scores'",
                      popover(
                        bsicons::bs_icon("robot", class = "ms-auto"), 
                        "Click and drag over points in the plot to filter the data table in the sidebar.",
                        title = "about interactivity")
                    ),
                    
                    conditionalPanel(
                      condition = "input.STAND_PLOT_OPT == 'Cummulative Scores'",
                      popover(
                        bsicons::bs_icon("gear"), 
                        awesomeRadio(
                          inputId = "SCORE_Y_OPT",
                          label = "Configure Y to display:",
                          selected = "distance from the mean",
                          choices = c("cumulative scores", "distance from the mean")
                        ),
                        title = "plot controls")),
                    
                    class = "d-flex align-items-center gap-1"
                    
                  ),
                  
                  card_body(
                    #height = 625,
                    conditionalPanel(
                      
                      condition = "input.STAND_PLOT_OPT == 'Standings'",
                      
                      plotOutput(
                        "SONGS_BUMP_PLOT", 
                        height = 550, 
                        click = "SANDINGS_PLOT_CLICK",
                        # dblclick = "BUMP_DCLICK",
                        # brush = brushOpts("BUMP_BRUSH",
                        #                   resetOnNew = TRUE)
                      )
                    ),
                    
                    conditionalPanel(
                      
                      condition = "input.STAND_PLOT_OPT == 'Cummulative Scores'",
                      
                      plotOutput("SONGS_CUM_PLOT", 
                                 height = 550, 
                                 brush = "SANDINGS_PLOT_BRUSH")
                    )
                  )
                  
                )
                
              )
            )
            

  ),
  
  ###################################################################################################
  ########################################   PAGE 5 UI   ############################################ 
  ###################################################################################################
  
  nav_panel(
    "Votes",
    page_fluid(
        
      "BRB"
      
        #   layout_sidebar(
        #     
        #     sidebar = sidebar(
        #       
        #       pickerInput("PICKER_SELECT_VOTE",
        #                   "Pick a Player",
        #                   choices = PICKER_SELECT_CHOICES,
        #                   selected = "All Pickers",
        #                   options = list(actionsBox = TRUE,
        #                                  dropupAuto = FALSE,
        #                                  style = "btn-info",
        #                                  container = 'body'
        #                                  
        #                                  #style = "btn-sm"
        #                   )
        #       )
        #     ),
        #     card(
        #     
        #   # card_header(
        #   #           
        #   # ),
        #   
        #   card_body(plotOutput("VOTES_PLOT", height = 625),
        #                class = "p-0"
        # ),
        # #full_screen = TRUE
        # )
        # )
      )
    )
  
  ),
  
  nav_spacer(),
  nav_item(h4("Itty Bitty Ditties [data explorer]", 
              #style = "color: #002f6c; font-weight: bold;"
  )),
  nav_item(link_git),
  nav_item(input_dark_mode(id = "DARK_MODE", mode = "light")), 
)