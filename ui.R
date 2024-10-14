
source("global.R")
thematic_on(font = "auto")

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
                  
                  hr(style = "margin-bottom: 0"),
                  
                  pickerInput("ROUND_SELECT",
                              "Pick a Round",
                              choices = ROUND_SELECT_CHOICES,
                              selected = "All Rounds",
                              options = pickerOptions(actionsBox = TRUE,
                                                      dropupAuto = FALSE,
                                                      style = "btn-primary")
                  ),
                  pickerInput("PICKER_SELECT",
                              "Pick a Picker",
                              choices = PICKER_SELECT_CHOICES,
                              selected = "All Pickers",
                              options = pickerOptions(actionsBox = TRUE,
                                                      dropupAuto = FALSE,
                                                      style = "btn-primary")
                  ),
                  
                  card(card_body(plotOutput("VOTES_DIST_PLOT", height = 240),
                                 class = "p-0"),
                       full_screen = TRUE
                  )),
                
                layout_column_wrap(
                  value_box(
                    title = "runtime",
                    value = textOutput("RUNTIME"),
                    showcase = bsicons::bs_icon("clock-fill",
                                                size = "0.70em"
                                                ),
                    # showcase_layout = showcase_left_center(
                    #   width = 0.2,
                    # ),
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
                    # showcase_layout = showcase_left_center(
                    #   width = 0.2,
                    # )
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
                    # showcase_layout = showcase_left_center(
                    #   width = 0.2,
                    # )
                  )
                ),
                
                layout_column_wrap(
                  width = NULL,
                  style = css(grid_template_columns = "3fr 2fr"),
                  
                  card(
                    sidebar = "GLOBAL_SIDEBAR",
                    full_screen = TRUE,
                    div(
                      card_body(
                        min_height = 500,
                        DTOutput(
                          "SONGS_DT", height = 500), 
                        height = "100%"),
                      style = "font-size:75%")
                  ),
                  
                  card(
                    full_screen = TRUE,
                    card_body(
                      class = "p-0",
                      layout_column_wrap(
                      width = "200px",
                      #title = "Votes",
                      navset_card_tab(
                        title = "Votes Summary",
                        nav_panel("Standings", 
                                  plotOutput("STANDINGS_PLOT")
                        ),
                        nav_panel("Allocations",
                                  plotOutput("VOTE_SUM_PLOT"))
                      )
                    ))
                  )
                )
              )
            )
  ),
  ###################################################################################################
  ########################################   PAGE 2 UI   ############################################ 
  ###################################################################################################
  nav_panel("Song Data",
            
            page_fluid(
              
              tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),

              layout_sidebar(
                sidebar = sidebar(
                  width = 230,
                  fillable = FALSE,
                  id = "GLOBAL_SIDEBAR",
                  open = "always",
                  img(src = "30.png", width = "100%"),
                  
                  hr(style = "margin-bottom: 0"),
                  
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
                              options = pickerOptions(dropupAuto = FALSE,
                                                      style = "btn-primary")),
                  
                  card(
                    card_body(
                      plotOutput("PARAM_DIST_PLOT", height = 200),
                      class = "p-0"
                      ),
                    full_screen = TRUE
                  ),
                  
                  sliderInput("HISTO_SLIDE",
                              "Number of bins:",
                              min = 5,
                              max = 30,
                              value = 15
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
                             # showcase_layout = showcase_left_center(
                             #   width = 0.2,
                             # )
                  ),
                  
                  value_box(
                    title = textOutput("MAX_TITLE"),
                     value = textOutput("MAX_VALUE"),
                    class = "p-0",
                    showcase = bsicons::bs_icon("arrow-up",
                                                size = "0.70em"
                    ),
                    # showcase_layout = showcase_left_center(
                    #   width = 0.2,
                    # ),
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
                  
                  card(
                    sidebar = "GLOBAL_SIDEBAR",
                    full_screen = TRUE,
                    
                    card_body(
                      class = "p-0",
                      layout_column_wrap(
                      width = "200px",
                      #title = "Votes",
                      navset_card_tab(
                        #title = "Votes Summary",
                        nav_panel("Box Plots", 
                                  plotOutput("BOXPLOTS", brush = "PLOT_BRUSH")
                        ),
                        nav_panel("Joy Plots",
                                  plotOutput("DENSE_HISTO_PLOT"))
                      )
                    ))
                    
                   
                  ),
                  
                  card(
                    full_screen = TRUE,
                    div(
                      card_body(
                        class = "p-0",
                        min_height = 500,
                        DTOutput(
                          "SONGS_DT2", height = 500), 
                        height = "100%"),
                      style = "font-size:75%")

                      )
                    )
                  )
                )
              
            ),
  
  nav_panel("Votes",
            
            h3("tbd")
            
            ),
  
  nav_spacer(),
  nav_item(h4("Itty Bitty Ditties [data explorer]")),
  nav_item(input_dark_mode(id = "DARK_MODE", mode = "dark")), 
)