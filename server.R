

server <- function(input, output, session) {
  
  DARK_MODE_TEXT_SWITCH <- reactive({
    
    if(input$DARK_MODE == "light") { "white"
  } else "black"
  
    
    })
  
  SONGS_DT_REACTIVE <- reactive({
    
    SONGS_DT_REACTIVE <- SONGS %>% 
      select(1, Runtime, 2:5, 47, track.duration_ms, Round, Picker_Alias, track.explicit, track.external_urls.spotify, `Playlist URL`, TRACK_ID) %>% 
      filter(if(input$ROUND_SELECT != "All Rounds") Round == input$ROUND_SELECT else TRUE) %>% 
      filter(if(input$PICKER_SELECT != "All Pickers") Picker == input$PICKER_SELECT else TRUE) %>% 
      mutate(Title2 = Title) %>% 
      mutate(Title = paste0("<a href='", track.external_urls.spotify, "' target='_blank'>", Title,"</a>")) %>% 
      mutate(Round = paste0("<a href='", `Playlist URL`, "' target='_blank'>", Round,"</a>")) %>% 
      select(-c(track.external_urls.spotify, `Playlist URL`))
  })
  
  output$SONGS_DT <- renderDT({
    
    datatable(
      SONGS_DT_REACTIVE(),
      style = "bootstrap",
      fillContainer = TRUE,
      escape = FALSE,
      #rownames = FALSE,
      selection = "single",
      options = list(
        paging = FALSE,
        scrollY = TRUE,
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(list(visible = FALSE, targets = c("Picker_Alias", "track.duration_ms", "track.explicit", "Title2", "TRACK_ID")))
      )
    ) %>% 
      formatStyle(1,
                  fontWeight = 'bold') %>% 
      formatStyle(c(0:13), fontSize = '85%')
  })
  
  SONGS_DT_REACTIVE_SEL <- reactive({
    
    if(length(input$SONGS_DT_rows_selected) > 0){
      
      SONGS_DT_REACTIVE_SEL_PRE <- input$SONGS_DT_rows_selected 
      
      SONGS_DT_REACTIVE_SEL <- SONGS_DT_REACTIVE()[SONGS_DT_REACTIVE_SEL_PRE, ]
      
    }      
    else {
      SONGS_DT_REACTIVE_SEL <- SONGS_DT_REACTIVE() 
      
      SONGS_DT_REACTIVE_SEL
    }
    
  })
  
  
  VOTES_SONGS_REACTIVE <- reactive({
    
    VOTES_SONGS_REACTIVE <- SONGS_DT_REACTIVE_SEL() %>% 
      right_join(VOTES %>% select(`Points Assigned`, Comment, `Voter ID`, TRACK_ID, Voter_Alias), by = join_by(TRACK_ID))
    
    
  })
  
  ### VALUE BOX OUTPUTS ###
  
  output$RUNTIME <- renderText({
    str_remove(str_remove(hms(sum(round(SONGS_DT_REACTIVE_SEL()$track.duration_ms/1000))), "^0+"), "^:+")
  })
  
  output$AVG_RUNTIME <- renderText({
    str_remove(str_remove(str_remove(hms(round(mean(SONGS_DT_REACTIVE_SEL()$track.duration_ms/1000))), "^0+"), "^:+"), "^0+")
  })
  
  output$PCT_EXPLICIT <- renderText({
    paste0(round(sum(SONGS_DT_REACTIVE_SEL()$track.explicit/nrow(SONGS_DT_REACTIVE_SEL())*100), digits = 1), "%")
  })
  
  output$STANDINGS_PLOT <- renderPlot({
    
    STANDINGS_PLOT_DF <- VOTES_SONGS_REACTIVE() %>% 
      filter(!is.na(Picker)) %>% 
      group_by(Picker) %>% 
      mutate(`Points Assigned` = as.integer(sum(`Points Assigned`))) %>% 
      ungroup() %>% 
      select(Picker, `Points Assigned`) %>% 
      distinct()
    
    STANDINGS_PLOT <- STANDINGS_PLOT_DF %>% 
      ggplot(aes(x = reorder(Picker, `Points Assigned`), y = `Points Assigned`))+ 
      geom_segment(aes(xend = reorder(Picker, `Points Assigned`), y = 0, yend = `Points Assigned`))+
      #geom_point(size = 10, aes(color = Picker))+ 
      geom_label(aes(label = `Points Assigned`, fill = Picker), color = DARK_MODE_TEXT_SWITCH(), size = 6, fontface = "bold") +
      theme(axis.text = element_text(size = 13, face = "bold"),
            axis.title = element_text(size = 13, face = "bold"),
            panel.grid.minor = element_blank(),
            legend.position = "none"
            #axis.ticks.x = element_blank(),
            #axis.text.x = element_blank(),
            #panel.grid.major = element_blank(), 
            #panel.background = element_blank()
      )+
      ylab("Points received") +
      xlab("Picker (votes receiver)")+
      scale_y_continuous(breaks = ~round(unique(pretty(.))), expand = expansion(mult = c( 0.08, 0.08)))+
      coord_flip() 
    
    STANDINGS_PLOT
    
  })
  
  YLAB_VOTES_PLOT <- reactive({
    
    if(length(input$SONGS_DT_rows_selected) > 0){
      
      SONGS_DT_SEL_TITLE_pre <- input$SONGS_DT_rows_selected
      
      YLAB_VOTES_PLOT <- ylab(paste0("Points allocated to ", as.character(SONGS_DT_REACTIVE()[SONGS_DT_SEL_TITLE_pre, 12])))
      
    }      
    else {
      
      YLAB_VOTES_PLOT <- ylab(paste0("Points allocated to ", input$PICKER_SELECT))      
      
    }
    
  })
  
  output$VOTE_SUM_PLOT <- renderPlot({
    
    VOTE_SUM_PLOT_DF <- VOTES_SONGS_REACTIVE() %>% 
      filter(!is.na(Picker)) %>% 
      group_by(Voter_Alias) %>% 
      mutate(`Votes Cast` = sum(`Points Assigned`)) %>% 
      ungroup() %>% 
      select(Voter_Alias, `Votes Cast`) %>% 
      distinct()
    
    VOTE_SUM_PLOT <-  VOTE_SUM_PLOT_DF%>% 
      ggplot(aes(x = reorder(factor(Voter_Alias), `Votes Cast`), y = `Votes Cast`))+ 
      annotate("segment", VOTE_SUM_PLOT_DF$Voter_Alias, xend = VOTE_SUM_PLOT_DF$Voter_Alias, y = 0, yend = VOTE_SUM_PLOT_DF$`Votes Cast`, linewidth = 0.5)+
      #geom_point(size = 10, pch = 21, bg = "gray30", color = "#FF13F0")+ 
      geom_label(aes(label = `Votes Cast`, fill = Voter_Alias), color = DARK_MODE_TEXT_SWITCH(), size = 6, fontface = "bold") +
      theme(axis.text = element_text(size = 13, face = "bold"),
            axis.title = element_text(size = 13, face = "bold"),
            panel.grid.minor = element_blank(),
            legend.position = "none"
            #axis.ticks.x = element_blank(),
            #axis.text.x = element_blank(),
            #panel.grid.major = element_blank(), 
            #panel.background = element_blank()
      )+
      YLAB_VOTES_PLOT()+
      #ylab(paste0("Points allocated to ", input$PICKER_SELECT))+
      xlab("Vote allocator")+
      scale_y_continuous(breaks = ~round(unique(pretty(.))), expand = expansion(mult = c( 0.08, 0.08)))+
      coord_flip()
    
    VOTE_SUM_PLOT
    
  })
  
  SONGS_DIST_REACTIVE <- reactive({
    
    SONGS_DIST_REACTIVE <- SONGS %>% 
      select(1, Runtime, 2:5, 48, 10, 8, track.duration_ms, Round, Picker_Alias, track.explicit) %>% 
      filter(if(input$ROUND_SELECT != "All Rounds") Round == input$ROUND_SELECT else TRUE) %>% 
      filter(if(input$PICKER_SELECT != "All Pickers") Picker == input$PICKER_SELECT else TRUE) 
    
  })
  
  SONGS_DT_REACTIVE_DIST_SEL <- reactive({
    
    if(length(input$SONGS_DT_rows_selected) > 0){
      
      SONGS_DT_REACTIVE_DIST_PRE <- input$SONGS_DT_rows_selected 
      
      SONGS_DT_REACTIVE_DIST_SEL <- SONGS_DIST_REACTIVE()[SONGS_DT_REACTIVE_DIST_PRE, ]
      
    }      
    else {
      
      SONGS_DIST_REACTIVE()
      
    }
    
  })
  
  SONGS_DIST_REACTIVE_VOTES <- reactive({
    
    SONGS_DIST_REACTIVE_VOTES <- VOTES %>% 
      right_join(SONGS_DT_REACTIVE_DIST_SEL(), by = join_by(TRACK_ID)) 
    
    
    SONGS_DIST_REACTIVE_VOTES
  })
  
  DIST_TITLE <- reactive({
    if (length(input$SONGS_DT_rows_selected) > 0){
      
      SONGS_DT_SEL_TITLE_pre <- input$SONGS_DT_rows_selected
      
      DIST_TITLE <- ggtitle("Point Distribution", 
                            subtitle = paste0("Song: ", as.character(SONGS_DT_REACTIVE()[SONGS_DT_SEL_TITLE_pre, 12]))
      )
    } else 
      
      DIST_TITLE <- ggtitle("Points distributed to", 
                            subtitle = paste0(input$PICKER_SELECT, " in ", str_trunc(input$ROUND_SELECT, 15))
      )
    
  })
  
  output$VOTES_DIST_PLOT <- renderPlot({
    
    VOTES_DIST_PLOT <- SONGS_DIST_REACTIVE_VOTES() %>% 
      ggplot(aes(x = `Points Assigned`))+
      geom_bar(fill = "#8CB5B3", width = 0.7, color = "#39FF14")+
      geom_label(aes(label = after_stat(count)), stat = "count", vjust = -0.5, colour = DARK_MODE_TEXT_SWITCH(), fontface = "bold", fill = "#8CB5B3")+
      theme(#axis.line = element_line(linetype = "solid"),
        panel.grid.minor = element_blank(),
        axis.title = element_text(face = "bold")
        #panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank(),
        #panel.background = element_blank()
      )+
      scale_y_continuous(breaks = ~round(unique(pretty(.))), expand = expansion(mult = c(0.05, 0.2)))+
      scale_x_continuous(breaks = ~round(unique(pretty(.))))+
      DIST_TITLE()
    
    VOTES_DIST_PLOT
    
  })
  
  ############################################################################################################
  #############################################  PAGE 2 SERVER  ##############################################
  ############################################################################################################
  
  
  SONGS_DT_REACTIVE_2 <- reactive({
    
    SONGS_DT_REACTIVE_2 <- SONGS %>% 
      select(1, Runtime, 2:5, 13:23, 48, 10, 8, `track popularity`, track.duration_ms, Round, Picker_Alias, track.explicit, track.external_urls.spotify, `Playlist URL`, round_abbr, `duration (mins)`) %>% 
      mutate(Title = paste0("<a href='", track.external_urls.spotify, "' target='_blank'>", Title,"</a>")) %>% 
      mutate(Round_link = paste0("<a href='", `Playlist URL`, "' target='_blank'>", Round,"</a>")) %>% 
      select(-c(track.external_urls.spotify, `Playlist URL`))
    
    
  })
  
  PARAM_DEF_UPDATE <- reactive({
    
    DEFINITIONS %>%
      filter(Variable == input$PARAM_SELECT)%>%
      pull("Definition")
    
  })
  
  PARAM_LINK_UPDATE <- reactive({
    
    DEFINITIONS %>%
      filter(Variable == input$PARAM_SELECT)%>%
      pull("Link")
    
  })
  
  
  observeEvent(input$PARAM_SELECT, {
    
    update_popover(
      id = "PARAM_POP",
      title = "Spotify definition",
      PARAM_DEF_UPDATE(),
      a("More info", href = PARAM_LINK_UPDATE(), target = "_blank")
      
    )
    
  })
  
  output$SONGS_DT2 <- renderDT({
    
    SONGS2_DT2 <- SONGS_DT_REACTIVE_2() %>% 
      select(1, 
             input$PARAM_SELECT, 
             Picker, 
             Round_link, 
             Points, 
             `Artist(s)`, 
             Album,
             round_abbr,
             Round
      ) %>% 
      arrange(desc(2))
    
    datatable({if (length(input$PLOT_BRUSH) > 0 & input$GROUP_SELECT == "Picker") {
      brushedPoints(SONGS2_DT2,
                    brush = input$PLOT_BRUSH, 
                    xvar = "Picker",
                    yvar = input$PARAM_SELECT
      )
      
    } else if(length(input$PLOT_BRUSH) > 0 & input$GROUP_SELECT == "Round") {
      brushedPoints(SONGS2_DT2, 
                    brush = input$PLOT_BRUSH, 
                    xvar = "round_abbr",
                    yvar = input$PARAM_SELECT
      )
      
    } else SONGS2_DT2 },
    style = "bootstrap",
    fillContainer = TRUE,
    escape = -c(1, 4),
    rownames = FALSE,
    selection = "none",
    options = list(
      order = list(list(1, 'desc')),
      paging = FALSE,
      scroller = TRUE,
      scrollY = TRUE,
      scrollX = TRUE,
      autoWidth = TRUE,
      columnDefs = list(list(visible = FALSE, targets = c("round_abbr", "Round")),
                        list(width = '175', targets = 1)
      )
    )
    ) %>% 
      formatStyle(2,
                  background = if(input$DARK_MODE == "light") { styleColorBar(range(SONGS2_DT2 %>% select(input$PARAM_SELECT)), 'lightgreen')
                  } else styleColorBar(range(SONGS2_DT2 %>% select(input$PARAM_SELECT)), '#6610f1'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center') %>% 
      formatStyle(c(1:2),
                  fontWeight = 'bold') %>% 
      formatStyle(c(2, 4, 6:7), "white-space"="nowrap") %>% 
      formatStyle(c(1:7), fontSize = '85%')
    
    
  })
  
  HISTO_DENSE <- reactive({
    
    if(input$HISTO_DENSE_OPT == "Density"){
      
      HISTO_DENSE <- geom_area(stat = "density", fill = "#8CB5B3", color = "#39FF14", alpha = 0.6)
    } else {
      HISTO_DENSE <- geom_histogram(bins = input$HISTO_SLIDE, fill = "#8CB5B3", color = "#39FF14")
    }
    
    
  })
  
  output$PARAM_DIST_PLOT <- renderPlot({
    
    SONGS %>% 
      ggplot(aes(x = .data[[input$PARAM_SELECT]]))+
      HISTO_DENSE()+
      theme(#axis.line = element_line(linetype = "solid"),
        #panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(
                                    face = "bold")
        #panel.background = element_blank()
      )
    
  })
  
  # X_LAB_BOX <- reactive({
  #   
  #   if(input$GROUP_SELECT == "Round") {
  #     scale_x_discrete(label=function(x) paste0(str_trunc(x, width = 12), "..."))
  #   } else
  #     NULL
  # }) 
  
  
  PLOT_TITLE_REACTIVE <- reactive({
    
    PLOT_TITLE_REACTIVE <- ggtitle(paste0(input$PARAM_SELECT, " by ", input$GROUP_SELECT))
    
  })
  
  
  output$BOXPLOTS <- renderPlot({
    
    SONGS_LONG %>%
      mutate(Round = round_abbr) %>% 
      filter(variable == input$PARAM_SELECT) %>% 
      #mutate(Round = paste0(str_trunc(Round, width = 12), "...")) %>% 
      ggplot(aes(x = reorder(.data[[input$GROUP_SELECT]], value, FUN = median), 
                 y = value, 
                 fill = .data[[input$GROUP_SELECT]]))+
      #scale_x_discrete(labels = Round)+
      geom_boxplot(outlier.shape = NA)+
      geom_jitter(width = 0.08)+
      theme(text = element_text(size = 12),
            axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1),
            #axis.title.x = element_blank(),
            axis.title = element_text(size = 14,
                                        face = "bold"),
            legend.position = "none",
            plot.title = element_text(size = 20, hjust = 1),
            #axis.line = element_line(linetype = "solid"),
            #panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            #panel.background = element_blank()
      )+
      ylab(input$PARAM_SELECT)+
      xlab(input$GROUP_SELECT)+
      PLOT_TITLE_REACTIVE()
    
  })
  
  output$DENSE_HISTO_PLOT <- renderPlot({
    
    SONGS_LONG %>% 
      mutate(Round = round_abbr) %>% 
      filter(variable == input$PARAM_SELECT) %>% 
      #mutate(Round = paste0(str_trunc(Round, width = 14), "...")) %>% 
      ggplot(aes(
        x = value, 
        y = reorder(.data[[input$GROUP_SELECT]], value, FUN = median), 
        fill = .data[[input$GROUP_SELECT]]
      ))+
      geom_density_ridges(quantile_lines = TRUE, quantiles = 2) +
      ylab(input$GROUP_SELECT)+
      theme(axis.text = element_text(size = 12), 
            plot.title = element_text(size = 20, hjust = 1),
            legend.position = "none",
            #axis.title.y = element_blank(),
            axis.title = element_text(size = 14,
                                        face = "bold"),
            #axis.line = element_line(linetype = "solid"),
            #panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            #panel.background = element_blank()
      )+
      scale_x_continuous(expand = c(0, 0))+
      scale_y_discrete(expand = expand_scale(mult = c(0.01, 0.15))) +
      xlab(input$PARAM_SELECT)+
      PLOT_TITLE_REACTIVE()
    
  })
  
  ### VALUE BOX OUTPUTS ###
  
  SONGS_LONG_REACT <- reactive({
    
    SONGS_LONG_REACT <- SONGS_LONG %>% 
      filter(variable == input$PARAM_SELECT) 
    
  })
  
  PARAM_MIN <- reactive({
    
    min(SONGS_LONG_REACT()$value)
    
  })
  
  
  
  output$MIN_TITLE <- renderText({
    
    paste0("min ", input$PARAM_SELECT)
    
  })
  
  output$MIN_VALUE <- renderText({
    
    MIN_VALUE <- as.character(round(min(SONGS_LONG_REACT()$value), 3))
    
  })
  
  output$MAX_TITLE <- renderText({
    
    paste0("max ", input$PARAM_SELECT)
    
  })
  
  output$MAX_VALUE <- renderText({
    
    MAX_VALUE <- as.character(round(max(SONGS_LONG_REACT()$value), 3))
    
  })
  
  output$MEDIAN_TITLE <- renderText({
    
    paste0("median ", input$PARAM_SELECT)
    
  })
  
  output$MEDIAN_VALUE <- renderText({
    
    MEDIAN_VALUE <- as.character(round(median(SONGS_LONG_REACT()$value), 3))
    
  })
  
  
  ############################################################################################################
  #############################################  PAGE 3 SERVER  ##############################################
  ############################################################################################################  
  SONGS_DT_REACTIVE_3 <- reactive({
    
    SONGS_DT_REACTIVE_3 <- SONGS %>% 
      select(1, Runtime, 2:5, 13:23, 48, 10, 8, `track popularity`, track.duration_ms, Round, Picker_Alias, "explicit" = track.explicit, track.external_urls.spotify, `Playlist URL`, round_abbr, "time signature" = time_signature, `key + mode`, key, mode) %>% 
      mutate(Title = paste0("<a href='", track.external_urls.spotify, "' target='_blank'>", Title,"</a>")) %>% 
      mutate(Round = paste0("<a href='", `Playlist URL`, "' target='_blank'>", Round,"</a>")) %>% 
      select(-c(track.external_urls.spotify, `Playlist URL`))
    
    
  })
  
  PARAM_DEF_UPDATE_CAT <- reactive({
    
    DEFINITIONS %>%
      filter(Variable == input$PARAM_SELECT_CAT)%>%
      pull("Definition")
    
  })
  
  PARAM_LINK_UPDATE_CAT <- reactive({
    
    DEFINITIONS %>%
      filter(Variable == input$PARAM_SELECT_CAT)%>%
      pull("Link")
    
  })
  
  
  observeEvent(input$PARAM_SELECT_CAT, {
    
    update_popover(
      id = "PARAM_POP_CAT",
      title = "Spotify definition",
      PARAM_DEF_UPDATE_CAT(),
      a("More info", href = PARAM_LINK_UPDATE_CAT(), target = "_blank")
      
    )
    
  })
  
  output$SONGS_DT3 <- renderDT({
    
    #req(input$GROUP_SELECT_CAT)
    
    SONGS2_DT3 <- SONGS_DT_REACTIVE_3() %>% 
      mutate(across(c(key, explicit, `key + mode`, `time signature`, mode), factor)) %>% 
      select(1, 
             input$PARAM_SELECT_CAT, 
             Picker, 
             Round, 
             Points, 
             `Artist(s)`, 
             Album,
             round_abbr
      ) %>% 
      group_by(input$PARAM_SELECT_CAT)  
    
    datatable(
      SONGS2_DT3, 
      style = "bootstrap",
      filter = "top",
      fillContainer = TRUE,
      escape = -c(1, 4),
      rownames = FALSE,
      selection = "none",
      options = list(
        #order = list(list(1, 'desc')),
        paging = FALSE,
        scroller = TRUE,
        scrollY = TRUE,
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(list(visible = FALSE, targets = c("round_abbr")),
                          list(width = '175', targets = 1)
        )
      )
    ) %>% 
      formatStyle(c(1:2),
                  fontWeight = 'bold') %>%  
      formatStyle(c(2, 4, 6:7), "white-space"="nowrap") %>% 
      formatStyle(c(1:7), fontSize = '85%') %>% 
      formatStyle(1, 
                  backgroundColor = )
  })
  
  PLOT_TITLE_CAT_REACTIVE <- reactive({
    
    PLOT_TITLE_CAT_REACTIVE <- ggtitle(paste0(input$PARAM_SELECT_CAT, " by ", input$GROUP_SELECT_CAT))
    
  })
  
  PROP_COUNT_Y <- reactive({
    
    if(input$PROP_COUNT_OPT == "proportion of songs"){
      PROP_COUNT_Y <- c(geom_col(color = "#212121", position = "fill", linewidth = 0.2),
                        geom_text(position = position_fill(vjust = 0.5), fontface = "bold", size = 4, color = "black"),
                        scale_y_continuous(name = "proportion of songs")
      )
      
    } else {
      PROP_COUNT_Y <- c(geom_col(color = "#212121", linewidth = 0.2),
                        geom_text(position = position_stack(vjust = 0.5), fontface = "bold", size = 4, color = "black"),
                        scale_y_continuous(name = "number of songs", breaks = ~round(pretty(.)))
                        #ylab("number of songs")
      )
    }
  })
  
  output$CATBAR_PLOT <- renderPlot({
    
    req(c(input$GROUP_SELECT_CAT, input$PARAM_SELECT_CAT))
    
    CAT_FREQ_DF <- SONGS_LONG_CAT %>% 
      mutate(Round = round_abbr) %>% 
      #mutate(Round = paste0(str_trunc(Round, width = 12), "...")) %>%
      filter(variable == input$PARAM_SELECT_CAT) %>% 
      select(value, input$GROUP_SELECT_CAT) %>% 
      table() %>% 
      as.data.frame() %>%
      filter(Freq > 0)
    
    ggplot(CAT_FREQ_DF, aes(x = .data[[input$GROUP_SELECT_CAT]],
                            y = Freq,
                            fill = value,
                            label = value))+
      PROP_COUNT_Y()+
      # scale_y_continuous(labels = scales::percent)+
      scale_fill_hue(#h = c(0, 360) + 15,
        c = 70,
        #l = 65,
        #h.start = 0
      )+
      xlab(input$GROUP_SELECT)+
      theme(text = element_text(size = 12),
            axis.text.x = element_text(angle = 40, vjust = 1, hjust = 1),
            #axis.title.x = element_blank(),
            axis.title = element_text(size = 14,
                                        face = "bold"),
            legend.position = "none",
            axis.line = element_line(linetype = "solid"),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            plot.title = element_text(size = 20, hjust = 1)
      )+
      PLOT_TITLE_CAT_REACTIVE()
    
    
    
  })
  
  CAT_FREQ_TAB_ROUND_DF <- reactive({
    
    
    req(c(input$GROUP_SELECT_CAT, input$PARAM_SELECT_CAT))
    
    
    CAT_FREQ_TAB_ROUND_DF <- SONGS_LONG_CAT %>% 
      filter(variable == input$PARAM_SELECT_CAT) %>% 
      select(value, Round) %>% 
      table() %>% 
      as.data.frame() %>% 
      group_by(Round, value) %>% 
      summarise(Freq = sum(Freq)) %>% 
      ungroup()
    
  })
  
  output$CAT_FREQ_TAB_ROUND <- renderDT({
    
    CAT_FREQ_TAB_ROUND <- CAT_FREQ_TAB_ROUND_DF() %>% 
      pivot_wider(names_from = value, values_from = Freq) %>% 
      datatable(rownames = FALSE,
                style = "bootstrap",
                fillContainer = TRUE,
                selection = 'none',
                options = list(dom = 't',
                               paging = FALSE,
                               scroller = TRUE,
                               scrollY = TRUE,
                               scrollX = TRUE,
                               autoWidth = TRUE)
      )
    
    CAT_FREQ_TAB_ROUND
    
    
  })
  
  CAT_FREQ_TAB_PICKER_DF <- reactive({
    
    CAT_FREQ_TAB_PICKER_DF <- SONGS_LONG_CAT %>% 
      filter(variable == input$PARAM_SELECT_CAT) %>% 
      select(value, Picker) %>% 
      table() %>% 
      as.data.frame() %>% 
      group_by(Picker, value) %>% 
      summarise(Freq = sum(Freq)) %>% 
      ungroup()
    
  })
  
  
  output$CAT_FREQ_TAB_PICKER <- renderDT({
    
    CAT_FREQ_TAB_PICKER <- CAT_FREQ_TAB_PICKER_DF() %>% 
      pivot_wider(names_from = value, values_from = Freq) %>% 
      datatable(rownames = FALSE,
                style = "bootstrap",
                fillContainer = TRUE,
                selection = 'none',
                options = list(dom = 't',
                               paging = FALSE,
                               scroller = TRUE,
                               scrollY = TRUE,
                               scrollX = TRUE,
                               autoWidth = TRUE)
      )
    
    CAT_FREQ_TAB_PICKER
    
    
  })
  
  
  #observeEvent(c(input$GROUP_SELECT_CAT, input$PARAM_SELECT_CAT, input$TREE_BAR_OPT), {
  
  output$TREEMAP_PLOT <- renderPlot({
    
    SONGS_LONG_CAT_MOS_DF_TREE <- SONGS_LONG_CAT %>% 
      filter(variable == input$PARAM_SELECT_CAT) %>% 
      select(value) %>% 
      group_by(value) %>% 
      mutate(count = n()) %>% 
      distinct()
    
    TREEMAP_PLOT <- SONGS_LONG_CAT_MOS_DF_TREE %>% 
      ggplot(
        aes(area = count, 
            fill = value,
            label = value
        )
      )+
      geom_treemap()+
      geom_treemap_text(place = "center",
                        color = "#212121",
                        #grow = TRUE
                        fontface = "bold")+
      scale_fill_hue(#h = c(0, 360) + 15,
        c = 80,
        #l = 65,
        #h.start = 0
      )+
      theme(legend.position = "none")
    
    TREEMAP_PLOT
    
  })
  
  #})
  
  
  
  
  output$CAT_FULL_BAR <- renderPlot({
    
    SONGS_LONG_CAT_MOS_DF_BAR <- SONGS_LONG_CAT %>% 
      filter(variable == input$PARAM_SELECT_CAT) %>% 
      select(value) %>% 
      group_by(value) %>% 
      mutate(count = n()) %>% 
      distinct()
    
    CAT_FULL_BAR <- SONGS_LONG_CAT_MOS_DF_BAR %>% 
      ggplot(aes(x = reorder(value, count), y = count, fill = value))+
      geom_col()+
      scale_fill_hue(#h = c(0, 360) + 15,
        c = 70,
        #l = 65,
        #h.start = 0
      )+
      #scale_fill_discrete()+
      theme(
        legend.position = "none",
        axis.title.y = element_blank(),
        #axis.line = element_line(linetype = "solid"),
        #panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        #panel.background = element_blank()
      )+
      coord_flip()
    
    CAT_FULL_BAR
    
  })
  
  output$FREQ_TAB_FULL <- renderDT({
    
    FREQ_DF <- SONGS_LONG_CAT_REACT() %>% 
      distinct() 
    
    
    FREQ_TAB_FULL <- FREQ_DF
    datatable(FREQ_DF, 
              rownames = FALSE,
              class = "display",
              style = "bootstrap",
              options = list(paging = FALSE,
                             searching = FALSE,
                             info = FALSE,
                             order = list(list(1, 'desc'))
              )
    ) %>% 
      formatStyle(1:2, fontSize = "80%")
    
  })
  
  SONGS_LONG_CAT_REACT <- reactive({
    
    SONGS_LONG_CAT_REACT <- SONGS_LONG_CAT %>% 
      filter(variable == input$PARAM_SELECT_CAT) %>% 
      select(value) %>% 
      group_by(value) %>% 
      mutate(count = n())
    
  })
  
  output$COMMON_TITLE <- renderText({
    
    paste0("most common ", input$PARAM_SELECT_CAT)
    
  })
  
  output$COMMON_VALUE <- renderText({
    
    COMMON_VALUE <- as.character(Mode2(SONGS_LONG_CAT_REACT()$value))
    
  })
  
  output$RARE_TITLE <- renderText({
    
    paste0("least common ", input$PARAM_SELECT_CAT)
    
  })
  
  output$RARE_VALUE <- renderText({
    
    RARE_VALUE <- as.character(NegMode2(SONGS_LONG_CAT_REACT()$value))
    
  })
########################################################################    
########################  PAGE 4  SERVER  ##############################    
########################################################################    
  
  # observe(session$getCurrentTheme(
  #   if (isTRUE(input$DARK_MODE)) dark else light
  # ))
  
  STANDINGS_TAB_PRE <- reactive({
    
    if(input$STAND_PLOT_OPT == 'Standings'){
    
    STANDINGS_TAB_PRE <- nearPoints(
      SONGS,
      coordinfo = input$SANDINGS_PLOT_CLICK, 
      xvar = "Round",
      yvar = "standings",
      threshold = 10,
      allRows = TRUE
    ) %>% 
      filter(Picker %in% c(input$PICKER_SELECT_2)) 
    
    } else {
      
      STANDINGS_TAB_PRE <- brushedPoints(
        SONGS,
        brush = input$SANDINGS_PLOT_BRUSH, 
        xvar = "ROUND_NUM",
        yvar = if(input$SCORE_Y_OPT == "cumulative scores") VOTES_TOTES else "DIST_MEAN",
        allRows = TRUE
      ) %>% 
        filter(Picker %in% c(input$PICKER_SELECT_2)) 
      
    }
    
  }) %>% 
    bindCache(
      input$STAND_PLOT_OPT,
      input$PICKER_SELECT_2,
      input$SANDINGS_PLOT_BRUSH,
      input$SCORE_Y_OPT,
      #SONGS_BUMP_PLOT_DF(),
      input$SANDINGS_PLOT_CLICK
    )
  
  output$STANDINGS_TAB <- renderDT({
    
    STANDINGS_TAB <- datatable({if ( sum(STANDINGS_TAB_PRE()$selected_) > 0) {
      STANDINGS_TAB_PRE() %>% 
        filter(ROUND_NUM >= input$STANDINGS_RANGE[1] & ROUND_NUM <= input$STANDINGS_RANGE[2]) %>%
        filter(selected_ == TRUE) %>% 
        select(Title, Points, Picker, Round, standings, VOTES_TOTES, DIST_MEAN)

    } else STANDINGS_TAB_PRE() %>% 
        filter(ROUND_NUM >= input$STANDINGS_RANGE[1] & ROUND_NUM <= input$STANDINGS_RANGE[2]) %>%
        select(Title, Points, Picker, Round, standings, VOTES_TOTES, DIST_MEAN)},
    
    style = "bootstrap",
    fillContainer = TRUE,
    #escape = -c(1, 4),
    rownames = FALSE,
    selection = "none",
    options = list(
      server = FALSE,
      dom = "t",
      order = list(list(1, 'desc')),
      paging = FALSE,
      scroller = TRUE,
      scrollY = TRUE,
      scrollX = TRUE,
      autoWidth = TRUE,
      columnDefs = list(list(visible = FALSE, targets = c("standings", "VOTES_TOTES", "DIST_MEAN")),
                        list(width = '110px', targets = 0),
                        list(width = '50px', targets = 1)
                        )
      )      
    )%>% 
    formatStyle(2,
                background = if(input$DARK_MODE == "light") { styleColorBar(range(STANDINGS_TAB_PRE() %>% select(Points)), 'lightgreen')
                } else styleColorBar(range(STANDINGS_TAB_PRE() %>% select(Points)), '#6610f1'),
                backgroundSize = '98% 88%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'center') %>% 
    formatStyle(c(1:2),
                fontWeight = 'bold') %>% 
    formatStyle(c(2, 4), "white-space"="nowrap") %>% 
    formatStyle(c(1:5), fontSize = '85%')
    
    STANDINGS_TAB
    
  }) 
  
  HIGHLIGHT <- reactive({
    if (input$PICKER_SELECT_2 == "None") NULL else input$PICKER_SELECT_2
  }) %>% 
    bindCache(input$PICKER_SELECT_2)

  
  # LABELS_BUMP_CUM <- reactive({
  #   
  #   if()
  #   
  #   LABELS_BUMP_CUM <- list(geom_text_repel(
  #     
  #     #data = ~head(.x, 1), 
  #     data = SONGS %>%
  #       filter(ROUND_NUM >= input$STANDINGS_RANGE[1] & ROUND_NUM <= input$STANDINGS_RANGE[2]) %>%
  #       filter(added_at == min(added_at)),
  #     aes(
  #       x = ROUND_NUM, 
  #       label = Picker),
  #     size = 5,
  #     direction = "y",
  #     nudge_x = -0.5,
  #     fontface = "bold"
  #   ),
  #     geom_label_repel(
  #       # data = ~tail(.x, 1),
  #       data = SONGS %>%
  #         filter(ROUND_NUM >= input$STANDINGS_RANGE[1] & ROUND_NUM <= input$STANDINGS_RANGE[2]) %>%
  #         filter(added_at == max(added_at)),
  #       aes(x = ROUND_NUM, 
  #           label = paste0(standings, " ", Picker),
  #           fill = Picker),
  #       color = DARK_MODE_TEXT_SWITCH(),
  #       size = 5,
  #       direction = "y",
  #       segment.colour = NA,
  #       nudge_x = 0.5,
  #       fontface = "bold",
  #       #check_overlap = TRUE,
  #       hjust = 0.1
  #     ))
  #   
  # })
  
  output$SONGS_BUMP_PLOT <- renderPlot({
  
    SONGS_BUMP_PLOT_DF <- SONGS %>% 
      select(ROUND_NUM, 
             standings,
             Round,
             Picker,
             VOTES_TOTES,
             added_at) %>% 
      filter(ROUND_NUM >= input$STANDINGS_RANGE[1] & ROUND_NUM <= input$STANDINGS_RANGE[2])
    
    SONGS_BUMP_PLOT <- SONGS_BUMP_PLOT_DF %>% 
      ggplot(aes(x = ROUND_NUM, y = standings, color = Picker))+
      geom_bump(aes(group = Picker), size = 2, smooth = 5) + 
      geom_point(size = 8.5)+
      scale_y_reverse(n.breaks = 11)+
      scale_x_continuous(guide = guide_axis(n.dodge = 3), 
                         limits = c(input$STANDINGS_RANGE[1] - 0.5, input$STANDINGS_RANGE[2] + 0.8), 
                         breaks = c(input$STANDINGS_RANGE[1]:input$STANDINGS_RANGE[2]), 
                         labels = unique(SONGS_BUMP_PLOT_DF$Round)
                         )+
      geom_text_repel(
        
        data = ~filter(.x, added_at == min(added_at)), 
        aes(
          x = min(ROUND_NUM),
          label = Picker),
        size = 5,
        direction = "y",
        nudge_x = -0.5,
        fontface = "bold"
      )+
      geom_label_repel(
        data = ~filter(.x, added_at == max(added_at)), 
        aes(x = ROUND_NUM, 
            label = paste0(standings, " ", Picker),
            fill = Picker),
        color = DARK_MODE_TEXT_SWITCH(),
        size = 5,
        direction = "y",
        segment.colour = NA,
        nudge_x = 0.5,
        fontface = "bold",
        hjust = 0.1
      )+
      geom_text(aes(label = VOTES_TOTES),
                color = DARK_MODE_TEXT_SWITCH(),
                size = 3.5,
                fontface = "bold",
                check_overlap = TRUE)+
      coord_cartesian(clip="off")+
      labs(x = paste0("Round (", "time \U2192)"))+
      theme(legend.position = "none",
            axis.text.x = element_text(
              size = 13,
              face = "bold"
            ),
            axis.text.y = element_text(size = 13, face = "bold"),
            panel.background = element_blank(),
            panel.grid = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_text(size = 16, face = "bold"),
            axis.ticks = element_blank()
      ) +
      gghighlight(
        Picker %in% c(input$PICKER_SELECT_2),
        use_direct_label = FALSE,
        unhighlighted_params = list(colour = NULL, fill = NULL, alpha = 0.1)
      )
  
  
    SONGS_BUMP_PLOT
  
  })%>% 
    bindCache(
      #SONGS_BUMP_PLOT_DF(),
      input$PICKER_SELECT_2,
      input$STANDINGS_RANGE[1],
      input$STANDINGS_RANGE[2],
      session$clientData$output_SONGS_BUMP_PLOT_bg,
      DARK_MODE_TEXT_SWITCH()
    )
  
  
  output$SONGS_CUM_PLOT <- renderPlot({
    
    SONGS_CUM_PLOT_PRE <- SONGS %>% 
      select(ROUND_NUM, 
             standings,
             Round,
             Picker,
             VOTES_TOTES,
             added_at,
             DIST_MEAN) %>% 
      filter(ROUND_NUM >= input$STANDINGS_RANGE[1] & ROUND_NUM <= input$STANDINGS_RANGE[2]) 

    SONGS_CUM_PLOT <- SONGS_CUM_PLOT_PRE %>% 
      ggplot(aes(x = ROUND_NUM, y = if(input$SCORE_Y_OPT == "cumulative scores") VOTES_TOTES else DIST_MEAN, color = Picker))+
      geom_line(aes(group = Picker), linewidth = 0.8) + 
      geom_point(size = 2.5)+
      scale_x_continuous(guide = guide_axis(n.dodge = 3), 
                         limits = c(input$STANDINGS_RANGE[1] - 0.5, input$STANDINGS_RANGE[2] + 0.8), 
                         breaks = c(input$STANDINGS_RANGE[1]:input$STANDINGS_RANGE[2]), 
                         labels = unique(SONGS_CUM_PLOT_PRE$Round))+
      geom_label_repel(
        data = ~filter(.x, added_at == max(added_at)), 
        aes(x = ROUND_NUM, 
            label = paste0(standings, " ", Picker),
            fill = Picker),
        color = DARK_MODE_TEXT_SWITCH(),
        size = 5,
        direction = "y",
        segment.colour = NA,
        nudge_x = 0.5,
        fontface = "bold",
        hjust = 0.1
      )+
      coord_cartesian(clip="off")+
      ylab(if(input$SCORE_Y_OPT == "cumulative scores") "cumulative scores" else "distance from mean cummulative score") +
      labs(x = paste0("Round (", "time \U2192)"))+
      theme(legend.position = "none",
            panel.grid.minor.x = element_blank(),
            axis.text.x = element_text(
              size = 13,
              face = "bold"
            ),
            axis.text.y=element_text(size = 12),
            axis.title.x = element_text(size = 16, face = "bold"),
            axis.title.y = element_text(size = 16,
                                        face = "bold"),
      ) +
      gghighlight(
        Picker %in% c(input$PICKER_SELECT_2),
        use_direct_label = FALSE,
        unhighlighted_params = list(colour = NULL, fill = NULL, alpha = 0.1)
      )
    
    
    SONGS_CUM_PLOT
  }) %>% 
    bindCache(
      #SONGS_BUMP_PLOT_DF(),
      input$PICKER_SELECT_2,
      input$SCORE_Y_OPT,
      input$STANDINGS_RANGE[1],
      input$STANDINGS_RANGE[2],
      session$clientData$output_SONGS_CUM_PLOT_bg,
      DARK_MODE_TEXT_SWITCH()
    )
  
    }
