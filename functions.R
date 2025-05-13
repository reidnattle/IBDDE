

PLOT_REG <- function(.variable = "All Variables", .voter = "All Voters", .round = "All Rounds", .reg = 2, .facet, .color = NULL, .formula = FALSE) {

  if(.voter == "All Voters" & .variable == "All Variables" & .round == "All Rounds") {
  
plot <- SONGS_LONG |>
  ggplot(aes(x = value, y = Points)) +
  geom_jitter(aes(text = Title), alpha = 0.6, size = 1.5, height = 0.02, width = 0.02)+
  stat_smooth(method = "lm", formula = y~poly(x, .reg, raw = TRUE)) +
  stat_poly_eq(formula = y~poly(x, .reg, raw = TRUE), 
               aes(color = ifelse(after_stat(p.value) <= 0.05, "#00ba8b", "grey"),
                   face = ifelse(after_stat(p.value) <= 0.05, "bold", "plain"),
                   label = paste(..rr.label.., ..p.value.label.., sep = "~~~")))+
  scale_color_identity(aesthetics = c("color", "face"))+
  theme(strip.text = element_text(size = 12,face = "bold"), 
        strip.background = element_rect(fill = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA),
        panel.border = element_rect(fill = NA)) +
  #stat_regline_equation(label.y.npc = 0.8, formula = y~poly(x, .reg)) +
  facet_wrap(vars(variable), scales = "free_x") +
  ylim(-5, 27)


plot <- ggpar(plot, legend = "right")

} else if (.voter == "All Voters" & .variable != "All Variables" & .round == "All Rounds" & .facet == "Voter_Alias") {

  plot <- SONGS_LONG_VOTER |>
    filter(variable == .variable) |> 
    #ggscatter(x = "value", y = "Points Assigned", font.label = c(4, "plain"), label.rectangle = TRUE)+
    ggplot(aes(x = value, y = `Points Assigned`)) +
    geom_jitter(alpha = 0.6, size = 1.5, height = 0.2, width = 0.02)+
    stat_smooth(method = "lm", formula = y~poly(x, .reg, raw = TRUE)) +
    stat_poly_eq(formula = y~poly(x, .reg, raw = TRUE), 
                 aes(color = ifelse(after_stat(p.value) <= 0.05, "#00ba8b", "grey"),
                     face = ifelse(after_stat(p.value) <= 0.05, "bold", "plain"),
                     label = paste(..rr.label.., ..p.value.label.., sep = "~~~")))+
    scale_color_identity(aesthetics = c("color", "face"))+
    #stat_regline_equation(label.y.npc = 0.8, formula = y~poly(x, .reg)) +
    facet_wrap(vars(Voter_Alias)) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.border = element_rect(fill = NA),
          panel.background = element_rect(fill = NA),
          strip.text = element_text(size = 12, face = "bold"),
          strip.background = element_rect(fill = NA)) +
    ylim(-1, 5)
  
  plot <- ggpar(plot, legend = "right") 
  

} else if (.voter == "All Voters" & .variable != "All Variables" & .round == "All Rounds" & .facet == "Round"){
  
  plot <- SONGS_LONG |>
    filter(variable == .variable) |> 
    ggplot(aes(x = value, y = Points)) +
    geom_jitter(alpha = 0.6, size = 1.5, height = 0.2, width = 0.02)+
    stat_smooth(method = "lm", formula = y~poly(x, .reg, raw = TRUE)) +
    stat_poly_eq(formula = y~poly(x, .reg, raw = TRUE), 
                 aes(color = ifelse(after_stat(p.value) <= 0.05, "#00ba8b", "grey"),
                     face = ifelse(after_stat(p.value) <= 0.05, "bold", "plain"),
                     label = paste(..rr.label.., ..p.value.label.., sep = "~~~")))+
    scale_color_identity(aesthetics = c("color", "face"))+
    #stat_regline_equation(label.y.npc = 0.8, formula = y~poly(x, .reg)) +
    facet_wrap(vars(Round))+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          strip.text = element_text(size = 12, face = "bold"),
          strip.background = element_rect(fill = NA),
          panel.background = element_rect(fill = NA),
          panel.border = element_rect(fill = NA)) +
    ylim(-5, 27)
  

  
} else if (.voter != "All Voters" & .variable == "All Variables" & .round == "All Rounds"){ 
  
plot <- SONGS_LONG_VOTER |>
  filter(Voter_Alias == .voter) |>
  #ggscatter(x = "value", y = "Points Assigned")+
  ggplot(aes(x = value, y = `Points Assigned`)) +
  geom_jitter(alpha = 0.6, size = 1.5, height = 0.25, width = 0.15)+
  #stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  stat_smooth(method = "lm", formula = y~poly(x, .reg, raw = TRUE))+
  stat_poly_eq(formula = y~poly(x, .reg, raw = TRUE), 
               aes(color = ifelse(after_stat(p.value) <= 0.05, "#00ba8b", "grey"),
                   face = ifelse(after_stat(p.value) <= 0.05, "bold", "plain"),
                   label = paste(..rr.label.., ..p.value.label.., sep = "~~~")))+
  scale_color_identity(aesthetics = c("color", "face"))+
  #stat_regline_equation(label.y.npc = 0.8, formula = y~poly(x, .reg, raw = TRUE)) +
  facet_wrap(vars(variable), scales = "free_x") +
  theme(strip.text = element_text(size = 12, face = "bold"), 
        strip.background = element_rect(fill = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA),
        panel.border = element_rect(fill = NA)) +
  ylim(-1, 5)

plot <- ggpar(plot, legend = "right")


} else if (.voter == "All Voters" & .variable == "All Variables" & .round != "All Rounds"){

  plot <- SONGS_LONG_VOTER |>
    filter(Round == .round) |>
    #ggscatter(x = "value", y = "Points Assigned")+
    ggplot(aes(x = value, y = `Points Assigned`)) +
    geom_jitter(alpha = 0.6, size = 1.5, height = 0.25, width = 0.15)+
    #stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
    stat_smooth(method = "lm", formula = y~poly(x, .reg, raw = TRUE))+
    stat_poly_eq(formula = y~poly(x, .reg, raw = TRUE), 
                 aes(color = ifelse(after_stat(p.value) <= 0.05, "#00ba8b", "grey"),
                     face = ifelse(after_stat(p.value) <= 0.05, "bold", "plain"),
                     label = paste(..rr.label.., ..p.value.label.., sep = "~~~")))+
    scale_color_identity(aesthetics = c("color", "face"))+
    #stat_regline_equation(label.y.npc = 0.8, formula = y~poly(x, .reg, raw = TRUE)) +
    facet_wrap(vars(variable), scales = "free_x") +
    theme(strip.text = element_text(size = 12, face = "bold"), 
          strip.background = element_rect(fill = NA),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = NA),
          panel.border = element_rect(fill = NA)) +
    ylim(-1, 5)
  
  plot <- ggpar(plot, legend = "right")
  

} else if (.voter == "All Voters" & .variable != "All Variables" & .round != "All Rounds"){
  
  plot <- SONGS_LONG_VOTER |>
    filter(variable == .variable) |>
    filter(Round == .round) |>
    #ggscatter(x = "value", y = "Points Assigned")+
    ggplot(aes(x = value, y = `Points Assigned`)) +
    geom_jitter(alpha = 0.6, size = 1.5, height = 0.25, width = 0.15)+
    #stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
    stat_smooth(method = "lm", formula = y~poly(x, .reg, raw = TRUE))+
    stat_poly_eq(formula = y~poly(x, .reg, raw = TRUE), 
                 aes(color = ifelse(after_stat(p.value) <= 0.05, "#00ba8b", "grey"),
                     face = ifelse(after_stat(p.value) <= 0.05, "bold", "plain"),
                     label = paste(..rr.label.., ..p.value.label.., sep = "~~~")))+
    scale_color_identity(aesthetics = c("color", "face"))+
    #stat_regline_equation(label.y.npc = 0.8, formula = y~poly(x, .reg, raw = TRUE)) +
    facet_wrap(vars(Voter_Alias), scales = "free_x") +
    theme(strip.text = element_text(size = 12, face = "bold"), 
          strip.background = element_rect(fill = NA),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = NA),
          panel.border = element_rect(fill = NA)) +
    ylim(-1, 5)
  
  plot <- ggpar(plot, legend = "right")
  

} else{
  
  plot <- SONGS_LONG_VOTER |>
    filter(Voter_Alias == .voter) |>
    filter(variable == .variable) |>
    ggplot(aes(x = value, y = `Points Assigned`)) +
    geom_jitter(alpha = 0.6, size = 1.5, height = 0.2, width = 0.02)+
    #stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
    stat_smooth(method = "lm", formula = y~poly(x, .reg, raw = TRUE))+
    stat_poly_eq(formula = y~poly(x, .reg, raw = TRUE), 
                 aes(color = ifelse(after_stat(p.value) <= 0.05, "#00ba8b", "grey"),
                     fontface = ifelse(after_stat(p.value) <= 0.05, "bold", "plain"),
                     label = paste(..rr.label.., ..p.value.label.., sep = "~~~")))+
    scale_color_identity(aesthetics = c("color", "face"))+
    ylim(-1, 5) +
    theme(panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = NA),
    panel.border = element_rect(fill = NA))
  #ggpar(plot, legend = "right")
}

  if(.formula == TRUE) {
    plot <- plot + 
      stat_regline_equation(label.y.npc = 0.8, formula = y~poly(x, .reg, raw = TRUE), size = 3) +
      #stat_regline_equation(label.y.npc = 0.8, formula = y~poly(x, .reg, raw = TRUE)) +
      theme(legend.position = "none")
    
    return(plot)
  }
  else return(plot)
}
