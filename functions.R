

PLOT_REG <- function(.variable = "All Variables", .voter = "All Voters", .round = "All Rounds", .reg = 2, .facet) {

  if(.voter == "All Voters" & .variable == "All Variables" & .round == "All Rounds") {
  
plot1 <- SONGS_LONG |>
  ggplot(aes(x = value, y = Points)) +
  geom_jitter(alpha = 0.6, size = 1.5, height = 0.02, width = 0.02)+
  stat_smooth(method = "lm", formula = y~poly(x, .reg, raw = TRUE)) +
  stat_poly_eq(formula = y~poly(x, .reg, raw = TRUE), 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")))+
  theme(strip.text = element_text(size = 12,face = "bold"), 
        strip.background = element_rect(fill = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA),
        panel.border = element_rect(fill = NA)) +
  #stat_regline_equation(label.y.npc = 0.8, formula = y~poly(x, .reg)) +
  facet_wrap(vars(variable), scales = "free_x") +
  ylim(-5, 27)


plot1 <- ggpar(plot1, legend = "right")

return(plot1)

} else if (.voter == "All Voters" & .variable != "All Variables" & .round == "All Rounds" & .facet == "Voter_Alias") {

  plot2 <- SONGS_LONG_VOTER |>
    filter(variable == .variable) |> 
    #ggscatter(x = "value", y = "Points Assigned", font.label = c(4, "plain"), label.rectangle = TRUE)+
    ggplot(aes(x = value, y = `Points Assigned`)) +
    geom_jitter(alpha = 0.6, size = 1.5, height = 0.2, width = 0.02)+
    stat_smooth(method = "lm", formula = y~poly(x, .reg, raw = TRUE)) +
    stat_poly_eq(formula = y~poly(x, .reg, raw = TRUE), 
                 aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")))+    
    #stat_regline_equation(label.y.npc = 0.8, formula = y~poly(x, .reg)) +
    facet_wrap(vars(Voter_Alias)) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.border = element_rect(fill = NA),
          panel.background = element_rect(fill = NA),
          strip.text = element_text(size = 12, face = "bold"),
          strip.background = element_rect(fill = NA)) +
    ylim(-1, 5)
  
  plot2 <- ggpar(plot2, legend = "right") 
  
  return(plot2)
  
} else if (.voter == "All Voters" & .variable != "All Variables" & .round == "All Rounds" & .facet == "Round"){
  
  plot2 <- SONGS_LONG |>
    filter(variable == .variable) |> 
    ggplot(aes(x = value, y = Points)) +
    geom_jitter(alpha = 0.6, size = 1.5, height = 0.2, width = 0.02)+
    stat_smooth(method = "lm", formula = y~poly(x, .reg, raw = TRUE)) +
    stat_poly_eq(formula = y~poly(x, .reg, raw = TRUE), 
                 aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")))+    
    #stat_regline_equation(label.y.npc = 0.8, formula = y~poly(x, .reg)) +
    facet_wrap(vars(Round))+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          strip.text = element_text(size = 12, face = "bold"),
          strip.background = element_rect(fill = NA),
          panel.background = element_rect(fill = NA),
          panel.border = element_rect(fill = NA)) +
    ylim(-5, 27)
  
  return(plot2)
  
  
} else if (.voter != "All Voters" & .variable == "All Variables" & .round == "All Rounds"){ 
  
plot3 <- SONGS_LONG_VOTER |>
  filter(Voter_Alias == .voter) |>
  #ggscatter(x = "value", y = "Points Assigned")+
  ggplot(aes(x = value, y = `Points Assigned`)) +
  geom_jitter(alpha = 0.6, size = 1.5, height = 0.25, width = 0.15)+
  #stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  stat_smooth(method = "lm", formula = y~poly(x, .reg, raw = TRUE))+
  stat_poly_eq(formula = y~poly(x, .reg, raw = TRUE), 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")))+    
  #stat_regline_equation(label.y.npc = 0.8, formula = y~poly(x, .reg, raw = TRUE)) +
  facet_wrap(vars(variable), scales = "free_x") +
  theme(strip.text = element_text(size = 12, face = "bold"), 
        strip.background = element_rect(fill = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA),
        panel.border = element_rect(fill = NA)) +
  ylim(-1, 5)

plot3 <- ggpar(plot3, legend = "right")

return(plot3)

} else if (.voter == "All Voters" & .variable == "All Variables" & .round != "All Rounds"){

  plot3 <- SONGS_LONG_VOTER |>
    filter(Round == .round) |>
    #ggscatter(x = "value", y = "Points Assigned")+
    ggplot(aes(x = value, y = `Points Assigned`)) +
    geom_jitter(alpha = 0.6, size = 1.5, height = 0.25, width = 0.15)+
    #stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
    stat_smooth(method = "lm", formula = y~poly(x, .reg, raw = TRUE))+
    stat_poly_eq(formula = y~poly(x, .reg, raw = TRUE), 
                 aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")))+    
    #stat_regline_equation(label.y.npc = 0.8, formula = y~poly(x, .reg, raw = TRUE)) +
    facet_wrap(vars(variable), scales = "free_x") +
    theme(strip.text = element_text(size = 12, face = "bold"), 
          strip.background = element_rect(fill = NA),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = NA),
          panel.border = element_rect(fill = NA)) +
    ylim(-1, 5)
  
  plot3 <- ggpar(plot3, legend = "right")
  
  return(plot3)
  
} else if (.voter == "All Voters" & .variable != "All Variables" & .round != "All Rounds"){
  
  plot3 <- SONGS_LONG_VOTER |>
    filter(variable == .variable) |>
    filter(Round == .round) |>
    #ggscatter(x = "value", y = "Points Assigned")+
    ggplot(aes(x = value, y = `Points Assigned`)) +
    geom_jitter(alpha = 0.6, size = 1.5, height = 0.25, width = 0.15)+
    #stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
    stat_smooth(method = "lm", formula = y~poly(x, .reg, raw = TRUE))+
    stat_poly_eq(formula = y~poly(x, .reg, raw = TRUE), 
                 aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")))+    
    #stat_regline_equation(label.y.npc = 0.8, formula = y~poly(x, .reg, raw = TRUE)) +
    facet_wrap(vars(Voter_Alias), scales = "free_x") +
    theme(strip.text = element_text(size = 12, face = "bold"), 
          strip.background = element_rect(fill = NA),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = NA),
          panel.border = element_rect(fill = NA)) +
    ylim(-1, 5)
  
  plot3 <- ggpar(plot3, legend = "right")
  
  return(plot3)
  
} else{
  
  plot2 <- SONGS_LONG_VOTER |>
    filter(Voter_Alias == .voter) |>
    filter(variable == .variable) |>
    ggplot(aes(x = value, y = `Points Assigned`)) +
    geom_jitter(alpha = 0.6, size = 1.5, height = 0.2, width = 0.02)+
    #stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
    stat_smooth(method = "lm", formula = y~poly(x, .reg, raw = TRUE))+
    stat_poly_eq(formula = y~poly(x, .reg, raw = TRUE), 
                 aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~"))) +
    ylim(-1, 5) +
    theme(panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = NA),
    panel.border = element_rect(fill = NA))
return(plot2)
  #ggpar(plot2, legend = "right")
}
}
