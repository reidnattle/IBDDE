
#source("IBDD_DATA_WRANG.R")

library(shiny)
library(tidyverse)
library(ragg)
library(bslib)
library(thematic)
library(DT)
library(shinyWidgets)
library(scales)
library(hms)
library(rsconnect)
library(ggridges)
library(treemapify)
library(paletteer)
library(ggbump)
library(gghighlight)
library(ggrepel)
library(showtext)
library(Cairo)

options(shiny.useragg = TRUE)
thematic_shiny(font = "auto")
#rsconnect::writeManifest()

SONGS <- read_rds("DATA/SONGS12.rds") 
VOTES <- read_rds("DATA/VOTES12.rds")

SONGS_LONG <- SONGS %>%
  pivot_longer(
    cols = c(
      danceability,
      energy,
      `loudness (dB)`,
      speechiness,
      acousticness,
      instrumentalness,
      liveness,
      valence,
      `tempo (BPM)`,
      `track popularity`,
      `duration (mins)`
    ),
    names_to = "variable"
  )

SONGS_LONG_CAT <- SONGS %>%
  mutate("time signature" = as.character(paste0(time_signature, "/4"))) %>%
  mutate("explicit" = as.character(track.explicit)) %>%
  #mutate(`time signature` = paste(time_signature, "/4")) %>%
  pivot_longer(
    cols = c(`time signature`, explicit, key, mode, `key + mode`),
    names_to = "variable"
  )


ROUND_SELECT_CHOICES <- SONGS %>%
  select(Round) %>%
  distinct() %>%
  as.vector()

ROUND_SELECT_CHOICES <- append(ROUND_SELECT_CHOICES, "All Rounds", after = 0)

PICKER_SELECT_CHOICES <- SONGS %>%
  select(Picker) %>%
  distinct()
PICKER_SELECT_CHOICES <- sort(PICKER_SELECT_CHOICES$Picker)

PICKER_SELECT_CHOICES <- append(PICKER_SELECT_CHOICES, "All Pickers", after = 0)

PICKER_SELECT_CHOICES_2 <- SONGS %>%
  select(Picker) %>%
  distinct()
PICKER_SELECT_CHOICES_2 <- sort(PICKER_SELECT_CHOICES_2$Picker)


VAR_SELECT_CHOICES <- SONGS_LONG %>%
  select(variable) %>%
  distinct()
VAR_SELECT_CHOICES <- as.list(sort(VAR_SELECT_CHOICES$variable))

VAR_SELECT_CHOICES_CAT <- SONGS_LONG_CAT %>%
  select(variable) %>%
  distinct()
VAR_SELECT_CHOICES_CAT <- as.list(sort(VAR_SELECT_CHOICES_CAT$variable))

DEFINITIONS <- read_csv("DATA/VAR_DEFS.csv")

Mode2 <- function(x) {
  ux <- unique(x)
  paste(ux[which(tabulate(match(x, ux)) == max(tabulate(match(x, ux))))], collapse = ", ")
  
}

NegMode2 <- function(x) {
  ux <- unique(x)
  paste(ux[which(tabulate(match(x, ux)) == min(tabulate(match(x, ux))))], collapse = ", ")
}

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}



# IBD logo colors:
#B77D67 Tan/brown
#8CB5B3 Sea Green?
#A23404 Red
#FEB266 Orange
#FEF4EB Cream
