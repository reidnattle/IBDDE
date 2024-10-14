
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

#rsconnect::writeManifest()

SONGS <- read_rds("DATA/SONGS12.rds") %>% 
  rename("track popularity" = track.popularity) %>% 
  rename("tempo (BPM)" = tempo) %>% 
  rename("loudness (dB)" = loudness) %>% 
  mutate(Picker = Picker_Alias) %>% 
  mutate(round_abbr = paste0(str_trunc(Round, width = 12), "..."))

VOTES <- read_rds("DATA/VOTES12.rds") 

SONGS_LONG <- SONGS %>% 
  pivot_longer(cols = c(danceability, 
                        energy, 
                        `loudness (dB)`, 
                        speechiness,
                        acousticness,
                        instrumentalness,
                        liveness,
                        valence,
                        `tempo (BPM)`, 
                        `track popularity`
                        ), names_to = "variable") 


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

VAR_SELECT_CHOICES <- SONGS_LONG %>% 
  select(variable) %>% 
  distinct()
VAR_SELECT_CHOICES <- sort(VAR_SELECT_CHOICES$variable)

DEFINITIONS <- read_csv("DATA/VAR_DEFS.csv") 
  

# IBD logo colors:
#B77D67 Tan/brown
#8CB5B3 Sea Green?
#A23404 Red
#FEB266 Orange
#FEF4EB Cream 
