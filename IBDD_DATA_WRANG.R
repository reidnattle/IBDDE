library(tidyverse)
library(spotifyr)
library(hms)

COMPETITORS_ALIAS <- read_csv("DATA/COMPETITORS_ALIAS.csv")

COMPETITORS <- read_csv("export/competitors.csv") %>% 
  bind_cols(COMPETITORS_ALIAS)

ROUNDS <- read_csv("export/rounds.csv") %>% 
  mutate(PL_ID = str_remove(`Playlist URL`, "https://open.spotify.com/playlist/"))

SUBMISSIONS <- read_csv("export/submissions.csv") %>% 
  mutate(TRACK_ID = str_remove(`Spotify URI`, "spotify:track:")) %>% 
  mutate(Created = date(as.POSIXct(Created, format = "%Y-%m-%d %H:%M:%S")))

VOTES_UP <- read_csv("export/votes.csv") 

VOTES <- VOTES_UP %>% 
  select(1, 2, 4) %>% 
  pivot_wider(names_from = `Voter ID`, values_from = `Points Assigned`, values_fill = 0) %>% 
  pivot_longer(cols = 2:13, names_to = "Voter ID", values_to = "Points Assigned") %>% 
  left_join(VOTES_UP %>% select(1:3, 5:6), by = join_by(`Spotify URI`, `Voter ID`)) %>% 
  mutate(TRACK_ID = str_remove(`Spotify URI`, "spotify:track:")) %>% 
  select(-`Spotify URI`) %>% 
  left_join(COMPETITORS, by = join_by(`Voter ID` == ID)) %>% 
  rename("Voter" = "Name",
         "Voter_Alias" = "Alias") %>%
  left_join(SUBMISSIONS %>% select(TRACK_ID, `Submitter ID`), by = join_by(TRACK_ID)) %>% 
  group_by(TRACK_ID) %>% 
  fill(`Round ID`, .direction = "downup") %>% 
  filter(`Submitter ID` != `Voter ID`)

VOTES_TOTES <- VOTES %>% 
  select(TRACK_ID, `Voter ID`, `Points Assigned`) %>% 
  group_by(TRACK_ID) %>% 
  mutate(Points = sum(`Points Assigned`)) %>% 
  select(1, 4) %>% 
  distinct()

source("SpotifyToke.R")
Sys.setenv(SPOTIFY_CLIENT_ID = SPOTIFY_CLIENT_ID)
Sys.setenv(SPOTIFY_CLIENT_SECRET = SPOTIFY_CLIENT_SECRET)
access_token <- get_spotify_access_token()

scopes <- function() {
  xml2::read_html("https://developer.spotify.com/documentation/general/guides/authorization/scopes/") %>% 
    rvest::html_elements("code") %>% rvest::html_text() %>% 
    unique()
}

print(spotifyr::scopes)
scopes = c(
  "user-library-read",
  "user-read-recently-played",
  "playlist-read-private",
  "playlist-read-collaborative",
  "user-read-private"
)

auth <- get_spotify_authorization_code(Sys.getenv("SPOTIFY_CLIENT_ID"),
                                       Sys.getenv("SPOTIFY_CLIENT_SECRET"),
                                       scopes)

TRACK_DATA <- seq (1, nrow(SUBMISSIONS),100) %>%
  map(function(x) {
    get_track_audio_features(SUBMISSIONS$TRACK_ID[x:(x+99)])
  }) %>% 
  reduce(rbind) %>%    
  drop_na()

PL_DATA <- get_playlist_audio_features(playlist_uris = ROUNDS$PL_ID) %>% 
  unnest(track.artists) %>% 
  select(-href, -id, -type, -uri, -external_urls.spotify) %>% 
  group_by(track.id) %>% 
  mutate(`Artist(s)` = paste0(name, collapse = ", ")) %>%
  select(-c(
    name,
    playlist_owner_name,
    playlist_owner_id,
    is_local,
    primary_color,
    added_by.href,
    added_by.id,
    added_by.type,
    added_by.uri,
    added_by.external_urls.spotify,
    track.type,
    track.episode,
    track.track,
    track.disc_number,
    track.is_local,
    track.album.type,
    track.album.album_type,
    video_thumbnail.url
            )) %>% 
  distinct()
  
SONGS <- SUBMISSIONS %>% 
  left_join(PL_DATA, by = join_by(TRACK_ID == track.id, `Artist(s)`)) %>% 
  left_join(COMPETITORS, by = join_by(`Submitter ID` == ID), suffix = c("_SUB", "_COMP")) %>% 
  rename("Picker_Alias" = "Alias") %>% 
  left_join(ROUNDS, by = join_by(`Round ID` == ID), suffix = c("_SUB", "_ROU")) %>% 
  left_join(VOTES_TOTES) %>% 
  mutate(Runtime = str_remove(str_sub(as_hms(track.duration_ms / 1000), 4, 8), "^0+")) %>% 
  select(-c(
    `Spotify URI`,
    `Visible To Voters`,
    track.name, 
    track.album.available_markets,
    track.album.id,
    track.album.name,
    Created_ROU,
    playlist_name
  )) %>% 
  relocate(Points, .after = `Artist(s)`) %>% 
  relocate(Name_SUB, .after = `Artist(s)`) %>% 
  rename("Picker" = Name_SUB,
         "Round" = Name_ROU,
         "Added" = Created_SUB) %>% 
  rename("track popularity" = track.popularity) %>%
  rename("tempo (BPM)" = tempo) %>%
  rename("loudness (dB)" = loudness) %>%
  mutate(key_mode = str_remove(key_mode, "or")) %>%
  mutate(Picker = Picker_Alias) %>%
  mutate(round_abbr = str_replace_all(str_trunc(Round, width = 15), "—", "-")) %>%
  mutate("duration (mins)" = round(track.duration_ms / 1000 / 60, 3)) %>%
  select(-c(key, mode)) %>%
  rename("mode" = mode_name,
         "key" = key_name,
         "key + mode" = key_mode) %>%
  group_by(Picker) %>%
  mutate(VOTES_TOTES = cumsum(Points)) %>%
  ungroup() %>%
  mutate(added_at = ymd(str_sub(added_at, end = -11))) %>% 
  group_by(Round) %>% 
  mutate(DIST_MEAN = VOTES_TOTES-mean(VOTES_TOTES)) %>% 
  mutate(standings = rank(-VOTES_TOTES, ties.method= "min")) %>% 
  ungroup() %>% 
  mutate(Round_2 = Round) %>% 
  mutate(Round = fct_reorder(str_trunc(Round, 25), added_at)) %>% 
  group_by(Round) %>% 
  mutate(ROUND_NUM = type.convert(Round, as.is = FALSE)) %>% 
  ungroup() %>% 
  mutate(ROUND_NUM = as.numeric(ROUND_NUM)) 

#write_rds(VOTES, "DATA/VOTES12.rds")
#write_rds(SONGS, "DATA/SONGS12.rds")
