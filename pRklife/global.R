library(dplyr)
library(spotifyr)
library(httr)
library(stringr)

require(RCurl) 
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

readRenviron(".Renviron")

# Spotify audio features w/ scores from 0 to 1 that we'll be using for 
# most of the analysis here
numeric_features <- c('danceability', 'energy', 'speechiness', 
                      'acousticness', 'instrumentalness', 
                      'liveness', 'valence')

get_full_blur_data <- function(){
  require(spotifyr)
  require(dplyr)
  access_token <- get_spotify_access_token()
  
  keep_columns <- c('artist', 'album_name', 'album_release_year', 'track_name', 'track_uri',
                    'danceability', 'energy', 'speechiness', 'acousticness', 'instrumentalness',
                    'liveness', 'valence',
                    'key_mode', 'tempo', 'external_urls.spotify')
  
  # Get song data
  blur <- get_artist_audio_features('blur') %>% 
    mutate(artist='Blur') %>% select(one_of(keep_columns))
  graham <- get_artist_audio_features('graham coxon') %>%
    mutate(artist='Graham Coxon') %>% select(one_of(keep_columns))
  damon <- get_artist_audio_features('damon albarn') %>%
    mutate(artist='Damon Albarn') %>% select(one_of(keep_columns))
  gorillaz <- get_artist_audio_features('gorillaz') %>%
    mutate(artist='Gorillaz') %>% select(one_of(keep_columns))
  gbq <- get_artist_audio_features('the good the bad and the queen') %>%
    mutate(artist='The Good, The Bad & The Queen') %>% select(one_of(keep_columns))
  
  # Merge datasets
  blur_and_co <- union(blur, graham) %>%
    union(damon) %>%
    union(gorillaz) %>%
    union(gbq)
  
  blur_and_co
}

# Some basic cleanup of the track data we pulled straight from Spotify
process_blur_data <- function(blur_data){
  blur_data$track_name <- gsub(" - 2012 Remastered Version", "", blur_data$track_name)
  blur_data$track_name <- gsub(" - 2012 Remaster", "", blur_data$track_name)
  blur_data$track_name <- gsub("Damon Albarn: Dr Dee, An English Opera: No. [0-9]*,", "",
                               blur_data$track_name, fixed=FALSE)
  blur_data$track_name <- gsub(" \\(feat. .*\\)", "", blur_data$track_name, fixed=FALSE)
  albums_to_exclude <- c('Dr Dee [Album Sampler]', 'Laika Come Home', 
                         'All The People... Blur Live At Hyde Park 02/07/2009',
                         'All The People... Blur Live At Hyde Park 03/07/2009', 
                         'Bustin + Dronin', "Bustin' + Dronin'", 'D-Sides',
                         'Parklive', 'Demon Days Live At The Manchester Opera House',
                         'Demon Days Live at the Manchester Opera House')
  blur_data <- filter(blur_data, !(album_name %in% albums_to_exclude))
  blur_data
}

# Use kNN to find the n closest songs based on audio features
get_n_closest_songs <- function(song, song_set, n, features=numeric_features){
  require(FNN)
  indices <- get.knnx(song_set %>% select(one_of(features)), 
                      song %>% select(one_of(features)), 
                      n)$nn.index
  song_set[indices,]
}

# More specifically, grab the top 3 closest non-Blur songs to a given Blur song
# and output the artist and track name
get_top_3_closest_table <- function(in_song){
  if(in_song != 'Choose:' & in_song != ''){
    song_row <- filter(blur_data, artist=='Blur' & track_name==in_song)
    song_top3 <- get_n_closest_songs(song_row, blur_data %>% filter(artist != 'Blur'),
                                      3)
    song_top3 <- mutate(song_top3, track=paste0('<a href=\"', external_urls.spotify,
                                                       '\">', track_name, '</a>'))
    song_top3 %>% select(artist, track)
  }
}

# Function to generate playlist with 'harmonic mixing' (idk look up Camelot numbers)
generate_playlist <- function(song_set, max_tracks){
  prev_song <- sample_n(song_set, 1)
  playlist <- prev_song
  
  for(i in 2:max_tracks){
    lil_song_set <- filter(song_set,
                           (code == prev_song$code |
                              (code == (prev_song$code + 1) & mode == prev_song$mode) |
                              (code == (prev_song$code - 1) & mode == prev_song$mode)) &
                             !(track_name %in% playlist$track_name))
    # we're not super wedded to the harmonic mixing thing
    if(nrow(lil_song_set) == 0) 
      lil_song_set <- filter(song_set, !(track_name %in% playlist$track_name))
    prev_song <- sample_n(lil_song_set, 1)
    playlist[i,] <- prev_song
  }
  playlist
}

# variables we'll be using in both UI and server
blur_data <- read.csv("data/blur_and_co_spotify_data.csv", stringsAsFactors=FALSE)
camelot <- read.csv("data/camelot_mappings.csv")
camelot$key_mode <- paste0(camelot$key, camelot$mode)

blur_data <- left_join(blur_data, camelot, by='key_mode')
rm(camelot)

blur_data <- process_blur_data(blur_data)

blur_songs <- blur_data %>% 
  filter(artist=='Blur') %>%
  select(track_name) %>%
  arrange(track_name)
