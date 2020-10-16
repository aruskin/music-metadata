# In iTunes, go to File -> Library -> Export Library
# Save the XML file somewhere

####### Functions

library(XML)
library(dplyr)
library(yaml)

format_itunes_data <- function(){
  # Select the XML file
  itunes_library_export <- file.choose() 
  
  # Be prepared for this to take a while!
  # Is there a faster function/package for reading in XML?
  ituneslib_raw <- readKeyValueDB(itunes_library_export)
  
  # Theres's some metadata we don't care about here--just grab Tracks
  tracksxml <- ituneslib_raw$Tracks
  rm(ituneslib_raw)
  gc()
  
  # Convert list into data frame
  tracksxml <- lapply(tracksxml, data.frame)
  ituneslib_df <- bind_rows(tracksxml)
  rm(tracksxml)
  gc()
  
  # We only care about songs that you actually played
  songs_played <- filter(ituneslib_df, !is.na(Play.Count))
  # Podcast field not a thing anymore in Music (macOS Catalina)
  if('Podcast' %in% colnames(songs_played))
    songs_played <- filter(songs_played, is.na(Podcast) | Podcast != 'TRUE')
  rm(ituneslib_df)
  gc()
  
  songs_played$Genre <- as.character(songs_played$Genre)
  songs_played$Artist <- as.character(songs_played$Artist)
  songs_played$Name <- as.character(songs_played$Name)
  songs_played
}

summarise_by_artist <- function(track_data){
  total_plays <- sum(track_data$Play.Count)
  total_tracks <- nrow(track_data)
  total_minutes <- sum(track_data$Play.Count*(track_data$Total.Time/(60*1000)))

  # Get some summary statistics by artist
  plays_by_artist <- track_data %>%
    dplyr::group_by(Artist) %>%
    dplyr::summarise(total_plays=sum(Play.Count),
              pct_plays=sum(Play.Count)/total_plays,
              total_tracks=n(),
              pct_tracks=n()/total_tracks,
              avg_plays=mean(Play.Count),
              var_plays=var(Play.Count),
              min_plays=min(Play.Count),
              max_plays=max(Play.Count),
              minutes_played=sum(Play.Count*Total.Time/(60*1000))) %>%
    dplyr::arrange(desc(minutes_played))
  
  plays_by_artist
}

summarise_by_artist_grouped <- function(artist_data, artist_groupings){
  total_plays <- sum(artist_data$total_plays)
  total_tracks <- sum(artist_data$total_tracks)
  artist_data$Artist.Group <- artist_data$Artist
  for(gp in names(artist_groupings)){
    artist_data$Artist.Group <- ifelse(
      artist_data$Artist %in% artist_groupings[[gp]],
      paste(gp, '& offshoots'), artist_data$Artist.Group)
  }
  
  artist_data_grouped <- artist_data %>%
    dplyr::group_by(Artist.Group) %>%
    dplyr::summarise(total_plays=sum(total_plays),
              pct_plays=sum(total_plays)/total_plays,
              total_tracks=sum(total_tracks),
              pct_tracks=sum(total_tracks)/total_tracks,
              minutes_played=sum(minutes_played)) %>%
    arrange(-minutes_played) %>%
    dplyr::rename(Artist = Artist.Group)
  
  artist_data_grouped
}

get_spotifyish_summary_long <- function(track_data, artist_data, service_name){
  total_plays <- sum(track_data$Play.Count)
  total_tracks <- nrow(track_data)
  total_minutes <- sum(track_data$Play.Count*(track_data$Total.Time/(60*1000)), na.rm=TRUE)
  # Copy Spotify's end-of-year wrap-up info
  spotifyish_summary <- list(
    diff_songs = total_tracks,
    minutes_listening = floor(total_minutes),
    top_artist = artist_data %>% arrange(-minutes_played) %>% pull(Artist) %>% .[1],
    hours_spent_w_top_artist = floor(max(artist_data$minutes_played, na.rm=TRUE)/60),
    top_5_artists = artist_data %>% arrange(-minutes_played) %>% pull(Artist) %>% .[1:5],
    top_5_songs = track_data %>% 
      arrange(-Play.Count) %>% 
      select(Name, Artist) %>% 
      .[1:5,]
  )
  
  cat(paste("This year you listened to", spotifyish_summary$diff_songs, "different songs in", service_name,
            "\nYou spent", spotifyish_summary$minutes_listening, "minutes listening this year",
            "\nYou spent", spotifyish_summary$hours_spent_w_top_artist, "hours with your favourite artist", 
            spotifyish_summary$top_artist,
            "\nTop Artists: \n", paste0(spotifyish_summary$top_5_artists, collapse="\n "),
            "\nTop Songs: \n", paste0(paste0(spotifyish_summary$top_5_songs$Name, ", ", 
                                             spotifyish_summary$top_5_songs$Artist), 
                                      collapse="\n ")))
}

get_spotifyish_summary_short <- function(track_data, artist_data){
  total_minutes <- sum(track_data$Play.Count*(track_data$Total.Time/(60*1000)), na.rm = TRUE)
  top_5_artists <-artist_data %>% 
    arrange(-minutes_played) %>%
    pull(Artist) %>% 
    .[1:5]
  top_5_songs <- track_data %>% 
    arrange(-Play.Count) %>% 
    select(Name, Artist) %>% 
    .[1:5,]
  
  has_genre <- FALSE
  top_genre <- ''
  if('Genre' %in% colnames(track_data)){
    has_genre <- TRUE
    top_genre <- track_data %>% 
      group_by(Genre) %>% 
      summarise(minutes_played=sum(Play.Count*Total.Time/(60*1000), na.rm = TRUE)) %>% 
      arrange(-minutes_played) %>% select(Genre) %>% filter(!is.na(Genre)) %>% .[1,]
  }
  print(top_genre)
  # Copy Spotify's end-of-year wrap-up info
  spotifyish_summary <- list(
    minutes_listening = floor(total_minutes),
    top_5_artists = top_5_artists,
    top_5_songs = top_5_songs,
    top_genre = top_genre
  )
  
  cat(paste("Minutes Listened \n", 
            spotifyish_summary$minutes_listening,
            ifelse(has_genre, paste0("\nTop Genre \n", spotifyish_summary$top_genre), ""),
            "\nTop Artists: \n", paste0(spotifyish_summary$top_5_artists, collapse="\n "),
            "\nTop Songs: \n", paste0(paste0(spotifyish_summary$top_5_songs$Name, ", ", 
                                             spotifyish_summary$top_5_songs$Artist), 
                                      collapse="\n ")))
}


####### Actually do this
songs_played <- format_itunes_data()
plays_by_artist <- summarise_by_artist(songs_played)
get_spotifyish_summary_long(songs_played, plays_by_artist, "iTunes")
get_spotifyish_summary_short(songs_played, plays_by_artist)

# These mappings are v tailored to my listening habits--for general use
# maybe better to upload a personalized mapping file or something
artist_groupings <- read_yaml('artist_groupings.yaml')

plays_by_artist_group <- summarise_by_artist_grouped(plays_by_artist, artist_groupings)
get_spotifyish_summary_long(songs_played, plays_by_artist_group, "iTunes")
get_spotifyish_summary_short(songs_played, plays_by_artist_group)

plays_by_artist_group %>% select(Artist, minutes_played) %>% .[1:10,]

songs_played %>% select(Artist, Name, Play.Count) %>% arrange(-Play.Count) %>% .[1:10,]