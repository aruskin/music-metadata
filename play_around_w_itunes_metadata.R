# In iTunes, go to File -> Library -> Export Library
# Save the XML file somewhere

####### Functions

library(XML)
library(plyr)
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
  ituneslib_df <- ldply(tracksxml)
  rm(tracksxml)
  gc()
  
  # We only care about songs that you actually played
  # Also fuck podcasts, right?
  songs.played <- filter(ituneslib_df, !is.na(Play.Count), is.na(Podcast) | Podcast != 'TRUE')
  rm(ituneslib_df)
  gc()
  
  songs.played$Genre <- as.character(songs.played$Genre)
  songs.played$Artist <- as.character(songs.played$Artist)
  songs.played$Name <- as.character(songs.played$Name)
  songs.played
}

summarise_by_artist <- function(track_data){
  total.plays <- sum(track_data$Play.Count)
  total.tracks <- nrow(track_data)
  total.minutes <- sum(track_data$Play.Count*(track_data$Total.Time/(60*1000)))

  # Get some summary statistics by artist
  plays.by.artist <- track_data %>%
    group_by(Artist) %>%
    summarise(total_plays=sum(Play.Count),
              pct_plays=sum(Play.Count)/total.plays,
              total_tracks=n(),
              pct_tracks=n()/total.tracks,
              avg_plays=mean(Play.Count),
              var_plays=var(Play.Count),
              min_plays=min(Play.Count),
              max_plays=max(Play.Count),
              minutes_played=sum(Play.Count*Total.Time/(60*1000))) %>%
    arrange(-minutes_played)
  
  plays.by.artist
}

summarise_by_artist_grouped <- function(artist_data, artist_groupings){
  total.plays <- sum(artist_data$total_plays)
  total.tracks <- sum(artist_data$total_tracks)
  artist_data$Artist.Group <- artist_data$Artist
  for(gp in names(artist_groupings)){
    artist_data$Artist.Group <- ifelse(
      artist_data$Artist %in% artist_groupings[[gp]],
      paste(gp, '& offshoots'), artist_data$Artist.Group)
  }
  
  artist_data_grouped <- artist_data %>%
    group_by(Artist.Group) %>%
    summarise(total_plays=sum(total_plays),
              pct_plays=sum(total_plays)/total.plays,
              total_tracks=sum(total_tracks),
              pct_tracks=sum(total_tracks)/total.tracks,
              minutes_played=sum(minutes_played)) %>%
    arrange(-minutes_played) %>%
    rename(Artist = Artist.Group)
  
  artist_data_grouped
}

get_spotifyish_summary_long <- function(track_data, artist_data){
  total.plays <- sum(track_data$Play.Count)
  total.tracks <- nrow(track_data)
  total.minutes <- sum(track_data$Play.Count*(track_data$Total.Time/(60*1000)))
  # Copy Spotify's end-of-year wrap-up info
  spotifyish_summary <- list(
    diff_songs = total.tracks,
    minutes_listening = floor(total.minutes),
    top_artist = artist_data %>% arrange(-minutes_played) %>% pull(Artist) %>% .[1],
    hours_spent_w_top_artist = floor(max(artist_data$minutes_played)/60),
    top_5_artists = artist_data %>% arrange(-minutes_played) %>% pull(Artist) %>% .[1:5],
    top_5_songs = track_data %>% 
      arrange(-Play.Count) %>% 
      select(Name, Artist) %>% 
      .[1:5,],
    top_genre = track_data %>% 
      group_by(Genre) %>% 
      summarise(minutes_played=sum(Play.Count*Total.Time/(60*1000))) %>% 
      arrange(-minutes_played) %>% select(Genre) %>% .[1,]
  )
  
  cat(paste("This year you listened to", spotifyish_summary$diff_songs, "different songs in iTunes",
            "\nYou spent", spotifyish_summary$minutes_listening, "minutes listening this year",
            "\nYou spent", spotifyish_summary$hours_spent_w_top_artist, "hours with your favourite artist", 
            spotifyish_summary$top_artist,
            "\nTop Artists: \n", paste0(spotifyish_summary$top_5_artists, collapse="\n "),
            "\nTop Songs: \n", paste0(paste0(spotifyish_summary$top_5_songs$Name, ", ", 
                                             spotifyish_summary$top_5_songs$Artist), 
                                      collapse="\n ")))
}

get_spotifyish_summary_short <- function(track_data, artist_data){
  total.minutes <- sum(track_data$Play.Count*(track_data$Total.Time/(60*1000)))
  
  # Copy Spotify's end-of-year wrap-up info
  spotifyish_summary <- list(
    minutes_listening = floor(total.minutes),
    top_5_artists = artist_data %>% arrange(-minutes_played) %>% pull(Artist) %>% .[1:5],
    top_5_songs = track_data %>% 
      arrange(-Play.Count) %>% 
      select(Name, Artist) %>% 
      .[1:5,],
    top_genre = track_data %>% 
      group_by(Genre) %>% 
      summarise(minutes_played=sum(Play.Count*Total.Time/(60*1000))) %>% 
      arrange(-minutes_played) %>% select(Genre) %>% .[1,]
  )
  
  cat(paste("Minutes Listened \n", 
            spotifyish_summary$minutes_listening,
            "\nTop Genre \n",
            spotifyish_summary$top_genre,
            "\nTop Artists: \n", paste0(spotifyish_summary$top_5_artists, collapse="\n "),
            "\nTop Songs: \n", paste0(paste0(spotifyish_summary$top_5_songs$Name, ", ", 
                                             spotifyish_summary$top_5_songs$Artist), 
                                      collapse="\n ")))
}


####### Actually do this
songs.played <- format_itunes_data()
plays.by.artist <- summarise_by_artist(songs.played)
get_spotifyish_summary_long(songs.played, plays.by.artist)
get_spotifyish_summary_short(songs.played, plays.by.artist)

# These mappings are v tailored to my listening habits--for general use
# maybe better to upload a personalized mapping file or something
artist_groupings <- read_yaml('artist_groupings.yaml')

plays.by.artist.group <- summarise_by_artist_grouped(plays.by.artist, artist_groupings)
get_spotifyish_summary_long(songs.played, plays.by.artist.group)
get_spotifyish_summary_short(songs.played, plays.by.artist.group)