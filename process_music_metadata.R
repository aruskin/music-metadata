library(XML)
library(dplyr)
library(yaml)
library(jsonlite)
library(lubridate)

##### iTunes/Music metadata #####

format_itunes_data <- function(){
  # Select the XML file
  # Expected to be in iTunes (or now Music) library export format
  # i.e., what you get from File -> Library -> Export Library
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

##### ListenBrainz metadata #####

get_min_lb_track_data <- function(list_item){
  min_data <- data.frame(Artist=NA, Album=NA, Name=NA, Total.Time=NA,
                         Listen.Date=NA, Recording.ID=NA, Source='[unspecified]',
                         stringsAsFactors=FALSE)
  min_data['Artist'] <- list_item[['track_metadata']][['artist_name']]
  min_data['Album'] <- list_item[['track_metadata']][['release_name']]
  min_data['Name'] <- list_item[['track_metadata']][['track_name']]
  if('duration_ms' %in% names(list_item[['track_metadata']][['additional_info']]))
    min_data['Total.Time'] <- list_item[['track_metadata']][['additional_info']][['duration_ms']]
  min_data['Listen.Date'] <- as_datetime(list_item[['listened_at']])
  min_data['Recording.ID'] <- list_item[['recording_msid']]
  if('listening_from' %in% names(list_item[['track_metadata']][['additional_info']]))
    min_data['Source'] <- list_item[['track_metadata']][['additional_info']][['listening_from']]
  else if ('origin_url' %in% names(list_item[['track_metadata']][['additional_info']])){
    url <- list_item[['track_metadata']][['additional_info']][['origin_url']]
    if(grepl("bandcamp.com", url, fixed=TRUE)){
      min_data['Source'] <- 'Bandcamp'
    } else if(grepl("youtube.com", url, fixed=TRUE)){
      min_data['Source'] <- 'YouTube'
    } else if(grepl("spotify.com", url, fixed=TRUE)){
      min_data['Source'] <- 'spotify'
    }
  }
  
  min_data
}

format_listenbrainz_data <- function(){
  # Select JSON file
  listenbrainz_export <- file.choose()
  data <- jsonlite::fromJSON(listenbrainz_export, simplifyVector = FALSE)
  
  play_level_data <- lapply(X = data, FUN = get_min_lb_track_data) %>%
    bind_rows()
  
  # Currently assuming same artist and song title = same song since many
  # entries (depending on listening source and LB's development stage when
  # data captured) won't have time or album info or other helpful IDs
  track_level_data <- play_level_data %>%
    dplyr::group_by(Artist, Name) %>%
    dplyr::summarise(Play.Count=n(), Total.Time=max(Total.Time, na.rm=TRUE)) %>%
    dplyr::mutate(Total.Time=ifelse(Total.Time==-Inf, NA, Total.Time))
  
  track_level_data   
}

##### More generic summary functions #####
summarise_by_artist <- function(track_data, include_time=TRUE){
  grand_total_plays <- sum(track_data$Play.Count)
  grand_total_tracks <- nrow(track_data)
  if(!include_time)
    track_data <- mutate(track_data, Total.Time = 0)
  
  # Get some summary statistics by artist
  plays_by_artist <- track_data %>%
    dplyr::group_by(Artist) %>%
    dplyr::summarise(total_plays=sum(Play.Count),
                     pct_plays=sum(Play.Count)/grand_total_plays,
                     total_tracks=n(),
                     pct_tracks=n()/grand_total_tracks,
                     avg_plays=mean(Play.Count),
                     var_plays=var(Play.Count),
                     min_plays=min(Play.Count),
                     max_plays=max(Play.Count),
                     minutes_played=sum(Play.Count*Total.Time/(60*1000)))

  if(include_time)
    plays_by_artist %>% dplyr::arrange(desc(minutes_played))
  else{
    plays_by_artist %>% select(-minutes_played) %>% arrange(desc(total_plays))
  }
}

summarise_by_artist_grouped <- function(artist_data, artist_groupings, include_time=TRUE){
  total_plays <- sum(artist_data$total_plays)
  total_tracks <- sum(artist_data$total_tracks)
  artist_data$Artist.Group <- artist_data$Artist
  if(!include_time) 
    artist_data$minutes_played <- 0
  
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
    dplyr::rename(Artist = Artist.Group)
  
  if(!include_time)
    artist_data_grouped <- artist_data_grouped %>% select(-minutes_played)
  artist_data_grouped
}

get_spotifyish_summary_long <- function(track_data, artist_data, service_name, include_time=TRUE){
  total_plays <- sum(track_data$Play.Count)
  total_tracks <- nrow(track_data)
  spotifyish_summary <- list(
    diff_songs = total_tracks,
    top_5_songs = track_data %>% 
      arrange(-Play.Count) %>% 
      select(Name, Artist) %>% 
      .[1:5,]
  )
  if(!include_time){
    spotifyish_summary[['top_artist']] <- artist_data %>% arrange(-total_plays) %>% pull(Artist) %>% .[1]
    spotifyish_summary[['top_5_artists']] <-  artist_data %>% arrange(-total_plays) %>% pull(Artist) %>% .[1:5]
  }else{
    spotifyish_summary[['top_artist']]  <- artist_data %>% arrange(-minutes_played) %>% pull(Artist) %>% .[1]
    spotifyish_summary[['top_5_artists']] <-  artist_data %>% arrange(-minutes_played) %>% pull(Artist) %>% .[1:5]
    spotifyish_summary[['minutes_listening']] <- floor(sum(track_data$Play.Count*(track_data$Total.Time/(60*1000)), na.rm=TRUE))
    spotifyish_summary[['hours_spent_w_top_artist']] <- floor(max(artist_data$minutes_played, na.rm=TRUE)/60)
  }
    
  cat(paste("This year you listened to", spotifyish_summary[['diff_songs']], "different songs in", service_name,
            ifelse(include_time, 
                   paste("\nYou spent", spotifyish_summary[['minutes_listening']], "minutes listening this year"),
                   ""),
            ifelse(include_time, 
                   paste("\nYou spent", spotifyish_summary[['hours_spent_w_top_artist']], "hours with your favourite artist"),
                   paste("\nYour top artist was")),
            spotifyish_summary[['top_artist']],
            "\nTop Artists: \n", paste0(spotifyish_summary[['top_5_artists']], collapse="\n "),
            "\nTop Songs: \n", paste0(paste0(spotifyish_summary[['top_5_songs']][['Name']], ", ", 
                                             spotifyish_summary[['top_5_songs']][['Artist']]), 
                                      collapse="\n ")))
}

get_spotifyish_summary_short <- function(track_data, artist_data, include_time=TRUE){
  spotifyish_summary <- list()
  
  if(!include_time){
    spotifyish_summary[['top_5_artists']] <- artist_data %>% 
      arrange(-total_plays) %>%
      pull(Artist) %>% 
      .[1:5]
  }else{
    spotifyish_summary[['minutes_listening']] <- sum(track_data$Play.Count*(track_data$Total.Time/(60*1000)), 
                                                     na.rm = TRUE) %>%
      floor()
    spotifyish_summary[['top_5_artists']]  <- artist_data %>% 
      arrange(-minutes_played) %>%
      pull(Artist) %>% 
      .[1:5]
  }
  
  spotifyish_summary[['top_5_songs']] <- track_data %>% 
    arrange(-Play.Count) %>% 
    select(Name, Artist) %>% 
    .[1:5,]
  
  if('Genre' %in% colnames(track_data)){
    spotifyish_summary[['top_genre']] <- track_data %>% 
      group_by(Genre) %>% 
      summarise(minutes_played=sum(Play.Count*Total.Time/(60*1000), na.rm = TRUE)) %>% 
      arrange(-minutes_played) %>% select(Genre) %>% filter(!is.na(Genre)) %>% .[1,]
  }
  
  cat(paste(ifelse('minutes_listening' %in% names(spotifyish_summary),
                   paste0("Minutes Listened \n", spotifyish_summary[['minutes_listening']]),
                    ''),
            ifelse('top_genre' %in% names(spotifyish_summary), 
                   paste0("\nTop Genre \n", spotifyish_summary[['top_genre']]),
                   ''),
            "\nTop Artists: \n", paste0(spotifyish_summary[['top_5_artists']], collapse="\n "),
            "\nTop Songs: \n", paste0(paste0(spotifyish_summary[['top_5_songs']][['Name']], ", ", 
                                             spotifyish_summary[['top_5_songs']][['Artist']]), 
                                      collapse="\n ")))
}