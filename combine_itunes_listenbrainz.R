source('process_music_metadata.R')
library(reshape2)

# These mappings are v tailored to my listening habits--for general use
# maybe better to upload a personalized mapping file or something
artist_groupings <- read_yaml('artist_groupings.yaml')

# Select files with iTunes and LsitenBrainz data exports
songs_played_itunes <- format_itunes_data()
songs_played_lb <- format_listenbrainz_data()
gc()

songs_played_itunes <- mutate(songs_played_itunes, Source='iTunes')
songs_played_lb  <- mutate(songs_played_lb , Source='ListenBrainz')

songs_played_all <- bind_rows(songs_played_itunes, songs_played_lb)

songs_played_all <- select(songs_played_all, Artist, Name, Play.Count, Source) %>%
  group_by(Artist, Name, Source) %>%
  summarise(Play.Count = sum(Play.Count)) %>%
  dcast(Artist + Name ~ Source, sum)

songs_played_all <- mutate(songs_played_all, Play.Count=iTunes+ListenBrainz)

# Get various summaries of iTunes data
plays_by_artist_itunes <- summarise_by_artist(songs_played_itunes)
get_spotifyish_summary_long(songs_played_itunes, plays_by_artist_itunes, "iTunes")
plays_by_artist_group_itunes <- summarise_by_artist_grouped(plays_by_artist_itunes, artist_groupings)
get_spotifyish_summary_long(songs_played_itunes, plays_by_artist_group_itunes, "iTunes")

# Get various summaries of ListenBrainz data
plays_by_artist_lb <- summarise_by_artist(songs_played_lb)
get_spotifyish_summary_long(songs_played_lb, plays_by_artist_lb, "ListenBrainz", include_time = FALSE)
plays_by_artist_group_lb <- summarise_by_artist_grouped(plays_by_artist_lb, artist_groupings, include_time = FALSE)
get_spotifyish_summary_long(songs_played_lb, plays_by_artist_group_lb, "ListenBrainz", include_time = FALSE)

plays_by_artist_all <- summarise_by_artist(songs_played_all, include_time = FALSE)
get_spotifyish_summary_long(songs_played_all, plays_by_artist_all, "iTunes and ListenBrainz", include_time = FALSE)
plays_by_artist_group <- summarise_by_artist_grouped(plays_by_artist_all, artist_groupings, include_time = FALSE)
get_spotifyish_summary_long(songs_played_all, plays_by_artist_group, "iTunes and ListenBrainz", include_time = FALSE)

# Look at total play count for top artists
plays_by_artist_group %>% select(Artist, total_plays) %>% arrange(-total_plays) %>% .[1:10,]
# Look at total play count for top songs
songs_played_all %>% select(Artist, Name, Play.Count) %>% arrange(-Play.Count) %>% .[1:10,]