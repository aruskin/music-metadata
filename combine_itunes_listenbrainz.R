source('process_music_metadata.R')
library(reshape2)
library(stringr)

NEW_YEAR <- '2022'
MIN_LISTEN_DATE <- paste0(NEW_YEAR, '-01-01')
MAX_LISTEN_DATE <- paste0(NEW_YEAR, '-12-31')
CHARTS_BY_YEAR_FILEPATH <- 'charts_by_year.csv'

# These mappings are v tailored to my listening habits--for general use
# maybe better to upload a personalized mapping file or something
artist_groupings <- read_yaml('artist_groupings.yaml')

russian_name_mappings <- read.csv('russian_band_name_mappings.csv')

# Select files with iTunes and ListenBrainz data exports
songs_played_itunes <- format_itunes_data()
songs_played_lb <- format_listenbrainz_data(min_listen_date=MIN_LISTEN_DATE, 
                                            max_listen_date=MAX_LISTEN_DATE)
gc()

songs_played_itunes <- mutate(songs_played_itunes, Source='iTunes')
songs_played_lb  <- mutate(songs_played_lb , Source='ListenBrainz')

songs_played_all <- bind_rows(songs_played_itunes, songs_played_lb)

songs_played_all <- select(songs_played_all, Artist, Name, Play.Count, Source) %>%
  left_join(russian_name_mappings, by=c("Artist"="name_in")) %>%
  mutate(Artist = ifelse(is.na(name_out), Artist, name_out)) %>%
  mutate(Artist = str_to_lower(Artist),
         Name = str_to_lower(Name)) %>%
  group_by(Artist, Name, Source) %>%
  summarise(Play.Count = sum(Play.Count)) %>%
  dcast(Artist + Name ~ Source, sum) %>%
  mutate(Play.Count=iTunes+ListenBrainz)

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

# Compare to previous years' charts
charts_by_year <- read.csv(CHARTS_BY_YEAR_FILEPATH)

new_chart <- plays_by_artist_all %>%
  select(Artist, total_plays) %>%
  mutate(rank=min_rank(desc(total_plays)))

colnames(new_chart) <- c('artist', paste0('total_plays_', NEW_YEAR), paste0('rank_', NEW_YEAR))

charts_by_year <- left_join(charts_by_year, new_chart, by='artist')
write.csv(charts_by_year, CHARTS_BY_YEAR_FILEPATH, row.names = FALSE, na='')
