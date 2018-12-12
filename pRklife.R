# note: using dev version of this! certain aspects of code won't work with CRAN version
library(spotifyr)

library(dplyr)
library(ggplot2)
library(ggfortify)
library(FNN)

Sys.setenv(SPOTIFY_CLIENT_ID = .rs.askForPassword("Enter Spotify Client ID"))
Sys.setenv(SPOTIFY_CLIENT_SECRET = .rs.askForPassword("Enter Spotify Client Secret"))
my_username <- readline(prompt="Enter Spotify user ID:")

access_token <- get_spotify_access_token()

# Get song data
blur <- get_artist_audio_features('blur') %>% 
  mutate(artist='Blur')
graham <- get_artist_audio_features('graham coxon') %>%
  mutate(artist='Graham Coxon')
damon <- get_artist_audio_features('damon albarn') %>%
  mutate(artist='Damon Albarn')
gorillaz <- get_artist_audio_features('gorillaz') %>%
  mutate(artist='Gorillaz')
gbq <- get_artist_audio_features('the good the bad and the queen') %>%
  mutate(artist='The Good, The Bad & The Queen')

# Filter down to albums we want
blur <- filter(blur, album_name %in% c('13', 'Blur', 'Leisure', 
                                       'Modern Life Is Rubbish',
                                       'Parklife', 'The Great Escape',
                                       'The Magic Whip', 'Think Tank'))
# don't really want to deal with Dr Dee and Journey to the West bc..soundtracks?
damon <- filter(damon, album_name == 'Everyday Robots')
graham <- filter(graham, album_name != 'The End Of The F***ing World (Original Songs and Score)' )
gorillaz <- filter(gorillaz, album_name %in% c('Demon Days', 'Gorillaz', 'Humanz',
                                               'The Fall', 'Plastic Beach', 'The Now Now'))

# Merge datasets
blur_and_co <- union(blur, graham) %>%
  union(damon) %>%
  union(gorillaz) %>%
  union(gbq) %>%
  select(artist, album_name, album_release_year, track_name, track_uri,
         danceability, energy, speechiness, acousticness, instrumentalness,
         liveness, valence,
         key_mode, tempo)

# Get mappings from key to Camelot number
camelot <- read.csv("camelot_mappings.csv")
camelot$key_mode <- paste0(camelot$key, camelot$mode)
blur_and_co <- left_join(blur_and_co, camelot, by='key_mode')

numeric_features <- c('danceability', 'energy', 'speechiness', 
                      'acousticness', 'instrumentalness', 
                      'liveness', 'valence')

# Can we see any patterns here? idk
autoplot(prcomp(blur_and_co %>% 
                  select(one_of(numeric_features))), 
         data = blur_and_co, colour = 'artist', size=2, 
         loadings=TRUE, loadings.label = TRUE, loadings.colour='black',
         loadings.label.size = 3)


# Start by putting Blur songs into clusters
# But how many clusters???
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- blur %>% select(one_of(numeric_features))
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
plot(wss)
# so...5? sure

set.seed(135)
blur_clusters <- kmeans(data, 5)
blur$cluster <- blur_clusters$cluster %>% as.factor()
# these actually seem like pretty legit clusters
# We have a Sad, Poignant Blur cluster (low energy, low valence, high acousticness)
# I'm partial to the high energy and high valence (aka bangers) clusters

# Assign all of the other songs to the nearest cluster center
pred.knn <- get.knnx(blur_clusters$center, 
                                 blur_and_co %>% select(numeric_features), 1)$nn.index[,1]
blur_and_co$cluster <- pred.knn %>% as.factor()
write.csv(blur_and_co, row.names = FALSE, file = 'blur_and_co_spotify_data.csv')

autoplot(prcomp(blur_and_co %>% 
                  select(one_of(numeric_features))), 
         data = blur_and_co, colour = 'artist', shape='cluster', size=3, 
         loadings=TRUE, loadings.label = TRUE, loadings.colour='black',
         loadings.label.size = 3)

blur_and_co %>% group_by(cluster) %>%
  summarise(danceability=mean(danceability),
            energy=mean(energy),
            valence=mean(valence),
            instrumentalness=mean(instrumentalness),
            acousticness=mean(acousticness),
            pct_gorillaz=sum(artist=='Gorillaz')/sum(artist != 'Blur'),
            pct_graham=sum(artist=='Graham Coxon')/sum(artist != 'Blur'),
            pct_GBQ=sum(artist=='The Good, The Bad & The Queen')/sum(artist != 'Blur'))
  

# Function to generate playlist with 'harmonic mixing'
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

# Let's make a playlist of non-Blur songs that are (hopefully!)
# as banging as 'Girls & Boys'

banging_cluster <- blur_and_co[grep('Girls (and|&) Boys', blur_and_co$track_name, fixed=FALSE),]$cluster
test <- generate_playlist(blur_and_co %>% 
                            filter(artist != 'Blur' & cluster==banging_cluster), 
                          20)

test %>% select(track_name, album_name, artist)

spotify_playlist <- create_playlist(username=my_username, 
                                    playlist_name='test blur rec code')

add_to_playlist(username = my_username, 
                playlist_id = spotify_playlist$id, 
                tracks =  test$track_uri)


