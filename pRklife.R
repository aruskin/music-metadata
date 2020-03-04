# Tested with R version 3.5.1
library(spotifyr) #2.1.1
library(dplyr) #0.8.3
library(ggplot2) #3.1.0
library(ggfortify) #0.4.5
library(FNN) #1.1.2.2

dedupe_tracks <- function(data){
  data <- data %>%
    mutate(number=1) %>%
    group_by(artist, album_name, track_name) %>%
    mutate(copy_num = cumsum(number))
  data <- filter(data, copy_num==1)
  select(data, -one_of(c('number', 'copy_num')))
}

get_blur_and_co_data <- function(){
  keep_columns <- c('artist', 'album_name', 'album_release_year', 'track_name', 'track_uri',
                    'danceability', 'energy', 'speechiness', 'acousticness', 'instrumentalness',
                    'liveness', 'valence',
                    'key_mode', 'tempo')
  
  # Get song data for each artist, filter down to non-live albums
  blur <- get_artist_audio_features('blur') %>% 
    mutate(artist='Blur') %>%
    filter(album_name %in% c('13', 'Blur', 'Leisure', 
                                   'Modern Life Is Rubbish',
                                   'Parklife', 'The Great Escape',
                                   'The Magic Whip', 'Think Tank')) %>%
    select(one_of(keep_columns)) %>%
    dedupe_tracks
  graham <- get_artist_audio_features('graham coxon') %>%
    mutate(artist='Graham Coxon') %>%
    select(one_of(keep_columns)) %>%
    dedupe_tracks
  damon <- get_artist_audio_features('damon albarn') %>%
    mutate(artist='Damon Albarn') %>%
    filter(album_name != 'Dr Dee [Album Sampler] ') %>%
    select(one_of(keep_columns)) %>%
    dedupe_tracks
  gorillaz <- get_artist_audio_features('gorillaz') %>%
    mutate(artist='Gorillaz') %>%
    filter(album_name != 'Demon Days Live at the Manchester Opera House') %>%
    select(one_of(keep_columns)) %>%
    dedupe_tracks
  gbq <- get_artist_audio_features('the good the bad and the queen') %>%
    mutate(artist='The Good, The Bad & The Queen') %>%
    select(one_of(keep_columns)) %>%
    dedupe_tracks
  # Merge datasets
  blur_and_co <- union(blur, graham) %>%
    union(damon) %>%
    union(gorillaz) %>%
    union(gbq) %>%
    ungroup
  
  # Get mappings from key to Camelot number
  camelot <- read.csv("camelot_mappings.csv")
  camelot$key_mode <- paste0(camelot$key, camelot$mode)
  blur_and_co <- left_join(blur_and_co, camelot, by='key_mode')
  blur_and_co
}

numeric_features <- c('danceability', 'energy', 'speechiness', 
                      'acousticness', 'instrumentalness', 
                      'liveness', 'valence')

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

###################################
# text file with SPOTIFY_CLIENT_ID and SPOTIFY_CLIENT_SECRET
readRenviron("pRklife/.Renviron")

access_token <- get_spotify_access_token()
blur_and_co <- get_blur_and_co_data()

# Start by putting Blur songs into clusters
set.seed(123)
# But how many clusters???
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- blur_and_co %>% filter(artist=="Blur") %>% select(one_of(numeric_features))
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
plot(wss)

set.seed(135)
blur_clusters <- kmeans(data, 6)
# Check out the cluster centers
blur_clusters$centers
# these actually seem like pretty legit clusters
# We have a Sad, Poignant Blur cluster (low energy, low valence, high acousticness)
# I'm partial to the high energy and high valence (aka bangers) clusters

# Assign all of the other songs to the nearest cluster center
pred.knn <- get.knnx(blur_clusters$center, 
                                 blur_and_co %>% select(numeric_features), 1)$nn.index[,1]
blur_and_co$cluster <- pred.knn %>% as.factor()

# Save data and assigned clusters in a CSV
write.csv(blur_and_co, row.names = FALSE, 
          file = 'blur_and_co_spotify_data_clustered.csv')

# Visualize clusters
autoplot(prcomp(blur_and_co %>% 
                  select(one_of(numeric_features))), 
         data = blur_and_co, colour = 'artist', shape='cluster', size=3, 
         loadings=TRUE, loadings.label = TRUE, loadings.colour='black',
         loadings.label.size = 3)

cluster_summary <- blur_and_co %>% group_by(cluster) %>%
  summarise(danceability=mean(danceability),
            energy=mean(energy),
            valence=mean(valence),
            instrumentalness=mean(instrumentalness),
            acousticness=mean(acousticness),
            pct_gorillaz=sum(artist=='Gorillaz')/sum(artist != 'Blur'),
            pct_graham=sum(artist=='Graham Coxon')/sum(artist != 'Blur'),
            pct_GBQ=sum(artist=='The Good, The Bad & The Queen')/sum(artist != 'Blur'))
  
######################################################
# Let's make a playlist of non-Blur songs that are (hopefully!)
# as banging as 'Girls & Boys'
banging_cluster <- blur_and_co[grep('Girls (and|&) Boys', 
                                    blur_and_co$track_name, 
                                    fixed=FALSE),]$cluster
test <- generate_playlist(blur_and_co %>% 
                            filter(artist != 'Blur' & 
                                     cluster==banging_cluster), 
                          20)
# Print out playlist
test %>% select(track_name, album_name, artist)

# Add playlist to my Spotify account
user_info <- get_my_profile()
spotify_playlist <- create_playlist(user_id=user_info$id, 
                                    name='test blur rec code')
add_tracks_to_playlist(playlist_id = spotify_playlist$id, 
                uris =  test$track_uri)
