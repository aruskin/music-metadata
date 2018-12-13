library(shiny)
library(shinyalert)

source("global.R")

ui <- fluidPage(
  # Application title
  useShinyalert(),
  titlePanel("pRklife"),
  h4(paste("Help! I'm still nor over Blur's dissolution & I'm intimidated",
           "by the scope of Damon Albarn & Graham Coxon's non-Blur material.")),
  h4("Where do I start?"),
  br(),
  h4("Pick your top Blur tracks:"),
  fluidRow(
    column(3,
           wellPanel(
             selectizeInput('song1', 'Song 1', choices=c('Choose:', blur_songs)),
             h5('Top 3 Most Similar:'),
             tableOutput('out1')
           )),
    column(3,
           wellPanel(
             selectizeInput('song2', 'Song 2', choices=c('Choose:', blur_songs)),
             h5('Top 3 Most Similar:'),
             tableOutput('out2')
           )),
    column(3,
           wellPanel(
             selectizeInput('song3', 'Song 3', choices=c('Choose:', blur_songs)),
             h5('Top 3 Most Similar:'),
             tableOutput('out3')
           )),
    column(3,
           wellPanel(
             selectizeInput('song4', 'Song 4',  choices=c('Choose:', blur_songs)),
             h5('Top 3 Most Similar:'),
             tableOutput('out4')
           ))
  ),
  br(),
  h4("Make me a playlist based on these tracks:"),
  fluidRow(
    column(2, 
           actionButton('generatePlaylist', 'Generate Playlist')
           # ,br(),
           # actionButton('makeOnSpotify', 'Add to Spotify')
           ),
    column(10, tableOutput('playlist'))
  )
)

server <- function(input, output, session){
  output$out1 <- renderTable(get_top_3_closest_table(input$song1))
  output$out2 <- renderTable({
    if(input$song2 == 'Song 2') showNotification('lol')
    get_top_3_closest_table(input$song2)
  })
  output$out3 <- renderTable(get_top_3_closest_table(input$song3))
  output$out4 <- renderTable(get_top_3_closest_table(input$song4))
  
  genPlaylist <- eventReactive(input$generatePlaylist, {
    song1 <- input$song1
    song2 <- input$song2
    song3 <- input$song3
    song4 <- input$song4
    # should probably do some checks here to make sure user has actually chosen songs
    # do we care if they have the same song chosen more than once? idk
    my_fave_songs <- c(song1, song2, song3, song4)
   
    grab_songs <- filter(blur_data, artist=='Blur' & track_name %in% my_fave_songs) 
    
    # get set of 15 closest non-Blur songs for each of the selected Blur tracks,
    # deduplicate set, and generate harmonically mixed playlist of 20 songs from
    # that pool
    recs <- get_n_closest_songs(song=grab_songs, 
                                song_set=blur_data %>% filter(artist != 'Blur'),
                                15, numeric_features) %>%
      unique
    generate_playlist(recs, min(20, nrow(recs)))
  }
  )
  
  # Display plalylist as table--eventually give user option to automatically
  # port it over to Spotify
  output$playlist <- renderTable({
    my_playlist <- genPlaylist()
    my_playlist %>% select(artist, track_name, album_name)
  })
}

shinyApp(ui, server)