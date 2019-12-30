library(spotifyr)
# library(httpuv)
# library(lubridate)
 library(dplyr)
# library(httr)
# library(purrr)
# library(stringr)
library(tidyr)

#1 - Authentication

df_token <- read.csv(file = 'token.csv')

Sys.setenv(SPOTIFY_CLIENT_ID = df_token %>% filter(token_type == 'client_id') %>% select(value))
Sys.setenv(SPOTIFY_CLIENT_SECRET = df_token %>% filter(token_type == 'client_secret_id') %>% select(value))

auth_token <- get_spotify_access_token()

#2 - Input 

df_playlist_input <- read.csv(file = 'playlist_input.csv',stringsAsFactors = F, encoding = 'UTF-8')

#3 - Getting Playlist Details

df_playlist_details <- data.frame()

for(playlist_i in df_playlist_input$playlist_id){
  
  playlist_details_i <- get_playlist(playlist_id = playlist_i ,authorization = auth_token)
  
  df_playlist_details_i <- data.frame(
    playlist_id = c(playlist_i),
    playlist_name = c(playlist_details_i$name),
    playlist_owner = c(playlist_details_i$owner$display_name),
    playlist_followers = c(playlist_details_i$followers$total),
    playlist_track_count = c(playlist_details_i$tracks$total)
    )
  
  df_playlist_details <- rbind(df_playlist_details, df_playlist_details_i)

}

#4 - Getting Track Details

df_playlist_track_details <- data.frame()

for(playlist_i in df_playlist_details$playlist_id){
  
  offset_i = round(
    df_playlist_details %>% 
      filter(playlist_id == playlist_i) %>% 
      select(playlist_track_count)
    ,digits = -2)$playlist_track_count
  
  track_count_i <-  (df_playlist_details %>% 
    filter(playlist_id == playlist_i) %>% 
    select(playlist_track_count))$playlist_track_count
  
  if(track_count_i < offset_i){
    offset_i = offset_i - 100
  }

  for(offset_j in seq(0,round(offset_i,digits = -2),100))  {
    

    df_playlist_track_details_j<-    data.frame(get_playlist_tracks(playlist_id = playlist_i ,offset = offset_j, authorization = auth_token)) %>% 
                                       mutate(
                                         playlist_id = playlist_i
                                       ) %>% 
                                        unnest(track.artists) %>% 
                                      select(
                                        track_id = track.id,
                                        track_name = track.name,
                                        artist_id = id,
                                        artist_name = name,
                                        album_id = track.album.id, 
                                        album_name = track.album.id, 
                                        album_release_date = track.album.release_date, 
                                        album_track_ct = track.album.total_tracks,
                                        album_type = track.album.type, 
                                        playlist_id
                                        
                                      )

    
    df_playlist_track_details <- rbind(df_playlist_track_details, df_playlist_track_details_j)
  }
}

write.csv(df_playlist_track_details, file='playlist_track_details.csv')
