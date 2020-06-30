## Show the user standings as a line graph over dates of played games with accumulating scores

# 1. combine user tables
# 2. Plot with ggplot
# 3. convert to plotly

user_standings_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    plotlyOutput(ns("standings"))
  )
}

user_standings <- function(input, output, session) {
  ## user_games is a list of tibbles 
  
  dfs <- imap(user_games, ~{
    out <- .x %>% mutate(user = !!user_names[.y])
    
  })
  df <- reduce(dfs, bind_rows)
  df <- inner_join(df, league_games) %>% mutate(points = pmap_int(., compute_game_score))
  df <- df %>% group_by(user, date_time) %>% summarise(points_on_days = sum(points, na.rm = T)) %>%
    # now only grouped by user
    mutate(acc_points = cumsum(points_on_days)) %>% ungroup()
  
  output$standings <- renderPlotly({
    p <- ggplot(df, aes(x = date_time, y = acc_points, color = user)) + 
      geom_line() + 
      geom_point(aes(text = glue("Points game-day: {points_on_days}
                                 Date: {format(date_time, format = '%b-%d %H:%M')}
                                 User: {user}") )) + xlab(NULL)
    
    ggplotly(p, tooltip = "text")
  })
  
}