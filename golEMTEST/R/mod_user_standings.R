#' user_standings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_user_standings_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns("standings"))
  )
}
    
#' user_standings Server Function
#'
#' @noRd 
mod_user_standings_server <- function(input, output, session){
  ns <- session$ns
  dfs <- purrr::imap(user_games, ~{
    out <- .x %>% dplyr::mutate(user = !!user_names[.y])
    
  })
  df <- purrr::reduce(dfs, bind_rows)
  df <- dplyr::inner_join(df, league_games) %>% dplyr::mutate(points = purrr::pmap_int(., compute_game_score))
  df <- df %>% dplyr::group_by(user, date_time) %>% dplyr::summarise(points_on_days = sum(points, na.rm = T)) %>%
    # now only grouped by user
    dplyr::mutate(acc_points = cumsum(points_on_days)) %>% dplyr::ungroup()
  
  output$standings <- plotly::renderPlotly({
    p <- ggplot2::ggplot(df, ggplot2::aes(x = date_time, y = acc_points, color = user)) + 
      ggplot2::geom_line() + 
      ggplot2::geom_point(ggplot2::aes(text = glue::glue("Points game-day: {points_on_days}
                                 Date: {format(date_time, format = '%b-%d %H:%M')}
                                 User: {user}") )) + xlab(NULL)
    
    plotly::ggplotly(p, tooltip = "text")
  })
}
    
## To be copied in the UI
# mod_user_standings_ui("user_standings_ui_1")
    
## To be copied in the server
# callModule(mod_user_standings_server, "user_standings_ui_1")
