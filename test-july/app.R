devtools::install_github("AndersT123/betr")
library(betr)

league_games <- scrape_games("01-07-2020", "31-07-2020")
run_app(to_database = path.expand("home/anders/test-july"))