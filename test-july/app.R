library(betr)

league_games <- scrape_games("01-07-2020", "31-07-2020")
run_app(to_database = path.expand("~/leagues/test-july"))