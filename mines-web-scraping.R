## Ryan Elmore
## Scraping Talk
## October 2, 2020

## Load packages for scraping (rvest), data manipulation (dplyr), and 
##  cleaning (janitor)

library(rvest)
library(dplyr)
library(janitor)

## Use R like a web browser
url <- "http://www.hockey-reference.com/teams/COL/2020.html"
tb <- xml2::read_html(url) %>%
  rvest::html_table()
View(tb)

## A Better way
## Selector Gadget using CSS
tb_2 <- xml2::read_html(url) %>%
  rvest::html_node(css = '#roster') %>%
  rvest::html_table() 
View(tb_2)

## Selector Gadget using xpath
tb_2 <- xml2::read_html(url) %>%
  rvest::html_node(xpath = '//*[(@id = "roster")]') %>%
  rvest::html_table() 

## Obtaining the links
links <- xml2::read_html(url) %>%
  rvest::html_nodes(xpath = '//*[(@id = "roster")]//a') %>%
  rvest::html_attr('href')
links

## Append to the table defined above
tb_2 <- tb_2 %>% 
  dplyr::mutate(., player_links = links)
View(tb_2)

## Nathan MacKinnon Info
url <- paste("https://www.hockey-reference.com", links[22], sep = "")
mac_tb <- xml2::read_html(url) %>% 
  rvest::html_table() %>% 
  .[[1]]
View(mac_tb)

## Clean the data up a bit 
names(mac_tb) <- mac_tb[1, ]
mac_tb <- mac_tb %>% 
  janitor::clean_names() %>% 
  dplyr::filter(!(season %in% c("Season", "Career")))

## Game Logs
game_log_url <- gsub(".html", "/gamelog/2020", url)
mac_gl_tb <- xml2::read_html(game_log_url) %>% 
  rvest::html_table() %>% 
  .[[1]]


## Write a Function
get_nhl_roster <- function(year, team){
  url <- paste("http://www.hockey-reference.com/teams/", 
               team, "/", year, ".html", sep = "")
  tb <- xml2::read_html(url) %>%
    rvest::html_table() %>% 
    .[[3]]
  return(tb)
}

## Example
nyr_19_df <- get_nhl_roster(2019, "NYR")

## Iterate across several websites team/year combos
teams <- c("COL", "DAL", "NYR")
years <- 2015:2019
for(i in seq_along(teams)){
  for(j in seq_along(years)){
    tb <- get_nhl_roster(years[j], teams[i]) %>% 
      mutate(team = teams[i], 
             year = years[j])
    if(exists("results")){
      results <- rbind(results, tb)
    } else{
      results <- tb
    }
  }
}
View(results)

