library(dplyr)
library(rvest)
library(RCurl)
options(stringsAsFactors=F)

Scrape <- function(country, year){
  url <- paste0("http://www.officeholidays.com/countries/", tolower(country), "/", year, ".php")
  if (!url.exists(url)){
    return(NULL)
  }
  html <- read_html(url)
  dates <- html_nodes(html, ".holiday td:nth-child(2)") %>% html_text()
  if (country=="USA" & year > 2013){
    node.num <- 4
  } else {
    node.num <- 3
  }
  names <- html_nodes(html, paste0(".holiday td:nth-child(", node.num, ")")) %>% html_text() %>% trimws()
  dates <- unlist(strsplit(dates, "\n") %>% lapply(function(x){x[2]})) %>% trimws()
  df <- data.frame(Names=names, Dates=dates)
}

ScrapeAll <- function(){
  countries <- setdiff(read.csv("Places.csv")$Country_Name_Holidays, NA)
  years <- 2012:2017
  for (country in countries){
    print(country)
    for (year in years){
      print(year)
      dir <- paste0("Holidays/", country, "/")
      f <- paste0(dir, year, ".csv")
      if (!file.exists(f)){
        df <- Scrape(country, year)
        print(df[1, ])
        dir.create(dir, showWarnings=F)
        if (!is.null(df)){
          write.csv(df, f, row.names=F)
        } else {
          print("NULL")
        }
      }
    }
  }
}