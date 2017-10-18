source("NewUtilities.R")

CountrySums <- function(){
  places <- read.csv("Places.csv")
  l <- list()
  for (y in 2012:2017){
    summ.f <- paste0("/Users/db2175/CRAN.Downloads/AllTenMinuteFiltered", y, 
                    ".txt")
    l[[as.character(y)]] <- fread(summ.f)
  }
  l <- rbindlist(l)
  country_sum <- l[, .(Number=.N), by=.(Country_Name, country)] %>% arrange(desc(Number))
  write.csv(country_sum, "Data/CountrySums.csv", row.names=F)
}

SelectCountries <- function(){
  places <- read.csv("Places.csv")
  csum <- fread("Data/CountrySums.csv") %>% filter(!is.na(Country_Name))
}

PlotCountrySums <- function(){
  country_sum <- fread("Data/CountrySums.csv")
  library(rworldmap)
  joined <- joinCountryData2Map(country_sum, joinCode="ISO2", nameJoinColumn="country", nameCountryColumn="Country_Name", suggestForFailedCodes=T, verbose=F)
  #pdf("NumberOfDownloads.pdf")
  mapCountryData(joined, nameColumnToPlot = "Number", catMethod="pretty", numCats=10, mapTitle="Number of downloads from http://cran.rstudio.com (Oct. 1 2012 to Aug. 11 2017)")
}

PlotSelectedCountries <- function(){
  places <- read.csv("Places.csv")
  country_sum <- fread("Data/CountrySums.csv") %>% filter(Country_Name %in% places$Country_Name)
  library(rworldmap)
  country_sum <- filter(country_sum, country %in% filter(places, Selected)$CountryCode)
  country_sum$Selected <- T
  joined <- joinCountryData2Map(country_sum, joinCode="ISO2", nameJoinColumn="country", nameCountryColumn="Country_Name", suggestForFailedCodes=T, verbose=F)
  #pdf("NumberOfDownloads.pdf")
  mapCountryData(joined, nameColumnToPlot = "Selected", catMethod="pretty", numCats=5, mapTitle="50 countries with highest number of downloads", addLegend=F)
}

FirstDownloadFile <- function(){
  fread("gunzip -c /Users/db2175/CRAN.Downloads/2012/2012-10-01.csv.gz")
}

FixPlaces2 <- function(){
  places <- read.csv("Places.csv")
  csum <- fread("Data/CountrySums.csv")[, .(Country_Name, country)]
  places <- merge(places, csum)
  setnames(places, "country", "CountryCode")
  write.csv(places, "Places.csv", row.names=F)
}

FixPlaces <- function(){
  places <- read.csv("Places.csv")
  csum <- fread("Data/CountrySums.csv")
  places$Selected <- places$Country_Name %in% csum$Country_Name[1:51]
  write.csv(places, "Places.csv", row.names=F)
}