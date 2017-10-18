Python <- function(country){
  code <- (read.csv("Places.csv") %>% filter(Country_Name_Fixed==country))$CountryCode
  time_zone <- (read.csv("Places.csv") %>% filter(Country_Name_Fixed==country))$Time.Zone
  l <- list()
  for (i in 1:2){
    da <- sprintf("%02d", i)
    fi <- paste0("/Users/db2175/pypi.downloads/06", da, ".csv.gz")
    l[[i]] <- fread(paste0("gunzip -c ", fi, " | grep ^", code), header=F)
  }
  all <- rbindlist(l)
  setnames(all, c("CountryCode", "UTC_Time"))
  all[, `:=`(UTC_Time=substr(UTC_Time, 1, 16))]
  utc.dt <- as.POSIXct(strptime(paste(all$UTC_Date, all$UTC_Time), 
                                  format="%Y-%m-%d %H:%M", tz="UTC"))
  attributes(utc.dt)$tzone <- time_zone
  all$Local_DateTime <- utc.dt
  browser()
}