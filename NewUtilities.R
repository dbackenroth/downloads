library(data.table)
library(dplyr)
library(chron)
library(tidyr)
library(gplots)
library(tibble)
options(stringsAsFactors=F)
options(scipen=20)
library(countrycode)

days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

month.names <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

working.days <- days[1:5]
weekend.days <- days[6:7]

MeanZeroFill <- function(x, len){
  mean(c(x, rep(0, len))[1:len])
}

Heatmap <- function(df, timeunit, timeunit.order){
  setnames(df, timeunit, "TimeUnit")
  sp <- spread(df, Country, Effect)
  rownames(sp) <- sp$TimeUnit
  sp <- select(sp, -TimeUnit)
  sp.matrix <- as.matrix(sp)
  rownames(sp.matrix) <- rownames(sp)
  sp.matrix <- sp.matrix[timeunit.order, ]
  colnames(sp.matrix) <- gsub("_", " ", colnames(sp.matrix))
  par(mar=c(7, 3, 3, 7))
  heatmap.2(sp.matrix, dendrogram="column", trace="none", col="redblue", Rowv=F, density.info="none", key.xlab="Log(change in activity)", key.title="", cexRow=0.9, adjCol=c(NA, .5), cexCol=0.9, margins=c(7,7))
}

# df should have column Country_Name 
# colname says which column to plot
PlotMap <- function(df, colname, title){
  places <- read.csv("Places.csv") %>% rename(country=CountryCode) %>% select(Country_Name_Fixed, country)
  df <- merge(df, places)
  library(rworldmap)
  joined <- joinCountryData2Map(df, joinCode="ISO2", nameJoinColumn="country")
  mapCountryData(joined, nameColumnToPlot = colname, catMethod="pretty", 
                 numCats=20, mapTitle=title)
}

GetInfoForCountry <- function(country_name_fixed="France", years=2012:2017, new=F){
  f <- paste0("CountryData/", country_name_fixed, ".csv.gz")
  if (!file.exists(f) | new){
    places <- read.csv("Places.csv")
    time_zone <- filter(places, Country_Name_Fixed==country_name_fixed)$Time.Zone
    country_name <- filter(places, Country_Name_Fixed==country_name_fixed)$Country_Name
    if (country_name=="Ivory Coast") country_name <- "Côte D'Ivoire" 
    l <- list()
    for (y in years){
      l[[y]] <- fread(
        paste0("/Users/db2175/CRAN.Downloads/AllTenMinuteFiltered", y, 
                             ".txt")) %>% 
        filter(Country_Name==country_name)
    }
    all <- bind_rows(l)
    utc.dt <- as.POSIXct(strptime(paste(all$UTC_Date, all$UTC_Time), 
                                  format="%Y-%m-%d %H:%M", tz="UTC"))
    attributes(utc.dt)$tzone <- time_zone
    all$Local_DateTime <- utc.dt
    all$Local_Year <- as.numeric(format(utc.dt, format="%Y"))
    all$Local_Month <- as.numeric(format(utc.dt, format="%m"))
    all$Local_Day <- as.numeric(format(utc.dt, format="%d"))
    all$Local_Weekday <- weekdays(as.Date(utc.dt, tz=time_zone))
    all$Local_Time <- format(utc.dt, format="%H:%M")
    all$Local_DayNumber <- as.numeric(format(utc.dt, format="%j"))
    all$Country_Name <- country_name_fixed
    unzipped <- gsub(".gz", "", f)
    write.csv(all, unzipped, row.names=F)
    system(paste0("gzip ", unzipped))
  } else {
    all <- fread(paste0("gunzip -c ", f))
  }
  all
}

DoAllCountries <- function(){
  places <- read.csv("Places.csv") %>% filter(!Time.Zone=="")
  for (cc in places$Country_Name_Fixed){
    print(cc)
    GetInfoForCountry(country_name_fixed=cc)
  }
}

WriteTenMinuteInfo <- function(year){
  # filtered is after filtering on r_version and not RcppArmadillo and Germany
  out.f <- paste0("/Users/db2175/CRAN.Downloads/AllTenMinuteFiltered", year, 
                    ".txt")
  WriteSummary(out.f, year)
}

WriteSummary <- function(out.f, year){
  unlink(out.f)
  first <- T
  for (month in 1:12){
    cat("Month", month, "\n")
    mo <- sprintf("%02d", month)
    for (day in 1:31){
      cat(day)
      da <- sprintf("%02d", day)
      fi <- paste0("/Users/db2175/CRAN.Downloads/", year, "/",
                   year, "-", mo, "-", da, ".csv.gz")
      if (file.exists(fi)){
        time.info <- fread(paste0("gunzip -c ", fi)) %>% as.data.frame() %>%
          mutate(UTC_StartMinute=10*floor(as.numeric(times(time)) * 6 * 24)) %>% filter(!is.na(r_version), !(country=="DE" & package=="RcppArmadillo"))
        # spurious RcppArmadillo downloads cause a strange bump for Germany in April-May-June 2016; downloads with is.na(r_version) cause a strange bump for France for January, Feburary, March 2015
        # select one download in each ten-minute window for each user
        time.info <- group_by(time.info, ip_id, UTC_StartMinute) %>%
            summarize(date=date[1], country=country[1])
        summ <- group_by(time.info, date, UTC_StartMinute, country) %>%
          summarize(N=n()) %>% ungroup() %>%
          mutate(UTC_StartMinute=as.numeric(UTC_StartMinute),
                 Country_Name=countrycode(country, origin="iso2c", 
                                          destination="country.name"))
        summ <- mutate(summ, UTC_Time=
                         paste0(sprintf("%02d", floor(UTC_StartMinute/60)), ":",
                                sprintf("%02d", UTC_StartMinute %% 60))) %>%
          select(-UTC_StartMinute) %>%
          rename(UTC_Date=date)
        write.table(summ, file=out.f, row.names=F, quote=F, sep="\t", append=T, col.names=first)
        if (first) first <- F
      }
    }
  }
}

#*******************************************************************************



# # adds structural zeros
# ProcessSummary <- function(sample.one, year, python=F){
#   if (!sample.one){
#     in.f <- paste0("/Users/db2175/CRAN.Downloads/All", year, ".txt")
#     out.f <- paste0("/Users/db2175/CRAN.Downloads/All", year, "WithZeros.txt")
#   } else {
#     in.f <- paste0("/Users/db2175/CRAN.Downloads/All", year, ".sampleone.txt")
#     out.f <- paste0("/Users/db2175/CRAN.Downloads/All", year, "WithZeros.sampleone.txt")
#   }
#   if (python){
#     in.f <- "/Users/db2175/pypi.downloads/Pythondat.txt"
#     out.f <- "/Users/db2175/pypi.downloads/Pythondatwithzeros.txt"
#   }
#   all <- fread(in.f)
#   if ("Time" %in% colnames(all)) all <- select(all, -Time)
#   all <- spread(all, UTC_Min, N, fill=0) %>%
#     gather(UTC_Min, N, -date, -country, -Weekday) %>%
#     mutate(Country_Name=countrycode(country, origin="iso2c", 
#                                     destination="country.name")) %>%
#     mutate(Continent=countrycode(country, origin="iso2c", 
#                                  destination="continent")) %>%
#     filter(!is.na(Country_Name)) %>%
#     mutate(UTC_Min=as.numeric(UTC_Min))
#   
#   all$Time <- as.character(times(all$UTC_Min / 144))
#   write.table(all, file=out.f, row.names=F, sep="\t", quote=F)
# }

# CombinePythonFiles <- function(){
#   l <- list()
#   out.f <- "/Users/db2175/pypi.downloads/Pythondat.txt"
#   first <- T
#   for (day in 1:30){
#     cat(day)
#     da <- sprintf("%02d", day)
#     fi <- paste0("/Users/db2175/pypi.downloads/06", da, ".csv.gz")
#     time.info <- fread(paste0("gunzip -c ", fi))[country_code %in% 
#                                                    c("DE", "ES", "FR", "GB", "IT", "PL")]
#     time.info[, `:=`(time=substr(timestamp, 12, 19))]
#     time.info[, `:=`(date=substr(timestamp, 1, 10))]
#     time.info <- mutate(time.info, 
#         UTC_Min=floor(as.numeric(times(time)) * 6 * 24))
#     filter.function <- function(df) {
#       group_by(df, date, UTC_Min, country_code) %>%
#         summarize(N=n())
#     }
#     summ <- filter.function(time.info) %>%
#          mutate(Weekday=factor(weekdays(as.Date(date)), levels=days))
#     summ <- rename(summ, country=country_code)
#     write.table(summ, file=out.f, row.names=F, quote=F, sep="\t", append=T, col.names=first)
#     if (first) first <- F
#   }
# }
# 
# MedianZeroFill <- function(x, len){
#   median(c(x, rep(0, len))[1:len])
# }


# GetDownloadsForCountry <- function(year, country_code){
#   l <- list()
#   for (month in 1:12){
#     mo <- sprintf("%02d", month)
#     for (day in 1:31){
#       cat(day)
#       da <- sprintf("%02d", day)
#       fi <- paste0("/Users/db2175/CRAN.Downloads/", year, "/",
#                  year, "-", mo, "-", da, ".csv.gz")
#       if (file.exists(fi)){
#         l[[paste(month, day)]] <- 
#           fread(paste0("gunzip -c ", fi)) %>% 
#           filter(country==country_code) %>%
#           mutate(Month=month, Day=day)
#       }
#     }
#   }
#   all <- bind_rows(l)
#   save(all, file="TempGB2016.Rdata")
#   #save(all, file="TempFrance2016.Rdata")
#   #rosna <- group_by(all, date) %>% summarize(RVISNA=mean(is.na(r_version)), ROSNA=mean(is.na(r_os)), RARCHNA=mean(is.na(r_arch)))
#   #plot(rosna$RVISNA)
#   #browser()
# }
# 


# WeekdaySum <- function(country_name, years){
#   info <- GetInfoForCountry(country_name=country_name) %>% filter(Local_Year %in% years)
#   weekday.sum <- group_by(info, Local_Weekday) %>%
#     summarize(Sum=sum(N)) %>% 
#     mutate(Local_Weekday=factor(Local_Weekday, levels=days)) %>%
#     arrange(Local_Weekday)
# }
# 
# AllWeekdaySums <- function(year){
#   places <- read.csv("Places.csv") %>% filter(!Time.Zone=="")
#   l <- list()
#   for (cnf in places$Country_Name_Fixed){
#     l[[cnf]] <- WeekdaySum(cnf, year) %>% mutate(Country=cnf)
#   }
#   all <- bind_rows(l)
#   day.props <- group_by(all, Country) %>%
#     mutate(Prop=Sum/sum(Sum))
#   spreaded <- select(day.props, -Sum) %>% 
#     spread(Country, Prop) 
#   weekdays <- spreaded$Local_Weekday
#   spreaded <- spreaded %>% select(-Local_Weekday) %>% as.matrix()
#   rownames(spreaded) <- weekdays
#   pal <- colorRampPalette(c("white", "blue"))(n = 299)
#   pdf(paste0("Weekday", year, ".pdf"), height=5, width=8)
#   heatmap.2(spreaded, Rowv=F, dendrogram="column",scale="none", trace="none", key=F, keysize=0.25, margins=c(6,8),col=pal, adjCol=0.9)
#   dev.off()
# }
# 
# WorkByMonth <- function(country_name="France", years=2013:2016){
#   info <- GetInfoForCountry(country_name=country_name) %>% filter(Local_Year %in% years)
#   summ <- group_by(info, Local_Year, Local_Month) %>%
#     summarize(S=sum(N)) %>% mutate(Country=country_name)
# }
# 
# AllWorkByMonth <- function(){
#   places <- read.csv("Places.csv") %>% filter(!Time.Zone=="")
#   l <- list()
#   for (cnf in places$Country_Name_Fixed){
#     l[[cnf]] <- WorkByMonth(country_name=cnf, years=2013:2016)
#   }
#   all <- bind_rows(l)
#   ggplot(all, aes(x=Local_Month, y=S, col=as.factor(Local_Year))) + facet_wrap(~Country, scales="free") + geom_line() + scale_color_manual("", values=cbbPalette) + theme_bw() + scale_x_continuous(breaks=c(1,4,7,10), labels=c("Ja", "Ap", "Ju", "Oc")) + xlab("Month") + ylab("Number of downloads per month")
#   ggsave("ByMonth.pdf", width=12, height=12)
# }

# see DailyPatternsAnalysis.R
# GetAllProfiles <- function(){
#   l <- list()
#   places <- read.csv("Places.csv") %>% filter(!Time.Zone=="") %>% filter(Selected)
#   for (cc in places$Country_Name_Fixed){
#     print(cc)
#     day_names <- c("Monday", "Tuesday", "Wednesday", "Thursday")
#     if (cc %in% c("Israel", "Iran")){
#         day_names <- c("Sunday", "Monday", "Tuesday", "Wednesday")
#     }
#       # merging.data <- read.csv("MergingData.csv")
#       # sum(merging.data$Local_Year %in% c(2015:2017) & merging.data$Local_Weekday %in% c("Monday", "Tuesday", "Wednesday", "Thursday"))
#     l[[paste0(cc)]] <- GetMeanDayProfile(
#         country_name=cc, years=2015:2017, day_names=day_names, num.days=545)
#   }
#   all <- bind_rows(l)
#   browser()
#   all <- mutate(all, NTime=as.numeric(times(paste0(Local_Time, ":00"))))
#   ggplot(all, aes(x=NTime, y=MeanActivity)) + 
#     facet_wrap(~Country_Name, scales="free_y") +
#     geom_line() + 
#     theme_bw() + 
#     xlab("") + 
#     ylab("Mean number of downloads") + 
#     scale_x_continuous(breaks=c(0.25, 0.75), 
#                        labels=c("6 AM", "6 PM"))
#   ggsave("WeekdayProfiles.pdf", height=12, width=12)
#   
#   
#   
#   all
# }
# 
# GetMeanDayProfile <- function(country_name="France", years, day_names="Monday", num.days){
#   info <- GetInfoForCountry(country_name=country_name) %>% 
#     filter(Local_Year %in% years, Local_Weekday %in% day_names)
#   #num.days <- length(unique(paste(info$Local_Year, info$Local_Month, info$Local_Day)))
#   time.medians <- group_by(info, Local_Time, Country_Name) %>%
#     summarize(MeanActivity=MeanZeroFill(N, num.days))
# }