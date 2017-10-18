shifted.day.times <- substr(times(seq(24/144, 1, by=1/144)), 1, 5)

ConvertTimeToShiftedOrder <- function(time){
  rr <- read.table("TimeDict4AM.txt", stringsAsFactors=F)
  time.dict <- as.character(rr$x)
  names(time.dict) <- rownames(rr)
  as.numeric(time.dict[time])
}

GetDataShifted <- function(country, years){
  info <- GetInfoForCountry(country_name=country) %>%
    filter(Local_Year %in% years)
  places <- read.csv("Places.csv")
  time_zone <- filter(places, Country_Name_Fixed==country)$Time.Zone
  dt <- as.POSIXct(info$Local_DateTime, tz=time_zone)
  cut.times <- as.POSIXct(paste0(min(years), "-01-01 04:00:00"), tz=time_zone) + c(0, seq(0:(365*length(years))) * (60*60*24))
  time.zone.code <- substr(format(cut.times[1], usetz=T), 21, 23)
  cut.times <- paste0(substr(cut.times, 1, 11), "04:00:00 ", time.zone.code)
  info$BIN <- cut(dt, breaks=as.POSIXct(cut.times, tz=time_zone), labels=F)
  info <- filter(info, !is.na(BIN))
  previous_day <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  names(previous_day) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  info <- group_by(info, BIN) %>% 
    mutate(Shifted_Weekday=ifelse(Local_Time[1] %in% shifted.day.times, Local_Weekday[1], 
                                  previous_day[Local_Weekday[1]])) %>%
    ungroup()
}

GetMeanDayProfile <- function(country, years, day_names, num.days){
  info <- GetDataShifted(country=country, years=years) %>% 
    filter(Shifted_Weekday %in% day_names)
  time.means <- group_by(info, Local_Time, Country_Name) %>%
    summarize(MeanActivity=MeanZeroFill(N, num.days))
  time.means$ShiftedOrder <- ConvertTimeToShiftedOrder(time.means$Local_Time)
  time.means <- arrange(time.means, as.numeric(ShiftedOrder))
}

GetAllProfiles <- function(){
  l <- list()
  places <- read.csv("Places.csv") %>% filter(!Time.Zone=="") %>% filter(Selected)
  for (cc in places$Country_Name_Fixed){
    print(cc)
    day_names <- c("Monday", "Tuesday", "Wednesday", "Thursday")
    if (cc %in% c("Israel", "Iran")){
      day_names <- c("Sunday", "Monday", "Tuesday", "Wednesday")
    }
    l[[paste0(cc)]] <- GetMeanDayProfile(
        country=cc, years=2014:2017, day_names=day_names, num.days=754)
  }
  all <- bind_rows(l)
  save(all, file="2014to17WeekdayProfiles.Rdata")
}

PlotAsians <- function(){
  load("2014to17WeekdayProfiles.Rdata")
  all <- mutate(all, NTime=as.numeric(times(paste0(Local_Time, ":00")))) %>% filter(Country_Name %in% c("China", "Hong_Kong", "Vietnam", "Turkey", "Colombia", "France")) %>% group_by(Country_Name) %>% mutate(MeanActivity=MeanActivity/sum(MeanActivity)) %>% ungroup() %>% mutate(Country_Name=gsub("_", " ", Country_Name))
  p <- ggplot(all, aes(x=ShiftedOrder, y=MeanActivity)) + 
    #scale_color_manual("", values=cbbPalette) + 
    geom_line() + 
    facet_wrap(~Country_Name, ncol=2)+
    theme_bw() + 
    xlab("") + 
    ylab("Normalized download activity") + 
    scale_x_continuous(breaks=c(1, 36, 72, 108), 
                       labels=c("4 AM", "10 AM", "4 PM", "10 PM")) 
  print(p)
  #ggsave("OtherProfiles.pdf", height=12, width=12)
}

PlotEuropeanLunchTimes <- function(){
  load("2014to17WeekdayProfiles.Rdata")
  all <- mutate(all, NTime=as.numeric(times(paste0(Local_Time, ":00")))) %>% filter(Country_Name %in% c("Italy", "Finland", "Germany", "Spain")) %>% group_by(Country_Name) %>% mutate(MeanActivity=MeanActivity/sum(MeanActivity)) 
  minima <- group_by(all, Country_Name) %>%
    mutate(MeanActivity=ifelse(ShiftedOrder>40 & ShiftedOrder<72, MeanActivity, Inf)) %>% 
    summarize(MinTime=ShiftedOrder[which.min(MeanActivity)])
  browser()
  p <- ggplot(all, aes(x=ShiftedOrder, y=MeanActivity)) + 
    #scale_color_manual("", values=cbbPalette) + 
    geom_line() + 
    facet_wrap(~Country_Name, ncol=1)+
    theme_bw() + 
    xlab("") + 
    ylab("Normalized download activity") + 
    scale_x_continuous(breaks=c(1, 36, 72, 108), 
                       labels=c("4 AM", "10 AM", "4 PM", "10 PM")) + 
    geom_vline(data=minima, aes(xintercept=MinTime))
  print(p)
  #ggsave("EuropeanLunchTimes.pdf", height=12, width=12)
}

#****************************************************************************




# PlotWeekdayProfiles <- function(){
#   load("2014to17WeekdayProfiles.Rdata")
#   all <- mutate(all, NTime=as.numeric(times(paste0(Local_Time, ":00"))))
#   p <- ggplot(all, aes(x=ShiftedOrder, y=MeanActivity)) + 
#        facet_wrap(~Country_Name, scales="free_y") +
#        geom_line() + 
#        theme_bw() + 
#        xlab("") + 
#        ylab("Mean number of downloads") + 
#        scale_x_continuous(breaks=c(1, 72), 
#                           labels=c("4 AM", "4 PM"))
#   ggsave("WeekdayProfiles.pdf", height=12, width=12)
# }
# 
# GetFits <- function(country, weekday, N=100, plot=F){
#   load("2016WeekdayProfiles.Rdata")
#   dat <- filter(all, Shifted_Weekday==weekday, Country_Name==country) %>% ungroup()
#   if (country=="UK"){
#     dat <- filter(dat, as.numeric(times(paste0(Local_Time, ":00"))) > 0.2 | 
#                     as.numeric(times(paste0(Local_Time, ":00"))) < 0.05)
#   }
#   fits <- PosteriorFromGamma(dat$MeanActivity, dat$ShiftedOrder, N=N) %>% mutate(Country=country)
#   if (plot){
#     plot.dat <- rename(dat, x=ShiftedOrder, Value=MeanActivity) %>%
#       mutate(Alpha=1, Type="Real", Rep="V-1") %>% select(x, Value, Alpha, Type, Rep)
#     fit.dat <- mutate(fits, Alpha=0.1, Type="Simulated") %>% select(-Country)
#     both.dat <- bind_rows(plot.dat, fit.dat)
#     br <- c(1, 37, 73, 109, 139)
#     p <- ggplot(both.dat, aes(x=x, y=Value, group=Rep, col=Type, alpha=I(Alpha))) + 
#       geom_line() + 
#       ylab("Activity") + 
#       scale_x_continuous(breaks=br, labels=ConvertShiftedOrderToTime(br)) + xlab("") + ggtitle(paste(country, weekday)) + scale_color_discrete("")
#     return(p)
#   }
#   return(fits)
# }
# 
# MMBS <- function(x){
#   len <- length(x)
#   m <- rep(0, len)
#   m[1] <- x[1]
#   m[len] <- x[len]
#   for (i in 2:(len-1)){
#     m[i] <- min(max(x[1:i]), max(x[i:len]))
#   }
#   m
# }
# 
# 
# PosteriorFromGamma <- function(yy, xx, N=100, family="Gamma"){
#   df <- data.frame(y=yy, x=xx)
#   library(mgcv, ggplot2, SemiPar)
#   #data(fossil, package = "SemiPar")
#   if (family=="Gamma"){
#     m <- gam(y ~ s(x, k = 50), family=Gamma(link=log), data = df, method = "REML")
#   } else {
#     m <- gam(y ~ s(x, k=50), data=df, method="REML")
#   }
#   rmvn <- function(n, mu, sig) { ## MVN random deviates
#     L <- mroot(sig)
#     m <- ncol(L)
#     t(mu + L %*% matrix(rnorm(m*n), m, n))
#   }
#   Vb <- vcov(m, unconditional=T)
#   newd <- with(df, data.frame(x = 1:144))
#   Cg <- predict(m, newd, type = "lpmatrix")
#   #resp <- predict(m, newd, type="response")
#   #return(resp)
#   set.seed(1)
#   sims <- rmvn(N, mu = coef(m), sig = Vb)
#   if (family=="Gamma"){
#     fits <- exp(Cg %*% t(sims))
#   } else {
#     fits <- Cg %*% t(sims)
#   }
#   fits <- as.data.frame(fits) %>% 
#     dplyr::mutate(x=newd$x) %>%
#     gather(Rep, Value, -x)
# }
# 
# MMBS <- function(x){
#   len <- length(x)
#   m <- rep(0, len)
#   m[1] <- x[1]
#   m[len] <- x[len]
#   for (i in 2:(len-1)){
#     m[i] <- min(max(x[1:i]), max(x[i:len]))
#   }
#   m
# }
# 
# 
# GetQuantiles <- function(x, quantiles){
#   cc <- cumsum(x)/sum(x)
#   out <- approx(cc, 1:144, quantiles)$y
#   l <- as.list(out)
#   names(l) <- paste0("Q", quantiles)
#   l
# }
# 
# # highest variance is Q0.05 and Q0.8
# AllCountriesInfo <- function(weekday){
#   N <- 10000
#   act <- list()
#   lunch <- list()
#   places <- read.csv("Places.csv") %>% filter(!Time.Zone=="")
#   if (F){
#     for (cc in places$Country_Name_Fixed){
#       print(cc)
#       ci <- CurveInformation(cc, weekday, N=N)
#       act[[cc]] <- ci$activity
#       lunch[[cc]] <- ci$lunch
#       print(lunch[[cc]])
#     }
#     save(act, lunch, file=paste0("ActLunch.", weekday, "Rdata"))
#   }
#   load(paste0("ActLunch.", weekday, "Rdata"))
#   quantile_names <- names(act[[1]])
#   all.act <- t(bind_rows(act))
#   colnames(all.act) <- quantile_names
#   all.lunch <- bind_rows(lunch)
#   all.lunch$Country <- places$Country_Name_Fixed
#   all.act <- as.data.frame(all.act)
#   all.act$Country <- rownames(all.act)
#   rownames(all.act) <- NULL
#   all.act <- all.act[, c("Country", "Q0.05", "Q0.8")]
#   all.act$Q0.05 <- ConvertShiftedOrderToTime(round(all.act$Q0.05))
#   all.act$Q0.8 <- ConvertShiftedOrderToTime(round(all.act$Q0.8))
#   all <- merge(all.act, all.lunch)
#   write.csv(all, "ActivityStatistics.csv", row.names=F)
# }
# 
# CurveInformation <- function(country, weekday, N){
#   fits <- GetFits(country, weekday, N=N)
#   # start at 3 AM, ratio to minima of maxima on both sides of lowest point in 68:98
#   fits.dt <- as.data.table(fits)
#   # 11:20    16:20
#   fits.dt[, `:=`(MinMaxBothSides=MMBS(Value)), by=Rep]
#   fits.dt[, `:=`(Ratio=Value/MinMaxBothSides), by=Rep]
#   lunch.dip <- fits.dt[x %in% 45:75, 
#                        .(WMIN=x[Ratio==min(Ratio)], RMIN=min(Ratio)), by=Rep] %>%
#     mutate(Country=country)
#   lunch <- summarize(lunch.dip, Dip_Time=ConvertShiftedOrderToTime(round(median(WMIN))),
#                      Dip_Time0.05=ConvertShiftedOrderToTime(round(quantile(WMIN, 0.05))), 
#                      Dip_Time0.95=ConvertShiftedOrderToTime(round(quantile(WMIN, 0.95))),
#                      Activity_Ratio=median(RMIN), 
#                      Activity_Ratio0.05=quantile(RMIN, 0.05), 
#                      Activity_Ratio0.95=quantile(RMIN, 0.95))
#   activity.dat <- fits.dt[, GetQuantiles(Value, seq(0.05, 0.95, by=0.05)), by=Rep] %>% as.data.frame() %>% select(-Rep) %>% as.matrix() %>% apply(2, median) 
#   list(lunch=lunch, activity=activity.dat, Country=country, Weekday=weekday)
# }
# 
# ConvertShiftedOrderToTime <- function(order){
#   rr <- read.table("TimeDict4AM.txt", stringsAsFactors=F)
#   time.dict <- rownames(rr)
#   names(time.dict) <- as.character(rr$x)
#   time.dict[as.character(order)]
# }
# 
# ClusterWeekdayProfiles <- function(){
#   load("2014to17WeekdayProfiles.Rdata")
#   spr <- ungroup(all) %>% spread(Country_Name, MeanActivity) %>% select(-Local_Time, -ShiftedOrder)
#   dists <- 1 - cor(spr)^2
#   dists <- dists[!rownames(dists)=="Hong_Kong", !colnames(dists)=="Hong_Kong"]
#   cc <- cmdscale(dists, k=2) %>% as.data.frame()
#   colnames(cc) <- c("X", "Y")
#   cc$Country <- gsub("_", " ", rownames(cc))
#   library(ggrepel)
#   p <- ggplot(cc, aes(x=X, y=Y, label=Country)) + 
#     geom_text_repel(size=3) + 
#     geom_point(color="red") + theme_bw() + xlab("") + ylab("")
#   browser()
# }