source("NewUtilities.R")
library(scam)
library(mclust)
library(ggplot2)
options(stringsAsFactors=F)

# save all fits, 
# effect of periodic basis,
# where are the big residuals--do they line up with holidays?
# do we need a separate factor for each of the holidays?  
# make Rmd document
# 

GetModelData <- function(type){
  places <- read.csv("Places.csv") %>% filter(Selected)
  weekday.effects <- list()
  dayofyear.effects <- list()
  for (cn in places$Country_Name_Fixed){
    print(cn)
    dat <- GetRegressionData(cn)
    fit <- DoRegression(cn, type)
    terms <- GetTerms(dat, fit)
    all <- cbind(dat, terms)
    weekday.effect <- unique(all[1:100, c("Local_Weekday", "as.factor(Local_Weekday)")])
    setnames(weekday.effect, c("Weekday", "Effect"))
    weekday.effect$Country <- cn
    dayofyear.effect <- unique(all[1:365, c("Local_DayNumber", "s(Local_DayNumber)")])
    setnames(dayofyear.effect, c("Local_DayNumber", "Effect"))
    dayofyear.effect$Country <- cn
    weekday.effects[[cn]] <- weekday.effect
    dayofyear.effects[[cn]] <- dayofyear.effect
  }
  df.weekday.effects <- rbindlist(weekday.effects)
  df.dayofyear.effects <- rbindlist(dayofyear.effects)
  write.csv(df.weekday.effects, "scam/ModelFits/WeekdayEffects.csv", row.names=F)
  write.csv(df.dayofyear.effects, "scam/ModelFits/DayOfYearEffects.csv", row.names=F)
}

DoAllRegressions <- function(type){
  places <- read.csv("Places.csv") %>% filter(Selected)
  for (cn in places$Country_Name_Fixed){
    if (cn=="Nigeria") next
    print(cn)
    DoRegression(cn, type)
  }
}

DoRegression <- function(Country, type){
  if (type=="simple"){
    mod.formula <- 'Activity ~ s(DaysFromStart, k=5, bs="mpi") + as.factor(Local_Weekday) + s(Local_Month, k=5) + as.factor(WeekdayHoliday)'
    save.dir <- "scam/ModelFits/mod5bfs_monthfactor_monotone/"
  }
  if (type=="complex0"){
    mod.formula <- 'Activity ~ s(DaysFromStart, k=5, bs="mpi") + as.factor(Local_Weekday) + s(Local_DayNumber, k=100, bs="cc")'
    save.dir <- "scam/ModelFits/mod100bfs_monotone_noholiday/"
  }
  if (type=="complex0.5"){
    mod.formula <- 'Activity ~ s(DaysFromStart, k=5, bs="mpi") + as.factor(Local_Weekday) + s(Local_DayNumber, k=100, bs="cc") + as.factor(WeekdayHoliday)'
    save.dir <- "scam/ModelFits/mod100bfs_monotone_holiday/"
  }
  if (type=="complex1"){
    mod.formula <- 'Activity ~ s(DaysFromStart, k=5, bs="mpi") + as.factor(Local_Weekday) + s(Local_DayNumber, k=25, bs="cc") + as.factor(WeekdayHoliday)'
    save.dir <- "scam/ModelFits/mod25bfs_monotone/"
  }
  if (type=="complex2"){
    mod.formula <- 'Activity ~ s(DaysFromStart, k=50, bs="mpi") + as.factor(Local_Weekday) + s(Local_DayNumber, k=100, bs="cc") + as.factor(WeekdayHoliday)'
    save.dir <- "scam/ModelFits/mod100bfs_fromstart_50_monotone/"
  }
  dir.create(save.dir, showWarnings=F)
  save.file <- paste0(save.dir, Country, ".Rdata")
  if (!file.exists(save.file)){
    dat <- GetRegressionData(Country)
  }
  fit <- FitSaveOrLoad(file=paste0(save.dir, Country, ".Rdata"), data=dat, 
                       formula=mod.formula)
}

FitSaveOrLoad <- function(file, data, formula=NULL){
  if (!file.exists(file)){
    fs.formula <- with(data, as.formula(formula))
    fit <- scam(fs.formula, family=Gamma(link="log"), data=data)
    save(fit, file=file)
  } else {
    load(file)
  }
  fit
}

GetPredictions <- function(data, fit){
  predict(fit, newdata=data, type="response")
}

GetTerms <- function(data, fit){
  predict(fit, newdata=data, type="terms")
}

# make data.frame with columns Date (1/1/2013) and Type (holiday or bridge)
ProcessHolidays <- function(Country){
  l <- list() 
  for (year in 2013:2017){
    f <- paste0("Holidays/", Country, "/", year, ".csv")
    if (file.exists(f)){
     l[[year]] <- read.csv(f) %>% mutate(Dates=paste0(Dates, ", ", year), 
                                         Year=year)
   }
  }
  if (length(l)==0) return(NULL)
  l <- bind_rows(l)
  l$Dates <- as.Date(l$Dates, format="%B %d, %Y")
  l$Weekday <- weekdays(l$Dates)
  thursdays <- filter(l, Weekday=="Thursday")
  bridge.fridays <- thursdays$Dates + 1
  tuesdays <- filter(l, Weekday=="Tuesday")
  bridge.mondays <- tuesdays$Dates - 1
  df <- data.frame(Date=c(l$Dates, bridge.fridays, bridge.mondays), 
                   Name=c(l$Names, 
                          paste0(thursdays$Names, "_bridge"),
                          paste0(tuesdays$Names, "_bridge")),
                   Type=c(rep("holiday", length(l$Dates)), 
                          rep("bridge", length(bridge.mondays)+length(bridge.fridays))))
  df$Date <- paste0(as.numeric(format(df$Date, format="%m")), "/",
                    as.numeric(format(df$Date, format="%d")), "/",
                    format(df$Date, format="%Y"))
  df$Year <- format(as.Date(df$Date, format="%m/%d/%Y"), format="%Y")
  df
}

# used to add structural zeros for countries with missing days
MakeMergingData <- function(){
  US <- GetRegressionData("USA") %>% select(Local_Year, Local_Day, Local_DayNumber, Local_Month, Local_Weekday)
  US$Activity <- 0.1
  write.csv(US, "MergingData.csv", row.names=F)
}

GetRegressionData <- function(Country){
  info <- GetInfoForCountry(Country)
  day.summ <- group_by(info, Local_Year, Local_Day, Local_DayNumber, 
                       Local_Month, Local_Weekday) %>%
    summarize(Activity=sum(N)) %>% arrange(Local_Year, Local_Month, Local_Day)
  
  day.summ <- mutate(day.summ, Code=paste(Local_Year, Local_Month, Local_Day)) %>% ungroup()
  #browser()
  merging.data <- read.csv("MergingData.csv") %>% mutate(Code=paste(Local_Year, Local_Month, Local_Day)) %>% filter(!Code %in% day.summ$Code)
  day.summ <- rbind(day.summ, merging.data)
  day.order <- day.summ$Local_Year + day.summ$Local_DayNumber/400
  day.summ$DaysFromStart <- rank(day.order)
  day.summ <- arrange(day.summ, DaysFromStart)
  #browser()
  day.summ <- mutate(day.summ, 
                     date=paste(Local_Month, Local_Day, Local_Year, sep="/"))
  hol.info <- ProcessHolidays(Country)
  if (!is.null(hol.info)){
    holidays <- filter(hol.info, Type=="holiday")$Date
    bridge.days <- filter(hol.info, Type=="bridge")$Date
   #%>%
    day.summ <- mutate(day.summ, Bridge=date %in% bridge.days)
    day.summ$WeekdayHoliday <- day.summ$date %in% holidays & day.summ$Local_Weekday %in% working.days
    day.summ$WeekendHoliday <- day.summ$date %in% holidays & day.summ$Local_Weekday %in% weekend.days
  }
  day.summ$Local_Weekday <- factor(day.summ$Local_Weekday, levels=days)
  #day.summ$Weekend <- as.factor(!day.summ$Local_Weekday %in% working.days)
  ungroup(day.summ)
}

#******************************************************************************


PTerm <- function(fit, data){
  fstring <- "%m/%d/%Y"
  terms.pred <- GetTerms(data, fit)
  data.frame(Date=as.Date(data$date[1:365], format=fstring), 
             Term=terms.pred[1:365, "s(Local_DayNumber)"])
}

NPTerm <- function(fit, data){
  fstring <- "%m/%d/%Y"
  terms.pred <- GetTerms(data, fit)
  data.frame(Date=as.Date(data$date, format=fstring), 
             Term=terms.pred[, "s(DaysFromStart)"])
}

# FindHolidays <- function(country="Slovenia", year=2015){
#   info <- GetInfoForCountry(country) %>% filter(Local_Year==year)
#   info.sub <- mutate(info, 
#                  Local_Time=times(paste0(Local_Time, ":00")), 
#                  Local_Date=paste(Local_Weekday, Local_Year, Local_Month, Local_Day, Local_Weekday, sep="_")) %>%
#     select(Local_Time, Local_Date, N) %>%
#     mutate(Code=paste(Local_Date, Local_Time)) %>%
#     filter(!duplicated(Code)) %>%
#     select(-Code)
#   # remove duplicate times for time change
#   info.spread <- spread(info.sub, Local_Time, N)
#   rownames(info.spread) <- info.spread$Local_Date
#   info.spread <- select(info.spread, -Local_Date)
#   info.spread <- t(as.matrix(info.spread))
#   info.spread[is.na(info.spread)] <- 0
#   raw.fit <- cmdscale(1 - cor(info.spread), eig=T, k=2)
#   pts <- raw.fit$points
#   plot(pts[, 1], pts[, 2], pch=16)
#   cl <- Mclust(pts, G=2)
#   print(table(cl$classification))
# }



# YearlyPattern2 <- function(Country){
#   day.summ <- GetRegressionData(Country)
#   # date arithmetic, days from minimum day
#   #day.summ$DayNumber <- day.summ$Local_Year + 
#   # add weekend
#   # add holidays and bridge days
#   # add day of week effects (compare M-Th, Fri, Sat, Sun)
#   # smooth seasonal effect
#   fit1 <- scam(Activity ~ 0 + 
#                  s(DaysFromStart, k=5, bs="mpi", m=2),
#                family=Gamma(link="log"), data=day.summ)
#   fs <- 'Activity ~ 0 + s(DaysFromStart, k=5, bs="mpi", m=2)'
#   fs.formula <- with(day.summ, as.formula(fs))
#   browser()
#   fit2 <- scam(Activity ~ 0 + 
#                  s(DaysFromStart, k=5, bs="mpi", m=2) + 
#                  as.factor(Local_Weekday %in% weekend.days),
#                family=Gamma(link="log"), data=day.summ)
#   fit3 <- scam(Activity ~ 0 + 
#                  s(DaysFromStart, k=5, bs="mpi", m=2) + 
#                  as.factor(ifelse(Local_Weekday %in% weekend.days, "Weekend", 
#                                   ifelse(Local_Weekday %in% "Friday", "Friday", "Other"))),
#                family=Gamma(link="log"), data=day.summ)
#   fit4 <- scam(Activity ~ 0 + 
#                  s(DaysFromStart, k=5, bs="mpi", m=2) + 
#                  as.factor(Local_Weekday),
#                family=Gamma(link="log"), data=day.summ)
#   fit5 <- scam(Activity ~ 0 + 
#                  s(DaysFromStart, k=5, bs="mpi", m=2) + 
#                  as.factor(Local_Weekday) + 
#                  s(Local_DayNumber, k=100),
#                family=Gamma(link="log"), data=day.summ)
#   fit6 <- scam(Activity ~ 0 + 
#                  s(DaysFromStart, k=5, bs="mpi", m=2) + 
#                  as.factor(Local_Weekday) + 
#                  s(Local_DayNumber, k=100) + 
#                  as.factor(WeekdayHoliday),
#                family=Gamma(link="log"), data=day.summ)
#   fit7 <- scam(Activity ~ 0 + 
#                  s(DaysFromStart, k=5, bs="mpi", m=2) + 
#                  as.factor(Local_Weekday) + 
#                  s(Local_DayNumber, k=100) + 
#                  as.factor(WeekdayHoliday) + 
#                  as.factor(Bridge),
#                family=Gamma(link="log"), data=day.summ)
#   fit8 <- scam(Activity ~ 0 + 
#                  s(DaysFromStart, k=100, bs="mpi") + 
#                  as.factor(Local_Weekday) + 
#                  s(Local_DayNumber, k=100) + 
#                  as.factor(WeekdayHoliday) + 
#                  as.factor(Bridge),
#                family=Gamma(link="log"), data=day.summ)
#   fit <- scam(Activity ~ 0 + 
#                 s(DaysFromStart, k=5, bs="mpi", m=2) + 
#                 s(Local_DayNumber, k=30) + 
#                 as.factor(Local_Weekday) + 
#                 as.factor(Bridge) +
#                 as.factor(WeekdayHoliday), 
#               family=Gamma(link="log"), data=day.summ)
#   browser()
#   pred <- predict(fit, newdata=day.summ, type="response")
#   browser()
#   pdf(paste0("scam/", Country, ".pdf"), height=4, width=8)
#   par(mfrow=c(1, 2))
#   plot(month.summ$Activity, pch=16, xlab="Month number", ylab="Activity", main=Country)
#   lines(pred, lwd=2, col="blue")
#   pred.period <- predict(fit, newdata=month.summ, type="terms", terms="s(Period)")
#   pred.local_month <- predict(fit, newdata=month.summ, type="terms")
#   lines(exp(pred.period + mean(pred.local_month[, 1])), col="red", lwd=2)
#   legend("topleft", col=c("blue", "red"), lwd=c(2,2), legend=c("model prediction", "monotonic smooth"), bty='n')
#   plot(pred.local_month[1:12], type='l', xlab="Month number", ylab="Month effect", lwd=2)
#   dev.off()
#   #browser()
#   #lines(pred.local_month, col="blue")
# }

# YearlyPattern <- function(Country, year=2016){
#   info <- GetInfoForCountry(Country)
#   month.summ <- group_by(info, Local_Year, Local_Month) %>%
#     summarize(Activity=sum(N))
#   month.summ$Period <- month.summ$Local_Year + (month.summ$Local_Month-1)/12
#   month.summ <- filter(month.summ, Local_Year >= 2013, !(Local_Year==2017 & Local_Month==8)) %>% ungroup()
#   fit <- scam(Activity ~ 0 + s(Period, k=5, bs="mpi", 
#                                m=2) + as.factor(Local_Month), #s(Local_Month, #bs="cc", 
#               #  k=8, m=2), 
#               family=Gamma(link="log"), data=month.summ)
#   pred <- predict(fit, newdata=month.summ, type="response")
#   pdf(paste0("scam/", Country, ".pdf"), height=4, width=8)
#   par(mfrow=c(1, 2))
#   plot(month.summ$Activity, pch=16, xlab="Month number", ylab="Activity", main=Country)
#   lines(pred, lwd=2, col="blue")
#   pred.period <- predict(fit, newdata=month.summ, type="terms", terms="s(Period)")
#   pred.local_month <- predict(fit, newdata=month.summ, type="terms")
#   lines(exp(pred.period + mean(pred.local_month[, 1])), col="red", lwd=2)
#   legend("topleft", col=c("blue", "red"), lwd=c(2,2), legend=c("model prediction", "monotonic smooth"), bty='n')
#   plot(pred.local_month[1:12], type='l', xlab="Month number", ylab="Month effect", lwd=2)
#   dev.off()
#   #browser()
#   #lines(pred.local_month, col="blue")
# }
# Heatmap <- function(df, timeunit, timeunit.order){
#   setnames(df, timeunit, "TimeUnit")
#   sp <- spread(df, Country, Effect)
#   rownames(sp) <- sp$TimeUnit
#   sp <- select(sp, -TimeUnit)
#   sp.matrix <- as.matrix(sp)
#   rownames(sp.matrix) <- rownames(sp)
#   sp.matrix <- sp.matrix[timeunit.order, ]
#   colnames(sp.matrix) <- gsub("_", " ", colnames(sp.matrix))
#   heatmap.2(sp.matrix, dendrogram="column", trace="none", col="redblue", Rowv=F, density.info="none", key.xlab="Log(change in activity)", key.title="")
# }

# PlotBothTerms <- function(npterm, pterm){
#   np.plot <- ggplot(npterm, aes(x=as.Date(Date), y=Term)) + scale_x_date() + geom_line() + theme_bw() + xlab("Date") + ylab("Non-periodic term")
#   p.plot <- ggplot(pterm, aes(x=as.Date(Date), y=Term)) + scale_x_date() + geom_line() + theme_bw() + xlab("Date") + ylab("Periodic term")
#   grid.arrange(np.plot, p.plot, nrow=1)
# }