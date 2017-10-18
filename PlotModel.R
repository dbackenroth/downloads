PlotDataAndPredictions <- function(country="USA", type="complex2"){
  source("YearlyPatternsAnalysis.R")
  dat <- GetRegressionData(country)
  fit <- DoRegression(country, type)
  dat$Prediction <- GetPredictions(dat, fit)
  dat$Type <- ifelse(dat$Local_Weekday %in% working.days, "Workweek", "Weekend")
  dat$Type <- factor(dat$Type, levels=c("Workweek", "Weekend"))
  p2 <- ggplot(dat, aes(x=as.Date(date, format="%m/%d/%Y"), y=Activity, col=Type)) + 
    geom_point(alpha=0.1) + 
    theme_bw() + xlab("") + geom_line(aes(x=as.Date(date, format="%m/%d/%Y"), y=Prediction, group=Type, col=Type), size=0.1) + scale_color_manual("", values=cbbPalette)
  print(p2)
}

PlotSmoothTerm <- function(country="USA", type="complex2"){
  source("YearlyPatternsAnalysis.R")
  dat <- GetRegressionData(country)
  fit <- DoRegression(country, type)
  terms <- GetTerms(dat, fit)
  if (type=="complex0"){
    colnames(terms) <- c("Effect_Weekday", "Effect_DaysFromStart", "Effect_Local_DayNumber")
  } else {
    colnames(terms) <- c("Effect_Weekday", "Effect_Holiday", "Effect_DaysFromStart", "Effect_Local_DayNumber")
  }
  hols <- filter(ProcessHolidays(country), Type=="holiday", Year==2016)
  dat <- cbind(dat, terms)
  p2 <- ggplot(filter(dat, Local_Weekday %in% working.days), aes(x=as.Date(date, format="%m/%d/%Y"), y=exp(Effect_Local_DayNumber))) + 
    geom_line() + 
    scale_x_date(limits=as.Date(c("1/1/2016", "12/31/2016"), format="%m/%d/%Y")) + 
    theme_bw() + ylab("Smooth day of year effect") + xlab("") + 
    geom_vline(xintercept=as.Date(hols$Date, format="%m/%d/%Y"), alpha=0.25) + 
    scale_y_continuous(breaks=c(0.5, 0.75, 1, 1.25, 1.5)) + 
    geom_text(data=hols, aes(x=as.Date(Date, format="%m/%d/%Y"), y=1, label=Name), angle=90)
  print(p2)
}