# a successful today

library(dplyr)
library(glue)
library(lubridate)
library(lexicon)
library(ggplot2)
library(Bolstad)
library(emoji)
library(crayon)
library(suncalc)
#library(rnoaa)

rm(list=ls());cat('\f')


# Functions----
# get_today_sunrisesets <- function(date.in.month = Sys.Date()){
#   require(dplyr)
#   require(suncalc)
#   require(lubridate)
#   
#   # get sunrise and sunset times
#   month.dltimes <- data.frame(date = c(date.in.month), 
#                               lon = c(-78.7875), 
#                               lat = c(35.8776))
#   suntimes <- suncalc::getSunlightTimes(data = month.dltimes, 
#                                         keep = c("sunrise", "sunset"), 
#                                         tz = Sys.timezone()) %>% as_tibble()
#   
#   out <- mutate(suntimes, 
#                 sr1 = tolower(strftime(x = sunrise, format = "%I:%M%p")), 
#                 ss1 = tolower(strftime(x = sunset, format = "%I:%M%p")), 
#                 daylight_hrs = round(as.numeric(sunset-sunrise),digits = 1))
#   return(out)
# }
# 
# get_today_moonrisesets <- function(date.in.month = Sys.Date()){
#   require(dplyr)
#   require(suncalc)
#   require(lubridate)
#   month.dltimes <- data.frame(date = c(date.in.month), 
#                               lon = c(-78.7875), 
#                               lat = c(35.8776))
#   moontimes <- suncalc::getMoonTimes(data = month.dltimes, 
#                                      #keep = c("sunrise", "sunset"), 
#                                      tz = Sys.timezone()) %>% as_tibble()
#   moonpos   <- suncalc::getMoonIllumination(date = date.in.month, 
#                                             keep = c("phase", "fraction"))
#   
#   (moonpos.def <- data.frame(def = c("New Moon", "Waxing Crescent", "First Quarter", 
#                                     "Waxing Gibbous", "Full Moon", "Waning Gibbous", 
#                                     "Last Quarter", "Waning Crescent"), 
#                              shape = c("New", "Crescent", "Quarter", "Gibbous", "Full", 
#                                        "Gibbous", "Quarter", "Crescent")))
#   
#   out <- mutate(moontimes, 
#                 mr1 = tolower(strftime(x = rise, format = "%I:%M%p")), 
#                 ms1 = tolower(strftime(x = set, format = "%I:%M%p")), 
#                 hrs_moonup = abs(round(as.numeric(set-rise),digits = 1)))
#   return(out)
#   
#   
# }
# 
# get_today_moonrisesets()

# daily.sl.hrs <- NULL
# for(i in as_date(ymd(20230101):ymd(20231231))){
#   daily.sl.hrs <- c(daily.sl.hrs, 
#                     get_today_sunrisesets(date.in.month = as_date(i))$daylight_hrs)
# }
# 
# get_today_sunrisesets()$daylight_hrs / max(daily.sl.hrs)
# 
# 
# get_monthly_sunrisesets <- function(date.in.month = Sys.Date()){
#   require(dplyr)
#   require(suncalc)
#   require(lubridate)
#   
#   # get first day of month
#   month.D1 <- (date.in.month %m-% days(lubridate::mday(date.in.month)-1))
#   
#   # get last day of month
#   month.DL <- month.D1
#   while(lubridate::month(month.D1) == lubridate::month(month.DL)){
#     month.DL <- month.DL %m+% days(1)
#   }
#   month.DL <- month.DL %m-% days(1)
#   
#   # get sunrise and sunset times
#   month.dltimes <- data.frame(date = c(month.D1, month.DL), 
#                               lon = c(-78.7875), 
#                               lat = c(35.8776))
#   suntimes <- suncalc::getSunlightTimes(data = month.dltimes, 
#                             keep = c("sunrise", "sunset"), 
#                             tz = Sys.timezone()) %>% as_tibble()
#   
#   out <- mutate(suntimes, 
#                 sr1 = tolower(strftime(x = sunrise, format = "%I:%M%p")), 
#                 ss1 = tolower(strftime(x = sunset, format = "%I:%M%p")), 
#                 daylight_hrs = round(as.numeric(sunset-sunrise),digits = 1))
#   return(out)
# }



this_thurDate <- function(x = Sys.Date()){
  n <- 0
  x.dow <- lubridate::wday(x,label = T, abbr = T) %>%
    as.character()
  while(x.dow != "Thu"){
    n <- n + 1
    if(n > 100){ break}
    x <- as_date(x %m+% days(1))
    x.dow <- lubridate::wday(x,label = T, abbr = T) %>%
      as.character()
  }
  # convert to numeric
  x <- gsub("-", "", x)
  x <- as.character(x) %>% as.numeric(x)
  return(x)
}

get.dates_in.month <- function(a.date = Sys.Date()){
  require(lubridate)
  
  # get the days in the month
  temp.month <- lubridate::month(a.date) %>%
    as.character()
  temp.year  <- year(a.date) %>%
    as.character()
  
  if(nchar(temp.month) == 1){
    temp.month <- glue("0{temp.month}")
  }
  
  temp.month_days <- ymd(glue("{temp.year}-{temp.month}-01")):(ymd(glue("{temp.year}-{temp.month}-01")) %m+% days(31))
  temp.month_days <- as_date(temp.month_days) %>%
    .[lubridate::month(.) == as.numeric(temp.month)]
  
  # retun the days
  return(temp.month_days)
}


summarise_mdays <- function(v.dates = get.dates_in.month(Sys.Date()), 
                            days.in.office = in_office.dates, 
                            days.vacation = vaca.dates, 
                            days.sick.other = sick_other.dates,
                            days.holidays = holiday.dates){
  require(lubridate)
  require(crayon)
  require(glue)
  out <- data.frame(date = v.dates, 
                    week = epiweek(v.dates), 
                    wday = lubridate::wday(v.dates))
  
  # add in missing dates so every week has 7 dates
  n_missingdays <- out %>%
    group_by(week) %>%
    summarise(n_days = n_distinct(wday)) %>%
    ungroup() %>%
    mutate(., 
           missing_days = 7 - n_days) %>%
    mutate(., 
           missing_days = ifelse(week == min(week), missing_days * -1, missing_days)) %>%
    .$missing_days %>%
    .[. != 0]
  
  
  temp.daterange <- range(v.dates)
  temp.min <- min(temp.daterange) %m+% days(min(n_missingdays))
  temp.max <- max(temp.daterange) %m+% days(max(n_missingdays))
  
  new.dates <- temp.min:temp.max
  new.dates <- as_date(new.dates)
  
  # new dataframe
  out <- data.frame(date = new.dates, 
                    week = epiweek(new.dates), 
                    month = lubridate::month(new.dates), 
                    wday = lubridate::wday(new.dates), 
                    wdayabbr = lubridate::wday(new.dates, label = T),
                    mday = mday(new.dates), 
                    year = year(new.dates)) %>%
    as_tibble() %>%
    mutate(., 
           mday2 = ifelse(nchar(as.character(mday)) == 1,
                          paste("0", as.character(mday), sep = ""), 
                          as.character(mday)))
  
  out$color <- NA
  #out$color[out$date == Sys.Date()] <- "today"
  out$color[out$date %in% days.in.office] <- "in_office"
  
  # vacation color
  if(any(out$date %in% days.vacation)){
    out$color[out$date %in% days.vacation] <- "vacation"
  }
  
  # sick/other color 
  if(any(out$date %in% days.sick.other)){
    out$color[out$date %in% days.sick.other] <- "sick_other"
  }
  
  
  # holiday color 
  if(any(out$date %in% days.holidays)){
    out$color[out$date %in% days.holidays] <- "holiday"
  }
  
  
  # normalize week numbers down to zero
  out$week_norm <- out$week - min(out$week) + 1
  
  # replace weekends
  out$mday2[out$wdayabbr %in% c("Sun","Sat")] <- "  "
  # out$mday2[out$wdayabbr %in% c("Sun")] <- "  "
  # 
  # # moon phase for saturdays
  # out$mday2[out$wdayabbr %in% c("Sat")] <- emoji::moon(date = out$date[out$wdayabbr == "Sat"])
  
  # replace non-current month
  out$mday2[out$month != unique(lubridate::month(v.dates))] <- "  "
  # replace today 
  #out$mday2[out$date == Sys.Date()] <- "xx"
  out$mday2[out$date == Sys.Date()] <- emoji::moon(date = Sys.Date()) # replaces today with moon phase
  
  #address colors 
  out$color[is.na(out$color)] <- "grey"
    out$mday2[out$color == "grey"] <- crayon::silver(out$mday2[out$color == "grey" & 
                                                                 !is.na(out$color)])
    out$mday2[out$color == "in_office"] <- black(bgGreen(out$mday2[out$color == "in_office"]))
    out$mday2[out$date == Sys.Date() ] <- bold(bold(out$mday2[out$date == Sys.Date() ]))
    
    if(any(out$date %in% days.holidays)){
      out$mday2[out$color == "holiday"] <- blue(bgRed(out$mday2[out$color == "holiday"]))
    }
    if(any(out$date %in% days.vacation)){
      out$mday2[out$color == "vacation"] <- white(bgRed(out$mday2[out$color == "vacation"]))
    }
    if(any(out$date %in% days.sick.other)){
      out$mday2[out$color == "sick_other"] <- black(bgCyan(out$mday2[out$color == "sick_other"]))
    }
    
    # remove formatting for days not in current month
    out$mday2[strip_style(out$mday2) == "  "] <- strip_style(out$mday2[strip_style(out$mday2) == "  "])
    
    
    
    # gen_output 
    output <- "Su M  Tu W  Th F  Sa\n"
    for(i in 1:nrow(out)){
      output <- c(output, 
                  out$mday2[i])
      # if new row
      if(i > 1 & 
         out$wdayabbr[i] == "Sat"){
        output <- c(output, "\n")
      }
      
      
      
    }
    #legend <- glue("{italic(\"----// Calendar Legend //----\")}\n{(\"Today\t\t(bold 'xx')\")}\n{silver(\"Remote\t\t(light grey)\")}\n{bgGreen(red(\"In-Office\t(red on green)\"))}\n{white(bgRed(\"Vacation\t(white on red)\"))}\n{bgRed(blue(\"Holiday\t\t(blue on red)\"))}")
    legend <- glue("{italic(\"--// Legend //--\")}\n  {bgGreen(black(\"- In-Office\t\"))}\n  {silver(\"- Remote\")}\n  {white(bgRed(\"- Vacation\t\"))}\n  {bgRed(blue(\"- Holiday\t\"))}\n  {bgCyan(black(\"- Sick/Other\t\"))}")
    output <- paste(output, 
                    sep = " ", collapse = " ") 
    output <- paste(output, legend, sep = "", collapse = "")
    return(output)
}


# summarise_mdays(v.dates = get.dates_in.month(Sys.Date()),
#                 days.in.office = (in_office.dates),
#                 days.vacation = c(vaca.dates),
#                 days.holidays = c(holiday.dates)) %>% cat()

# crayon::show_ansi_colors(8)



# Vars.dates----
in_office.dates  <- ymd(c(20230215,20230216,20230217,20230222,20230223,20230224, 
                          20230301,20230302,20230308,20230309,20230316,20230331, 
                          20230405,20230406,20230412,20230413,20230419,20230420))
vaca.dates       <- ymd(c(20230327,20230328, 
                          20230622,20230623,
                          20230717,20230718,20230719,20230720,20230721))
sick_other.dates <- ymd(c(19810513))
holiday.dates    <- ymd(c(20230101, 20230116, 20230410, 20230529, 20230619, 
                          20230704, 20230904, 20231123, 20231124, 20231225, 
                          20231226, 20231227))

# holiday.dates   <- "New Year’s Day
# Martin Luther King Jr. Birthday
# Good Friday or Easter Monday or Passover
# Memorial Day
# Independence Day
# Labor Day
# Thanksgiving Day (2 Days)
# Christmas Day (3 days) (or these 3 days may be used for Hanukkah or Yom Kippur)
# Floating Holiday (Veteran’s Day, President’s Day, employee’s birthday, or other religious holiday)"


# Vars.projs----
cur.projs <- data.frame(name = c("clicktime", 
                                 "veterans workgroup key metrics for PPT",
                                 "congressional district maps", 
                                 "schedule meeting with katie to go over BO reporting for re-mapping broken reports",
  "PIT - figure out why some data in csv export doesn't match HMIS",
                                 "BoS Dashboard",
                                 #"PIT - finish final code corrections/changes",
                                 "vaccination data processing",
                                 "PIT weekly 2023 data pulldown"), 
                        due = (c(
                          ymd(20230406) %m+% days(c(0,1,1,1)), 
                          Sys.Date() %m+% days(1), 
                          Sys.Date() %m+% days(1), 
                          #Sys.Date() %m+% days(0), 
                          Sys.Date() %m+% days(0),
                          ymd(this_thurDate())))) %>%
  mutate(., 
         due = ifelse(is.na(due), Sys.Date(), due)) %>%
  mutate(., 
         due = as_date(due))


cur.projs

# process----
cur.projs <- cur.projs[!(cur.projs$name == "PIT weekly 2023 data pulldown" & 
                           as.character(lubridate::wday(Sys.Date(),label=T,abbr=T)) != "Thu"),]

accomplish.tasks <- c(cur.projs$name[cur.projs$due <= Sys.Date()])
progress.tasks   <- c(cur.projs$name[cur.projs$due > Sys.Date()])



# additional regular tasks----
# if(wday(Sys.Date()) == 6){
#   accomplish.tasks <- c(accomplish.tasks, 
#                         "complete clicktime for this week") %>% unique() %>% 
#     .[order(.)]
# }

if(wday(Sys.Date()) == 2){
  accomplish.tasks <- c(accomplish.tasks, 
                        "Smartsheet updates",
                        "Block focus times for this week", 
                        "Data Center weekly tactical meeting") %>% unique() %>% 
    .[order(.)]
}
if(wday(Sys.Date()) == 3){
  accomplish.tasks <- c(accomplish.tasks, 
                        "weekly check-in with andrea") %>% unique() %>% 
    .[order(.)]
}


if(mday(Sys.Date()) %in% 1:5){
  progress.tasks <- c(progress.tasks, 
                      "vaccination data upload") %>%
    unique() %>%
    .[order(.)]
}


if(mday(Sys.Date()) %in% 6:14){
  library(crayon)
  cat(bgCyan("expect email back from DHS by the 13th re: vaccination upload\n\n"));Sys.sleep(5)
  progress.tasks <- c(progress.tasks, 
                      "DHHS vaccination report download and distribution to partners") %>%
    unique() %>%
    .[order(.)]
}




# work location----


work.location <- ifelse(Sys.Date() %in% in_office.dates, 
                        italic("Duraleigh Office"), italic("Home Office"))


# moods----
poss.mood <- c("smiley", "tada", "100", "thumbs up","trophy","handshake",   # basic good mood
               #"jack_o_lantern", zombie", "ghost", "skull_and_crossbones", # for halloween season
               #"fallen leaf",  "pumpkin", "turkey",                        # for autumn
               #"santa", "cold face",                                       # for winter
               "coffee",                                                    # good for when tired
               "gym","computer", "disk"  # generic
) 


#emoji::day_in_synodic_cycle(Sys.Date())
names(emoji::emoji_name) %>%
  .[!grepl("_tone$|_hair$|_bald$", .)]

set.seed(Sys.Date())
pm1 <- sample(poss.mood, size = 1)
set.seed(Sys.Date())
emkw <- emoji_keyword[[pm1]] %>%
  .[sample(1:length(.), size = 1)]

mood.var         <- emoji::emoji(emkw)

# how many hours of sunlight today? ----
srss.rdu <- suncalc::getSunlightTimes(date = Sys.Date(), 
                                      lat = 35.8801, 
                                      lon = -78.7880, 
                                      keep = c("sunrise", "sunset")) %>%
  .[,c("sunrise", "sunset")] 

sunlight.time <- srss.rdu$sunset - srss.rdu$sunrise
class(sunlight.time)

sunlight.hours <- as.numeric(sunlight.time) %>% as.integer()
sunlight.mins <- as.integer((as.numeric(sunlight.time) %% sunlight.hours)*60)

sunlight.time <- glue("{sunlight.hours} hrs and {sunlight.mins} minutes of daylight today")

# max and min daylight during year

df.daylight <- data.frame(date = as_date(ymd(20230101):ymd(20231231)), 
                          lon = -78.7880, 
                          lat = 35.8001) %>% as_tibble()

df.year <- getSunlightTimes(data = df.daylight, keep = c("sunrise", "sunset")) %>%
  as_tibble() %>%
  mutate(., 
         daylight = as.numeric(sunset - sunrise)) #%>%
#.[.$daylight == max(.$daylight) | 
#   .$daylight == min(.$daylight),]

df.year$sunrise <- df.year$sunrise %>% with_tz(., tzone = Sys.timezone())
df.year$sunset <- df.year$sunset %>% with_tz(., tzone = Sys.timezone())


df.year$sunrise_time
df.year$sunset_time 

df.year$work_start <- as_datetime(NA)
df.year$work_end   <- as_datetime(NA)

# saturdays, sundays, holidays, etc
df.year$work_start[lubridate::wday(df.year$date, label = T) %in% 
                     c("Sat", "Sun")] <- NA
df.year$work_end[lubridate::wday(df.year$date, label = T) %in% 
                   c("Sat", "Sun")]   <- NA

# in-office days (wed-thu)
df.year$work_start[lubridate::wday(df.year$date, label = T) %in% 
                     c("Wed", "Thu")] <- force_tz(df.year$date[lubridate::wday(df.year$date, label = T) %in% 
                                                                 c("Wed", "Thu")] %m+%
                                                    hours(6) %m+% minutes(30), 
                                                  tzone = Sys.timezone())
df.year$work_end[lubridate::wday(df.year$date, label = T) %in% 
                   c("Wed", "Thu")]   <- force_tz(df.year$date[lubridate::wday(df.year$date, label = T) %in% 
                                                                 c("Wed", "Thu")] %m+% 
                                                    hours(16) %m+% minutes(45), 
                                                  tzone = Sys.timezone())

# remote days (mon-tue-fri)
df.year$work_start[lubridate::wday(df.year$date, label = T) %in%
                     c("Mon", "Fri")] <- force_tz(df.year$date[lubridate::wday(df.year$date, label = T) %in% 
                                                                 c("Mon", "Fri")] %m+%
                                                    hours(7) , 
                                                  tzone = Sys.timezone())
df.year$work_end[lubridate::wday(df.year$date, label = T) %in% 
                   c("Mon", "Fri")]   <- force_tz(df.year$date[lubridate::wday(df.year$date, label = T) %in% 
                                                                 c("Mon","Fri")] %m+% 
                                                    hours(16) %m+% minutes(30), 
                                                  tzone = Sys.timezone())

df.year$work_start[lubridate::wday(df.year$date, label = T) %in%
                     c("Tue")] <- force_tz(df.year$date[lubridate::wday(df.year$date, label = T) %in% 
                                                          c("Tue")] %m+%
                                             hours(7) , 
                                           tzone = Sys.timezone())
df.year$work_end[lubridate::wday(df.year$date, label = T) %in% 
                   c("Tue")]   <- force_tz(df.year$date[lubridate::wday(df.year$date, label = T) %in% 
                                                          c("Tue")] %m+% 
                                             hours(14) , 
                                           tzone = Sys.timezone())


df.year$work_start <- df.year$work_start %>% with_tz(., tzone = Sys.timezone())
df.year$work_end   <- df.year$work_end   %>% with_tz(., tzone = Sys.timezone())


df.year$daylight_not_working <- NA
df.year$daylight_not_working[is.na(df.year$work_start)] <- df.year$daylight[is.na(df.year$work_start)]


df.year$daylight_not_working[!is.na(df.year$work_start)] <-
  (ifelse(((df.year$work_start[!is.na(df.year$work_start)] - df.year$sunrise[!is.na(df.year$work_start)]) / 60 / 60) >0, 
          (df.year$work_start[!is.na(df.year$work_start)] - df.year$sunrise[!is.na(df.year$work_start)]) / 60 / 60, 0)) +
  ifelse(((df.year$sunset[!is.na(df.year$work_start)] - df.year$work_end[!is.na(df.year$work_start)])/60/1) > 0, 
         ((df.year$sunset[!is.na(df.year$work_start)] - df.year$work_end[!is.na(df.year$work_start)])/60/1), 0) 

ggplot() + 
  geom_line(data = df.year, 
            aes(x = date, y = sunrise - as_datetime(as_date(sunrise)), 
                color = "sunrise"))+
  geom_line(data = df.year, 
            aes(x = date, y = sunset - as_datetime(as_date(sunset)), 
                color = "sunset")) +
  geom_vline(aes(xintercept = Sys.Date(), 
                 linetype = "Today"))


df.year %>%
  #group_by(month = lubridate::month(date,label = T)) %>%
  group_by(dow = lubridate::wday(date,label=T)) %>%
  summarise(n_days = n(), 
            t_daylight_hrs = sum(daylight), 
            t_dl.not.work = sum(daylight_not_working)) %>%
  ungroup() %>%
  mutate(., 
         pct_dl.not.work = t_dl.not.work / t_daylight_hrs)

# tidy----
accomplish.tasks <- accomplish.tasks %>%
  .[. != ""] %>%
  paste("\t* ", . ,sep = "") %>%
  paste(., sep = "\n", collapse = "\n") 
progress.tasks <- progress.tasks %>%
  .[. != ""] %>%
  paste("\t* ", . ,sep = "") %>%
  paste(., sep = "\n", collapse = "\n")


# monthly sunrise sunset----

template <- glue("\n\nTO:\tstaff@ncceh.org\nFROM:\tTim\nSUBJ:\t[staff] {strftime(Sys.Date(), format = \"%m.%d.%y\", tz = Sys.timezone())} - Check-In\n\n{bold(toupper(\"Schedule/Out of the Office\"))}: {work.location}\n\n{bold(toupper(\"Today will be successful if I accomplish these tasks\"))}: 
{accomplish.tasks}\n\n{bold(toupper(\"Today will be successful if I make significant progress on these tasks\"))}:
{progress.tasks}\n\n{bold(toupper(\"Mood\"))}: {mood.var}\n\n{bold(toupper(paste({strftime(Sys.Date(),format = \"%B\")},\"CALENDAR:\")),sep = \" \")}\n")

cat('\f')
cat(template,"\n",summarise_mdays(v.dates = get.dates_in.month(Sys.Date()), 
                                  days.in.office = in_office.dates))

 
