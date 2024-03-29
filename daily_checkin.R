# daily check-in 2.1

# a successful today

library(dplyr)
library(glue)
library(lubridate)
library(ggplot2)
library(emoji)
library(crayon)

rm(list=ls());cat('\f')


# Functions----
abby_wfh <- function(xdate) {
  require(lubridate)
  ref_date <- ymd(20230607) %>% as.numeric()
  
  out <- as.numeric(xdate)
  
  out <- ((out %% ref_date)/14) == as.integer((out %% ref_date)/14)
  return(out)
}

camp_pudo <- function(xdate, kcdates){
  require(lubridate)
  out.do <- NA
  out.pu <- NA
  
  # monday
  if(lubridate::wday(xdate,label = T) == "Mon"){
    out.do <- "tim"
    out.pu <- "abby"
  }
  # tuesday
  if(lubridate::wday(xdate,label = T) == "Tue"){
    out.do <- "tim"
    out.pu <- "tim"
  }
  # thursday
  if(lubridate::wday(xdate,label = T) == "Thu"){
    out.do <- "tim"
    out.pu <- "abby"
  }
  # friday
  if(lubridate::wday(xdate,label = T) == "Fri"){
    out.do <- "abby"
    out.pu <- "abby"
  }
  # wednesday_Awfh
  if(lubridate::wday(xdate,label = T) == "Wed" & abby_wfh(xdate)){
    out.do <- "abby"
    out.pu <- "abby"
  }
  # wednesday_Awfh
  if(lubridate::wday(xdate,label = T) == "Wed" & !abby_wfh(xdate)){
    out.do <- "tim"
    out.pu <- "abby"
  }
  
  out <- data.frame(date = xdate, 
                    dow = lubridate::wday(xdate, label = T), 
                    drop_off = out.do, 
                    pickup = out.pu)
  
  return(out)
  
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
                            days.holidays = holiday.dates, 
                            anchor.days = anchor.day.dates, 
                            dentist.dates = dental.apt.dates, 
                            doctor.dates = dr.apt.dates){
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
  
  # in-office color
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
  #out$mday2[out$date == Sys.Date()] <- emoji::moon(date = Sys.Date()) # replaces today with moon phase
  out$mday2[out$date == Sys.Date()] <- crayon::red(crayon::bold(mday(Sys.Date())))
  out$mday2[out$date == Sys.Date()] <- crayon::underline(crayon::bold(ifelse(test = nchar(mday(Sys.Date())) == 2, 
                                                                             yes  = as.character(mday(Sys.Date())), 
                                                                             no   = paste("0", mday(Sys.Date()), sep = "", collapse = ""))))
  
  # replace birthday
  if(month(Sys.Date()) == 5){
    out[out$month == 5 & out$mday == 13,]$mday2 <- "\U0001f382"
  }
  
  # replace dental apt
  if(any(dentist.dates %in% out$date)){
    out[out$date %in% dentist.dates,]$mday2 <- "\U0001f9b7"
  }
  
  # replace doctor apt
  if(any(doctor.dates %in% out$date)){
    out[out$date %in% doctor.dates,]$mday2 <- "\U0001fa7a"
  }
  
  # replace anchor day
  if(month(Sys.Date()) %in% month(anchor.days)){
    cur.anchor.date <- anchor.days[month(anchor.days)==month(Sys.Date())]
    
    out[out$date == cur.anchor.date,]$mday2 <- "⚓"
  }
  
  #address colors 
  out$color[is.na(out$color)] <- "grey"
  out$mday2[out$color == "grey"] <- crayon::silver(out$mday2[out$color == "grey" & 
                                                               !is.na(out$color)])
  out$mday2[out$color == "in_office"] <- black(bgGreen(out$mday2[out$color == "in_office"]))
  out$mday2[out$date == Sys.Date() ]  <- bold(bold(out$mday2[out$date == Sys.Date() ]))
  
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
  out$mday2[strip_style(out$mday2) == "  " & 
              out$date != Sys.Date()] <- strip_style(out$mday2[strip_style(out$mday2) == "  " & 
                                                                 out$date != Sys.Date()])
  
  
  
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
  #legend <- glue("{italic(\"--// Legend //--\")}\n  {bgGreen(black(\"- In-Office\t\"))}\n  {silver(\"- Remote\")}\n  {white(bgRed(\"- Vacation\t\"))}\n  {bgRed(blue(\"- Holiday\t\"))}\n  {bgCyan(black(\"- Sick/Other\t\"))}\n  -{emoji::emoji(\"tooth\")}/{emoji::emoji(\"stethoscope\")} Med Apt")
  legend <- glue("{italic(\"--// Legend //--\")}\n  {white(bgRed(\"- Vacation\t\"))}\n  {bgRed(blue(\"- Holiday\t\"))}\n  {bgCyan(black(\"- Sick/Other\t\"))}\n  -{emoji::emoji(\"tooth\")}/{emoji::emoji(\"stethoscope\")} Med Apt")
  output <- paste(output, 
                  sep = " ", collapse = " ") 
  output <- paste(output, legend, sep = "", collapse = "")
  return(output)
}




# Vars.dates----
in_office.dates  <- ymd(c(20230215,20230216,20230217,20230222,20230223,20230224, 
                          20230301,20230302,20230308,20230309,20230316,20230331, 
                          20230405,20230406,20230412,20230413,20230426, 
                          20230508,20230516,20230524,20230525, 
                          20230607,20230628#,#20230629
))

dr.apt.dates     <- ymd(c(19810513))
dental.apt.dates <- ymd(c(20230531,20240112))#ymd(c(20230531))
vaca.dates       <- ymd(c(20230327,20230328, 
                          20230622,20230623,
                          20230717,20230718,20230719,20230720,20230721))

sick_other.dates <- ymd(c(19810513))

holiday.dates    <- ymd(c(20240101, # NYD    *
                          20240115, # MLK_Jr *
                          20240329, # GF     *
                          20240513, # FLOAT  *
                          20240527, # MD     *
                          20240619, # JT     *
                          20240704, # ID     *
                          20230904, # LD     *
                          20231123, # THX-1  * (2 FOR THX)
                          20231124, # THX-2  * (2 FOR THX)
                          20231225, # XMAS-1 * (3 FOR XMAS, YK, HK)
                          20231226, # XMAS-2 * (3 FOR XMAS, YK, HK)
                          20231227))# XMAS-3 * (3 FOR XMAS, YK, HK)

anchor.day.dates <- ymd(c(20231002))

kids.camp <- ymd(c(NA))



# vars.wellbeing----


cur.wellbeing <- c(#"check-in with someone i haven't talked to in a while", 
  "show interest and empathy in others",
  "be kind to myself", 
  "Do 1 thing today that makes me happy",
  "take a walk", 
  "identify a personal asset")

# Vars.projs----

#pip stuff, 
cur.projs <- data.frame(name = c("timesheet", 
                                 "BoS Dashboard - Universe", 
                                 "BoS Racial Equity Dialogue #2 Meeting"), 
                        due  = (c(Sys.Date() %m+% days(c(0))
                        ))) %>%
  mutate(., 
         due = ifelse(is.na(due), Sys.Date(), due)) %>%
  mutate(., 
         due = as_date(due)) 

# process----
cur.projs <- cur.projs[!(cur.projs$name == "PIT weekly 2023 data pulldown" & 
                           as.character(lubridate::wday(Sys.Date(),label=T,abbr=T)) != "Thu"),]

accomplish.tasks <- c(cur.projs$name[cur.projs$due <= Sys.Date()])
progress.tasks   <- c(cur.projs$name[cur.projs$due > Sys.Date()])



# additional regular tasks----
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

# # vacction data upload----
# if(mday(Sys.Date()) %in% 1:5){
#   progress.tasks <- c(progress.tasks, 
#                       "vaccination data upload") %>%
#     unique() %>%
#     .[order(.)]
# }
# if(mday(Sys.Date()) %in% 6:14){
#   library(crayon)
#   cat(bgCyan("expect email back from DHS by the 13th re: vaccination upload\n\n"));Sys.sleep(5)
#   progress.tasks <- c(progress.tasks, 
#                       "DHHS vaccination report download and distribution to partners") %>%
#     unique() %>%
#     .[order(.)]
# }

# work location----
work.location <- ifelse(Sys.Date() %in% in_office.dates, 
                        italic("Duraleigh Office"), italic("Home Office"))
# pickup kids from school----
if(lubridate::wday(Sys.Date(),label=T) == "Tue"){
  work.location <- paste(work.location, " (Leaving at 2:00pm to pickup kids from school)", 
            sep = "", collapse = "")
}




# tidy----
accomplish.tasks <- accomplish.tasks %>%
  .[. != ""] %>%
  paste("\t* ", . ,sep = "") %>%
  paste(., sep = "\n", collapse = "\n") 
progress.tasks <- progress.tasks %>%
  .[. != ""] %>%
  paste("\t* ", . ,sep = "") %>%
  paste(., sep = "\n", collapse = "\n")


# RUN TO HERE-----

# moods----
mood.var <- readline(prompt = cat(crayon::inverse("Enter Mood Below:")))



if(mood.var == ""){
  rm(mood.var)
}
mood.var <- trimws(mood.var, whitespace = "\"")


# Build Email----

template <- glue("\n\nTO:\tstaff@ncceh.org\nFROM:\tTim\nSUBJ:\t[staff] {strftime(Sys.Date(), format = \"%m.%d.%y\", tz = Sys.timezone())} - Check-In\n\n{bold(toupper(\"WORK STATUS/LOCATION\"))}: {work.location}\n\n{bold(\"TODAY I WILL SUPPORT MY AND MY COLLEAGUES' WELLBEING BY:\")}\n\t* {sample(cur.wellbeing,size=1)}\n\n{bold(toupper(\"Today will be successful if I accomplish these tasks\"))}: 
{accomplish.tasks}\n\n{bold(toupper(\"Today will be successful if I make significant progress on these tasks\"))}:
{progress.tasks}\n\n{bold(toupper(\"Mood\"))}: {mood.var}\n\n{bold(toupper(paste({strftime(Sys.Date(),format = \"%B\")},\"CALENDAR:\")),sep = \" \")}\n")

cat('\f')
cat(template,"\n",summarise_mdays(v.dates = get.dates_in.month(Sys.Date()), 
                                  days.in.office = in_office.dates))
