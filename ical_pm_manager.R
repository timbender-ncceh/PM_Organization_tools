library(dplyr)
library(calendar)
library(lubridate)
library(ggplot2)
library(uuid)
library(glue)
library(openxlsx)


print("see andrea email from 3/7/23 on clicktime categories");Sys.sleep(10)


# https://cran.rstudio.com/web/packages/calendar/calendar.pdf

rm(list=ls());cat('\f')


# To Do Items---- 

# 2. incorporate "clicktime_cat" to all parts of the code


# 6. figure out how to automatically remove lines from .xlsx only after they've
# been verified in PIP_calendar while not allowing any calendar item to be
# double_entered

# 9. if PIP_calendar events have to leading "[clicktime_cat]" pattern in summary field, append a "[missing]" to them


# funs----
get.MTG.mgts <- function(a.date = Sys.Date(), 
                         mtg_list = mtg.ical.list){
  require(lubridate)
  which_mtg_on_date <- NULL
  for(i in 1:length(mtg_list)){
    try(temp.start <- mtg_list[[i]] %>%
          grep(pattern = "^DTSTART", x = ., value = T) %>%
          gsub(pattern = "^DTSTART:", "", x = .) %>%
          ymd_hms(., tz = Sys.timezone()))
    try(temp.end <- mtg_list[[i]] %>%
          grep(pattern = "^DTEND", x = ., value = T) %>%
          gsub(pattern = "^DTEND:", "", x = .) %>%
          ymd_hms(., tz = Sys.timezone()))
    
    out <- F
    try(out <- a.date == as_date(temp.start) & 
          a.date == as_date(temp.end))
    
    which_mtg_on_date <- c(which_mtg_on_date, 
                           out) 
    rm(out, temp.start, temp.end)
    
  }
  
  which_mtg_on_date <- which(which_mtg_on_date)
  
  # return the data
  out <- mtg_list[which_mtg_on_date]
  out <- ical_list2df(out) %>% as_tibble()
  
  return(out)
}
ical_list2df <- function(ic_list){
  require(lubridate)
  out.df <- NULL
  
  for(i in 1:length(ic_list)){
    # DTSTART
    temp.start <- grep(pattern = "DTSTART", 
                       x = ic_list[[i]], 
                       value = T) %>%
      gsub(pattern = "^DTSTART:", "", .) %>%
      ymd_hms(., tz = Sys.timezone())
    
    # DTEND
    temp.end <- grep(pattern = "DTEND", 
                     x = ic_list[[i]], 
                     value = T) %>%
      gsub(pattern = "^DTEND:", "", .) %>%
      ymd_hms(., tz = Sys.timezone())
    
    # SUMMARY
    temp.summ <- grep(pattern = "SUMMARY", 
                      x = ic_list[[i]], 
                      value = T) %>%
      gsub(pattern = "^SUMMARY:", "", .) 
    
    # DESCRIPTION
    temp.desc <- grep(pattern = "DESCRIPTION", 
                      x = ic_list[[i]], 
                      value = T) %>%
      gsub(pattern = "^DESCRIPTION:", "", .) 
    
    out.df <- rbind(out.df, 
                    data.frame(Event_Title = temp.summ, 
                               Desc = temp.desc,
                               Start1 = temp.start, 
                               End1 = temp.end, 
                               from_ical = T))
    
    rm(temp.start, temp.desc, temp.summ, temp.desc)
  }
  return(out.df)
}

lead0 <- function(x){
  # adds a leading zero to numbers - useful for setting time from string
  if(!is.character(x)){
    x <- as.character(x)
  }
  if(nchar(x) == 1){
    out <- paste("0", x, sep = "")
  }else{
    out <- x
  }
  return(out)
}

ic_guid <- function(n=1){
  # overwrites built-in function of ic_guid() in library(calendar) with a more
  # appropriate uuid-generating function from the library(uuid).  see
  # https://stackoverflow.com/questions/10492817/how-can-i-generate-a-guid-in-r/10493590#10493590
  return(paste("ical", uuid::UUIDgenerate(F,n=n), sep = "-"))
}

gen_ical.event <- function(event_title,desc = NA, 
                           start1,end1 = NA, my.tz = "America/New_York"){
  require(lubridate)
  # make sure start1 has correct time zone
  if(tz(start1) != my.tz){
    stop("start time has incorrect and/or missing timezone")
  }
  
  # handle null end inputs
  if(is.na(end1)){
    end1 <- 1  # default duration of 1 hour
  }
  
  # generate ical event
  out <- ic_event(uid = ic_guid(), 
                  start_time = start1, 
                  end_time = end1, 
                  summary = event_title, 
                  more_properties = T, 
                  event_properties = calendar::properties)
  
  # add DESCRIPTION
  if(!is.na(desc)){
    out$DESCRIPTION <- desc
  }else{
    out$DESCRIPTION <- ""
  }
  
  # return
  return(out)
}

# set calendar wd
setwd("C:/Users/TimBender/Documents/R/ncceh/calendars")

# Vars----
cal.input.link <- "tim_cal_input.xlsx"

# Import Data----
# get secret google calendar urls
source("secret_ical_url.R")

download.file(url = secret.ical.url_MTG, 
              destfile = "tim_MTG_calendar.ics")
download.file(url = secret.ical.url_PM, 
              destfile = "tim_PM_calendar.ics")

# load spreadsheet of calendar changes
cal.input <- openxlsx::read.xlsx(cal.input.link) %>%
  as_tibble()

# tidy----

# if is.na(year){ assume  year == year(today)}
cal.input$start1_year[is.na(cal.input$start1_year)] <- year(Sys.Date())
cal.input$end1_year[is.na(cal.input$end1_year)] <- year(Sys.Date())

# if is.na(month){ assume  month == month(today)}
cal.input$start1_month[is.na(cal.input$start1_month)] <- lubridate::month(Sys.Date())
cal.input$end1_month[is.na(cal.input$end1_month)] <- lubridate::month(Sys.Date())

# if is.na(mday){ assume  mday == mday(today)}
cal.input$start1_mday[is.na(cal.input$start1_mday)] <- lubridate::mday(Sys.Date())
cal.input$end1_mday[is.na(cal.input$end1_mday)] <- lubridate::mday(Sys.Date())

# AM/PM  - hrs 1:6 become (1:6)+12
cal.input$start1_hr <- ifelse(cal.input$start1_hr %in% 1:6, cal.input$start1_hr + 12, cal.input$start1_hr)
cal.input$end1_hr   <- ifelse(cal.input$end1_hr   %in% 1:6, cal.input$end1_hr   + 12, cal.input$end1_hr  )

# put together Start1 and End1
cal.input$Start1 <- paste(cal.input$start1_year, "-",
                          unlist(lapply(X = cal.input$start1_month, FUN = lead0)), "-",
                          unlist(lapply(X = cal.input$start1_mday, FUN = lead0)), " ", 
                          unlist(lapply(cal.input$start1_hr, lead0)), ":",
                          unlist(lapply(cal.input$start1_min, lead0)), 
                          sep = "") %>%
  ymd_hm(., tz = Sys.timezone())
cal.input$End1 <- paste(cal.input$end1_year, "-",
                        unlist(lapply(X = cal.input$end1_month, FUN = lead0)), "-",
                        unlist(lapply(X = cal.input$end1_mday, FUN = lead0)), " ", 
                        unlist(lapply(cal.input$end1_hr, lead0)), ":",
                        unlist(lapply(cal.input$end1_min, lead0)), 
                        sep = "")%>%
  ymd_hm(., tz = Sys.timezone())

cal.input$Desc <- as.character(cal.input$Desc)

# make clicktime_cat all UPPERCASE all the time. 
cal.input$clicktime_cat <- toupper(cal.input$clicktime_cat)

# append clicktime_cat as "[clicktime_cat] summary" in summary field of every xlsx file and trickle-down to every calendar.  
cal.input$Event_Title <- paste("[", cal.input$clicktime_cat, "] ", cal.input$Event_Title, sep = "")

# remove all old .ics files from directory
old_ics_files <- list.files(pattern = "\\.ics$") %>%
  .[!. %in% c(#"tim.bender@ncceh.org.ics", 
    #"tim_meeting_calendar.ics", 
    "tim_MTG_calendar.ics", 
    "tim_PM_calendar.ics")]

# confirm that wd checks out as safety check
if(getwd() == "C:/Users/TimBender/Documents/R/ncceh/calendars"){
  # delete the old ics files
  file.remove(old_ics_files)
  cat(glue("Remaining ics files:\n * {paste(list.files(pattern = \".ics$\"), sep = \"\n * \", collapse = \"\n * \")}"))
}

# check that new calendar items aren't already in calendar----
# to-do----
# 3. make PM_calendar data frame, PIP_calendar data frame, and xlsx_calendar
# data frame consistent with 6 columns: 1) clicktime_cat, summary,
# description, start_datetime, end_datetime, from_source

# 4. Only check for identical entries between calendars with the following
# fields: summary, start_datetime, end_datetime

# 5. make sure both PIP_calendar includes all meetings from PM_calendar

cal.input

pm.ical.list <- calendar::ic_list(x = readLines("tim_PM_calendar.ics"))
mtg.ical.list <- calendar::ic_list(x = readLines("tim_MTG_calendar.ics"))

pm.ical.df        <- ical_list2df(pm.ical.list) %>% as_tibble()
mtg.ical.df.today <- get.MTG.mgts(a.date = Sys.Date())

pm.ical.df <- rbind(pm.ical.df, 
                    mtg.ical.df.today)

pm.xlsx.df <- cal.input[,c("Event_Title", "Desc", "Start1", "End1")] %>%
  mutate(., 
         from_xlsx = T) %>% 
  as_tibble()

pm.ical.df
pm.xlsx.df

pm.xlsx_alread_in_cal <- full_join(pm.ical.df, pm.xlsx.df) %>%
  mutate(., 
         from_ical = ifelse(is.na(from_ical), F, from_ical), 
         from_xlsx = ifelse(is.na(from_xlsx), F, from_xlsx)) %>%
  .[.$from_ical & .$from_xlsx,c("Event_Title", "Desc", "Start1", "End1")] %>%
  mutate(., 
         already_in_cal = T)

# remove items already in cal from xlsx
pm.xlsx_alread_in_cal
cal.input

cal.input <- left_join(cal.input, 
                       pm.xlsx_alread_in_cal) %>%
  mutate(., 
         already_in_cal = ifelse(is.na(already_in_cal), F, already_in_cal)) %>%
  .[!.$already_in_cal,] %>% 
  .[!colnames(.) %in% c("already_in_cal")]

# pull in meetings from specific day of MTG calendar




# add another week's worth of recurring items. ----

# first check to see if they're already in the calendar

this.weeks.dates <- data.frame(date = as_date(unique(c(Sys.Date():(Sys.Date() %m+% days(4)), 
                                                Sys.Date():(Sys.Date() %m-% days(4)))))) %>%
  mutate(., 
         today = date == Sys.Date(), 
         week = week(date), 
         wday = lubridate::wday(date, label = T)) %>%
  .[order(.$date),] %>%
  .[.$week == .$week[.$today == T],] %>%
  .[.$wday %in% c("Mon", "Tue", "Wed", "Thu", "Fri"),] %>%
  .$date


# check this week 
if(sum(unique(as_date(pm.ical.df[pm.ical.df$Event_Title %>% 
                     grepl("\\[RECURRING\\] - Morning Planning", ., 
                           ignore.case = F),]$Start1)) %in%
  this.weeks.dates) != 5){
  # not in this week; must add vvv
  cal.input <- rbind(cal.input, 
        data.frame(clicktime_cat = c("recurring"), 
                   Event_Title   = c("[RECURRING] - Morning Planning"), 
                   Desc          = c(NA), 
                   start1_year   = c(year(this.weeks.dates)), 
                   start1_month  = c(lubridate::month(this.weeks.dates)), 
                   start1_mday   = c(mday(this.weeks.dates)), 
                   start1_hr     = c(7,8,7,7,7), 
                   start1_min    = c(0), 
                   Start1        = c(NA), 
                   end1_year   = c(year(this.weeks.dates)), 
                   end1_month  = c(lubridate::month(this.weeks.dates)), 
                   end1_mday   = c(mday(this.weeks.dates)), 
                   end1_hr     = c(hour(NA)), 
                   end1_min    = c(minute(NA)), 
                   End1        = c(NA))) %>%
    mutate(., 
           end1_hr = start1_hr + 1,
           end1_min = start1_min)
  
  cal.input$Start1 <- paste(cal.input$start1_year, "-",
                            unlist(lapply(X = cal.input$start1_month, FUN = lead0)), "-",
                            unlist(lapply(X = cal.input$start1_mday, FUN = lead0)), " ", 
                            unlist(lapply(cal.input$start1_hr, lead0)), ":",
                            unlist(lapply(cal.input$start1_min, lead0)), 
                            sep = "") %>%
    ymd_hm(., tz = Sys.timezone())
  cal.input$End1 <- paste(cal.input$end1_year, "-",
                          unlist(lapply(X = cal.input$end1_month, FUN = lead0)), "-",
                          unlist(lapply(X = cal.input$end1_mday, FUN = lead0)), " ", 
                          unlist(lapply(cal.input$end1_hr, lead0)), ":",
                          unlist(lapply(cal.input$end1_min, lead0)), 
                          sep = "")%>%
    ymd_hm(., tz = Sys.timezone())
  
  cal.input
}

# check next week
if(sum(unique(as_date(pm.ical.df[pm.ical.df$Event_Title %>% 
                                 grepl("\\[RECURRING\\] - Morning Planning", ., 
                                       ignore.case = F),]$Start1)) %in%
       (this.weeks.dates %m+% days(7))) != 5){
  # not in next week; must add vvv
  cal.input <- rbind(cal.input, 
                     data.frame(clicktime_cat = c("recurring"), 
                                Event_Title   = c("[RECURRING] - Morning Planning"), 
                                Desc          = c(NA), 
                                start1_year   = c(year( (this.weeks.dates %m+% days(7)))), 
                                start1_month  = c(lubridate::month( (this.weeks.dates %m+% days(7)))), 
                                start1_mday   = c(mday( (this.weeks.dates %m+% days(7)))), 
                                start1_hr     = c(7,8,7,7,7), 
                                start1_min    = c(0), 
                                Start1        = c(NA), 
                                end1_year   = c(year( (this.weeks.dates %m+% days(7)))), 
                                end1_month  = c(lubridate::month( (this.weeks.dates %m+% days(7)))), 
                                end1_mday   = c(mday( (this.weeks.dates %m+% days(7)))), 
                                end1_hr     = c(hour(NA)), 
                                end1_min    = c(minute(NA)), 
                                End1        = c(NA))) %>%
    mutate(., 
           end1_hr = start1_hr + 1,
           end1_min = start1_min)
  
  cal.input$Start1 <- paste(cal.input$start1_year, "-",
                            unlist(lapply(X = cal.input$start1_month, FUN = lead0)), "-",
                            unlist(lapply(X = cal.input$start1_mday, FUN = lead0)), " ", 
                            unlist(lapply(cal.input$start1_hr, lead0)), ":",
                            unlist(lapply(cal.input$start1_min, lead0)), 
                            sep = "") %>%
    ymd_hm(., tz = Sys.timezone())
  cal.input$End1 <- paste(cal.input$end1_year, "-",
                          unlist(lapply(X = cal.input$end1_month, FUN = lead0)), "-",
                          unlist(lapply(X = cal.input$end1_mday, FUN = lead0)), " ", 
                          unlist(lapply(cal.input$end1_hr, lead0)), ":",
                          unlist(lapply(cal.input$end1_min, lead0)), 
                          sep = "")%>%
    ymd_hm(., tz = Sys.timezone())
  
  cal.input
  
}

pm.ical.df[pm.ical.df$Event_Title %>% 
             grepl("FOCUS TIME - Morning Planning", ., ignore.case = F),] %>%
  .[as_date(.$Start1) %in% this.weeks.dates,]



# generate ics file from source----
new.ics.export_PM <- NULL

for(i in 1:nrow(cal.input)){
  #if(cal.input$Add_to_PM_cal[i] == T | is.na(cal.input$Add_to_PM_cal[i])){
  
  # cal.input$in_PM_cal[i] <- T
  # cal.input$Add_to_PM_cal[i] <- F
  
  new.ics.export_PM <- rbind(new.ics.export_PM, 
                             gen_ical.event(event_title = cal.input$Event_Title[i], 
                                            desc        = cal.input$Desc[i], 
                                            start1      = cal.input$Start1[i], 
                                            end1        = cal.input$End1[i]))
  #openxlsx::write.xlsx(x = cal.input, file = "tim_cal_input.xlsx")
  
  #}
}


new.ics.export_PM

# name ics file out----
file.name.out <- glue("CAL{gsub(\":\",\"_\",gsub(\" \",\"__\",Sys.time()))}.ics")

# write to ics files----
ic_write(ic = new.ics.export_PM, 
         file = file.name.out)


# remove imported files 
if(getwd() == "C:/Users/TimBender/Documents/R/ncceh/calendars"){
  # delete the old ics files
  file.remove(c("tim_MTG_calendar.ics", "tim_PM_calendar.ics"))
  cat(glue("Remaining ics files:\n * {paste(list.files(pattern = \".ics$\"), sep = \"\n * \", collapse = \"\n * \")}"))
}


# update xlsx file
write.xlsx(x = cal.input, file = "tim_cal_input.xlsx")


# return to regular wd----
setwd("C:/Users/TimBender/Documents/R/ncceh")
