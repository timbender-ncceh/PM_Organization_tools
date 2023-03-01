library(dplyr)
library(calendar)
library(lubridate)
library(ggplot2)
library(uuid)
library(glue)
library(openxlsx)
library(data.table)

# https://cran.rstudio.com/web/packages/calendar/calendar.pdf

rm(list=ls());cat('\f');gc()



# VARS/MISC----

near_term <- Sys.Date() %m+% weeks(2)

wd.calendar    <- "C:/Users/TimBender/Documents/R/ncceh/calendars"
wd.home        <- "C:/Users/TimBender/Documents/R/ncceh"
cal.excel.link <- "tim_cal_input.xlsx"

# set wd----
setwd(wd.calendar)

# Load secret calendar vars----
source("secret_ical_url.R")

# Functions----
df_sum.start.end <- function(list1 = mtg.ical.list){
  df.out <- NULL
  
  for(i in 1:length(list1)){
    temp.summary <- grep(pattern = "^SUMMARY", list1[[i]], value = T)
    if(length(temp.summary) == 0){temp.summary <- NA}
    temp.start <- grep(pattern = "^DTSTART", list1[[i]], value = T)
    if(length(temp.start) == 0){temp.start <- NA}
    temp.end <- grep(pattern = "^DTEND", list1[[i]], value = T)
    if(length(temp.end) == 0){temp.end <- NA}
    
    df.out <- rbind(df.out, 
                    data.frame(rid = i, 
                               summary = temp.summary, 
                               start = temp.start, 
                               end = temp.end))
    
    rm(temp.summary, temp.start, temp.end)
  }
  
  df.out$start_date     <- gsub(pattern = "^DTSTART;TZID=America/New_York:|^DTSTART:|^DTSTART;VALUE=DATE:", 
                                replacement = "", x = df.out$start) %>% as_date()
  df.out$start_datetime <- gsub(pattern = "^DTSTART;TZID=America/New_York:|^DTSTART:|^DTSTART;VALUE=DATE:", 
                                replacement = "", x = df.out$start) %>% as_datetime(., tz = Sys.timezone())
  df.out$end_date       <- gsub(pattern = "^DTEND;TZID=America/New_York:|^DTEND:|^DTEND;VALUE=DATE:", 
                                replacement = "", x = df.out$end) %>% as_date()
  df.out$end_datetime   <- gsub(pattern = "^DTEND;TZID=America/New_York:|^DTEND:|^DTEND;VALUE=DATE:", 
                                replacement = "", x = df.out$end) %>% as_datetime(., tz = Sys.timezone())
  
  df.out$summary <- df.out$summary %>%
    gsub("^SUMMARY:", "", .)
  
  df.out <- df.out[,c("rid", "summary", "start_date", "start_datetime", "end_date", "end_datetime")]
  
  return(as_tibble(df.out))
}
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

# Import/Tidy Meeting_Calendar----
download.file(url = secret.ical.url_MTG, 
              destfile = "tim_MTG_calendar.ics")


# Import/Tidy PM_Calendar----
download.file(url = secret.ical.url_PM, 
              destfile = "tim_PM_calendar.ics")

# Import/Tidy Excel_File of New PM Calendar items----
cal.excel.link <- openxlsx::read.xlsx(cal.excel.link) %>% as_tibble()


# AM/PM check
cal.excel.link$start1_hr[cal.excel.link$start1_hr %in% c(1:5)] <- cal.excel.link$start1_hr[cal.excel.link$start1_hr %in% c(1:5)] + 12
cal.excel.link$end1_hr[cal.excel.link$end1_hr %in% c(1:5)] <- cal.excel.link$end1_hr[cal.excel.link$end1_hr %in% c(1:5)] + 12

# if is.na(year){ assume  year == year(today)}
cal.excel.link$start1_year[is.na(cal.excel.link$start1_year)] <- year(Sys.Date())
cal.excel.link$end1_year[is.na(cal.excel.link$end1_year)] <- year(Sys.Date())

# if is.na(month){ assume  month == month(today)}
cal.excel.link$start1_month[is.na(cal.excel.link$start1_month)] <- lubridate::month(Sys.Date())
cal.excel.link$end1_month[is.na(cal.excel.link$end1_month)] <- lubridate::month(Sys.Date())

# if is.na(mday){ assume  mday == mday(today)}
cal.excel.link$start1_mday[is.na(cal.excel.link$start1_mday)] <- lubridate::mday(Sys.Date())
cal.excel.link$end1_mday[is.na(cal.excel.link$end1_mday)] <- lubridate::mday(Sys.Date())


cal.excel.link$Start1 <- paste(cal.excel.link$start1_year, "-",
                               unlist(lapply(X = cal.excel.link$start1_month, FUN = lead0)), "-",
                               unlist(lapply(X = cal.excel.link$start1_mday, FUN = lead0)), " ", 
                               unlist(lapply(cal.excel.link$start1_hr, lead0)), ":",
                               unlist(lapply(cal.excel.link$start1_min, lead0)), 
                               sep = "") %>%
  ymd_hm(., tz = Sys.timezone())
cal.excel.link$End1 <- paste(cal.excel.link$end1_year, "-",
                             unlist(lapply(X = cal.excel.link$end1_month, FUN = lead0)), "-",
                             unlist(lapply(X = cal.excel.link$end1_mday, FUN = lead0)), " ", 
                             unlist(lapply(cal.excel.link$end1_hr, lead0)), ":",
                             unlist(lapply(cal.excel.link$end1_min, lead0)), 
                             sep = "")%>%
  ymd_hm(., tz = Sys.timezone())

cal.excel.link$Desc <- as.character(cal.excel.link$Desc)


# Analysis----
# 0) remove all old .ics files from directory----
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


# 1) Get all near-term calendar items from Meeting_Calendar
mtg.ical.list <- calendar::ic_list(x = readLines("tim_MTG_calendar.ics"))

# summary, dtstart, dtend
df_mtg <- df_sum.start.end(mtg.ical.list)

df_mtg.nt <- df_mtg[between(x = df_mtg$start_date, 
        lower = Sys.Date(), 
        upper = near_term) & 
  (df_mtg$start_date == df_mtg$end_date),] %>%
  mutate(., 
         source = "MTG", 
         source_rid = paste(source,rid,sep="_"))
df_mtg.nt <- df_mtg.nt[!colnames(df_mtg.nt) %in% c("rid", "source")]


# 2) Get all near-term calendar items from PM_Calendar
pm.ical.list <- calendar::ic_list(x = readLines("tim_PM_calendar.ics"))

# summary, dtstart, dtend
df_pm <- df_sum.start.end(pm.ical.list)

df_pm.nt <- df_pm[between(x = df_pm$start_date, 
                            lower = Sys.Date(), 
                            upper = near_term) & 
                      (df_pm$start_date == df_pm$end_date),] %>%
  mutate(., 
         source = "PM", 
         source_rid = paste(source,rid,sep="_"))
df_pm.nt <- df_pm.nt[!colnames(df_pm.nt) %in% c("rid", "source")]


# 3) Get all           calendar items from Excel_File

cal.excel.link

df_excel <- cal.excel.link[,c("Event_Title", "Start1", "End1")] %>%
  mutate(., 
         rid = 1:length(End1), 
         source = "excel", 
         source_rid = paste(source,rid,sep="_"))

df_excel <- df_excel[!colnames(df_excel) %in% c("rid", "source")]
df_excel$start_date <- as_date(df_excel$Start1)
df_excel$end_date   <- as_date(df_excel$End1)

colnames(df_excel) <- c("summary", "start_datetime", "end_datetime", "source_rid", 
                        "start_date", "end_date")
df_excel <- df_excel[,c("summary", "start_date", "start_datetime", "end_date", "end_datetime", "source_rid")]

# 4) ID Meeting_Calendar items not in PM_Calendar and ADD_TO      Excel_File
# df_excel <- rbind(df_excel, 
#                   anti_join(x = df_mtg.nt, y = df_excel, 
#                             by = c("summary", "start_date", "start_datetime", "end_date", "end_datetime"))) %>%
#   group_by(summary,start_date,start_datetime,end_date,end_datetime) %>%
#   summarise(n = n()) %>%
#   .[order(.$n,decreasing = T),]

rbind(df_excel, 
      df_mtg.nt, 
      df_pm.nt) %>%
  mutate(., 
         source = gsub("_.*$", "", source_rid)) %>%
  as.data.table() %>%
  dcast(., 
        summary + start_date + 
          start_datetime + 
          end_date ~ 
          #end_datetime ~
          source, 
        fun.aggregate = length, fill = 0) %>%
  as.data.frame() %>%
  mutate(., 
         n_distinct_source = (MTG > 0) + (PM > 0) + (excel > 0)) %>%
  .[order(.$start_datetime, .$summary ),]

# 5) ID Excel_File       items     in PM_Calendar and REMOVE_FROM Excel_File



# Write new ICS file----


# Write new Excel File----


# set wd----
setwd(wd.home)
