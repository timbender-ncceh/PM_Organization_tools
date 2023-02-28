library(dplyr)
library(calendar)
library(lubridate)
library(ggplot2)
library(uuid)
library(glue)
library(openxlsx)

# https://cran.rstudio.com/web/packages/calendar/calendar.pdf

rm(list=ls());cat('\f')

# funs----
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

# tidy
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


# cal.input$in_PM_cal <- as.logical(cal.input$in_PM_cal)
# cal.input$Add_to_PM_cal <- as.logical(cal.input$Add_to_PM_cal)
# cal.input$in_MTG_cal <- as.logical(cal.input$in_MTG_cal)
# 
# cal.input$in_MTG_cal    <- ifelse(is.na(cal.input$in_MTG_cal), F, cal.input$in_MTG_cal)
# cal.input$in_PM_cal     <- ifelse(is.na(cal.input$in_PM_cal), F, cal.input$in_PM_cal)
# cal.input$Add_to_PM_cal <- ifelse(is.na(cal.input$Add_to_PM_cal), T, cal.input$Add_to_PM_cal)



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
cal.input

pm.ical.list <- calendar::ic_list(x = readLines("tim_PM_calendar.ics"))
mtg.ical.list <- calendar::ic_list(x = readLines("tim_MTG_calendar.ics"))

pm.ical.df <- ical_list2df(pm.ical.list) %>% as_tibble()
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
file.name.out <- glue("CAL{gsub(\":\", \"_\", gsub(\" \", \"__\", ymd_hm(\"20230301 08:00 am\", tz = Sys.timezone())))}.ics")


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
