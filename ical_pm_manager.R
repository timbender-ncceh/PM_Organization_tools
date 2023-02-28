library(dplyr)
library(calendar)
library(lubridate)
library(ggplot2)
library(uuid)
library(glue)

# https://cran.rstudio.com/web/packages/calendar/calendar.pdf

rm(list=ls());cat('\f')

# funs----
ic_guid <- function(n=1){
  # overwrites built-in function of ic_guid in library(calendar) with a custom uuid-generating 
  # function that uses the library(uuid)
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

# set calendar wd----
setwd("C:/Users/TimBender/Documents/R/ncceh/calendars")


# download meeitng calendar----
source("secret_ical_url.R")

# remove all old .ics files---
old_ics_files <- list.files(pattern = "\\.ics$") %>%
  .[!. %in% c("tim.bender@ncceh.org.ics", 
              "tim_meeting_calendar.ics")]

# download gmail calendar----
download.file(url = "https://calendar.google.com/calendar/ical/tim.bender%40ncceh.org/private-afa899746cf309779ec61d9410336532/basic.ics", 
              destfile = "tim_meeting_calendar.ics")


# delete old .ics files----

# confirm that wd checks out 
if(getwd() == "C:/Users/TimBender/Documents/R/ncceh/calendars"){
  # delete the old ics files
  file.remove(old_ics_files)
}

# generate ics file from source----
multi_ics <- rbind(gen_ical.event(event_title = "<title>", 
                                  desc        = "<desc>", 
                                  start1      = ymd_hm("20230301 07:30AM", tz = Sys.timezone()), 
                                  end1        = 1, # default = 1hr
                                  my.tz       = "America/New_York"))


# name ics file out----
file.name.out <- glue("CAL{gsub(\":\", \"_\", gsub(\" \", \"__\", ymd_hm(\"20230301 08:00 am\", tz = Sys.timezone())))}.ics")


# write to ics files----
ic_write(ic = multi_ics, 
         file = file.name.out)

# return to regular wd----
setwd("C:/Users/TimBender/Documents/R/ncceh")
