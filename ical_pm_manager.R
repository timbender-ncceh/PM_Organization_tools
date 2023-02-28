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

lead0 <- function(x){
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

# set calendar wd
setwd("C:/Users/TimBender/Documents/R/ncceh/calendars")

# Vars----
cal.input.link <- "tim_cal_input.xlsx"

# Import Data----
# download google calendar
source("secret_ical_url.R")

download.file(url = secret.ical.url_MTG, 
              destfile = "tim_MTG_calendar.ics")
download.file(url = secret.ical.url_PM, 
              destfile = "tim_PM_calendar.ics")

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


cal.input$in_PM_cal <- as.logical(cal.input$in_PM_cal)
cal.input$Add_to_PM_cal <- as.logical(cal.input$Add_to_PM_cal)
cal.input$in_MTG_cal <- as.logical(cal.input$in_MTG_cal)

cal.input$in_MTG_cal    <- ifelse(is.na(cal.input$in_MTG_cal), F, cal.input$in_MTG_cal)
cal.input$in_PM_cal     <- ifelse(is.na(cal.input$in_PM_cal), F, cal.input$in_PM_cal)
cal.input$Add_to_PM_cal <- ifelse(is.na(cal.input$Add_to_PM_cal), T, cal.input$Add_to_PM_cal)


# remove all old .ics files
old_ics_files <- list.files(pattern = "\\.ics$") %>%
  .[!. %in% c(#"tim.bender@ncceh.org.ics", 
              #"tim_meeting_calendar.ics", 
              "tim_MTG_calendar.ics", 
              "tim_PM_calendar.ics")]
# confirm that wd checks out 
if(getwd() == "C:/Users/TimBender/Documents/R/ncceh/calendars"){
  # delete the old ics files
  file.remove(old_ics_files)
  cat(glue("Remaining ics files:\n * {paste(list.files(pattern = \".ics$\"), sep = \"\n * \", collapse = \"\n * \")}"))
}

# generate ics file from source----
new.ics.export_PM <- NULL

for(i in 1:nrow(cal.input)){
  if(cal.input$Add_to_PM_cal[i] == T | is.na(cal.input$Add_to_PM_cal[i])){
    
    # cal.input$in_PM_cal[i] <- T
    # cal.input$Add_to_PM_cal[i] <- F
    
    new.ics.export_PM <- rbind(new.ics.export_PM, 
                               gen_ical.event(event_title = cal.input$Event_Title[i], 
                                              desc        = cal.input$Desc[i], 
                                              start1      = cal.input$Start1[i], 
                                              end1        = cal.input$End1[i]))
    #openxlsx::write.xlsx(x = cal.input, file = "tim_cal_input.xlsx")
    
  }
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


# return to regular wd----
setwd("C:/Users/TimBender/Documents/R/ncceh")
