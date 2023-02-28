# README: PM_Organization_tool

A set of R scripts that will allow you to add items to your google calendar via an ical import.  The ical (actually .ics) file is generated from a linked excel spreadsheet that has 14 fields but, really only corresponds to Start_time, End_time, Event_title, and Event_description.  

The Script will pulldown your current calendar from google (via your secret ical url), check those calendar events against the proposed items in the spreadsheet, and add items from the spreadsheet that are not already in your calendar.  Additionally, it will overwrite the excel file with a new file, omitting all records that have been verified as having been not just exported to an .ics file but that verified that ics file was imported to google previously.  

The script also does some stuff with file management, and efforts were made to make that as safe as possibe by checking directories before proceeding (and hard_coding directory names).  
