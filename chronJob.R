# -------------------------------------------------- 
# Script Name : chronJob.R
# Purpose: Automate running of Render_the_Diseases_monitoring_tables_pdf.R, which in turn runs Diseases_monitoring_tables_pdf.Rmd
# 
# Note: This uses chron job so it only works in unix or linux platform
# -----------------------------------------------------------------------
#
#' Script Status: Stable
#' _____________________
#' updates done:
#'                  1. Fully automated to run daily at 18:58
#'   

#set path for the Render_the_Diseases_monitoring_tables_pdf.R (this is the file that calls Diseases_monitoring_tables_pdf.Rmd markdown)

#change the path to the folder that contains Render_the_Diseases_monitoring_tables_pdf.R script
path= "/Users/francescgrauortiz/Googledrive1/automation_scripts/"
#install this library in case it is missing.
if(!require(cronR)) install.packages("cronR"); library(cronR)

#The script to create job on
rscript =paste0(path,"Render_the_Diseases_monitoring_tables_pdf.R")

f <- system.file(package = "cronR", "extdata", rscript)
#create the command
cmd <- cron_rscript("/Users/francescgrauortiz/Googledrive1/automation_scripts/Render_the_Diseases_monitoring_tables_pdf.R")
cmd
#add your jobs
#cron_add(cmd, frequency = 'daily', id = 'webscrapping', at = '17:23', days_of_week = c(4))

cron_add(cmd, frequency = "daily", at='17:45',days_of_week=c(2),id='webscrapping',tags = "webscrap", 
         description = "automating scripts", dry_run = F)

#cron_rm(id = "webscrapping")
