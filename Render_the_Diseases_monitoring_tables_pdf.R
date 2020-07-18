# -------------------------------------------------- 
# Script Name : Render_the_Diseases_monitoring_tables_pdf.R
# Purpose:     Runs Diseases_monitoring_tables_pdf.Rmd markdown file
# 
# Script status: Stable
# --------------------------------------------------


#install this library in case it is missing.
if(!require(rmarkdown)) install.packages("rmarkdown"); library(rmarkdown)

#change the path to the folder that contains Diseases_monitoring_tables_pdf_auto.Rmd rmarkdown file

path<- "C:/Users/Lenovo/googledrive1/scripts/"

#render the file (just like running the file)
rmarkdown::render(paste0(path,"Diseases_monitoring_tables_pdf_auto.Rmd"))


