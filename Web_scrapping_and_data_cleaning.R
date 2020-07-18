# -------------------------------------------------- 
# Script Name : Web_scrapping_and data_cleaning.R
# Purpose: Scrapping of websites for data;then clean and output csv files
# 
#    											
# --------------------------------------------------

#' Script Status: Stable
#' Last updates done:
#'                  1. Output datasets: disease_dataset.xlsx and disease_dataset.xlsx for use in shiny Application
#'                  2. Added Indonesia floods datasets
#'                  3. Wrapped all sites extraction code inside functions then wrapped the function call inside trycatchlog





#' Load the libraries
#'
library("rvest")
library(tabulizer)
library(dplyr)
library(miniUI)
library(stringr)
library(tidyverse)
library(pdftools)
#install rlist if it is missing then load it
if(!require(rlist)) install.packages("rlist"); library(rlist)
library(xlsx)
library(condformat)
library("readxl")
library(tidyr)
library(tidyverse)
library(DT)
library(reshape2)
library("ggplot2")
library(lubridate)
library('rapportools')
library("XML")
library(plyr)
library(data.table)
library(googlesheets4)
library(gargle)
#install this library in case it is missing.
if(!require(tryCatchLog)) install.packages("tryCatchLog"); library(tryCatchLog)

#' Change directory to point to data folder
#' 
mypath<- "/home/ubuntu/Projects/data/" 

#' ----------------------------------------------------------------------------------------------
#' scrapping Dengue cases for Nepal
#' 
#' 
#' ----------------------------------------------------------------------------------------------
#' 

#Creating a function to wrap napel dengue extaction

#napel dengue function begins here
napel_dengue <- function(){

  
site <- "https://www.edcd.gov.np/ewars"
#wrap read_html into try function and suppress error. This ensures that if website fails 
#to respond on first request, the second  and third request is send

#set webpage to null to help in the loop
webpage <- NULL
trial <- 1
while( is.null(webpage) && trial <= 3 ) {
  trial <- trial + 1
  try(
    webpage <- read_html(site),silent = TRUE
  )
}

webpage <- webpage %>% html_nodes("span") %>% html_nodes("a") %>% html_attr("href")

#set url to null to help in the loop
url <- NULL
trial <- 1
while( is.null(url) && trial <= 3 ) {
  trial <- trial + 1
  try(
    url <- pdf_text(webpage[1]),silent = TRUE
  )
}

date_slipt <- url[1] %>% str_split("\r", simplify = TRUE) %>% str_split("\n", simplify = TRUE)
date <- trimws(date_slipt [3,2])
save_lastupdate<-paste("Last update: ", date)
month1 <- strsplit(date, " ")
month2 <- ifelse(length(month1[[1]]) == 2, gsub('[0-9]+', '', month1[[1]][1]), month1[[1]][2])
data_slipt <- url[2] %>% str_split("\r", simplify = TRUE) %>% grep('Dengue|dengue', ., value = T) 
data1 <-  gsub("[\n]", "", data_slipt ) 
data2 <- gsub("^ *|(?<= ) | *$", "", data1, perl = TRUE)
data3 <- data2[2] %>% str_split(" ", simplify = TRUE)
nepal_cases <- data3[1,2]


#word_to_number function is defined in dependancies script
total <- ifelse(nepal_cases == "No", 0, nepal_cases)
total <- as.numeric(total)
total <- ifelse(is.na(total) == TRUE, word_to_number(nepal_cases), total)
total <- as.numeric(total)
#'Read in previous dataset to be updated

my_data <- read_excel(paste0(mypath,"Nepal_dengue.xlsx"))
my_data_date <- my_data
new_cases <- ifelse(date != my_data_date[13,5], total, 0)
save_new_case<-paste("New Cases: ", new_cases)
my_data <- my_data[-c(13), ] 
my_data <- transform(my_data, `2020` = as.numeric(`2020`))
my_data <- my_data %>% dplyr::rename("2017" =  "X2017", "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")

means <- data.frame(ID=my_data[,1], Means=rowMeans(my_data[,-1], na.rm=TRUE))

means$ID <- as.character(means$ID)

means <- means %>% mutate(Above = means$Means*1.25, Below = means$Means*0.75)

my_data[1, '2020'] <- ifelse(date != my_data_date[13,5] & month2 == "January", my_data$`2020`[1]+total, my_data$`2020`[1]+0)
my_data[2, '2020'] <- ifelse(date != my_data_date[13,5] & month2 == "February", my_data$`2020`[2]+total, my_data$`2020`[2]+0)
my_data[3, '2020'] <- ifelse(date != my_data_date[13,5] & month2 == "March", my_data$`2020`[3]+total, my_data$`2020`[3]+0)
my_data[4, '2020'] <- ifelse(date != my_data_date[13,5] & month2 == "April", my_data$`2020`[4]+total, my_data$`2020`[4]+0)
my_data[5, '2020'] <- ifelse(date != my_data_date[13,5] & month2 == "May", my_data$`2020`[5]+total, my_data$`2020`[5]+0)
my_data[6, '2020'] <- ifelse(date != my_data_date[13,5] & month2 == "June", my_data$`2020`[6]+total, my_data$`2020`[6]+0)
my_data[7, '2020'] <- ifelse(date != my_data_date[13,5] & month2 == "July", my_data$`2020`[7]+total, my_data$`2020`[7]+0)
my_data[8, '2020'] <- ifelse(date != my_data_date[13,5] & month2 == "August", my_data$`2020`[8]+total, my_data$`2020`[8]+0)
my_data[9, '2020'] <- ifelse(date != my_data_date[13,5] & month2 == "September", my_data$`2020`[9]+total, my_data$`2020`[9]+0)
my_data[10, '2020'] <- ifelse(date != my_data_date[13,5] & month2 == "October", my_data$`2020`[10]+total, my_data$`2020`[10]+0)
my_data[11, '2020'] <- ifelse(date != my_data_date[13,5] & month2 == "November", my_data$`2020`[11]+total, my_data$`2020`[11]+0)
my_data[12, '2020'] <- ifelse(date != my_data_date[13,5] & month2 == "December", my_data$`2020`[12]+total, my_data$`2020`[12]+0)

#store date of last update into the dataset
my_data[13, '2020'] <- date
data_date_nepal <- my_data
#write external file after adding new cases to it

write.xlsx(data_date_nepal, paste0(mypath,"Nepal_dengue.xlsx"), sheetName="Sheet1", 
           col.names=TRUE, row.names=FALSE, append=FALSE)

my_data <- my_data[-c(13), ]
my_data <- transform(my_data, `2020` = as.numeric(`2020`))
my_data <- my_data %>% dplyr::rename("2017" =  "X2017", "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")

#merge the means and my_data based on month

my_data<- inner_join(my_data,means,by=c("Month"="ID"))

my_data$seventen_mean <- ifelse(my_data$`2017` >my_data$Above, 2,
                                ifelse(my_data$`2017` < my_data$Below, 0,
                                       1))
my_data$eigthten_mean <- ifelse(my_data$`2018` > my_data$Above, 2,
                                ifelse(my_data$`2018` < my_data$Below, 0,
                                       1))
my_data$nineten_mean <- ifelse(my_data$`2019` > my_data$Above, 2,
                               ifelse(my_data$`2019` < my_data$Below, 0,
                                      1))
my_data$twente_mean <- ifelse(my_data$`2020` > my_data$Above, 2,
                              ifelse(my_data$`2020` < my_data$Below, 0,
                                     1))
my_data<- dplyr::select(my_data,-c(Means,Above,Below))
#save lastupdate and new cases as last rows. This will be extracted and used in rmardown
my_data[13,1]<-save_lastupdate
my_data[14,1]<-save_new_case
#save data to be used in rmardown
saveRDS(my_data, file =paste0(mypath,"save_data_nepal_dengue.Rdata"))

#end of napel dengue function
}

#calling napeldengue function inside trycatchlog

tryCatchLog(napel_dengue(),error = function(e) {})

#' ----------------------------------------------------------------------------------------------
#' scrapping Dengue cases for Sirlanka
#' 
#' 
#' ----------------------------------------------------------------------------------------------
#' 


#writing a function to wrap sirlanka dengue extraction

#begin of sirlanka_dengue function
sirlanka_dengue <- function(){
  
site <- "http://www.epid.gov.lk/web/index.php?option=com_casesanddeaths&Itemid=448&lang=en"

webpage <- NULL
trial <- 1
while( is.null(webpage) && trial <= 3 ) {
  trial <- trial + 1
  try(
    webpage <- read_html(site),silent = TRUE
  )
}

Date <- webpage %>% html_nodes("form") %>% html_nodes("div")  %>% html_nodes("span")  %>% html_text() %>% grep('Date', ., value = T)
save_lastupdate<-paste("Last update: ", Date)
data <- webpage %>% html_nodes(xpath='//*[@id="rt-mainbody"]/form/table[2]') %>% html_table()

my_data <- read_excel(paste0(mypath,"sri_lanka_dengue.xlsx"))

my_data <- my_data[-c(13:19), ]
my_data_before <- sum(my_data$`2020`)
means <- data.frame(ID=my_data[,1], Means=rowMeans(my_data[,-1], na.rm=TRUE))

means$Month <- as.character(means$Month)

means <- means %>% mutate(Above = means$Means*1.25, Below = means$Means*0.75)

my_data[1, '2020'] <- data[[1]]$X2[1]
my_data[2, '2020'] <- data[[1]]$X2[2]
my_data[3, '2020'] <- data[[1]]$X2[3]
my_data[4, '2020'] <- data[[1]]$X2[4]
my_data[5, '2020'] <- data[[1]]$X2[5]
my_data[6, '2020'] <- data[[1]]$X2[6]
my_data[7, '2020'] <- data[[1]]$X2[7]
my_data[8, '2020'] <- data[[1]]$X2[8]
my_data[9, '2020'] <- data[[1]]$X2[9]
my_data[10, '2020'] <- data[[1]]$X2[10]
my_data[11, '2020'] <- data[[1]]$X2[11]
my_data[12, '2020'] <- data[[1]]$X2[12]

save_data_srilanka <- as.data.frame(my_data)
#' Save data to be used in ggplot
saveRDS(save_data_srilanka, file =paste0(mypath,"sri_lanka_dengue_ggplot.Rdata"))

#write data to excel for the subsequent update when the site updates data
write.xlsx(save_data_srilanka, paste0(mypath,"sri_lanka_dengue.xlsx"), sheetName="Sheet1", 
           col.names=TRUE, row.names=FALSE, append=FALSE)

my_data_now <- sum(save_data_srilanka$`2020`)
new_cases <- my_data_now - my_data_before
save_new_case<-paste("New Cases: ", new_cases)

#merge means

my_data<- inner_join(my_data,means,by=c("Month"))

my_data$ten_mean <- ifelse(my_data$`2010` > my_data$Above, 2,
                           ifelse(my_data$`2010` < my_data$Below, 0,
                                  1))

my_data$eleven_mean <- ifelse(my_data$`2011` > my_data$Above, 2,
                              ifelse(my_data$`2011` < my_data$Below, 0,
                                     1))

my_data$twelve_mean <- ifelse(my_data$`2012` > my_data$Above, 2,
                              ifelse(my_data$`2012` < my_data$Below, 0,
                                     1))

my_data$thirdten_mean <- ifelse(my_data$`2013` > my_data$Above, 2,
                                ifelse(my_data$`2013` < my_data$Below, 0,
                                       1))
my_data$fourthten_mean <- ifelse(my_data$`2014` > my_data$Above, 2,
                                 ifelse(my_data$`2014` < my_data$Below, 0,
                                        1))
my_data$fiveten_mean <- ifelse(my_data$`2015` > my_data$Above, 2,
                               ifelse(my_data$`2015` < my_data$Below, 0,
                                      1))
my_data$sixten_mean <- ifelse(my_data$`2016` > my_data$Above, 2,
                              ifelse(my_data$`2016` < my_data$Below, 0,
                                     1))
my_data$seventen_mean <- ifelse(my_data$`2017` > my_data$Above, 2,
                                ifelse(my_data$`2017` < my_data$Below, 0,
                                       1))
my_data$eigthten_mean <- ifelse(my_data$`2018` > my_data$Above, 2,
                                ifelse(my_data$`2018` < my_data$Below, 0,
                                       1))
my_data$nineten_mean <- ifelse(my_data$`2019` > my_data$Above, 2,
                               ifelse(my_data$`2019` < my_data$Below, 0,
                                      1))
my_data$twente_mean <- ifelse(my_data$`2020` > my_data$Above, 2,
                              ifelse(my_data$`2020` < my_data$Below, 0,
                                     1))
my_data<- dplyr::select(my_data,-c(Means,Above,Below))
#save new cases and last update date as last two rows. This will be used in rmarkdown
my_data[13,1]<-save_lastupdate
my_data[14,1]<-save_new_case
saveRDS(my_data, file =paste0(mypath,"sri_lanka_dengue.Rdata"))

#end of sirlanka dengue function

}

#calling sirlanka_dengue function inside trycatchlog

tryCatchLog(sirlanka_dengue(),error = function(e) {})

#' ----------------------------------------------------------------------------------------------
#' scrapping dengue data from fiji
#' 
#' 
#' ----------------------------------------------------------------------------------------------
#' 
#' Creating a function to wrap fiji dengue extaction
#' 
#begin of fiji dengue function 
fiji_dengue <- function(){
   
site <- "https://reliefweb.int/updates?search=Pacific+Syndromic+Surveillance+System+Weekly+Bulletin"
webpage <- read_html(site) %>% html_nodes("h4") %>% html_nodes("a") %>% html_attr("href")
web <- read_html(webpage[1]) %>% html_nodes("ul") %>% html_nodes("li") %>%  html_nodes("a") %>% html_attr("href")
date1 <- pdf_text(web[11])[1]
date_slipt <- date1 %>% str_split("\r\n", simplify = TRUE) 
date <- trimws(date_slipt[,5])
save_lastupdate<-paste("Last update: ", date)
month1 <- strsplit(date, " ")
month2 <- month1[[1]][3]
df_results <- extract_tables(web[11], pages = 3)
data <- as.data.frame(df_results)
data <- data[-c(1,2), ]
data <- data[!sapply(data, function(x) all(x == ""))]
final <- data[ , c(1, length( names( data ) ) ) ] %>% 
   dplyr::rename(
     "Countries" = X1,
     "Dengue Cases" = colnames(data)[ncol(data)]
   )
 final1 <- final %>% filter(
   Countries == "French Polynesia" | Countries == "Fiji" | Countries == "Marshall Islands"
 )
 
 finalFiji1<- as.character(final1[1,2])
 total<- as.numeric(finalFiji1)
 
 my_data <- read_excel(paste0(mypath,"Fiji_dengue.xlsx"))
 
 my_data_date <- my_data
 new_cases <- ifelse(date != my_data_date[13,4], total, 0)
 save_new_case<-paste("New Cases: ", new_cases)
 my_data <- my_data[-c(13), ] 
 my_data <- transform(my_data, `2020` = as.numeric(`2020`))
 my_data <- my_data %>% dplyr::rename( "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")
 
 means <- data.frame(Month=my_data[,1], Means=rowMeans(my_data[,-1], na.rm=TRUE))
 means <- means %>% mutate(Above = means$Means*1.25, Below = means$Means*0.75)
 #convert month to characters from factor
 means$Month <- as.character(means$Month)
 my_data <- as.data.frame(my_data)
 

 my_data[1, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Jan", my_data$`2020`[1]+total, my_data$`2020`[1]+0)
 my_data[2, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Feb", my_data$`2020`[2]+total, my_data$`2020`[2]+0)
 my_data[3, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Mar", my_data$`2020`[3]+total, my_data$`2020`[3]+0)
 my_data[4, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Apr", my_data$`2020`[4]+total, my_data$`2020`[4]+0)
 my_data[5, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(May", my_data$`2020`[5]+total, my_data$`2020`[5]+0)
 my_data[6, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Jun", my_data$`2020`[6]+total, my_data$`2020`[6]+0)
 my_data[7, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Jul", my_data$`2020`[7]+total, my_data$`2020`[7]+0)
 my_data[8, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Aug", my_data$`2020`[8]+total, my_data$`2020`[8]+0)
 my_data[9, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Sep", my_data$`2020`[9]+total, my_data$`2020`[9]+0)
 my_data[10, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Oct", my_data$`2020`[10]+total, my_data$`2020`[10]+0)
 my_data[11, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Nov", my_data$`2020`[11]+total, my_data$`2020`[11]+0)
 my_data[12, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Dec", my_data$`2020`[12]+total, my_data$`2020`[12]+0)
 
 #save externally for use in ggplot later
 save_data <- my_data
 saveRDS(save_data, file = paste0(mypath,"save_data_fiji_dengue_ggplot.Rdata"))
 
 my_data[13, '2020'] <- date
 data_date_fiji <- my_data
 
 #write to excel for subsequent update when data is updated by the site
 write.xlsx(data_date_fiji, paste0(mypath,"Fiji_dengue.xlsx"), sheetName="Sheet1", 
            col.names=TRUE, row.names=FALSE, append=FALSE)

 my_data <- my_data[-c(13), ]
 my_data <- transform(my_data, `2020` = as.numeric(`2020`))
 my_data <- my_data %>% dplyr::rename( "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")
 #merge my_data together with means
 my_data<- inner_join(my_data,means, by="Month")
 
 my_data$eigthten_mean <- ifelse(my_data$`2018` > my_data$Above, 2,
                                 ifelse(my_data$`2018` < my_data$Below, 0,
                                        1))
 my_data$nineten_mean <- ifelse(my_data$`2019` > my_data$Above, 2,
                                ifelse(my_data$`2019` < my_data$Below, 0,
                                       1))
 my_data$twente_mean <- ifelse(my_data$`2020` > my_data$Above, 2,
                               ifelse(my_data$`2020` < my_data$Below, 0,
                                      1))
 #save the processed data 
 my_data<- dplyr::select(my_data,-c(Means,Above,Below))
 #save last update date and new cases as last two rows. This will be used in rmarkdown
 my_data[13,1]<-save_lastupdate
 my_data[14,1]<-save_new_case
 saveRDS(my_data, file = paste0(mypath,"save_data_fiji_dengue.Rdata"))
 

 }
 
 # calling fiji dengue function inside trycatchlog
 
 tryCatchLog(fiji_dengue(),error = function(e) {})



#' ----------------------------------------------------------------------------------------------
#' scrapping Dengue cases in French Polynesia:
#' 
#' 
#' ----------------------------------------------------------------------------------------------
#' Creating a function to wrap French Polynesia dengue extaction

#begining of french_polynesia_dengue function
 french_polynesia_dengue <- function(){

 site <- "https://reliefweb.int/updates?search=Pacific+Syndromic+Surveillance+System+Weekly+Bulletin"
 webpage <- read_html(site) %>% html_nodes("h4") %>% html_nodes("a") %>% html_attr("href")
 web <- read_html(webpage[1]) %>% html_nodes("ul") %>% html_nodes("li") %>%  html_nodes("a") %>% html_attr("href")
 date1 <- pdf_text(web[11])[1]
 date_slipt <- date1 %>% str_split("\r\n", simplify = TRUE) 
 date <- trimws(date_slipt[,5])
 save_lastupdate<-paste("Last update: ", date)
 month1 <- strsplit(date, " ")
 month2 <- month1[[1]][3]
 df_results <- extract_tables(web[11], pages = 3)
 data <- as.data.frame(df_results)
 data <- data[-c(1,2), ]
 data <- data[!sapply(data, function(x) all(x == ""))]
 final <- data[ , c(1, length( names( data ) ) ) ] %>% 
   dplyr::rename(
     "Countries" = X1,
     "Dengue Cases" = colnames(data)[ncol(data)]
   )
 final1 <- final %>% filter(
   Countries == "French Polynesia" | Countries == "Fiji" | Countries == "Marshall Islands"
 )
 
 finalFP<- as.character(final1[2,2])
 total<- as.numeric(finalFP)
 
 my_data <- read_excel(paste0(mypath,"French_Polynesya_dengue.xlsx"))
 
 my_data_date <- my_data
 new_cases <- ifelse(date != my_data_date[13,4], total, 0)
 save_new_case<-paste("New Cases: ", new_cases)
 my_data <- my_data[-c(13), ] 
 my_data <- transform(my_data, `2020` = as.numeric(`2020`))
 my_data <- my_data %>% dplyr::rename( "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")
 
 means <- data.frame(Month=my_data[,1], Means=rowMeans(my_data[,-1], na.rm=TRUE))
 #convert month to characters from factor
 means$Month <- as.character(means$Month)
 means <- means %>% mutate(Above = means$Means*1.25, Below = means$Means*0.75)
 my_data <- as.data.frame(my_data)
 
 
 my_data[1, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Jan", my_data$`2020`[1]+total, my_data$`2020`[1]+0)
 my_data[2, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Feb", my_data$`2020`[2]+total, my_data$`2020`[2]+0)
 my_data[3, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Mar", my_data$`2020`[3]+total, my_data$`2020`[3]+0)
 my_data[4, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Apr", my_data$`2020`[4]+total, my_data$`2020`[4]+0)
 my_data[5, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(May", my_data$`2020`[5]+total, my_data$`2020`[5]+0)
 my_data[6, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Jun", my_data$`2020`[6]+total, my_data$`2020`[6]+0)
 my_data[7, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Jul", my_data$`2020`[7]+total, my_data$`2020`[7]+0)
 my_data[8, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Aug", my_data$`2020`[8]+total, my_data$`2020`[8]+0)
 my_data[9, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Sep", my_data$`2020`[9]+total, my_data$`2020`[9]+0)
 my_data[10, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Oct", my_data$`2020`[10]+total, my_data$`2020`[10]+0)
 my_data[11, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Nov", my_data$`2020`[11]+total, my_data$`2020`[11]+0)
 my_data[12, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Dec", my_data$`2020`[12]+total, my_data$`2020`[12]+0)
 
 #save data to used for ggplot later
 save_data <- my_data
 saveRDS(save_data, file = paste0(mypath,"french_polynesia_dengue_ggplot.Rdata"))
 
 my_data[13, '2020'] <- date
 data_date_french_polynesia <- my_data
 #write data to excel for future update when the site updates the data.
 
 write.xlsx(data_date_french_polynesia, paste0(mypath,"French_Polynesya_dengue.xlsx"), sheetName="Sheet1", 
            col.names=TRUE, row.names=FALSE, append=FALSE)
 
 my_data <- my_data[-c(13), ]
 my_data <- transform(my_data, `2020` = as.numeric(`2020`))
 my_data <- my_data %>% dplyr::rename( "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")
 
 #merge my_data with means datset
 my_data <- inner_join(my_data,means, by="Month")
 
 my_data$eigthten_mean <- ifelse(my_data$`2018` > my_data$Above, 2,
                                 ifelse(my_data$`2018` < my_data$Below, 0,
                                        1))
 my_data$nineten_mean <- ifelse(my_data$`2019` > my_data$Above, 2,
                               ifelse(my_data$`2019` < my_data$Below, 0,
                                       1))
 my_data$twente_mean <- ifelse(my_data$`2020` > my_data$Above, 2,
                               ifelse(my_data$`2020` < my_data$Below, 0,
                                      1))
 #save the processed data
 my_data<- dplyr::select(my_data,-c(Means,Above,Below))
 #last update date and new cases number into the last two rows. This will be used in rmarkdown
 my_data[13,1]<-save_lastupdate
 my_data[14,1]<-save_new_case
 
 saveRDS(my_data, file = paste0(mypath,"french_polynesia_dengue.Rdata"))
 
 
}

 #' Calling french polynesia dengue function inside trycatchlog
 
 tryCatchLog(french_polynesia_dengue(),error = function(e) {})

#' ----------------------------------------------------------------------------------------------
#' scrapping Dengue cases in Marshall Islands:
#' 
#' 
#' ----------------------------------------------------------------------------------------------
#' 
#' Creating a function to wrap Marshall Islands extaction
#' 
#begin of marshall islands dengue function
 marshall_islands_dengue <- function(){
   
 
 site <- "https://reliefweb.int/updates?search=Pacific+Syndromic+Surveillance+System+Weekly+Bulletin"
 webpage <- read_html(site) %>% html_nodes("h4") %>% html_nodes("a") %>% html_attr("href")
 web <- read_html(webpage[1]) %>% html_nodes("ul") %>% html_nodes("li") %>%  html_nodes("a") %>% html_attr("href")
 date1 <- pdf_text(web[11])[1]
 date_slipt <- date1 %>% str_split("\r\n", simplify = TRUE) 
 date <- trimws(date_slipt[,5])
 save_lastupdate<-paste("Last update: ", date)
 month1 <- strsplit(date, " ")
 month2 <- month1[[1]][3]
 df_results <- extract_tables(web[11], pages = 3)
 data <- as.data.frame(df_results)
 data <- data[-c(1,2), ]
 data <- data[!sapply(data, function(x) all(x == ""))]
 final <- data[ , c(1, length( names( data ) ) ) ] %>% 
   dplyr::rename(
     "Countries" = X1,
     "Dengue Cases" = colnames(data)[ncol(data)]
   )
 final1 <- final %>% filter(
   Countries == "French Polynesia" | Countries == "Fiji" | Countries == "Marshall Islands"
 )
 
 finalMI<- as.character(final1[3,2])
 total<- as.numeric(finalMI)
 
 my_data <- read_excel(paste0(mypath,"Marshall_Islands_dengue.xlsx"))
 
 my_data_date <- my_data
 new_cases <- ifelse(date != my_data_date[13,4], total, 0)
 save_new_case<-paste("New Cases: ", new_cases)
 my_data <- my_data[-c(13), ] 
 my_data <- transform(my_data, `2020` = as.numeric(`2020`))
 my_data <- my_data %>% dplyr::rename( "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")
 
 means <- data.frame(Month=my_data[,1], Means=rowMeans(my_data[,-1], na.rm=TRUE))
 #convert month to characters from factors
 means$Month <- as.character(means$Month)
 means <- means %>% mutate(Above = means$Means*1.25, Below = means$Means*0.75)
 my_data <- as.data.frame(my_data)
 
 
 my_data[1, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Jan", my_data$`2020`[1]+total, my_data$`2020`[1]+0)
 my_data[2, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Feb", my_data$`2020`[2]+total, my_data$`2020`[2]+0)
 my_data[3, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Mar", my_data$`2020`[3]+total, my_data$`2020`[3]+0)
 my_data[4, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Apr", my_data$`2020`[4]+total, my_data$`2020`[4]+0)
 my_data[5, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(May", my_data$`2020`[5]+total, my_data$`2020`[5]+0)
 my_data[6, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Jun", my_data$`2020`[6]+total, my_data$`2020`[6]+0)
 my_data[7, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Jul", my_data$`2020`[7]+total, my_data$`2020`[7]+0)
 my_data[8, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Aug", my_data$`2020`[8]+total, my_data$`2020`[8]+0)
 my_data[9, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Sep", my_data$`2020`[9]+total, my_data$`2020`[9]+0)
 my_data[10, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Oct", my_data$`2020`[10]+total, my_data$`2020`[10]+0)
 my_data[11, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Nov", my_data$`2020`[11]+total, my_data$`2020`[11]+0)
 my_data[12, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "(Dec", my_data$`2020`[12]+total, my_data$`2020`[12]+0)
 
 
 #save data to used for ggplot later
 save_data <- my_data
 saveRDS(save_data, file = paste0(mypath,"marshall_dengue_ggplot.Rdata"))
 
 my_data[13, '2020'] <- date
 data_date_marshall <- my_data
 
 #write data to excel for future update when the site updates the data.
 write.xlsx(data_date_marshall,paste0(mypath,"Marshall_Islands_dengue.xlsx"), sheetName="Sheet1", 
            col.names=TRUE, row.names=FALSE, append=FALSE)

 my_data <- my_data[-c(13), ]
 my_data <- transform(my_data, `2020` = as.numeric(`2020`))
 my_data <- my_data %>% dplyr::rename( "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")
 
 #merging my_data with means
 
 my_data<- inner_join(my_data,means,by="Month")
 
 my_data$eigthten_mean <- ifelse(my_data$`2018` > my_data$Above, 2,
                                ifelse(my_data$`2018` < my_data$Below, 0,
                                        1))
 my_data$nineten_mean <- ifelse(my_data$`2019` > my_data$Above, 2,
                                ifelse(my_data$`2019` < my_data$Below, 0,
                                       1))
 my_data$twente_mean <- ifelse(my_data$`2020` > my_data$Above, 2,
                               ifelse(my_data$`2020` < my_data$Below, 0,
                                      1))

 #save the processed data
 my_data<- dplyr::select(my_data,-c(Means,Above,Below))
 #last update date and new cases number into the last two rows. This will be used in rmarkdown
 my_data[13,1]<-save_lastupdate
 my_data[14,1]<-save_new_case
 saveRDS(my_data, file = paste0(mypath,"marshall_dengue.Rdata"))
 
 #end of marshall dengue function
 }
 
 #calling marshall_islands_dengue inside trycatchlog
 
 tryCatchLog(marshall_islands_dengue(),error = function(e) {})

#' ----------------------------------------------------------------------------------------------
#' scrapping Dengue cases in Malaysia
#' 
#' 
#' ----------------------------------------------------------------------------------------------
#' 

#' Creating a function to wrap malaysia dengue extaction
#' 
#begin of malaysia_dengue function
malaysia_dengue <- function(){
  
site <- "http://idengue.arsm.gov.my/"
web <- site %>% read_html() %>% html_nodes("table")
Date <- web[1]%>% html_nodes("th")%>% html_nodes("span") 
date <- Date[3] %>% html_text %>% trimws()
save_lastupdate<-paste("Last update: ", date)
month1 <- strsplit(date, " ")
month2 <- month1[[1]][2]


my_data <- read_excel(paste0(mypath,"Malaysia_Dengue.xlsx"))
#my_data[1] <- NULL
my_data_date <- my_data
my_data <- my_data[-c(13), ] 
my_data <- transform(my_data, `2020` = as.numeric(`2020`))
my_data <- my_data %>% dplyr::rename( "2016" =  "X2016", "2017" =  "X2017", "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")
data <- web[2]%>% html_nodes("td")
total1 <- sum(my_data$`2020`)
total2 <- data[3]%>% html_text %>% trimws()
total2 <- gsub(",","",total2)
total2 <- as.numeric(total2)
total <- total2 - total1
new_cases <- ifelse(date != my_data_date[13,6], total, 0)
save_new_case<-paste("New Cases: ", new_cases)


means <- data.frame(Month=my_data[,1], Means=rowMeans(my_data[,-1], na.rm=TRUE))
means$Month <- as.character(means$Month)
means <- means %>% mutate(Above = means$Means*1.25, Below = means$Means*0.75)



my_data[1, '2020'] <- ifelse(date != my_data_date[13,6] & month2 == "Jan", my_data$`2020`[1]+total, my_data$`2020`[1]+0)
my_data[2, '2020'] <- ifelse(date != my_data_date[13,6] & month2 == "Feb", my_data$`2020`[2]+total, my_data$`2020`[2]+0)
my_data[3, '2020'] <- ifelse(date != my_data_date[13,6] & month2 == "Mar", my_data$`2020`[3]+total, my_data$`2020`[3]+0)
my_data[4, '2020'] <- ifelse(date != my_data_date[13,6] & month2 == "Apr", my_data$`2020`[4]+total, my_data$`2020`[4]+0)
my_data[5, '2020'] <- ifelse(date != my_data_date[13,6] & month2 == "May", my_data$`2020`[5]+total, my_data$`2020`[5]+0)
my_data[6, '2020'] <- ifelse(date != my_data_date[13,6] & month2 == "Jun", my_data$`2020`[6]+total, my_data$`2020`[6]+0)
my_data[7, '2020'] <- ifelse(date != my_data_date[13,6] & month2 == "Jul", my_data$`2020`[7]+total, my_data$`2020`[7]+0)
my_data[8, '2020'] <- ifelse(date != my_data_date[13,6] & month2 == "Aug", my_data$`2020`[8]+total, my_data$`2020`[8]+0)
my_data[9, '2020'] <- ifelse(date != my_data_date[13,6] & month2 == "Sep", my_data$`2020`[9]+total, my_data$`2020`[9]+0)
my_data[10, '2020'] <- ifelse(date != my_data_date[13,6] & month2 == "Oct", my_data$`2020`[10]+total, my_data$`2020`[10]+0)
my_data[11, '2020'] <- ifelse(date != my_data_date[13,6] & month2 == "Nov", my_data$`2020`[11]+total, my_data$`2020`[11]+0)
my_data[12, '2020'] <- ifelse(date != my_data_date[13,6] & month2 == "Dec", my_data$`2020`[12]+total, my_data$`2020`[12]+0)

#save data to used for ggplot later
save_data <- my_data
saveRDS(save_data, file =paste0(mypath,"Malaysia_dengue_ggplot.Rdata"))

my_data[13, '2020'] <- date
data_date_malaysia <- my_data
write.xlsx(data_date_malaysia,paste0(mypath,"Malaysia_Dengue.xlsx"), sheetName="Sheet1", 
           col.names=TRUE, row.names=FALSE, append=FALSE)

my_data <- my_data[-c(13), ]
my_data <- transform(my_data, `2020` = as.numeric(`2020`))
my_data <- my_data %>% dplyr::rename( "2016" =  "X2016", "2017" =  "X2017", "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")

#merge my_data and means dataset
my_data<- inner_join(my_data,means,by="Month")

my_data$sixten_mean <- ifelse(my_data$`2016` > my_data$Above, 2,
                              ifelse(my_data$`2016` < my_data$Below, 0,
                                     1))
my_data$seventen_mean <- ifelse(my_data$`2017` > my_data$Above, 2,
                                ifelse(my_data$`2017` < my_data$Below, 0,
                                       1))
my_data$eigthten_mean <- ifelse(my_data$`2018` > my_data$Above, 2,
                                ifelse(my_data$`2018` < my_data$Below, 0,
                                       1))
my_data$nineten_mean <- ifelse(my_data$`2019` > my_data$Above, 2,
                               ifelse(my_data$`2019` < my_data$Below, 0,
                                      1))
my_data$twente_mean <- ifelse(my_data$`2020` > my_data$Above, 2,
                              ifelse(my_data$`2020` < my_data$Below, 0,
                                     1))

#save the processed data
my_data<- dplyr::select(my_data,-c(Means,Above,Below))
#last update date and new cases number into the last two rows. This will be used in rmarkdown
my_data[13,1]<-save_lastupdate
my_data[14,1]<-save_new_case

saveRDS(my_data, file = paste0(mypath,"Malaysia_dengue.Rdata"))
#end of malaysia_dengue function
}

#calling malaysia_dengue function inside trycatchlog 

tryCatchLog(malaysia_dengue(),error = function(e) {})

#' ----------------------------------------------------------------------------------------------
#' scrapping Polio cases in Afghanistan
#' 
#' 
#' ----------------------------------------------------------------------------------------------
#' 
#' Creating a function to wrap Afghanistan Polio extaction
#'
#'#begining of afghanistan_polio function
afghanistan_polio <- function(){
  url <- 'http://polioeradication.org/polio-today/polio-now/this-week/'
  webpage <- read_html(url)
  title_html <- html_nodes(webpage,'h2')
  date <- html_text(title_html[1])
  save_lastupdate<-paste("Last update: ", date)
  month1 <- strsplit(date, " ")
  month2 <- month1[[1]][7]
  info_html <- html_nodes(webpage,'.panel-collapse')
  Afghanistan <-  html_text(info_html[1])
  Afghanistan <-toString(Afghanistan)
  afghanistan_cases <- gsub( " .*$", "", Afghanistan )
  
  #word_to_number function is defined in dependancies script
  total <- ifelse(afghanistan_cases == "No", 0, word_to_number(afghanistan_cases))
  
  my_data <- read_excel(paste0(mypath,"afghanistan_polio.xlsx"))
  
  my_data_date <- my_data
  new_cases <- ifelse(date != my_data_date[13,7], total, 0)
  save_new_case<-paste("New Cases: ", new_cases)
  my_data <- my_data[-c(13), ] 
  my_data <- transform(my_data, `2020` = as.numeric(`2020`))
  my_data <- my_data %>% dplyr::rename( "2015" =  "X2015", "2016" =  "X2016", "2017" =  "X2017", "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")
  
  means <- data.frame(Month=my_data[,1], Means=rowMeans(my_data[,-1], na.rm=TRUE))
  means <- means %>% mutate(Above = means$Means*1.25, Below = means$Means*0.75)
  my_data <- as.data.frame(my_data)
  means$Month <- as.character(means$Month)
  
  my_data[1, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "January", my_data$`2020`[1]+total, my_data$`2020`[1]+0)
  my_data[2, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "February", my_data$`2020`[2]+total, my_data$`2020`[2]+0)
  my_data[3, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "March", my_data$`2020`[3]+total, my_data$`2020`[3]+0)
  my_data[4, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "April", my_data$`2020`[4]+total, my_data$`2020`[4]+0)
  my_data[5, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "May", my_data$`2020`[5]+total, my_data$`2020`[5]+0)
  my_data[6, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "June", my_data$`2020`[6]+total, my_data$`2020`[6]+0)
  my_data[7, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "July", my_data$`2020`[7]+total, my_data$`2020`[7]+0)
  my_data[8, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "August", my_data$`2020`[8]+total, my_data$`2020`[8]+0)
  my_data[9, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "September", my_data$`2020`[9]+total, my_data$`2020`[9]+0)
  my_data[10, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "October", my_data$`2020`[10]+total, my_data$`2020`[10]+0)
  my_data[11, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "November", my_data$`2020`[11]+total, my_data$`2020`[11]+0)
  my_data[12, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "December", my_data$`2020`[12]+total, my_data$`2020`[12]+0)
  
  save_data <- my_data
  saveRDS(save_data, file =paste0(mypath,"afghanistan_polio_ggplot.Rdata"))
  
  my_data[13, '2020'] <- date
  data_date_afghanistan <- my_data
  
  write.xlsx(data_date_afghanistan,paste0(mypath,"afghanistan_polio.xlsx"), sheetName="Sheet1", 
             col.names=TRUE, row.names=FALSE, append=FALSE)
  
  my_data <- my_data[-c(13), ]
  my_data <- transform(my_data, `2020` = as.numeric(`2020`))
  my_data <- my_data %>% dplyr::rename( "2015" =  "X2015", "2016" =  "X2016", "2017" =  "X2017", "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")
  
  #Merge my_data with means dataset
  my_data <- inner_join(my_data,means, by="Month")
  
  my_data$fiveten_mean <- ifelse(my_data$`2015` > my_data$Above, 2,
                                 ifelse(my_data$`2015` < my_data$Below, 0,
                                        1))
  my_data$sixten_mean <- ifelse(my_data$`2016` > my_data$Above, 2,
                                ifelse(my_data$`2016` < my_data$Below, 0,
                                       1))
  my_data$seventen_mean <- ifelse(my_data$`2017` > my_data$Above, 2,
                                  ifelse(my_data$`2017` < my_data$Below, 0,
                                         1))
  my_data$eigthten_mean <- ifelse(my_data$`2018` > my_data$Above, 2,
                                  ifelse(my_data$`2018` < my_data$Below, 0,
                                         1))
  my_data$nineten_mean <- ifelse(my_data$`2019` > my_data$Above, 2,
                                 ifelse(my_data$`2019` < my_data$Below, 0,
                                        1))
  my_data$twente_mean <- ifelse(my_data$`2020` > my_data$Above, 2,
                                ifelse(my_data$`2020` < my_data$Below, 0,
                                       1))
  #save the processed data
  my_data<- dplyr::select(my_data,-c(Means,Above,Below))
  my_data[13,1]<-save_lastupdate
  my_data[14,1]<-save_new_case
  saveRDS(my_data, file =paste0(mypath,"afghanistan_polio.Rdata"))
  
  
#end of afghanistan_polio function
}

#calling afghanistan_polio function inside trycatchlog

tryCatchLog(afghanistan_polio(),error = function(e) {})

#' ----------------------------------------------------------------------------------------------
#' scrapping Polio cases in Pakistan
#' 
#' 
#' ----------------------------------------------------------------------------------------------
#' 
#' Creating a function to wrap Pakistan Polio extaction

#begin of pakistan_polio function
pakistan_polio <- function(){

  url <- 'http://polioeradication.org/polio-today/polio-now/this-week/'
  webpage <- read_html(url)
  title_html <- html_nodes(webpage,'h2')
  date <- html_text(title_html[1])
  save_lastupdate<-paste("Last update: ", date)
  month1 <- strsplit(date, " ")
  month2 <- month1[[1]][7]
  info_html <- html_nodes(webpage,'.panel-collapse')
  Pakistan <-  html_text(info_html[2])
  pakistan_cases <- gsub( " .*$", "", Pakistan )
  #word_to_number function is defined in dependancies script
  total <- ifelse(pakistan_cases == "No", 0, word_to_number(pakistan_cases))
  
  my_data <- read_excel(paste0(mypath,"pakistan_polio.xlsx"))
  
  my_data_date <- my_data
  new_cases <- ifelse(date != my_data_date[13,7], total, 0)
  save_new_case<-paste("New Cases: ", new_cases)
  my_data <- my_data[-c(13), ] 
  my_data <- transform(my_data, `2020` = as.numeric(`2020`))
  my_data <- my_data %>% dplyr::rename( "2015" =  "X2015", "2016" =  "X2016", "2017" =  "X2017", "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")
  
  means <- data.frame(Month=my_data[,1], Means=rowMeans(my_data[,-1], na.rm=TRUE))
  means <- means %>% mutate(Above = means$Means*1.25, Below = means$Means*0.75)
  my_data <- as.data.frame(my_data)
  means$Month <- as.character(means$Month)
  
  my_data[1, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "January", my_data$`2020`[1]+total, my_data$`2020`[1]+0)
  my_data[2, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "February", my_data$`2020`[2]+total, my_data$`2020`[2]+0)
  my_data[3, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "March", my_data$`2020`[3]+total, my_data$`2020`[3]+0)
  my_data[4, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "April", my_data$`2020`[4]+total, my_data$`2020`[4]+0)
  my_data[5, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "May", my_data$`2020`[5]+total, my_data$`2020`[5]+0)
  my_data[6, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "June", my_data$`2020`[6]+total, my_data$`2020`[6]+0)
  my_data[7, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "July", my_data$`2020`[7]+total, my_data$`2020`[7]+0)
  my_data[8, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "August", my_data$`2020`[8]+total, my_data$`2020`[8]+0)
  my_data[9, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "September", my_data$`2020`[9]+total, my_data$`2020`[9]+0)
  my_data[10, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "October", my_data$`2020`[10]+total, my_data$`2020`[10]+0)
  my_data[11, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "November", my_data$`2020`[11]+total, my_data$`2020`[11]+0)
  my_data[12, '2020'] <- ifelse(date != my_data_date[13,7] & month2 == "December", my_data$`2020`[12]+total, my_data$`2020`[12]+0)
  
  save_data <- my_data
  saveRDS(save_data, file = paste0(mypath,"pakistan_polio_ggplot.Rdata"))
  
  my_data[13, '2020'] <- date
  data_date_pakistan <- my_data
  
  write.xlsx(data_date_pakistan,paste0(mypath,"pakistan_polio.xlsx"), sheetName="Sheet1", 
             col.names=TRUE, row.names=FALSE, append=FALSE)
  
  my_data <- my_data[-c(13), ]
  my_data <- transform(my_data, `2020` = as.numeric(`2020`))
  my_data <- my_data %>% dplyr::rename( "2015" =  "X2015", "2016" =  "X2016", "2017" =  "X2017", "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")
  
  #merge my_data together with means dataset
  my_data<- inner_join(my_data,means, by="Month")
  my_data$fiveten_mean <- ifelse(my_data$`2015` > my_data$Above, 2,
                                 ifelse(my_data$`2015` < my_data$Below, 0,
                                        1))
  my_data$sixten_mean <- ifelse(my_data$`2016` > my_data$Above, 2,
                                ifelse(my_data$`2016` < my_data$Below, 0,
                                       1))
  my_data$seventen_mean <- ifelse(my_data$`2017` > my_data$Above, 2,
                                  ifelse(my_data$`2017` < my_data$Below, 0,
                                         1))
  my_data$eigthten_mean <- ifelse(my_data$`2018` > my_data$Above, 2,
                                  ifelse(my_data$`2018` < my_data$Below, 0,
                                         1))
  my_data$nineten_mean <- ifelse(my_data$`2019` > my_data$Above, 2,
                                 ifelse(my_data$`2019` < my_data$Below, 0,
                                        1))
  my_data$twente_mean <- ifelse(my_data$`2020` > my_data$Above, 2,
                                ifelse(my_data$`2020` < my_data$Below, 0,
                                       1))
  #save the processed data
  my_data<- dplyr::select(my_data,-c(Means,Above,Below))
  #last update date and new cases number into the last two rows. This will be used in rmarkdown
  my_data[13,1]<-save_lastupdate
  my_data[14,1]<-save_new_case
  saveRDS(my_data, file = paste0(mypath,"pakistan_polio.Rdata"))
  
#end of pakistan_polio function
}

# calling pakistan_polio function inside trycatchlog

tryCatchLog(pakistan_polio(),error = function(e) {})

#' ----------------------------------------------------------------------------------------------
#' scrapping Measles cases in Bangladesh
#' 
#' 
#' ----------------------------------------------------------------------------------------------
#' Creating a function to wrap Bangladesh Measles extaction
#'
#'begin of bangladesh_measles function 
bangladesh_measles <- function(){


site <- "https://www.who.int/bangladesh/emergencies/Rohingyacrisis/ewars"
webpage <- read_html(site) %>% html_nodes("div") %>% html_nodes("p") %>% html_nodes("a") %>% html_attr("href")
date1 <- pdf_text(webpage[1])[1]
date_slipt <- date1 %>% str_split("\r\n", simplify = TRUE) 
date <- trimws(date_slipt[2])
save_lastupdate<-paste("Last update: ", date)
month1 <- gsub('[[:digit:]]+', '', date)
month1.1 <- strsplit(month1, " ")
month2 <- month2 <- ifelse(length(month1.1[[1]]) >= 8, month1.1[[1]][7],
                           ifelse(length(month1.1[[1]]) == 5, month1.1[[1]][4],
                                  month1.1[[1]][6]))
data1 <- pdf_text(webpage[1]) %>% grep('Measles', ., value = T)
data2 <- str_extract(data1[1], "(?i)(?<=Total\\D)\\d+")
total <- data2 %>% as.numeric()

my_data <- read_excel(paste0(mypath,"Bangladesh_Measles.xlsx"))

my_data_date <- my_data
new_cases <- ifelse(date != my_data_date[13,4], total, 0)
save_new_case<-paste("New Cases: ", new_cases)
my_data <- my_data[-c(13), ] 
my_data <- transform(my_data, `2020` = as.numeric(`2020`))
my_data <- my_data %>% dplyr::rename( "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")

means <- data.frame(Month=my_data[,1], Means=rowMeans(my_data[,-1], na.rm=TRUE))
means$Month <- as.character(means$Month)
means <- means %>% mutate(Above = means$Means*1.25, Below = means$Means*0.75)

my_data[1, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "January", my_data$`2020`[1]+total, my_data$`2020`[1]+0)
my_data[2, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "February", my_data$`2020`[2]+total, my_data$`2020`[2]+0)
my_data[3, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "March", my_data$`2020`[3]+total, my_data$`2020`[3]+0)
my_data[4, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "April", my_data$`2020`[4]+total, my_data$`2020`[4]+0)
my_data[5, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "May", my_data$`2020`[5]+total, my_data$`2020`[5]+0)
my_data[6, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "June", my_data$`2020`[6]+total, my_data$`2020`[6]+0)
my_data[7, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "July", my_data$`2020`[7]+total, my_data$`2020`[7]+0)
my_data[8, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "August", my_data$`2020`[8]+total, my_data$`2020`[8]+0)
my_data[9, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "September", my_data$`2020`[9]+total, my_data$`2020`[9]+0)
my_data[10, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "October", my_data$`2020`[10]+total, my_data$`2020`[10]+0)
my_data[11, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "November", my_data$`2020`[11]+total, my_data$`2020`[11]+0)
my_data[12, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "December", my_data$`2020`[12]+total, my_data$`2020`[12]+0)


save_data <- my_data
saveRDS(save_data, file = paste0(mypath,"Bangladesh_Measles_ggplot.Rdata"))

my_data[13, '2020'] <- date
data_date_bangladesh_measles <- my_data

write.xlsx(data_date_bangladesh_measles,paste0(mypath,"Bangladesh_Measles.xlsx"), sheetName="Sheet1", 
           col.names=TRUE, row.names=FALSE, append=FALSE)

my_data <- my_data[-c(13), ]
my_data <- transform(my_data, `2020` = as.numeric(`2020`))
my_data <- my_data %>% dplyr::rename( "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")

#merge my_data with means dataset
my_data <- inner_join(my_data,means,by="Month")

my_data$eigthten_mean <- ifelse(my_data$`2018` > my_data$Above, 2,
                                ifelse(my_data$`2018` < my_data$Below, 0,
                                       1))
my_data$nineten_mean <- ifelse(my_data$`2019` > my_data$Above, 2,
                               ifelse(my_data$`2019` < my_data$Below, 0,
                                      1))
my_data$twente_mean <- ifelse(my_data$`2020` > my_data$Above, 2,
                              ifelse(my_data$`2020` < my_data$Below, 0,
                                     1))
#save the processed data
my_data<- dplyr::select(my_data,-c(Means,Above,Below))
#last update date and new cases number into the last two rows. This will be used in rmarkdown
my_data[13,1]<-save_lastupdate
my_data[14,1]<-save_new_case
saveRDS(my_data, file = paste0(mypath,"Bangladesh_Measles.Rdata"))

#end of bangladesh_measles function
}

#calling bangladesh_measles function inside trycatchlog
tryCatchLog(bangladesh_measles(),error = function(e) {})

#' ----------------------------------------------------------------------------------------------
#' scrapping Diarrhea cases in Bangladesh
#' 
#' 
#' ----------------------------------------------------------------------------------------------
#'Creating a function to wrap Bangladesh Diarrhea extaction
#'
#' bangladesh_diarrhea function begins here
bangladesh_diarrhea <- function(){

site <- "https://www.who.int/bangladesh/emergencies/Rohingyacrisis/ewars"
webpage <- read_html(site) %>% html_nodes("div") %>% html_nodes("p") %>% html_nodes("a") %>% html_attr("href")
date1 <- pdf_text(webpage[1])[1]
date_slipt <- date1 %>% str_split("\r\n", simplify = TRUE) 
date <- trimws(date_slipt[2])
save_lastupdate<-paste("Last update: ", date)
month1 <- gsub('[[:digit:]]+', '', date)
month1.1 <- strsplit(month1, " ")
month2 <- month2 <- ifelse(length(month1.1[[1]]) >= 8, month1.1[[1]][7],
                           ifelse(length(month1.1[[1]]) == 5, month1.1[[1]][4],
                                  month1.1[[1]][6]))
data1 <- pdf_text(webpage[1]) %>% grep('Diarrhoeal Disease', ., value = T)
data2 <- str_match(data1, "A total (.*?) cases")
total <- gsub(" ", "", data2[,2], fixed = TRUE) %>% as.numeric()


my_data <- read_excel(paste0(mypath,"Bangladesh_Diarrhea.xlsx"))

my_data_date <- my_data
new_cases <- ifelse(date != my_data_date[13,4], total, 0)
save_new_case<-paste("New Cases: ", new_cases)
my_data <- my_data[-c(13), ] 
my_data <- transform(my_data, `2020` = as.numeric(`2020`))
my_data <- my_data %>% dplyr::rename( "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")

means <- data.frame(Month=my_data[,1], Means=rowMeans(my_data[,-1], na.rm=TRUE))
means$Month <- as.character(means$Month)
means <- means %>% mutate(Above = means$Means*1.25, Below = means$Means*0.75)
my_data <- as.data.frame(my_data)


my_data[1, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "January", my_data$`2020`[1]+total, my_data$`2020`[1]+0)
my_data[2, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "February", my_data$`2020`[2]+total, my_data$`2020`[2]+0)
my_data[3, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "March", my_data$`2020`[3]+total, my_data$`2020`[3]+0)
my_data[4, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "April", my_data$`2020`[4]+total, my_data$`2020`[4]+0)
my_data[5, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "May", my_data$`2020`[5]+total, my_data$`2020`[5]+0)
my_data[6, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "June", my_data$`2020`[6]+total, my_data$`2020`[6]+0)
my_data[7, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "July", my_data$`2020`[7]+total, my_data$`2020`[7]+0)
my_data[8, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "August", my_data$`2020`[8]+total, my_data$`2020`[8]+0)
my_data[9, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "September", my_data$`2020`[9]+total, my_data$`2020`[9]+0)
my_data[10, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "October", my_data$`2020`[10]+total, my_data$`2020`[10]+0)
my_data[11, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "November", my_data$`2020`[11]+total, my_data$`2020`[11]+0)
my_data[12, '2020'] <- ifelse(date != my_data_date[13,4] & month2 == "December", my_data$`2020`[12]+total, my_data$`2020`[12]+0)


save_data <- my_data
saveRDS(save_data, file = paste0(mypath,"Bangladesh_Diarrhea_ggplot.Rdata"))

my_data[13, '2020'] <- date

data_date_bangladesh_diarrhea <- my_data

write.xlsx(data_date_bangladesh_diarrhea,paste0(mypath,"Bangladesh_Diarrhea.xlsx"), sheetName="Sheet1", 
           col.names=TRUE, row.names=FALSE, append=FALSE)

my_data <- my_data[-c(13), ]
my_data <- transform(my_data, `2020` = as.numeric(`2020`))
my_data <- my_data %>% dplyr::rename( "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")
#merge my_data together with means dataset
my_data <- inner_join(my_data,means, by="Month")

my_data$eigthten_mean <- ifelse(my_data$`2018` > my_data$Above, 2,
                                ifelse(my_data$`2018` < my_data$Below, 0,
                                       1))
my_data$nineten_mean <- ifelse(my_data$`2019` > my_data$Above, 2,
                               ifelse(my_data$`2019` < my_data$Below, 0,
                                      1))
my_data$twente_mean <- ifelse(my_data$`2020` > my_data$Above, 2,
                              ifelse(my_data$`2020` < my_data$Below, 0,
                                     1))
#save the processed data
my_data<- dplyr::select(my_data,-c(Means,Above,Below))
#last update date and new cases number into the last two rows. This will be used in rmarkdown
my_data[13,1]<-save_lastupdate
my_data[14,1]<-save_new_case
saveRDS(my_data, file = paste0(mypath,"Bangladesh_Diarrhea.Rdata"))

#end of bangladesh_diarrhea function
}

# Calling bangladesh_diarrhea function inside trycatchlog

tryCatchLog(bangladesh_diarrhea(),error = function(e) {})

#'----------------------------------------------------------------------------------------------
#'  scrapping Drought Situation Report- Sri Lanka
#'  
#'  
#'----------------------------------------------------------------------------------------------
#' 

#' Creating a function to wrap sirlanka drought  extaction
#' 
# beginning of sirlanka_drought function
sirlanka_drought <- function(){

site <- "http://www.dmc.gov.lk/index.php?option=com_dmcreports&view=reports&Itemid=273&report_type_id=1&lang=en"
date <- read_html(site) %>% html_nodes("td")
save_lastupdate<-paste("Last update: ", gsub("\\D+\\D+", "\\1",  date[10]))



my_data <- read_excel(paste0(mypath,"sri_lanka_drought.xlsx"))


my_data <- as.data.frame(my_data)


#my_data[1, '2020'] <- my_data$`2020`[1]+
#my_data[2, '2020'] <- my_data$`2020`[2]+
#my_data[3, '2020'] <- my_data$`2020`[3]+
#my_data[4, '2020'] <- my_data$`2020`[4]+
#my_data[5, '2020'] <- my_data$`2020`[5]+
#my_data[6, '2020'] <- my_data$`2020`[6]+
#my_data[7, '2020'] <- my_data$`2020`[7]+
#my_data[8, '2020'] <- 7707
#my_data[9, '2020'] <- my_data$`2020`[9]+
#my_data[10, '2020'] <- 82906
#my_data[11, '2020'] <- 103445
#my_data[12, '2020'] <- 124348
#my_data[13, '2020'] <- 178426
#my_data[14, '2020'] <- 262032
#my_data[15, '2020'] <- 248340
#my_data[16, '2020'] <- 297039
#my_data[17, '2020'] <- my_data$`2020`[17]+
#my_data[18, '2020'] <- my_data$`2020`[18]+
#my_data[19, '2020'] <- my_data$`2020`[19]+
#my_data[20, '2020'] <- my_data$`2020`[20]+
#my_data[21, '2020'] <- my_data$`2020`[21]+
#my_data[22, '2020'] <- my_data$`2020`[22]+
#my_data[23, '2020'] <- my_data$`2020`[23]+
#my_data[24, '2020'] <- my_data$`2020`[24]+
#my_data[25, '2020'] <- my_data$`2020`[25]+
#my_data[26, '2020'] <- my_data$`2020`[26]+
#my_data[27, '2020'] <- my_data$`2020`[27]+
#my_data[28, '2020'] <- my_data$`2020`[28]+
#my_data[29, '2020'] <- my_data$`2020`[29]+
#my_data[30, '2020'] <- my_data$`2020`[30]+
#my_data[31, '2020'] <- my_data$`2020`[31]+
#my_data[32, '2020'] <- my_data$`2020`[32]+
#my_data[33, '2020'] <- my_data$`2020`[33]+
#my_data[34, '2020'] <- my_data$`2020`[34]+
#my_data[35, '2020'] <- my_data$`2020`[35]+
#my_data[36, '2020'] <- my_data$`2020`[36]+
#my_data[37, '2020'] <- my_data$`2020`[37]+
#my_data[38, '2020'] <- my_data$`2020`[38]+
#my_data[39, '2020'] <- my_data$`2020`[39]+
#my_data[40, '2020'] <- my_data$`2020`[40]+
#my_data[41, '2020'] <- my_data$`2020`[41]+
#my_data[42, '2020'] <- my_data$`2020`[42]+
#my_data[43, '2020'] <- my_data$`2020`[43]+
#my_data[44, '2020'] <- my_data$`2020`[44]+
#my_data[45, '2020'] <- my_data$`2020`[45]+
#my_data[46, '2020'] <- my_data$`2020`[46]+
#my_data[47, '2020'] <- my_data$`2020`[47]+
#my_data[48, '2020'] <- my_data$`2020`[48]+
#my_data[49, '2020'] <- my_data$`2020`[49]+
#my_data[50, '2020'] <- my_data$`2020`[50]+
#my_data[51, '2020'] <- my_data$`2020`[51]+
#my_data[52, '2020'] <- my_data$`2020`[52]+

save_data_srilanka_drought <- my_data

my_data$twente_mean <- ifelse(my_data$`2020` > 50000, 2,
                              ifelse(my_data$`2020` < 30000, 0,
                                     1))

#'save the processed data
#last update date and new cases number into the last two rows. This will be used in rmarkdown
my_data$lastupdate[1]<-save_lastupdate


saveRDS(my_data, file = paste0(mypath,"save_data_srilanka_drought.Rdata"))
write.xlsx(save_data_srilanka_drought,paste0(mypath,"sri_lanka_drought.xlsx"), sheetName="Sheet1", 
           col.names=TRUE, row.names=FALSE, append=FALSE)

#end of sirlanka_drought function
}

# calling sirlanka_drought function inside trycatchlog

tryCatchLog(sirlanka_drought(),error = function(e) {})

 #' ----------------------------------------------------------------------------------------------
 #' scrapping Flood Situation Report- Indonesia
 #' 
 #' 
 #' ----------------------------------------------------------------------------------------------
 #'
#Beginning of indonesia_floods
#' Creating a function to wrap Indonesia floods extaction

indonesia_floods <- function(){

site <- "http://adinet.ahacentre.org/reports/"
my_data <- read_excel(paste0(mypath,"Indonesia_flooding.xlsx"))
#my_data[1] <- NULL
urls1 <- my_data %>% as.data.frame()

webpage <- read_html(site) %>% html_nodes("h3") %>% html_nodes("a") %>% grep('Indonesia', ., value = T) %>% grep('Floods|floods|Flood|Flashflood|Flooding|flood|flashflood|flooding', ., value = T)  
url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
urls <- str_extract(webpage, url_pattern)
urls <- urls[urls %in% urls1[,1] == FALSE]
urls<- (ifelse(is.empty(urls)==TRUE, "http://adinet.ahacentre.org/", urls))
scrape_test = function(link) {name <- link %>% read_html %>% html_nodes("tr")%>% html_text()
return(name)
}
d <- vector("list", length(urls))
for (i in seq_along(urls)) {
  if (!(urls[i] %in% names(d))) {
    cat(paste("Doing", urls[i], "..."))
    ok <- FALSE
    counter <- 0
    while (ok == FALSE & counter <= 5) {
      counter <- counter + 1
      out <- tryCatch({                  
        scrape_test(urls[i])
      },
      error = function(e) {
        Sys.sleep(2)
        e
      }
      )
      if ("error" %in% class(out)) {
        cat(".")
      } else {
        ok <- TRUE
        cat(" Done.")
      }
    }
    cat("\n")
    d[[i]] <- out
    names(d)[i] <- urls[i]
  }
} 
data1 <- d %>% map(. %>% grep('Affected Persons:', ., value = T))
data2 <- gsub(",","",data1)
data2 <- as.numeric(gsub("Affected Persons: ", "", data1))
data2[is.na(data2)] <- 0
data2 <- (ifelse(is.empty(data2)==TRUE, 0, data2))
scrape_test1 = function(link1) {name <- link1 %>% read_html %>% html_nodes('.r_date')%>% html_text()
return(name)
}
date <- vector("list", length(urls))
for (i in seq_along(urls)) {
  if (!(urls[i] %in% names(date))) {
    cat(paste("Doing", urls[i], "..."))
    ok <- FALSE
    counter <- 0
    while (ok == FALSE & counter <= 5) {
      counter <- counter + 1
      out <- tryCatch({                  
        scrape_test1(urls[i])
      },
      error = function(e) {
        Sys.sleep(2)
        e
      }
      )
      if ("error" %in% class(out)) {
        cat(".")
      } else {
        ok <- TRUE
        cat(" Done.")
      }
    }
    cat("\n")
    date[[i]] <- out
    names(date)[i] <- urls[i]
  }
} 
date1 <- (ifelse(urls=="http://adinet.ahacentre.org/", "", sapply(date, "[[", 1)))
month1 <- (ifelse(is.empty(date1)==TRUE, NA, strsplit(date1, " ")))
month2 <- (ifelse(is.na(month1)==TRUE, NA, sapply(month1, "[[", 2)))
data_name <- "Affected people"
month_name <- "Month"
df <- data.frame(data2,month2)
names(df) <- c(data_name,month_name)
total <- ddply(df, .(Month), summarise, 'New Cases' = sum(`Affected people`))
total

#If there are not cases what Date have no data then error

my_data <- read_excel(paste0(mypath,"Indonesia_floods.xlsx"))

#my_data[1] <- NULL
my_data <- transform(my_data, `2020` = as.numeric(`2020`))
my_data <- my_data %>% dplyr::rename( "2017" =  "X2017","2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")
means <- data.frame(ID=my_data[,1], Means=rowMeans(my_data[,-1], na.rm=TRUE))
means <- means %>% mutate(Above = means$Means*1.25, Below = means$Means*0.75)

my_data[1, '2020'] <- my_data$`2020`[1]+sum(ifelse(total$Month == "Jan", total$`New Cases`, 0))
my_data[2, '2020'] <- my_data$`2020`[2]+sum(ifelse(total$Month == "Feb", total$`New Cases`, 0))
my_data[3, '2020'] <- my_data$`2020`[3]+sum(ifelse(total$Month == "Mar", total$`New Cases`, 0))
my_data[4, '2020'] <- my_data$`2020`[4]+sum(ifelse(total$Month == "Apr", total$`New Cases`, 0))
my_data[5, '2020'] <- my_data$`2020`[5]+sum(ifelse(total$Month == "May", total$`New Cases`, 0))
my_data[6, '2020'] <- my_data$`2020`[6]+sum(ifelse(total$Month == "Jun", total$`New Cases`, 0))
my_data[7, '2020'] <- my_data$`2020`[7]+sum(ifelse(total$Month == "Jul", total$`New Cases`, 0))
my_data[8, '2020'] <- my_data$`2020`[8]+sum(ifelse(total$Month == "Aug", total$`New Cases`, 0))
my_data[9, '2020'] <- my_data$`2020`[9]+sum(ifelse(total$Month == "Sep", total$`New Cases`, 0))
my_data[10, '2020'] <- my_data$`2020`[10]+sum(ifelse(total$Month == "Oct", total$`New Cases`, 0))
my_data[11, '2020'] <- my_data$`2020`[11]+sum(ifelse(total$Month == "Nov", total$`New Cases`, 0))
my_data[12, '2020'] <- my_data$`2020`[12]+sum(ifelse(total$Month == "Dec", total$`New Cases`, 0))

save_my_data <- my_data

saveRDS(save_my_data, file = paste0(mypath,"Flood_Indonesia_ggplot.Rdata"))

write.xlsx(save_my_data,paste0(mypath,"Indonesia_floods.xlsx"), sheetName="Sheet1", 
           col.names=TRUE, row.names=FALSE, append=FALSE)

urls <- as.data.frame(urls)
colnames(urls) <- c("x")
urls <- merge(urls, urls1, by = "x", all = TRUE)
write.xlsx(urls, paste0(mypath,"Indonesia_flooding.xlsx"), sheetName="Sheet1", col.names=TRUE, row.names=FALSE, append=FALSE)

#convert to similar class before joining
means$ID<- as.character(means$ID)
my_data <- inner_join(save_my_data,means, by=c("Month"="ID"))


my_data$seventen_mean <- ifelse(my_data$`2017` > my_data$Above, 2,
                                ifelse(my_data$`2017` < my_data$Below, 0,
                                       1))
my_data$eigthten_mean <- ifelse(my_data$`2018` > my_data$Above, 2,
                                ifelse(my_data$`2018` < my_data$Below, 0,
                                       1))
my_data$nineten_mean <- ifelse(my_data$`2019` > my_data$Above, 2,
                               ifelse(my_data$`2019` < my_data$Below, 0,
                                      1))
my_data$twente_mean <- ifelse(my_data$`2020` > my_data$Above, 2,
                              ifelse(my_data$`2020` < my_data$Below, 0,
                                     1))


#save the processed data
my_data<- dplyr::select(my_data,-c(Means,Above,Below))
#last update date and new cases number into the last two rows. This will be used in rmarkdown
my_data[13,1]<-paste("Last update: ", as.character(total$Month)) 
my_data[14,1]<-paste("New Cases: ", total[2]) 
saveRDS(my_data, file = paste0(mypath,"Flood_Indonesia.Rdata"))

#end of indonesia_floods function
}

# Calling indonesia_floods function inside trycatchlog
tryCatchLog(indonesia_floods(),error = function(e) {})

#' ----------------------------------------------------------------------------------------------
#' scrapping Flood Situation Report- Myanmar
#' 
#' 
#' ----------------------------------------------------------------------------------------------

#' Creating a function to wrap Myanmar floods extaction
#begin of myanmar_floods
myanmar_floods <- function(){

site <- "http://adinet.ahacentre.org/reports/"
my_data <- read_excel(paste0(mypath,"Myanmar_flooding.xlsx"))
my_data[1] <- NULL
urls1 <- my_data %>% as.data.frame()
webpage <- read_html(site) %>% html_nodes("h3") %>% html_nodes("a") %>% grep('Myanmar', ., value = T) %>% grep('Floods|floods|Flood|Flashflood|Flooding|flood|flashflood|flooding', ., value = T) 
url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
urls <- str_extract(webpage, url_pattern)
urls <- urls[urls %in% urls1[,1] == FALSE]
urls<- (ifelse(is.empty(urls)==TRUE, "http://adinet.ahacentre.org/", urls))
scrape_test = function(link) {name <- link %>% read_html %>% html_nodes("tr")%>% html_text()
return(name)
}
d <- vector("list", length(urls))
for (i in seq_along(urls)) {
  if (!(urls[i] %in% names(d))) {
    cat(paste("Doing", urls[i], "..."))
    ok <- FALSE
    counter <- 0
    while (ok == FALSE & counter <= 5) {
      counter <- counter + 1
      out <- tryCatch({                  
        scrape_test(urls[i])
      },
      error = function(e) {
        Sys.sleep(2)
        e
      }
      )
      if ("error" %in% class(out)) {
        cat(".")
      } else {
        ok <- TRUE
        cat(" Done.")
      }
    }
    cat("\n")
    d[[i]] <- out
    names(d)[i] <- urls[i]
  }
} 
data1 <- d %>% map(. %>% grep('Affected Persons:', ., value = T))
data2 <- gsub(",","",data1)
data2 <- as.numeric(gsub("Affected Persons: ", "", data1))
data2[is.na(data2)] <- 0
data2 <- (ifelse(is.empty(data2)==TRUE, 0, data2))
scrape_test1 = function(link1) {name <- link1 %>% read_html %>% html_nodes('.r_date')%>% html_text()
return(name)
}
date <- vector("list", length(urls))
for (i in seq_along(urls)) {
  if (!(urls[i] %in% names(date))) {
    cat(paste("Doing", urls[i], "..."))
    ok <- FALSE
    counter <- 0
    while (ok == FALSE & counter <= 5) {
      counter <- counter + 1
      out <- tryCatch({                  
        scrape_test1(urls[i])
      },
      error = function(e) {
        Sys.sleep(2)
        e
      }
      )
      if ("error" %in% class(out)) {
        cat(".")
      } else {
        ok <- TRUE
        cat(" Done.")
      }
    }
    cat("\n")
    date[[i]] <- out
    names(date)[i] <- urls[i]
  }
} 
date1 <- (ifelse(urls=="http://adinet.ahacentre.org/", "", sapply(date, "[[", 1)))
month1 <- (ifelse(is.empty(date1)==TRUE, NA, strsplit(date1, " ")))
month2 <- (ifelse(is.na(month1)==TRUE, NA, sapply(month1, "[[", 2)))
data_name <- "Affected people"
month_name <- "Month"
df <- data.frame(data2,month2)
names(df) <- c(data_name,month_name)
total <- ddply(df, .(Month), summarise, 'New Cases' = sum(`Affected people`))
total

#If there are not cases what Date have no data then error


my_data <- read_excel(paste0(mypath,"Myanmar_floods.xlsx"))
my_data[1] <- NULL
my_data <- transform(my_data, `2020` = as.numeric(`2020`))
my_data <- my_data %>% dplyr::rename( "2017" =  "X2017","2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")
means <- data.frame(ID=my_data[,1], Means=rowMeans(my_data[,-1], na.rm=TRUE))
means <- means %>% mutate(Above = means$Means*1.25, Below = means$Means*0.75)

my_data[1, '2020'] <- my_data$`2020`[1]+sum(ifelse(total$Month == "Jan", total$`New Cases`, 0))
my_data[2, '2020'] <- my_data$`2020`[2]+sum(ifelse(total$Month == "Feb", total$`New Cases`, 0))
my_data[3, '2020'] <- my_data$`2020`[3]+sum(ifelse(total$Month == "Mar", total$`New Cases`, 0))
my_data[4, '2020'] <- my_data$`2020`[4]+sum(ifelse(total$Month == "Apr", total$`New Cases`, 0))
my_data[5, '2020'] <- my_data$`2020`[5]+sum(ifelse(total$Month == "May", total$`New Cases`, 0))
my_data[6, '2020'] <- my_data$`2020`[6]+sum(ifelse(total$Month == "Jun", total$`New Cases`, 0))
my_data[7, '2020'] <- my_data$`2020`[7]+sum(ifelse(total$Month == "Jul", total$`New Cases`, 0))
my_data[8, '2020'] <- my_data$`2020`[8]+sum(ifelse(total$Month == "Aug", total$`New Cases`, 0))
my_data[9, '2020'] <- my_data$`2020`[9]+sum(ifelse(total$Month == "Sep", total$`New Cases`, 0))
my_data[10, '2020'] <- my_data$`2020`[10]+sum(ifelse(total$Month == "Oct", total$`New Cases`, 0))
my_data[11, '2020'] <- my_data$`2020`[11]+sum(ifelse(total$Month == "Nov", total$`New Cases`, 0))
my_data[12, '2020'] <- my_data$`2020`[12]+sum(ifelse(total$Month == "Dec", total$`New Cases`, 0))

save_my_data <- my_data

saveRDS(save_my_data, file = paste0(mypath,"Myanmar_floods_ggplot.Rdata"))

#We rewrite the Excel file to include the new number of cases and the date
write.xlsx(save_my_data, paste0(mypath,"Myanmar_floods.xlsx"), sheetName="Sheet1", 
           col.names=TRUE, row.names=TRUE, append=FALSE)

urls <- as.data.frame(urls)
colnames(urls) <- c("x")
urls <- merge(urls, urls1, by = "x", all = TRUE)
write.xlsx(urls, paste0(mypath,"Myanmar_flooding.xlsx"), sheetName="Sheet1", col.names=TRUE, row.names=TRUE, append=FALSE)

#convert to similar class before joining
means$ID<- as.character(means$ID)
my_data <- inner_join(save_my_data,means, by=c("Month"="ID"))


my_data$seventen_mean <- ifelse(my_data$`2017` > my_data$Above, 2,
                                ifelse(my_data$`2017` < my_data$Below, 0,
                                       1))
my_data$eigthten_mean <- ifelse(my_data$`2018` > my_data$Above, 2,
                                ifelse(my_data$`2018` < my_data$Below, 0,
                                       1))
my_data$nineten_mean <- ifelse(my_data$`2019` > my_data$Above, 2,
                               ifelse(my_data$`2019` < my_data$Below, 0,
                                      1))
my_data$twente_mean <- ifelse(my_data$`2020` > my_data$Above, 2,
                              ifelse(my_data$`2020` < my_data$Below, 0,
                                     1))


#save the processed data
my_data<- dplyr::select(my_data,-c(Means,Above,Below))
#last update date and new cases number into the last two rows. This will be used in rmarkdown
my_data[13,1]<-paste("Last update: ", as.character(total$Month)) 
my_data[14,1]<-paste("New Cases: ", total[2]) 
saveRDS(my_data, file = paste0(mypath,"Myanmar_floods.Rdata"))

#end of myanmar_floods function
}

# Calling myanmar_floods function inside trycatchlog

tryCatchLog(myanmar_floods(),error = function(e) {})

#' ----------------------------------------------------------------------------------------------
#' scrapping Flood Situation Report- Philippines
#' 
#' 
#' ----------------------------------------------------------------------------------------------

#' Creating a function to wrap Philippines floods extaction
#beginning of philippines_floods function
philippines_floods <- function(){

site <- "http://adinet.ahacentre.org/reports/"
my_data <- read_excel(paste0(mypath,"Philippines_flooding.xlsx"))
my_data[1] <- NULL
urls1 <- my_data %>% as.data.frame()
webpage <- read_html(site) %>% html_nodes("h3") %>% html_nodes("a") %>% grep('Philippines', ., value = T) %>% grep('Floods|floods|Flood|Flashflood|Flooding|flood|flashflood|flooding', ., value = T) 
url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
urls <- str_extract(webpage, url_pattern)
urls <- urls[urls %in% urls1[,1] == FALSE]
urls<- (ifelse(is.empty(urls)==TRUE, "http://adinet.ahacentre.org/", urls))
scrape_test = function(link) {name <- link %>% read_html %>% html_nodes("tr")%>% html_text()
return(name)
}
d <- vector("list", length(urls))
for (i in seq_along(urls)) {
  if (!(urls[i] %in% names(d))) {
    cat(paste("Doing", urls[i], "..."))
    ok <- FALSE
    counter <- 0
    while (ok == FALSE & counter <= 5) {
      counter <- counter + 1
      out <- tryCatch({                  
        scrape_test(urls[i])
      },
      error = function(e) {
        Sys.sleep(2)
        e
      }
      )
      if ("error" %in% class(out)) {
        cat(".")
      } else {
        ok <- TRUE
        cat(" Done.")
      }
    }
    cat("\n")
    d[[i]] <- out
    names(d)[i] <- urls[i]
  }
} 
data1 <- d %>% map(. %>% grep('Affected Persons:', ., value = T))
data2 <- gsub(",","",data1)
data2 <- as.numeric(gsub("Affected Persons: ", "", data1))
data2[is.na(data2)] <- 0
data2 <- (ifelse(is.empty(data2)==TRUE, 0, data2))
scrape_test1 = function(link1) {name <- link1 %>% read_html %>% html_nodes('.r_date')%>% html_text()
return(name)
}
date <- vector("list", length(urls))
for (i in seq_along(urls)) {
  if (!(urls[i] %in% names(date))) {
    cat(paste("Doing", urls[i], "..."))
    ok <- FALSE
    counter <- 0
    while (ok == FALSE & counter <= 5) {
      counter <- counter + 1
      out <- tryCatch({                  
        scrape_test1(urls[i])
      },
      error = function(e) {
        Sys.sleep(2)
        e
      }
      )
      if ("error" %in% class(out)) {
        cat(".")
      } else {
        ok <- TRUE
        cat(" Done.")
      }
    }
    cat("\n")
    date[[i]] <- out
    names(date)[i] <- urls[i]
  }
} 
date1 <- (ifelse(urls=="http://adinet.ahacentre.org/", "", sapply(date, "[[", 1)))
month1 <- (ifelse(is.empty(date1)==TRUE, NA, strsplit(date1, " ")))
month2 <- (ifelse(is.na(month1)==TRUE, NA, sapply(month1, "[[", 2)))
data_name <- "Affected people"
month_name <- "Month"
df <- data.frame(data2,month2)
names(df) <- c(data_name,month_name)
total <- ddply(df, .(Month), summarise, 'New Cases' = sum(`Affected people`))
total

#If there are not cases what Date have no data then error


my_data <- read_excel(paste0(mypath,"Philippines_floods.xlsx"))
my_data[1] <- NULL
my_data <- transform(my_data, `2020` = as.numeric(`2020`))
my_data <- my_data %>% dplyr::rename( "2017" =  "X2017","2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")
means <- data.frame(ID=my_data[,1], Means=rowMeans(my_data[,-1], na.rm=TRUE))
means <- means %>% mutate(Above = means$Means*1.25, Below = means$Means*0.75)

my_data[1, '2020'] <- my_data$`2020`[1]+sum(ifelse(total$Month == "Jan", total$`New Cases`, 0))
my_data[2, '2020'] <- my_data$`2020`[2]+sum(ifelse(total$Month == "Feb", total$`New Cases`, 0))
my_data[3, '2020'] <- my_data$`2020`[3]+sum(ifelse(total$Month == "Mar", total$`New Cases`, 0))
my_data[4, '2020'] <- my_data$`2020`[4]+sum(ifelse(total$Month == "Apr", total$`New Cases`, 0))
my_data[5, '2020'] <- my_data$`2020`[5]+sum(ifelse(total$Month == "May", total$`New Cases`, 0))
my_data[6, '2020'] <- my_data$`2020`[6]+sum(ifelse(total$Month == "Jun", total$`New Cases`, 0))
my_data[7, '2020'] <- my_data$`2020`[7]+sum(ifelse(total$Month == "Jul", total$`New Cases`, 0))
my_data[8, '2020'] <- my_data$`2020`[8]+sum(ifelse(total$Month == "Aug", total$`New Cases`, 0))
my_data[9, '2020'] <- my_data$`2020`[9]+sum(ifelse(total$Month == "Sep", total$`New Cases`, 0))
my_data[10, '2020'] <- my_data$`2020`[10]+sum(ifelse(total$Month == "Oct", total$`New Cases`, 0))
my_data[11, '2020'] <- my_data$`2020`[11]+sum(ifelse(total$Month == "Nov", total$`New Cases`, 0))
my_data[12, '2020'] <- my_data$`2020`[12]+sum(ifelse(total$Month == "Dec", total$`New Cases`, 0))

save_my_data <- my_data
saveRDS(save_my_data, file = paste0(mypath,"Philippines_floods_ggplot.Rdata"))

#We rewrite the Excel file to include the new number of cases and the date
write.xlsx(save_my_data, paste0(mypath,"Philippines_floods.xlsx"), sheetName="Sheet1", 
           col.names=TRUE, row.names=TRUE, append=FALSE)

urls <- as.data.frame(urls)
colnames(urls) <- c("x")
urls <- merge(urls, urls1, by = "x", all = TRUE)
write.xlsx(urls, paste0(mypath,"Philippines_flooding.xlsx"), sheetName="Sheet1", col.names=TRUE, row.names=TRUE, append=FALSE)

#convert to similar class before joining
means$ID<- as.character(means$ID)
my_data <- inner_join(save_my_data,means, by=c("Month"="ID"))


my_data$seventen_mean <- ifelse(my_data$`2017` > my_data$Above, 2,
                                ifelse(my_data$`2017` < my_data$Below, 0,
                                       1))
my_data$eigthten_mean <- ifelse(my_data$`2018` > my_data$Above, 2,
                                ifelse(my_data$`2018` < my_data$Below, 0,
                                       1))
my_data$nineten_mean <- ifelse(my_data$`2019` > my_data$Above, 2,
                               ifelse(my_data$`2019` < my_data$Below, 0,
                                      1))
my_data$twente_mean <- ifelse(my_data$`2020` > my_data$Above, 2,
                              ifelse(my_data$`2020` < my_data$Below, 0,
                                     1))


#save the processed data
my_data<- dplyr::select(my_data,-c(Means,Above,Below))
#last update date and new cases number into the last two rows. This will be used in rmarkdown
my_data[13,1]<-paste("Last update: ", as.character(total$Month)) 
my_data[14,1]<-paste("New Cases: ", total[2]) 
saveRDS(my_data, file = paste0(mypath,"Philippines_floods.Rdata"))
#end of philippines_floods function
}
#Calling philippines_floods function inside trycatchlog

tryCatchLog(philippines_floods(),error = function(e) {})

#' ----------------------------------------------------------------------------------------------
#' scrapping Flood Situation Report- Thailand
#' 
#' 
#' ----------------------------------------------------------------------------------------------

#' Creating a function to wrap Thailand floods extaction
#Begining of thailand_floods function
thailand_floods<- function(){

site <- "http://adinet.ahacentre.org/reports/"
my_data <- read_excel(paste0(mypath,"Thailand_flooding.xlsx"))
my_data[1] <- NULL
urls1 <- my_data %>% as.data.frame()
webpage <- read_html(site) %>% html_nodes("h3") %>% html_nodes("a") %>% grep('Thailand', ., value = T) %>% grep('Floods|floods|Flood|Flashflood|Flooding|flood|flashflood|flooding', ., value = T) 
url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
urls <- str_extract(webpage, url_pattern)
urls <- urls[urls %in% urls1[,1] == FALSE]
urls<- (ifelse(is.empty(urls)==TRUE, "http://adinet.ahacentre.org/", urls))
scrape_test = function(link) {name <- link %>% read_html %>% html_nodes("tr")%>% html_text()
return(name)
}
d <- vector("list", length(urls))
for (i in seq_along(urls)) {
  if (!(urls[i] %in% names(d))) {
    cat(paste("Doing", urls[i], "..."))
    ok <- FALSE
    counter <- 0
    while (ok == FALSE & counter <= 5) {
      counter <- counter + 1
      out <- tryCatch({                  
        scrape_test(urls[i])
      },
      error = function(e) {
        Sys.sleep(2)
        e
      }
      )
      if ("error" %in% class(out)) {
        cat(".")
      } else {
        ok <- TRUE
        cat(" Done.")
      }
    }
    cat("\n")
    d[[i]] <- out
    names(d)[i] <- urls[i]
  }
} 
data1 <- d %>% map(. %>% grep('Affected Persons:', ., value = T))
data2 <- gsub(",","",data1)
data2 <- as.numeric(gsub("Affected Persons: ", "", data1))
data2[is.na(data2)] <- 0
data2 <- (ifelse(is.empty(data2)==TRUE, 0, data2))
scrape_test1 = function(link1) {name <- link1 %>% read_html %>% html_nodes('.r_date')%>% html_text()
return(name)
}
date <- vector("list", length(urls))
for (i in seq_along(urls)) {
  if (!(urls[i] %in% names(date))) {
    cat(paste("Doing", urls[i], "..."))
    ok <- FALSE
    counter <- 0
    while (ok == FALSE & counter <= 5) {
      counter <- counter + 1
      out <- tryCatch({                  
        scrape_test1(urls[i])
      },
      error = function(e) {
        Sys.sleep(2)
        e
      }
      )
      if ("error" %in% class(out)) {
        cat(".")
      } else {
        ok <- TRUE
        cat(" Done.")
      }
    }
    cat("\n")
    date[[i]] <- out
    names(date)[i] <- urls[i]
  }
} 
date1 <- (ifelse(urls=="http://adinet.ahacentre.org/", "", sapply(date, "[[", 1)))
month1 <- (ifelse(is.empty(date1)==TRUE, NA, strsplit(date1, " ")))
month2 <- (ifelse(is.na(month1)==TRUE, NA, sapply(month1, "[[", 2)))
data_name <- "Affected people"
month_name <- "Month"
df <- data.frame(data2,month2)
names(df) <- c(data_name,month_name)
total <- ddply(df, .(Month), summarise, 'New Cases' = sum(`Affected people`))
total

#If there are not cases what Date have no data then error


my_data <- read_excel(paste0(mypath,"Thailand_floods.xlsx"))
my_data[1] <- NULL
my_data <- transform(my_data, `2020` = as.numeric(`2020`))
my_data <- my_data %>% dplyr::rename( "2017" =  "X2017","2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")
means <- data.frame(ID=my_data[,1], Means=rowMeans(my_data[,-1], na.rm=TRUE))
means <- means %>% mutate(Above = means$Means*1.25, Below = means$Means*0.75)

my_data[1, '2020'] <- my_data$`2020`[1]+sum(ifelse(total$Month == "Jan", total$`New Cases`, 0))
my_data[2, '2020'] <- my_data$`2020`[2]+sum(ifelse(total$Month == "Feb", total$`New Cases`, 0))
my_data[3, '2020'] <- my_data$`2020`[3]+sum(ifelse(total$Month == "Mar", total$`New Cases`, 0))
my_data[4, '2020'] <- my_data$`2020`[4]+sum(ifelse(total$Month == "Apr", total$`New Cases`, 0))
my_data[5, '2020'] <- my_data$`2020`[5]+sum(ifelse(total$Month == "May", total$`New Cases`, 0))
my_data[6, '2020'] <- my_data$`2020`[6]+sum(ifelse(total$Month == "Jun", total$`New Cases`, 0))
my_data[7, '2020'] <- my_data$`2020`[7]+sum(ifelse(total$Month == "Jul", total$`New Cases`, 0))
my_data[8, '2020'] <- my_data$`2020`[8]+sum(ifelse(total$Month == "Aug", total$`New Cases`, 0))
my_data[9, '2020'] <- my_data$`2020`[9]+sum(ifelse(total$Month == "Sep", total$`New Cases`, 0))
my_data[10, '2020'] <- my_data$`2020`[10]+sum(ifelse(total$Month == "Oct", total$`New Cases`, 0))
my_data[11, '2020'] <- my_data$`2020`[11]+sum(ifelse(total$Month == "Nov", total$`New Cases`, 0))
my_data[12, '2020'] <- my_data$`2020`[12]+sum(ifelse(total$Month == "Dec", total$`New Cases`, 0))

save_my_data <- my_data

saveRDS(save_my_data, file = paste0(mypath,"Thailand_floods_ggplot.Rdata"))

#We rewrite the Excel file to include the new number of cases and the date
write.xlsx(save_my_data, paste0(mypath,"Thailand_floods.xlsx"), sheetName="Sheet1", 
           col.names=TRUE, row.names=TRUE, append=FALSE)

urls <- as.data.frame(urls)
colnames(urls) <- c("x")
urls <- merge(urls, urls1, by = "x", all = TRUE)
write.xlsx(urls, paste0(mypath,"Thailand_flooding.xlsx"), sheetName="Sheet1", col.names=TRUE, row.names=TRUE, append=FALSE)

#convert to similar class before joining
means$ID<- as.character(means$ID)
my_data <- inner_join(save_my_data,means, by=c("Month"="ID"))


my_data$seventen_mean <- ifelse(my_data$`2017` > my_data$Above, 2,
                                ifelse(my_data$`2017` < my_data$Below, 0,
                                       1))
my_data$eigthten_mean <- ifelse(my_data$`2018` > my_data$Above, 2,
                                ifelse(my_data$`2018` < my_data$Below, 0,
                                       1))
my_data$nineten_mean <- ifelse(my_data$`2019` > my_data$Above, 2,
                               ifelse(my_data$`2019` < my_data$Below, 0,
                                      1))
my_data$twente_mean <- ifelse(my_data$`2020` > my_data$Above, 2,
                              ifelse(my_data$`2020` < my_data$Below, 0,
                                     1))


#save the processed data
my_data<- dplyr::select(my_data,-c(Means,Above,Below))
#last update date and new cases number into the last two rows. This will be used in rmarkdown
my_data[13,1]<-paste("Last update: ", as.character(total$Month)) 
my_data[14,1]<-paste("New Cases: ", total[2]) 
saveRDS(my_data, file = paste0(mypath,"Thailand_floods.Rdata"))

#end of thailand_floods function
}

#Calling thailand_floods function inside trycatchlog
tryCatchLog(thailand_floods(),error = function(e) {})

#' ----------------------------------------------------------------------------------------------
#' scrapping Flood Situation Report- Afghanistan
#' 
#' 
#' ----------------------------------------------------------------------------------------------

#' Creating a function to wrap Afghanistan Flood extaction
#'Beginning of afghanistan_floods function
afghanistan_floods <- function(){

site <- "https://afghanistan.iom.int/hap-reports"
webpage <- read_html(site) %>% html_nodes(xpath = '//*[@id="block-views-1f641f11609969c129467338bd047b2e"]') %>% html_nodes("a") %>% html_attr("href")
web <- webpage[1]
web <- "https://afghanistan.iom.int/sites/default/files/Reports/iom-hap-biweekly_report_03_jun_to_16_jun_2020.pdf"
date1 <- pdf_text(web)[1] %>% str_split("\r\n", simplify = TRUE)
date2 <- match(date1  %>% grep('Period:', ., value = T), date1)
#Trim the space first before splitting the string
#date <- date1[,date2+1] %>% trimws() %>% str_split("              ", simplify = TRUE) 
date <- date1[,date2+1] %>% str_split("\\s{2,}", simplify = TRUE) %>% last() %>% trimws()
#grab the last column of the matrix
#date<-date[,ncol(date)]

month1 <- date %>% str_split(" ", simplify = TRUE)
month2 <- month1[2]

lastupdate<-paste("Last update: ", date)

data1 <- pdf_text(web)
data2 <- match(data1 %>% grep('Natural Disaster Initial Incident Information Summary', ., value = T), data1)
data3 <- data1[data2] %>% str_split("\r\n", simplify = TRUE) %>% grep('Flood ', ., value = T)
data4 <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", data3, perl=TRUE)
data5 <- data4 %>% map(. %>% str_match("Flood.* (.*?)")) 
data6 <- data5 %>% map(. %>% str_split(" ", simplify = TRUE))
data7 <- sapply(data6, "[[", 5)
data8 <- gsub(",","",data7)
total <- as.numeric(data8) %>% sum

my_data <- read_excel(paste0(mypath,"Afghanistan_floods.xlsx"))
my_data[1] <- NULL
my_data_date <- my_data
new_cases <- ifelse(date != my_data_date[13,10], total, 0)
savenewcases<-paste("New Cases: ", new_cases)
my_data <- my_data[-c(13), ] 
my_data <- transform(my_data, `2020` = as.numeric(`2020`))
my_data <- my_data %>% dplyr::rename( "2012" =  "X2012", "2013" =  "X2013", "2014" =  "X2014", "2015" =  "X2015", "2016" =  "X2016", "2017" =  "X2017", "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")

means <- data.frame(ID=my_data[,1], Means=rowMeans(my_data[,-1], na.rm=TRUE))
means <- means %>% mutate(Above = means$Means*1.25, Below = means$Means*0.75)
my_data <- as.data.frame(my_data)

my_data[1, '2020'] <- ifelse(date != my_data_date[13,10] & month2 == "January", my_data$`2020`[1]+total, my_data$`2020`[1]+0)
my_data[2, '2020'] <- ifelse(date != my_data_date[13,10] & month2 == "February", my_data$`2020`[2]+total, my_data$`2020`[2]+0)
my_data[3, '2020'] <- ifelse(date != my_data_date[13,10] & month2 == "March", my_data$`2020`[3]+total, my_data$`2020`[3]+0)
my_data[4, '2020'] <- ifelse(date != my_data_date[13,10] & month2 == "April", my_data$`2020`[4]+total, my_data$`2020`[4]+0)
my_data[5, '2020'] <- ifelse(date != my_data_date[13,10] & month2 == "May", my_data$`2020`[5]+total, my_data$`2020`[5]+0)
my_data[6, '2020'] <- ifelse(date != my_data_date[13,10] & month2 == "Jun", my_data$`2020`[6]+total, my_data$`2020`[6]+0)
my_data[7, '2020'] <- ifelse(date != my_data_date[13,10] & month2 == "July", my_data$`2020`[7]+total, my_data$`2020`[7]+0)
my_data[8, '2020'] <- ifelse(date != my_data_date[13,10] & month2 == "August", my_data$`2020`[8]+total, my_data$`2020`[8]+0)
my_data[9, '2020'] <- ifelse(date != my_data_date[13,10] & month2 == "September", my_data$`2020`[9]+total, my_data$`2020`[9]+0)
my_data[10, '2020'] <- ifelse(date != my_data_date[13,10] & month2 == "October", my_data$`2020`[10]+total, my_data$`2020`[10]+0)
my_data[11, '2020'] <- ifelse(date != my_data_date[13,10] & month2 == "November", my_data$`2020`[11]+total, my_data$`2020`[11]+0)
my_data[12, '2020'] <- ifelse(date != my_data_date[13,10] & month2 == "December", my_data$`2020`[12]+total, my_data$`2020`[12]+0)

save_data <- my_data
saveRDS(save_data, file = paste0(mypath,"Afghanistan_floods_ggplot.Rdata"))
my_data[13, '2020'] <- date
data_date_afghanistan_floods <- my_data

write.xlsx(data_date_afghanistan_floods, paste0(mypath,"Afghanistan_floods.xlsx"), sheetName="Sheet1", 
           col.names=TRUE, row.names=TRUE, append=FALSE)

my_data <- my_data[-c(13), ]

my_data <- transform(my_data, `2020` = as.numeric(`2020`))
my_data <- my_data %>% dplyr::rename( "2012" =  "X2012", "2013" =  "X2013", "2014" =  "X2014", "2015" =  "X2015", "2016" =  "X2016", "2017" =  "X2017", "2018" =  "X2018", "2019" =  "X2019", "2020" =  "X2020")

means$ID<- as.character(means$ID)
my_data <- inner_join(my_data,means, by=c("Month"="ID"))


my_data$twelve_mean <- ifelse(my_data$`2012` > my_data$Above, 2,
                              ifelse(my_data$`2012` < my_data$Below, 0,
                                     1))
my_data$thirten_mean <- ifelse(my_data$`2013` > my_data$Above, 2,
                               ifelse(my_data$`2013` < my_data$Below, 0,
                                      1))
my_data$fourten_mean <- ifelse(my_data$`2014` > my_data$Above, 2,
                               ifelse(my_data$`2014` < my_data$Below, 0,
                                      1))
my_data$fiveten_mean <- ifelse(my_data$`2015` > my_data$Above, 2,
                               ifelse(my_data$`2015` < my_data$Below, 0,
                                      1))
my_data$sixten_mean <- ifelse(my_data$`2016` > my_data$Above, 2,
                              ifelse(my_data$`2016` < my_data$Below, 0,
                                     1))
my_data$seventen_mean <- ifelse(my_data$`2017` > my_data$Above, 2,
                                ifelse(my_data$`2017` < my_data$Below, 0,
                                       1))
my_data$eigthten_mean <- ifelse(my_data$`2018` > my_data$Above, 2,
                                ifelse(my_data$`2018` < my_data$Below, 0,
                                       1))
my_data$nineten_mean <- ifelse(my_data$`2019` > my_data$Above, 2,
                               ifelse(my_data$`2019` < my_data$Below, 0,
                                      1))
my_data$twente_mean <- ifelse(my_data$`2020` > my_data$Above, 2,
                              ifelse(my_data$`2020` < my_data$Below, 0,
                                     1))

#save the processed data
my_data<- dplyr::select(my_data,-c(Means,Above,Below))
#last update date and new cases number into the last two rows. This will be used in rmarkdown
my_data[13,1]<-lastupdate
my_data[14,1]<-savenewcases 
saveRDS(my_data, file = paste0(mypath,"afghanistan_floods.Rdata"))
#End of afghanistan_floods function
}

#Calling afghanistan_floods
tryCatchLog(afghanistan_floods(),error = function(e) {})

#' ----------------------------------------------------------------------------------------------
#' The section below is meant for processing the all the datasets produced into a format 
#' required by shiny application
#' 
#' ----------------------------------------------------------------------------------------------

#' The function below is for transforming data for a particular site into long format after keeping only the necessary 
#' variables.
#' 
#' The function takes the following arguments
#' 1. dt1 = this is the r data object to be transformed
#' 2. from1,to1 = the begining and end of variables to be pulled to long format
#' 3. cntry1 = the country variable
#' 4  disease1 = the disease or disaster name

##create a dummy dataset to convert months from characters to digits

#Wrapping the processing inside a function

#Beginning of processing_data function
processing_data <- function(){


digit_month <- tribble(
  ~Month,~digits,
  
  "Jan",'01',
  "Feb",'02',
  "Mar",'03',
  "Apr",'04',
  "May",'05',
  "Jun",'06',
  "Jul",'07',
  "Aug",'08',
  "Sep",'09',
  "Oct",'10',
  "Nov",'11',
  "Dec",'12'
  
)


appdata<- function(dt1,from1,to1,cntry1,disease1){
save_data <- readRDS(file =paste0(mypath,dt1) )[1:12,] %>% dplyr::select(starts_with("Month"),starts_with("20")) %>%
            dplyr::inner_join(digit_month,by="Month") %>%
            tidyr::gather(key="Year",value="Value",from1:to1) %>% 
            mutate(Date=paste("01",digits,Year,sep = "/"),Country=cntry1,Disease=disease1)

}

#nepal dengue
nepal_dengue<- appdata(dt="save_data_nepal_dengue.Rdata",from ='2017',to= '2020',cntry ="Nepal",disease = "Dengue"  )
#sirlanka dengue
sirlanka_dengue<- appdata(dt="sri_lanka_dengue.Rdata",from ='2010',to= '2020',cntry ="Sri Lanka",disease = "Dengue"  )
#fiji dengue
fiji_dengue<- appdata(dt="save_data_fiji_dengue.Rdata",from ='2018',to= '2020',cntry ="Fiji",disease = "Dengue"  )
#french polynesia dengue
french_polynesia_dengue<- appdata(dt="french_polynesia_dengue.Rdata",from ='2018',to= '2020',cntry ="French Polynesia",disease = "Dengue")
#marshall dengue
marshall_dengue<- appdata(dt="marshall_dengue.Rdata",from ='2018',to= '2020',cntry ="Marshall",disease = "Dengue")
#Malaysia dengue
malaysia_dengue<- appdata(dt="Malaysia_dengue.Rdata",from ='2016',to= '2020',cntry ="Malaysia",disease = "Dengue")
#Afghanistan polio
afghanistan_polio<- appdata(dt="afghanistan_polio.Rdata",from ='2015',to= '2020',cntry ="Afghanistan",disease = "Polio")
#pakistan polio
pakistan_polio<- appdata(dt="pakistan_polio.Rdata",from ='2015',to= '2020',cntry ="Pakistan",disease = "Polio")
#Bangladesh Measles
Bangladesh_Measles<- appdata(dt="Bangladesh_Measles.Rdata",from ='2018',to= '2020',cntry ="Bangladesh",disease = "Measles")
#Bangladesh Diarrhea
Bangladesh_Diarrhea<- appdata(dt="Bangladesh_Diarrhea.Rdata",from ='2018',to= '2020',cntry ="Bangladesh",disease = "Diarrhea")

#append the above datasets together
disease_dataset<- dplyr::bind_rows(nepal_dengue,sirlanka_dengue,fiji_dengue,french_polynesia_dengue,
                                   marshall_dengue,malaysia_dengue,afghanistan_polio,pakistan_polio,
                                   Bangladesh_Measles,Bangladesh_Diarrhea )

#convert date to date class
#disease_dataset$Date<- lubridate::dmy(disease_dataset$Date)

#drop unnecessary variables
disease_dataset<- dplyr::select(disease_dataset,-c(Month,Year)) %>% dplyr::rename(Event=Disease) %>% dplyr::mutate(EventType="Disease") 

#write to google drive
##---------------------------------------------------------------------------------------------------------------------------------
## Disaster section
##----------------------------------------------------------------------------------------------------------------------------------
#Sirlanka drought
#read the data for srilanka drought
srilanka_drought<-readRDS(file =paste0(mypath,"save_data_srilanka_drought.Rdata") ) %>% dplyr::mutate(rownum=row_number()) %>%
                  dplyr::select(-c(Month))

## Fill in the NA within months
srilanka_drought<- dplyr::mutate(srilanka_drought,Month= case_when(rownum < 6 ~ "Jan",
                                                  rownum >= 6 & rownum < 10 ~ "Feb",
                                                  rownum >= 10 & rownum < 14 ~ "Mar",
                                                  rownum >= 14 & rownum < 18 ~ "Apr",
                                                  rownum >= 18 & rownum < 23 ~ "May",
                                                  rownum >= 23 & rownum < 27 ~ "Jun",
                                                  rownum >= 27 & rownum < 32 ~ "Jul",
                                                  rownum >= 32 & rownum < 36 ~ "Aug",
                                                  rownum >= 36 & rownum < 40 ~ "Sep",
                                                  rownum >= 40 & rownum < 45 ~ "Oct",
                                                  rownum >= 45 & rownum < 49 ~ "Nov",
                                                  rownum >= 49 & rownum <= 52 ~ "Dec",
                                                  ))


#Collapse sum on months then transform to long format
srilanka_drought<- dplyr::group_by(srilanka_drought,Month) %>% dplyr::summarise(`2020`=sum(`2020`,na.rm=T))%>%
                    dplyr::inner_join(digit_month,by="Month") %>%
                    tidyr::gather(key="Year",value="Value",`2020`) %>%
                    mutate(Date=paste("01",digits,Year,sep = "/"),Country="Sri Lanka",Disease="Drought")
                  


#Indonesia floods
Indonesia_floods<- appdata(dt="Flood_Indonesia.Rdata",from ='2017',to= '2020',cntry ="Indonesia",disease = "Floods")

#Myanmar floods
Myanmar_floods<- appdata(dt="Myanmar_floods.Rdata",from ='2017',to= '2020',cntry ="Myanmar",disease = "Floods")

#Philippines floods
Philippines_floods<- appdata(dt="Philippines_floods_ggplot.Rdata",from ='2017',to= '2020',cntry ="Philippines",disease = "Floods")

#Thailand floods
Thailand_floods<- appdata(dt="Thailand_floods_ggplot.Rdata",from ='2017',to= '2020',cntry ="Thailand",disease = "Floods")

#Afghanistan floods
Afghanistan_floods<- appdata(dt="Afghanistan_floods_ggplot.Rdata",from ='2012',to= '2020',cntry ="Afghanistan",disease = "Floods")

##combine the distater data
disaster_data<- bind_rows(srilanka_drought,Indonesia_floods,Myanmar_floods,Philippines_floods,Thailand_floods,Afghanistan_floods) %>% 
  dplyr::rename(Event=Disease) %>% dplyr::mutate(EventType="Disaster")

##drop variables not required
disaster_data<- dplyr::select(disaster_data,-c(Month,Year)) 

#convert date to date class
#disaster_data$Date<- lubridate::dmy(disaster_data$Date)
##combine disease and disaster into one dataset

disease_n_disaster <- bind_rows(disease_dataset,disaster_data) %>% dplyr::select(Country,EventType,Event,Date	,Value)

#replace na with zero
disease_n_disaster$Value= ifelse(is.na(disease_n_disaster$Value),0,disease_n_disaster$Value)



write.csv(as.data.frame(disease_n_disaster), paste0(mypath,"disease_n_disaster_final.csv"))

#End of processing_data function
}

#Calling processing_data function inside trycatchlog

tryCatchLog(processing_data(),error = function(e) {})


