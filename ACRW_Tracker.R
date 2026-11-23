## Acute Chronic Workload Ratio Activity Tracker
## Program name: ACWR_Tracker.R
## Author: Erika Mead
## Date: 2026
## Purpose: allow user to input daily activities and intensity and duration
##          calculate acute chronic workload ratio
##          create graphs to visualize
##          allow user to plan next activity duration and intensity to stay within
##          optimal ratio
##########################################################################################

install.packages("shiny")
install.packages("ggplot2")
install.packages("shinyBS")
install.packages("colourpicker")
install.packages("dplyr")
install.packages("DT")
install.packages("googlesheets4")
install.packages("googledrive")
install.packages("lubridate")

library(shiny)
library(ggplot2)
library(shinyBS)
library(colourpicker)
library(dplyr)
library(DT)
library(googlesheets4)
library(googledrive)
library(lubridate)

#-------------------------------------------------------------------------------------
# Google Service Account Authentication
#-------------------------------------------------------------------------------------
gs4_auth(path="science-fair-25-26-480503-0d897a28ca09.json")
drive_auth(path="science-fair-25-26-480503-0d897a28ca09.json")

gs4_auth(path = ".secrets/service-account.json")
drive_auth(path = ".secrets/service-account.json")

#-------------------------------------------------------------------------------------
# Google Sheet Information
#-------------------------------------------------------------------------------------
sheet_id <-"https://docs.google/com/spreadsheets/d/1iiHv9shnGi-0TNIEIZx43yIRRBwuEihofoQ4t-bYDH0/edit"
data_tab<-"data"
activities_tab<-"activities"
colors_tab <- "colors"



