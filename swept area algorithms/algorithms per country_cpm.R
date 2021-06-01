##############################################
#Procedures to calculate DATRAS data products
#
#############################################
#Authors: Cecilia Kvaavik and Colin Millar
#May 2021


library(icesDatras)
library(dplyr)
library(dbplyr)
library(tidyr)
library(tibble)


# Select survey, year and quarter
years <- 2004:2020
survey <- "NS-IBTS"
quarters <- 1:4

#Download HH exchange data
HH <- getDATRAS(record = "HH", survey, years, quarters)

#Make -9/0 into NA
HH[HH == 0] <- NA
HH[HH == -9] <- NA

# Make a call_distance column that include calculations for NA values
data_all <- HH %>%
  filter(HaulVal == "V") %>%
  transform(call_distance = ifelse(!is.na(Distance), Distance, HaulDur/60*1852*GroundSpeed)) #%>%
  #tibble()


###########################
#   FILTER BY COUNTRY     #
###########################

source("swept area algorithms/utilities.R")

data_by <- by(data_all, data_all$Country, calculate_doorspread)

data_combined <-
  do.call(
    rbind,
    data_by
  )

data_combined
#write.csv(data_combined,"C:/Users/ceciliak/Documents/ceciliak/swept area algorithms/NSIBTS04_20.csv") 
