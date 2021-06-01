##############################################
#Procedures to calculate DATRAS data products
#
#############################################
#Authors: Cecilia Kvaavik and Vaishav Soni 
#May 2021


library(icesDatras)
library(dplyr)
library(dbplyr)
library(tidyr)
#library(tibble)


#########
# NIGFS #  
#########
# Select survey, year and quarter
years <- 2005:2020
survey <- "NIGFS"
quarters <- 1:4

#Download HH exchange data
HH <- getDATRAS(record = "HH", survey, years, quarters)

#Make -9/0 into NA
HH[HH == 0] <- NA
HH[HH == -9] <- NA

#Make a call_distance column that include calculations for NA values
dataNI <- HH
dataNI <- dataNI %>% filter(HaulVal == "V")

dataNI <- dataNI %>% mutate(call_distance = ifelse(!is.na(Distance), 
                       Distance,
                       1.852 * 360 * 60 / 2 * pi*(acos(cos(radians(ShootLat)) * cos(radians(HaulLat)) * cos(radians(HaulLong) - radians(ShootLong)) + sin(radians(ShootLat)) * sin(radians(HaulLat))))


#sum(is.na(dataNI$Distance))#1384
#sum(is.na(dataNI$WingSpread))#1384
#sum(is.na(dataNI$DoorSpread))#4

#calculating missing DoorSpread and Wingspread
dataNI <- transform(dataNI, call_door = ifelse(!is.na(DoorSpread), DoorSpread, (7.49+(7.70*log(Depth)))))

dataNI <- transform(dataNI, call_wing_DS = ifelse(!is.na(WingSpread), WingSpread, (5.28+0.27*DoorSpread)))
dataNI <- transform(dataNI, call_wing_DP = ifelse(!is.na(WingSpread)| !is.na(DoorSpread), WingSpread, (8.61+(1.79*log(Depth)))))

dataNI <- dataNI %>% mutate(call_wing = coalesce(call_wing_DS, call_wing_DP))          


dataNI %>%
  mutate(
    SweptAreaDSKM2 = call_distance * call_door / 1000000,
    SweptAreaWSKM2 = call_distance * call_wing / 1000000) 

write.csv(dataNI,"C:/Users/ceciliak/Documents/ceciliak/swept area algorithms/NIGFS.csv")   


############
# SCOROC #
############

# Select survey, year and quarter
years <- 2011:2020
survey <- "SCOROC"
quarters <- 1:4

#Download HH exchange data
HH <- getDATRAS(record = "HH", survey, years, quarters)

#Make -9/0 into NA
HH[HH == 0] <- NA
HH[HH == -9] <- NA

#Make a call_distance column that include calculations for NA values
dataSR <- HH
dataSR <- dataSR %>% filter(HaulVal == "V")
#sum(is.na(data$Distance))

dataSR <- dataSR %>% mutate(
  call_distance = ifelse(!is.na(Distance), 
                         Distance,
                        HaulDur/60*1852*GroundSpeed))
                              

#sum(is.na(dataSR$WingSpread))#0
#sum(is.na(dataSR$DoorSpread))#3

#calculate missing DoorSpread 
dataSR <- transform(dataSR, call_door = ifelse(!is.na(DoorSpread) , DoorSpread, (28.57 + (21.27*log(Depth)))))

#calculate missing wingspread 
dataSR <- transform(dataSR, call_wing = ifelse((!is.na(WingSpread)), WingSpread, (12.43+(1.48*log(Depth)))))

dataSR %>%
  mutate(
    SweptAreaDSKM2 = call_distance * call_door / 1000000,
    SweptAreaWSKM2 = call_distance * call_wing / 1000000) 


write.csv(dataSR,"C:/Users/ceciliak/Documents/ceciliak/swept area algorithms/SCOROC.csv") 

############
# ROCKALL #
############

# Select survey, year and quarter
years <- 1999:2009
survey <- "ROCKALL"
quarters <- 1:4

#Download HH exchange data
HH <- getDATRAS(record = "HH", survey, years, quarters)

#Make -9/0 into NA
HH[HH == 0] <- NA
HH[HH == -9] <- NA

#Make a call_distance column that include calculations for NA values
dataRK <- HH
dataRK <- dataRK %>% filter(HaulVal == "V")
#sum(is.na(data$Distance))

dataRK <- dataRK %>% mutate(
  call_distance = ifelse(!is.na(Distance), 
                         Distance,
                         HaulDur/60*1852*GroundSpeed))


sum(is.na(dataRK$WingSpread))#19
sum(is.na(dataRK$DoorSpread))#8

#calculate missing DoorSpread 
dataRK <- transform(dataRK, call_door = ifelse(!is.na(DoorSpread) , DoorSpread, (-23.35+(21.27*log(Depth)))))

#calculate missing wingspread 
dataRK <- transform(dataRK, call_wing = ifelse((!is.na(WingSpread)), WingSpread, (10.16+(2.01*log(Depth)))))



dataRK %>%
  mutate(
    SweptAreaDSKM2 = call_distance * call_door / 1000000,
    SweptAreaWSKM2 = call_distance * call_wing / 1000000) 


write.csv(dataRK,"C:/Users/ceciliak/Documents/ceciliak/swept area algorithms/ROCKALL.csv") 


############
# SCOWCGFS #
############
# Select survey, year and quarter
years <- 2011:2020
survey <- "SCOWCGFS"
quarters <- 1:4

#Download HH exchange data
HH <- getDATRAS(record = "HH", survey, years, quarters)

#Make -9/0 into NA
HH[HH == 0] <- NA
HH[HH == -9] <- NA

#Make a call_distance column that include calculations for NA values
dataSw <- HH
dataSw <- dataSw %>% filter(HaulVal == "V")
#sum(is.na(data$Distance))

dataSw <- dataSw %>% mutate(
  call_distance = ifelse(!is.na(Distance), 
                         Distance,
                         HaulDur/60*1852*GroundSpeed))

#sum(is.na(dataSw$WingSpread))#24
#sum(is.na(dataSw$DoorSpread))#3

dataSW <- dataSW %>% mutate(
  call_door = ifelse(
    !is.na(DoorSpread),
    DoorSpread,
    case_when(
      Quarter == 1 & SweepLngt <= 60 ~ -3.15 + 16.54  * log(Depth),
      Quarter == 1 & SweepLngt > 60 ~ -21.5 +24.14 * log(Depth),
      Quarter == 4 & SweepLngt <= 60 ~ -19.1 + 20.3 * log(Depth),
      Quarter == 4 & SweepLngt > 60 ~ -12.24 + 21.96 * log(Depth)
    )
  )
)
dataSW <- dataSW %>% mutate(
  call_wing = ifelse(
    !is.na(WingSpread),
    WingSpread,
    case_when(
      Quarter == 1  & SweepLngt <= 60 ~ 5.89 + 3.09 * log(Depth),
      Quarter == 1  & SweepLngt > 60 ~ 11.28 + 1.86 * log(Depth),
      Quarter == 4 & SweepLngt <= 60 ~ 2.89 + 3.73 * log(Depth),
      Quarter == 4 & SweepLngt > 60 ~ 9.24 + 2.16 * log(Depth)
    )
  ))


dataSW %>%
  mutate(
    SweptAreaDSKM2 = call_distance * call_door / 1000000,
    SweptAreaWSKM2 = call_distance * call_wing / 1000000) 


write.csv(dataSW,"C:/Users/ceciliak/Documents/ceciliak/swept area algorithms/SCOWCGFS.csv") 


############
# SWC-IBTS #
############

# Select survey, year and quarter
years <- 1985:2010
survey <- "SWC-IBTS"
quarters <- 1:4

#Download HH exchange data
HH <- getDATRAS(record = "HH", survey, years, quarters)

#Make -9/0 into NA
HH[HH == 0] <- NA
HH[HH == -9] <- NA

#Make a call_distance column that include calculations for NA values
dataSI <- HH
dataSI<- dataSI %>% filter(HaulVal == "V")
#sum(is.na(data$Distance))

dataSI <- dataSI %>% mutate(
  call_distance = ifelse(!is.na(Distance), 
                         Distance,
                         HaulDur/60*1852*GroundSpeed))



sum(is.na(dataSI$WingSpread))#897
sum(is.na(dataSI$DoorSpread))#858

#calculate missing DoorSpread 
dataSI <- transform(dataSI, call_door = ifelse(!is.na(DoorSpread) , DoorSpread, -23.35 + 21.27*log(Depth)))

#calculate missing wingspread 
dataSI <- transform(dataSI, call_wing = ifelse((!is.na(WingSpread)), WingSpread, 10.16 + 2.01*log(Depth)))

dataSI %>%
  mutate(
    SweptAreaDSKM2 = call_distance * call_door / 1000000,
    SweptAreaWSKM2 = call_distance * call_wing / 1000000) 




write.csv(dataSI,"C:/Users/ceciliak/Documents/ceciliak/swept area algorithms/SWC-IBTS.csv")


############
# FR-CGFS  #
############
# Select survey, year and quarter
years <- 2015:2020
survey <- "FR-CGFS"
quarters <- 1:4

#Download HH exchange data
HH <- getDATRAS(record = "HH", survey, years, quarters)

#Make -9/0 into NA
HH[HH == 0] <- NA
HH[HH == -9] <- NA

#Make a call_distance column that include calculations for NA values
dataFC <- HH
dataFC <- dataFC %>% filter(HaulVal == "V")
#sum(is.na(data$Distance))

dataFC <- dataFC %>% mutate(
  call_distance = ifelse(!is.na(Distance), 
                         Distance,
                         HaulDur/60*1852*GroundSpeed))


#sum(is.na(dataFC$WingSpread))#73
#sum(is.na(dataFC$DoorSpread))#0

#calculate missing DoorSpread 
dataFC <- transform(dataFC, call_door = ifelse(!is.na(DoorSpread) , DoorSpread, 0.92+14.94*log(Depth)))

#calculate missing wingspread 
dataFC<- transform(dataFC, call_wing = ifelse((!is.na(WingSpread)), WingSpread, 6.62 + 0.17 * log(DoorSpread)))


dataFC %>%
  mutate(
    SweptAreaDSKM2 = call_distance * call_door / 1000000,
    SweptAreaWSKM2 = call_distance * call_wing / 1000000) 

write.csv(dataFC,"C:/Users/ceciliak/Documents/ceciliak/swept area algorithms/FR-CGFS.csv") 


############
#   EVHOE  #
############
# Select survey, year and quarter
years <- 2001:2020
survey <- "EVHOE"
quarters <- 1:4

#Download HH exchange data
HH <- getDATRAS(record = "HH", survey, years, quarters)

#Make -9/0 into NA
HH[HH == 0] <- NA
HH[HH == -9] <- NA

#Make a call_distance column that include calculations for NA values
dataEV <- HH
dataEV <- dataEV %>% filter(HaulVal == "V")
#sum(is.na(data$Distance))

dataEV <- dataEV %>% mutate(
  call_distance = ifelse(!is.na(Distance), 
                         Distance,
                         HaulDur/60*1852*GroundSpeed))



sum(is.na(dataEV$WingSpread))#1087
sum(is.na(dataEV$DoorSpread))#670

dataEV <- dataEV %>%
  mutate(
    call_door = ifelse(
      !is.na(DoorSpread),
      DoorSpread,
      ifelse(
        SweepLngt <= 60,
        6.29 + 14.46 * log(Depth) ,
        -15.08 + 21.78* log(Depth) 
      )
    ),
    call_wing = ifelse(
      !is.na(WingSpread),
      WingSpread,
      ifelse(
        SweepLngt <= 60,
        11.03 + 1.92 * log(Depth) ,
        12.26 + 1.67* log(Depth)
      )
    )
  )

#calculate missing DoorSpread 
dataEV <- transform(dataEV, call_door = ifelse(!is.na(DoorSpread) , DoorSpread, (2.93+(17.66*log(Depth)))))

#calculate missing wingspread 
dataEV <- transform(dataEV, call_wing = ifelse((!is.na(WingSpread)), WingSpread, (7.94+(2.31*log(Depth)))))


dataEV %>%
  mutate(
    SweptAreaDSKM2 = call_distance * call_door / 1000000,
    SweptAreaWSKM2 = call_distance * call_wing / 1000000) 


write.csv(dataEV,"C:/Users/ceciliak/Documents/ceciliak/swept area algorithms/EVHOE.csv") 


############
# SP-NORTH #
############

# Select survey, year and quarter
years <- 1990:2020
survey <- "SP-NORTH"
quarters <- 1:4

#Download HH exchange data
HH <- getDATRAS(record = "HH", survey, years, quarters)

#Make -9/0 into NA
HH[HH == 0] <- NA
HH[HH == -9] <- NA

#Make a call_distance column that include calculations for NA values
dataSN <- HH
dataSN <- dataSN %>% filter(HaulVal == "V")
#sum(is.na(data$Distance))

dataSN <- dataSN %>% mutate(
  call_distance = ifelse(!is.na(Distance), 
                         Distance,
                         HaulDur/60*1852*GroundSpeed))

#sum(is.na(dataSN$WingSpread))#2077
#sum(is.na(dataSN$DoorSpread))#2316

#calculate missing DoorSpread 
dataSN <- transform(dataSN, call_door = ifelse(!is.na(DoorSpread) , DoorSpread, (13.42 + 16.25*log(Depth))))

#calculate missing wingspread 
dataSN <- transform(dataSN, call_wing = ifelse(!is.na(WingSpread), WingSpread, (7.1 + 2.46 *log(Depth))))


dataSN %>%
  mutate(
    SweptAreaDSKM2 = call_distance * call_door / 1000000,
    SweptAreaWSKM2 = call_distance * call_wing / 1000000)

write.csv(dataSN,"C:/Users/ceciliak/Documents/ceciliak/swept area algorithms/SP-NORTH.csv") 


############
# SP-PORC  #
############

# Select survey, year and quarter
years <- 2001:2020
survey <- "SP-PORC"
quarters <- 1:4

#Download HH exchange data
HH <- getDATRAS(record = "HH", survey, years, quarters)

#Make -9/0 into NA
HH[HH == 0] <- NA
HH[HH == -9] <- NA

#Make a call_distance column that include calculations for NA values
dataSP <- HH
dataSP <- dataSP %>% filter(HaulVal == "V")
#sum(is.na(data$Distance))

dataSP <- dataSP %>% mutate(
  call_distance = ifelse(!is.na(Distance), 
                         Distance,
                         HaulDur/60*1852*GroundSpeed))

sum(is.na(dataSP$WingSpread))#2077
sum(is.na(dataSP$DoorSpread))#2316

#calculate missing DoorSpread 
dataSP <- transform(dataSP, call_door = ifelse(!is.na(DoorSpread) , DoorSpread, (-56.94+(32.32*log(Depth)))))

#calculate missing wingspread 
dataSP <- transform(dataSP, call_wing = ifelse((!is.na(WingSpread)), WingSpread, (4.13+(2.59*log(Depth)))))


dataSP %>%
  mutate(
    SweptAreaDSKM2 = call_distance * call_door / 1000000,
    SweptAreaWSKM2 = call_distance * call_wing / 1000000)

write.csv(dataSP,"C:/Users/ceciliak/Documents/ceciliak/swept area algorithms/SP-PORC.csv") 

############
# SP-ARSA  #
############
# Select survey, year and quarter
years <- 1996:2020
survey <- "SP-ARSA"
quarters <- 1:4

#Download HH exchange data
HH <- getDATRAS(record = "HH", survey, years, quarters)

#Make -9/0 into NA
HH[HH == 0] <- NA
HH[HH == -9] <- NA

#Make a call_distance column that include calculations for NA values
dataSA <- HH
dataSA<- dataSA %>% filter(HaulVal == "V")
#sum(is.na(data$Distance))

dataSA <- dataSA %>% mutate(
  call_distance = ifelse(!is.na(Distance), 
                         Distance,
                         HaulDur/60*1852*GroundSpeed))

#sum(is.na(dataSA$WingSpread))#224
#sum(is.na(dataSA$DoorSpread))#906


dataSA <- dataSA %>% mutate(
  call_door = ifelse(
    !is.na(DoorSpread),
    DoorSpread,
    case_when(
      Quarter == 1 & SweepLngt <= 60 ~ 7.61 + 11.51 * log(Depth),
      Quarter == 1 & SweepLngt > 60 ~ -4.93 + 18.93 * log(Depth),
      Quarter == 4 & SweepLngt <= 60~ -7.19 + 15.95 * log(Depth),
      Quarter == 4 & SweepLngt > 60 ~ -3.49 + 15.95 * log(Depth)
        )
      )
    )
dataSA <- dataSA %>% mutate(
call_wing = ifelse(
  !is.na(WingSpread),
  WingSpread,
  case_when(
    Quarter == 1  & SweepLngt <= 60 ~ 2.19 + 3.87 * log(Depth),
    Quarter == 1  & SweepLngt > 60 ~ 7.06 + 2.51 * log(Depth),
    Quarter == 4 & SweepLngt <= 60 ~ -2.08 + 2.51 * log(Depth),
    Quarter == 4 & SweepLngt > 60 ~ 7.34 + 2.51 * log(Depth)
  )
))

dataSA %>%
  mutate(
    SweptAreaDSKM2 = call_distance * call_door / 1000000,
    SweptAreaWSKM2 = call_distance * call_wing / 1000000) 



############
# IE-IGFS  #
############
# Select survey, year and quarter
years <- 2003:2020
survey <- "IE-IGFS"
quarters <- 1:4

#Download HH exchange data
HH <- getDATRAS(record = "HH", survey, years, quarters)

#Make -9/0 into NA
HH[HH == 0] <- NA
HH[HH == -9] <- NA

#Make a call_distance column that include calculations for NA values
dataIG <- HH
dataIG <- dataIG %>% filter(HaulVal == "V")
#sum(is.na(data$Distance))

dataIG <- dataIG %>% mutate(
  call_distance = ifelse(!is.na(Distance), 
                         Distance,
                         ifelse(is.na(GroundSpeed),
                                HaulDur/60*1852*GroundSpeed,
                                1.852 * 360 * 60 / 2 * pi*(acos(cos(radians(ShootLat)) * cos(radians(HaulLat)) * cos(radians(HaulLong) - radians(ShootLong)) + sin(radians(ShootLat)) * sin(radians(HaulLat)))))))


dataIG <- dataIG %>%
  mutate(
    call_door = ifelse(
      !is.na(DoorSpread),
      DoorSpread,
      ifelse(
        SweepLngt <= 60,
        38.0 + 10.83 * log(Depth),
        29.76 + 17.8 * log(Depth)
      )
    ),
    call_wing = ifelse(
      !is.na(WingSpread),
      WingSpread,
      ifelse(
        SweepLngt <= 60,
        12.69 + 2.13 * log(Depth),
        14.2 + 1.8 * log(Depth)
      )
    )
  )

dataIG %>%
  mutate(
    SweptAreaDSKM2 = call_distance * call_door / 1000000,
    SweptAreaWSKM2 = call_distance * call_wing / 1000000) 

write.csv(dataIG,"C:/Users/ceciliak/Documents/ceciliak/swept area algorithms/IE-IGFS.csv") 

############
# IE-AIMS  #
############
# Select survey, year and quarter
years <- 2016:2020
survey <- "IE-IAMS"
quarters <- 1:4

#Download HH exchange data
HH <- getDATRAS(record = "HH", survey, years, quarters)

#Make -9/0 into NA
HH[HH == 0] <- NA
HH[HH == -9] <- NA

#Make a call_distance column that include calculations for NA values
dataIA <- HH
dataIA <- dataIA %>% filter(HaulVal == "V")
#sum(is.na(data$Distance))

dataIA <- dataIA %>% mutate(
  call_distance = ifelse(!is.na(Distance), 
                         Distance,
                         HaulDur/60*1852*GroundSpeed))

#calculate missing DoorSpread 
dataIA<- transform(dataIA, call_door = ifelse(!is.na(DoorSpread) , DoorSpread, (16.06 + 14.66 * log(Depth))))

#calculate missing wingspread 
dataIA <- transform(dataIA, call_wing = ifelse((!is.na(WingSpread)), WingSpread, (14.49 + 2.75 * log(Depth))))



dataIA %>%
  mutate(
    SweptAreaDSKM2 = call_distance * call_door / 1000000,
    SweptAreaWSKM2 = call_distance * call_wing / 1000000) 

write.csv(dataIA,"C:/Users/ceciliak/Documents/ceciliak/swept area algorithms/IE-IAMS.csv") 
