#####################################################################
############ Code Beyond main effects - Data preparation ############
#####################################################################

############ Introduction ############ 
# I received the datasets for the mega-analysis all in different formats 
# In this script, I am putting them in the same structure + make remove all variables
# that I received, but did not need

# Goal: 1 File per dataset in long-format with the following variables
# 1. New ID --> remove original ID variable
# 2. Age
# 3. Sex
# 4. time (observation across all)
# 5. week/day (DD/ESM)
# 6. PA items (if available)
# 7. NA items
# 8. Depression baseline items
# 9. Depression FU items (if available)

############ Packages ############ 
library(haven) #for reading in SPSS files
library(plyr) #for data wrangling
library(dplyr) #for data wrangling
library(here) #for showing where files are
library(esmpack) #for ESM functions

############ Dataset 1: RADAR ############ 

### Read in data

#all data are in 1 dataset (wide format)
RADAR_data <-read_sav(file=here::here("data","1_raw_data_as_received","1_RADAR data.sav"))

### Rename variables
RADAR_data<-plyr::rename(RADAR_data, 
                   c("sex" =  "Sex",
                     "age" = "Age",
                     "ID_GEZIN" = "ID"))

RADAR_data<-as.data.frame(RADAR_data) 

### Reshape into longformat
# find out where variables are in df
which(colnames(RADAR_data)=="dm12aa01_1") # item 1 (happiness) wave 1.2 - beep 1
which(colnames(RADAR_data)=="dm54aa01_5") # item 1 (happiness) wave 5.4 - beep 75
which(colnames(RADAR_data)=="dm54aa02_5") # item 2 (happiness) wave 5.4 - beep 75
which(colnames(RADAR_data)=="dm54aa03_5") # item 3 (happiness) wave 5.4 - beep 75
which(colnames(RADAR_data)=="dm54aa04_5") # item 4 (anger) wave 5.4 - beep 75
which(colnames(RADAR_data)=="dm54aa05_5") # item 5 (anger) wave 5.4 - beep 75
which(colnames(RADAR_data)=="dm54aa06_5") # item 6 (anger) wave 5.4 - beep 75
which(colnames(RADAR_data)=="dm54aa07_5") # item 7 (anxiety) wave 5.4 - beep 75
which(colnames(RADAR_data)=="dm54aa08_5") # item 8 (anxiety) wave 5.4 - beep 75
which(colnames(RADAR_data)=="dm54aa09_5") # item 9 (anxiety) wave 5.4 - beep 75
which(colnames(RADAR_data)=="dm54aa10_5") # item 10 (sadness) wave 5.4 - beep 75
which(colnames(RADAR_data)=="dm54aa11_5") # item 11 (sadness) wave 5.4 - beep 75
which(colnames(RADAR_data)=="dm54aa12_5") # item 12 (sadness) wave 5.4 - beep 75

# restructuring
RADAR_data_long<-reshape(RADAR_data, 
                           varying=list
                           (PA1=c(161:235),
                             PA2=c(236:310),
                             PA3=c(311:385),
                             NA1=c(386:460),
                             NA2=c(461:535),
                             NA3=c(536:610),
                             NA4=c(611:685),
                             NA5=c(686:760),
                             NA6=c(761:835),
                             NA7=c(836:910),
                             NA8=c(911:985),
                             NA9=c(986:1060)),
                           v.names=c("PA1","PA2","PA3",
                                     "NA1","NA2","NA3",
                                     "NA4","NA5","NA6",
                                     "NA7","NA8","NA9"),
                           direction="long",
                           times=1:75,
                           timevar="time") 

### order by ID and time
RADAR_data_long<-RADAR_data_long[order(RADAR_data_long$ID,RADAR_data_long$time) , ]

### Make new ID variable
RADAR_data_long <- 
  RADAR_data_long %>%
  group_by(ID) %>%
  mutate(ID = cur_group_id()) %>%
  mutate(ID = paste("RADAR", ID, sep="_"))

### Check Sex and Age variables
table(RADAR_data_long$Sex)
range(RADAR_data_long$Age,na.rm = TRUE)

### Construct week variable
#was not in the dataset
RADAR_data_long$week<-NA

# year 1 #
RADAR_data_long$week[RADAR_data_long$time<=5 & RADAR_data_long$time>=1] <-1
RADAR_data_long$week[RADAR_data_long$time<=10 & RADAR_data_long$time>=6] <-2
RADAR_data_long$week[RADAR_data_long$time<=15 & RADAR_data_long$time>=11] <-3

# year 2 #
RADAR_data_long$week[RADAR_data_long$time<=20 & RADAR_data_long$time>=16] <-4
RADAR_data_long$week[RADAR_data_long$time<=25 & RADAR_data_long$time>=21] <-5
RADAR_data_long$week[RADAR_data_long$time<=30 & RADAR_data_long$time>=26] <-6

# year 3 #
RADAR_data_long$week[RADAR_data_long$time<=35 & RADAR_data_long$time>=31] <-7
RADAR_data_long$week[RADAR_data_long$time<=40 & RADAR_data_long$time>=36] <-8
RADAR_data_long$week[RADAR_data_long$time<=45 & RADAR_data_long$time>=41] <-9

# year 4 #
RADAR_data_long$week[RADAR_data_long$time<=50 & RADAR_data_long$time>=46] <-10
RADAR_data_long$week[RADAR_data_long$time<=55 & RADAR_data_long$time>=51] <-11
RADAR_data_long$week[RADAR_data_long$time<=60 & RADAR_data_long$time>=56] <-12

# year 5 # 
RADAR_data_long$week[RADAR_data_long$time<=65 & RADAR_data_long$time>=61] <-13
RADAR_data_long$week[RADAR_data_long$time<=70 & RADAR_data_long$time>=66] <-14
RADAR_data_long$week[RADAR_data_long$time<=75 & RADAR_data_long$time>=71] <-15

### Possible cleaning of "faulty beeps"
# checking if number of days was maximum 75 
range(RADAR_data_long$time, na.rm = TRUE) #correct

### Compute scale scores
# Calculating means of items - Depression #

which(colnames(RADAR_data_long)=="ra11aa02") # dep item 2 - wave 1 (baseline)
which(colnames(RADAR_data_long)=="ra11aa30") # dep item 30 - wave 1 (baseline)
which(colnames(RADAR_data_long)=="ra61aa02") # dep item 2 - wave 6 (follow-up)
which(colnames(RADAR_data_long)=="ra61aa30") # dep item 30 - wave 6 (follow-up)


RADAR_data_long$DepB <- combitems(c(4:26), data=RADAR_data_long)
RADAR_data_long$DepF <- combitems(c(135:157), data=RADAR_data_long)

# Calculating means of items - PA #
which(colnames(RADAR_data_long)=="PA1") 
RADAR_data_long$PAff <- combitems(c(163:165), data=RADAR_data_long)

# Calculating means of items - NA #
which(colnames(RADAR_data_long)=="NA1") 
which(colnames(RADAR_data_long)=="NA9") 
RADAR_data_long$NAff <- combitems(c(166:174), data=RADAR_data_long)

# order
RADAR_data_long<-RADAR_data_long[order(RADAR_data_long$ID,RADAR_data_long$time) , ]

### Removal of all variables that you do not need
dataset1_RADAR_processed <- 
  RADAR_data_long %>%
  select("ID", "Age", "Sex", "time", "week",
         "PA1","PA2","PA3",
         "NA1","NA2","NA3",
         "NA4","NA5","NA6",
         "NA7","NA8","NA9",
         "PAff", "NAff",
         starts_with('ra11aa', ignore.case = FALSE),"DepB",
         starts_with('ra61aa', ignore.case = FALSE),"DepF")

### Check for NaN's
df <- as.data.frame(cbind(lapply(lapply(dataset1_RADAR_processed, is.nan), sum)))
rownames(subset(df, df$V1 != 0))
# recode NaN's into NA's
dataset1_RADAR_processed[sapply(dataset1_RADAR_processed, is.nan)] <- NA

### Write csv file
write.csv(dataset1_RADAR_processed, here::here("data", "2_processed_data_for_OSF", "dataset1_RADAR_processed.csv"), row.names = FALSE)

############ Dataset 2: Swinging Moods ############ 

### Read in data
## ESM 
SM_data_ESM <-read_sav(file=here::here("data","1_raw_data_as_received","2_Swinging Moods ESM data.sav"))

# reorder variables 
SM_data_ESM <- SM_data_ESM[, c(1:7,8,10,12,13,15,18,9,11,14,16,17,19)]
names(SM_data_ESM)

## Depression
# Baseline
SM_data_DepB <-read_sav(file=here::here("data","1_raw_data_as_received","2_Swinging Moods Dep B data.sav"))


# Follow-up
SM_data_DepFU <-read_sav(file=here::here("data","1_raw_data_as_received","2_Swinging Moods Dep F data.sav"))

### Rescale emotion items
#to 1 to 9 to enable merging with other datasets 

psych::describe(SM_data_ESM[, c(8:19)])

SM_data_ESM$PA1<-scales::rescale(SM_data_ESM$PA1, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))
SM_data_ESM$PA2<-scales::rescale(SM_data_ESM$PA2, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))
SM_data_ESM$PA3<-scales::rescale(SM_data_ESM$PA3, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))
SM_data_ESM$PA4<-scales::rescale(SM_data_ESM$PA4, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))
SM_data_ESM$PA5<-scales::rescale(SM_data_ESM$PA5, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))
SM_data_ESM$PA6<-scales::rescale(SM_data_ESM$PA6, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))

SM_data_ESM$NA1<-scales::rescale(SM_data_ESM$NA1, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))
SM_data_ESM$NA2<-scales::rescale(SM_data_ESM$NA2, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))
SM_data_ESM$NA3<-scales::rescale(SM_data_ESM$NA3, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))
SM_data_ESM$NA4<-scales::rescale(SM_data_ESM$NA4, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))
SM_data_ESM$NA5<-scales::rescale(SM_data_ESM$NA5, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))
SM_data_ESM$NA6<-scales::rescale(SM_data_ESM$NA6, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))

psych::describe(SM_data_ESM[, c(8:19)]) #problem solved

### Merge into one dataset in longformat
SM_data_long <- left_join(SM_data_ESM, SM_data_DepB, 
                            by = "ID")

SM_data_long <- left_join(SM_data_long, SM_data_DepFU, 
                          by = "ID")

### Rename variables
SM_data_long<-plyr::rename(SM_data_long, c("Sex_d" = "Sex"))

### Make new ID variable
SM_data_long <- 
  SM_data_long %>%
  group_by(ID) %>%
  mutate(ID = cur_group_id()) %>%
  mutate(ID = paste("SM", ID, sep="_"))

### Check Sex and Age variables
table(SM_data_long$Sex)
range(SM_data_long$Age,na.rm = TRUE)


### Possible cleaning of "faulty beeps"
# Originally, for this study, there were 9 beeps scheduled, however, due to a malfunction, there were sometimes more beeps for some participants.
# I decided to delete any beep that is more than 9 per day to stay consistent

# For that, I will make a new observation variable per day, so that I can delete all observations within each day that are more than 9
# (originally there was only an observation variable for all observations but not PER day)
subdataset <- SM_data_long %>% group_by(ID, Day) %>% dplyr::mutate(obs = rank(Obs, ties.method = 'first'))%>% arrange(Day, obs)

subdataset<-subdataset[order(subdataset$ID,subdataset$Obs) , ]

range(subdataset$obs, na.rm = TRUE)
range(subdataset$Day, na.rm = TRUE)

# Delete observations with more than 9 beeps per day
subdataset1 <-subset(subdataset, obs<10) 
range(subdataset1$obs)

# Make new overall observation variable
# running from 1 to 54 (cause 6 days x 9 beeps)
subdataset2 <- subdataset1 %>% group_by(ID) %>% dplyr::mutate(time = rank(Obs, ties.method = 'first'))%>% arrange(Obs)

SM_data_long<-subdataset2[order(subdataset2$ID,subdataset2$time) , ]

range(SM_data_long$time)

### Compute scale scores
# Calculating means of items - Depression #
# Note: The following items need to be recoded
# Dep_4, dep_8, dep_12, dep_16

# Baseline 
range(SM_data_long$dep_4,na.rm=TRUE) #min - max is 1-4

SM_data_long$dep_4R <- 5 - SM_data_long$dep_4
SM_data_long$dep_8R <- 5 - SM_data_long$dep_8
SM_data_long$dep_12R <- 5 - SM_data_long$dep_12
SM_data_long$dep_16R <- 5 - SM_data_long$dep_16


## Follow-up 
SM_data_long$Fdep4R <- 5 - SM_data_long$Fdep4
SM_data_long$Fdep8R <- 5 - SM_data_long$Fdep8
SM_data_long$Fdep12R <- 5 - SM_data_long$Fdep12
SM_data_long$Fdep16R <- 5 - SM_data_long$Fdep16

names(SM_data_long)

SM_data_long$DepB <- combitems(c(28:30,32:34,36:38,40:42,44:47,76:79), data=SM_data_long)
SM_data_long$DepF <- combitems(c(54:56,58:60,62:64,66:68,70:73,80:83), data=SM_data_long)

# Calculating means of items - PA #
which(colnames(SM_data_long)=="PA1") 
which(colnames(SM_data_long)=="PA6") 
SM_data_long$PAff <- combitems(c(8:13), data=SM_data_long)

# Calculating means of items - NA #
which(colnames(SM_data_long)=="NA1") 
which(colnames(SM_data_long)=="NA6") 
SM_data_long$NAff <- combitems(c(14:19), data=SM_data_long)

### Removal of all variables that you do not need
dataset2_SM_processed <- 
  SM_data_long %>%
  select("ID", "Age", "Sex", "time", "Day",
         "PA1","PA2","PA3",
         "PA4","PA5","PA6",
         "NA1","NA2","NA3",
         "NA4","NA5","NA6",
         "PAff", "NAff",
         starts_with('dep_', ignore.case = FALSE),"DepB",
         starts_with('Fdep', ignore.case = FALSE),"DepF")

### Check for NaN's
df <- as.data.frame(cbind(lapply(lapply(dataset2_SM_processed, is.nan), sum)))
rownames(subset(df, df$V1 != 0))
# recode NaN's into NA's
dataset2_SM_processed[sapply(dataset2_SM_processed, is.nan)] <- NA


### Write csv file
write.csv(dataset2_SM_processed, here::here("data", "2_processed_data_for_OSF", "dataset2_SM_processed.csv"), row.names = FALSE)

############ Dataset 3: Mood in Emerging Adults ############ 

### Read in data
## ESM 
MA_data_ESM <-read_sav(file=here::here("data","1_raw_data_as_received","3_Mood in emerging adults ESM data.sav"))

# reorder variables 
MA_data_ESM <- MA_data_ESM[, c(1:6, 8, 10, 11, 13, 16, 7, 9, 12, 14, 15, 17)]
names(MA_data_ESM)

# convert emotion items to numeric
MA_data_ESM <- MA_data_ESM %>% 
  mutate_at(c(6:17), as.numeric) 

## Depression
# Baseline
MA_data_DepB <-read_sav(file=here::here("data","1_raw_data_as_received","3_Mood in emerging adults Dep B data.sav"))


# Follow-up
MA_data_DepFU <-read_sav(file=here::here("data","1_raw_data_as_received","3_Mood in emerging adults Dep F data.sav"))

### Rescale emotion items
# to 1 to 9 to enable merging with other datasets 

psych::describe(MA_data_ESM[, c(6:17)])

# items NA5 and PA6 have values of 0, although the scale goes from 1-9
# these will be deleted (cause probably a mistake)
MA_data_ESM$PA6[MA_data_ESM$PA6==0] <- NA
MA_data_ESM$NA5[MA_data_ESM$NA5==0] <- NA

psych::describe(MA_data_ESM[, c(6:17)])

MA_data_ESM$PA1<-scales::rescale(MA_data_ESM$PA1, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))
MA_data_ESM$PA2<-scales::rescale(MA_data_ESM$PA2, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))
MA_data_ESM$PA3<-scales::rescale(MA_data_ESM$PA3, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))
MA_data_ESM$PA4<-scales::rescale(MA_data_ESM$PA4, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))
MA_data_ESM$PA5<-scales::rescale(MA_data_ESM$PA5, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))
MA_data_ESM$PA6<-scales::rescale(MA_data_ESM$PA6, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))

MA_data_ESM$NA1<-scales::rescale(MA_data_ESM$NA1, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))
MA_data_ESM$NA2<-scales::rescale(MA_data_ESM$NA2, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))
MA_data_ESM$NA3<-scales::rescale(MA_data_ESM$NA3, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))
MA_data_ESM$NA4<-scales::rescale(MA_data_ESM$NA4, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))
MA_data_ESM$NA5<-scales::rescale(MA_data_ESM$NA5, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))
MA_data_ESM$NA6<-scales::rescale(MA_data_ESM$NA6, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))

psych::describe(MA_data_ESM[, c(6:17)]) #problem solved

### Merge into one dataset in longformat
# rename idvariable from ESM dataset to enable merging
MA_data_ESM<-plyr::rename(MA_data_ESM, c("idsona" = "id"))
                                                   
MA_data_long <- left_join(MA_data_ESM, MA_data_DepB, 
                          by = "id")

MA_data_long <- left_join(MA_data_long, MA_data_DepFU, 
                          by = "id")

### Make new ID variable
MA_data_long <- 
  MA_data_long %>%
  group_by(id) %>%
  mutate(ID = cur_group_id()) %>%
  mutate(ID = paste("MA", ID, sep="_"))


### Rename variables
MA_data_long<-plyr::rename(MA_data_long, c("Bsex" = "Sex",
                                           "day" = "Day",
                                           "numb" = "Obs"))

### Check Sex and Age variables
table(MA_data_long$Sex)
range(MA_data_long$Age,na.rm = TRUE)

# There were two individuals with -6 as age. These will be set to NA
MA_data_long$Age[MA_data_long$Age==-6] <- NA
range(MA_data_long$Age,na.rm = TRUE)

### Possible cleaning of "faulty beeps"
# Originally, for this study, there were 5 beeps scheduled, however, due to a malfunction, there were sometimes more beeps for some participants.
# I decided to delete any beep that is more than 5 to stay consistent 
# For that, I will make a new observation variable per day, so that I can delete all observations within each day that are more 
# than 5 (originally there was only an observation variable for all observations but not PER day)

subdataset <- MA_data_long %>% group_by(ID, Day) %>% dplyr::mutate(obs = rank(Obs, ties.method = 'first'))%>% arrange(Day, obs)

subdataset<-subdataset[order(subdataset$ID,subdataset$Obs) , ]

range(subdataset$obs, na.rm = TRUE)
range(subdataset$Day, na.rm = TRUE)

# Delete observations with more than 5 beeps per day
subdataset1 <-subset(subdataset, obs<6) 
range(subdataset1$obs)

# Make new overall observation variable
#running from 1 to 55 (cause 11 days x 5 beeps)
subdataset2 <- subdataset1 %>% group_by(ID) %>% dplyr::mutate(time = rank(Obs, ties.method = 'first'))%>% arrange(Obs)

MA_data_long<-subdataset2[order(subdataset2$ID,subdataset2$time) , ]

range(MA_data_long$time)

### Compute scale scores
# Calculating means of items - Depression #
# Note: The following items need to be recoded
#Dep_4, dep_8, dep_12, dep_16

# Baseline 
range(MA_data_long$cesd4,na.rm=TRUE) #min - max is 1-4

MA_data_long$cesd4R <- 5 - MA_data_long$cesd4
MA_data_long$cesd8R <- 5 - MA_data_long$cesd8
MA_data_long$cesd12R <- 5 - MA_data_long$cesd12
MA_data_long$cesd16R <- 5 - MA_data_long$cesd16


## Follow-up ##
MA_data_long$Fdep4R <- 5 - MA_data_long$Fdep4
MA_data_long$Fdep8R <- 5 - MA_data_long$Fdep8
MA_data_long$Fdep12R <- 5 - MA_data_long$Fdep12
MA_data_long$Fdep16R <- 5 - MA_data_long$Fdep16

names(MA_data_long)

MA_data_long$DepB <- combitems(c(32:34,36:38,40:42,44:46,48:51,77:80), data=MA_data_long)
MA_data_long$DepF <- combitems(c(54:56,58:60,62:64,66:68,70:73,81:84), data=MA_data_long)

# Calculating means of items - PA #
which(colnames(MA_data_long)=="PA1") 
which(colnames(MA_data_long)=="PA6") 
MA_data_long$PAff <- combitems(c(6:11), data=MA_data_long)

# Calculating means of items - NA #
which(colnames(MA_data_long)=="NA1") 
which(colnames(MA_data_long)=="NA6") 
MA_data_long$NAff <- combitems(c(12:17), data=MA_data_long)

### Removal of all variables that you do not need
dataset3_MA_processed <- 
  MA_data_long %>%
  select("ID", "Age", "Sex", "time", "Day",
         "PA1","PA2","PA3",
         "PA4","PA5","PA6",
         "NA1","NA2","NA3",
         "NA4","NA5","NA6",
         "PAff", "NAff",
         starts_with('cesd', ignore.case = FALSE),"DepB",
         starts_with('Fdep', ignore.case = FALSE),"DepF")

### Check for NaN's
df <- as.data.frame(cbind(lapply(lapply(dataset3_MA_processed, is.nan), sum)))
rownames(subset(df, df$V1 != 0))
# recode NaN's into NA's
dataset3_MA_processed[sapply(dataset3_MA_processed, is.nan)] <- NA


### Write csv file
write.csv(dataset3_MA_processed, here::here("data", "2_processed_data_for_OSF", "dataset3_MA_processed.csv"), row.names = FALSE)

############ Dataset 4: Emotions in Daily Life ############ 

### Read in data
## ESM 
ED_data_ESM <-read_sav(file=here::here("data","1_raw_data_as_received","4_Emotions in daily life ESM data.sav"))

# Convert Date and time variables
ED_data_ESM$ResponseDateTime <- as.POSIXct(ED_data_ESM$ResponseDateTime, origin="1582-10-14")
ED_data_ESM$ResponseDate <-  as.Date(ED_data_ESM$ResponseDateTime)
ED_data_ESM$ResponseDatenum <-as.numeric(ED_data_ESM$ResponseDate)
  
# Rename emotion items
ED_data_ESM<-plyr::rename(ED_data_ESM, c("Time" = "Obs",
                                         "Day" = "valid_obs_day",
                                         "PA_enth" = "PA1",
                                         "NA_irri" = "NA1",
                                         "PA_tevr" = "PA2",
                                         "NA_verv" = "NA2",
                                         "PA_ener"  = "PA3",
                                         "NA_nerv" = "NA3",
                                         "PA_kalm" = "PA4",
                                         "NA_verdr" = "NA4",
                                         "PA_daadk" = "PA5",
                                         "NA_boos" = "NA5",
                                         "PA_vrol" = "PA6",
                                         "NA_somb" = "NA6",
                                         "PA_dankb" = "PA7"))

# reorder variables 
ED_data_ESM <- ED_data_ESM[, c(1:8, 11:13, 15, 17, 19, 9, 10, 14, 16, 18, 20, 21)]
names(ED_data_ESM)

## Depression
# Baseline
ED_data_DepB <-read_sav(file=here::here("data","1_raw_data_as_received","4_Emotions in daily life Dep B data.sav"))


ED_data_DepB<-plyr::rename(ED_data_DepB, 
                             c("age_yr" =  "Age",
                               "gesl" = "Sex",
                               "BDI" = "DepB"))

### Rescale emotion items
# to 1 to 9 to enable merging with other datasets 

psych::describe(ED_data_ESM[, c(8:20)])

ED_data_ESM$PA1<-scales::rescale(ED_data_ESM$PA1, to = c(1,9), from = range(0,100, na.rm = TRUE, finite = TRUE))
ED_data_ESM$PA2<-scales::rescale(ED_data_ESM$PA2, to = c(1,9), from = range(0,100, na.rm = TRUE, finite = TRUE))
ED_data_ESM$PA3<-scales::rescale(ED_data_ESM$PA3, to = c(1,9), from = range(0,100, na.rm = TRUE, finite = TRUE))
ED_data_ESM$PA4<-scales::rescale(ED_data_ESM$PA4, to = c(1,9), from = range(0,100, na.rm = TRUE, finite = TRUE))
ED_data_ESM$PA5<-scales::rescale(ED_data_ESM$PA5, to = c(1,9), from = range(0,100, na.rm = TRUE, finite = TRUE))
ED_data_ESM$PA6<-scales::rescale(ED_data_ESM$PA6, to = c(1,9), from = range(0,100, na.rm = TRUE, finite = TRUE))
ED_data_ESM$PA7<-scales::rescale(ED_data_ESM$PA7, to = c(1,9), from = range(0,100, na.rm = TRUE, finite = TRUE))

ED_data_ESM$NA1<-scales::rescale(ED_data_ESM$NA1, to = c(1,9), from = range(0,100, na.rm = TRUE, finite = TRUE))
ED_data_ESM$NA2<-scales::rescale(ED_data_ESM$NA2, to = c(1,9), from = range(0,100, na.rm = TRUE, finite = TRUE))
ED_data_ESM$NA3<-scales::rescale(ED_data_ESM$NA3, to = c(1,9), from = range(0,100, na.rm = TRUE, finite = TRUE))
ED_data_ESM$NA4<-scales::rescale(ED_data_ESM$NA4, to = c(1,9), from = range(0,100, na.rm = TRUE, finite = TRUE))
ED_data_ESM$NA5<-scales::rescale(ED_data_ESM$NA5, to = c(1,9), from = range(0,100, na.rm = TRUE, finite = TRUE))
ED_data_ESM$NA6<-scales::rescale(ED_data_ESM$NA6, to = c(1,9), from = range(0,100, na.rm = TRUE, finite = TRUE))

psych::describe(ED_data_ESM[, c(8:20)]) #problem solved

### Merge into one dataset in longformat
ED_data_long <- left_join(ED_data_ESM, ED_data_DepB, 
                          by = "Eth_ID")

### Make new ID variable
ED_data_long <- 
  ED_data_long %>%
  group_by(Eth_ID) %>%
  mutate(ID = cur_group_id()) %>%
  mutate(ID = paste("ED", ID, sep="_"))

### Check Sex and Age variables
table(ED_data_long$Sex)
# recode into 0 and 1 to be consistent with other datasets
ED_data_long$Sex[ED_data_long$Sex==1] <- 0
ED_data_long$Sex[ED_data_long$Sex==2] <- 1
table(ED_data_long$Sex)

range(ED_data_long$Age,na.rm = TRUE)
# There were some people with a higher age than 29 (going up until 61)
# All of these will be deleted
nsub(ID, ED_data_long)
ED_data_long <-subset(ED_data_long, Age < 30)
nsub(ID, ED_data_long) #201 of 244 included


### Possible cleaning of "faulty beeps"

# Originally, for this study, there were 5 beeps scheduled for 14 days, however, due to a malfunction, there were sometimes more beeps and days
# for some participants. 
# I decided to delete any beep that is more than 5 and 14 days to stay consistent

# Add day variable within ID based on date
#(because there was no variable indicating the day, thus I constructed it based on the date)
ED_data_long<-ED_data_long %>% 
  dplyr::group_by(ID) %>% 
  dplyr::mutate(Day=dense_rank(as.numeric(ResponseDate)))

range(ED_data_long$Obs)
range(ED_data_long$Day)

# For that, I will make a new observation variable per day, so that I can delete all observations within each day that are more than 5
subdataset <- ED_data_long %>% group_by(ID, Day) %>% dplyr::mutate(obs = rank(Obs, ties.method = 'first'))%>% arrange(Day, obs)

subdataset<-subdataset[order(subdataset$ID,subdataset$Obs) , ]

# Delete observations with more than 5 beeps per day
subdataset1 <-subset(subdataset, obs<6) 
range(subdataset1$obs)

# Delete all days after day 14
subdataset2 <-subset(subdataset1, Day<15) 
range(subdataset2$Day)

# Make new overall observation variable
# running from 1 to 70 (cause 14 days x 5 beeps)
subdataset3 <- subdataset2 %>% group_by(ID) %>% dplyr::mutate(time = rank(Obs, ties.method = 'first'))%>% arrange(Obs)

ED_data_long<-subdataset3[order(subdataset3$ID,subdataset3$time) , ]

range(ED_data_long$time)


### Compute scale scores
# Calculating means of items - Depression #

# Baseline 
ED_data_long$DepB <- combitems(c(28:48), data=ED_data_long)

# Calculating means of items - PA #
which(colnames(ED_data_long)=="PA1") 
which(colnames(ED_data_long)=="PA7") 
ED_data_long$PAff <- combitems(c(8:14), data=ED_data_long)

# Calculating means of items - NA #
which(colnames(ED_data_long)=="NA1") 
which(colnames(ED_data_long)=="NA6") 
ED_data_long$NAff <- combitems(c(15:20), data=ED_data_long)

### Removal of all variables that you do not need
dataset4_ED_processed <- 
  ED_data_long %>%
  select("ID", "Age", "Sex", "time", "Day",
         "PA1","PA2","PA3",
         "PA4","PA5","PA6", "PA7",
         "NA1","NA2","NA3",
         "NA4","NA5","NA6",
         "PAff", "NAff",
         starts_with('bdi', ignore.case = FALSE),"DepB")

### Check for NaN's
df <- as.data.frame(cbind(lapply(lapply(dataset4_ED_processed, is.nan), sum)))
rownames(subset(df, df$V1 != 0))
# recode NaN's into NA's
dataset4_ED_processed[sapply(dataset4_ED_processed, is.nan)] <- NA


### Write csv file
write.csv(dataset4_ED_processed, here::here("data", "2_processed_data_for_OSF", "dataset4_ED_processed.csv"), row.names = FALSE)
 


############ Dataset 5: Emotion Regulation in Action ############ 
### Read in data
## ESM & Depression in one dataset 
EA_data_long<-read_sav(file=here::here("data","1_raw_data_as_received","5_Emotion regulation in action data.sav"))

# reorder variables 
EA_data_long <- EA_data_long[, c(1,4:11,16,20,21,24,12:15,17:19,22,23,178,179,201:226)]
names(EA_data_long)

### Rescale emotion items
#to 1 to 9 to enable merging with other datasets 

psych::describe(EA_data_long[, c(8:22)])

EA_data_long$PA1<-scales::rescale(EA_data_long$PA1, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))
EA_data_long$PA2<-scales::rescale(EA_data_long$PA2, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))
EA_data_long$PA3<-scales::rescale(EA_data_long$PA3, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))
EA_data_long$PA4<-scales::rescale(EA_data_long$PA4, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))
EA_data_long$PA5<-scales::rescale(EA_data_long$PA5, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))
EA_data_long$PA6<-scales::rescale(EA_data_long$PA6, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))

EA_data_long$NA1<-scales::rescale(EA_data_long$NA1, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))
EA_data_long$NA2<-scales::rescale(EA_data_long$NA2, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))
EA_data_long$NA3<-scales::rescale(EA_data_long$NA3, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))
EA_data_long$NA4<-scales::rescale(EA_data_long$NA4, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))
EA_data_long$NA5<-scales::rescale(EA_data_long$NA5, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))
EA_data_long$NA6<-scales::rescale(EA_data_long$NA6, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))
EA_data_long$NA7<-scales::rescale(EA_data_long$NA7, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))
EA_data_long$NA8<-scales::rescale(EA_data_long$NA8, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))
EA_data_long$NA9<-scales::rescale(EA_data_long$NA9, to = c(1,9), from = range(1,7, na.rm = TRUE, finite = TRUE))

psych::describe(EA_data_long[, c(8:22)])



### Make new ID variable
EA_data_long <- 
  EA_data_long %>%
  group_by(ID) %>%
  mutate(ID = cur_group_id()) %>%
  mutate(ID = paste("EA", ID, sep="_"))


### Rename variables
EA_data_long<-plyr::rename(EA_data_long, c("age_d" =  "Age",
                                           "sex_d" = "Sex",
                                           "obs" = "Obs"))

### Check Sex and Age variables
table(EA_data_long$Sex)
range(EA_data_long$Age,na.rm = TRUE)

### Add day variable within ID based on date
# (dataset only contained a day variable that indicated whether assessments were on a Friday, Saturday or Sunday)
EA_data_long<-EA_data_long %>% 
  dplyr::group_by(ID) %>% 
  dplyr::mutate(Day=dense_rank(date))

range(EA_data_long$Obs)
range(EA_data_long$Day)

### Possible cleaning of "faulty beeps"
# Originally, for this study, there were 4 beeps scheduled during Friday and 
# 9 beeps scheduled during the weekend, however, due to a malfunction, there 
# were sometimes more beeps for some participants. I decided to delete any beep that is 
# more than 4 and 9, respectively to stay consistent

# For that, I will make a new observation variable per day, so that I can delete 
# all observations within each day that are more than 4 and 9 respectively (there 
# was only an overall observation variable, but no observation variable per day)

subdataset<- EA_data_long %>% group_by(ID, Day) %>% dplyr::mutate(obs = rank(Obs, ties.method = 'first'))%>% arrange(Day, obs)
subdataset<-subdataset[order(subdataset$ID,subdataset$Obs) , ]
tapply(subdataset$obs, subdataset$day, max)

# Delete observations with more than 4 or 9 beeps per day
# (new_day=1 is Friday, new_day = 2 is Saturday, new_day = 3 is Sunday; 
# note that Friday had 4 beeps, Saturday and Sunday had 9)
subdataset2 <-subset(subdataset, day == "Fri" & obs<5 | day == "Sat" & obs<10 | day == "Sun" & obs<10) 
range(subdataset2$obs)

# correct beeps per day
tapply(subdataset2$obs, subdataset2$day, max)

# Make new overall observation variable
# running from 1 to 44 (cause 2 days x 4 beeps and 4 days x 9 beeps)
subdataset3 <- subdataset2 %>% group_by(ID) %>% dplyr::mutate(time = rank(Obs, ties.method = 'first'))%>% arrange(Obs)

EA_data_long<-subdataset3[order(subdataset3$ID,subdataset3$time) , ]

range(EA_data_long$time) #no adolescent had all 44 beeps (highest was 43)

### Compute scale scores

# Calculating means of items - Depression #

# Baseline 
range(EA_data_long$cdi2,na.rm=TRUE) #min - max is 1-3
# recoding
EA_data_long$cdi2R <- 4 - EA_data_long$cdi2
EA_data_long$cdi5R <- 4 - EA_data_long$cdi5
EA_data_long$cdi7R <- 4 - EA_data_long$cdi7
EA_data_long$cdi8R <- 4 - EA_data_long$cdi8
EA_data_long$cdi10R <- 4 - EA_data_long$cdi10
EA_data_long$cdi11R <- 4 - EA_data_long$cdi11
EA_data_long$cdi13R <- 4 - EA_data_long$cdi13
EA_data_long$cdi15R <- 4 - EA_data_long$cdi15
EA_data_long$cdi16R <- 4 - EA_data_long$cdi16
EA_data_long$cdi18R <- 4 - EA_data_long$cdi18
EA_data_long$cdi21R <- 4 - EA_data_long$cdi21
EA_data_long$cdi24R <- 4 - EA_data_long$cdi24
EA_data_long$cdi25R <- 4 - EA_data_long$cdi25

names(EA_data_long)

# calculation
EA_data_long$DepB <- combitems(c(25,27,28,30,35,37,40,42,43,45,46,49,50,53:65), data=EA_data_long)

# Calculating means of items - PA #
which(colnames(EA_data_long)=="PA1") 
which(colnames(EA_data_long)=="PA6") 
EA_data_long$PAff <- combitems(c(8:13), data=EA_data_long)

# Calculating means of items - NA #
which(colnames(EA_data_long)=="NA1") 
which(colnames(EA_data_long)=="NA9") 
EA_data_long$NAff <- combitems(c(14:22), data=EA_data_long)

### Removal of all variables that you do not need
dataset5_EA_processed <- 
  EA_data_long %>%
  select("ID", "Age", "Sex", "time", "Day",
         "PA1","PA2","PA3",
         "PA4","PA5","PA6",
         "NA1","NA2","NA3",
         "NA4","NA5","NA6",
         "NA7","NA8","NA9",
         "PAff", "NAff",
         starts_with('cdi', ignore.case = FALSE),"DepB")

### Check for NaN's
df <- as.data.frame(cbind(lapply(lapply(dataset5_EA_processed, is.nan), sum)))
rownames(subset(df, df$V1 != 0))
# recode NaN's into NA's
dataset5_EA_processed[sapply(dataset5_EA_processed, is.nan)] <- NA


### Write csv file
write.csv(dataset5_EA_processed, here::here("data", "2_processed_data_for_OSF", "dataset5_EA_processed.csv"), row.names = FALSE)

############ Dataset 6: LASER ############ 

### Read in data
## ESM 
LASER_data_ESM <-read_sav(file=here::here("data","1_raw_data_as_received","6_LASER data.sav"))

# Rename variables
LASER_data_ESM<-plyr::rename(LASER_data_ESM, c("Prompt" = "time",
                                         "First_Intensity" = "NAff",
                                         "Feeling_Now" = "Bip_aff",
                                         "Time" = "obs_withinday",
                                         "Female" = "Sex",
                                         "PARTID" = "ID"))
## Depression
# Baseline
LASER_data_DepB <-read_sav(file=here::here("data","1_raw_data_as_received","6_LASER Dep items.sav"))

# Rename variables
LASER_data_DepB<-plyr::rename(LASER_data_DepB, c("PARTID" = "ID"))

### Rescale emotion items
# to 1 to 9 to enable merging with other datasets 

psych::describe(LASER_data_ESM$NAff)

LASER_data_ESM$NAff<-scales::rescale(LASER_data_ESM$NAff, to = c(1,9), from = range(1,100, na.rm = TRUE, finite = TRUE))

psych::describe(LASER_data_ESM$NAff)


### Merge into one dataset in longformat
# ID number is numeric in ESM, but string in DepB dataset (there is an "L" in front of
# the ID number)
# I am adding it in the ESM dataset
LASER_data_ESM$ID<-as.character(LASER_data_ESM$ID)
LASER_data_ESM$ID <- paste("L", LASER_data_ESM$ID , sep = "")

LASER_data_long <- left_join(LASER_data_ESM, LASER_data_DepB, 
                          by = "ID")

### Make new ID variable
LASER_data_long <- 
  LASER_data_long %>%
  group_by(ID) %>%
  mutate(ID = cur_group_id()) %>%
  mutate(ID = paste("LASER", ID, sep="_"))

### Check Sex and Age variables
table(LASER_data_long$Sex)
range(LASER_data_long$Age,na.rm = TRUE)
# There were some people with a higher age than 29 (going up until 48)
# All of these will be deleted
LASER_data_long <-subset(LASER_data_long, Age < 30) 
range(LASER_data_long$Age,na.rm = TRUE)
n_LASER<-nsub(ID, LASER_data_long)
n_LASER #182 of 185 included


### Possible cleaning of "faulty beeps"
# checking if number of beeps was maximum 42
range(LASER_data_long$time, na.rm = TRUE) #correct


### Compute scale scores
# Calculating means of items - Depression #

# Baseline 
# Some items had characters and have already been recoded
# These items are: BDI_15R and BDI_17R
LASER_data_long$DepB <- combitems(c(36:49,51,53:56), data=LASER_data_long)

# There is just 1 item for NA (negative affect) and 1 item for BA (bipolar affect)
# So, no scale scores can be calculated for the affect items

### Removal of all variables that you do not need
dataset6_LASER_processed <- 
  LASER_data_long %>%
  select("ID", "Age", "Sex", "time", "Day",
         "NAff", "Bip_aff",
         starts_with('BDI', ignore.case = FALSE),"DepB",
         -"BDI_MEAN.x",-"BDI_MEAN.y",
         -"BDI_15",-"BDI_17")

### Check for NaN's
df <- as.data.frame(cbind(lapply(lapply(dataset6_LASER_processed, is.nan), sum)))
rownames(subset(df, df$V1 != 0))
# recode NaN's into NA's
dataset6_LASER_processed[sapply(dataset6_LASER_processed, is.nan)] <- NA


### Write csv file
write.csv(dataset6_LASER_processed, here::here("data", "2_processed_data_for_OSF", "dataset6_LASER_processed.csv"), row.names = FALSE)





############ Dataset 7: YES ############ 

### Read in data
## ESM 
YES_data_ESM <-read_sav(file=here::here("data","1_raw_data_as_received","7_YES data.sav"))

# Rename variables
YES_data_ESM<-plyr::rename(YES_data_ESM, c("PromptNumber" = "time",
                                           "INTENSITY_1" = "NAff",
                                           "FEELINGNOW" = "Bip_aff",
                                           "Time" = "obs_withinday",
                                           "Gender_1" = "Sex",
                                           "Age_1" = "Age",
                                           "PARTID" = "ID"))

# Transform affect variables into numeric
YES_data_ESM$Bip_aff<-as.numeric(YES_data_ESM$Bip_aff)
YES_data_ESM$NAff<-as.numeric(YES_data_ESM$NAff)

# assign 99999 as missing value
YES_data_ESM[YES_data_ESM == 99999] <- NA

## Depression
# Baseline
YES_data_DepB <-read_sav(file=here::here("data","1_raw_data_as_received","7_YES Dep B items.sav"))

# There were 3 IDs that had two depression scores in there (which differed).
# I cross-checked with the file that I had gotten with the ESM data that also contained the 
# Depression sumscore to identify the "correct" one. The duplicate IDs are removed

YES_data_DepB$PARTID[duplicated(YES_data_DepB$PARTID)]
YES_data_DepB <- YES_data_DepB %>% group_by(PARTID) %>% dplyr::mutate(number = rank(CDI1_MEAN, ties.method = 'first'))

# ID Y1026
YES_data_ESM %>%
  dplyr::filter(ID == "Y1026")  %>%
  dplyr::select(ID, CDI1_TOTAL)

YES_data_DepB %>%
  dplyr::filter(PARTID == "Y1026")  %>%
  dplyr::select(PARTID, CDI1_MEAN, number)

# 1.73 is correct (number = 2)

# ID 1033
YES_data_ESM %>%
  dplyr::filter(ID == "Y1033")  %>%
  dplyr::select(ID, CDI1_TOTAL)

YES_data_DepB %>%
  dplyr::filter(PARTID == "Y1033")  %>%
  dplyr::select(PARTID, CDI1_MEAN, number)

# 1.38 is correct (number = 2)

# ID Y1170
YES_data_ESM %>%
  dplyr::filter(ID == "Y1170")  %>%
  dplyr::select(ID, CDI1_TOTAL)

YES_data_DepB %>%
  dplyr::filter(PARTID == "Y1170")  %>%
  dplyr::select(PARTID, CDI1_MEAN, number)

# 1.23 is correct (number = 2)


# remove rows in Depression datasets that are double 
YES_data_DepB <- YES_data_DepB %>%
  dplyr::mutate(exclude = 
                  ifelse(PARTID=="Y1026" & number == 1|
                         PARTID=="Y1033" & number == 1|
                         PARTID=="Y1170" & number == 1,
                         1,0)) %>%
  dplyr::filter(!exclude == 1)



# Follow-up
YES_data_DepFU <-read_sav(file=here::here("data","1_raw_data_as_received","7_YES Dep F items.sav"))

# Rename variables
YES_data_DepB<-plyr::rename(YES_data_DepB, c("PARTID" = "ID"))
YES_data_DepFU<-plyr::rename(YES_data_DepFU, c("PARTID" = "ID"))

### Rescale emotion items
# to 1 to 9 to enable merging with other datasets 

psych::describe(YES_data_ESM$NAff)

YES_data_ESM$NAff<-scales::rescale(YES_data_ESM$NAff, to = c(1,9), from = range(1,100, na.rm = TRUE, finite = TRUE))

psych::describe(YES_data_ESM$NAff)


### Merge into one dataset in longformat
YES_data_long <- left_join(YES_data_ESM, YES_data_DepB, 
                             by = "ID")

YES_data_long <- left_join(YES_data_long, YES_data_DepFU, 
                          by = "ID")

### Make new ID variable
YES_data_long <- 
  YES_data_long %>%
  group_by(ID) %>%
  mutate(ID = cur_group_id()) %>%
  mutate(ID = paste("YES", ID, sep="_"))

### Check Sex and Age variables
table(YES_data_long$Sex)
# there is 1 person that has a 2 (possibly mistake, since it is not labeled in SPSS file). This will be set to NA
YES_data_long$Sex[YES_data_long$Sex==2] <- NA
table(YES_data_long$Sex)

range(YES_data_long$Age,na.rm = TRUE)

### Possible cleaning of "faulty beeps"
# checking if number of beeps was maximum 42
range(YES_data_long$time, na.rm = TRUE) #correct


### Compute scale scores
# Calculating means of items - Depression #

# Some items were reversed but have already been recoded
YES_data_long$DepB <- combitems(c(21,23,24,26,31,33,36,38,39,41,42,45:59), data=YES_data_long)
YES_data_long$DepF <- combitems(c(64,66,67,69,74,76,79,81,82,84,85,88:102), data=YES_data_long)

# There is just 1 item for NA (negative affect) and 1 item for BA (bipolar affect)
# So, no scale scores can be calculated for the affect items

### Removal of all variables that you do not need
dataset7_YES_processed <- 
  YES_data_long %>%
  select("ID", "Age", "Sex", "time", "Day",
         "NAff", "Bip_aff",
         starts_with('CDI', ignore.case = FALSE),"DepB","DepF",
         -"CDI1_MEAN",-"CDI2_MEAN",
         -"CDI1_TOTAL",-"CDI2_TOTAL")

### Check for NaN's
df <- as.data.frame(cbind(lapply(lapply(dataset7_YES_processed, is.nan), sum)))
rownames(subset(df, df$V1 != 0))
# recode NaN's into NA's
dataset7_YES_processed[sapply(dataset7_YES_processed, is.nan)] <- NA


### Write csv file
write.csv(dataset7_YES_processed, here::here("data", "2_processed_data_for_OSF", "dataset7_YES_processed.csv"), row.names = FALSE)

