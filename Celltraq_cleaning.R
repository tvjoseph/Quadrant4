# JMJPFU
# 17 08 2016
# Celltraq data
###########################################################

# Reading in data

Vod_bat_discharge <- read.csv("C:/Customers/Celltraq/Celltraq_CSV_DATA/Celltraq_CSV_DATA/By Company/Vodafone/Vodafone_Battery_Discharge_Data.csv",sep="|",header=FALSE)

Vod_bat_vt <- read.csv("C:/Customers/Celltraq/Celltraq_CSV_DATA/Celltraq_CSV_DATA/By Company/Vodafone/Vodafone Battery VT.csv",sep="|",header=FALSE)

Vod_string_discharge <- read.csv("C:/Customers/Celltraq/Celltraq_CSV_DATA/Celltraq_CSV_DATA/By Company/Vodafone/Vodafone String Discharge Data.csv",sep="|",header=FALSE)

Vod_string_measurement <- read.csv("C:/Customers/Celltraq/Celltraq_CSV_DATA/Celltraq_CSV_DATA/By Company/Vodafone/Vodafone String Measurements.csv",sep="|",header=FALSE)



# Inserting Column names for the data



################################################################################


colnames(Vod_string_discharge) <- c("Unique_ID", "Site_Name", "Plant_Name", "String_Name",  "Measurement_Timestamp", "Voltage", "Current", "Manufacturer", "Model", "Battery_Type", "Battery_Voltage")

colnames(Vod_bat_discharge) <- c("Unique_ID", "Site_Name", "Plant_Name", "String_Name", "Battery_no", "Measurement_Timestamp", "Voltage", "Current", "Manufacturer", "Model", "Battery_Type", "Battery_Voltage")

colnames(Vod_bat_vt) <- c("Unique_ID", "Site_Name", "Plant_Name", "String_Name","Battery_no", "Measurement_Timestamp", "Voltage", "Voltage_High_Alarm", "Voltage_High_Warning","Voltage_Low_Warning", "Voltage_Low_Alarm", "Temperature","Temperature_High_Alarm", "Temperature_High_Warning","Temperature_Low_Warning", "Temperature_Low_Alarm","Manufacturer", "Model", "Battery_Type", "Battery_Voltage")

colnames(Vodafone.Battery.Conductance) <- c("Unique_ID", "Site_Name", "Plant_Name", "String_Name","Battery_no", "Measurement_Timestamp", "Conductance", "Conductance_High_Alarm", "Conductance_High_Warning","Conductance_Low_Warning", "Conductance_Low_Alarm","Manufacturer", "Model", "Battery_Type", "Battery_Voltage")

colnames(Vod_string_measurement) <- c("Unique_ID", "Site_Name", "Plant_Name", "String_Name", "Measurement_Timestamp", "Voltage", "Voltage_High_Alarm", "Voltage_High_Warning","Voltage_Low_Warning", "Voltage_Low_Alarm", "Temperature","Temperature_High_Alarm", "Temperature_High_Warning","Temperature_Low_Warning", "Temperature_Low_Alarm","Current", "Ripple_current","Manufacturer", "Model", "Battery_Type", "Battery_Voltage")

# Cleaning - 1
# The first cell of row 1 is a bit problem area. Need to clean that up a bit
Vod_string_discharge[1,1] <- Vod_string_discharge[2,1]
Vod_bat_vt[1,1] <- Vod_bat_vt[2,1]
Vod_bat_discharge[1,1] <- Vod_bat_discharge[2,1]
Vod_string_measurement[1,1] <- Vod_string_measurement[2,1]
Vodafone.Battery.Conductance[1,1] <- Vodafone.Battery.Conductance[2,1]

# Saving to csv

write.csv(Vod_string_discharge,"Vod_string_discharge.csv")

write.csv(Vod_string_measurement,"Vod_string_measurement.csv")

# Doing some summary stats with the data

# No of unique id's for the data

uid_df <- data.frame(table(Vod_bat_vt$Unique_ID))

     uid_df <- data.frame(sort(uid_df$Freq,decreasing = TRUE))
     
# Let us take one of the tools and get a feel of the data
     
library(dplyr)
     
     one_id <- Vod_bat_vt %>% filter(Unique_ID=="6C6F3E57-53FC-4215-9162-60A85CEBBAFD")

     # Let us now split the time stamp data
     
     one_id$year <- strptime(one_id$Measurement_Timestamp,format="%Y-%m-%d")
     one_id$time <- strftime(one_id$Measurement_Timestamp,format="%T") # Strips the time a charachter
     
     
    # One first level of visualisation
     
     plot(one_id$year,one_id$Voltage)
     lines(one_id$year,one_id$Voltage)
     
     plot(one_id$year,one_id$Voltage_High_Alarm)
     lines(one_id$year,one_id$Voltage_High_Alarm)

     plot(one_id$year,one_id$Voltage_High_Alarm)
     lines(one_id$year,one_id$Voltage_High_Alarm)
     
     # Lets write this data in csv
     write.csv(one_id,"one_id.csv")
     
     #########################
     # JMJPFU
     # 19 Aug 2016
     
     # Saving an RDS file
     
     saveRDS(Vod_bat_discharge,file="Vod_bat_discharge.rds")
     
     # JMJPFU
     # 22 Aug 2016
     
     write.csv(Vodafone.Battery.Conductance,"vod_conductance.csv")
     
     library(rattle)
     rattle()
     
     # ggobi wasnt starting. So the below command was tried
     
     source("http://www.ggobi.org/downloads/install.r")
  
     #
     write.csv(Vod_string_discharge,"vod_discharge.csv")
     
     ############################################################
     # JMJPFU
     # 23 Aug 2016
     
     # Today let us try merging the data sets so that it forms on single data set.
     
     # First experimentation on that
     
     # As a first step let us try looking at the Unique IDs and identifying how many of the
     # unique IDs repeat accross the 
     
     # JMJPFU
     # 25 Aug 2015
     
     # Trying to load the text file of Tiger
     
     tran_cell <- read.table("C:/Customers/Celltraq/BMISFPV_Aug10/BMISFPV_Aug10.txt",sep=",")
     
     
     # Let us now write the mrg files
     
     write.csv(Mrg_4,"mrg_4.csv")
     
     # As this is failing, let us start doing this file by file
     
     write.csv(Mrg_4[1:200000,],"mrg_41.csv")
     write.csv(Mrg_4[200001:400000,],"mrg_42.csv")
     write.csv(Mrg_4[400001:600000,],"mrg_43.csv")
     write.csv(Mrg_4[600001:800000,],"mrg_44.csv")
     write.csv(Mrg_4[800001:1000000,],"mrg_45.csv")
     write.csv(Mrg_4[1000001:1200000,],"mrg_46.csv")
     write.csv(Mrg_4[1200001:1400000,],"mrg_47.csv")
     write.csv(Mrg_4[1400001:1600000,],"mrg_48.csv")
     write.csv(Mrg_4[1600001:1800000,],"mrg_49.csv")
     write.csv(Mrg_4[1800001:2000000,],"mrg_50.csv")
     write.csv(Mrg_4[2000001:2200000,],"mrg_51.csv")
     write.csv(Mrg_4[2200001:2400000,],"mrg_52.csv")
     write.csv(Mrg_4[2400001:2600000,],"mrg_53.csv")
     write.csv(Mrg_4[2600001:2800000,],"mrg_54.csv")
     write.csv(Mrg_4[2800001:3000000,],"mrg_55.csv")
     write.csv(Mrg_4[3000001:3200000,],"mrg_56.csv")
     write.csv(Mrg_4[3200001:3400000,],"mrg_57.csv")
     write.csv(Mrg_4[3400001:3600000,],"mrg_58.csv")
     write.csv(Mrg_4[3600001:3800000,],"mrg_59.csv")
     write.csv(Mrg_4[3800001:4000000,],"mrg_60.csv")
     write.csv(Mrg_4[4000001:4200000,],"mrg_61.csv")
     write.csv(Mrg_4[4200001:4400000,],"mrg_62.csv")
     write.csv(Mrg_4[4400001:4600000,],"mrg_63.csv")
     write.csv(Mrg_4[4600001:4800000,],"mrg_64.csv")
     write.csv(Mrg_4[4800001:5000000,],"mrg_65.csv")
     write.csv(Mrg_4[5000001:5200000,],"mrg_66.csv")
     write.csv(Mrg_4[5200001:5400000,],"mrg_67.csv")
     
     
     write.csv(Mrg_4[5400001:5492750,],"mrg_68.csv")
     write.csv(Mrg_4[1600001:1800000,],"mrg_46.csv")
     write.csv(Mrg_4[1800001:2000000,],"mrg_47.csv")