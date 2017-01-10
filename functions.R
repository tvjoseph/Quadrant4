# JMJPFU
# 23 Sept 2016

# Removing some objects from R

rm(All_discharge_consol)
rm(newsubset)
save(Battery_discharge,file = "Battery_discharge.RData")

save(Battery_discharge1,file = "Battery_discharge1.RData")

save(Battery_discharge2,file = "Battery_discharge2.RData")

rm(Battery_discharge2)

# JMJPFU
# 11-Oct-2016

# Now to create certain functions ans a whole program to create new variables for the batteries.

##############################################################################

# Function 1 : To create a new data set for the selected batteries

bat_select <- function(battery){ # Give the name of the battery where the dataframe has to be developed
  
  # For creating the relevant data sets from all the three measurement groups
  
  
  filt4 <- filt1 %>% filter(Unique_ID == battery)
  filt4 <- unique(filt4) # To remove duplicate data if any
  Temp_bat <- Battery_discharge %>% filter(Unique_ID == battery)
  Temp_bat <- unique(Temp_bat) # To remove duplicate data if any
  Temp_bat_sec <- Battery_voltemp %>% filter(Unique_ID == battery)
  Temp_bat_sec <- unique(Temp_bat_sec) # To remove duplicate data if any
  
  # Consolidating all info together
  
  if(nrow(Temp_bat) > 0 & nrow(Temp_bat_sec)>0){
    
    Temp_bat1 <- Temp_bat %>% select(Measurement_Timestamp,Voltage)
    Temp_bat1$measure <- "Voltage"
    
    Temp_bat2 <- Temp_bat %>% filter(Current < 50) %>% select(Measurement_Timestamp,Current)
    Temp_bat2$measure <- "Current"
    
    Temp_bat3 <- filt4 %>% select(Measurement_Timestamp,Conductance)
    Temp_bat3$measure <- "Conductance"
    
    Temp_bat4 <- Temp_bat_sec %>% select(Measurement_Timestamp,Temperature)
    Temp_bat4$measure <- "Temperature"
    
    Temp_bat5 <- Temp_bat_sec %>% select(Measurement_Timestamp,Voltage)
    Temp_bat5$measure <- "Volttemp"
    
    colnames(Temp_bat1) <- colnames(Temp_bat2) <- colnames(Temp_bat3) <- colnames(Temp_bat4) <- colnames(Temp_bat5) <- c("Measurement_Timestamp","Variable","measure")
    
    Temp_bat_consol <- rbind(Temp_bat1,Temp_bat2,Temp_bat3,Temp_bat4,Temp_bat5)
    
    # Date variables
    
    Temp_bat_consol$Date <- as.Date(as.character(Temp_bat_consol$Measurement_Timestamp))
    
    Temp_bat_consol$Date1 <- as.POSIXct(as.character(Temp_bat_consol$Measurement_Timestamp))
    
  } # End of the if file when all data sets are available
  
  else if(nrow(Temp_bat) > 0 & nrow(Temp_bat_sec)==0){
    
    Temp_bat1 <- Temp_bat %>% select(Measurement_Timestamp,Voltage)
    Temp_bat1$measure <- "Voltage"
    
    Temp_bat2 <- Temp_bat %>% filter(Current < 50) %>% select(Measurement_Timestamp,Current)
    Temp_bat2$measure <- "Current"
    
    Temp_bat3 <- filt4 %>% select(Measurement_Timestamp,Conductance)
    Temp_bat3$measure <- "Conductance"
    
    colnames(Temp_bat1) <- colnames(Temp_bat2) <- colnames(Temp_bat3) <- c("Measurement_Timestamp","Variable","measure")
    
    Temp_bat_consol <- rbind(Temp_bat1,Temp_bat2,Temp_bat3)
    
    # Date variables
    
    Temp_bat_consol$Date <- as.Date(as.character(Temp_bat_consol$Measurement_Timestamp))
    
    Temp_bat_consol$Date1 <- as.POSIXct(as.character(Temp_bat_consol$Measurement_Timestamp))
    
  } # End of the if condition when there is no data in the VT file
  
  Temp_bat_consol # Returning the data frame
  
  
} # End of the function

#######################################################################################################
# Function 2 : To calculate the slopes and the required data for the Voltage

bat_features <- function(bt,bat_test,Temp_bat_consol,float_volt){
  
  # Looping over all the battery cases of discharge voltage
  
  temp_volt_all <- data.frame(matrix(nrow=0,ncol = 5)) # Creating a data frame for consolidating accross all the dates
  
  colnames(temp_volt_all) <- names(Temp_bat_consol) # naming the temp data frame
  
  for(i in 1:bt){
    
    temp_dt <- paste(bat_test[i,1]) # Getting the dates where the discharge profiles are calculated
    
    Temp_bat_spot2 <- Temp_bat_consol %>% filter(Date == temp_dt ) # Taking all the data pertaining to that date
    
    # Getting the voltage data
    
    temp_volt <- Temp_bat_spot2 %>% filter(measure == "Voltage")
    
    temp_volt <- temp_volt[complete.cases(temp_volt),]
    
    # Looping over all the voltage data so as to find difference between voltage so as to see transition areas
    
    for(j in 1:(nrow(temp_volt)-1)){
      
      temp_volt$voltdiff[j] <- temp_volt$Variable[j+1] - temp_volt$Variable[j]
      
    }
    
    cou <- 0
    
    temp_volt$sign <- NA
    
    for(k in 1:(nrow(temp_volt)-1)){
      
      mult <- temp_volt$voltdiff[k] * temp_volt$voltdiff[k+1] # Doing a multiplication to find transition areas
      
      if(mult <=0){
        
        if(mult < 0){
          
          cou <- cou+1
          
          temp_volt$sign[k+1] <- paste0("change" ,cou)  }
        
        else if(mult == 0 & (temp_volt$voltdiff[k]< 0||temp_volt$voltdiff[k+1]< 0) ){ 
          
          cou <- cou+1
          
          temp_volt$sign[k+1] <- paste0("change" ,cou) 
          
        } # End of the else if statement
        
      } # If statement if mult < 0
    } # End of for loop for looping over all temp_volt data
    
    # Use the above data of sign and difference in voltage to calculate the slopes and depth of discharge
    
    temp_volt_set1 <- temp_volt %>% filter(!is.na(temp_volt$sign)) # taking only those values where the transition happens
    
    # Looping over all the sets where there is a transition
    
    # Do the next loops only if there is a sign change
    
    if(nrow(temp_volt_set1) > 0){
      
      for(i in 1:nrow(temp_volt_set1)){
        
        # For the first transition calculating the time difference and the difference between the variable values
        
        if(i==1){temp_volt_set1$timediff[i] <- as.numeric(difftime(temp_volt_set1$Date1[i],temp_volt$Date1[1], units = "secs"))
        temp_volt_set1$slp[i] <- temp_volt$Variable[1]-temp_volt_set1$Variable[i]
        
        temp_range <- min(temp_volt %>% filter(Date1 >= temp_volt$Date1[1] & Date1 <= temp_volt_set1$Date1[i]  ) %>% select(Variable))
        
        temp_volt_set1$dd[i] <- (temp_range/float_volt)
        
        }
        # For the Last transition calculating the time difference and the difference between the variable values
        else if(i==nrow(temp_volt_set1)){temp_volt_set1$timediff[i] <- as.numeric(difftime(temp_volt$Date1[nrow(temp_volt)],temp_volt_set1$Date1[i], units = "secs"))
        temp_volt_set1$slp[i] <- temp_volt_set1$Variable[i] - temp_volt$Variable[nrow(temp_volt)]
        temp_range <- min(temp_volt %>% filter(Date1 <= temp_volt$Date1[nrow(temp_volt)] & Date1 >= temp_volt_set1$Date1[i]) %>% select(Variable))
        
        temp_volt_set1$dd[i] <- (temp_range/float_volt)
        
        }
        # For all inbetween transitions calculating the time difference and the difference between the variable values
        else{temp_volt_set1$timediff[i] <- as.numeric(difftime(temp_volt_set1$Date1[i],temp_volt_set1$Date1[i-1], units = "secs"))
        temp_volt_set1$slp[i] <- temp_volt_set1$Variable[i-1] - temp_volt_set1$Variable[i]
        temp_range <- min(temp_volt %>% filter(Date1 <= temp_volt_set1$Date1[i] & Date1 >= temp_volt_set1$Date1[i-1]) %>% select(Variable))
        
        temp_volt_set1$dd[i] <- (temp_range/float_volt)
        
        }
        
        
      }
      
      
    } # End of If Loop to check if there is a changing profile
    
    else{
      
      temp_volt_set1 <- temp_volt[nrow(temp_volt),]
      
      temp_volt_set1$timediff <- as.numeric(difftime(temp_volt$Date1[nrow(temp_volt)],temp_volt$Date1[1], units = "secs"))
      
      temp_volt_set1$slp <- temp_volt$Variable[1]-temp_volt$Variable[nrow(temp_volt)]
      
      temp_range <- min(temp_volt %>% filter(Date1 >= temp_volt$Date1[1] & Date1 <= temp_volt$Date1[nrow(temp_volt)]  ) %>% select(Variable))
      
      temp_volt_set1$dd <- (temp_range/float_volt)
      
    } # End of the else condition if there is no changing profile
    
    # Take a subset of only those values where the time is more than 15 seconds
    
    #temp_volt_set2 <- temp_volt_set1 %>% filter(timediff > 14)
    
    # Creating a new subset for taking all discharge profiles
    
    temp_volt_set2 <- temp_volt_set1 %>% filter(timediff > 0)
    
    if(nrow(temp_volt_set2) > 0){
      
      temp_volt_set2$slope <- temp_volt_set2$slp / temp_volt_set2$timediff
      
      # Classifying whether it is a charge or discharge
      
      for(i in 1:nrow(temp_volt_set2)){
        
        temp <- temp_volt_set2$slp[i]
        
        ifelse(temp > 0,temp_volt_set2$profile[i] <- "Discharge",temp_volt_set2$profile[i] <- "Charge")
        
      }
      
      
      # Now to make some new variables out of the above readings
      
      # Making a new data set for variable as slope
      
      temp_volt_set3 <- temp_volt_set2 # Creating a new data set
      
      temp_volt_set3$Variable <- temp_volt_set3$slope # Making slope as the variable
      
      temp_volt_set3$measure <- temp_volt_set3$profile
      
      # Making a new data set only for Depth of discharge
      
      temp_volt_set4 <- temp_volt_set2 %>% filter(profile == "Discharge")
      
      if(nrow(temp_volt_set4)>0){
        
        temp_volt_set4$Variable <- temp_volt_set4$dd
        
        temp_volt_set4$measure <- "DOD"
      }
      
      # Combining these two together
      
      temp_volt_set <- rbind(temp_volt_set3,temp_volt_set4)
      
      temp_volt_set <- temp_volt_set[,1:5]
      
      
      temp_volt_all <- rbind(temp_volt_all,temp_volt_set)
      
    } # End of if loop for Temp_volt_set2 > 0
    
    
  
    
    } # End of loop for looping over all the time frame for a battery
  
  ###########################################################################################################
  
 
  temp_volt_all # Return the final set
  
} # End of the function

#############################################################################################################

# JMJPFU
# 17-Oct-2016

################ Starting function for calculating the mean slopes ############

dis_slope <- function(start_date,dis_rec){
  
  # Starting a counter for 15 readings i.e 4.5 years of slope
  month_consol <- data.frame(matrix(nrow=1,ncol=0))
  
  for(i in 1:15){
    if(i <=4){ # For first 4 iteratios
      
      month_1 <- start_date + (6*i*30*24*60*60)
      month_2 <- start_date + (6*(i-1)*30*24*60*60)
      month_i_rec <- dis_rec %>% filter(Date1 > month_2 & Date1 <= month_1) %>% select(Variable) %>% summarise(avg = mean(Variable,na.rm=TRUE))
      
      month_consol <- cbind(month_consol,month_i_rec)
      cou = 0
      
    }else{
      cou = cou+1
      mult = (6*i) - (3*cou)
      month_1 <- start_date + (mult*30*24*60*60)
      month_2 <- start_date + ((mult-3)*30*24*60*60)
      month_i_rec <- dis_rec %>% filter(Date1 > month_2 & Date1 <= month_1) %>% select(Variable) %>% summarise(avg = mean(Variable,na.rm=TRUE))
      
      month_consol <- cbind(month_consol,month_i_rec)
      
      
    } # End of the else condition
   
    
  } # End of the for loop
  
  month_consol # Return the consolidated records
  
} # End of the function 

############## Function for calculating the Conductance features #################

con_slope <- function(start_date,bat_rec){
  
  con_rec <- bat_rec %>% filter(measure == "Conductance")
  month_consol <- data.frame(matrix(nrow=1,ncol=0))
  
  for(i in 1:15){ # 15 because we are taking the data for first 15 sets of data: First 2 years at an interval of every 6 months and after that every 3 months
    if(i <=4){ # For first 4 iteratios. 4 because the first 2 years we take data at an interval of 6 months 
      
      month_1 <- start_date + (6*i*30*24*60*60)
      month_2 <- start_date + (6*(i-1)*30*24*60*60)
      month_i_rec <- con_rec %>% filter(Date1 > month_2 & Date1 <= month_1) %>% select(Variable,Measurement_Timestamp)
      
      if(nrow(month_i_rec) > 1){
        # Taking only data which is 3 standard deviation from mean for modelling
        up <- mean(month_i_rec$Variable) + (3*(sd(month_i_rec$Variable))) # Uppper range
        low <- mean(month_i_rec$Variable) - (3*(sd(month_i_rec$Variable))) # Lower range
        
        if(up != low){
          month_i_rec <- month_i_rec %>% filter(Variable > low & Variable < up) # Filtering the new data
          mod1 <- lm(Variable~as.numeric(Measurement_Timestamp),data=month_i_rec) # Modelling
          int <- data.frame(mod1$coefficients[1]) # Intercept of the readings
          slp <-  data.frame(mod1$coefficients[2]) # Slope of the reading
          
          month_consol <- cbind(month_consol,int,slp)
          cou = 0
        }else{
          int <- data.frame(NA)
          slp <- data.frame(NA)
          month_consol <- cbind(month_consol,int,slp)
          cou = 0
        }
        
      }else{ # If the number of records are less than 2
        
        int <- data.frame(NA)
        slp <- data.frame(NA)
        month_consol <- cbind(month_consol,int,slp)
        cou = 0
      } # End of the else loop when the records are less than 2
      
    }else{
      cou = cou+1
      mult = (6*i) - (3*cou)
      month_1 <- start_date + (mult*30*24*60*60)
      month_2 <- start_date + ((mult-3)*30*24*60*60)
      month_i_rec <- con_rec %>% filter(Date1 > month_2 & Date1 <= month_1) %>% select(Variable,Measurement_Timestamp)
      
      if(nrow(month_i_rec) > 1){
        # Taking only data which is 3 standard deviation from mean for modelling
        up <- mean(month_i_rec$Variable) + (3*(sd(month_i_rec$Variable))) # Uppper range
        low <- mean(month_i_rec$Variable) - (3*(sd(month_i_rec$Variable))) # Lower range
        
        
        # Only if the data show some variability there will be a linear model. Otherwise there will not be any model
        if(up !=low){
          month_i_rec <- month_i_rec %>% filter(Variable >= low & Variable <= up) # Filtering the new data
          mod1 <- lm(Variable~as.numeric(Measurement_Timestamp),data=month_i_rec) # Modelling
          int <- data.frame(mod1$coefficients[1]) # Intercept of the readings
          slp <-  data.frame(mod1$coefficients[2]) # Slope of the reading
          
          month_consol <- cbind(month_consol,int,slp)
        }else{
          int <- data.frame(NA)
          slp <- data.frame(NA)
          month_consol <- cbind(month_consol,int,slp)
        }
        
        
      }else{ # If the number of records are less than 2
        
        int <- data.frame(NA)
        slp <- data.frame(NA)
        month_consol <- cbind(month_consol,int,slp)
        
      } # End of the else loop when the records are less than 2
      
      
      
    } # End of the else condition
    
    
  } # End of the for loop
  
  
  month_consol # Returning this value
  
  
} # End of the function

######################### Function to get the DOD data ##########################

dod_max <- function(start_date,dod_rec){
  # Starting a counter for 15 readings i.e 4.5 years of max DOD
  month_consol <- data.frame(matrix(nrow=1,ncol=0))
  
  for(i in 1:15){
    if(i <=4){ # For first 4 iteratios
      
      month_1 <- start_date + (6*i*30*24*60*60)
      month_2 <- start_date + (6*(i-1)*30*24*60*60)
      month_i_rec <- dod_rec %>% filter(Date1 > month_2 & Date1 <= month_1) %>% select(Variable) %>% summarise(Minimum = min(Variable,na.rm=TRUE))
      
      month_consol <- cbind(month_consol,month_i_rec)
      cou = 0
      
    }else{
      cou = cou+1
      mult = (6*i) - (3*cou)
      month_1 <- start_date + (mult*30*24*60*60)
      month_2 <- start_date + ((mult-3)*30*24*60*60)
      month_i_rec <- dod_rec %>% filter(Date1 > month_2 & Date1 <= month_1) %>% select(Variable) %>% summarise(Minimum = min(Variable,na.rm=TRUE))
      
      month_consol <- cbind(month_consol,month_i_rec)
      
      
    } # End of the else condition
    
    
  } # End of the for loop
  
  month_consol # Return the consolidated records
  
  
  
}

############################################
# JMJPFU
# 19-Dec-2016

# Starting a new function for calculating the conductance slope where we look at if the conductance slope continuously falls
# with some gap in first set of readings to the other


condropFeat <- function(batlist,condf,thresh,thresh2){
  # batlist : This is the list of batteries which needs to be passed whose conductance drop needs to be calculated
  # condf : This is the conductance data frame which has to be passed for calculating the conductance
  # thresh : This is the threshold drop in conductance which is passed as a parameter 
  # thresh2 : This is another measure of threshold which should be applied to the subsequent points indicating the value to which those values can rise
  
  confinDf <- data.frame(matrix(nrow=0,ncol=4)) # Defining a data frame to consolidate all the results for the batlist
  failFeat <- data.frame(matrix(nrow=length(batlist),ncol=2)) # Defining another data frame to store the consolidated failure drops
  names(failFeat) <- c("Battery","FailDrop")
  secname <- paste0("Drop_",thresh)
  thirname <- paste0("Dropclass_",thresh)
  names(confinDf) <- c("Battery","Date1",secname,thirname)
  
  
  for(i in 1:length(batlist)){
    
    #print(i)
    
    
    tempBat <- batlist[i] # Take one battery at a time
    
    failFeat[i,1] <- tempBat # Load the battery name for the consolidated data frame
    
    batRec <- condf %>% filter(Battery == tempBat) %>% filter(measure == "Conductance") %>% arrange(Date1) # Take the relevant battery record
    
    
    findDf <- data.frame(matrix(nrow=nrow(batRec),ncol=4)) # Defining the individual battery data extractor
    
    names(findDf) <- c("Battery","Date1",secname,thirname)
    
    batRec$Condev <- 0 # Not required
    findDf$Battery <- tempBat
    
    for(j in 1:(nrow(batRec)-1)){
      
      varPre <- batRec$Variable[j] # Take the preceeding value of Conductance
      varSub <- batRec$Variable[j+1] # Take the subsequent value of Conductance
      conDate <- batRec$Date1[j+1] # Take the date
      conDiff <- varSub - varPre # Take the difference in conductance values
      
      if(varSub < varPre) { conThre <- (varPre * thresh * -1)  }else{ conThre <- varPre * thresh  } # Finding the threshold drop
      
      findDf[j+1,3] <- conDiff # Storing the difference value
      findDf[j+1,4] <- "NormalDrop"
      findDf$Date1[j+1] <- paste(conDate)
      
      if(conDiff < 0 & conDiff < conThre){ # Do any action only if the drop is greater than the threshold values
        
      
        conValPre <- min(batRec %>% filter(Date1 < conDate) %>% select(Variable)) # minimum value of the pre-value should not be less than the current value
        
        # The below if condition is required to take care of any values which is the last point
        
        if(j+1 != nrow(batRec)){ 
          
          conValPost <- max(batRec %>% filter(Date1 > conDate) %>% select(Variable)) # Maximum value after the point
          conRise <- conValPre + (conThre * thresh2) # Defining the point to which the ConValPost can rise
          
          if(varSub < conValPre & conValPost <= conRise){       
            
            findDf[j+1,4] <- "FailDrop"
            
          } # End of if value which checks if the drop is Failure indicating drop
          
        }else{
            
          if(varSub <= conRise){       
            
            findDf[j+1,4] <- "FailDrop"
            
          } # End of if value which checks if the drop is Failure indicating drop
          
          
          } # End of the else condition
        
        
        
        
      } # End of IF condition which checks if the drop is greater than the stipulated threshold 
      
      
      
    } # Looping over the records for the selected battery
    
    findDf[1,2] <- paste(batRec$Date1[1]) # Paste the first date
    
    findDf[1,3] <- 0 # default the drop value for the first record
    
    findDf[1,4] <- "NormalDrop" # default the drop class for the first record
    
    conFailDrop <- findDf %>% filter(findDf[,4] == "FailDrop") # Taking a consolidated view of all the failure drops
    
    if(nrow(conFailDrop) > 0){ 
      
      failFeat[i,2] <- sum(conFailDrop[,3]) # Sum up all the failure drops
      
    }else{
        
      failFeat[i,2] <- 0 # The Failure drop would be 0
      
      } # End of the if condition to check for any Failure drop
    
    
    confinDf <- rbind(confinDf,findDf) # Consolidating the DF value
    
  } # End of the for loop for running over the battery list
  
  
  conresult <- list(result1 = failFeat,result2 = confinDf) # Returning the results
  
  
} # End of the function


# Take the date of the j+1 value
# Look for all values of conductance before the J+1 the date
# If the no value before the compared date is below the taken value and no value after J+1 the point is above this point, Take note of the value as big drop value
# Keep doing this for all drops. The big drop has to be noted as a new charachter feature 
# If the above condition is not met, just take a note of the conDiff values against the J+1 element and keep moving
# Also check for a condition for the extreme points of j & J+1. Check for conditions that there are no more points

##########################################################################################################
# JMJPFU
# 21-Dec-2016

# A new function to calculate the features of batteries

batFeat <- function(bat_list,batdf){
  # batlist is the list of batteries for which we need the data frame
  # batdf is the consolidated data frame from which we need to take information about the batteries
  
  # First create the empty data frame
  
  Bat_Feat_new <- data.frame(matrix(nrow=0,ncol=65))
  colnames(Bat_Feat1_new) <- c("PD1dis","PD2dis","PD3dis","PD4dis","PD5dis","PD6dis","PD7dis","PD8dis","PD9dis","PD10dis","PD11dis","PD12dis","PD13dis","PD14dis","PD15dis","PD1in","PD1sl","PD2in","PD2sl","PD3in","PD3sl","PD4in","PD4sl","PD5in","PD5sl","PD6in","PD6sl","PD7in","PD7sl","PD8in","PD8sl","PD9in","PD9sl","PD10in","PD10sl","PD11in","PD11sl","PD12in","PD12sl","PD13in","PD13sl","PD14in","PD14sl","PD15in","PD15sl","PD1dod","PD2dod","PD3dod","PD4dod","PD5dod","PD6dod","PD7dod","PD8dod","PD9dod","PD10dod","PD11dod","PD12dod","PD13dod","PD14dod","PD15dod","Battery","Plant","Site","String","Condrop")
  
  for(i in 1:nrow(bat_list)){
    temp_bat <- paste(bat_list$Battery[i]) # Take one battery at a time
    temp_list <- bat_list[i,] # Take all the relevant records of the battery from the battery list
    # bat_rec <- bat_newfeat4 %>% filter(Battery == temp_bat) # Take all information from the selected battery
    bat_rec <- batdf %>% filter(Battery == temp_bat)
    temp_dates <- unique(bat_rec %>% select(Date1)) # Take all the unique dates
    start_date <- range(temp_dates$Date1)[1] # Calculating the start date so as to extract features every 6 months
    
    # First calculate the features related to Discharge slopes
    
    dis_rec <- bat_rec %>% filter(measure == "Discharge") # taking only the discharge data
    
    dis_df <- dis_slope(start_date,dis_rec) # Calling the function to calculate the discharge slopes
    
    names(dis_df) <- c("PD1dis","PD2dis","PD3dis","PD4dis","PD5dis","PD6dis","PD7dis","PD8dis","PD9dis","PD10dis","PD11dis","PD12dis","PD13dis","PD14dis","PD15dis")
    
    # Imputing 'NA' to all NaN values
    
    for(i in 1:15){
      
      if(dis_df[1,i]=="NaN"){dis_df[1,i] <- NA}
      
    }
    
    # Calculating the Conductance slopes
    
    con_df <- con_slope(start_date,bat_rec) # Getting a df with 30 variables for slopes
    names(con_df) <- c("PD1in","PD1sl","PD2in","PD2sl","PD3in","PD3sl","PD4in","PD4sl","PD5in","PD5sl","PD6in","PD6sl","PD7in","PD7sl","PD8in","PD8sl","PD9in","PD9sl","PD10in","PD10sl","PD11in","PD11sl","PD12in","PD12sl","PD13in","PD13sl","PD14in","PD14sl","PD15in","PD15sl")
    
    dod_rec <- bat_rec %>% filter(measure == "DOD") # taking only the discharge data
    
    dod_df <- dod_max(start_date,dod_rec) # Running the function to find the lowest point of DOD within the period of year
    
    # Imputing NA when the DOD is "Inf" value
    
    for(i in 1:15){
      
      if(dod_df[1,i]==Inf){dod_df[1,i] <- NA}
      
    }
    
    names(dod_df) <- c("PD1dod","PD2dod","PD3dod","PD4dod","PD5dod","PD6dod","PD7dod","PD8dod","PD9dod","PD10dod","PD11dod","PD12dod","PD13dod","PD14dod","PD15dod")
    
    # Condolidating all teh values
    
    
    bat_con1 <- cbind(dis_df,con_df,dod_df) # Condolidating the conductance and discharge data
    
    bat_con1$Battery <- temp_bat # Adding the battery name
    bat_con1$Plant <- temp_list$Plant
    bat_con1$Site <- temp_list$Site
    bat_con1$String <- temp_list$String
    # Getting new features for % drop in conductance from highest to lowest points.
    
    con_rec <- bat_rec %>% filter(measure == "Conductance") %>% select(Variable) # taking only the discharge data
    bat_con1$Condrop <- min(con_rec$Variable,na.rm = TRUE)/max(con_rec$Variable,na.rm = TRUE)
    
    # Combining with the original Battery Feature DF
    
    Bat_Feat_new <- rbind(Bat_Feat_new,bat_con1)
    
    
    
  } # End of the for loop
  
  
  Bat_Feat_new
  
} # End of the function

############################################################3

# Another function to create additional features related to conductance and DOD drops
batFeatAddn <- function(Bat_Feat1_new,batdf){
  
  # Bat_Feat1_new : This is the list of batteries with the features ( 65 nos)
  # batdf : Data frame which gives all the details of batteries
  
  
  Bat_Feat1_new$Dod50 <- Bat_Feat1_new$Dod80 <- Bat_Feat1_new$Dodtop <- NA
  
  Bat_Feat1_new$con50 <- Bat_Feat1_new$con80<- Bat_Feat1_new$contop <- NA
  
  for(i in 1:nrow(Bat_Feat1_new)){
    
    temp_bat <- paste(Bat_Feat1_new$Battery[i]) # Take one battery at a time
    
    #Features for DOD %
    bat_rec <- batdf %>% filter(Battery == temp_bat & measure == "DOD") %>% select(Variable)
    # 
    totdis <- nrow(bat_rec) # No of records with discharge profiles
    # 
    dod50 <- nrow(bat_rec %>% filter(Variable < 0.5))
    dod80 <- nrow(bat_rec %>% filter(Variable <= 0.85 & Variable >= 0.5)) # Changed the value from 0.8 to 0.85 on 4-Jan-2016
    dodtop <- totdis - (dod50 + dod80)
    # 
    Bat_Feat1_new$Dod50[i] <- (dod50/totdis)*100
    Bat_Feat1_new$Dod80[i] <- (dod80/totdis)*100
    Bat_Feat1_new$Dodtop[i] <- (dodtop/totdis)*100
    
    # Similar conductance drop bands
    
    bat_rec <- batdf %>% filter(Battery == temp_bat & measure == "Conductance") %>% select(Variable)
    
    totdis <- nrow(bat_rec) # No of records with Conductance profiles
    
    conmax <- max(bat_rec$Variable,na.rm = TRUE)
    
    con50 <- nrow(bat_rec %>% filter(Variable < (0.5 * conmax)))
    con80 <- nrow(bat_rec %>% filter(Variable >= (0.5 * conmax) & Variable <= (0.8 * conmax)))
    contop <- nrow(bat_rec %>% filter(Variable > (0.8 * conmax)))
    
    
    Bat_Feat1_new$con50[i] <- (con50/totdis)*100
    Bat_Feat1_new$con80[i] <- (con80/totdis)*100
    Bat_Feat1_new$contop[i] <- (contop/totdis)*100
    
    
    
  }
  
  
  
  Bat_Feat1_new # Return the data frame
  
  
  
} # End of the function

############################################################
# JMJPFU
# 2-Jan-2016
# This is a new function to calculate the New set of features for training the battery

# A new function to calculate the features of batteries

batFeat1 <- function(bat_list,batdf){
  # batlist is the battery for which we need to extract the dataframe
  # batdf is the consolidated data frame from which we need to take information about the batteries
  
  # First create the empty data frame
  
  batConFeats <- data.frame(matrix(nrow=0,ncol=15)) # Creating an empty data frame
  
  colnames(batConFeats) <- c("featDischarge","conSlope","dodMin","Dod50","Dod85","Dodtop","conDrop","con50","con80","contop","voltSD","Battery","Counter","Bench1","Bench2")
  
  temp_bat <- paste(bat_list) # Taking the battery
  
  bat_rec <- batdf %>% filter(Battery == temp_bat) # Take all relevant records from the battery
  
  # Create a new function to get temp_dates so that the duration is every 3 months and not as and when the discharge test is done
  
  datesUnique <- range(bat_rec$Date) # Getting the ranges of dates
  
  upperDate <- datesUnique[2] # Getting the upper date
  lowerDate <- datesUnique[1] # Getting the lower date
  
  temp_dates <- dateLister(upperDate,lowerDate) # Running the function for getting the dates
  
  #temp_dates <- unique(bat_rec %>% filter(measure=="Voltage") %>% select(Date)) # Take only the dates for the voltage as a benchmark
  
  # End of function call for temp_dates
  
  dis_rec <- bat_rec %>% filter(measure == "Discharge") # taking only the discharge data
  
  disDates <- unique(dis_rec %>% select(Date)) # List of all discharge dates
  
  disMean <- mean(dis_rec$Variable,na.rm = TRUE) # taking a mean value of discharge 
  
  dod_rec <- bat_rec %>% filter(measure == "DOD") # taking only the Depth of discharge data
  
  # Finding the Conductance max value based on mean of first 10% of the values
  
  conRec <- bat_rec %>% filter(measure == "Conductance") # Getting the conductace values
  
  conmax <- mean(conRec[1:round(nrow(conRec)*.1),2],na.rm = TRUE) # Find first 10% of values of the variable ( column 2) and find its mean
  
  
  
  # Start a for loop from back wards of the benchmark dates
  
  for(i in nrow(temp_dates):2){ 
    
    batTrainFeats <- data.frame(matrix(nrow=1,ncol=0)) # Creating an empty data frame
    
    bench_date <- temp_dates$Date[i] # Taking a benchmark date where features have to be extracted
    
    bench_date2 <- temp_dates$Date[i-1] # Taking the second benchmark date
    
    if(i == nrow(temp_dates) & i != 2){ # A check for last date. Also including another condition for i not being 2
      
      disData <- dis_rec %>% filter(Date >= bench_date2)
      
      if(nrow(disData) > 0){disData <- disData %>% summarise(featDischarge = mean(Variable)) }else{disData <- data.frame(disMean);names(disData) <- "featDischarge"}
      
      batTrainFeats <- cbind(batTrainFeats,disData) # Attaching the discharge data
      
      # Taking the features for slope of Conductance
      
      con_df <- conSlope(bench_date,bench_date2,bat_rec) # Running the new function for calculating the slope of Conductance
      
      names(con_df) <- "conSlope" # Renaming the conductance slopes
      
      row.names(con_df) <- NULL # Removing the row names
      
      batTrainFeats <- cbind(batTrainFeats,con_df) # Attaching the Conductance features
      
      # Taking features for Depth of discharge
      
      dod_df <- dodMin(bench_date,bench_date2,dod_rec) # Running the function to find the lowest point of DOD within the period of year
      
      batTrainFeats <- cbind(batTrainFeats,dod_df) # Attaching the DOD features
      
      dodRec <- dod_rec %>% filter(Date >= bench_date2) %>% select(Variable) 
      
      # If the dodRec generates an empty dataframe, the values get to infinity which is not desired. So we should impute it with
      # mean values
      
      if(nrow(dodRec) > 0){
       
        totdis <- nrow(dodRec) # No of records with discharge profiles
        # 
        dod50 <- nrow(dodRec %>% filter(Variable <= 0.5))
        dod85 <- nrow(dodRec %>% filter(Variable <= 0.85 & Variable > 0.5))
        dodtop <- totdis - (dod50 + dod85)
        
        Dod50 <- data.frame((dod50/totdis)*100);names(Dod50) <- "Dod50"
        Dod85 <- data.frame((dod85/totdis)*100);names(Dod85) <- "Dod85"
        Dodtop <- data.frame((dodtop/totdis)*100);names(Dodtop) <- "Dodtop"
        
        batTrainFeats <- cbind(batTrainFeats,Dod50,Dod85,Dodtop) # Attaching the DOD features 
        
        
      }else{
        
        dodRec <- dod_rec
        
        totdis <- nrow(dodRec) # No of records with discharge profiles
        # 
        dod50 <- nrow(dodRec %>% filter(Variable <= 0.5))
        dod85 <- nrow(dodRec %>% filter(Variable <= 0.85 & Variable > 0.5))
        dodtop <- totdis - (dod50 + dod85)
        
        Dod50 <- data.frame((dod50/totdis)*100);names(Dod50) <- "Dod50"
        Dod85 <- data.frame((dod85/totdis)*100);names(Dod85) <- "Dod85"
        Dodtop <- data.frame((dodtop/totdis)*100);names(Dodtop) <- "Dodtop"
        
        batTrainFeats <- cbind(batTrainFeats,Dod50,Dod85,Dodtop) # Attaching the DOD features 
        
        
      }
      
      
      # Taking features for Conductance
      
      con_rec <- conRec %>% filter(Date >= bench_date2) %>% select(Variable) # taking only the discharge data
      
      if(nrow(con_rec) > 0){
        
        Condrop <- data.frame(min(con_rec$Variable,na.rm = TRUE)/max(con_rec$Variable,na.rm = TRUE)) # Calculating the percentage drop in conductance
        
        names(Condrop) <- "conDrop"
        
        batTrainFeats <- cbind(batTrainFeats,Condrop) # Attaching the DOD features
        
       
      }else{
        
        # In case the number of records are nill, then all the values of conductance has to be taken
        
        con_rec <- conRec %>% select(Variable) # taking all discharge data
        
        # The formulae for Condrop is changed slightly below to include mean instead of the minimum value
        
        Condrop <- data.frame(mean(con_rec$Variable,na.rm = TRUE)/max(con_rec$Variable,na.rm = TRUE)) # Calculating the percentage drop in conductance
        
        names(Condrop) <- "conDrop"
        
        batTrainFeats <- cbind(batTrainFeats,Condrop) # Attaching the DOD features
        
      }
      
      # Getting other variables
      
      totdis <- nrow(con_rec) # No of records with Conductance profiles
      
      
      con50 <- nrow(con_rec %>% filter(Variable < (0.5 * conmax))) # conmax defined in the beginning part of the function
      con80 <- nrow(con_rec %>% filter(Variable >= (0.5 * conmax) & Variable < (0.8 * conmax)))
      contop <- nrow(con_rec %>% filter(Variable >= (0.8 * conmax)))
      
      
      con50 <- data.frame((con50/totdis)*100);names(con50) <- "con50"
      con80 <- data.frame((con80/totdis)*100);names(con80) <- "con80"
      contop <- data.frame((contop/totdis)*100);names(contop) <- "contop"
      
      batTrainFeats <- cbind(batTrainFeats,con50,con80,contop) # Attaching the Conductance values features
      
      
      
      # Extracting features for Voltage
      
      voltRec <- bat_rec %>% filter(measure == "Voltage") %>% filter(Date >= bench_date2) %>% select(Variable) # taking only the Voltage data
      
      if(nrow(voltRec) > 0){voltRec <- data.frame(sd(voltRec$Variable,na.rm = TRUE))}else{voltRec <- data.frame(0)}
      
      names(voltRec) <- "voltSD" # Renaming the Voltage variable
      
      batTrainFeats <- cbind(batTrainFeats,voltRec) # Attaching the Voltage value features
      
    } # End of the IF condition for extracting features after the benchmark dates
    
    # Extracting features for the bench mark date
    
    if(i < nrow(temp_dates) & i > 2){
      
      disData <- dis_rec %>% filter(Date <= bench_date & Date >= bench_date2) # Taking relevant Discharge data between the two date ranges
      
      # Need to check if there is any data after filtering the discharge records. If there is no data the mean value of the discharge(disMean) is imputed
      
      if(nrow(disData) > 0){disData <- disData %>% summarise(featDischarge = mean(Variable)) }else{disData <- data.frame(disMean);names(disData) <- "featDischarge"} # Taking the mean value between the ranges of the discharge profile only if there are data in the dataframe
      
      batTrainFeats <- cbind(batTrainFeats,disData) # Attaching the Postdata
      
      # Taking the features for slope of Conductance
      
      con_df <- conSlope(bench_date,bench_date2,bat_rec) # Running the new function for calculating the slope of Conductance
      names(con_df) <- "conSlope" # Renaming the conductance slopes
      
      row.names(con_df) <- NULL # Removing the row names
      
      batTrainFeats <- cbind(batTrainFeats,con_df) # Attaching the Conductance features
      
      # Taking features for Depth of discharge
      
      dod_df <- dodMin(bench_date,bench_date2,dod_rec) # Running the function to find the lowest point of DOD within the period of year
      
      batTrainFeats <- cbind(batTrainFeats,dod_df) # Attaching the DOD features
      
      dodRec <- dod_rec %>% filter(Date >= bench_date2 & Date <= bench_date) %>% select(Variable) 
      
      # If the dodRec generates an empty dataframe, the values get to infinity which is not desired. So we should impute it with
      # mean values
      
      if(nrow(dodRec) > 0){
        
        totdis <- nrow(dodRec) # No of records with discharge profiles
        # 
        dod50 <- nrow(dodRec %>% filter(Variable <= 0.5))
        dod85 <- nrow(dodRec %>% filter(Variable <= 0.85 & Variable > 0.5))
        dodtop <- totdis - (dod50 + dod85)
        
        Dod50 <- data.frame((dod50/totdis)*100);names(Dod50) <- "Dod50"
        Dod85 <- data.frame((dod85/totdis)*100);names(Dod85) <- "Dod85"
        Dodtop <- data.frame((dodtop/totdis)*100);names(Dodtop) <- "Dodtop"
        
        batTrainFeats <- cbind(batTrainFeats,Dod50,Dod85,Dodtop) # Attaching the DOD features 
        
        
      }else{
        
        dodRec <- dod_rec
        
        totdis <- nrow(dodRec) # No of records with discharge profiles
        # 
        dod50 <- nrow(dodRec %>% filter(Variable <= 0.5))
        dod85 <- nrow(dodRec %>% filter(Variable <= 0.85 & Variable > 0.5))
        dodtop <- totdis - (dod50 + dod85)
        
        Dod50 <- data.frame((dod50/totdis)*100);names(Dod50) <- "Dod50"
        Dod85 <- data.frame((dod85/totdis)*100);names(Dod85) <- "Dod85"
        Dodtop <- data.frame((dodtop/totdis)*100);names(Dodtop) <- "Dodtop"
        
        batTrainFeats <- cbind(batTrainFeats,Dod50,Dod85,Dodtop) # Attaching the DOD features 
        
        
      }
      
      
      # Taking features for Conductance
      
      con_rec <- conRec %>% filter(Date >= bench_date2 & Date <= bench_date) %>% select(Variable) # taking only the discharge data
      
      if(nrow(con_rec) > 0){
        
        Condrop <- data.frame(min(con_rec$Variable,na.rm = TRUE)/max(con_rec$Variable,na.rm = TRUE)) # Calculating the percentage drop in conductance
        
        names(Condrop) <- "conDrop"
        
        batTrainFeats <- cbind(batTrainFeats,Condrop) # Attaching the DOD features
        
        
      }else{
        
        # In case the number of records are nill, then all the values of conductance has to be taken
        
        con_rec <- conRec %>% select(Variable) # taking all discharge data
        
        # The formulae for Condrop is changed slightly below to include mean instead of the minimum value
        
        Condrop <- data.frame(mean(con_rec$Variable,na.rm = TRUE)/max(con_rec$Variable,na.rm = TRUE)) # Calculating the percentage drop in conductance
        
        names(Condrop) <- "conDrop"
        
        batTrainFeats <- cbind(batTrainFeats,Condrop) # Attaching the DOD features
        
      }
      
      # Getting other variables
      
      totdis <- nrow(con_rec) # No of records with Conductance profiles
      
      
      con50 <- nrow(con_rec %>% filter(Variable < (0.5 * conmax))) # conmax defined in the beginning part of the function
      con80 <- nrow(con_rec %>% filter(Variable >= (0.5 * conmax) & Variable < (0.8 * conmax)))
      contop <- nrow(con_rec %>% filter(Variable >= (0.8 * conmax)))
      
      
      con50 <- data.frame((con50/totdis)*100);names(con50) <- "con50"
      con80 <- data.frame((con80/totdis)*100);names(con80) <- "con80"
      contop <- data.frame((contop/totdis)*100);names(contop) <- "contop"
      
      batTrainFeats <- cbind(batTrainFeats,con50,con80,contop) # Attaching the Conductance values features
      
      # Extracting features for Voltage
      
      voltRec <- bat_rec %>% filter(measure == "Voltage") %>% filter(Date >= bench_date2 & Date <= bench_date) %>% select(Variable) # taking only the Voltage data
      if(nrow(voltRec) > 0){voltRec <- data.frame(sd(voltRec$Variable,na.rm = TRUE))}else{voltRec <- data.frame(0)}
      
      names(voltRec) <- "voltSD" # Renaming the Voltage variable
      
      batTrainFeats <- cbind(batTrainFeats,voltRec) # Attaching the Voltage value features
      
      
    } # End of IF condition between last date and 2nd date
    
    # Doing a check for Predata. Pre data is for those data which falls before the first benchmark dates.
    
    if(i == 2){ # A check for first record
      
      disData <- dis_rec %>% filter(Date <= bench_date)
      
      # Need to check if there is any data after filtering the discharge records. If there is no data the mean value of the discharge(disMean) is imputed
      
      if(nrow(disData) > 0){disData <- disData %>% summarise(featDischarge = mean(Variable)) }else{disData <- data.frame(disMean);names(disData) <- "featDischarge"}
      batTrainFeats <- cbind(batTrainFeats,disData) # Consolidating in a data frame
      
      # Taking the features for slope of Conductance
      
      con_df <- conSlope(bench_date,bench_date2,bat_rec) # Running the new function for calculating the slope of Conductance
      names(con_df) <- "conSlope" # Renaming the conductance slopes
      
      row.names(con_df) <- NULL # Removing the row names
      
      batTrainFeats <- cbind(batTrainFeats,con_df) # Attaching the Conductance features
      
      # Taking features for Depth of discharge
      
      dod_df <- dodMin(bench_date,bench_date2,dod_rec) # Running the function to find the lowest point of DOD within the period of year
      
      batTrainFeats <- cbind(batTrainFeats,dod_df) # Attaching the DOD features
      
      dodRec <- dod_rec %>% filter(Date <= bench_date) %>% select(Variable) 
      
      # If the dodRec generates an empty dataframe, the values get to infinity which is not desired. So we should impute it with
      # mean values
      
      if(nrow(dodRec) > 0){
        
        totdis <- nrow(dodRec) # No of records with discharge profiles
        # 
        dod50 <- nrow(dodRec %>% filter(Variable <= 0.5))
        dod85 <- nrow(dodRec %>% filter(Variable <= 0.85 & Variable > 0.5))
        dodtop <- totdis - (dod50 + dod85)
        
        Dod50 <- data.frame((dod50/totdis)*100);names(Dod50) <- "Dod50"
        Dod85 <- data.frame((dod85/totdis)*100);names(Dod85) <- "Dod85"
        Dodtop <- data.frame((dodtop/totdis)*100);names(Dodtop) <- "Dodtop"
        
        batTrainFeats <- cbind(batTrainFeats,Dod50,Dod85,Dodtop) # Attaching the DOD features 
        
        
      }else{
        
        dodRec <- dod_rec
        
        totdis <- nrow(dodRec) # No of records with discharge profiles
        # 
        dod50 <- nrow(dodRec %>% filter(Variable <= 0.5))
        dod85 <- nrow(dodRec %>% filter(Variable <= 0.85 & Variable > 0.5))
        dodtop <- totdis - (dod50 + dod85)
        
        Dod50 <- data.frame((dod50/totdis)*100);names(Dod50) <- "Dod50"
        Dod85 <- data.frame((dod85/totdis)*100);names(Dod85) <- "Dod85"
        Dodtop <- data.frame((dodtop/totdis)*100);names(Dodtop) <- "Dodtop"
        
        batTrainFeats <- cbind(batTrainFeats,Dod50,Dod85,Dodtop) # Attaching the DOD features 
        
        
      }
      
      
      # Taking features for Conductance
      
      con_rec <- conRec %>% filter(Date <= bench_date) %>% select(Variable) # taking only the discharge data
      
      if(nrow(con_rec) > 0){
        
        Condrop <- data.frame(min(con_rec$Variable,na.rm = TRUE)/max(con_rec$Variable,na.rm = TRUE)) # Calculating the percentage drop in conductance
        
        names(Condrop) <- "conDrop"
        
        batTrainFeats <- cbind(batTrainFeats,Condrop) # Attaching the DOD features
        
        
      }else{
        
        # In case the number of records are nill, then all the values of conductance has to be taken
        
        con_rec <- conRec %>% select(Variable) # taking all discharge data
        
        # The formulae for Condrop is changed slightly below to include mean instead of the minimum value
        
        Condrop <- data.frame(mean(con_rec$Variable,na.rm = TRUE)/max(con_rec$Variable,na.rm = TRUE)) # Calculating the percentage drop in conductance
        
        names(Condrop) <- "conDrop"
        
        batTrainFeats <- cbind(batTrainFeats,Condrop) # Attaching the DOD features
        
      }
      
      # Getting other variables
      
      totdis <- nrow(con_rec) # No of records with Conductance profiles
      
      
      con50 <- nrow(con_rec %>% filter(Variable < (0.5 * conmax)))# conmax defined in the beginning part of the function
      con80 <- nrow(con_rec %>% filter(Variable >= (0.5 * conmax) & Variable < (0.8 * conmax)))
      contop <- nrow(con_rec %>% filter(Variable >= (0.8 * conmax)))
      
      
      con50 <- data.frame((con50/totdis)*100);names(con50) <- "con50"
      con80 <- data.frame((con80/totdis)*100);names(con80) <- "con80"
      contop <- data.frame((contop/totdis)*100);names(contop) <- "contop"
      
      
      batTrainFeats <- cbind(batTrainFeats,con50,con80,contop) # Attaching the Conductance values features
      
      # Extracting features for Voltage
      
      voltRec <- bat_rec %>% filter(measure == "Voltage") %>% filter(Date <= bench_date) %>% select(Variable) # taking only the Voltage data
      if(nrow(voltRec) > 0){voltRec <- data.frame(sd(voltRec$Variable,na.rm = TRUE))}else{voltRec <- data.frame(0)}
      
      names(voltRec) <- "voltSD" # Renaming the Voltage variable
      
      batTrainFeats <- cbind(batTrainFeats,voltRec) # Attaching the Voltage value features
      
    } # End of the IF condition for extracting features for the first set
    
    batTrainFeats$Battery <- temp_bat # Attaching the battery name
    batTrainFeats$Counter <- i # Attaching a counter for each record
    batTrainFeats$Bench1 <- bench_date # Attaching the first benchmark
    batTrainFeats$Bench2 <- bench_date2 # Attaching the first benchmark
   
    batConFeats <- rbind(batConFeats,batTrainFeats) # Making a consolidated data frame
    
  } # End of the for loop for looping over the temp_dates dataframe
  
  batConFeats # Returning the consolidated data frame
  
} # End of the batFeat1 Function




#########################################
# JMJPFU
# 3-Jan-2017 : A new Function for calculating the conductance slopes

############## Function for calculating the Conductance features #################

conSlope <- function(start_date,last_date,bat_rec){
  # bat_rec : This is the data frame which has all the conductance features for the battery
  # start_date : This is the first benchmark date
  # last_date : This is the second benchmark date
  
  con_rec <- bat_rec %>% filter(measure == "Conductance") # Take the relevant conductance data
  
  month_consol <- data.frame(matrix(nrow=1,ncol=0)) # Create an empty data frame
  
  month_i_rec <- con_rec %>% filter(Date >= last_date & Date <= start_date) %>% select(Variable,Measurement_Timestamp) # Take the relevant conductance data
  
  if(nrow(month_i_rec) > 1){
    # Taking only data which is 3 standard deviation from mean for modelling
    up <- mean(month_i_rec$Variable) + (3*(sd(month_i_rec$Variable))) # Uppper range
    low <- mean(month_i_rec$Variable) - (3*(sd(month_i_rec$Variable))) # Lower range
    
    if(up != low){
      month_i_rec <- month_i_rec %>% filter(Variable > low & Variable < up) # Filtering the new data
      mod1 <- lm(Variable~as.numeric(Measurement_Timestamp),data=month_i_rec) # Modelling
      slp <-  data.frame(mod1$coefficients[2]) # Slope of the reading
      
      month_consol <- cbind(month_consol,slp)
      cou = 0
    }else{
      slp <- data.frame(NA)
      month_consol <- cbind(month_consol,slp)
      cou = 0
    }
    
  }else{ # If the number of records are less than 2
    
    slp <- data.frame(NA)
    month_consol <- cbind(month_consol,slp)
    cou = 0
  } # End of the else loop when the records are less than 2
  
  
  
  month_consol # Returning the consolidated data
  
  
} # End of the function
  
  ############################################
  

######################### New Function to get the DOD data ##########################

dodMin <- function(start_date,last_date,dod_rec){
  
  month_consol <- data.frame(matrix(nrow=1,ncol=0)) # Creating an empty dataframe
  
  month_i_rec <- dod_rec %>% filter(Date >= last_date & Date <= start_date) %>% select(Variable)
  
  dodMean <- mean(dod_rec$Variable,na.rm = TRUE)
  
  if(nrow(month_i_rec) > 0){month_i_rec <- month_i_rec %>% summarise(dodMin = min(Variable,na.rm=TRUE))}else{month_i_rec <- data.frame(dodMean);names(month_i_rec) <- "dodMin"}
 
  
  month_consol <- cbind(month_consol,month_i_rec)
  
  month_consol # Return the consolidated records
  
} # End of the function
  
################### New function to get the list of dates for the batFeat1 function #############

dateLister <- function(upperDate,lowerDate){
  
  totMonths <- as.numeric(upperDate - lowerDate) # Gets the difference in Days
  
  totRows <- floor(totMonths/90) - 1 # Subtracting 3 to factor for UpperDate, LowerDate and a factor of safety
  
  tempDates <- data.frame(matrix(nrow=(totRows+2),ncol=1)) # Creating an empty data frame
  
  names(tempDates) <- "Date"
  
  for(i in 2:(totRows+1)){
    
    tempDates[i,1] <- paste(upperDate - ((i-1)*30*3))
    
    
  }
  
  tempDates[1,1] <- paste(upperDate)
  tempDates[nrow(tempDates),1] <- paste(lowerDate)
  
  tempDates <- tempDates %>% arrange(Date)
  
  tempDates$Date <- as.Date(tempDates$Date)
  
  tempDates
  
} # End of the function
  