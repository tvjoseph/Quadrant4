# JMJPFU
# 19-Sept-2016

# In this script let us make a large function to consolidate the unique information of Students and School according to month wise consolidation


# Task 1 : Get all the unique Student IDs together over which we will run the For loop

# Student list we will get from the Student_School_Association file.

library(dplyr)


for(i in 1:nrow(Student_school_Association)){
  
  tempid <- paste(Student_school_Association[i,1]) # Getting the Id of the student
  
  tempred <- Student_school_Association[i,] # Get all the records for that student
  
  # Data 1 ##  First getting the Discipline data for this student
  
  temp_discipline <- Stud_Disc_InAsc_Master %>% filter(StudentUniqueId==tempid)
  
  if(nrow(temp_discipline) > 0){
    
    tab2 <- Stud_Disc_Act_Master %>% filter(StudentUniqueId==tempid) # Taking the info from the Discipline action data
    tab3 <- merge(temp_discipline,Discipline_Indicator_Master,by="IncidentIdentifier",all.x=TRUE) # getting all information
    tab3$IncidentDate <- as.Date(as.character(tab3$IncidentDate)) # Converting the date to a charachter object and then making a new date format
    
    tab3$Month <- format(tab3$IncidentDate,"%B") # Making a new variable for Month
    
    tab3$Day <- format(tab3$IncidentDate,"%a") # Making a new variable for day
    
    tab3 <- tab3 %>% select(SchoolName,BehaviorDetailedDescription,IncidentDate)
    
    colnames(tab3)[3] <- "Date"
    
    temp_discipline <- merge(tab3,Discipline_template,by="BehaviorDetailedDescription",all.x=TRUE) 
    
    colnames(temp_discipline)[4] <- "Discore"
    
    
  } # If condition for checking whether there are any discipline data for the considered student
  
  
  # Data 2 ## Getting the Attendence data like the discipline data
  
  temp_attendence <- attend_trans %>% filter(StudentUniqueStateId==tempid) # Not a single incidence of absenteeism
  
  # Take only relevant columns in temp_attendence
  
  temp_attendence <- temp_attendence %>% select(StudentUniqueStateId,AttendanceEventReason,StateOrganizationId,LocalCourseCode,ClassPeriodName,Location,Date,Month,Day,Week,reasons,counts,HispanicLatinoEthnicity,Race,EconomicDisadvantaged,SchoolFoodServicesEligibility,normrace,normpop)
  
  # Data 3 ## Getting the grade information of the particular student
  
  temp_grade <- stud_grade %>% filter(ns1.StudentUniqueStateId==tempid)
  
  # Data 4 ## Getting the temp timeline
  
  temp_timeline <- samp_dates
  
  colnames(temp_timeline)[1] <- "Date" # Renaming the temp_timeline date column
  
  # Data Merge 1 # Attendence data Merge
  
  if(nrow(temp_attendence) > 0){
    
    temp_timeline <- merge(temp_timeline,temp_attendence,by="Date",all=TRUE) # merged all records of attendence
    
  }else {
    
    temp_timeline[,3:19] <- NA
    colnames(temp_timeline)[3:19] <- c("StudentUniqueStateId","AttendanceEventReason","StateOrganizationId","LocalCourseCode","ClassPeriodName","Location","Month","Day","Week","reasons","counts","HispanicLatinoEthnicity","Race","EconomicDisadvantaged","SchoolFoodServicesEligibility","normrace","normpop")
    
    
  } # End of If loop for temp_attendence merge
  
  # JMJPFU
  # 20-Sept-2016
  
  # Data Merge 2 - Discipline data
  
  # Cleaning up the discipline data
  
  if(nrow(temp_discipline) > 0) {
    
    # Merging the data sets
    
    temp_timeline <- merge(temp_timeline,temp_discipline,by="Date",all=TRUE)
    
    
  }else{
    
    temp_timeline[,19:22] <- NA 
    colnames(temp_timeline)[20:22] <- c("BehaviorDetailedDescription","SchoolName","Discore" )
    
    
  } # End of the If loop for discipline merge
  
  
  # Data Merge 3 - Getting the grade data
  
  # Getting the basic dates
  
  grad_dates <- data.frame(Dates=c("2015-10-13","2015-11-10","2015-12-16","2016-02-04","2016-03-07","2016-04-07","2016-06-10"))
  
  grad_dates[,2:28] <- NA # Defining the rest 27 variables pertaining to grades
  
  colnames(grad_dates)[1] <- "Date"
  
  colnames(grad_dates)[2:28] <- c("01","02","03","04","05","06","07","08","09","10","11","12","13","23","26","28","29","RA", "RB","EA","EB","MA","MB","AS","BS","SA","SB") 
  
  # Calling the function to fix the grade details
  
  if(nrow(temp_grade) > 0){
    
    samp_grade <- get_grade(temp_grade,grad_dates) # Getting all the grade information in samp_grade 
    
  }else{samp_grade <- grad_dates }
  
 
  
  # Merging the grade info with the temp_timline
  
  temp_timeline <- merge(temp_timeline,samp_grade,by="Date",all=TRUE) 
  
  # Data Operation 3 - Cleaning up all the merged data and creating a final data frame
  
  Final_timeline <- data.frame(matrix(nrow=1,ncol=49))
  colnames(Final_timeline) <- names(temp_timeline)
  
  # Now to eliminate all those rows where there is no data.
  
  cou = 0
  
  for(i in 1:nrow(temp_timeline)){
    
    for(j in 3:49){
      
      if(!is.na(temp_timeline[i,j])){ 
        cou = cou+1
        for(k in 1:49){
          Final_timeline[cou,k] <- paste(temp_timeline[i,k])
        }
        break
      } # End of the if loop
      
    } # End of the columns for loop
    
  } # End of the rows For loop
  
  # Changing the Date in Final timeline to a Date format
  
  Final_timeline$Date <- as.Date(Final_timeline$Date)
  
  # Converting all characters to numeric
  
  for(i in c(18,19,22:49)){
    Final_timeline[,i] <- as.numeric(Final_timeline[,i]) 
  }
  
  # Creating a consolidated grade
  for(i in 1:nrow(Final_timeline)){
    Final_timeline$Congrade[i] <- sum(Final_timeline[i,23:49],na.rm=TRUE)
  }
  
  Final_timeline$Month <- format(Final_timeline$Date,"%B")
  
  Final_timeline$Month <- factor(Final_timeline$Month,month.name)
  
  # Creating the final data set for all the students
  
  All_Stud_timeline <- rbind(All_Stud_timeline,Final_timeline)
  
  
  
  
} # End of the file where we iterate over the list of students

# Tomorrow, get the other variables also as part of this table and construct the total data frame to work with


  
  All_Stud_timeline <- data.frame(matrix(nrow=1,ncol=50))
colnames(All_Stud_timeline) <- names(Final_timeline)
