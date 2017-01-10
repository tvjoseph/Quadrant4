# JMJPFU
# 3-Nov-2016

bin1_list <- Bat_Feat1_new %>% filter(Condrop < .2) %>% select(Battery) # 2

bin1_list <-  Bat_Feat1_new %>% filter(Condrop < .5 & Condrop >= .2) %>% select(Battery) # 7

bin1_list <-  Bat_Feat1_new %>% filter(Condrop < .65 & Condrop >= .5) %>% select(Battery) # 4

bin1_list <-  Bat_Feat1_new %>% filter(Condrop < .80 & Condrop >= .65) %>% select(Battery) # 100

bin1_list <-  Bat_Feat1_new %>% filter(Condrop < .90 & Condrop >= .80) %>% select(Battery) # 492

bin1_list <-  Bat_Feat1_new %>% filter(Condrop >= .90) %>% select(Battery) # 394

################################################

bin_data <- Bat_Feat1_new %>% filter(Battery %in% bin1_list$Battery)


bin_data$compo <- ((bin_data$Dod50 * 5) + (bin_data$Dod80 * 3))/bin_data$Condrop

bin_data <- bin_data %>% arrange(desc(compo))

# JMJPFU
# 2-Jan-2017

# In this script file, we will try to create a training set from the specific date.

# Step 1 : Let us take the battery 2 from the failed battery list

batFailed # List of failed batteries

# Taking the battery 2 data only.

bat2Exp <- batFailed$Battery[3] # This is the failed battery

bat2Data <- bat_newfeat5 %>% filter(Battery == bat2Exp) # Taking the relevant data

# let us find the date ranges to identify the failure date

failDate <- range(unique(bat2Data$Date1))[2]

# Let us also take the battery Features for the relevant battery

bat2Features <- batteryFeatures %>% filter(Battery == bat2Exp)

# Now that we have seen the relevant data, we will approach this as follows
# 1. Take the failed Date
# 2. Take three dates before failed date including the failed date.
# 3. Extract features as per the model of batteryFeatures for only those three dates. The features to be extracted are the following
  # a. Discharge slopes for the previous three periods
  # b. Conductance slopes for the previous three periods
  # c. DOD features
  # d. Condoctance drop features
  # e. DodTop, Dod85, and DOD50 features
  # f. Contop,Con80 & Con50 features
  # g. Faildrop features

# Observations from data

# The dates for various readings vary. Conductance values are on a daily basis. Temperature and Voltage have the most number of values
# which are around 864. Discharge voltage have the lowest number of readings and the same is usually on a monthly basis.
# The idea is to take the dates of discharge voltage as a feature and all other readings should be derivations from those dates
# If last date > last discharge voltage date : Then take a feature as "days after last benchmark date"
# If first date < first discharge voltage date : Then take a feature as days before first benchmark date"
# All other observations should be between the benchmark dates.

# The above applies for Conductance drops

# Tomorrow
# Make a similar logic for Conductance features also 
# JMJPFU
# 3-Jan-2017

# Testing the batFeat1 function

bat2Exp <- batFailed$Battery[9]

featTest2  <- batFeat1(bat2Exp,bat_newfeat5)

# By Gods Grace, the feature extraction was good.
# Tomorrow
# Rework on the DOD50 etc and Con50 etc concepts. Need to check on the maximum values and the % allocation

# JMJPFU
# 4-Jan-2017

# The new approach for conductance is as follows

# 1. Consolidate all the values in the first 10% of conductance value. 
# 2. Find the mean value of this 10% and then do the comparison based on this value
# 3. This will be implemented on the new function 

# Trying some visualisation

ggplot(data=Temp_bat_consol,aes(as.factor(Date1),Variable,color=measure)) + geom_point() + facet_grid(measure~Battery,scales = "free")
