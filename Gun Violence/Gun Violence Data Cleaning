## MMA 860: Team Bremner: March-April 2021 Team Project: "The impact of socioeconomics on violence."

#### 00 Project Background ####
#   Why do people resort to violence?  
#   Violence is a powerful and unpleasant tool. Typically, it’s caused by emotions such as anger or fear. Sometimes, violence can be practical or transactional, such as a gang dispute over territory or a hit on an important political figure. While it is normal for individuals to experience negative emotions, some respond with violence.  Why?
#   
# In this project, we will explore a few socioeconomic data sets that our team has identified as points of interest.
#  1.	Median Household Income
#  2.	Poverty Level
#  3.	High-school graduation rate
#  4.	Police Activity / Shootings
#  5.	Ethnic background
# 
# In our socioeconomic modelling, we will use US datasets to explore how socioeconomics relates to violence. Using multiple regression using the points listed above to help predict gun violence in various American cities. The project will utilize techniques listed below:
#   



#### 00.1 Dependencies, working Directory & loading files #### 
Packages <- c('tidyverse', 'mice', 'readxl', 'MASS','caret','ggplot2', 'tidyr', 'lubridate','lattice', 'car','estimatr','stringr')
lapply(Packages, library, character.only = TRUE)

# Set Working directory 
getwd()
setwd("~/Desktop/R/860/Team Project/")
getwd()

#  Load project files




#### 01. Data Exploration #### 
# Data exploration code and comments to be pasted in this section



#### 02. Data join & merge #### 
#reading in our datafile 
library(readxl)
library(tidyverse)
library(dplyr)
# Note about datafile: The original gun-violence-data_01-2013_03-2018.csv file was saved as "gun violence base data_original.xlsx" Excel and 
# all socioecinomics data were added to this file in different sheets.
#  The "gun violence base data_original.xlsx" is used as the master data file.
gun_data <- read_excel("C:\\Users\\derek\\OneDrive\\Desktop\\gun violence\\gun violence base data.xlsx", sheet=1)
demographic_data <- read_excel("C:\\Users\\derek\\OneDrive\\Desktop\\gun violence\\gun violence base data.xlsx", sheet=2)

head(demographic_data)
str(demographic_data)
#creating our join
#join_data_2 did not work as expected, created over 25 million records 
#join_data_2 <- left_join(gun_data,demographic_data,by=c("latitude", "longitude"))

#join_data_3 did work, however, testing other joins to see if they're more effective
#join_data_3 <- left_join(gun_data,demographic_data,by=c("city","state"))

#join_data_4 did not work as expected, created over 12 million records 
#join_data_4 <- left_join(gun_data,demographic_data,by="latitude")

#join_data_5 did not work as expected, created over 12 million records 
#join_data_5 <- left_join(gun_data,demographic_data,by="latitude")

#join_data_6 did not work as expected, double join created more records than computer could process 
#join_data_6 <- left_join(gun_data,demographic_data,by=c("latitude", "longitude"))

#recreated join_data_3 as join_data_7 as this was our most effective method of joining the data
join_data_7 <- left_join(gun_data,demographic_data,by=c("city","state"))

#testing for any missing records with the join we decided to pursue
md.pattern(join_data_3)

install.packages("xlsx")
library(xlsx)
#exporting joined file to excel
write.xlsx(join_data_7, file="mma860.xlsx",sheetName="Base Data", append = FALSE)

library(rio)
export(join_data_7,"MMA 860 Project.xlsx")

memory.limit(size=80000)


#### 03. Feature Engineering #### 
# 1. Load library and files 
# Packages <- c('tidyverse', 'mice', 'readxl', 'MASS','caret','ggplot2', 'tidyr', 'lubridate','lattice', 'car','estimatr','stringr')
# lapply(Packages, library, character.only = TRUE)
# rm(Packages)
#file1 <- read_excel('gun violence base data - Derek.xlsx', sheet = 1)
file1 <- join_data_7
head(file1)
summary(file1)
str(file1)

# 2. Remove useless and redounded columns 
df <- subset(file1, select = -c(address,notes,participant_age,participant_name,city_and_state,state_id,latitude.y,longitude.y))
str(df)

#3. Change type to numeric 
df$high_school_completion_rate <- as.numeric(df$high_school_completion_rate)
df$poverty_rate <- as.numeric(df$poverty_rate)
df$median_income <- as.numeric(df$median_income)
str(df)

# 4. Crete column called  # of Male of Participant,# of Female of Participant 
df2 <- df
str(df2)

df2$"Number of Male(Participant)" <- str_count(df2$participant_gender , "Male") 
df2$"Number of Female(Participant)" <- str_count(df2$participant_gender , "Female") 
#change NA to 0
df2$`Number of Male(Participant)`[is.na(df2$`Number of Male(Participant)`)] <- 0
df2$`Number of Female(Participant)`[is.na(df2$`Number of Female(Participant)`)] <- 0
sum(is.na(df2$`Number of Female(Participant)`))
sum(is.na(df2$`Number of Male(Participant)`))

##4b.Crete column called  # of Adult of Participant,# of Teen of Child,
df2$"Number of Adult" <- str_count(df2$participant_age_group , "Adult") 
df2$"Number of Teen" <- str_count(df2$participant_age_group , "Teen") 
df2$"Total # Child" <- str_count(df2$participant_age_group , "Child")
sum(is.na(df2$"Number of Adult"))
sum(is.na(df2$"Number of Teen"))
sum(is.na(df2$"Total # Child"))
df2$`Number of Adult`[is.na(df2$`Number of Adult`)] <- 0
df2$`Number of Teen`[is.na(df2$`Number of Teen`)] <- 0
df2$`Total # Child`[is.na(df2$`Total # Child`)] <- 0

#5. Calculate total # Participant 
#Since the gender and age group total # not match, so we willuse the max number as total participant number
df2$"Total # Participant" <- ifelse((df2$"Number of Female(Participant)" + df2$"Number of Male(Participant)")>(df2$"Number of Adult" + df2$"Number of Teen" + df2$"Total # Child"),
                                    (df2$"Number of Female(Participant)" + df2$"Number of Male(Participant)"),
                                    (df2$"Number of Adult" + df2$"Number of Teen" + df2$"Total # Child") )

##5b. Participant Status
df2$"Participant Arrested" <- str_count(df2$participant_status, "Arrested") 
df2$"Participant Unharmed" <- str_count(df2$participant_status, "Unharmed") 
df2$"Participant Injured" <- str_count(df2$participant_status, "Injured") 
df2$"Participant Killed" <- str_count(df2$participant_status, "Killed") 
sum(is.na(df2$"Participant Arrested"))
sum(is.na(df2$"Participant Unharmed"))
sum(is.na(df2$"Participant Injured"))
sum(is.na(df2$"Participant Killed"))

df2$`Participant Arrested`[is.na(df2$`Participant Arrested`)] <- 0
df2$`Participant Injured`[is.na(df2$`Participant Injured`)] <- 0
df2$`Participant Unharmed`[is.na(df2$`Participant Unharmed`)] <- 0
df2$`Participant Killed`[is.na(df2$`Participant Killed`)] <- 0


##5c. Participant Type
df2$"Number of Victims" <- str_count(df2$participant_type, "Victim") 
df2$"Number of Suspects" <- str_count(df2$participant_type, "Suspect") 
sum(is.na(df2$"Number of Victims"))
sum(is.na(df2$"Number of Suspects"))
df2$`Number of Victims`[is.na(df2$`Number of Victims`)] <- 0
df2$`Number of Suspects`[is.na(df2$`Number of Suspects`)] <- 0
str(df2)

#6. If include massive shoot? 
df2$"Massive Shooting (Y/N)" <-  ifelse(  str_count(df2$incident_characteristics , "Mass Shooting")>0, 1,0)
sum(is.na(df2$"Massive Shooting (Y/N)"))
df2$`Massive Shooting (Y/N)`[is.na(df2$`Massive Shooting (Y/N)`)] <- 0
str(df2)

#7. some gun types - include all 
df2$"NumberofUnknown" <- str_count(df2$gun_type , "Unknown") 
df2$"Numberof9mm" <- str_count(df2$gun_type , "9mm") 
df2$"NumberofShotgun" <- str_count(df2$gun_type , "Shotgun") 
df2$"Numberof40SW" <- str_count(df2$gun_type , "40 SW")  
df2$"Numberof45Auto" <- str_count(df2$gun_type , "45 Auto") 
df2$"Numberof44Mag" <- str_count(df2$gun_type , "44 Mag") 
df2$"NumberofRifle" <- str_count(df2$gun_type , "Rifle") 
df2$"Numberof357Mag" <- str_count(df2$gun_type , "357 Mag") 
df2$"Numberof38Spl" <- str_count(df2$gun_type , "38 Spl") 
df2$"NumberofOther" <- str_count(df2$gun_type , "Other") 
df2$"NumberofHandgun" <- str_count(df2$gun_type , "Handgun") 
df2$"Numberof2LR" <- str_count(df2$gun_type , "2 LR") 
df2$"NumberofRifle" <- str_count(df2$gun_type , "Rifle") 
df2$"Numberof80Auto" <- str_count(df2$gun_type , "80 Auto") 
df2$"Numberof9mm" <- str_count(df2$gun_type , "9mm") 
df2$"Numberof7.62AK47" <- str_count(df2$gun_type , "AK-47") 
df2$"Numberof6gauge" <- str_count(df2$gun_type , "6 gauge") 
df2$"Numberof22LR" <- str_count(df2$gun_type , "22 LR") 
df2$"Numberof223RemAR15" <- str_count(df2$gun_type , "AR-15") 
df2$"Numberof20gauge" <- str_count(df2$gun_type , "20 gauge") 
df2$"Numberof32Auto" <- str_count(df2$gun_type , "32 Auto") 
df2$"Numberof25Auto" <- str_count(df2$gun_type , "25 Auto") 
df2$"Numberof12gauge" <- str_count(df2$gun_type , "12 gauge") 
df2$"Numberof410gauge" <- str_count(df2$gun_type , "410 gauge") 
df2$"Numberof380Auto" <- str_count(df2$gun_type , "380 Auto") 
df2$"Numberof3006Spr" <- str_count(df2$gun_type , "30-06 Spr") 
df2$"Numberof3030Win" <- str_count(df2$gun_type , "30-30 Win") 
df2$"Numberof308Win" <- str_count(df2$gun_type , "308 Win") 
df2$"Numberof10mm" <- str_count(df2$gun_type , "10mm") 
df2$"Numberof300Win" <- str_count(df2$gun_type , "300 Win") 
df2$"Numberof28gauge" <- str_count(df2$gun_type , "28 gauge") 
df2$"Numberof16gauge" <- str_count(df2$gun_type , "16 gauge") 
str(df2)

#7.b replace NA to 0
df2$"NumberofUnknown"[is.na(df2$"NumberofUnknown")] <- 0
df2$"Numberof9mm" [is.na(df2$"Numberof9mm")] <- 0
df2$"NumberofShotgun" [is.na(df2$"NumberofShotgun")] <- 0
df2$"Numberof40SW"[is.na(df2$"Numberof40SW")] <- 0
df2$"Numberof45Auto"[is.na(df2$"Numberof45Auto")] <- 0
df2$"Numberof44Mag"[is.na(df2$"Numberof44Mag")] <- 0
df2$"NumberofRifle"[is.na(df2$"NumberofRifle")] <- 0
df2$"Numberof357Mag"[is.na(df2$"Numberof357Mag")] <- 0
df2$"Numberof38Spl"[is.na(df2$"Numberof38Spl")] <- 0
df2$"NumberofOther"[is.na(df2$"NumberofOther")] <- 0
df2$"NumberofHandgun"[is.na(df2$"NumberofHandgun")] <- 0
df2$"Numberof2LR"[is.na(df2$"Numberof2LR")] <- 0
df2$"NumberofRifle"[is.na(df2$"NumberofRifle")] <- 0
df2$"Numberof80Auto"[is.na(df2$"Numberof80Auto")] <- 0
df2$"Numberof9mm"[is.na(df2$"Numberof9mm")] <- 0
df2$"Numberof7.62AK47"[is.na(df2$"Numberof7.62AK47")] <- 0
df2$"Numberof6gauge"[is.na(df2$"Numberof6gauge")] <- 0
df2$"Numberof22LR"[is.na(df2$"Numberof22LR")] <- 0
df2$"Numberof223RemAR15"[is.na(df2$"Numberof223RemAR15")] <- 0
df2$"Numberof20gauge"[is.na(df2$"Numberof20gauge")] <- 0
df2$"Numberof32Auto"[is.na(df2$"Numberof32Auto")] <- 0
df2$"Numberof25Auto"[is.na(df2$"Numberof25Auto")] <- 0
df2$"Numberof12gauge"[is.na(df2$"Numberof12gauge")] <- 0
df2$"Numberof410gauge"[is.na(df2$"Numberof410gauge")] <- 0
df2$"Numberof380Auto"[is.na(df2$"Numberof380Auto")] <- 0
df2$"Numberof3006Spr"[is.na(df2$"Numberof3006Spr")] <- 0
df2$"Numberof3030Win"[is.na(df2$"Numberof3030Win")] <- 0
df2$"Numberof308Win"[is.na(df2$"Numberof308Win")] <- 0
df2$"Numberof10mm"[is.na(df2$"Numberof10mm")] <- 0
df2$"Numberof300Win"[is.na(df2$"Numberof300Win")] <- 0
df2$"Numberof28gauge"[is.na(df2$"Numberof28gauge")] <- 0
df2$"Numberof16gauge"[is.na(df2$"Numberof16gauge")] <- 0
str(df2)

#7.c consolidate them into 4 categories
df2$"Unknown(gun type)" <-  df2$"NumberofUnknown"
df2$"SG(gun type)" <- df2$"Numberof16gauge" + df2$"Numberof12gauge" + df2$"Numberof28gauge" + df2$"Numberof20gauge" + df2$"Numberof410gauge" + df2$"Numberof6gauge" + df2$"NumberofShotgun"
df2$"Other(gun type)" <- df2$"NumberofOther"
df2$"H(gun type)" <- df2$"Numberof10mm" + df2$"Numberof25Auto" + df2$"Numberof32Auto" + df2$"Numberof9mm" + df2$"NumberofHandgun" + df2$"Numberof357Mag" + df2$"Numberof38Spl" + df2$"Numberof40SW" + df2$"Numberof44Mag" + df2$"Numberof45Auto" + df2$"Numberof80Auto" + df2$"Numberof9mm"
df2$"HR(gun type)" <- df2$"Numberof2LR" + df2$"Numberof3006Spr" + df2$"Numberof3030Win" + df2$"Numberof308Win" + df2$"Numberof300Win" 
df2$"Hunting Rifle(gun type)" <- df2$"NumberofRifle"
df2$"AR(gun type)" <- df2$"Numberof22LR" + df2$"Numberof223RemAR15" + df2$"Numberof380Auto" + df2$"Numberof7.62AK47" 
str(df2)

#Remove useless columns
df2 <- subset(df2, select = -c(
  NumberofUnknown,
  Numberof9mm,
  NumberofShotgun, 
  Numberof40SW, 
  Numberof45Auto, 
  Numberof44Mag,
  NumberofRifle,
  Numberof357Mag,
  Numberof38Spl,
  NumberofOther,
  NumberofHandgun, 
  Numberof2LR,
  NumberofRifle,
  Numberof80Auto,
  Numberof9mm,
  Numberof7.62AK47, 
  Numberof6gauge, 
  Numberof22LR,
  Numberof223RemAR15,
  Numberof20gauge, 
  Numberof32Auto,
  Numberof25Auto, 
  Numberof12gauge, 
  Numberof410gauge,
  Numberof380Auto, 
  Numberof3006Spr, 
  Numberof3030Win,
  Numberof308Win, 
  Numberof10mm,
  Numberof300Win,
  Numberof28gauge,
  Numberof16gauge 
))

str(df2)

#rm(df2$"Number of gauge")

#7b. Number of Stolen Guns
df2$"unknown" <- str_count(df2$gun_stolen, "Unknown") 
df2$"stolen" <- str_count(df2$gun_stolen, "Stolen") 
df2$"not stolen" <- str_count(df2$gun_stolen, "Not-stolen") 

df2$"unknown"[is.na(df2$"unknown")] <- 0
df2$"stolen"[is.na(df2$"stolen")] <- 0
df2$"not stolen"[is.na(df2$"not stolen")] <- 0

sum(is.na(df2$"stolen"))
sum(is.na(df2$"not stolen"))
sum(is.na(df2$"unknown"))

sum(df2$unknown)
sum(df2$stolen)
sum(df2$"not stolen")

df2$"n_guns_involved"[is.na(df2$"n_guns_involved")] <- 0
sum(is.na(df2$"n_guns_involved"))

#relationship
df2$"Family" <- str_count(df2$participant_relationship, "Family") 
df2$"Friends" <- str_count(df2$participant_relationship, "Friends") 
df2$"Co-worker" <- str_count(df2$participant_relationship, "Co-worker") 
df2$"Robbery" <- str_count(df2$participant_relationship, "Robbery") 
df2$"Neighbor" <- str_count(df2$participant_relationship, "Neighbor") 
df2$"Gang vs Gang" <- str_count(df2$participant_relationship, "Gang vs Gang") 
df2$"Aquaintance" <- str_count(df2$participant_relationship, "Aquaintance") 
df2$"Significant others" <- str_count(df2$participant_relationship, "Significant others") 
df2$"Home Invasion" <- str_count(df2$participant_relationship, "Home Invasion") 

df2$"Family"[is.na(df2$"Family")] <- 0
df2$"Friends"[is.na(df2$"Friends")] <- 0
df2$"Co-worker"[is.na(df2$"Co-worker")] <- 0
df2$"Robbery"[is.na(df2$"Robbery")] <- 0
df2$"Neighbor"[is.na(df2$"Neighbor")] <- 0
df2$"Gang vs Gang"[is.na(df2$"Gang vs Gang")] <- 0
df2$"Aquaintance"[is.na(df2$"Aquaintance")] <- 0
df2$"Significant others"[is.na(df2$"Significant others")] <- 0
df2$"Home Invasion"[is.na(df2$"Home Invasion")] <- 0

sum(is.na(df2$"Family"))
sum(is.na(df2$"Friends"))
sum(is.na(df2$"Co-worker"))
sum(is.na(df2$"Robbery"))
sum(is.na(df2$"Neighbor"))
sum(is.na(df2$"Gang vs Gang"))
sum(is.na(df2$"Aquaintance"))
sum(is.na(df2$"Significant others"))
sum(is.na(df2$"Home Invasion"))


# 8. Remove useless and redounded columns for df2 
df3 <- subset(df2, select = -c(participant_gender,gun_type,participant_age_group,incident_characteristics,
                               participant_status,participant_type,gun_stolen,participant_relationship))
str(df3)
summary(df3)



#9. Rename Columns 
names(df3)[names(df3) == 'incident_id'] <- 'Incident ID'
names(df3)[names(df3) == 'state'] <- 'State'
names(df3)[names(df3) == 'city'] <- 'City'
names(df3)[names(df3) == 'n_killed'] <- 'Num of Killed'
names(df3)[names(df3) == 'n_injured'] <- 'Num of Injured'
names(df3)[names(df3) == 'congressional_district'] <- 'Congressional District Code'
names(df3)[names(df3) == 'latitude.x'] <- 'Latitude'
names(df3)[names(df3) == 'longitude.x '] <- 'Longitude'
names(df3)[names(df3) == 'n_guns_involved'] <- 'Num of Guns'
names(df3)[names(df3) == 'poverty_rate'] <- 'Poverty Rate'
names(df3)[names(df3) == 'median_income'] <- 'Median Income'
names(df3)[names(df3) == 'Total # Child '] <- 'Number of Children'
names(df3)[names(df3) == 'Total # Participant'] <- 'Total Participants'
str(df3)

#Search for missing data
md.pattern(file1)
md.pattern(df3)

#10. Export to excel 
library("writexl")
write_xlsx(df3,"Task2-Clean Data For Analysis.xlsx")

#11. Summary Statistics
mean(df3$`Num of Killed`, na.rm=TRUE)
mean(df3$`Num of Injured`, na.rm=TRUE)
mean(df3$`Number of Male(Participant)`, na.rm=TRUE)
mean(df3$`Number of Female(Participant)`, na.rm=TRUE)
mean(df3$`Num of Guns`, na.rm=TRUE)
mean(df3$`Total Participants`, na.rm=TRUE)

sd(df3$`Num of Killed`, na.rm=TRUE)
sd(df3$`Num of Injured`, na.rm=TRUE)
sd(df3$`Number of Male(Participant)`, na.rm=TRUE)
sd(df3$`Number of Female(Participant)`, na.rm=TRUE)
sd(df3$`Num of Guns`, na.rm=TRUE)
sd(df3$`Total Participants`, na.rm=TRUE)


#12. Bivariate analysis 
cor(df3$`Total Participants`, df3$`Median Income`, use="complete.obs")
cor(df3$`Total Participants`, df3$`Poverty Rate`, use="complete.obs")
cor(df3$`Total Participants`, df3$`Massive Shooting (Y/N)`, use="complete.obs")

cor(df3[c('Total Participants', 'Massive Shooting (Y/N)', 'Participant Injured','Num of Guns')], use="complete.obs")

#12. visualization
df3 %>%
  ggplot(aes(`Num of Injured`)) +
  geom_histogram(binwidth = 1, color = "black",fill = "grey") +
  geom_vline(xintercept = mean(df3$`Num of Injured`), lwd = 2) +
  labs(title = "Distribution of Num of Injured",
       x = "Num of Injured",
       y = "Median Income") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(7.5,35,2.5))

library(mice)
md.pattern(df3)

#14. tidy the gender
library(tidyr)
tidy_df3 <- gather(df3, 'Gender',' # of Gender','Number of Male(Participant)':'Number of Female(Participant)')
str(tidy_df3)

counts <- table(tidy_df3$Gender, tidy_df3$` # of Gender`)
viz5 <- barplot(counts, main="Number of Participant Distribution by Gender",
                xlab="Number of Participant", col=c("darkblue","red"),
                legend = rownames(counts))
viz5

#15. Data structure 
summary(df3)
str(df3)
head(df3)

#16. Outlier analysis 
boxplot(df3$`Num of Killed`,ylab = "Num of Killed")
boxplot(df3$`Num of Injured`,ylab = "Num of Injured")
boxplot(df3$`Median Income`,ylab = "Income")


#library(udpipe)
#library(textrank)
#library(lattice)
#stats1 <- txt_freq(x = df2$participant_relationship)
#stats$key <- factor(stats$key, levels = rev(stats$key))
#barchart(key ~ freq, data = head(stats, 30), col = "cadetblue", main = "Most Occurring Crimes", xlab = "Freq")

#stats <- txt_freq(x = df2$gun_type)
#stats$key <- factor(stats$key, levels = rev(stats$key))
#barchart(key ~ freq, data = head(stats, 30), col = "cadetblue", main = "Most Occurring Crimes", xlab = "Freq")

########################################################33
#Clean Task2 state and city
Task2$City<-gsub("\\(.+?\\)", "", Task2$City)
#Task2$state_abb<-state.abb[match(Task2$State,state.name)]
Task2$State<-toupper(Task2$State)
state$State<-toupper(state$State)
Task2 <- subset(Task2,select=-c(high_school_completion_rate,`Poverty Rate`,`Median Income`,`%_white`,`%_black`,`%_native_american`
                                ,`%_asian`,`%_hispanic`))
library(tm)
library(tidyverse)
stopwords = c("City","city","Country","country","CDP","town","village","Town","Village","borough","and",
              "municipality","-Clarke County unified government (balance)","-Richmond County consolidated government (balance)")
Task_abb<-left_join(Task2,state,by=c("State"))
Task_abb$City<-sub(Task_abb$City, pattern = "Saint", replacement = "St.")
Task_abb$City<-sub(Task_abb$City, pattern = "Bronx", replacement = "Bronxville")
Task_abb$City<-sub(Task_abb$City, pattern = "Chesterfield", replacement = "Chester")
#Task_abb$City<-sub(Task_abb$City, pattern = "Wilkes Barre", replacement = "Wilkes-Barre")
#Task_abb$City<-sub(Task_abb$City, pattern = "Winston Salem", replacement = "Winston-Salem")

Task_abb$City<-removeWords(Task_abb$City,stopwords)
Task_abb$state_all<- with(Task_abb, paste0(State_abb,sep = " ", City))
Task_abb$state_all<-gsub(" ", "", Task_abb$state_all, fixed = TRUE)
Task_abb$state_all<-toupper(Task_abb$state_all)
Task_abb$state_all <- str_replace_all(Task_abb$state_all, fixed(" "), "")
summary(Task_abb)
length(Task_abb$state_all)

#Geo data Join in 

share<-read.csv(file.choose())
High<-read.csv(file.choose())
Income<-read.csv(file.choose())
Poverty<-read.csv(file.choose())

pop$city<-removeWords(pop$city,stopwords)
pop$city<-str_replace(pop$city, '(.+)/.+', '\\1')
pop$city<-str_replace(pop$city, '(.+)-.+', '\\1')
pop$city<-gsub('-',' ',pop$city)
pop$city<-gsub("\\(.*?\\)", "", pop$city)
pop$city<- str_replace_all(pop$city, fixed(" "), "")

share$City<-removeWords(share$City,stopwords)
share$City<-str_replace(share$City, '(.+)/.+', '\\1')
share$City<-str_replace(share$City, '(.+)-.+', '\\1')
share$City<-gsub('-',' ',share$City)
share$City<-gsub("\\(.*?\\)", "", share$City)
share$City<- str_replace_all(share$City, fixed(" "), "")

High$City<-removeWords(High$City,stopwords)
High$City<-str_replace(High$City, '(.+)/.+', '\\1')
High$City<-str_replace(High$City, '(.+)-.+', '\\1')
High$City<-gsub('-',' ',High$City)
High$City<-gsub("\\(.*?\\)", "", High$City)
High$City<- str_replace_all(High$City, fixed(" "), "")

Income$City<-removeWords(Income$City,stopwords)
Income$City<-str_replace(Income$City, '(.+)/.+', '\\1')
Income$City<-str_replace(Income$City, '(.+)-.+', '\\1')
Income$City<-gsub('-',' ',Income$City)
Income$City<-gsub("\\(.*?\\)", "", Income$City)
Income$City<- str_replace_all(Income$City, fixed(" "), "")

Poverty$City<-removeWords(Poverty$City,stopwords)
Poverty$City<-str_replace(Poverty$City, '(.+)/.+', '\\1')
Poverty$City<-str_replace(Poverty$City, '(.+).+', '\\1')
Poverty$City<-gsub('-',' ',Poverty$City)
Poverty$City<-gsub("\\(.*?\\)", "", Poverty$City)
Poverty$City<- str_replace_all(Poverty$City, fixed(" "), "")

join1<-full_join(High, Income, by=c("Geographic.Area","City"))
join2<-full_join(join1, share, by=c("Geographic.Area"="Geographic.area","City"))
join3<-full_join(join2, Poverty, by=c("Geographic.Area","City"))
join<-full_join(join3, pop, by=c("Geographic.Area"="state_id","City"="city"))
summary(join)

#Clean Geo state and city
#join3$City<-gsub('-',' ',join3$City)
#join3$City<-gsub("\\(.*?\\)", "", join3$City)
#share$City<-sub("\\s[^ ]+$", "", share$City) remove after last blank string
#join3$City<-removeWords(join3$City,stopwords)

join$state_all<- with(join, paste0(join$Geographic.Area,sep = " ", City))
join$state_all<-gsub(" ", "", join$state_all, fixed = TRUE)
join$state_all_geo<-toupper(join$state_all)
summary(join)
length(join$state_all)
#duplication
join<-join[!duplicated(join$state_all_geo), ]

############################join####################################
library(dplyr)
Task3<-left_join(Task_abb,join, by = c("state_all"="state_all_geo"))
Task3$`City.x`<- str_replace_all(Task3$`City.x`, fixed(" "), "")


check<-sqldf('SELECT *
            from Cities where City ="Louisville" 
              ')
check<-sqldf('SELECT *
            from Task3 where `City.x` ="Louisville" 
              ')


Task3$share_white  <- as.numeric(Task3$share_white )
Task3$share_black  <- as.numeric(Task3$share_black )
Task3$share_native_american  <- as.numeric(Task3$share_native_american )
Task3$share_asian<- as.numeric(Task3$share_asian )
Task3$share_hispanic<- as.numeric(Task3$share_hispanic )
Task3$Median.Income<- as.numeric(Task3$Median.Income)
Task3$percent_completed_hs<- as.numeric(Task3$percent_completed_hs)
Task3$poverty_rate<- as.numeric(Task3$poverty_rate)
summary(Task3)


#aggregate this data to weekly level
library(lubridate)
Task3$Week <- week(Task3$date)
Task3$Month <- month(Task3$date)
Task3$Year <- substring(Task3$date,1,4)

library(sqldf)
check<-sqldf('SELECT *
            from Cities where City ="Chicago" 
              ')


Monthly<-sqldf('SELECT distinct Year,Month,State,`City.x` as City,count(distinct `Incident ID`) as Number_Incident,
     sum( `Num of Killed`) as Number_Killed,
     sum( `Num of Injured`) as Number_Injured,Latitude,`longitude.x` as longitude,
     
            sum( `Num of Guns`) as Number_Guns ,
            sum( `Number of Male(Participant)`) as `Number_Male(Participant)`,
         
            
            sum( `Number of Female(Participant)`) as `Number_Female(Participant)`,
            sum( `Number of Adult`) as Number_Adult,
            sum( `Number of Teen`) as Number_Teen,
            sum( `Total # Child`) as Total_Child,
            sum( `Total Participants`) as Total_Participants,
            sum( `Participant Arrested`) as Participant_Arrested,
            sum( `Participant Unharmed`) as Participant_Unharmed,
            sum( `Participant Injured`) as Participant_Injured,
            sum( `Participant Killed`) as Participant_Killed,
            
          
            
            sum( `Number of Victims`) as Number_Victims,
            sum( `Number of Suspects`) as Number_Suspects,
            
            
            
            sum(`Massive Shooting (Y/N)`) as Num_Massive_Shooting,
            
            sum( `Unknown(gun type)`) as Num_Unknown_guntype,
            sum( `SG(gun type)`) as Num_SG_guntype,
            sum( `Other(gun type)`) as Num_Other_guntype,
            
 
            sum( `H(gun type)`) as Num_H_guntype,
            sum( `HR(gun type)`) as Num_HR_guntype,
            sum( `Hunting Rifle(gun type)`) as Num_HuntingRifle_guntype,
            sum( `AR(gun type)`) as Num_AR_guntype,
            
            sum( `unknown`) as Num_unknown,
            sum( `stolen`) as Num_stolen,
            sum( `not stolen`) as Num_not_stolen,
             
             
            sum( `Family`) as Number_Family,
            sum( `Friends`) as Number_Friends,
            sum( `Co-worker`) as Num_Co_worker,
            sum( `Robbery`) as Number_Robbery,
            sum( `Neighbor`) as Number_Neighbor,
            sum( `Gang vs Gang`) as Num_GangGang,
            sum( `Aquaintance`) as Num_Aquaintance,
            sum( `Significant others`) as Num_Significant_others,
            sum( `Home Invasion`) as Num_Home_Invasion
             
            , percent_completed_hs, `Median.Income`,share_white,share_black,
            share_native_american,share_asian,share_hispanic,poverty_rate
            
            from Task3 GROUP BY Year,Month,State,City')
summary(Task3)

Cities<-sqldf('SELECT distinct State,`City.x` as City,count(distinct `Incident ID`) as Number_Incident,
     sum( `Num of Killed`) as Number_Killed,
     sum( `Num of Injured`) as Number_Injured,Latitude,`longitude.x` as longitude,
     
            sum( `Num of Guns`) as Number_Guns ,
            sum( `Number of Male(Participant)`) as `Number_Male(Participant)`,
         
            
            sum( `Number of Female(Participant)`) as `Number_Female(Participant)`,
            sum( `Number of Adult`) as Number_Adult,
            sum( `Number of Teen`) as Number_Teen,
            sum( `Total # Child`) as Total_Child,
            sum( `Total Participants`) as Total_Participants,
            sum( `Participant Arrested`) as Participant_Arrested,
            sum( `Participant Unharmed`) as Participant_Unharmed,
            sum( `Participant Injured`) as Participant_Injured,
            sum( `Participant Killed`) as Participant_Killed,
            
          
            sum( `Number of Victims`) as Number_Victims,
            sum( `Number of Suspects`) as Number_Suspects,
            
            
            sum(`Massive Shooting (Y/N)`) as Num_Massive_Shooting,
            
            sum( `Unknown(gun type)`) as Num_Unknown_guntype,
            sum( `SG(gun type)`) as Num_SG_guntype,
            sum( `Other(gun type)`) as Num_Other_guntype,

            sum( `H(gun type)`) as Num_H_guntype,
            sum( `HR(gun type)`) as Num_HR_guntype,
            sum( `Hunting Rifle(gun type)`) as Num_HuntingRifle_guntype,
            sum( `AR(gun type)`) as Num_AR_guntype,
            
            sum( `unknown`) as Num_unknown,
            sum( `stolen`) as Num_stolen,
            sum( `not stolen`) as Num_not_stolen,
             
             
            sum( `Family`) as Number_Family,
            sum( `Friends`) as Number_Friends,
            sum( `Co-worker`) as Num_Co_worker,
            sum( `Robbery`) as Number_Robbery,
            sum( `Neighbor`) as Number_Neighbor,
            sum( `Gang vs Gang`) as Num_GangGang,
            sum( `Aquaintance`) as Num_Aquaintance,
            sum( `Significant others`) as Num_Significant_others,
            sum( `Home Invasion`) as Num_Home_Invasion
             
            , percent_completed_hs, `Median.Income`,share_white,share_black,
            share_native_american,share_asian,share_hispanic,poverty_rate,population
            
            from Task3 where Year <>"2013" GROUP BY State,City
              ')
summary(Cities)


Cities$State_City<- with(Cities, paste0(Cities$State,sep = "_", Cities$City))
Cities$Avg_Incident_Per_Month<- Cities$Number_Incident/51
Cities$Avg_Incident_Per_Month_Per_Capita<-ifelse(Cities$Avg_Incident_Per_Month != 0,
                           Cities$Avg_Incident_Per_Month / Cities$population, 0)





freq<-sqldf('SELECT distinct Year,Month, count(distinct City) as City
   from Monthly GROUP BY Year,Month')

freq1<-sqldf('SELECT distinct Year,Month, sum( Number_Incident) as Number_Incident
   from Monthly GROUP BY Year,Month')

library("writexl")
library("xlsx")

write_xlsx(Cities,"Task3-aggregate to City_v3.xlsx")

write_xlsx(Monthly,"Task3-aggregate to Monthly with City.xlsx")
write.xlsx(freq,"freq.xlsx",sheetName = "City Freq", append = FALSE)
write.xlsx(freq1,"freq.xlsx",sheetName = "Number_Incident Freq", append = TRUE)
getwd()



#### 04. Modeling #### 
#Author: Madhu
#Loading the packages and libraries necessary
Packages <- c('tidyverse', 'mice', 'readxl', 'MASS','ggplot2', 'tidyr', 'lubridate','lattice', 'car')
lapply(Packages, library, character.only = TRUE)
install.packages("caret")
library(caret)
install.packages("olsrr")
library(olsrr)
#Read the file into R
project_file <- read_excel(file.choose()) #data file to be used is Task3-aggregate to City_v3.xlsx
head(project_file)
summary(project_file)

#Remove the missing NA values from the dataset
project_file1 <- project_file[complete.cases(project_file), ]

#Train and test datasets 85,15
sample <- sample.int(n = nrow(project_file1), 
                    size = floor(.85*nrow(project_file1)), replace = F)
train <- project_file1[sample, ]
test <- project_file1[-sample, ]
head(train)

#Include all vars for the model
reg <- lm(Avg_Incident_Per_Month_Per_Capita ~ . , train)
summary(reg1) #Lot of non Significant variables in the model, with least R2 and many outliers
plot(reg1)

#Reg model 1 only including socioeconomic variables
reg1 <- lm(Avg_Incident_Per_Month_Per_Capita ~  percent_completed_hs + Median.Income + share_white + share_black + share_native_american + share_asian + share_hispanic + poverty_rate, train)
summary(reg1) #R value is extremely low with non Significant variables share black share white and share native American
plot(reg1) 
ols_plot_cooksd_bar(reg1) #Cooks plot to find the outliers

#Tag the outliers
install.packages("ggstatsplot")
library(ggstatsplot)
ggbetweenstats(train, outlier.tagging = TRUE)

#Eliminate outliers
train <- train[-c(4038, 2487), ] #Outliers found from reression 1 model

#Model2 after removing outliers and non significant variables 
reg2 <- lm(Avg_Incident_Per_Month_Per_Capita ~ percent_completed_hs + Median.Income + share_asian + share_hispanic, train)
summary(reg2) #Low r square still with all significant variables 
par(mfrow=c(2,2))
plot(reg2) 
ols_plot_cooksd_bar(reg2)

#Removing non sig vars
reg3 <- lm(Avg_Incident_Per_Month_Per_Capita ~ percent_completed_hs + share_asian + share_hispanic + poverty_rate, train)
summary(reg3) #reduced r2 to 0.004 from 0.008
par(mfrow=c(2,2))
plot(reg3) 
ols_plot_cooksd_bar(reg3)

#Removing outliers !!
train <- train[-c(4348, 480, 774, 4177, 4538), ]
reg3.1 <- lm(Avg_Incident_Per_Month_Per_Capita ~ percent_completed_hs + share_asian + share_hispanic, train)
summary(reg3.1)
par(mfrow=c(2,2))
plot(reg3.1)
ols_plot_cooksd_bar(reg3.1)

#hetero test for reg2 model 
ncvTest(reg2) #hetero present 
install.packages("lmtest")
library(lmtest)
bptest(reg2) #p-value = 0.8983

#getting t test for the variables from the model
install.packages("sandwich")
library(sandwich)
coeftest(reg3.1, vcov = vcovHC(reg3.1, "HC1"))

#Prediction using the model outcome which is not accurate, but testing 
pred <- predict(reg2, test)
data.frame( R2 = R2(pred, test$Avg_Incident_Per_Month_Per_Capita),
            RMSE = RMSE(pred, test$Avg_Incident_Per_Month_Per_Capita),
            MAE = MAE(pred, test$Avg_Incident_Per_Month_Per_Capita))
###OUTPUT #R2         RMSE          MAE
###########1 0.001919582 0.0003399029 4.391557e-05

#Trying different ways and playing around with the variables and outliers from each regression
#Model 3
train <- train[-c(7022, 3449, 4559, 11908, 11903, 7446, 5551, 6658), ]
reg3 <- lm(Avg_Incident_Per_Month_Per_Capita ~ percent_completed_hs + Median.Income + share_white + share_black + share_native_american + share_asian + share_hispanic + poverty_rate, train)
summary(reg3)
par(mfrow=c(2,2))
plot(reg3)

#Model 4 
train <- train[-c(3656, 2379, 8263), ]
reg4 <- lm(Avg_Incident_Per_Month_Per_Capita ~ percent_completed_hs + Median.Income + share_white + share_black + share_native_american + share_asian + share_hispanic + poverty_rate, train)
summary(reg4)
par(mfrow=c(2,2))
plot(reg4)

#Model 5
train <- train[-c(5937, 10836, 6422), ]
reg5 <- lm(Avg_Incident_Per_Month_Per_Capita ~ percent_completed_hs + Median.Income + share_white + share_black + share_native_american + share_asian + share_hispanic + poverty_rate, train)
summary(reg5)
par(mfrow=c(2,2))
plot(reg5)

#Model 6
reg6 <- lm(Avg_Incident_Per_Month_Per_Capita ~ percent_completed_hs + Median.Income + share_asian + share_hispanic + poverty_rate, train)
summary(reg6)
par(mfrow=c(2,2))
plot(reg6)
ols_plot_cooksd_bar(reg6)

#Model 7
train <- train[-c(2473, 2361, 8562, 1725), ]
reg7 <- lm(Avg_Incident_Per_Month_Per_Capita ~ percent_completed_hs + Median.Income + share_white + share_black + share_native_american + share_asian + share_hispanic + poverty_rate, train)
summary(reg7)
par(mfrow=c(2,2))
plot(reg7)
ncvTest(reg7)
ols_plot_cooksd_bar(reg7)
#There are too many outliers to remove which might imporve the t test score of the variables, however the R2 for the model is still incredibly low. 

########
outlier_values <- boxplot.stats(train$Median.Income)$out  # outlier values for the median_income variables
boxplot(train$Median.Income, main="Median Income", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)
car::outlierTest(reg6) #10 obs are extreme, this code helps me to get the outlier values in the regression from car package.



#### 04. Executive Dashboard #### 








