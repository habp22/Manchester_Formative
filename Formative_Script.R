#######################FORMATIVE ASSESSMENT#####################################

#install.packages("haven")
library(haven)
library(dplyr) #for my way of merging 
library(stringr) #for str_count
library(tidyr)
#install.packages("reshape")
library(reshape)
#install.packages("plyr")
library(plyr) #used for rbindfill 
#install.packages("mice")
library(mice)
#install.packages("corrplot")
library(corrplot)
#install.packages("VIM")
library(VIM)

setwd("C:/Users/Harri/Documents/Manchester/Manchester_Formative")

#Read in data sets
London_Dist <- read.csv("London District codes.csv")
London_Demo <- read.table("London ward data demographics.dat", header = TRUE, sep = '\t')
London_Envi <- read.csv("London ward data environment.csv")
London_Health <- read_sas("london ward data health.sas7bdat")
London_socio <- read_sav("London ward data socioeconomic.sav")


## PREPARING DATA

# Checking column names
colnames(London_Dist)
colnames(London_Demo)
colnames(London_Envi)
colnames(London_Health)
colnames(London_socio)


# Changing column name for ease
names(London_Demo)[names(London_Demo) == "ï..Wardname"] <- "Wardname"
names(London_Dist)[names(London_Dist) == "ï..District"] <- "District"
names(London_Envi)[names(London_Envi) == "ï..Wardcode"] <- "Wardcode"


# Checking missing data
sum(is.na(London_Demo))
sum(is.na(London_Dist))
sum(is.na(London_Envi))
sum(is.na(London_Health))
sum(is.na(London_socio))

# PREPARING LONDON_SOCIO DATA
# Removing empty rows
which(is.na(London_socio$hhSocialRented))
London_socio <- London_socio[-c(622:657),]
sum(is.na(London_socio))
# Extracting district code from Wardcode
London_socio$Districtcode <- substr(London_socio$Wardcode, 0, 4)
# Checking for unexpected values
which(grepl('[^[:alnum:]]', London_socio$Wardcode)) # Row 108 has an unexpected value
London_socio$Wardcode[108] # OOAGGK#
# Removing hashtag from entry
London_socio$Wardcode <- gsub("#", "", London_socio$Wardcode)
# Checking for duplicated rows
which(duplicated(London_socio))

# PREPARING ENVIRONMENT DATA
# Adding district code to data
London_Envi$Districtcode <- substr(London_Envi$Wardcode, 0, 4)
# Checking wardcodes are at most 6 characters
which(str_count(London_Envi$Wardcode) > 6) # 127
London_Envi$Wardcode[127] # 00BAGDag # Likely that ag is a mistake (Checked with London_Socio and this is an error)
# Removing extra characters
London_Envi$Wardcode <- substr(London_Envi$Wardcode, 0, 6)
London_Envi$Wardcode[127] # Check
# Checking for repeating rows
which(duplicated(London_Envi))

# PREPARING DEMO DATA
# Changing & to and
London_Demo$Wardname <- gsub("&", "and", London_Demo$Wardname)
# CHECKING FOR ACCIDENTIAL MERGED DATA
# Trying to split data in Demo which has merged rows 
#Find out how many characters are in the wardnames:
Count <- str_count(London_Demo$Wardname, "\\w+")
#If the characters are 10, it indicates that there's some extra data that's ended up in that column 
which(Count > 10) #  44  48  52  77  80  90 111 126 195 275 280 297 305
#Create an data frame of the "errors" aka those cols found above that have extra data:
London_Error <- London_Demo[c(44,  48,  52,  77,  80,  90, 111, 126, 195, 275, 280, 297, 305),]

# Merging columns together to enable extracting separate wards
London_Error$Wardname <- str_c(London_Error$Wardname, '\t', London_Error$Children, '\t', London_Error$Greaterthan65, 
                               '\t', London_Error$nonwhite, '\t', London_Error$NotBorninUK, '\t', London_Error$NotEnglishspeaking)

# Splitting observations based on wards
sep <- str_split_fixed(London_Error$Wardname, "\n", n = 72)
sep <- melt(sep)
#Separating into wards
sep <- str_split_fixed(sep$value, "\t", n = 6)

#Removing empty rows
data <- sep[!apply(sep == "", 1, all),]
# Adding prepared data back into the dataset (by removing incorrect rows)
London_Demo <- London_Demo[-c(44,  48,  52,  77,  80,  90, 111, 126, 195, 275, 280, 297, 305),]

# Changing into dataframe
Data <- as.data.frame(data)
# Renaming columns to fit with Demo data
names(Data)[1] <- "Wardname"
names(Data)[2] <- "Children"
names(Data)[3] <- "Greaterthan65"
names(Data)[4] <- "nonwhite"
names(Data)[5] <- "NotBorninUK"
names(Data)[6] <- "NotEnglishspeaking"

# Merging changed data (Managed to change this so that they have the same number of columns- no idea what I did before)
# This has sorted some of the NA's
London_Demo <- rbind(London_Demo, Data)
which(is.na(London_Demo))
# Sorting out index number
rownames(London_Demo) <- 1:nrow(London_Demo)

# Making all columns into numeric
i <- c(2, 6) 
London_Demo$Children <- as.numeric(as.character(London_Demo$Children))
London_Demo$Greaterthan65 <- as.numeric(as.character(London_Demo$Greaterthan65))
London_Demo$nonwhite <- as.numeric(as.character(London_Demo$nonwhite))
London_Demo$NotBorninUK <- as.numeric(as.character(London_Demo$NotBorninUK))
London_Demo$NotEnglishspeaking <- as.numeric(as.character(London_Demo$NotEnglishspeaking))

# Adding district name to Demo
London_Demo$District <- sapply(strsplit(London_Demo$Wardname, "-", fixed = T), function(x) (x[1]))
#Get rid of spaces after district 
London_Demo$District <- trimws(London_Demo$District, "r")
unique(London_Demo$District) # Check


# PREPARING HEALTH DATA
# Adding district name to Health data
London_Health$District <- sapply(strsplit(London_Health$Wardname, "-", fixed = T), function(x) (x[1]))
# Removing spaces after district
London_Health$District <- trimws(London_Health$District, "r")
unique(London_Demo$District) # Check

# Removing atmospheres in Health wardnames
London_Health$Wardname <- gsub("'", '', London_Health$Wardname)

# Changing & to and
London_Health$Wardname <- gsub("&", "and", London_Health$Wardname)

# 132
# Merging datasets

#Merge health and demo on wardname
Health_Demo <- merge(London_Health, London_Demo, by = c("Wardname", "District"), all = TRUE)
# Checking for Missing data
# Fixing row index
rownames(Health_Demo) <- 1:nrow(Health_Demo)
# Checking missing data
Missing_Data <- Health_Demo[rowSums(is.na(Health_Demo)) > 0,] # Issue with Saint and St (need consistency, change all to St)
# Changing Saint to St. in Demo data
London_Demo$Wardname <- gsub("Saint", 'St.', London_Demo$Wardname)

# Merging again to check
Health_Demo <- merge(London_Health, London_Demo, by = c("Wardname", "District"), all = TRUE)
Health_Demo[rowSums(is.na(Health_Demo)) > 0,] # This information is unavailable in the different datasets (3 rows with missing values)



#@Eleanor sorry, I merge differently to you but I've left your way in but commented out for when you come back to it. 
#I tend to get fewer errors this way.
#Health_Demo = London_Health %>% right_join(London_Demo,by=c("Wardname", "District"))

# Merge Socio and Envi on Wardcode
Socio_Envi <- merge(London_socio, London_Envi, by = c("Wardcode", "Districtcode"), all = TRUE)
# Fixing row index
rownames(Socio_Envi) <- 1:nrow(Socio_Envi)
# Checking missing data
Missing_Data <- Socio_Envi[rowSums(is.na(Socio_Envi)) > 0,] #3 rows of missing data 
# This data is just unavailable in the Socio dataset

#Socio_Envi = London_socio %>% left_join(London_Envi,by=c("Wardcode", "Districtcode"))

#Get rid of spaces before district code 
London_Dist$Districtcode <- gsub('\\s+', '', London_Dist$Districtcode)
London_Envi$Districtcode <- gsub('\\s+', '', London_Envi$Districtcode)
London_Dist$Districtcode

# Merging Socioeconomic, Environment and District codes by Districtcode
Merged <- merge(x = Socio_Envi, y = London_Dist, by = "Districtcode", all.x = TRUE)
#Merged = Socio_Envi %>% left_join(London_Dist, by=c("Districtcode"))

# Checking missing data
Missing_Data <- Merged[rowSums(is.na(Merged)) > 0,] #3 rows of missing data (Same as previously)

# Merging all data together by district name
All_data <- merge(x = Merged, y = Health_Demo, by = c("District", "Population2011Census"), all = TRUE)
#All_data = Merged %>% left_join(Health_Demo, by=c("District", "Population2011Census"))
# Checking missing data
Missing_Data <- All_data[rowSums(is.na(All_data)) > 0,] # 8 rows of missing data 
# Rows 3, 395, and 557 is same row which had missing data previously
# 536 was unavailable for Demo data
# Highly likely that row 403 and 412 are the same wards but information on population was not available from Socio data
# Same with row 490 and 492


# Checking for outliers
par(mfrow=c(3,5))
boxplot(All_data$hhSocialRented, main = "Social Rented")
boxplot(All_data$JobSeekers, main = "Job Seekers")
boxplot(All_data$Noqual, main = "No qualifications")
boxplot(All_data$Carsperhousehold, main = "Cars per household")
boxplot(All_data$Crimerate, main = "Crime rate")
# Observation 615 and 617 have very high crime rates +1500- likely to be an extra zero inserted
boxplot(All_data$Openspace, main = "Open space percentage")
boxplot(All_data$GeneralFertilityRate, main = "Fertility rate")
boxplot(All_data$Malelifeexpectancy, main = "Male life expectancy")
# Observation 537 has life expectancy of 178?? Very unlikely...
boxplot(All_data$Femalelifeexpectancy, main = "Female life expectancy")
boxplot(All_data$Children, main = "Percentage with children")
boxplot(All_data$Greaterthan65, main = "Greater than 65")
boxplot(All_data$nonwhite, main = "Percentage non-white")
boxplot(All_data$NotBorninUK, main = "Percentage not born in UK")
boxplot(All_data$NotEnglishspeaking, main = "Non-English speaking")



#Checking missing data
sum(is.na(All_data))
md.pattern(All_data)
colSums(is.na(All_data))
which(colSums(is.na(All_data))>0)
names(which(colSums(is.na(All_data))>0))

#Gonna try mean imputation on a couple cols:
All_data$Population2011Census[is.na(All_data$Population2011Census)] <- mean(All_data$Population2011Census, na.rm = TRUE)
All_data$Crimerate[is.na(All_data$Crimerate)] <- mean(All_data$Crimerate, na.rm = TRUE)
All_data$Openspace[is.na(All_data$Openspace)] <- mean(All_data$Openspace, na.rm = TRUE)
All_data$GeneralFertilityRate[is.na(All_data$GeneralFertilityRate)] <- mean(All_data$GeneralFertilityRate, na.rm = TRUE)
All_data$Malelifeexpectancy[is.na(All_data$Malelifeexpectancy)] <- mean(All_data$Malelifeexpectancy, na.rm = TRUE)
All_data$Femalelifeexpectancy[is.na(All_data$Femalelifeexpectancy)] <- mean(All_data$Femalelifeexpectancy, na.rm = TRUE)
All_data$Children[is.na(All_data$Children)] <- mean(All_data$Children, na.rm = TRUE)
All_data$Greaterthan65[is.na(All_data$Greaterthan65)] <- mean(All_data$Greaterthan65, na.rm = TRUE)
All_data$nonwhite[is.na(All_data$nonwhite)] <- mean(All_data$nonwhite, na.rm = TRUE)
All_data$NotBorninUK[is.na(All_data$NotBorninUK)] <- mean(All_data$NotBorninUK, na.rm = TRUE)
All_data$NotEnglishspeaking[is.na(All_data$NotEnglishspeaking)] <- mean(All_data$NotEnglishspeaking, na.rm = TRUE)

#Mean imputation got it down from 219 to 165 NAs -> with changes got 26 NA values
sum(is.na(All_data))

# Checking missing data
Missing_Data <- All_data[rowSums(is.na(All_data)) > 0,] # 8 rows of missing data 

#Turns out we have 23 rows of missing data left, Tom had 10 rows left but Tom was better than us so it makes sense. 
sum(!complete.cases(All_data))
#Below tells you which rows are missing
which(rowSums(is.na(All_data))>0)
Missing_Data <- All_data[rowSums(is.na(All_data)) > 0,] # 8 rows of missing data


#Multiple Imputation 
All_DataNONA <- mice(All_data,m=10,maxit=50,meth='pmm')
summary(All_DataNONA)
#Checking missing values on multiple imputation, reduced from 165 to 138
# This makes sense to me bc some of the NAs are like place names and shit which can't be imputed. 
completeData <- complete(All_DataNONA,2)
sum(is.na(completeData)) # This has been reduced to 9


#Which rows:
#3 395 403 412 490 492 536 557

#Now all that's left is to build the predictive model (unless you wanna try getting NAs down more but idk):
predmodel <- with(data = All_DataNONA, exp = lm(Femalelifeexpectancy ~ Noqual + JobSeekers + Crimerate + Openspace + nonwhite + hhSocialRented + Carsperhousehold))
combine <- pool(predmodel)
summary(combine)
