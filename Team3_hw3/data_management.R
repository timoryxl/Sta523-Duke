#####################
### Load Packages
#####################

library(dplyr)
library(data.table)
library(magrittr)
library(rgdal)
library(stringr)



#####################
### Load NYC data
#####################

# Read data file
nyc = fread("/home/vis/cr173/Sta523/data/nyc/nyc_311.csv") %>%
      tbl_df() # Turn it into a dataframe for data management purposes

# See names of variables
names(nyc)

# Define relevant variables for analysis
keep.var = c("Location.Type","Incident.Zip",
             "Incident.Address","Street.Name",
             "Cross.Street.1","Cross.Street.2",
             "Intersection.Street.1","Intersection.Street.2",
             "Address.Type","Borough","Complaint.Type",
             "Created.Date")

# Keep only relevant variables
nyc=nyc[,keep.var]

# Remove object from global environment
rm(keep.var) 

# Remove whitespace from beginning and end of string
str_trim(nyc$Incident.Address)

# Remove white spaces (captures 2 or more white spaces)
str_replace(nyc$Incident.Address, "[ ]{2,}"," ")

### CHARACTERISTICS OF STREET NAMES IN NYC DATASET:
# 1. All street names are capitalized: e.g. 21ST STREET
# 2. Some street numbers are spelled out: e.g. FIRST STREET
# 3. Street numbers have "st", "nd", "rd", "th" added to them: e.g. 24TH AVENUE
# 4. Addresses sometimes have a range of house numbers: e.g. 42-65 KISSENA BOULEVARD

# GOAL: Change all 1st, 2nd, 3rd, 4th, etc.
# Because in pluto data it is "2 STREET" not "2ND STREET"
str_replace(nyc$Incident.Address, "1ST","1")
str_replace(nyc$Incident.Address, "2ND","2")
str_replace(nyc$Incident.Address, "3RD","3")
for (i in 4:9){
  str_replace(nyc$Incident.Address, paste0(i,"TH"),as.character(i))
}

# Remove unnecessary object
rm(i)

# GOAL: Replace all spelled out street names
# Because in pluto data it is "1 STREET" not "FIRST STREET"
str_replace(nyc$Incident.Address, "FIRST STREET","1 STREET")
str_replace(nyc$Incident.Address, "SECOND STREET","2 STREET")
str_replace(nyc$Incident.Address, "THIRD STREET","3 STREET")
str_replace(nyc$Incident.Address, "FOURTH STREET","4 STREET")
str_replace(nyc$Incident.Address, "FIFTH STREET","5 STREET")
str_replace(nyc$Incident.Address, "SIXTH STREET","6 STREET")
str_replace(nyc$Incident.Address, "SEVENTH STREET","7 STREET")
str_replace(nyc$Incident.Address, "EIGTH STREET","8 STREET")
str_replace(nyc$Incident.Address, "NINTH STREET","9 STREET")

# Do the same for the names of avenues
str_replace(nyc$Incident.Address, "FIRST AVENUE","1 AVENUE")
str_replace(nyc$Incident.Address, "SECOND AVENUE","2 AVENUE")
str_replace(nyc$Incident.Address, "THIRD AVENUE","3 AVENUE")
str_replace(nyc$Incident.Address, "FOURTH AVENUE","4 AVENUE")
str_replace(nyc$Incident.Address, "FIFTH AVENUE","5 AVENUE")
str_replace(nyc$Incident.Address, "SIXTH AVENUE","6 AVENUE")
str_replace(nyc$Incident.Address, "SEVENTH AVENUE","7 AVENUE")
str_replace(nyc$Incident.Address, "EIGTH AVENUE","8 AVENUE")
str_replace(nyc$Incident.Address, "NINTH AVENUE","9 AVENUE")

# GOAL: Replace addresses with multiple house numbers to a single house number
# Because in pluto data it is "40 MAIN STREET" not "40-50 MAIN STREET"

str_replace(nyc$Incident.Address, "-[0-9]{1,}", "")

# Rename variable "Incident.Address" so that it can be merged with Pluto
nyc$Address=nyc$Incident.Address

# Remove white spaces (this code captures 2 or more white spaces)
str_replace(nyc$Incident.Address, "[ ]{2,}"," ")

### Work on intersections
head(nyc$Intersection.Street.1, n=1000)

# Remove white spaces (this code captures 2 or more white spaces)
str_replace(nyc$Intersection.Street.1, "[ ]{2,}"," ")
str_replace(nyc$Intersection.Street.2, "[ ]{2,}"," ")

# Rename for merging purposes (with intersection data)
nyc$Stree1=nyc$Intersection.Street.1 # notice Stree1 not Stree_t_1 (no "t")
nyc$Street2=nyc$Intersection.Street.2



#####################
### Load Pluto data
#####################

load("/home/vis/cr173/Sta523/data/nyc/pluto/pluto.Rdata")

# IN PLUTO:
# All street names are capitalized. e.g. 2 AVENUE
# Street numbers are not spelled out. e.g. 1 STREET
# There is no 1st, 2nd, 3rd, just 1, 2, 3: e.g. 3 AVENUE
# Addresses do not contain multiple numbers. All refer to a single house number. e.g. 19 3 AVENUE

# Remove white spaces (this code captures 2 or more white spaces)
str_replace(pluto$Address, "[ ]{2,}"," ")



#####################
### Merge NYC and Pluto Data
#####################

# Join the two data frames (nyc and pluto)
merged_data=left_join(nyc,pluto,by=c("Address"))

# Alternative method
# join=merge(nyc2,pluto,by="Address", all.x=T)

# Display head and tail of joint data frame
head(merged_data, n=1000)
tail(merged_data$Address, n=1000)

### Check the variable names in merged_data
names(merged_data)

### Save first data file for data analysis
data.to.save.1 <- merged_data[,c("Borough.x",
                                 "Borough.y", 
                                 "x",
                                 "y",
                                 "Complaint.Type",
                                 "Created.Date")]

# Create local path to save the data file
dir.create(path= "~/data", recursive = TRUE, showWarnings = FALSE)

# Save the data file to the newly created directory
save(data.to.save.1, file="~/data/analysis_data.Rdata")



#####################
### Load Intersection data
#####################

# Load Rdata object on intersections
load("/home/vis/cr173/Sta523/data/nyc/intersections/intersections.Rdata")

# Check for variable names
names(data) # Note: Stree1 not Street1
head(data, n=100)

# Replace whitespaces (captures 2 or more white spaces)
str_replace(data$Stree1, "[ ]{2,}"," ")
str_replace(data$Street2, "[ ]{2,}"," ")



#####################
### Merge NYC and intersection data
#####################

# Do a left join of the two data frames (original merged_data and intersections data)
merged_data=left_join(merged_data,data,by=c("Stree1","Street2"))

# Take the important variables to save the data file
data.to.save <- merged_data[,c("Borough.x",
                              "Borough.y", 
                              "x",
                              "y",
                              "longitude",
                              "latitude",
                              "Complaint.Type",
                              "Created.Date")]

# Create local path to save the data file
dir.create(path= "data", recursive = TRUE, showWarnings = FALSE)

# Save the data file to the newly created directory
save(data.to.save, file="data/merged_data.Rdata")


