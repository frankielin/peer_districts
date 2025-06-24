#####
## Title: Load District Financials
## Description: Download the district financials from the Urban Institute "API"
## Date Created: 2025-04-25
## Date Updated: 2025-04-25
####

######################
## Loading Packages ## # Note that this should be added to a general file that
###################### # we load in at the start
library(data.table)
library(educationdata)

##################
## Loading Data ##
##################
## Loading fiannce data from the UI "API"
data <- get_education_data(level = "school-districts",
                           source = "ccd",
                           topic = "finance")

## Note that data dictionary is located at: https://educationdata.urban.org/documentation/school-districts.html#ccd_finance
## Important variables:
## - leaid: Identifies district (NCES)
## - Anything that starts with exp: These are the expenditure variables

## Loading poverty levels form dataas well
saipe_data <- get_education_data(level = "school-districts",
                           source = "saipe")


## Loading data on enrollment type
school_enrollment <- get_education_data(level = "schools",
                                     source = "ccd",
                                     topic = "directory",
                                     filters = list(year = 2000:2019))

############
## Export ##
############
## Missing data from 1992 to 1993
## Data stops at 2020
write.csv(data,"../data/UI_district_finances.csv", row.names = FALSE)
write.csv(saipe_data,"../data/UI_district_sapie.csv", row.names = FALSE)
write.csv(school_enrollment,"../data/UI_district_directory.csv", row.names = FALSE)



# library(educationdata)
# library(data.table)
# 
# school_enrollment=get_education_data(level = "schools",
#                                      source = "ccd",
#                                      topic = "directory",
#                                      filters = list(year = 2000:2019))
# 
# in_cz=fread("C:/Users/rwmoo/OneDrive/Documents/Classes/Research/school_referenda/data/seda_crosswalk_5.0.csv")
# 
# cz_xwalk=unique(subset(in_cz, select=c("ncessch", "sedacz",  "sedametro")))
# cz_xwalk$ncessch=str_pad(cz_xwalk$ncessch, width = 12, side = "left", pad="0")
# enrollment_cz=data.table(merge(school_enrollment, cz_xwalk, by="ncessch"))
# 
# fwrite(enrollment_cz, "C:/Users/rwmoo/OneDrive/Documents/Classes/Research/school_referenda/data/school_enrollment_cz.csv")

