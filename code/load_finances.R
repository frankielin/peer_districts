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
## Loading data from the UI "API"
data <- get_education_data(level = "school-districts",
                           source = "ccd",
                           topic = "finance")

## Note that data dictionary is located at: https://educationdata.urban.org/documentation/school-districts.html#ccd_finance
## Important variables: 
## - leaid: Identifies district (NCES)
## - Anything that starts with exp: These are the expenditure variables
##anything that starts with exp



############
## Export ##
############
write.csv(data,"../data/UI_district_finances.csv", row.names = FALSE)
