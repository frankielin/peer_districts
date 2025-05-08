rm(list = ls())

######################
## Loading Packages ##
######################
library(sf)
library(data.table)
library(ggplot2)
library(stringr)
library(tidyr)
library(foreach)
library(doParallel)
library(lfe)

##################
## Loading Data ##
##################
## Loading Referendum Data 
in_bonds = fread("../data/referendum/bonds_biasi.csv") # Bond Ref Data

## Commuting Zone Data 
# leaid_bounds=read_sf("../data/us_district_shapefile/schooldistrict_sy1718_tl18.shp") # There is no shp file in the folder?
in_cz = fread("../data/seda_crosswalk_5.0.csv")

## Loading Shape Files 
leaid_bounds=read_sf("../data/referendum/us_district_shapefile") # Loading overall shp files (is this correct to pull? I assume no)

## Loading district finance data 
district_finances = fread("../data/UI_district_finances.csv")

###################
## Cleaning Data ##
###################
#### Commuting Zones Data 
seda_dist_cz_xwalk=unique(subset(in_cz,leatype="Regular public school district", select=c("leaid", "fips", "sedacz")))
seda_dist_cz_xwalk[, n_in_cz:=uniqueN(leaid), by=c("fips", "sedacz")] 
seda_dist_cz_xwalk <- seda_dist_cz_xwalk[
  , .SD[which.max(n_in_cz)],
  by = leaid
][
  , .(leaid, fips, sedacz, n_in_cz)
]

seda_dist_cz_xwalk[, fips := sprintf("%02d", as.integer(fips))] # note that we have a fips code already for the 
seda_dist_cz_xwalk[, leaid := sprintf("%07d", as.integer(leaid))]

#### Bonds Data
## Creating variables 
in_bonds <- in_bonds[!is.na(leaid)]
in_bonds <- in_bonds[!is.na(year)]
in_bonds <- in_bonds[!(state %in% c("KS", "MA", 'MD', 'MO', 'NE'))] # filters out sus states

in_bonds[, id := 1:nrow(in_bonds)]
in_bonds[, leaid := sprintf("%07d", as.integer(leaid))]
in_bonds[, state_fips := substr(leaid, 1,2)]
in_bonds = merge(in_bonds, seda_dist_cz_xwalk, by = 'leaid')
in_bonds[, vote_share:=votesyes/totvotes]
in_bonds[is.na(votesharereqd), votesharereqd:=.5]
in_bonds[, centered_vote_share:=vote_share-votesharereqd]

## Creating summary table of the bonds per district-year
summary_bonds = in_bonds[,.(
  bond_count    = uniqueN(id),
  bond_instance = uniqueN(id)>=1
), 
by = .(leaid, year)
]


#### CREATE Base Dataframe Creation base dataframe should be all the 
## Filtering district finance data with data that are available for
states_in_bonds = unique(in_bonds[,state_fips])
district_finances[,fips:=sprintf("%02d", as.integer(fips))] # rename fips into the two digit state fips 
district_finances = district_finances[.(states_in_bonds), on = .(fips)] # Filters (binary search bitches)

## Merging on bond data 
print(paste("Pre-merge rows:", nrow(district_finances)))
district_finances = summary_bonds[
  district_finances,
  on = .(leaid = leaid, year = year),
  nomatch = NA
] # this is apparently the syntax for a left join this sucks lol I'll just use the normal merge syntax from now on
print(paste("Post-merge rows:", nrow(district_finances)))

## Checking the merge (will need to do some diagnosis on what is dropped later ) 
print(paste("N Summary Bonds:", nrow(summary_bonds)))
print(paste("Merged rows:", sum(!is.na(district_finances[,bond_instance])))) # I lose 700 observations let's ignore this for now

## Merging Commuting Zone 
district_finances = seda_dist_cz_xwalk[
  district_finances,
  on = .(leaid = leaid, fips = fips),
  nomatch = NA
] # note that some schools are missing districts (they are missing the )


## Checking the share of districts within CZ that had a bond measure in the past X years 
set_year_past = 3

left = district_finances[,c('leaid','fips','sedacz','year')]
left[,max_year:= year-1]
left[,min_year:= year-set_year_past]
instances = in_bonds[,c('leaid','year','fips','sedacz')]
setnames(instances, 
         c('leaid','year','fips','sedacz'), 
         paste0("instance_", c('leaid','year','fips','sedacz')))

print(nrow(left))
left = instances[
  left,
  on = .(instance_sedacz = sedacz,
         instance_fips = fips,
         instance_year <= max_year,
         instance_year >= min_year),
  nomatch = NA,
  mult = "all"
]

left = left[!is.na(instance_leaid)] # Dropping rows that have zero past instances
N_past_refs_dat = left[,.(past_unique_ref_districts = uniqueN(instance_leaid)), by = .(leaid, year)]

print(nrow(district_finances))
district_finances = N_past_refs_dat[
  district_finances,
  on = .(leaid = leaid, year = year),
  nomatch = NA
] # Merging back onto the main dataframe


## Checking the share of districts in the CA that WON a bond measure within X years
set_year_past = 3

left = district_finances[,c('leaid','fips','sedacz','year')]
left[,max_year:= year-1]
left[,min_year:= year-set_year_past]
winning_instances = in_bonds[pass == 1,c('leaid','year','fips','sedacz', "centered_vote_share")]
setnames(winning_instances, 
         c('leaid','year','fips','sedacz', 'centered_vote_share'), 
         paste0("instance_", c('leaid','year','fips','sedacz', "centered_vote_share")))

print(nrow(left))
left = winning_instances[
  left,
  on = .(instance_sedacz = sedacz,
         instance_fips = fips,
         instance_year <= max_year,
         instance_year >= min_year),
  nomatch = NA,
  mult = "all"
]

left = left[!is.na(instance_leaid)] # Dropping rows that have zero past instances
left = left[instance_leaid != leaid] # Removing instances where the district itself wins here 
N_past_winning_refs_dat = left[,.(past_unique_winning_ref_districts = uniqueN(instance_leaid),
                                  past_avg_winning_margin_districts = mean(instance_centered_vote_share)), by = .(leaid, year)]

print(nrow(district_finances))
district_finances = N_past_winning_refs_dat[
  district_finances,
  on = .(leaid = leaid, year = year),
  nomatch = NA
] # Merging back onto the main dataframe


## Checking the share of districts in the CA that LOST a bond measure within X years
set_year_past = 3

left = district_finances[,c('leaid','fips','sedacz','year')]
left[,max_year:= year-1]
left[,min_year:= year-set_year_past]
losing_instances = in_bonds[pass == 0,c('leaid','year','fips','sedacz')]
setnames(losing_instances, 
         c('leaid','year','fips','sedacz'), 
         paste0("instance_", c('leaid','year','fips','sedacz')))

print(nrow(left))
left = losing_instances[
  left,
  on = .(instance_sedacz = sedacz,
         instance_fips = fips,
         instance_year <= max_year,
         instance_year >= min_year),
  nomatch = NA,
  mult = "all"
]

left = left[!is.na(instance_leaid)] # Dropping rows that have zero past instances
N_past_losing_refs_dat = left[,.(past_unique_losing_ref_districts = uniqueN(instance_leaid)), by = .(leaid, year)]

print(nrow(district_finances))
district_finances = N_past_losing_refs_dat[
  district_finances,
  on = .(leaid = leaid, year = year),
  nomatch = NA
] # Merging back onto the main dataframe

## Finding the last year of the referendum
setorder(district_finances, leaid, year)
events = district_finances[bond_instance == TRUE, .(leaid, event_year = year)]
district_finances[, year_of_last_ref := { # Imma be so for real this is ChatGPT'd because writing myself was like 20 lines
  last_year = NA_integer_
  out = integer(.N)
  
  for (i in seq_len(.N)) {
    out[i] = last_year
    if (isTRUE(bond_instance[i])) last_year = year[i]
  }
  out
}, by = leaid]


###################
## Analysis Code ##
###################
#### Create the Analysis Dataframe
analysis_dat = district_finances
setorder(analysis_dat, leaid, year)

## Keeping data at the state level if we have at least 5 years prior of data 
min_max_years = district_finances[!is.na(bond_instance), .(
  min_year = min(year),
  max_year = max(year)
),
by = .(fips)] 

analysis_dat = min_max_years[
  district_finances,
  on = .(fips),
  nomatch = NA
] 

analysis_dat = analysis_dat[(year >= min_year + 5) & (year <= max_year)]

## Replacing NAs with zeros (this should be correct since I remove years without ish)
analysis_dat[is.na(bond_count), bond_count:=0]
analysis_dat[is.na(bond_instance), bond_instance:=0]
analysis_dat[is.na(past_unique_ref_districts), past_unique_ref_districts:=0]
analysis_dat[is.na(past_unique_winning_ref_districts), past_unique_winning_ref_districts:=0]
analysis_dat[is.na(past_unique_losing_ref_districts), past_unique_losing_ref_districts:=0]

## Removing districts where I do not know the commuting zone
analysis_dat = analysis_dat[!is.na(sedacz)]

#### Creating variables
## Misc.
analysis_dat[, share_past_ref := past_unique_ref_districts/n_in_cz]
analysis_dat[, share_past_losing_ref := past_unique_losing_ref_districts/n_in_cz]
analysis_dat[, share_past_winning_ref := past_unique_winning_ref_districts/n_in_cz]
analysis_dat[, cz_combined := paste0(fips,"_",sedacz)]
analysis_dat[,rev_state_total := rev_state_total/1000000]
analysis_dat[,rev_local_total := rev_local_total/1000000]
analysis_dat[,exp_total := exp_total/1000000]

## Pulling the last 
analysis_dat[,first_ref := (bond_instance == 1)*is.na(year_of_last_ref)]
analysis_dat[,years_since_last_ref:= year - year_of_last_ref]
analysis_dat[,recent_ref := years_since_last_ref >= 3]
analysis_dat[is.na(recent_ref),recent_ref := 0]

check = analysis_dat[, c('year', 'leaid', 'bond_instance', 'first_ref', 'year_of_last_ref')]


#### Regression lol
summary(felm(bond_instance ~ 
               past_unique_ref_districts + 
               share_past_winning_ref + 
               recent_ref + 
               rev_state_total +
               rev_local_total + 
               exp_total
             | year + leaid, analysis_dat)) 




