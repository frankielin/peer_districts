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
library(stargazer)

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

## Loading district crosswalk data 
touching_districts=fread("../data/referendum/touching_districts.csv")

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


#######################################
## CREATING COMMUTING ZONE DATAFRAME ##
#######################################
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
] 
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
left = left[instance_leaid != leaid] # Removing instances where the district itself wins here 
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
                                  past_avg_winning_margin_districts = mean(instance_centered_vote_share, na.rm = TRUE)), by = .(leaid, year)]

print(nrow(district_finances))
district_finances = N_past_winning_refs_dat[
  district_finances,
  on = .(leaid = leaid, year = year),
  nomatch = NA
] # Merging back onto the main dataframe

sum(is.na(winning_instances$instance_centered_vote_share))
sum(is.na(N_past_winning_refs_dat)) # this is weird

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
left = left[instance_leaid != leaid] # Removing instances where the district itself wins here 
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


#######################################
## Neighbors processing and Cleaning ##
#######################################
#### Brute Force Implementation
## For every year, I want to just cerge on the total number of forward looking referendum
min_year = min(in_bonds[, year] , na.rm = TRUE)
max_year = max(in_bonds[, year] , na.rm = TRUE)

years = min_year:max_year

## Creating a variable counting the number of possible neighbors
tot_neighbors_dat = touching_districts[,.(N_neighbors = .N), by = .(leaid)]

## Creating expanded dataset with the set number of
expanded_dat = touching_districts[, .(year = years), by = .(leaid, neighbor_leaid)]
expanded_dat[, merge_year_start := year -3 ]
expanded_dat[, merge_year_end   := year -1 ]

to_merge_bond_instances = in_bonds[,c('leaid', 'year', 'id', 'pass')] # these are the set of bonds that I care about at all
setnames(to_merge_bond_instances,
         old = c("leaid", "year"),
         new = c("leaid_instance", "year_instance"))

nrow(expanded_dat)
expanded_dat[, leaid := sprintf("%07d", as.integer(leaid))]
expanded_dat[, neighbor_leaid := sprintf("%07d", as.integer(neighbor_leaid))]
expanded_dat = to_merge_bond_instances[
  expanded_dat,
  on = .(leaid_instance = neighbor_leaid,
         year_instance >= merge_year_start,
         year_instance <= merge_year_end),
  nomatch = NA,
  mult = "all"
]


setnames(expanded_dat,
         old = c("leaid_instance"),
         new = c("neighbor_leaid")) ## So then I wanna collapse on this and the year

## Keeping observations with an instance
expanded_dat  = expanded_dat[!is.na(id)] # this means that there was at least a merged deal

nrow(expanded_dat)
expanded_dat  = expanded_dat[neighbor_leaid!=leaid] # This should drop nothing
nrow(expanded_dat)

## Collapsing Data by the 
expanded_dat <- expanded_dat[,
                             .(neighbor_winner = max(pass)),
                             by = .(leaid, neighbor_leaid, year)]

## Collapsing and calculating the unique neighbors that are considered for a neighbor in the future year
neighbors_ref_merge_dat = expanded_dat[,
                                       .(N_ref_past_districts_neighbors = uniqueN(neighbor_leaid),
                                         N_win_past_districts_neighbors = sum(neighbor_winner)),
                                       by = .(leaid, year)] # in this case everything should be strictly larger than 1

## Creating a datframe that just tells us the number of total neighbors
tot_neighbors_dat = touching_districts[,.(N_neighbors = .N), by = .(leaid)]
tot_neighbors_dat[,leaid := sprintf("%07d", as.integer(leaid))]

## Merging onto Dataframe
district_finances <- merge(district_finances, neighbors_ref_merge_dat, 
                           by = c('leaid', 'year'), all.x = TRUE)

district_finances <- merge(district_finances, tot_neighbors_dat, 
                           by = c('leaid'), all.x = TRUE)

district_finances[is.na(N_ref_past_districts_neighbors) & (!is.na(N_neighbors)), N_ref_past_districts_neighbors := 0]
district_finances[is.na(N_ref_past_districts_neighbors) & (!is.na(N_neighbors)), N_win_past_districts_neighbors := 0]

nrow(district_finances[is.na(N_neighbors)]) ## Have Rachel check this please?

nrow(district_finances[is.na(n_in_cz)])

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
# analysis_dat = analysis_dat[min_year <= 1998]
# analysis_dat = analysis_dat[year >= 2009]

## Replacing NAs with zeros (this should be correct since I remove years without ish)
analysis_dat[is.na(bond_count), bond_count:=0]
analysis_dat[is.na(bond_instance), bond_instance:=0]
analysis_dat[is.na(past_unique_ref_districts), past_unique_ref_districts:=0]
analysis_dat[is.na(past_unique_winning_ref_districts), past_unique_winning_ref_districts:=0]
analysis_dat[is.na(past_unique_losing_ref_districts), past_unique_losing_ref_districts:=0]

## Removing districts where I do not know the commuting zone
analysis_dat = analysis_dat[!is.na(sedacz)]

#### Creating variables
## Creating Data for CZ
analysis_dat[, share_past_ref := past_unique_ref_districts/(n_in_cz-1)]
analysis_dat[, share_past_losing_ref := past_unique_losing_ref_districts/(n_in_cz-1)]
analysis_dat[, share_past_winning_ref := past_unique_winning_ref_districts/(n_in_cz-1)]

## Creating Data for neighbors
analysis_dat[, share_past_ref_neighbors := N_ref_past_districts_neighbors/(N_neighbors)]
analysis_dat[, share_past_winning_ref_neighbors := N_win_past_districts_neighbors/(N_neighbors)]

## All things that are important
analysis_dat[, cz_combined := paste0(fips,"_",sedacz)]
analysis_dat[, year_state:= paste0(fips, year) ]
analysis_dat[,rev_state_total := rev_state_total/100000000]
analysis_dat[,rev_local_total := rev_local_total/100000000]
analysis_dat[,exp_total := exp_total/100000000]
analysis_dat[,salaries_total := salaries_total/100000000]
analysis_dat[,enrollment_fall_responsible := enrollment_fall_responsible/1000]


## Creating a variable if this a recent referendum (less than or equal to 3 years)
analysis_dat[,first_ref := (bond_instance == 1)*is.na(year_of_last_ref)]
analysis_dat[,years_since_last_ref:= year - year_of_last_ref]
analysis_dat[,recent_ref := years_since_last_ref >= 3]
analysis_dat[is.na(recent_ref),recent_ref := 0]

check = analysis_dat[, c('year', 'leaid', 'bond_instance', 'first_ref', 'year_of_last_ref')]


checker = analysis_dat[is.na(share_past_winning_ref)]
checker = analysis_dat[is.na(share_past_winning_ref_neighbors)]
nrow(checker)

analysis_dat = analysis_dat[!is.na(share_past_winning_ref) & !is.na(share_past_winning_ref_neighbors)]


#### Regression lol
## CZ
out_cz <- felm(bond_instance ~ 
               share_past_winning_ref + 
               past_unique_ref_districts + 
               recent_ref + 
               rev_state_total +
               rev_local_total + 
               exp_total+
              enrollment_fall_responsible
             | year_state + leaid | 0 | leaid, data = analysis_dat)
         
summary(out_cz)

## Neighbors 
out_neighbors <- felm(bond_instance ~ 
              share_past_winning_ref_neighbors + 
              N_ref_past_districts_neighbors + 
              recent_ref + 
              rev_state_total +
              rev_local_total + 
              exp_total +
              enrollment_fall_responsible
            | year_state + leaid | 0 | leaid, data = analysis_dat)

summary(out_neighbors)

a = sum(analysis_dat$bond_instance)
b = length(!is.na(analysis_dat$bond_instance))
a/b

summary(analysis_dat[,share_past_ref])
summary(analysis_dat[,share_past_winning_ref])

stargazer(out_cz, out_neighbors)



# 
# stargazer(out)
# 
# colnames(analysis_dat)

unique(analysis_dat$fips)

unique(analysis_dat$fips[analysis_dat$min_year < 1995])

