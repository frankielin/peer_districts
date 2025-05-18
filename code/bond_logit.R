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

## Loading SAIPES Data 
district_saipe=fread("../data/UI_district_sapie.csv")

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


#### SAPIE Data (Poverty Stats)
## Removing DC and Hawaii
bad_states = c(11, 15)
district_saipe = district_saipe[!(fips%in%bad_states) ]

## Measuring the Average Poverty Rate Throughout the Years
average_poverty_dat = district_saipe[, # maybe filter for certain years here might be good to do? Or I can let it vary year by year
                                 .(avg_poverty_pct = mean(est_population_5_17_poverty_pct), na.rm = TRUE), 
                                 by = .(leaid, fips) ]  # I dont think that the poverty quantile should really change year to year 

## Calculating different quantiles of the poverty rates 
average_poverty_dat[, poverty_qtile := cut(avg_poverty_pct,
                                       breaks = quantile(avg_poverty_pct, probs = seq(0, 1, 0.2), na.rm = TRUE),
                                       include.lowest = TRUE,
                                       labels = FALSE),
                by = fips]

average_poverty_dat[, poverty_dtile := cut(avg_poverty_pct,
                                           breaks = quantile(avg_poverty_pct, probs = seq(0, 1, 0.1), na.rm = TRUE),
                                           include.lowest = TRUE,
                                           labels = FALSE),
                    by = fips]

average_poverty_dat = average_poverty_dat[,c('leaid', 'avg_poverty_pct', 'poverty_qtile', 'poverty_dtile')]

average_poverty_dat[, leaid := sprintf("%07d", as.integer(leaid))]

table(is.na(average_poverty_dat$poverty_qtile)) 
table(is.na(average_poverty_dat$poverty_dtile)) 

#######################################
## CREATING COMMUTING ZONE DATAFRAME ##
#######################################
#### CREATE Base Dataframe Creation base dataframe should be all the 
## Filtering district finance data with data that are available for
states_in_bonds = unique(in_bonds[,state_fips])
district_finances[,fips:=sprintf("%02d", as.integer(fips))] # rename fips into the two digit state fips 
district_finances = district_finances[.(states_in_bonds), on = .(fips)] # Filters (binary search bitches)

## Merging poverty data
district_finances <- merge(district_finances, average_poverty_dat, by = 'leaid',  all.x = TRUE)
table(is.na(district_finances$poverty_qtile)) ## Some of this is just flat missing from the data 

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
district_finances[is.na(N_win_past_districts_neighbors) & (!is.na(N_neighbors)), N_win_past_districts_neighbors := 0]

nrow(district_finances[is.na(N_neighbors)]) ## Have Rachel check this please?

nrow(district_finances[is.na(n_in_cz)])

table(is.na(district_finances[,N_neighbors]))

###################
## Analysis Data ##
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
analysis_dat[is.na(past_unique_ref_districts), past_unique_ref_districts := 0]
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
analysis_dat[, year_cz:= paste0(cz_combined, year) ]
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
nrow(checker)
checker = analysis_dat[is.na(share_past_winning_ref_neighbors)]
nrow(checker)
checker = analysis_dat[is.na(N_neighbors)]
nrow(checker)

analysis_dat = analysis_dat[!is.na(share_past_winning_ref) & !is.na(share_past_winning_ref_neighbors)]


##########################
## BASELINE REGRESSIONS ##
##########################
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

stargazer(out_cz, out_neighbors)

baseline_referendum_rate = mean(analysis_dat[,bond_instance])
baseline_referendum_rate

##############################
## HETEROGENITY REGRESSIONS ##
##############################
## Checking my coverage for the heterogeneity analysis 
table(analysis_dat[,poverty_qtile]) # we are slightly more over represented by higher poverty districts
table(is.na(analysis_dat[,poverty_qtile])) # Okay so we aren't missing any

analysis_dat[, poverty_qtile_1_share_past_winning_ref := (poverty_qtile == 1) * share_past_winning_ref]
analysis_dat[, poverty_qtile_2_share_past_winning_ref := (poverty_qtile == 2) * share_past_winning_ref]
analysis_dat[, poverty_qtile_3_share_past_winning_ref := (poverty_qtile == 3) * share_past_winning_ref]
analysis_dat[, poverty_qtile_4_share_past_winning_ref := (poverty_qtile == 4) * share_past_winning_ref]
analysis_dat[, poverty_qtile_5_share_past_winning_ref := (poverty_qtile == 5) * share_past_winning_ref]

analysis_dat[, poverty_qtile_1_share_past_winning_ref_neighbors := (poverty_qtile == 1) * share_past_winning_ref_neighbors]
analysis_dat[, poverty_qtile_2_share_past_winning_ref_neighbors := (poverty_qtile == 2) * share_past_winning_ref_neighbors]
analysis_dat[, poverty_qtile_3_share_past_winning_ref_neighbors := (poverty_qtile == 3) * share_past_winning_ref_neighbors]
analysis_dat[, poverty_qtile_4_share_past_winning_ref_neighbors := (poverty_qtile == 4) * share_past_winning_ref_neighbors]
analysis_dat[, poverty_qtile_5_share_past_winning_ref_neighbors := (poverty_qtile == 5) * share_past_winning_ref_neighbors]

table(analysis_dat[,poverty_dtile]) # we are slightly more over represented by higher poverty districts
table(is.na(analysis_dat[,poverty_dtile])) # Okay so we aren't missing any

analysis_dat[, poverty_dtile_1_share_past_winning_ref := (poverty_dtile == 1) * share_past_winning_ref]
analysis_dat[, poverty_dtile_2_share_past_winning_ref := (poverty_dtile == 2) * share_past_winning_ref]
analysis_dat[, poverty_dtile_3_share_past_winning_ref := (poverty_dtile == 3) * share_past_winning_ref]
analysis_dat[, poverty_dtile_4_share_past_winning_ref := (poverty_dtile == 4) * share_past_winning_ref]
analysis_dat[, poverty_dtile_5_share_past_winning_ref := (poverty_dtile == 5) * share_past_winning_ref]
analysis_dat[, poverty_dtile_6_share_past_winning_ref := (poverty_dtile == 6) * share_past_winning_ref]
analysis_dat[, poverty_dtile_7_share_past_winning_ref := (poverty_dtile == 7) * share_past_winning_ref]
analysis_dat[, poverty_dtile_8_share_past_winning_ref := (poverty_dtile == 8) * share_past_winning_ref]
analysis_dat[, poverty_dtile_9_share_past_winning_ref := (poverty_dtile == 9) * share_past_winning_ref]
analysis_dat[, poverty_dtile_10_share_past_winning_ref := (poverty_dtile == 10) * share_past_winning_ref]

analysis_dat[, poverty_dtile_1_share_past_winning_ref_neighbors := (poverty_dtile == 1) * share_past_winning_ref_neighbors]
analysis_dat[, poverty_dtile_2_share_past_winning_ref_neighbors := (poverty_dtile == 2) * share_past_winning_ref_neighbors]
analysis_dat[, poverty_dtile_3_share_past_winning_ref_neighbors := (poverty_dtile == 3) * share_past_winning_ref_neighbors]
analysis_dat[, poverty_dtile_4_share_past_winning_ref_neighbors := (poverty_dtile == 4) * share_past_winning_ref_neighbors]
analysis_dat[, poverty_dtile_5_share_past_winning_ref_neighbors := (poverty_dtile == 5) * share_past_winning_ref_neighbors]
analysis_dat[, poverty_dtile_6_share_past_winning_ref_neighbors := (poverty_dtile == 6) * share_past_winning_ref_neighbors]
analysis_dat[, poverty_dtile_7_share_past_winning_ref_neighbors := (poverty_dtile == 7) * share_past_winning_ref_neighbors]
analysis_dat[, poverty_dtile_8_share_past_winning_ref_neighbors := (poverty_dtile == 8) * share_past_winning_ref_neighbors]
analysis_dat[, poverty_dtile_9_share_past_winning_ref_neighbors := (poverty_dtile == 9) * share_past_winning_ref_neighbors]
analysis_dat[, poverty_dtile_10_share_past_winning_ref_neighbors := (poverty_dtile == 10) * share_past_winning_ref_neighbors]

## Commuting Zones
out_cz <- felm(bond_instance ~
                 poverty_dtile_2_share_past_winning_ref +
                 poverty_dtile_3_share_past_winning_ref +
                 poverty_dtile_4_share_past_winning_ref +
                 poverty_dtile_5_share_past_winning_ref +
                 poverty_dtile_6_share_past_winning_ref +
                 poverty_dtile_7_share_past_winning_ref +
                 poverty_dtile_8_share_past_winning_ref +
                 poverty_dtile_9_share_past_winning_ref +
                 poverty_dtile_10_share_past_winning_ref +
                 share_past_winning_ref +
                 past_unique_ref_districts +
                 recent_ref +
                 rev_state_total +
                 rev_local_total +
                 exp_total+
                 enrollment_fall_responsible
               | year_state + leaid | 0 | leaid, data = analysis_dat)

summary(out_cz)

out_cz <- felm(bond_instance ~ 
                 poverty_qtile_2_share_past_winning_ref +
                 poverty_qtile_3_share_past_winning_ref +
                 poverty_qtile_4_share_past_winning_ref +
                 poverty_qtile_5_share_past_winning_ref +
                 share_past_winning_ref +
                 past_unique_ref_districts + 
                 recent_ref + 
                 rev_state_total +
                 rev_local_total + 
                 exp_total+
                 enrollment_fall_responsible
               | year_state + leaid | 0 | leaid, data = analysis_dat)

summary(out_cz)

## Extract coefficients from the out_cz model
coef_cz <- as.data.table(summary(out_cz)$coefficients, keep.rownames = "term")
coef_cz <- coef_cz[term %like% "poverty_qtile_"]
coef_cz[, qt := 2:5]
coef_cz[, model := "Commuting Zone"]
coef_cz[, ci_lower := Estimate - 1.96 * `Cluster s.e.`]
coef_cz[, ci_upper := Estimate + 1.96 * `Cluster s.e.`]

## Non Commuting Zones
out_neighbors <- felm(bond_instance ~
                        poverty_dtile_2_share_past_winning_ref_neighbors +
                        poverty_dtile_3_share_past_winning_ref_neighbors +
                        poverty_dtile_4_share_past_winning_ref_neighbors +
                        poverty_dtile_5_share_past_winning_ref_neighbors +
                        poverty_dtile_6_share_past_winning_ref_neighbors +
                        poverty_dtile_7_share_past_winning_ref_neighbors +
                        poverty_dtile_8_share_past_winning_ref_neighbors +
                        poverty_dtile_9_share_past_winning_ref_neighbors +
                        poverty_dtile_10_share_past_winning_ref_neighbors +
                        share_past_winning_ref_neighbors +
                        N_ref_past_districts_neighbors +
                        recent_ref +
                        rev_state_total +
                        rev_local_total +
                        exp_total +
                        enrollment_fall_responsible
                      | year_state + leaid | 0 | leaid, data = analysis_dat)

summary(out_neighbors)

out_neighbors <- felm(bond_instance ~ 
                        poverty_qtile_2_share_past_winning_ref_neighbors + 
                        poverty_qtile_3_share_past_winning_ref_neighbors + 
                        poverty_qtile_4_share_past_winning_ref_neighbors + 
                        poverty_qtile_5_share_past_winning_ref_neighbors + 
                        share_past_winning_ref_neighbors +
                        N_ref_past_districts_neighbors + 
                        recent_ref + 
                        rev_state_total +
                        rev_local_total + 
                        exp_total +
                        enrollment_fall_responsible
                      | year_state + leaid | 0 | leaid, data = analysis_dat)

summary(out_neighbors)



## Extract coefficients from the out_neighbors model
coef_neighbors <- as.data.table(summary(out_neighbors)$coefficients, keep.rownames = "term")
coef_neighbors <- coef_neighbors[term %like% "poverty_qtile_"]
coef_neighbors[, qt := 2:5]
coef_neighbors[, model := "Bordering"]
coef_neighbors[, ci_lower := Estimate - 1.96 * `Cluster s.e.`]
coef_neighbors[, ci_upper := Estimate + 1.96 * `Cluster s.e.`]


## Creating plots 
coef_all <- rbind(coef_cz, coef_neighbors)

hetero_plot <- ggplot(coef_all, aes(x = factor(qt), y = Estimate, color = model, shape = model)) +
  geom_point(position = position_dodge(width = 0.4), size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.2, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(x = "Poverty Quantile", y = "Coefficient Estimate",
       color = "Model", shape = "Model") +
  theme_minimal()

hetero_plot

ggsave(paste0("../figs/hetero_plot.png"),plot = hetero_plot, width = 8, height = 4, dpi = 300)
