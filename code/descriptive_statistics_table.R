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
library(educationdata)

##################
## Loading Data ##
##################
## Loading Referendum Data 
in_bonds = fread("C:/Users/rwmoo/OneDrive/Documents/Classes/Research/school_referenda/data/referendum/bonds_biasi.csv") # Bond Ref Data

## Commuting Zone Data 
in_cz=fread("C:/Users/rwmoo/OneDrive/Documents/Classes/Research/school_referenda/data/seda_crosswalk_5.0.csv")

## Loading Shape Files 
leaid_bounds=read_sf("C:/Users/rwmoo/OneDrive/Documents/Classes/Research/school_referenda/data/us_district_shapefile") # Loading overall shp files (is this correct to pull? I assume no)

## Loading district finance data 
district_finances = fread("C:/Users/rwmoo/OneDrive/Documents/Classes/Research/school_referenda/data/UI_district_finances.csv")

# load district charateristics
enrollment_cz= fread("C:/Users/rwmoo/OneDrive/Documents/Classes/Research/school_referenda/data/school_enrollment_cz.csv")
enrollment_cz[charter==1, dist_charter:=sum(enrollment, na.rm=T), by=c("year", "leaid", "charter")]
enrollment_cz[is.na(dist_charter), dist_charter:=max(dist_charter, na.rm=T), by=c("year", "leaid")]
enrollment_cz[free_or_reduced_price_lunch>=0, frpl:=sum(free_or_reduced_price_lunch, na.rm=T), by=c("year", "leaid")]
enrollment_cz[is.na(frpl), frpl:=max(frpl, na.rm=T), by=c("year", "leaid")]
enrollment_cz[, dist_enrollment:=sum(enrollment, na.rm=T), by=c("year", "leaid")]
enrollment_cz[dist_enrollment>0, charter_share:=dist_charter / dist_enrollment]
enrollment_cz[dist_enrollment>0, frpl_share:=frpl / dist_enrollment]
enrollment_cz[is.na(charter_share), charter_share:= 0]
enrollment_cz[is.na(frpl_share), frpl_share:= 0]

enrollment_cz[urban_centric_locale %in% c(6, 7, 8, 41, 42, 43), urbanicity := "rural"]
enrollment_cz[urban_centric_locale %in% c(1, 2, 11, 12, 13), urbanicity := "urban"]
enrollment_cz[urban_centric_locale %in% c(3, 4,5, 21, 22, 23), urbanicity := "suburban"]

enrollment_cz[urbanicity=="urban", dist_urban:=sum(enrollment, na.rm=T), by=c("year", "leaid", "urbanicity")]
enrollment_cz[urbanicity!="urban", dist_urban:=max(dist_urban, na.rm=T), by=c("year", "leaid")]

enrollment_cz[urbanicity=="suburban", dist_suburban:=sum(enrollment, na.rm=T), by=c("year", "leaid", "urbanicity")]
enrollment_cz[urbanicity!="suburban", dist_suburban:=max(dist_suburban, na.rm=T), by=c("year", "leaid")]

enrollment_cz[urbanicity=="rural", dist_rural:=sum(enrollment, na.rm=T), by=c("year", "leaid", "urbanicity")]
enrollment_cz[urbanicity!="rural", dist_rural:=max(dist_rural, na.rm=T), by=c("year", "leaid")]


enrollment_cz[dist_enrollment>0 & !is.na(dist_urban), share_urban:=dist_urban / dist_enrollment]
enrollment_cz[, share_urban:= max(share_urban, na.rm=T), by=c("leaid", "year")]
enrollment_cz[is.na(share_urban), share_urban:=0]

enrollment_cz[dist_enrollment>0 & !is.na(dist_suburban), share_suburban:=dist_suburban / dist_enrollment]
enrollment_cz[, share_suburban:= max(share_suburban, na.rm=T), by=c("leaid", "year")]
enrollment_cz[is.na(share_suburban), share_suburban:= 0]

enrollment_cz[dist_enrollment>0 & !is.na(dist_rural), share_rural:=dist_rural / dist_enrollment]
enrollment_cz[, share_rural:= max(share_rural, na.rm=T), by=c("leaid", "year")]
enrollment_cz[is.na(share_rural), share_rural:= 0]

enrollment_cz[leaid=="1700010" & year==2000]

# test proficiency #
proficiency=get_education_data(level = "school-districts",
                                     source = "edfacts",
                                     topic = "assessments",
                                     filters = list(year = 2010, grade_edfacts = 8))
test_scores=unique(subset(proficiency, select=c("leaid", "math_test_pct_prof_midpt", "read_test_pct_prof_midpt")))
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
analysis_dat[, year_state:= paste0(fips, year) ]
analysis_dat[enrollment_fall_responsible>0, per_pupil_exp := 100*exp_total / enrollment_fall_responsible]
analysis_dat[,rev_state_total := rev_state_total/1000000]
analysis_dat[,rev_local_total := rev_local_total/1000000]
analysis_dat[,exp_total := exp_total/1000000]
analysis_dat[,rev_total := rev_total/1000000]
analysis_dat[,salaries_total := salaries_total/1000000]

## Lagged Financials
analysis_dat[, c("rev_total_l", "exp_total_l", "enrollment_l") :=  shift(.SD), by=leaid, .SDcols=c("rev_total", "exp_total", "enrollment_fall_responsible")]
# percent change
analysis_dat[enrollment_l>0, change_enroll := (enrollment_fall_responsible - enrollment_l) / enrollment_l]
analysis_dat[rev_total_l>0, change_rev := (rev_total - rev_total_l)/ rev_total_l]
analysis_dat[exp_total_l>0, change_exp := (exp_total - exp_total_l)/ exp_total_l]

## Pulling the last 
analysis_dat[,first_ref := (bond_instance == 1)*is.na(year_of_last_ref)]
analysis_dat[,years_since_last_ref:= year - year_of_last_ref]
analysis_dat[,recent_ref := years_since_last_ref >= 3]
analysis_dat[is.na(recent_ref),recent_ref := 0]
analysis_dat[, total_ref := sum(bond_instance), by="leaid"]
analysis_dat[first_ref==1, year_of_first_ref := min(year, na.rm=T), by="leaid"]
analysis_dat[bond_instance==T, year_of_newest_ref := max(year, na.rm=T), by="leaid"]
analysis_dat[, use_window := year_of_newest_ref - year_of_first_ref]
analysis_dat[, cz_first_ref := min(year_of_first_ref, na.rm=T), by="cz_combined"]
analysis_dat[enrollment_fall_responsible>0, per_pupil_exp := 100 * exp_total / enrollment_fall_responsible]

dist_chars=unique(subset(enrollment_cz,leaid %in% seda_dist_cz_xwalk$leaid, select=c("year", "leaid", "charter_share", "frpl_share", "share_urban", "share_suburban", "share_rural")))

dist_chars$leaid=as.character(dist_chars$leaid)
analysis_dat=merge(dist_chars, analysis_dat, by=c('year', 'leaid'))
analysis_dat=merge(test_scores, analysis_dat, by=c( 'leaid'))


check = analysis_dat[, c('year', 'leaid', 'bond_instance', 'first_ref', 'year_of_last_ref')]
##### descriptives #####
recent_leader = unique(subset(analysis_dat, past_unique_ref_districts==0 & bond_instance==T, 
                       select=c("rev_total", "exp_total", "enrollment_fall_responsible", "per_pupil_exp",
                                "charter_share", "frpl_share", "total_ref", "share_urban", "share_suburban","share_rural", 
                                "use_window", "math_test_pct_prof_midpt", "read_test_pct_prof_midpt")))

never_user = unique(subset(analysis_dat, total_ref==0, 
                    select=c("rev_total", "exp_total", "enrollment_fall_responsible", "per_pupil_exp",
                             "charter_share", "frpl_share", "total_ref", "share_urban", "share_suburban",
                             "share_rural", "use_window", "math_test_pct_prof_midpt", "read_test_pct_prof_midpt")))

ever_user = unique(subset(analysis_dat, total_ref>0, 
                   select=c("rev_total", "exp_total", "enrollment_fall_responsible", "per_pupil_exp",
                            "charter_share", "frpl_share", "total_ref", "share_urban", "share_suburban", 
                            "share_rural", "use_window", "math_test_pct_prof_midpt", "read_test_pct_prof_midpt")))

first_mover = unique(subset(analysis_dat, year==cz_first_ref & bond_instance==T, 
                     select=c("rev_total", "exp_total", "enrollment_fall_responsible", "per_pupil_exp",
                              "charter_share", "frpl_share", "total_ref", "share_urban", "share_suburban","share_rural",
                              "use_window", "math_test_pct_prof_midpt", "read_test_pct_prof_midpt")))



types_list = list(recent_leader, never_user, ever_user, first_mover)
names(types_list) = c("recent_leader", "never_user", "ever_user", "first_mover")
table_dat = rbindlist(types_list, idcol = "type")
table(table_dat$type)


desc_stats= tableone::CreateTableOne(data=table_dat, strata = "type")
tab_csv <- print(desc_stats,
                 printToggle = FALSE,
                 nonnormal=c("rev_total", "exp_total", "enrollment_fall_responsible", "per_pupil_exp") )
write.csv(tab_csv,"C:/Users/rwmoo/OneDrive/Documents/Classes/Research/school_referenda/Graphs/descriptives.csv")



