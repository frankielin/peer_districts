######################
## Loading Packages ##
######################
library(data.table)
library(ggplot2)
library(binsreg)
library(dplyr)


##################
## Loading Data ##
##################

rm(list=ls())

## Commuting Zone Crosswalk
# in_cz=fread("C:/Users/rwmoo/OneDrive/Documents/Classes/Research/school_referenda/data/seda_crosswalk_5.0.csv")
in_cz=fread("../data/seda_crosswalk_5.0.csv")

## Referendum data 
# in_bonds=fread("C:/Users/rwmoo/OneDrive/Documents/Classes/Research/school_referenda/data/referendum/bonds_biasi.csv")
in_bonds=fread("../data/referendum/bonds_biasi.csv")

###################
## Cleaning Data ##
###################
# ## Creating a CZ-to-District Crosswalk
# seda_dist_cz_xwalk=unique(subset(in_cz,leatype="Regular public school district", select=c("leaid", "fips", "sedacz" )))
# seda_dist_cz_xwalk$flag=1
# seda_dist_cz_xwalk[, n_in_cz:=sum(flag), by=c("fips", "sedacz")] # fips is stats fips 
# cz=unique(seda_dist_cz_xwalk, by=c("fips", "sedacz"))
# summary(cz$n_in_cz) # Checking the number of districts in a commuting zone
# 
# ## Merging commting zones onto the 
# bonds_small=subset(in_bonds,!is.na(year), select=c("amount", "year", "leaid", "bond_use", "pass", "state", "votesyes", "totvotes", "votesharereqd"))
# bonds_cz=merge(bonds_small, cz, by="leaid") 
# print(paste("Ref lost in merge:", nrow(bonds_small) -nrow(bonds_cz))) # Losing some bonds through the merge (but very minor so who cares)
# 


## Creating LEAID school number to the dsitrict 
seda_dist_cz_xwalk=unique(subset(in_cz,leatype="Regular public school district", select=c("leaid", "fips", "sedacz")))
seda_dist_cz_xwalk[, n_in_cz:=uniqueN(leaid), by=c("fips", "sedacz")] 
seda_dist_cz_xwalk <- seda_dist_cz_xwalk[
  , .SD[which.max(n_in_cz)],
  by = leaid
][
  , .(leaid, fips, sedacz, n_in_cz)
]

print(nrow(seda_dist_cz_xwalk))
print(length(unique(seda_dist_cz_xwalk$leaid)))

print(nrow(in_bonds))
bonds_cz = subset(in_bonds,!is.na(year), select=c("amount", "year", "leaid", "bond_use", "pass", "state", "votesyes", "totvotes", "votesharereqd"))
print(nrow(bonds_cz)) # why the fuck is this duplicating upwards? lol fuck 

bonds_cz = merge(bonds_cz, seda_dist_cz_xwalk, by = 'leaid')
bonds_cz = bonds_cz[!is.na(n_in_cz)]


####################
## FL TESTING BIN ##
####################
# ## Creating new variables
bonds_cz[,id:=1:nrow(bonds_cz)]
tester = bonds_cz
# tester[,id:=1:nrow(bonds_cz)] # creating an ID key for the referendums

## Creating a subset datasetI need stat fips plus the seda cz as the id for the location
instance = tester[,c('leaid', 'year', 'sedacz', 'fips')]
setnames(instance, old = c("leaid", "year", "sedacz", 'fips'), new = c("leaid_instance", "year_instance", "sedacz_instance", 'fips_instance'))

## Fuzzy Joining on the date range
tester[,id:=1:nrow(bonds_cz)]
tester[,join_start_year := year+1]
tester[,join_end_year   := year+3]

print(nrow(tester))
check <- tester[
  instance,
  on = .(sedacz = sedacz_instance,
         fips = fips_instance,
         join_start_year <= year_instance,
         join_end_year >= year_instance),
  nomatch = NA,
  mult = "all"
]
print(nrow(check))
check <- check[leaid != leaid_instance] # Dropping if the current proposing district (for LOO style analysis), idk if we need this technically
print(nrow(check))
check <- check[, .(ref_future_districts = uniqueN(leaid_instance)), by = .(id)]

## Merging the districts proposing back onto the main bond instance dataframe
tester <- tester[
  check,
  on = .(id = id),
  nomatch = NA
]
tester[,share_ref_future_TESTER := ref_future_districts/n_in_cz]
tester = tester[,c('id', 'share_ref_future_TESTER')]
tester = tester[!is.na(id)]
setorder(tester, id)


##################


## Function to determine the share of individuals in the same CZ that submit a 
## referendum from -2 to current year
share_neighbor_ref_past=function(in_row){
  in_year=bonds_cz$year[in_row]
  n_neighbors=bonds_cz$n_in_cz[in_row]
  
  neighbor_refs_past=subset(bonds_cz, sedacz== bonds_cz$sedacz[in_row] &
                              fips== bonds_cz$fips[in_row] & 
                              year %in% (in_year-2):(in_year))
  
  
  n_past=nrow(unique(neighbor_refs_past, by="leaid"))
  
  share_past=n_past/n_neighbors
  
  return(share_past)
}

## Function to determine the share of individuals in the same CZ that submit a 
## referendum year plus one to year plus one to three
share_neighbor_ref_future=function(in_row){
  in_year=bonds_cz$year[in_row]
  n_neighbors=bonds_cz$n_in_cz[in_row]
  
  neighbor_refs_future=subset(bonds_cz, sedacz== bonds_cz$sedacz[in_row] &
                                fips== bonds_cz$fips[in_row] & 
                                year %in% (in_year+1):(in_year+3)) # checks in the year beyong 
  
  
  n_future=nrow(unique(neighbor_refs_future, by="leaid"))
  
  share_future=n_future/n_neighbors
  
  return(share_future)
}

## Applying to the funciton (TBH not sure how this works but let's compare it against my function)
# share_ref_future_list=lapply(1:nrow(bonds_cz), share_neighbor_ref_future)
# share_ref_past_list=lapply(1:nrow(bonds_cz), share_neighbor_ref_past)

## Adding columns 
# bonds_cz$share_ref_future=unlist(share_ref_future_list)
# bonds_cz$share_ref_past=unlist(share_ref_past_list)
bonds_cz[, vote_share:=votesyes/totvotes]
bonds_cz[is.na(votesharereqd), votesharereqd:=.5]
bonds_cz[, centered_vote_share:=vote_share-votesharereqd]

## Merge checking from my data 
bonds_cz <- merge(bonds_cz , tester, by = "id")
# bonds_cz$diff = abs(bonds_cz$share_ref_future-bonds_cz$share_ref_future_TESTER)
# summary(bonds_cz$diff)
# a = bonds_cz[diff>0]


##############
## Analysis ##
##############
## Defining the analysis data frame
analysis_dat = bonds_cz[!is.na(centered_vote_share)] # This cuts some observations that we should look at
analysis_dat = analysis_dat[abs(centered_vote_share)<0.49]
# analysis_dat = analysis_dat[!(state == "CA")]
# analysis_dat = analysis_dat[!(state == "TX")]
# analysis_dat = bonds_cz[abs(centered_vote_share) < 0.005]

## Creating variables for analysis dataframe
analysis_dat =analysis_dat[,winner:=(centered_vote_share>=0)]

####  Histogram of winning margins (For loop into a grid of states that we can look at)
states = unique(analysis_dat[,state])
states = c("ALL", states, "ALL_CORRECTED") # Adding an option for all states
FD_bandwidth <- function(x){out = 2 * IQR(x) / length(x)^(1/3)} # Function that produces "optimal" bins for histogram

## Loop and save through each state distinction
for (curr_state in states){
  if (curr_state == "ALL"){ # Keeping all states
    hist_dat = analysis_dat
  } else if (curr_state == "ALL_CORRECTED"){ # Keeping states that I manually select
    hist_dat = analysis_dat[!(state %in% c("KS", "MA", 'MD', 'MO', 'NE'))] # NEED TO STILL CHANGE THIS 
  } else {
    hist_dat = analysis_dat[state == curr_state]
  }
  
  ## Get the optimal bin width
  opt_width_hist <- FD_bandwidth(hist_dat$centered_vote_share)
  
  hist_plot <- 
    ggplot(hist_dat, aes(x = centered_vote_share)) +
    geom_histogram(binwidth = opt_width_hist, fill = "#2C77B8", color = "white", alpha = 0.6, boundary = 0) +
    geom_vline(xintercept = 0, color = "lightcoral", linetype = "solid", linewidth = 0.3) +
    labs(
      title = "Distribution of Voter Share (Centered)",
      x = "Vote Share (Centered)",
      y = "Count"
    ) +
    theme_minimal(base_size = 14)
  
  ggsave(paste0("../figs/vote_share_histogram_",curr_state,".png"),plot = hist_plot, width = 8, height = 6, dpi = 300)
}


#### Binscatter
## Loop and save through each state distinction
for (curr_state in states){
  if (curr_state == "ALL"){ # Keeping all states
    scatter_dat = analysis_dat
  } else if (curr_state == "ALL_CORRECTED"){ # Keeping states that I manually select
    scatter_dat = analysis_dat[!(state %in% c("KS", "MA", 'MD', 'MO', 'NE'))] # NEED TO STILL CHANGE THIS 
  } else {
    scatter_dat = analysis_dat[state == curr_state]
  }
  
  ## Creating data for bin scatter
  below <- scatter_dat[centered_vote_share < 0][order(centered_vote_share)]
  above <- scatter_dat[centered_vote_share > 0][order(centered_vote_share)]
  
  n_rows_below <- nrow(below)
  n_rows_above <- nrow(above)
  n_per_bin <- max(min(round(n_rows_above / 20), round(n_rows_below / 20)), 1)
  
  
  # Compute number of bins
  n_bins_below <- ceiling(n_rows_below / n_per_bin)
  n_bins_above <- ceiling(n_rows_above / n_per_bin)
  
  # Sort by distance from 0 (absolute value), then assign bin index
  below <- below[order(abs(centered_vote_share))]
  above <- above[order(abs(centered_vote_share))]
  
  # Assign bins: center-outward
  below[, bin := paste0("L", rep(1:n_bins_below, each = n_per_bin))[1:.N]]
  above[, bin := paste0("R", rep(1:n_bins_above, each = n_per_bin))[1:.N]]
  
  # Combine and aggregate
  binned_dat <- rbind(below, above)[
    , .(
      centered_vote_share = mean(centered_vote_share, na.rm = TRUE),
      share_ref_mean = mean(share_ref_future_TESTER, na.rm = TRUE),
      n = .N
    ),
    by = bin
  ][order(centered_vote_share)]
  
  ## Plot the bin scatter
  bin_plot <- 
    ggplot(binned_dat, aes(x = centered_vote_share, y = share_ref_mean)) +
    geom_point() +
    geom_vline(xintercept = 0, color = "lightcoral", linewidth = 0.3) +
    labs(
      x = "Vote Share (Centered)",
      y = "Probability",
      title = "Binscatter"
    ) +
    theme_minimal(base_size = 14) +
    stat_smooth(data=scatter_dat,
                aes(centered_vote_share, share_ref_future_TESTER, group = winner),
                method = "lm")
  
  ## Save Data
  ggsave(paste0("../figs/futher_vote_binscatter_",curr_state,".png"),plot = bin_plot, width = 8, height = 6, dpi = 300)
}

bin_plot
