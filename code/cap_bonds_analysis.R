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

##################
## Loading Data ##
##################
## Bond referendum data
in_bonds=fread("../data/referendum/bonds_biasi.csv") # Bond Ref Data

## Shape files
# leaid_bounds=read_sf("../data/us_district_shapefile/schooldistrict_sy1718_tl18.shp") # There is no shp file in the folder?
leaid_bounds=read_sf("../data/referendum/us_district_shapefile") # Loading overall shp files (is this correct to pull? I assume no)

###################
## Cleaning Data ##
###################
## Creating variables
in_bonds[,leaid:=str_pad(leaid, 7, "left", "0")] # Creating an ID
in_bonds[, STATEFP:=substr(leaid, 1,2)]          # State Fips Code

## Filtering for states that are within the referendum data
keep_state_bounds=subset(leaid_bounds, STATEFP  %chin% unique(in_bonds$STATEFP)) 
rm(leaid_bounds)
gc()

## Pre-process border data
# split by state, find neighbors within state. this is mostly for computational
# ease, misses cross state border neighbors
state_bounds=split(keep_state_bounds, keep_state_bounds$STATEFP) # This is a list of dataframes 
sf_use_s2(FALSE)
state_dist_neighbors_list=lapply(state_bounds, st_touches) # I get an error here? Seems important too? 

for(i in 1:length(state_bounds)){
  state_bounds[[i]]$row_number=1:nrow(state_bounds[[i]]) 
}
ordered_state_bounds=rbindlist(state_bounds)

## Pre-processing referendum data 
in_bonds=subset(in_bonds, !is.na(leaid) & !is.na(year))
in_bonds$flag=1
in_bonds[, n_proposed_year:=sum(flag), by=c("leaid", "year")]
table(in_bonds$n_proposed_year)
# View(in_bonds[n_proposed_year>1]) # Checking multi-proposers (more than one ref a year)

in_bonds[, share_passed_year:=sum(pass)/n_proposed_year, by=c("leaid", "year")]
hist(in_bonds$share_passed_year) # Checking passage rates (fractions are multi-proposers)

uniform_ref=subset(in_bonds, share_passed_year %in% c(0,1)) # Creating a dataframe of referendum that all pass or don't
uniform_ref[, total_amount:=sum(as.numeric(amount_imputed), na.rm=T), by=c("leaid", "year")]
uniform_ref[, vote_share:=weighted.mean(pctyes, totvotes), by=c("leaid", "year")]

unique_ref=unique(subset(uniform_ref, select=c("leaid","STATEFP", "year", "total_amount", "vote_share", "votesharereqd")))

setnames(ordered_state_bounds, "GEOID", "leaid")

ref_w_bounds=merge(unique_ref, ordered_state_bounds, by= c("leaid", "STATEFP"), all.x = T)

## Function checks the number of neighboring districts that make a ref in the following year (from the current year)
share_neighbor_ref_future=function(in_year, in_dist_rownum, in_state){
  neighbor_ids=subset(state_bounds[[in_state]][state_dist_neighbors_list[[in_state]][[in_dist_rownum]],], select="GEOID")
  neighbor_refs_future=subset(unique_ref, leaid %chin% neighbor_ids$GEOID &
                                year %in% (in_year):(in_year+1))
  
  
  n_neighbors=nrow(neighbor_ids)
  n_future=nrow(unique(neighbor_refs_future, by="leaid"))
  share_future=ifelse(n_neighbors>0, n_future/n_neighbors, 0)
  return(share_future)
}

## Function checks the number of neighboring districts that make a ref in the past 3 years (to the current year)
share_neighbor_ref_past=function(in_year, in_dist_rownum, in_state){
  neighbor_ids=subset(state_bounds[[in_state]][state_dist_neighbors_list[[in_state]][[in_dist_rownum]],], select="GEOID")
  neighbor_refs_past=subset(unique_ref, leaid %chin% neighbor_ids$GEOID & year %in% (in_year-3):(in_year))
  
  n_neighbors=nrow(neighbor_ids)
  n_past=nrow(unique(neighbor_refs_past, by="leaid"))
  
  share_past=n_past/n_neighbors
  
  return(share_past)
}

share_ref_future_list=lapply(1:nrow(ref_w_bounds), 
                             function(x) share_neighbor_ref_future(ref_w_bounds$year[x], ref_w_bounds$row_number[x], ref_w_bounds$STATEFP[x]))

share_ref_past_list=lapply(1:nrow(ref_w_bounds), 
                           function(x) share_neighbor_ref_past(ref_w_bounds$year[x], ref_w_bounds$row_number[x], ref_w_bounds$STATEFP[x]))

ref_w_bounds$share_ref_future=unlist(share_ref_future_list)
ref_w_bounds$share_ref_past=unlist(share_ref_past_list)

ref_w_bounds[, scaled_share:=vote_share-votesharereqd]
ref_w_bounds[, pass_flag:=ifelse(vote_share>votesharereqd, 1,0)]


ref_w_bounds[,tri_weight:=ifelse(abs(scaled_share)<.1, abs(scaled_share)/.1, 0 )]

#################
## Regressions ##
#################
## Quick RD
rd=lm(share_ref_future ~ pass_flag + scaled_share + pass_flag:scaled_share + share_ref_past, data=ref_w_bounds)
summary(rd)

## "Binscattering" along the victory share
ref_w_bounds[scaled_share<0, quantile_bin:= ntile(scaled_share,n=10)]
ref_w_bounds[scaled_share>=0, quantile_bin:= ntile(scaled_share,n=10)+10]
ref_w_bounds[, bin_mean_ref:=mean(share_ref_future), by=quantile_bin]

plot_data=unique(subset(ref_w_bounds, select=c("quantile_bin", "bin_mean_ref")))
ggplot(plot_data, aes(x=quantile_bin, y=bin_mean_ref))+ geom_point()+geom_vline(xintercept = 11)
