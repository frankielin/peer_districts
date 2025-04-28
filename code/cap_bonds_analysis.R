library(sf)
library(data.table)
library(ggplot2)
library(stringr)
library(tidyr)
library(foreach)
library(doParallel)

in_bonds=fread("Classes/Research/school_referenda/data/referendum/bonds_biasi.csv")
leaid_bounds=read_sf("Classes/Research/school_referenda/data/us_district_shapefile/schooldistrict_sy1718_tl18.shp")

in_bonds[,leaid:=str_pad(leaid, 7, "left", "0")]
in_bonds[, STATEFP:=substr(leaid, 1,2)]

keep_state_bounds=subset(leaid_bounds, STATEFP  %chin% unique(in_bonds$STATEFP))
rm(leaid_bounds)
gc()

# split by state, find neighbors within state. this is mostly for computational
# ease, misses cross state border neighbors
state_bounds=split(keep_state_bounds, keep_state_bounds$STATEFP)
sf_use_s2(FALSE)
state_dist_neighbors_list=lapply(state_bounds, st_touches)

for(i in 1:length(state_bounds)){
  state_bounds[[i]]$row_number=1:nrow(state_bounds[[i]])
}
ordered_state_bounds=rbindlist(state_bounds)

in_bonds=subset(in_bonds, !is.na(leaid) & !is.na(year))
in_bonds$flag=1
in_bonds[, n_proposed_year:=sum(flag), by=c("leaid", "year")]
table(in_bonds$n_proposed_year)
# district can have several ref in a year, some pass and some fail
View(in_bonds[n_proposed_year>1])

in_bonds[, share_passed_year:=sum(pass)/n_proposed_year, by=c("leaid", "year")]
# most mass is on 1 and zero, for now just keep these and treat as one big ref
hist(in_bonds$share_passed_year)

uniform_ref=subset(in_bonds, share_passed_year %in% c(0,1))
uniform_ref[, total_amount:=sum(as.numeric(amount_imputed), na.rm=T), by=c("leaid", "year")]
uniform_ref[, vote_share:=weighted.mean(pctyes, totvotes), by=c("leaid", "year")]

unique_ref=unique(subset(uniform_ref, select=c("leaid","STATEFP", "year", "total_amount", "vote_share", "votesharereqd")))

setnames(ordered_state_bounds, "GEOID", "leaid")

ref_w_bounds=merge(unique_ref, ordered_state_bounds, by= c("leaid", "STATEFP"), all.x = T)


share_neighbor_ref_future=function(in_year, in_dist_rownum, in_state){
  neighbor_ids=subset(state_bounds[[in_state]][state_dist_neighbors_list[[in_state]][[in_dist_rownum]],], select="GEOID")
  neighbor_refs_future=subset(unique_ref, leaid %chin% neighbor_ids$GEOID &
                                year %in% (in_year+1):(in_year+3))
  
  
  n_neighbors=nrow(neighbor_ids)
  n_future=nrow(unique(neighbor_refs_future, by="leaid"))
  share_future=ifelse(n_neighbors>0, n_future/n_neighbors, 0)
  return(share_future)
}

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
rd=lm(share_ref_future ~ pass_flag + scaled_share + pass_flag:scaled_share + share_ref_past, data=ref_w_bounds)

summary(rd)

