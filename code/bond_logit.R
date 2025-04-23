install.packages("sf")
library(sf)
library(data.table)
library(ggplot2)
library(stringr)
library(tidyr)
library(foreach)
library(doParallel)

in_bonds=fread("~/school_ref/bonds_biasi.csv")
leaid_bounds=read_sf("~/school_ref/us_district_shapefile/schooldistrict_sy1718_tl18.shp")

in_bonds[,leaid:=str_pad(leaid, 7, "left", "0")]
in_bonds[, STATEFP:=substr(leaid, 1,2)]

keep_state_bounds=subset(leaid_bounds, STATEFP  %chin% unique(in_bonds$STATEFP))
rm(leaid_bounds)
gc()

#nrow(in_bonds)/nrow(expand.grid(keep_state_bounds$GEOID, in_bonds$year))
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
in_bonds=subset(in_bonds, !is.na(leaid) & !is.na(year))
in_bonds$flag=1
in_bonds[, n_proposed_year:=sum(flag), by=c("leaid", "year")]
in_bonds[, share_passed_year:=sum(pass)/n_proposed_year, by=c("leaid", "year")]
uniform_ref=subset(in_bonds, share_passed_year %in% c(0,1))
uniform_ref[, total_amount:=sum(as.numeric(amount_imputed), na.rm=T), by=c("leaid", "year")]
uniform_ref[, vote_share:=weighted.mean(pctyes, totvotes), by=c("leaid", "year")]

unique_ref=unique(subset(uniform_ref, select=c("leaid","STATEFP", "year", "total_amount", "vote_share")))
gc()

share_neighbor_ref_past=function(in_year, in_dist_rownum, in_state){
  neighbor_ids=subset(state_bounds[[in_state]][state_dist_neighbors_list[[in_state]][[in_dist_rownum]],], select="GEOID")
  neighbor_refs_past=subset(unique_ref, leaid %chin% neighbor_ids$GEOID & year %in% (as.numeric(in_year)-3):as.numeric(in_year))
  
  n_neighbors=nrow(neighbor_ids)
  n_past=nrow(unique(neighbor_refs_past, by="leaid"))
  
  share_past=n_past/n_neighbors
  
  return(share_past)
}


# logit prob 
all_dist_years=expand.grid(na.omit(keep_state_bounds$GEOID), na.omit(in_bonds$year))
colnames(all_dist_years)=c("leaid", "year")
gc()
all_dist_years$dist_year=paste0(all_dist_years$leaid, all_dist_years$year)
dist_year_propose=unique(subset(in_bonds,!is.na(year) & !is.na(leaid) & 
                                  leaid %in% keep_state_bounds$GEOID, 
                                select=c("leaid", "year", "flag")))
dist_year_propose$dist_year=paste0(dist_year_propose$leaid, dist_year_propose$year)

all_dist_years$flag=ifelse(all_dist_years$dist_year %chin% dist_year_propose$dist_year,1,0)

all_dist_years$leaid <- factor(all_dist_years$leaid)
all_dist_years$STATEFP=substr(all_dist_years$leaid,1,2)

dist_rownum_xwalk=data.table(subset(ordered_state_bounds, select=c("row_number", "STATEFP", "GEOID")))
setnames(dist_rownum_xwalk, "GEOID", "leaid")
dist_years_numbered=merge(all_dist_years,dist_rownum_xwalk, by=c("STATEFP", "leaid"))


share_all_dist_ref_past_list=lapply(1:nrow(dist_years_numbered),function(i) share_neighbor_ref_past(dist_years_numbered$year[i], dist_years_numbered$row_number[i],
                          dist_years_numbered$STATEFP[i]))

dist_years_numbered$share_ref_past=unlist(share_all_dist_ref_past_list)

write.table(dist_years_numbered, "~/school_ref/logit_data.txt", colnames=F)

logit <- glm(flag ~ year + share_ref_past, data = dist_years_numbered, family = "binomial")
summary(logit)
