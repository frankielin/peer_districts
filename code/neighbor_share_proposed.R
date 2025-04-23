library(sf)
library(stringr)
library(ggplot2)
library(tidyr)
library(data.table)

in_district_bounds=read_sf("/Classes/Research/school_referenda/data/Wisconsin_School_Districts_shapefiles/WI_School_Districts.shp")
in_district_bounds$row_number=1:nrow(in_district_bounds)
# for each district, find the name and ID of all districts it touches
#touching=st_touches(in_district_bounds)
# for each district, find the name and ID of all districts within 20km (20,000 meters)
touching=st_intersects(st_buffer(in_district_bounds, dist = 20000), in_district_bounds)

# Build data of each referendum and the number of neighboring districts that passed a ref
# in the 3 years before and 3 years after
all_ref=fread("/Classes/Research/school_referenda/data/referendum/all_ref.csv")
all_ref$SDID=str_pad(all_ref$District, width = 4, side = "left",pad = "0")
all_ref=subset(all_ref, !is.na(District))
all_ref[,year:=year(`Vote Date`)]
all_ref$flag=1
all_ref[, n_proposed_year:=sum(flag), by=c("District", "year")]
table(all_ref$n_proposed_year)
# district can have several ref in a year, some pass and some fail
View(all_ref[n_proposed_year>1])

all_ref[, pass_flag:=ifelse(Result=="Passed", 1, 0)]
all_ref[, share_passed_year:=sum(pass_flag)/n_proposed_year, by=c("District", "year")]
# most mass is on 1 and zero, for now just keep these and treat as one big ref
# should probably change this to keep all ref but we should discuss how to treat vote share
hist(all_ref$share_passed_year)

uniform_ref=subset(all_ref, share_passed_year %in% c(0,1))
uniform_ref[, yes_votes:=sum(Yes), by=c("District", "year")]
uniform_ref[, no_votes:=sum(No), by=c("District", "year")]
uniform_ref[, total_amount:=sum(as.numeric(`Total Amount`), na.rm=T), by=c("District", "year")]
uniform_ref[, vote_share:=yes_votes/(yes_votes+no_votes)]

unique_ref=unique(subset(uniform_ref,!is.na(year), select=c("SDID", "year", "total_amount", "vote_share")))

ref_w_bounds=merge(unique_ref, in_district_bounds, by="SDID", all.x = T)


share_neighbor_ref_future=function(in_year, in_dist_rownum){
  neighbor_ids=subset(in_district_bounds[touching[[in_dist_rownum]],], select="SDID")
  neighbor_refs_past=subset(unique_ref, SDID %chin% neighbor_ids$SDID & year %in% (in_year-3):(in_year))

  # this is removing neighbors who already passed a ref in the past 3 years from both the 
  # numerator and denominator when calculating the share. 
  neighbor_refs_future=subset(unique_ref, SDID %chin% neighbor_ids$SDID &
                                !(SDID %chin% neighbor_refs_past$SDID) & 
                                year %in% (in_year+1):(in_year+3))

  
  n_neighbors=nrow(neighbor_ids)-length(unique(neighbor_refs_past$SDID))
  n_future=nrow(unique(neighbor_refs_future, by="SDID"))
  share_future=ifelse(n_neighbors>0, n_future/n_neighbors, 0)
  return(share_future)
}

share_neighbor_ref_past=function(in_year, in_dist_rownum){
  neighbor_ids=subset(in_district_bounds[touching[[in_dist_rownum]],], select="SDID")
  neighbor_refs_past=subset(unique_ref, SDID %chin% neighbor_ids$SDID & year %in% (in_year-3):(in_year))
  
  n_neighbors=nrow(neighbor_ids)
  n_past=nrow(unique(neighbor_refs_past, by="SDID"))
  
  share_past=n_past/n_neighbors
  
  return(share_past)
}

share_ref_future_list=lapply(1:nrow(ref_w_bounds), 
 function(x) share_neighbor_ref_future(ref_w_bounds$year[x], ref_w_bounds$row_number[x]))

share_ref_past_list=lapply(1:nrow(ref_w_bounds), 
  function(x) share_neighbor_ref_past(ref_w_bounds$year[x], ref_w_bounds$row_number[x]))

ref_w_bounds$share_ref_future=unlist(share_ref_future_list)
ref_w_bounds$share_ref_past=unlist(share_ref_past_list)
hist(ref_w_bounds$share_ref_past)
ref_w_bounds[, scaled_share:=vote_share-.5]
ref_w_bounds[, pass_flag:=ifelse(vote_share>.5, 1,0)]

rd=lm(share_ref_future ~ pass_flag * scaled_share + share_ref_past , data=ref_w_bounds)

summary(rd)


# logit prob 
all_dist_years=expand.grid(in_district_bounds$SDID, 1990:2024)
colnames(all_dist_years)=c("SDID", "year")
dist_year_propose=unique(subset(all_ref,!is.na(year), select=c("SDID", "year", "flag")))

all_dist_years_ref=merge(all_dist_years, dist_year_propose, by=c("SDID", "year"), all=T)
all_dist_years_ref$flag=ifelse(is.na(all_dist_years_ref$flag), 0, 1)

all_dist_years_ref$SDID <- factor(all_dist_years_ref$SDID)
all_dist_years_ref_bounds=merge(all_dist_years_ref, in_district_bounds, by="SDID", all.x = T)

share_all_dist_ref_past_list=lapply(1:nrow(all_dist_years_ref_bounds), 
                           function(x) share_neighbor_ref_past(all_dist_years_ref_bounds$year[x], all_dist_years_ref_bounds$row_number[x]))

all_dist_years_ref_bounds$share_ref_past=unlist(share_all_dist_ref_past_list)

logit <- glm(flag ~ year + share_ref_past, data = all_dist_years_ref_bounds, family = "binomial")
summary(logit)
