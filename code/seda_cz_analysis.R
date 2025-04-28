library(data.table)
library(ggplot2)
library(binsreg)
library(dplyr)

in_cz=fread("C:/Users/rwmoo/OneDrive/Documents/Classes/Research/school_referenda/data/seda_crosswalk_5.0.csv")
in_bonds=fread("C:/Users/rwmoo/OneDrive/Documents/Classes/Research/school_referenda/data/referendum/bonds_biasi.csv")

seda_dist_cz_xwalk=unique(subset(in_cz,leatype="Regular public school district", select=c("leaid", "fips", "sedacz" )))
seda_dist_cz_xwalk$flag=1
seda_dist_cz_xwalk[, n_in_cz:=sum(flag), by=c("fips", "sedacz")]
cz=unique(seda_dist_cz_xwalk, by=c("fips", "sedacz"))
summary(cz$n_in_cz)

bonds_small=subset(in_bonds,!is.na(year), select=c("amount", "year", "leaid", "bond_use", "pass", "state", "votesyes", "totvotes", "votesharereqd"))

# losing some bonds
bonds_cz=merge(bonds_small, seda_dist_cz_xwalk, by="leaid")


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


share_neighbor_ref_future=function(in_row){
  in_year=bonds_cz$year[in_row]
  n_neighbors=bonds_cz$n_in_cz[in_row]
  
  neighbor_refs_future=subset(bonds_cz, sedacz== bonds_cz$sedacz[in_row] &
                              fips== bonds_cz$fips[in_row] & 
                              year %in% (in_year+1):(in_year+3))
  
  
  n_future=nrow(unique(neighbor_refs_future, by="leaid"))
  
  share_future=n_future/n_neighbors
  
  return(share_future)
}

share_ref_future_list=lapply(1:nrow(bonds_cz),  share_neighbor_ref_future)

share_ref_past_list=lapply(1:nrow(bonds_cz),share_neighbor_ref_past)

bonds_cz$share_ref_future=unlist(share_ref_future_list)
bonds_cz$share_ref_past=unlist(share_ref_past_list)
bonds_cz[, vote_share:=votesyes/totvotes]
bonds_cz[is.na(votesharereqd), votesharereqd:=.5]

bonds_cz[, centered_vote_share:=vote_share-votesharereqd]

ggplot(bonds_cz, aes(x=centered_vote_share, y=share_ref_future))+geom_point()

rd=lm(share_ref_future ~ pass + centered_vote_share + pass:centered_vote_share + share_ref_past, data=bonds_cz)

summary(rd)

bonds_cz[centered_vote_share<0, quantile_bin:= ntile(centered_vote_share,n=10)]
bonds_cz[centered_vote_share>=0, quantile_bin:= ntile(centered_vote_share,n=10)+10]
bonds_cz[, bin_mean_ref:=mean(share_ref_future), by=quantile_bin]

plot_data=unique(subset(bonds_cz, select=c("quantile_bin", "bin_mean_ref")))
ggplot(plot_data, aes(x=quantile_bin, y=bin_mean_ref))+ geom_point()+geom_vline(xintercept = 11)
