library(sf)
library(data.table)
library(stringr)
library(ggplot2)

in_bonds=fread("Classes/Research/school_referenda/github/peer_districts/data/referendum/bonds_biasi.csv")
leaid_bounds=read_sf("Classes/Research/school_referenda/github/peer_districts/data/us_district_shapefile/schooldistrict_sy1718_tl18.shp")

in_bonds[,leaid:=str_pad(leaid, 7, "left", "0")]
in_bonds[, STATEFP:=substr(leaid, 1,2)]
keep_state_bounds=subset(leaid_bounds, STATEFP  %chin% unique(in_bonds$STATEFP))
rm(leaid_bounds)
gc()

sf_use_s2(FALSE)
# split by state, find neighbors within state. this is mostly for computational
# ease, misses cross state border neighbors
state_bounds=split(keep_state_bounds, keep_state_bounds$STATEFP)


pad_and_intersect=function(in_data){
  tryCatch({
  sf_use_s2(T)
  padded_data=st_buffer(in_data,200)
  sf_use_s2(F)
  return(st_intersects(padded_data))},
  error=function(msg){
    sf_use_s2(F)
    return(st_touches(in_data))}
  )
}


state_dist_neighbors_list=lapply(state_bounds, pad_and_intersect)

for(i in 1:length(state_bounds)){
  state_bounds[[i]]$row_number=1:nrow(state_bounds[[i]])
}
ordered_state_bounds=rbindlist(state_bounds)


single_dist_neighbors=function(in_leaid, in_dist_rownum, in_state){

  neighbor_ids=subset(state_bounds[[in_state]][state_dist_neighbors_list[[in_state]][[in_dist_rownum]],], select="GEOID")
  n_neighbors=nrow(neighbor_ids)
  out_mat=data.frame(matrix(data=c(rep(in_leaid, n_neighbors), neighbor_ids$GEOID),nrow = n_neighbors, ncol=2, byrow = F))

  return(out_mat)
}

all_neighbors_list=lapply(1:nrow(ordered_state_bounds), 
function(x) single_dist_neighbors(ordered_state_bounds$GEOID[x], ordered_state_bounds$row_number[x], ordered_state_bounds$STATEFP[x]))

all_neighbors=rbindlist(all_neighbors_list)
colnames(all_neighbors)=c("leaid", "neighbor_leaid")
write.csv(all_neighbors, "Classes/Research/school_referenda/github/peer_districts/data/touching_districts.csv",row.names = F)


