##########
######################
## Loading Packages ##
######################
library(data.table)
library(ggplot2)
library(stringr)
library(lfe)
library(stargazer)

##################
## Loading Data ##
##################
## Loading Referendum Data 
in_bonds = fread("C:/Users/rwmoo/OneDrive/Documents/Classes/Research/school_referenda/data/referendum/bonds_biasi.csv") # Bond Ref Data

enrollment_cz= fread("C:/Users/rwmoo/OneDrive/Documents/Classes/Research/school_referenda/data/school_enrollment_cz.csv")
# calculate the share of students enrolled in a charter school at the CZ level
enrollment_cz[, cz_total_enrollment:=sum(enrollment, na.rm=T), by=c("sedacz", "fips","year")]
enrollment_cz[, cz_type_enrollment:=sum(enrollment, na.rm=T), by=c("charter", "sedacz", "fips", "year")]
enrollment_cz[, cz_charter_enrollment:= ifelse(charter==1,cz_type_enrollment, NA )]
enrollment_cz[, cz_charter_enrollment:= ifelse(any(!is.na(cz_charter_enrollment)), 
                                               max(cz_charter_enrollment, na.rm=T), 0 ),
                                               by=c("sedacz", "fips", "year")]
enrollment_cz[, share_charter:=cz_charter_enrollment/cz_total_enrollment]
# save data at the CZ-year level
cz_share_charter=unique(subset(enrollment_cz,  select=c("sedacz", "fips", "share_charter", "year")))

in_cz=fread("C:/Users/rwmoo/OneDrive/Documents/Classes/Research/school_referenda/data/seda_crosswalk_5.0.csv")

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

## Creating summary table of the bonds per cz-year
summary_bonds = in_bonds[,.(
  bond_count    = uniqueN(id),
  bond_instance = uniqueN(id)>=1
), 
by = .(sedacz,fips, year)
]


#### Charter enrollment deciles
## Calculating different quantiles of charter enrollment

cz_share_charter$fips=as.character(cz_share_charter$fips)
cz_charter_bonds=merge(cz_share_charter, summary_bonds, by=c("sedacz", "fips", "year"))
cz_charter_bonds[, cz_id:=paste0(fips, sedacz)]

n_in_cz=unique(subset(seda_dist_cz_xwalk, select=c("fips", "sedacz", "n_in_cz")))
cz_charter_bonds=merge(cz_charter_bonds, n_in_cz, by=c("sedacz", "fips"))

# number of bonds in the cz divided by the number of districts in the cz, average bonds per district in each year
cz_charter_bonds[, bonds_per_dist := bond_count/ n_in_cz]
cz_charter_bonds[share_charter>0, dev_share_charter:=(share_charter-mean(share_charter, na.rm=T))/sd(share_charter), by="sedacz"]
cz_charter_bonds[, sedacz:=paste0(fips, sedacz)]
# do CZs with more charter students have less ref? Without FEs, yes. with FEs all null
test = felm(log(bonds_per_dist) ~ 
              share_charter  | sedacz + year| 0 | sedacz   , data=cz_charter_bonds)
summary(test)

# District level analysis
# do districts with more charter students respond less to neighbors ref?

# keep districts that are in the seda_cz_xwalk
dist_charter_enroll= unique(subset(enrollment_cz, leaid %chin% seda_dist_cz_xwalk$leaid, 
                                   select=c("year", "leaid", "fips", "share_charter", "sedacz",  'enrollment')))
# assign qtile score based on CZ SHARE CHARTER
dist_charter_enroll[, share_charter_qtile := cut(share_charter,
                                           breaks = quantile(share_charter, probs = seq(0, 1, 0.2), na.rm = TRUE),
                                           include.lowest = TRUE,
                                           labels = FALSE)]

dist_summary_bonds = in_bonds[,.(
  bond_count    = uniqueN(id),
  bond_instance = uniqueN(id)>=1,
  bond_pass     = sum(pass)
), 
by = .(leaid, year)
]

set_year_past = 3

left = dist_charter_enroll[,c('leaid','fips','sedacz','year')]
left[,max_year:= year-1]
left[,min_year:= year-set_year_past]
instances = in_bonds[,c('leaid','year','fips','sedacz')]
setnames(instances, 
         c('leaid','year','fips','sedacz'), 
         paste0("instance_", c('leaid','year','fips','sedacz')))

print(nrow(left))
left$fips=as.character(left$fips)
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

print(nrow(dist_charter_enroll))
dist_charter_enroll = N_past_refs_dat[
  dist_charter_enroll,
  on = .(leaid = leaid, year = year),
  nomatch = NA
] # Merging back onto the main dataframe
sum(dist_summary_bonds[leaid %in% dist_charter_enroll$leaid & year>=2000]$bond_instance)

dist_charter_enroll$leaid=as.character(dist_charter_enroll$leaid)
dist_charter_enroll = dist_summary_bonds[
  dist_charter_enroll,
  on = .(leaid = leaid, year = year),
  nomatch = NA
] 
table(dist_charter_enroll$share_charter_qtile)

dist_charter_enroll[, n_in_cz:=uniqueN(leaid), by=c("fips", "sedacz")] 
dist_charter_enroll[, share_past_ref:=ifelse(!is.na(past_unique_ref_districts), 
                                             past_unique_ref_districts/n_in_cz, 0)]
dist_charter_enroll[, share_pass_ref:=ifelse(!is.na(past_unique_ref_districts), 
                                             bond_pass/n_in_cz, 0)]

dist_charter_enroll[is.na(bond_instance), bond_instance:=FALSE]
dist_charter_enroll[, charter_share_qtile_1_share_past_ref := (share_charter_qtile == 1) * share_past_ref]
dist_charter_enroll[, charter_share_qtile_2_share_past_ref := (share_charter_qtile == 2) * share_past_ref]
dist_charter_enroll[, charter_share_qtile_3_share_past_ref := (share_charter_qtile == 3) * share_past_ref]
dist_charter_enroll[, charter_share_qtile_4_share_past_ref := (share_charter_qtile == 4) * share_past_ref ]
dist_charter_enroll[, charter_share_qtile_5_share_past_ref := (share_charter_qtile == 5) * share_past_ref]

dist_charter_enroll[, year_state:=paste0(year, fips)]
dist_charter_enroll[, share_charter_qtile:= as.factor(share_charter_qtile)]
out_cz <- felm(bond_instance ~ 
                 charter_share_qtile_2_share_past_ref +
                 charter_share_qtile_3_share_past_ref +
                 charter_share_qtile_4_share_past_ref +
                 charter_share_qtile_5_share_past_ref +
                 enrollment| year_state + leaid | 0 | leaid, data = dist_charter_enroll)

summary(out_cz)
# no significant difference in responsive to neighbor ref based on CZ charter enrollment, but suggestive downward trend

level_cz <- felm(bond_instance ~ 
                share_charter_qtile +
                share_past_ref +
                enrollment| year_state + leaid | 0 | leaid, data = dist_charter_enroll)

summary(level_cz)
# no significant difference in the average likelihood of proposing a bond and no clear trend

out_continuous_share <- felm(bond_instance ~ share_charter +
                 share_past_ref +
                 enrollment| year_state + leaid | 0 | leaid, data = dist_charter_enroll)

summary(out_continuous_share)
# when left as a continuous variable the share of the CZ that is enrolled in a charter is negative and marginally significant
# for predicting a district's probability of proposing a bond


coef_neighbors <- as.data.table(summary(out_cz)$coefficients, keep.rownames = "term")
coef_neighbors <- coef_neighbors[term %like% "_qtile"]
coef_neighbors[, qt := 2:5]
coef_neighbors[, model := "CZ"]
coef_neighbors[, ci_lower := Estimate - 1.96 * `Cluster s.e.`]
coef_neighbors[, ci_upper := Estimate + 1.96 * `Cluster s.e.`]


## Creating plots 

hetero_plot <- ggplot(coef_neighbors, aes(x = factor(qt), y = Estimate, color = model, shape = model)) +
  geom_point(position = position_dodge(width = 0.4), size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.2, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(x = "Share Charter Enrollment Quantile", y = "Coefficient Estimate",
       color = "Model", shape = "Model") +
  theme_minimal()

hetero_plot
