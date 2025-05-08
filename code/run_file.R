# run file for peer districts scripts

# reads in raw Wisconsin referendum data and outputs seperate csv files for
# operational ref, capital ref, and all ref.
# i've already uploaded these cleaned csv files so you don't actually have to run this
source('./referenda_cleaning.R')

## Pull school finance data from Urban Institute
source("./load_finances.R")

# RD and probit just for Wisconsin ref
source('./wi_ref_analysis.R')

# RD for capital bonds from Biasi, La Fortune, Schonholzer (2024)
# the vote margin here is incorrect, need to change vote margin for states 
# in years with super majority requirements. Online appendix fig A2 has a map of thresholds
source('./cap_bonds_analysis.R')

# use commute zone definition of neighbors
source('./seda_cz_analysis.R')

# DON'T RUN!
# logit of propbaility of own proposal on share of neioghbors who passed in the last few years.
# written to run on slurm, still takes forever, defineilty needs to be written more efficiently. 
source(bond_logit)


# Next steps:
# 1. bin scatter RD. there is a binsreg package that does binscatter with covariates but i don't see a way to
# make it respect regression discontinuities, but I haven't looked that hard either.
# 2. probit of own prob of ref on share of neighbors and financials. 
# 3. Try a version of the rd and probit with all districts in the same commuting zone
# as "neighbors". SEDA has a crosswalk from school ID to commuting zone called
# "SEDA_crosswalk_5.0" in the ancillary data section: https://edopportunity.org/opportunity/data/downloads/