library(educationdata)
library(data.table)

school_enrollment=get_education_data(level = "schools",
                                     source = "ccd",
                                     topic = "directory",
                                     filters = list(year = 2000:2019))

in_cz=fread("C:/Users/rwmoo/OneDrive/Documents/Classes/Research/school_referenda/data/seda_crosswalk_5.0.csv")

cz_xwalk=unique(subset(in_cz, select=c("ncessch", "sedacz",  "sedametro")))
cz_xwalk$ncessch=str_pad(cz_xwalk$ncessch, width = 12, side = "left", pad="0")
enrollment_cz=data.table(merge(school_enrollment, cz_xwalk, by="ncessch"))

fwrite(enrollment_cz, "C:/Users/rwmoo/OneDrive/Documents/Classes/Research/school_referenda/data/school_enrollment_cz.csv")
