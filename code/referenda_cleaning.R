library(data.table)
library(readxl)
library(stringr)
library(lubridate)
in_ref=as.data.table(read_xls("./data/referenda_data.xls"))
in_24_fall_ref=fread("./data/Fall Referenda 2024 - Referenda.csv")
in_24_spring_ref=fread("./data/Spring Referenda 2024 - Referenda.csv")
all_24_ref=rbind(in_24_fall_ref,in_24_spring_ref, fill=T)
all_24_ref$VoteDate=mdy(all_24_ref$VoteDate)
all_24_ref_small=subset(all_24_ref, select=c("AgencyCode", "AgencyName", "VoteDate", "Amount",
                                      "ReferendumTypeCode", "YesVotes", "NoVotes", "ReferendumStatusCode",
                                      "BriefDescription"))
in_ref$`Vote Date`=ymd(in_ref$`Vote Date`)
all_ref=rbind(in_ref, all_24_ref_small,use.names=FALSE)

all_ref[str_detect(str_to_lower(`Brief Description`), "remod|build new|building new|repair|construct|renovat|HVAC|roof|window|demolition|demolish|facilities|improvements|capital|new|classroom|gym|cafeteria|pool|athletic|ADA|library|electrical|energy|fixtures|equitment|add | field|maintain|update|replace|center| land"), capital_ref:=1]
capital_ref=subset(all_ref, capital_ref==1)
all_ref[str_detect(str_to_lower(`Brief Description`), "salary|retain|attract|retention|class size|operating|operational"), op_ref:=1]
all_ref[op_ref==1]
op_ref=subset(all_ref, op_ref==1)
write.csv(capital_ref, file = "./data/capital_ref.csv")
write.csv(op_ref, file = "./data/operational_ref.csv")
write.csv(all_ref, file = "./data/all_ref.csv",row.names = F)

