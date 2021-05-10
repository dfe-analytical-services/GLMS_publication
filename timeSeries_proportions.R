
#----Description of Script----
#This scripts takes the data the outputs for part-time workers and 
#puts it in the format needed for the new (Explore Education and Statistic Platform) EES  platform


#Contents-----
#1. the GLMS.prj outputs called NonGrad_yr_proportion.rds, grad_yr_proportion.rds and 
#     timeseries_pt_proportion are combined into one table and QAed
#2. The summary table is formatted so that it can be read by the platform
#3. A corresponding meta file is made for the summary table


#----What format for EES ?----
#a) .csv files. For each data file a meta data file is needed
#b) tidy data format
#c)UtF-8 file encoding


#----load library----
library("dplyr")
#library(plyr)
#library(lubridate)


#filepath <- "W:/HEA-SLR-TEAM/GLMS/GLMS_2020V1/"

#1.  ----upload part-time analyis outputs  ----
#The rds outputs from the GLMS program are uploaded from the shared area

NonGrad_yr_proportion<-readRDS(paste0(filepath, "Outputs_for_GLMS/EES_rds/NonGrad_yr_proportion_",start_year,"_",end_year,".rds"))

grad_yr_proportion <-readRDS(paste0(filepath, "Outputs_for_GLMS/EES_rds/grad_yr_proportion_",start_year,"_",end_year,".rds"))

timeseries_pt_proportion <- readRDS(paste0(filepath,"Outputs_for_GLMS/EES_rds/timeseries_pt_proportion_",start_year,"_",end_year,".rds"))

#1.1 ---Combine outputs------
#take part-time analysis data tables and combine into one data

pt_timeseries <- rbind(NonGrad_yr_proportion %>% 
                        select("time_period", "graduate_type", "Population", "Breakdown", "pt_employed_proportion" ),
                      grad_yr_proportion  %>% 
                        select("time_period", "graduate_type", "Population", "Breakdown", "pt_employed_proportion" ),
                      timeseries_pt_proportion %>% 
                        select("time_period", "graduate_type", "Population", "Breakdown", "pt_employed_proportion" ))

#1.3 ----QA:timeseries table------

#check data Binding
print("Are the number of rows retained when the data tables are joined ?")
NROW(pt_timeseries) == NROW(NonGrad_yr_proportion) + NROW(grad_yr_proportion) + NROW(timeseries_pt_proportion)
NROW(unique(pt_timeseries)) == NROW(unique(NonGrad_yr_proportion)) + NROW(unique(grad_yr_proportion)) + NROW(unique(timeseries_pt_proportion)) 
 
#Check for NA's and zeros
print("Are there any Na's or zeros as a result of the data joining in a table?")
print(apply(pt_timeseries, 2, function(x) any((x==0))))   #none
print(apply(pt_timeseries, 2, function(x) any(is.na(x)))) #none

#2. ----make summary table for EES platform----
# adapt summary table so that it can be read on the EES platform

#reformat AgeGroup
pt_timeseries$Population <- recode(pt_timeseries$Population,AgeGroup="Age group") 

#rename columns, add labeling columns and round up the proportion value
pt_timeseries%>%
  dplyr::rename(graduate_characteristic = "Population") %>%
  dplyr::rename(proportion_part_time_workers = "pt_employed_proportion")%>%
  mutate(proportion_part_time_workers = proportion_part_time_workers *100) %>%
  mutate(proportion_part_time_workers = formatC(proportion_part_time_workers, digits = 1, format = "f"))%>%
  mutate(graduate_breakdown = paste0(graduate_characteristic," ",Breakdown))%>%
  mutate(time_identifier="Calendar year",
         geographic_level="National",
         country_code="E92000001",
         country_name="England") %>%
         select("time_period",
                "time_identifier",
                "geographic_level",
                "country_code",
                "country_name",
                "graduate_type",
                "graduate_characteristic",
                "graduate_breakdown",
                "proportion_part_time_workers") -> pt_timeseries_a
#2.1 ----save final data file-------
write.csv(pt_timeseries_a,
          paste0(filepath, "Outputs_for_GLMS/EES_csv/part_time_working_proportions_",label_EES,".csv"),row.names = FALSE,quote = FALSE,fileEncoding = "UTF-8")

#3. -----Make meta file------

#Make a Meta data file for GradBreak_Employ_Demographics data file

pt_timeseries_a.meta <- data.frame("col_name" = c("proportion_part_time_workers",
                                                   "graduate_type",
                                                   "graduate_characteristic",
                                                   "graduate_breakdown"),
                                    "col_type" = c("Indicator",
                                                   "Filter",
                                                   "Filter",
                                                   "Filter"),
                                    "label" = c("Proportion Of Part Time Workers",
                                                "Graduate Type",
                                                "Graduate Characteristic",
                                                "Graduate Breakdown"),
                                    "indicator_grouping" = c("", "", "", ""),
                                    "indicator_unit" = c("%", "", "", ""),
                                    "indicator_dp"= c("1","", "",""),
                                    "filter_hint" = c("", "Select the graduate type you are interested in","Select the characteristic you are interested in","Select the breakdown you are interested in"),
                                    "filter_grouping_column"=c("","","",""))


#3.1 ----Save final meta file ----
write.csv(pt_timeseries_a.meta,
          paste0(filepath, "Outputs_for_GLMS/EES_csv/part_time_working_proportions_",label_EES,".meta.csv"),row.names = FALSE,quote = FALSE,fileEncoding = "UTF-8")




