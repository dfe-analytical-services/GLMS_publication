#----Introduction----- 
#The GLMS supporting data with worksheet title: 
#  Quarterly Time Series Data 
#has been generated from Emp_confidence_Intervals.csv   

#----Description of Script----
#This scripts takes the data in Emp_confidence_Intervals.csv
#and puts it in the format needed for the new DfE EES platform.

#Specifically:
#(i) Employment Rates for the Emp_confidence_Intervals is found in
#    one data table and has a corresponding meta file

#----What format is needs ?----
#a) .csv files. For each data file a meta data file is needed
#b) tidy data format
#c)UtF-8 file encoding


#----load library-------
library("dplyr")
library(plyr)
library(lubridate)

#----Upload data ----
#The outputs from the GLMS program are uploaded from the shared area

Emp_Confidence_interval<-readRDS(paste0(filepath, "Outputs_folder/Emp_confidence_Intervals_",start_year,"_",end_year,".rds"))

#----data file: Quarterly Time Series (Employment Rates)-----

#Select the variables needed
Emp_Confidence_interval_a<-Emp_Confidence_interval%>%
  select("population",         # whether Graduate,PostGrad,Non Grad)
         "year",              # i.e 2007 - 2019
         "Quarter",           # Q1 - Q2
         "Emp_prop",          #Employment Rate
         "Emp_prop_lower",    # Employment Rate Lower Bound  
         "Emp_prop_upper",    # Employment Rate Upper Bound
         "HSEmp_prop",        #High-Skill Employment Rate
         "HSEmp_prop_lower",  #High-Skill Employment Rate Lower Bound
         "HSEmp_prop_upper",  # High-Skill Employment Rate Upper Bound
         "Unemp_prop",        #Unemployment Rate
         "Unemp_prop_lower",  #Unemployment Rate Lower Bound  
         "Unemp_prop_upper", #Unemployment Rate Upper Bound
         "age", #either 16-64 or 21-30 
         "ILO unemployed_count", #sample size unemployed
         "In employment_count", #sample size employed
         "HS_emp_count")      # sample size high skill


#Rename colnames to match standards for platform and be consist with naming in Year_salaries
colnames(Emp_Confidence_interval_a)[colnames(Emp_Confidence_interval_a)
                             %in%c("population",
                                   "year",
                                   "age",
                                   "ILO unemployed_count",
                                   "In employment_count")]<-c("graduate_type",
                                              "time_period",
                                              "age_group",
                                              "unemployed_count",
                                              "employment_count")



#Revalues for age_group so more explicit
Emp_Confidence_interval_a$age_group<- revalue(Emp_Confidence_interval_a$age_group, c("16-64"="Age Group 16-64"))
Emp_Confidence_interval_a$age_group<- revalue(Emp_Confidence_interval_a$age_group, c("21-30"="Age Group 21-30" ))



#Mutate columns needed to meet minium standards for time and geography. 
Emp_Confidence_interval_b<-Emp_Confidence_interval_a%>%
  mutate(geographic_level="National",
         country_code="E92000001",
         country_name="England",
         time_identifier="Calendar year")

#Reformat the employemt rates to display values
#as percentage number to 1 decimal place to be consistent with rounding historically used in the GLMS publication

#Emp_prop and boundaries (employment rate). Ensure sample sizes <4 are surpressed (see assumptions log)
Emp_Confidence_interval_c<-Emp_Confidence_interval_b%>%
  mutate(Emp_prop = ifelse(employment_count > 3, Emp_prop*100, ":"))%>%
  mutate(Emp_prop_lower=Emp_prop_lower*100)%>%
  mutate(Emp_prop_upper=Emp_prop_upper*100)%>%
  mutate(Emp_prop=formatC(Emp_prop, digits = 1, format = "f"))%>%
  mutate(Emp_prop_lower=formatC(Emp_prop_lower, digits = 1, format = "f"))%>%
  mutate(Emp_prop_upper=formatC(Emp_prop_upper, digits = 1, format = "f"))

#HSEmp_prop and boundaries. Ensure sample sizes <4 are surpressed (see assumptions log)
Emp_Confidence_interval_d<-Emp_Confidence_interval_c%>%
  mutate(HSEmp_prop = ifelse(HS_emp_count>3, HSEmp_prop*100, ":"))%>%
  mutate(HSEmp_prop_lower = HSEmp_prop_lower*100)%>%
  mutate(HSEmp_prop_upper = HSEmp_prop_upper*100)%>%
  mutate(HSEmp_prop = formatC(HSEmp_prop, digits = 1, format = "f"))%>%
  mutate(HSEmp_prop_lower = formatC(HSEmp_prop_lower, digits = 1, format = "f"))%>%
  mutate(HSEmp_prop_upper = formatC(HSEmp_prop_upper, digits = 1, format = "f"))

#Unemp_prop and boundaries. Ensure sample sizes <4 are surpressed (see assumptions log)
Emp_Confidence_interval_e <-Emp_Confidence_interval_d%>%
  mutate(Unemp_prop = ifelse(unemployed_count >3, Unemp_prop*100, ":" ))%>%
  mutate(Unemp_prop_lower = Unemp_prop_lower*100)%>%
  mutate(Unemp_prop_upper = Unemp_prop_upper*100)%>%
  mutate(Unemp_prop = formatC(Unemp_prop, digits = 1, format = "f"))%>%
  mutate(Unemp_prop_lower = formatC(Unemp_prop_lower, digits = 1, format = "f"))%>%
  mutate(Unemp_prop_upper = formatC(Unemp_prop_upper, digits = 1, format = "f"))

#revalue the observations in the Quarter column
Emp_Confidence_interval_g<-Emp_Confidence_interval_e%>%
  mutate(Quarter=paste0("Q",Quarter))
       
# organise the order of collumns to fit the standard for the EES platform

Emp_Confidence_interval_fin<-Emp_Confidence_interval_g%>%
  select("time_period",
         "time_identifier",
         "Quarter",
         "geographic_level",
         "country_code",
         "country_name",
         "age_group",
         "graduate_type",
         "Emp_prop",
         "Emp_prop_upper",
         "Emp_prop_lower", 
         "HSEmp_prop",
         "HSEmp_prop_upper",
         "HSEmp_prop_lower",
         "Unemp_prop",
         "Unemp_prop_upper",
         "Unemp_prop_lower")

write.csv(Emp_Confidence_interval_fin,
          paste0(filepath, "Outputs_folder/EES_csv/Employment_Rate_by_Quarterly_Time_Series_",label_EES,".csv"),row.names = FALSE,quote = FALSE,fileEncoding = "UTF-8")


#QA check for NA's, zeros or surpressed values

print(" Na's in the table ")
print(apply(Emp_Confidence_interval_fin,2,function(x) any(is.na(x))))

print(" zeros in the table ?")
print(apply(Emp_Confidence_interval_fin,2,function(x) any((x == "0"))))


print(": in the table ")
print(apply(Emp_Confidence_interval_fin,2,function(x) any((x == ":"))))

#----meta file: Quarterly Time Series (Employment Rates)-----

#Make a table for the meta data

Emp_Confidence_interval.meta <- data.frame("col_name" = c("Emp_prop",
                                                          "Emp_prop_upper",
                                                          "Emp_prop_lower",
                                                          "HSEmp_prop",
                                                          "HSEmp_prop_upper",
                                                          "HSEmp_prop_lower",
                                                          "Unemp_prop",
                                                          "Unemp_prop_upper",
                                                          "Unemp_prop_lower",
                                                          "age_group",
                                                          "graduate_type",
                                                          "Quarter"),
                                           "col_type" = c("Indicator",
                                                   "Indicator",
                                                   "Indicator",
                                                   "Indicator",
                                                   "Indicator",
                                                   "Indicator",
                                                   "Indicator",
                                                   "Indicator",
                                                   "Indicator",
                                                   "Filter",
                                                   "Filter",
                                                   "Filter"),
                                    "label" = c("Employment Rate",
                                                "Employment Rate Upper Bound",
                                                "Employment Rate Lower Bound",
                                                "High Skill Employment Rate",
                                                "High Skill Employment Rate Upper Bound",
                                                "High Skill Employment Rate Lower Bound",
                                                "Unemployment Rate",
                                                "Unemployment Rate Upper Bound ",
                                                "Unemployment Rate Lower Bound",
                                                "Age Group",
                                                "Graduate Type",
                                                "Quarter"),
                                    "indicator_grouping" = c("","","","","","","","","","","",""),
                                    "indicator_unit" = c("%","%","%","%","%","%","%","%","%","","",""),
                                    "indicator_dp" = c("1","1","1","1","1","1","1","1","1","","",""),
                                    "filter_hint" = c("", "","","","","","","","","Select the Age group you are interested in","Select the Graduate type you are interested in","Select which Quarter of the year you are interested in"),
                                    "filter_grouping_column"=c("","","","","","","","","","","",""))



write.csv(Emp_Confidence_interval.meta,
          paste0(filepath, "Outputs_folder/EES_csv/Employment_Rate_by_Quarterly_Time_Series_",label_EES,".meta.csv"),row.names = FALSE,quote = FALSE,fileEncoding = "UTF-8")


