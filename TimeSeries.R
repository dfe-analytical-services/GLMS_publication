#-----Introduction----
#"Graduate Labour Market Statistics has historically been published on GOV.UK The supporting data with worksheet titles: 
#  Headline Statistics by Year 
#  Headline Statistics 
# are generated from the files Yearly_employment_rates.csv and Yearly_salaries.csv. 

#The GLMS project that is used to make Yearly_employment_rates.csv and Yearly_salaries.csv and the corresponding rds files 
# is fround in https://dfe-gov-uk.visualstudio.com/HEFE-Higher-Education-Analysis/_git/SFM-LFS-Analysis  . The branch is GLMS_V12020

#----Description of Script----
#This scripts takes the data in Yearly_employment_rates.rds and Yearly_salaries.rds
#and puts it in the format needed for the new DfE EES platform.
# see https://rsconnect/rsc/stats-production-guidance/

#Specifically:
#(i) Employment Rates for the Time Series is found in
#    one data table and has a corresponding meta file
#(ii) Salaries for the Time Series is found in
#    one data table and has a corresponding meta file

#----What format is needs ?----
#a) .csv files. For each data file a meta data file is needed
#b) tidy data format
#c)UtF-8 file encoding


#----library----
library("dplyr")
library(plyr)
library(lubridate)


#----Upload data ----
#The outputs from the GLMS program are uploaded from the shared area


Yearly_employment_rates<-readRDS(paste0(filepath, "Outputs_for_GLMS/EES_rds/Yearly_employment_",start_year,"_",end_year,".rds"))

Yearly_salaries<-readRDS(paste0(filepath, "Outputs_for_GLMS/EES_rds/Yearly_salaries_",start_year,"_",end_year,".rds"))



#----data file: Time Series (Employment Rates) from Yearly_Employment_rates-----


#Select the variables needed and
#Mutate columns needed to meet minium standards for time and geography.
Employment_Rates<-Yearly_employment_rates%>%
  select("year",
         "Age_range",
         "Population",
         "Employment_rate",
         "High_skill_emp_rate",
          "Unemployment_rate",
         "inactivity_rate",
         "employed_sample",
         "HS_employed_sample",
         "unemployed_sample",
         "inactive_sample") %>%
  mutate(time_identifier="Calendar year",
         geographic_level="National",
         country_code="E92000001",
         country_name="England")


#Rename colnames to match standards for EES platform 
colnames(Employment_Rates)[colnames(Employment_Rates)
                           %in%c("year",
                                 "Age_range",
                                 "Population")]<-c("time_period",
                                                   "age_group",
                                                   "graduate_type")




 


Employment_Rates_a2<-Employment_Rates%>%
  ungroup()%>%
  filter(!graduate_type=="Not in scope")


Employment_Rates_b<-Employment_Rates_a2%>%
  mutate(Employment_rate = ifelse(employed_sample >=31, Employment_rate*100, ":"))%>%
  mutate(High_skill_emp_rate = ifelse(HS_employed_sample >=31, High_skill_emp_rate*100, ":"))%>%
  mutate(Unemployment_rate = ifelse(unemployed_sample >=31, Unemployment_rate*100, ":"))%>% 
  mutate(inactivity_rate = ifelse(inactive_sample >=31, inactivity_rate*100, ":"))%>%       #Needed for Yearly Statistics
  mutate(Employment_rate=formatC(Employment_rate, digits = 1, format = "f"))%>%
  mutate(High_skill_emp_rate=formatC(High_skill_emp_rate, digits = 1, format = "f"))%>%
  mutate(Unemployment_rate=formatC(Unemployment_rate, digits = 1, format = "f"))%>%
  mutate(inactivity_rate=formatC(inactivity_rate, digits = 1, format = "f"))


#Revalue some of the values in the age_group collumns so that age_group="16-64" is age_group="Age Group 16-64" and
#age_group="21-30" is age_group="Age Group 21-30"
Employment_Rates_b$age_group <- revalue(Employment_Rates_b$age_group, c("16-64"="Age Group 16-64"))
Employment_Rates_b$age_group <- revalue(Employment_Rates_b$age_group, c("21-30"="Age Group 21-30"))



# organise the order of collumns to fit the standard for the platform

Employment_Rates_fin<-Employment_Rates_b%>%
  select("time_period",
         "time_identifier",
         "geographic_level",
         "country_code",
         "country_name",
         "age_group",
         "graduate_type",
         "Employment_rate",
         "High_skill_emp_rate",
         "Unemployment_rate",
         "inactivity_rate")


readr::write_csv(Employment_Rates_fin,
          paste0(filepath, "Outputs_for_GLMS/EES_csv/Employment_Rates_by_Graduate_Type_",label_EES,".csv"),quote = FALSE)



#QA check for NA's,0 and - in outputs
print(" Na's in the table ")
print(apply(Employment_Rates_fin, 2, function(x) any(is.na(x))))

print(" zeros in the table ")
print(apply(Employment_Rates_fin, 2, function(x) any((x== "0"))))

print(" : in the table, data surpressed ")
print(apply(Employment_Rates_fin, 2, function(x) any((x== ":"))))

#----meta file: Time Series (Employment Rates) from Yearly_Employment_rates-----

#Make a table for the meta data

Employment_Rates.meta <- data.frame("col_name" = c("Employment_rate",
                                                   "High_skill_emp_rate",
                                                   "Unemployment_rate",
                                                   "inactivity_rate",
                                                   "age_group",
                                                   "graduate_type"),
                                    "col_type" = c("Indicator",
                                                   "Indicator",
                                                   "Indicator",
                                                   "Indicator",
                                                   "Filter",
                                                   "Filter"),
                                    "label" = c("Employment Rate",
                                                "High Skill Employment Rate",
                                                "Unemployment Rate",
                                                "Inactivity Rate",
                                                "Age Group",
                                                "Graduate Type"),
                                    "indicator_grouping" = c("","", "","","",""),
                                    "indicator_unit" = c("%","%","%","%","",""),
                                    "indicator_dp"= c("1","1", "1","1","",""),
                                    "filter_hint" = c("", "","","","Select the Graduate Age Group you are interested in","Select the Graduate Type you are interested in"),
                                    "filter_grouping_column"=c("","","","","",""))



readr::write_csv(Employment_Rates.meta,
          paste0(filepath, "Outputs_for_GLMS/EES_csv/Employment_Rates_by_Graduate_Type_",label_EES,".meta.csv"),quote = FALSE)


#----data file: Median Salaries by Year from Yearly_salaries: data file----

#Select the variables needed
Yearly_salaries_new<-Yearly_salaries%>%
  select("year",
         "AgeGroup" ,
         "Graduate_Type",
         "median",
         "SEX")


#Rename colnames to match standards for platform
colnames(Yearly_salaries_new)[colnames(Yearly_salaries_new)%in%c("year","AgeGroup","SEX")]<-c("time_period","age_group","gender")

#Mutate columns needed to meet minium standards for time and geography. Add a coulmn to show
#which catrogorical information will be grouped
Yearly_salaries_new_a<-Yearly_salaries_new%>%
  mutate(time_identifier="Calendar year",
         geographic_level="National",
         country_code="E92000001",
         country_name="England")


#Change Median salaries to be the yearly median salary,then Round up the median salaries to the nearest £500. See data assumptions
#log for the rounding convention
Yearly_salaries_new_b<-Yearly_salaries_new_a%>%
  mutate(median=ifelse(leap_year(time_period),median*366/7,median*365/7))%>%
  mutate(p=median%%500)%>%  
  mutate(rd_up=median+500-p)%>% #Numbers for rounding up
  mutate(rd_down=median-p)%>%   #Numbers for rounding down
  mutate(median=ifelse(500-p<=p,rd_up,rd_down)) #decision on rounding

#Remove surplus columns
Yearly_salaries_new_b<-Yearly_salaries_new_b[,-(11:13),drop=FALSE]

#Need to filter out data so that it matches the GLMS publication
#The information needed is AgeGroup=21-30 for gender=male, female and Total
#you wany AgeGroup = Total(16-64 years) for gender= male, female and All
Yearly_salaries_new_c<-Yearly_salaries_new_b%>%
  filter(age_group=="21-30"|age_group == "Total")



#For AgeGroup="Total". This is the same as AgeGroup=" Age Group 16-64". To be consistent with the the GLMS publication and format of
#EES platform rename values
Yearly_salaries_new_c$age_group <- revalue(Yearly_salaries_new_c$age_group, c("Total"="Age Group 16-64"))
Yearly_salaries_new_c$age_group <- revalue(Yearly_salaries_new_c$age_group, c("21-30"="Age Group 21-30"))

#For gender="All". This is the same as gender="Total". To be consistent with the the publication then rename
#the value in the dataframe. To be consistent with the the publication standards rename thios value
Yearly_salaries_new_c$gender <- revalue(Yearly_salaries_new_c$gender, c("All"="Total"))
Yearly_salaries_new_c$gender <- revalue(Yearly_salaries_new_c$gender, c("Female"="Gender Female"))
Yearly_salaries_new_c$gender <- revalue(Yearly_salaries_new_c$gender, c("Male"="Gender Male"))
                                                   

#Remove Not in Scope data and group by time_period
Yearly_salaries_new_d<-Yearly_salaries_new_c%>%
  filter(!Graduate_Type=="Not in scope")

#Rename Colnames so letters are all lower case

colnames(Yearly_salaries_new_d)[colnames(Yearly_salaries_new_d)
                             %in%c("Graduate_Type")]<-c("graduate_type")


# organise the order of collumns to fit the standard for the EES platform

Yearly_salaries_new_fin<-Yearly_salaries_new_d%>%
  select("time_period",
         "time_identifier",
         "geographic_level",
         "country_code",
         "country_name",
         "age_group",
         "graduate_type",
         "gender",
         "median")



readr::write_csv(Yearly_salaries_new_fin,
          paste0(filepath, "Outputs_for_GLMS/EES_csv/Yearly_salaries_by_gender_",label_EES,".csv"),quote = FALSE)





#QA check for NA's in outputs
print("Na's in table")
print(apply(Yearly_salaries_new_fin,2,function(x) any(is.na(x))))




#----meta file: Median Salaries by Year from Yearly_salaries: data file----

#Make a table for the meta data

Yearly_salaries_by_gender.meta <- data.frame("col_name" = c("median", "age_group","graduate_type","gender"),
                                             "col_type" = c("Indicator","Filter","Filter","Filter"),
                                             "label" = c("Median Salary to the nearest £500",
                                                         "Age Group", "Graduate Type","Gender"),
                                             "indicator_grouping" = c("", "","",""),
                                             "indicator_unit" = c("£","","",""),
                                             "indicator_dp" = c("", "","",""),
                                             "filter_hint" = c("","","",""),
                                             "filter_grouping_column"=c("","","",""))



#OPTIONAL:encoding step------------------
#Only use if using Source function to run this script. Since source function automatically adds encoding 
Yearly_salaries_by_gender2.meta <- as.data.frame(sapply(Yearly_salaries_by_gender.meta, function(x) gsub("Â£", "£", x)))

write.csv(Yearly_salaries_by_gender2.meta,
          paste0(filepath, "Outputs_for_GLMS/EES_csv/Yearly_salaries_by_gender_", label_EES, ".meta.csv"),quote = FALSE,row.names = FALSE)


#OPTIonal:encoding------------
#In scenario that you are running this script line by line use write_csv to add encoding automatically
#readr::write_csv(Yearly_salaries_by_gender.meta,
          #paste0(filepath, "Outputs_for_GLMS/EES_csv/Yearly_salaries_by_gender_", label_EES, ".meta.csv"),quote = FALSE)

