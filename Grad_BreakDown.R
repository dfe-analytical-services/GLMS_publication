#-----Introduction----

#x is end_year
#1.Graduate_breakdown_x.csv (Gives Employment Rates) 
#2. GRAD_SAL_x.csv (Gives Salaries)
#3.young_grad_sal_x.csv (Gives Salaries)



#----Description of Script----
#This scripts takes the data  Graduate_breakdown_x.rds,GRAD_SAL_x.rds files and 
#and young_grad_sal_x.rds and puts it in the format needed for the new (Explore Education and Statistic Platform) EES  platform

#Specifically:
#(i) Employment Rates and Salaries for Graduate Breakdowns where the demographic is Age Group is put into
#    one data table and has a corresponding meta file
#(ii) For all other Graduate Breakdowns  i.e where the demographic is not  Age Group then  data files for
#     Employment Rates and Salaries are put into separate files respectively, and each has a corresponding meta file.


#----What format is needs ?----
#a) .csv files. For each data file a meta data file is needed
#b) tidy data format
#c)UtF-8 file encoding



#----library----
library("dplyr")
library(plyr)
library(lubridate)




#----Upload outputs of GLMS.prj ----
#The rds outputs from the GLMS program are uploaded from the shared area

Graduate_breakdown_x<-readRDS(paste0(filepath, "Outputs_folder/Graduate_breakdown_lev_", year, ".rds"))

grad_sal_x <-readRDS(paste0(filepath, "Outputs_folder/grad_sal_", year, ".rds"))

young_grad_sal_x <- readRDS(paste0(filepath, "Outputs_folder/young_grad_sal_", year, ".rds"))






#----data files: Graduate breakdown (Employment Rates)-----

#Rename colnames to match standards for platform and be consist with naming in Year_salaries
colnames(Graduate_breakdown_x)[colnames(Graduate_breakdown_x)
                               %in%c("Group","Population","Breakdown")]<-c("graduate_group","graduate_characteristic","graduate_breakdown")


#Mutate columns needed to meet minium standards for time and geography. 
Graduate_breakdown_a<-Graduate_breakdown_x%>%
  mutate(time_identifier="Calendar year",
         geographic_level="National",
         country_code="E92000001",
         country_name="England",
         time_period=paste0(end_year))

#Reformat the "Employment_rate","High_skill_emp_rate","Unemployment_rate" to display values
#as percentage number to 1 decimal place to be consistent with past GLMS publications
Graduate_breakdown_b<-Graduate_breakdown_a%>%
  mutate(Employment_rate=Employment_rate*100)%>%
  mutate(High_skill_emp_rate=High_skill_emp_rate*100)%>%
  mutate(Unemployment_rate=Unemployment_rate*100)%>% 
  mutate(Inactivity_rate=Inactivity_rate*100)%>%       #Needed for Headline Statistics
  mutate(Employment_rate=formatC(Employment_rate, digits = 1, format = "f"))%>%
  mutate(High_skill_emp_rate=formatC(High_skill_emp_rate, digits = 1, format = "f"))%>%
  mutate(Unemployment_rate=formatC(Unemployment_rate, digits = 1, format = "f"))%>%
  mutate(Inactivity_rate=formatC(Inactivity_rate, digits = 1, format = "f"))

#Filter for invalid responses or responses not used in the publication. -8 is LFS coding for No Answer from respondent
Graduate_breakdown_c<-Graduate_breakdown_b%>%
  filter(!graduate_breakdown=="-8")%>%
  filter(!graduate_breakdown=="Dont Know")%>%
  filter(!graduate_breakdown=="Other")%>%
  filter(!Unemployment_rate=="NaN")%>%
  filter(!graduate_breakdown=="NA")%>%
  filter(!graduate_breakdown=="Pass")%>%
  filter(!graduate_breakdown=="61-64")

#For Sample Sizes <= 30 need to surpress the employment rate values in the data, Footnote should be Data to small sample size (fewer than 30)		
#use ":" to label these value
Graduate_breakdown_d<-Graduate_breakdown_c%>%
  mutate(Employment_rate=ifelse(Employed_sample>=31,Employment_rate, ":"))%>%
  mutate(High_skill_emp_rate=ifelse(High_Skill_employed_sample>=31,High_skill_emp_rate, ":"))%>%
  mutate(Unemployment_rate=ifelse(Unemployed_sample>=31,Unemployment_rate, ":"))%>%
  mutate(Inactivity_rate=ifelse(Inactive_sample>=31,Inactivity_rate, ":"))

#Remove data  that does not belong in graduate_group=="Graduates:21-30", namely  graduate_breakdown=31+
Graduate_breakdown_e<-Graduate_breakdown_d%>%
  filter(!(graduate_characteristic=="AgeGroup" & graduate_group=="Graduates:21-30"& graduate_breakdown=="31-40"))%>%
  filter(!(graduate_characteristic=="AgeGroup" & graduate_group=="Graduates:21-30"& graduate_breakdown=="41-50"))%>%
  filter(!(graduate_characteristic=="AgeGroup" & graduate_group=="Graduates:21-30"& graduate_breakdown=="51-60"))

#Revalue  the values in the Graduate_breakdown column
Graduate_breakdown_f<-Graduate_breakdown_e%>%
  mutate(graduate_breakdown=ifelse(graduate_characteristic=="AgeGroup",paste0("Age Group ",graduate_breakdown),
                                   ifelse(graduate_characteristic=="DEGCLS7",paste0("Degree class ",graduate_breakdown),
                                          ifelse(graduate_characteristic=="DISEA",paste0("Disability status ",graduate_breakdown),
                                                 ifelse(graduate_characteristic=="ETHNICITY",paste0("Ethnicity ",graduate_breakdown),
                                                        ifelse(graduate_characteristic=="GOVTOF2",paste0("Government Office Region ",graduate_breakdown),
                                                               ifelse(graduate_characteristic=="SEX",paste0("Gender ",graduate_breakdown),
                                                                      ifelse(graduate_characteristic=="STEM","Subject Group STEM",
                                                                             ifelse(graduate_characteristic=="LEM","Subject Group LEM",
                                                                                    ifelse(graduate_characteristic=="OSSAH","Subject Group OSSAH",
                                                                                           "x"))))))))))

#Replace the "/" with "or"
Graduate_breakdown_f$graduate_breakdown<- revalue(Graduate_breakdown_f$graduate_breakdown,c("Ethnicity Asian / Asian British"="Ethnicity Asian or Asian British"))
Graduate_breakdown_f$graduate_breakdown<- revalue(Graduate_breakdown_f$graduate_breakdown,c("Ethnicity Black / African / Caribbean / Black British"="Ethnicity Black or African or Caribbean or Black British"))
Graduate_breakdown_f$graduate_breakdown<- revalue(Graduate_breakdown_f$graduate_breakdown,c("Ethnicity Other ethnic group."="Ethnicity Other ethnic group."))
Graduate_breakdown_f$graduate_breakdown<- revalue(Graduate_breakdown_f$graduate_breakdown,c("Ethnicity White"="Ethnicity White"))

#Revalue  the values in the graduate_characteristic column
Graduate_breakdown_g<-Graduate_breakdown_f%>%
  mutate(graduate_characteristic=ifelse(graduate_characteristic=="AgeGroup","Age Group ",
                                        ifelse(graduate_characteristic=="DEGCLS7","Degree class",
                                               ifelse(graduate_characteristic=="DISEA","Disability status ",
                                                      ifelse(graduate_characteristic=="ETHNICITY","Ethnicity ",
                                                             ifelse(graduate_characteristic=="GOVTOF2","Government Office Region ",
                                                                    ifelse(graduate_characteristic=="SEX","Gender",
                                                                           ifelse(graduate_characteristic=="STEM","Subject Group",
                                                                                  ifelse(graduate_characteristic=="LEM","Subject Group",
                                                                                         ifelse(graduate_characteristic=="OSSAH","Subject Group",
                                                                                                "x"))))))))))
#Remove : in values for graduate_group as this can not be read by the platform
Graduate_breakdown_g$graduate_group<- revalue(Graduate_breakdown_g$graduate_group, c("Graduates:16-64"="Graduates 16-64"))
Graduate_breakdown_g$graduate_group<- revalue(Graduate_breakdown_g$graduate_group, c("Graduates:21-30"="Graduates 21-30"))



# organise the order of columns to fit the standard for the platform. Note the employment totals and sample
#are the  sum over the whole year whereas the rates are the mean rates over the four quarters. We do not
# publish the employment totals .

Graduate_breakdown_fin<-Graduate_breakdown_g%>%
  select("time_period",
         "time_identifier",
         "geographic_level",
         "country_code",
         "country_name",
         "graduate_group",
         "graduate_characteristic",
         "graduate_breakdown",
         "Employment_rate",
         "High_skill_emp_rate",
         "Unemployment_rate",
         "Inactivity_rate")


#For the rows that contain graduate_characteristic == "Age Group " need to be filtered
#out into another dataframe for ease of use on the EES platform. Make two data frames

#Employment Rates for different Age Groups of Graduates
#The Salaries for different Age Groups of Graduates will be combined in sections EMPLOYMENT RATE & SALARY
GradBreak_Employ_AgeGroup<-Graduate_breakdown_fin%>%
  filter(graduate_characteristic == "Age Group ")

#Employment Rates for different Demographics of Graduates that DO NOT include Demographics=AgeGroup
GradBreak_Employ_Demographics<-Graduate_breakdown_fin%>%
  filter(!graduate_characteristic == "Age Group ")

#Save Gradbreakdown Employment Rates that do not include Demographics=Age Group 
readr::write_csv(GradBreak_Employ_Demographics,
          paste0(filepath, "Outputs_for_folder/EES_csv/GradBreak_Employ_Demographics_",year,".csv"),quote = FALSE)


#----QA CHECKS:Employment Rates----

#QA the filtering of the data frame into two parts by:
#a)Is the totol number of rows retained after filtering ?
if(nrow(Graduate_breakdown_fin)==nrow(GradBreak_Employ_Demographics)+nrow(GradBreak_Employ_AgeGroup)) {
  print("The script gives the same totols")}else{
    print("The script does not give the same totols")
  }
#b)Do I have missing data ?
#Bind together the same smaller data sets.Use anti-join between the bound tables and the orginal table to see if there are no matches found i.e
#you looking to see if there is data in the orginal table that is not found in the samller tables

GradBreak_employ_bind<-rbind(GradBreak_Employ_Demographics,GradBreak_Employ_AgeGroup)
if(nrow(anti_join(GradBreak_employ_bind,Graduate_breakdown_fin))==0){
  print("There is no missing data after filtering")}else{
    print("There is missing data after filtering")
  }

#QA -Check for NA's in the output
print("Are there any Na's in the table ?")
print(apply(GradBreak_Employ_Demographics, 2, function(x) any(is.na(x))))

#----meta files: Graduate breakdown Demographic = !Age Group, (Employment Rates)-----

#Make a Meta data file for GradBreak_Employ_Demographics data file
GradBreak_Employ_Demographics.meta <- data.frame("col_name" = c("Employment_rate",
                                                                "High_skill_emp_rate",
                                                                "Unemployment_rate",
                                                                "Inactivity_rate",
                                                                "graduate_breakdown",
                                                                "graduate_group"),
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
                                                             "Graduate Breakdown",
                                                             "Graduate Group"),
                                                 "indicator_grouping" = c("","", "","","",""),
                                                 "indicator_unit" = c("%","%","%","%","",""),
                                                 "indicator_dp" = c("1","1","1","1","",""),
                                                 "filter_hint" = c("", "","","","Select the Graduate breakdown you are interested in","Select the Graduate group you are interested in"),
                                                 "filter_grouping_column"=c("","","","","graduate_characteristic",""))


readr::write_csv(GradBreak_Employ_Demographics.meta,
          paste0(filepath, "Outputs_folder/EES_csv/GradBreak_Employ_Demographics_",year,".meta.csv"),quote = FALSE)


#----data:Median Salaries from Grad salaries ----

#Select the variables needed from GRAD_SAL_x
grad_sal_x_a<-grad_sal_x%>%
  select("Value",
         "sample" ,
         "population",
         "median",
         "Value1",
         "variable")

#Mutate columns needed  in grad_sal_x to meet minium standards for time and geography. Add a column to show
#which catrogorical information will be grouped i.e characteristic_group 
grad_sal_x_b<-grad_sal_x_a%>%
  mutate(time_identifier="Calendar year",
         geographic_level="National",
         country_code="E92000001",
         country_name="England",
         time_period=as.numeric(end_year),
         graduate_group="Graduates 16-64")


#Select the variables needed from young_gran_sal_x
young_grad_sal_x_a<-young_grad_sal_x%>%
  select("Value",
         "sample" ,
         "population",
         "median",
         "Value1",
         "variable")

#Mutate columns needed  in GRAD_SAL_x to meet minium standards for time and geography. Add a coulmn to show
#which catrogorical information will be grouped i.e characteristic_group 
young_grad_sal_x_b<-young_grad_sal_x_a%>%
  mutate(time_identifier="Calendar year",
         geographic_level="National",
         country_code="E92000001",
         country_name="England",
         time_period=as.numeric(paste0(end_year)),
         graduate_group="Graduates 21-30")


#Join grad_sal_x and young_grad_sal_x to make one large table
All_grad_sal_x<-bind_rows(grad_sal_x_b,young_grad_sal_x_b)

#adjust median to give yearly median salary to the closest £500. 
All_grad_sal_x_a<-All_grad_sal_x%>%
  mutate(median=ifelse(leap_year(time_period),median*366/7,median*365/7))%>%
  mutate(p=median%%500)%>%  
  mutate(rd_up=median+500-p)%>% #Numbers for rounding up
  mutate(rd_down=median-p)%>%   #Numbers for rounding down
  mutate(median=ifelse(500-p<=p,rd_up,rd_down)) #decision on rounding

#Remove surplus columns
All_grad_sal_x_a<-All_grad_sal_x_a[,-(13:15),drop=FALSE]

#Rename colnames to match standards for platform
colnames(All_grad_sal_x_a)[colnames(All_grad_sal_x_a)%in%c("Value","sample", "variable"  )]<-c("graduate_breakdown","sample_size","graduate_characteristic")


#Filter for invalid responses or responses not used in the publication, -8 is LFS code for No answer
All_grad_sal_x_b<-All_grad_sal_x_a%>%
  filter(!graduate_breakdown=="-8")%>%
  filter(!graduate_breakdown=="Dont Know")%>%
  filter(!graduate_breakdown=="NA")


#Change values in $graduate_breakdown==1 into values
All_grad_sal_x_c<-All_grad_sal_x_b%>%
  mutate(graduate_breakdown=ifelse(graduate_characteristic=="STEM","STEM",
                                   ifelse(graduate_characteristic=="LEM","LEM",
                                          ifelse(graduate_characteristic=="OSSAH","OSSAH",
                                                 graduate_breakdown))))


#Replace the "/" with "or"
All_grad_sal_x_c$graduate_breakdown<- revalue(All_grad_sal_x_c$graduate_breakdown, c("Asian / Asian British"=" Asian or Asian British"))
All_grad_sal_x_c$graduate_breakdown<- revalue(All_grad_sal_x_c$graduate_breakdown, c("Black / African / Caribbean / Black British"="Black or African or Caribbean or Black British"))
All_grad_sal_x_c$graduate_breakdown<- revalue(All_grad_sal_x_c$graduate_breakdown, c("Other ethnic group."="Other ethnic group"))


#Remove the numbers from occuptaion and industry breakdowns that are used in GLMS from $graduate_breakdown and $Value1
All_grad_sal_x_c$graduate_breakdown<- revalue(All_grad_sal_x_c$graduate_breakdown, c("1  'Managers, Directors And Senior Officials'"="Managers or Directors or Senior Officials"))
All_grad_sal_x_c$graduate_breakdown<- revalue(All_grad_sal_x_c$graduate_breakdown, c("2  'Professional Occupations'" ="Professional Occupations"))
All_grad_sal_x_c$graduate_breakdown<- revalue(All_grad_sal_x_c$graduate_breakdown, c("3  'Associate Professional And Technical Occupations'"="Associate Professional And Technical Occupations"))
All_grad_sal_x_c$graduate_breakdown<- revalue(All_grad_sal_x_c$graduate_breakdown, c("C -Manufacturing"="Manufacturing"))
All_grad_sal_x_c$graduate_breakdown<- revalue(All_grad_sal_x_c$graduate_breakdown, c("F - Construction" ="Construction"))
All_grad_sal_x_c$graduate_breakdown<- revalue(All_grad_sal_x_c$graduate_breakdown, c("10 'Medium Skilled Employment'"  ="Medium Skilled Employment"))
All_grad_sal_x_c$graduate_breakdown<- revalue(All_grad_sal_x_c$graduate_breakdown, c( "11 'Low Skilled Employment'" ="Low Skilled Employment"))
All_grad_sal_x_c$graduate_breakdown<- revalue(All_grad_sal_x_c$graduate_breakdown, c("G,I -Distribution, hotels and restaurants"="Distribution or Hotels or Restaurants"))
All_grad_sal_x_c$graduate_breakdown<- revalue(All_grad_sal_x_c$graduate_breakdown, c("H,J -Transport and communication" ="Transport and Communication"))
All_grad_sal_x_c$graduate_breakdown<- revalue(All_grad_sal_x_c$graduate_breakdown, c("K,L,M,N - Banking and finance"="Banking and Finance"))
All_grad_sal_x_c$graduate_breakdown<- revalue(All_grad_sal_x_c$graduate_breakdown, c("O,P,Q - Public admin, education and health"  ="Public Admin or Education or Health"))


All_grad_sal_x_c$Value1<-revalue(All_grad_sal_x_c$Value1,c("C -Manufacturing"="Manufacturing"))
All_grad_sal_x_c$Value1<-revalue(All_grad_sal_x_c$Value1, c("F - Construction" ="Construction"))
All_grad_sal_x_c$Value1<-revalue(All_grad_sal_x_c$Value1, c("G,I -Distribution, hotels and restaurants"="Distribution or Hotels or Restaurants"))
All_grad_sal_x_c$Value1<-revalue(All_grad_sal_x_c$Value1, c("H,J -Transport and communication" ="Transport and Communication"))
All_grad_sal_x_c$Value1<-revalue(All_grad_sal_x_c$Value1, c("K,L,M,N - Banking and finance"="Banking and Finance"))
All_grad_sal_x_c$Value1<-revalue(All_grad_sal_x_c$Value1, c("O,P,Q - Public admin, education and health"  ="Public Admin or Education or Health"))

#Filter out data not used in GLMs, namely graduate_breakdown = 
#"R,S,T,U - Other services"
#"B,D,E - Energy and water" 
#"A - Agriculture, forestry and fishing" 
#do the same for the $Value1
All_grad_sal_x_d<-All_grad_sal_x_c%>%
  filter(!graduate_breakdown=="R,S,T,U - Other services")%>%
  filter(!graduate_breakdown=="B,D,E - Energy and water")%>%
  filter(!graduate_breakdown=="A - Agriculture, forestry and fishing")


#Revalue  the values in the graduate_characteristic column
All_grad_sal_x_e<-All_grad_sal_x_d%>%
  mutate(graduate_characteristic=ifelse(graduate_characteristic=="AgeGroup","Age Group",
                                        ifelse(graduate_characteristic=="DEGCLS7","Degree class",
                                               ifelse(graduate_characteristic=="DISEA","Disability status",
                                                      ifelse(graduate_characteristic=="ETHNICITY","Ethnicity",
                                                             ifelse(graduate_characteristic=="GOVTOF2","Government Office Region",
                                                                    ifelse(graduate_characteristic=="SEX","Gender",
                                                                           ifelse(graduate_characteristic=="STEM","Subject Group",
                                                                                  ifelse(graduate_characteristic=="LEM","Subject Group",
                                                                                         ifelse(graduate_characteristic=="OSSAH","Subject Group",
                                                                                                ifelse(graduate_characteristic=="INDE07M","Industry",
                                                                                                       ifelse(graduate_characteristic=="SEX_INDE07M","Industry and Gender",
                                                                                                              graduate_characteristic))))))))))))








trimws(All_grad_sal_x_e$graduate_characteristic)
trimws(All_grad_sal_x_e$graduate_breakdown)

#Revalue  the values in the characteristic_breakdown column
All_grad_sal_x_f<-All_grad_sal_x_e%>%
  mutate(graduate_breakdown=paste0(graduate_characteristic," ",graduate_breakdown))


#Revalue  the values in the graduate_breakdown column to include information on gender break down and Industry
All_grad_sal_x_g<-All_grad_sal_x_f%>%
  mutate(graduate_breakdown=ifelse(graduate_breakdown=="Industry and Gender Female",paste0("Industry " ,Value1, " and Gender Female"),
                                   ifelse(graduate_breakdown=="Industry and Gender Male",paste0("Industry " ,Value1, " and Gender Male"),
                                          graduate_breakdown)))


#Filter out data not used in GLMs, namely graduate_breakdown that includes the inudustry below with Gender breakdown, any -9 or -8 or Na left, (-8, -9 is LFS code for No answer and Did not Ask respectively)
#"R,S,T,U - Other services"
#"B,D,E - Energy and water" 
#"A - Agriculture, forestry and fishing" 
#"Degree class Other"
#"Degree class Pass"
All_grad_sal_x_h<-All_grad_sal_x_g%>%
  filter(!graduate_breakdown=="Industry A - Agriculture, forestry and fishing and Gender Female")%>%
  filter(!graduate_breakdown=="Industry A - Agriculture, forestry and fishing and Gender Male")%>%
  filter(!graduate_breakdown=="Industry R,S,T,U - Other services and Gender Female")%>%
  filter(!graduate_breakdown=="Industry R,S,T,U - Other services and Gender Male")%>%
  filter(!graduate_breakdown=="Industry B,D,E - Energy and water and Gender Male")%>%
  filter(!graduate_breakdown=="Industry B,D,E - Energy and water and Gender Female")%>%
  filter(!graduate_breakdown=="Industry NA and Gender Male")%>%
  filter(!graduate_breakdown=="Degree class Other")%>%
  filter(!graduate_breakdown=="Degree class Pass")

#For Sample Sizes <=30 need to suppress the median salary values in the data, Footnote should be Data not available due to small sample size (fewer than 30)		
#use ":" to label these value
All_grad_sal_x_i<-All_grad_sal_x_h%>%
  mutate(median=ifelse(sample_size >=31,median, ":"))

#For the 25th April 2019 published GLMS, the sample size for Industry only was published so
#other sample sizes need to be surpressed to be consistent with this
All_grad_sal_x_j<-All_grad_sal_x_i%>%
  mutate(sample_size=ifelse(grepl("Industry",All_grad_sal_x_i$graduate_breakdown),sample_size,"Not published"))


#organise the order of collumns to fit the standard for the platform

All_grad_sal_x_fin<-All_grad_sal_x_j%>%
  select("time_period",
         "time_identifier",
         "geographic_level",
         "country_code",
         "country_name",
         "graduate_group",
         "graduate_characteristic",
         "graduate_breakdown",
         "median",
         "sample_size")


All_grad_sal_x_fin$time_period <- as.character(All_grad_sal_x_fin$time_period)

#For rows in the data frame All_grad_sal_x_fin that contain graduate_characteristic == "Age Group " need to be filtered
#out into another dataframe for ease of use on the EES platform. Make two data frames

#Salaries for different AgeGroups of Graduates
#Employment Rate for different AgeGroups of Graduates will be combined with salaries in section Employment Rate & Salary
GradBreak_salary_AgeGroup<-All_grad_sal_x_fin%>%
  filter(graduate_characteristic == "Age Group")

#Salaries for different Demographics of Graduates that DO NOT include Demographics=AgeGroup
GradBreak_salary_Demographics<-All_grad_sal_x_fin%>%
  filter(!graduate_characteristic == "Age Group")


#----Qa: Salaries----
#a)Is the totol number of rows retained after filtering ?
if(nrow(All_grad_sal_x_fin)==nrow(GradBreak_salary_Demographics)+nrow(GradBreak_salary_AgeGroup)) {
  print("The script gives the same totols")}else{
    print("The script does not give the same totols")
  }

#b)Do I have missing data ?
#Bind together the same smaller data sets.Use âanti-joinâ between the bound tables and the orginal table to see if there are no matches found i.e
#you are looking to see if there is data in the orginal table that is not found in the smaller tables

GradBreak_salary_bind<-rbind(GradBreak_salary_Demographics,GradBreak_salary_AgeGroup)
if(nrow(anti_join(GradBreak_salary_bind,All_grad_sal_x_fin))==0){
  print("There is no missing data after filtering")}else{
    print("There is missing data after filtering")
  }

#Save Salary Data for Graduate Breakdown for all demographics except age group
readr::write_csv(GradBreak_salary_Demographics,
          paste0(filepath, "Outputs_folder/EES_csv/GradBreak_salary_Demographics_",year,".csv"),quote = FALSE)



#QA Check for Na's in outputs
apply(GradBreak_Employ_Demographics, 2, function(x) any(is.na(x)))

#---GradBreak_salary_Demographics: meta file (Graduate Breakdown not including Age Group)-----

GradBreak_salary_Demographics.meta <- data.table("col_name" = c("median",
                                                                "sample_size",
                                                                "graduate_group",
                                                                "graduate_breakdown"),
                                                 "col_type" = c("Indicator",
                                                                "Indicator",
                                                                "Filter",
                                                                "Filter"),
                                                 "label" = c("Median Salary to the nearest (£500)",
                                                             "Sample Size",
                                                             "Graduate Age Group",
                                                             "Graduate Breakdown"),
                                                 "indicator_grouping" = c("","", "",""),
                                                 "indicator_unit" = c("£","","",""),
                                                 "indicator_dp" = c("","","",""),
                                                 "filter_hint" = c("", "","Select the Graduate Age Group you interested in","Select the Graduate characteristic you interested in"),
                                                 "filter_grouping_column"=c("","","","graduate_characteristic"))



#OPTIONAL:encoding step------------------
#Only use if using Source function to run this script. Since source function automatically adds encoding 
GradBreak_salary_Demographics2.meta <- as.data.frame(sapply(GradBreak_salary_Demographics.meta, function(x) gsub("Â£", "£", x)))

write.csv(GradBreak_salary_Demographics2.meta,
          paste0(filepath, "Outputs_folder/EES_csv/GradBreak_salary_Demographics_",year,".meta.csv"),quote = FALSE,row.names = FALSE)


#------EMPLOYMENT RATE & SALARY for graduate_characteristic == Age Group----

#Combine Employment Rates and Salaries in one data soure for graduate_breakdown==AgeGroup

#Select salary info with labelling information (note salary information for AgeGroup
#==60-64  and   sample size  has historically not been published so do not include it)
GradBreak_salary_AgeGroup_a<-GradBreak_salary_AgeGroup%>%
  filter(!graduate_breakdown=="Age Group 61-64", !graduate_breakdown=="Age Group 16-20")%>%
  select("graduate_group","graduate_breakdown","median")

#Combine Employment rates and salary information into one table
GradBreak_AgeGroup<-left_join(GradBreak_Employ_AgeGroup,GradBreak_salary_AgeGroup_a)

#QA left join
#a)Is the totol number of rows retained after joining ?
if(nrow(GradBreak_AgeGroup)==nrow(GradBreak_salary_AgeGroup_a)&nrow(GradBreak_AgeGroup)==nrow(GradBreak_Employ_AgeGroup)) {
  print("The script gives the same totols")}else{
    print("The script does not give the same totols")
  }

#b)Do I have missing data ?
#Since left joining by "graduate_group" & "graduate_breakdown" and this does this
#without returning an erroe then there is no missing data

#For   GradBreak_AgeGroup$graduate_group = "Graduates 21-30" & GradBreak_AgeGroup$graduate_breakdown="Age Group 21-30"
# compared to GradBreak_AgeGroup$graduate_group = "Graduates 16-64" & GradBreak_AgeGroup$graduate_breakdown="Age Group 21-30"
#Gives the same information, so remove row where GradBreak_AgeGroup$graduate_group = "Graduates 21-30" & GradBreak_AgeGroup$graduate_breakdown="Age Group 21-30" and 
#remove column  GradBreak_AgeGroup as this becomes redundant

GradBreak_AgeGroup_fin<-GradBreak_AgeGroup%>%
  filter(!graduate_group == "Graduates 21-30")%>%
  select(!graduate_group)

#GradBreak_AgeGroup_fin<-GradBreak_AgeGroup_fin[,!(names(GradBreak_AgeGroup_fin)=="graduate_group")]  

#Save Gradbreakdown Employment Rates that do not include Demographics=Age Group 

write_csv(GradBreak_AgeGroup_fin,
          paste0(filepath, "Outputs_folder/EES_csv/GradBreak_AgeGroup_",year,".csv"),quote = FALSE)



#QA, check for Na's in CSV outputs 
print("Are there any Na's in the table ?")
print(apply(GradBreak_AgeGroup_fin, 2, function(x) any(is.na(x))))

#-----meta data file:EMPLOYMENT RATE & SALARY in graduate_characteristic == Age Group----

GradBreak_AgeGroup.meta <- data.frame("col_name" = c("Employment_rate",
                                                     "High_skill_emp_rate",
                                                     "Unemployment_rate",
                                                     "Inactivity_rate",
                                                     "median",
                                                     "graduate_breakdown"),
                                      "col_type" = c("Indicator",
                                                     "Indicator",
                                                     "Indicator",
                                                     "Indicator",
                                                     "Indicator",
                                                     "Filter"),
                                      "label" = c("Employment Rate",
                                                  "High Skill Employment Rate",
                                                  "Unemployment Rate",
                                                  "Inactivity Rate",
                                                  "Median Salary to the nearest (£500)",
                                                  "Graduate Breakdown"),
                                      "indicator_grouping" = c("","","","","",""),
                                      "indicator_unit" = c("%","%","%","%","",""),
                                      "indicator_dp" = c("1","1","1","1","",""),
                                      "filter_hint" = c("","","","","","Select the Graduate breakdown you are interested in"),
                                      "filter_grouping_column"=c("","","","","",""))



readr::write_csv(GradBreak_AgeGroup.meta,
          paste0(filepath, "Outputs_folder/EES_csv/GradBreak_AgeGroup_",year,".meta.csv"),quote = FALSE)

