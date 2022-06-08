#----Introduction----

#This code imports LFS data from SPSS files and finds  employment rates, median salaries and the proportion of
#of employed people that work part time based on various characteristics. The analysis looks at
#these labour market statistics for different graduate types.


#Contents----
#1. This sections sources functions
#2. set year range and filepaths for data sets 
#3  Gets quarterly datasets into the global environment


#1.----Functions used within the code----           

#1.1----SPSS data to R data:Upload saved rds data ----

#Add in the function to change SPSS data to R data and to upload the converted saved datasets into the global enviroment 2.1.a.
#This code will also give a warning if the SPSS data has not convert well into R data. If this happens do 1.2 OPTIONAL:csv datasets
#to R data. 
source("Code/GLMS/GLMS_from_LFS_data.R")

#1.2.----OPTIONAL: csv datasets to R data----
#If spss datasets do not read well use this code to convert to .rds files after
#they are manually converted to .csv files. 
# Note you need to input the names of the .csv files into this page script. Then run source("Code/GLMS_from_LFS_data.R") to
#check all .rds files have been read in correctly
#source("Code/GLMS/csv_to_rds_data.R")

#1.3----Recode variables----
#Add in the function to recode variables in the LFS data to those used in the publication
source("Code/GLMS/GLMS_Recode_Variables1.R")

#1.4----Employment Rate----
#Add in the function to calculate the employment rate
source("Code/GLMS/GLMS_Employment_Rate_levels2.R")  #this also adds columns to output the 
#employment,unemeployment and population levels that corresponds to the sample size.

#1.5----Salaries----
#Add in the function to calculate median (and 25th & 75th percentiles)
source("Code/GLMS/GLMS_Salaries.R")


#2.----Set data range and get datasets ----                           
start_year = 2007 #Which year do you want to use LFS data from? 
end_year = 2021 #Which year do you want the LFS data up to?

filepath <- "W:/HE-ALTERNATIVE-PROVIDERS/GLMS/GLMS 2021v1/"

#2.1---- OPTIONAL STEP  Convert SPSS datasets to R:output ----
#Convert all LFS data from Q1 of the start year to Q4 of the end year into R data format
#UNLESS this has been previously completed and files are in the shared area. In which case skip this step.

#sapply(start_year:end_year, function(y)sapply(1:4,function(x)readLFS(x,y)))

#If analysing year 2020, Q1,Q2  have been re-weighted 
#use the re-weighted datasets
#ifelse(2020 %in% start_year:end_year,
       #sapply(2020:2020, function(y)sapply(1:2,function(x)readLFS_reweight(x,y))), NA)     


#2.1.a----Upload .rds files to global enviroment:Output----
#After Import/If you have the .rds files already run
sapply(start_year:end_year, function(y)sapply(1:4, function(x) read_LFS_from_project(x,y)))

#if using 2020 data, Check that the re-weighted 2020 Q1 and Q2 datasets have been saved and loaded to the global environment
#The re-weighted datasets contain columns with _old suffix 
#ifelse(2020 %in% start_year:end_year,any(grepl("_old", names(Q1_2020))), NA )
#ifelse(2020 %in% start_year:end_year,any(grepl("_old", names(Q2_2020))), NA )

#2.2----for Q1_2015: Assign values labels to numbers -----
#Older versions of HIQUAL15 don't have value labels- just numbers- but we can use the value labels 
#of Q1_2016 to recode the numbers as text strings.

library(data.table) 

#check that if you have the value labels loaded from 2016 datasets if analysing Q1_2015
ifelse(!(2016 %in% start_year:end_year) & 2015 %in% start_year:end_year,
       "warning need to load 2016 data",NA)

#assigns missing values to Q1_2015 but you need to make sure datasets 2016 are loaded 
#too as the value labels are taken from this
ifelse(2015 %in% start_year:end_year & 2016 %in% start_year:end_year,
       source("Code/GLMS/assign.R"),NA)
       

#3.Outputs:analysis  -----

#3.1----Recode the LFS variables to those used in the publication----

sapply(start_year:end_year, function(y)sapply(1:4, function(x)recode_variables(x,y)))

#3.2----Output employment rates-------
#This outputs the proportion in employment for each quarter. The script also finds the average mean employment rate for a year.
#This reduces seasonality in the data

sapply(start_year:end_year, function(y)sapply(1:4, function(x)Employment_rate(x,y)))

#Create headline stats for each year (mean average of rates in each quarter)

sapply(start_year:end_year,function(x)year_average_employment_rate(x))
list<-lapply(start_year:end_year,function(x)get(paste0("Headline_",x)))

Yearly_employment<-Reduce(rbind,list)

write.csv(Yearly_employment,
          paste0(filepath,"Outputs_for_GLMS/Yearly_employment_",start_year,"_",end_year,".csv"))

saveRDS(object = Yearly_employment,
        file = paste0(filepath, "Outputs_for_GLMS/EES_rds/Yearly_employment_",start_year,"_",end_year,".rds"))

#And employment rate for the graduate breakdowns
Graduate_breakdown_year(end_year)

#3.3----Output  salaries----
#This uses the data for the whole year to output median salaries
sapply(start_year:end_year, function(y)Average_Salaries(y))

list<-lapply(start_year:end_year,function(x)get(paste0("Sal_",x)))

Yearly_salaries<-Reduce(rbind,list)

write.csv(Yearly_salaries,
          paste0(filepath, "Outputs_for_GLMS/Yearly_salaries_",start_year,"_",end_year,".csv"))

saveRDS(object = Yearly_salaries,
        file = paste0(filepath, "Outputs_for_GLMS/EES_rds/Yearly_salaries_",start_year,"_",end_year,".rds"))

#And salaries for the graduate breakdowns

Graduate_breakdown_salaries(end_year)

#3.4----Output: Proportion of Part time Workers------
#Add in scripts to find the proportion of employed people that work part time.

# Non graduates with defined demographic breakdowns
source("Code/GLMS/GLMS_proportion_grad_brk.R")
# Graduates with demographic breakdowns
source("Code/GLMS/GLMS_proportion_nongrad_brk.R")
# Graduates and  Non Graduates by  Working Age (16-64) 
source("Code/GLMS/GLMS_proportion.R")

#3.5----Output:Confidence Intervals----

source("Code/GLMS/GLMS_confidence_intervals.R")

