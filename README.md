**Introduction**  
These scripts produce supporting data for the Graduate Labour Market 2020 publication, where 2020 refers to the survey year.

**Requirements**  
i . *Software requirements*  
 Installation of R 3.6.2 or higher  
 Installation of RTools40 or higher  

 ii. *Data Requirements*  
Access to Labour Force Survey Quarterly Datasets  

iii. folder structure  
Create an area to save data files in and then:  
Create Output_folder containing EES_rds, EES_csv folders,   
Create dataset folders  Rds_datasets, csv_datasets.  

**Code Description**  
The main script is called **GLMS_outputs.R**

The main script calls on the following scripts:  
a. GLMS_from_LFS_data.R  
This script imports the SPSS data sets and uploads saved rds files to the global enviroment.  

b. csv_to_rds_data.R (optional step)  
This script is used to convert * csv files into * rds files.  

c. assign.R  
This scripts assigns missing values corresponding to the HIQUAL15 variable (highest qualification) for dataset quarter 1, year 2015.  

d. GLMS_Recode_Variables.R  
This script recodes the variables in the LFS to those used in the publication.  

e. GLMS_Employment_Rate.R  
This scripts calculates the employment rate.  

f. GLMS_Salaries.R  
This script calculates the 25th and 75th percentile median salaries.  

g. GLMS_proportion.R,  
h. GLMS_proportion_nongrad_brk.R,  
i. GLMS_proportion_grad_brk.R  
These scripts estimate the proportion of non gradautes and graduates that work part time.  

j. GLMS_confidence_intervals.R  
This script calculates the confidence intervals for quarterly employment rates.  

 k. Grad_BreakDown.R, Quart_TimeSeries.R, timeSeries_proportions.R   
This script converts all analysis outputs into the format that is readable by the Explore Education Statistics (EES) platform    

