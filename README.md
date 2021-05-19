## **Background**  
This R project produces supporting data for the Graduate Labour Market (GLMS) 2020 publication, where 2020 refers to the survey year.

This project contains code scripts that:  
1. Imports quarterly Labour Force Survey (LFS) data sets 
2.  Finds the employment rate (from individuals that work full-time and part-time), median salaries (from individuals that work full-time) and the proportion of people that work part-time for different graduates types based on defined characteristics.  

The LFS data is from sample surveys so the statistics produced are always estimates.

## **Requirements**  
i .  *Software requirements*  
 Installation of R 3.6.2 or higher  
 Installation of RTools40 or higher  

 ii.  *Data Requirements*  
Access to LFS Quarterly Datasets,  
this is only accessible to specified users.

ii.  Programming skills required (for editing or troubleshooting)    
R at an intermediate level

### **How to use**  
*Packages*  
Package control is handled using renv. If this is your first time using the project  
you may need to run renv::restore() 

*Data*  
The project sends and receives data from an internal area which is restricted to named
users.

### **Code Description**  
The main script to run the program is called **GLMS_outputs.R**

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

### **Contributing**  
If you're interested in contributing to this project, get in touch with us  The GitHub repo is only a mirror and any changes made directly to it will be overwritten when the next update is mirrored.

### **Contact**  
statistics.development@education.gov.uk  



