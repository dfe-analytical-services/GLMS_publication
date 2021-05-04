#---Introduction----
#Graduate Labour Market Statistics has historically been published on GOV.UK.
#DfE have produced a newplatform called Explore Education Statistics (EES) where all DfE publications will eventually be published. 
#The reason for this change is to improve Writing and Visualising Statistics as outlined here
#https://rsconnect/rsc/stats-production-guidance/cd.html

#----Description of Script----
#This is the master script to covert the outputs of the GLMS.prj from script GLMS_outputs.R into the format that is
#(i)  Readable by the EES platform, see see https://rsconnect/rsc/stats-production-guidance/
#(ii) Is consistent with the historic publications of the GLMS supporting data on GOV.UK 
#(iii) Create corresponding meta files for each data file for the EES platform

#----changeable Variables-----
#Make sure these variables are already in your global environment as the scripts below need them to run: 
#filepath <- "W:/HEA-SLR-TEAM/GLMS/GLMS_2020V1/"
#year <- 2020     #Input year of GLMS publication
#label_EES <- 200720 #labels for EES platform
#end_year<-2020
#start_year<-2007

#----Convert data files for EES----


#Year 2019 Employment Rates and Salaries for Grads,PostGrad and Non Grad with defined demographic profiles
source("Code/EES_Code/Grad_BreakDown.R")

#Yearly Employment Rates and Salaries for Grads,PostGrad and Non Grad
source("Code/EES_Code/TimeSeries.R")

#Proportions of Part time workers timeseries
source("Code/EES_Code/timeSeries_proportions.R")

#Quarterly Employment Rates and confidence intervals for Grads,PostGrad and Non Grad
source("Code/EES_Code/Quart_TimeSeries.R")




#Check that the csv files are created in the shared area
