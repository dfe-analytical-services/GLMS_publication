#--------Introduction----
#This scripts finds the what proportion of those that are employed that work part-time(PT) 
#The PT workers proportion is found for working age Graduates and Non-Graduates

#--------Contents----------
#1. a function that takes modified quarterly data set and finds counts/sample size by graduate type = Graduate and Non Grad
#   and calculates the proportion of those employed that work part-time(PT)
#2. Modifies quarterly datasets by selecting the relevant people weight. This is used in pt_proportion_tb
#   function.
#3. QA outputs of proportion function
#4. finds the average proportion of workers that work PT  by graduate type and year
#5. Summary table of average proportion of graduate types that work part-time for a  year 

#NOTE PT = part-time, FT = full-time, WK = work

#----load library----
library("reshape2")
library("rlang")

#----1. Part-time(PT) Proportions data frame ----
#The all dataset has been modified in step 2 so as to ensure the correct people weight is used

pt_proportion_tb_a<-function(all){
  
  #find the sample size and no. of people in each graduate type 
  all %>%
    group_by(Graduate_Type)%>% summarise(total=sum(freq),sample=sum(sample))->full_pop
  

    #Make in_employment table by select those in employment and group by grad type. Then find
    #sample size etc.
   
    all[which(all$ILODEFR=="In employment"),] %>%
      group_by(Graduate_Type)%>% summarise(count=sum(freq),sample=sum(sample))-> in_employment_FTPTWK
    
    
 
    #Make in_employment table for PT workers only by select those in employment and those in PT work, group by grad type. 
    #Then find sample size etc. 
    
    all[which(all$ILODEFR=="In employment" & all$FTPTWK == "Part-time"),] %>%
      group_by(Graduate_Type)%>% summarise(count=sum(freq),sample=sum(sample)) ->in_employment_PTWK
    
    #Make a data frame for employed stats
    pt_proportion_a<-data.frame( in_employment_FTPTWK$Graduate_Type,                              #names of graduate_type for each grad type
                                in_employment_PTWK$count,                                    #all employed sample size
                                in_employment_FTPTWK$count,                                     #all employed count
                                in_employment_PTWK$sample,                                      #PT workers sample size
                                in_employment_FTPTWK$sample,                                       #PT workers count
                                in_employment_PTWK$count/in_employment_FTPTWK$count)            # proportion of PT workers that are in the employed population                                                                 #full_population by grad type
  
#collumn names
    colnames(pt_proportion_a)<-c("graduate_type",
                                "employed-total-PT","employed-total-FTPT",
                                "employed-sample-PT","employed-sample-FTPT",
                                "pt_employed_proportion")
    return(pt_proportion_a)
    

  }

#----2. pt_proportion function----
#Employment_rate function makes datasets  (all) which are used pt_proportion_tb function
#the most recent people weight is picked and samples/counts for defined population is found

pt_proportion_a<-function(quarter, year){

  #get the dataset from the global environment
    dataset<-get(paste0("Q",quarter,"_",year))
    
  #find the weight variable and just include people that have identified themselves 
  #with a working pattern
    weight<-tail(sort(names(dataset[grep("PWT", names(dataset))])),1)
    dataset<-dataset[which(dataset[,weight]>0 & !is.na(dataset$FTPTWK)),]
  
  # count all people by age, graduate type, ILO employment status and SOC group: weight people by the weight variable  (such as PWT16)
  #also note sample size
  all<-dataset%>%
    group_by(AgeGroup,Graduate_Type,ILODEFR,SOCHE,FTPTWK) %>%
    dplyr::summarise(freq=sum(!!sym(weight)), sample=n())
  
  #take all dataset and output a new dataframe using pt_proportion function
  pt_proportion<-pt_proportion_tb_a(all)
  
  #save dataframe part-time proportions to global environment
  assign(paste0("pt_proportion_Q",quarter,"_",year),pt_proportion,envir = globalenv())

  
}

#Range of quarterly datasets 
sapply(start_year:end_year, function(y)sapply(1:4, function(x)pt_proportion_a(x,y)))

#3. ----QA: quarterly datasets-----
#Things to check include:
#Are the sample sizes large enough ? 
# Check if quarterly data have samples sizes >=31

QA_quart_proportion <- function(quarter,year){
  
  dat <- get(paste0("pt_proportion_Q",quarter,"_",year)) %>%
    filter (graduate_type == "Graduate" | graduate_type == "Non Grad") # Just select Grads and Non Grads
  
  
  #check all quarterly data sets for zeros and NA's and that sample sizes are >=31
  print(ifelse(all(!is.na(dat)) & all(dat[,4:5] != 0) & all(dat[,4:5] >= 31),
         paste0("pt_proportion_Q",quarter,"_",year," has passed QA checks"),
         paste0("Warning: pt_proportion_Q",quarter,"_",year," has not passed QA checks")))
  
}

#outputs for Qa
sapply(start_year:end_year, function(y)sapply(1:4, function(x)QA_quart_proportion (x,y)))





 
#3.Headline stats:yearly dataset-------------------------

#quarterly datasets combined to yearly datasets and mean rates and sample sizes, counts found
  year_average_pt_tb<-function(year){
  
  #select dataset: for example if quarter =1 and year =2016 this will get Q1_2016 and set this to 'dataset'
  dataset1<-get(paste0("pt_proportion_Q1_",year))
  dataset2<-get(paste0("pt_proportion_Q2_",year))
  dataset3<-get(paste0("pt_proportion_Q3_",year))
  dataset4<-get(paste0("pt_proportion_Q4_",year))
  
  year_data_proportion<-rbind(dataset1,dataset2,dataset3,dataset4)%>%
    filter (graduate_type == "Graduate" | graduate_type == "Non Grad")       #filter for Graduate amd Non Grads
  
  
  #group by age range and graduate type (i.e population), summarise the statistics for 
  #the year
  
  year_data_proportion%>%
    group_by(graduate_type)%>%summarise(pt_employed_proportion = mean.default(pt_employed_proportion),
    employed_PT_sample=sum(`employed-sample-PT`),
    employed_PT_total=sum(`employed-total-PT`),
    employed_FTPT_sample=sum(`employed-sample-FTPT`), 
    employed_FTPT_total=sum(`employed-total-FTPT`)) -> year_average_pt_proprtion
              
              
  
  
  #label the year_avearge table with year
  year_average_pt_proprtion$time_period<-year
  
  assign(paste0("pt_proportion_",year),year_average_pt_proprtion,envir = globalenv())
  
  
}
sapply(start_year:end_year,function(x) year_average_pt_tb(x))

#4. ----Summary table pt proportions for Grads and non grads by year-----
#Make a timeseries for pt proportions Grads and non grads 
list_timeseries_pt_prop <-lapply(start_year:end_year,function(x)get(paste0("pt_proportion_",x)))
timeseries_pt_proportion <- Reduce(rbind,list_timeseries_pt_prop) %>%
  mutate(Population = "AgeGroup")%>%
  mutate(Breakdown = "16-64")

saveRDS(object = timeseries_pt_proportion,
        file = paste0(filepath, "timeseries_pt_proportion_",start_year,"_",end_year,".rds"))


#4.1 ----QA summary table----
#Check for NA's and zeros in the table
print("Are there any zeros or Na's in the summary table ")
print(apply(timeseries_pt_proportion, 2, function(x) any((x==0))))
print(apply(timeseries_pt_proportion, 2, function(x) any(is.na(x))))


  

