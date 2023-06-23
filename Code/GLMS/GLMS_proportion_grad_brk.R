#--------Introduction----
#This scripts finds the what proportion of workers that work, work part-time(PT) 
#The PT workers proportion is found for Graduates with demographic breakdowns of AgeGroup and SEX

#--------Contents----------
#1. Modify quarterly data sets to select the most up to date people weight. Summarise counts/sample size by demographic (used for manual checks)
#2. a function that applies demographic breakdowns to modified quarterly data sets, then Summarises the counts/sample size.
#3. finds the Proportion of PT workers found by Graduate breakdown and year
#4. QA scripts for quarterly proportion function
#5. finds the on average proportion of workers that work PT  by graduate breakdown and year
#6. Summary table of average proportion of graduate part-time workers by year 

#NOTE PT = part-time, FT = full-time, WK = work

#----load library----
library("reshape2")
library("rlang")


#1.------Modify Datasets----
# Employment_counts_by function modifies quarterly datasets so that the most up to date people weight "PWT" is selected
# and only the people that have identidied themselves with a PT or FT work pattern are included
Employment_counts_by<-function(dataset,demographic){
  weight<-tail(sort(names(dataset[grep("PWT", names(dataset))])),1)
  dataset<-dataset[which(dataset[,weight]>0 & !is.na(dataset$FTPTWK)),]
  as.data.frame(
    dataset%>%
      group_by_(demographic,"Graduate_Type","ILODEFR","SOCHE")%>%
      dplyr::summarise(sample=n(),
                       population=sum(!!sym(weight))))
}

#2.------Variable splits/breakdowns-----
variable_splits<-function(dataset, variable_y){
  
  #dataset has been modified by Employment_counts_by(dataset, demographic)
  #i.e demographic = "SEX". Then counts, sample sizes and employment rates are found for various demographic
  #breakdowns
  
  #find how many are employed by demographic 
  dataset[which(dataset$ILODEFR=="In employment"),] %>%
    group_by_(variable_y)%>% summarise(employed.count=sum(population), employed.sample=sum(sample))->in_employment
  
  list<-list(in_employment)
  
  total_count<-Reduce(function(x, y) merge(x, y, all.x=T, 
                                           by=c(variable_y)), list, accumulate=F)
  
  
  pt_proport_breakdown<-data.frame( total_count[variable_y],         #labels demographic breakdown is i.e for demographic= SEX, breakdown is male, female
                                    total_count$employed.sample,   #employment sample by demographic breakdown
                                    total_count$employed.count)    #employment count by demographic breakdown
  
  colnames(pt_proport_breakdown)<-c("Breakdown",
                                    "Employed_sample",
                                    "employment_total")
  
  return(pt_proport_breakdown)
}

#3.------PT proportions for Graduate breakdowns---------------

pt_proport_breakdown<-function(quarter,year){
  
  #select dataset: for example if quarter =1 and year =2016 this will get Q1_2016 and set this to 'dataset'
  dataset <- get(paste0("Q",quarter,"_",year))
  
  #3.1 ----Gradautes: select graduates and include everyone that works full-time (FTWK) and part-time (PTWK)---------
  
  #select  just graduates from dataset who work both full-time (FTWK) and part time (PTWK)
  dataset <- dataset[which(dataset$Graduate_Type=="Graduate"),]
  
  #quarterly dataset is modified by Employment_counts function where demographic = AgeGroup,  then the dataset is
  #piped into the variable split function where variable_y = "AgeGroup" . A table summarising statistics on graduates
  # by AgeGroup breakdown is made.
  Employment_counts_by(dataset,"AgeGroup")%>%  variable_splits(.,"AgeGroup")-> Age_group_levels
  
  # 16-20 and 61-64 need to filtered as these breakdowns gives errors when joining PT and FT data .
  #because some  datasets do not have information on this breakdown.
  Age_group_levels %>% filter(Breakdown != "16-20" & Breakdown !="61-64" ) -> Age_group_levels2 
  Age_group_levels2$Population <- "AgeGroup"    #describes the population eg AgeGroups
  
  
  #demographic "SEX"
  Employment_counts_by(dataset,"SEX") %>% variable_splits(.,"SEX")->Gender_levels
  Gender_levels$Population <- "SEX" #describes the population eg sex
  
  #combines all the graduate FTPTWK breakdowns in a table
  quarter_lev <- rbind(Age_group_levels2, Gender_levels)
  
  #3.1a. save QA join of FTPTWK demographic data to join_c-------------
  NROW(quarter_lev) == NROW(Age_group_levels2) + NROW(Gender_levels)&
    NROW(unique(quarter_lev)) == NROW(unique(Age_group_levels2)) + NROW(unique(Gender_levels)) -> join_c
  
  #labels the table with the group it belongs to 
  quarter_lev$Group<-"FTPTWK"
  
  #3.2 -----Part time (PTWK)  Working  Graduates--------
  
  #dataset selects  graduates that work part-time and find levels for each demographic  
  dataset <- dataset[which(dataset$Graduate_Type == "Graduate" & dataset$FTPTWK == "Part-time"),]
  
  Employment_counts_by(dataset,"SEX") %>% variable_splits(.,"SEX")->Gender_levels
  Gender_levels$Population<-"SEX"
  
  Employment_counts_by(dataset,"AgeGroup")%>% variable_splits(.,"AgeGroup")->Age_group_levels
  
  # 16-20 and 61-64 need to filtered as gives errors when joining PT and FT data .
  #because some  datasets do not have information on this breakdown.
  Age_group_levels %>% filter(Breakdown !="16-20" & Breakdown !="61-64" )->Age_group_levels2 
  
  Age_group_levels2$Population<-"AgeGroup"    #describes the population eg AgeGroups
  
  #comines all the graduate PTWK breakdowns in a table
  quarter_lev_2 <- rbind(Age_group_levels2, Gender_levels)

#3.2a. save QA joins of PT demographic data to join_b-------------
  NROW(quarter_lev_2) == NROW(Age_group_levels2) + NROW(Gender_levels)&
    NROW(unique(quarter_lev_2)) == NROW(unique(Age_group_levels2)) + NROW(unique(Gender_levels)) ->join_b
  
  #label table
  quarter_lev_2$Group <- "PTWK"
  
  #3.3 ----Join Graduate PTWK levels and FTPTWK levels in a table-----
  quarter_lev_3 <- inner_join(quarter_lev, quarter_lev_2, by = c("Population","Breakdown"))
  
  #3.3a.save QA PT and FTPT data to join_a----------
  NROW(quarter_lev_3) == NROW(quarter_lev)&
    NROW(quarter_lev_3) == NROW(quarter_lev_2)&
    NROW(unique(quarter_lev_3)) == NROW(unique(quarter_lev))&
    NROW(unique(quarter_lev_3)) == NROW(unique(quarter_lev_2)) ->join_a
  
  #3.4 save summary of QA of all table joins in this function to join_d----
  join_a & join_b & join_c -> join_d
  #save to global environment
  assign(paste0("QA_table_join_Grad_pt_Q",quarter,"_",year),join_d,envir = globalenv())

  
  #3.54.----PT proportions for all Grad breakdowns---------------
  quarter_lev_4<-quarter_lev_3 %>%
    mutate(pt_employed_proportion = employment_total.y/employment_total.x)%>%
    select(Breakdown,                                                #order the columns so easier to QA
           Population, 
           Group.x,
           employment_total.x,
           Group.y,
           employment_total.y,
           Employed_sample.x,
           Employed_sample.y,
           pt_employed_proportion)
  
  
  
#save to global environment 
  assign(paste0("Grad_pt_proportion_bk_Q",quarter,"_",year),quarter_lev_4,envir = globalenv())

  

  
  
}
#output pt_proport_breakdown
sapply(start_year:end_year, function(y)sapply(1:4, function(x)pt_proport_breakdown(x,y)))


#4.----QA checks for quarterly data sets------------------


#4.1 function to check the quarterly data table joins
QA_quart_Gradjoin <- function(quarter,year){
  
  dat_1 <- get(paste0("QA_table_join_Grad_pt_Q",quarter,"_",year)) 
  
  
  #if number of rows retained when making quarterly data sets QA joins pass
  print(ifelse(dat_1,
         paste0("table_joins for Grad_pt_Q",quarter,"_",year," has passed QA checks"),
         paste0("Warning: table_joins for Grad_pt_Q",quarter,"_",year," has not passed QA checks")))
  
}

#outputs for QA
sapply(start_year:end_year, function(y)sapply(1:4, function(x)QA_quart_Gradjoin (x,y)))


#4.2 other check_1/4 data set
#(i)Sample sizes
#Are the sample sizes large enough ? Historically yearly data sets should have a sample size of >=31. Decide that quarterly Part-part-time proportions
#should be based on samples sizes >=31
#(ii) Are there any NA's or 
#(iii)Zeros in the tables

QA_quart_Gradproportion <- function(quarter,year){
  
  dat <- get(paste0("Grad_pt_proportion_bk_Q",quarter,"_",year)) 
    
  
  #check all quarterly data sets for zeros and NA's and that sample sizes are >=31
  print(ifelse(all(!is.na(dat)) & all(dat != 0) & all(dat[,7:8] >= 31),
         paste0("Grad_pt_proportion_bk_Q",quarter,"_",year," has passed QA checks"),
         paste0("Warning: Grad_pt_proportion_bk_Q",quarter,"_",year," has not passed QA checks")))
  
}

#outputs for QA
sapply(start_year:end_year, function(y)sapply(1:4, function(x)QA_quart_Gradproportion (x,y)))



#5.----Average Part-time proportions by year----

pt_prop_breakdown_year<-function(year){
  
  #select dataset: for example if quarter =1 and year =2016 this will get Q1_2016 and set this to 'dataset'
  dataset1<-pt_proport_breakdown(1,year)
  dataset2<-pt_proport_breakdown(2,year)
  dataset3<-pt_proport_breakdown(3,year)
  dataset4<-pt_proport_breakdown(4,year)
  
  year_data<- rbind(dataset1,dataset2,dataset3,dataset4)
  
  #5.1 QA rbind:yearly data ------  
  NROW(year_data) == NROW(dataset1) + NROW(dataset2) + NROW(dataset3) + NROW(dataset4) &
    NROW(unique(year_data)) == NROW(unique(dataset1))+ NROW(unique(dataset2)) +NROW(unique(dataset3))+ NROW(unique(dataset4)) ->year_data_prop
  
  ifelse(year_data_prop, paste0("pass QA_join: Grad_prop_", year),
         paste0("Warning: error in join Grad_prop_", year)) ->year_data_prop
  # save to global environment 
  assign(paste0("join_grad_data_Qa_",year),year_data_prop,envir = globalenv())
  
  #find the mean of the proportion of pt workers for a year
  means<-ddply(year_data, .(Population, Breakdown), colwise(mean, .(pt_employed_proportion),na.rm=TRUE))
  
  #find the  sample size and population totals that the sample size represents
  samples<-ddply(year_data, .(Population, Breakdown),colwise(sum, .(Employed_sample.x, 
                                                                    Employed_sample.y),na.rm=TRUE))
  #put means and sample size, populations in a tables
  output<-merge(x=means, y=samples, by=c("Population","Breakdown"), all=TRUE)
  
  output<-output[c("Population",
                   "Breakdown",
                   "pt_employed_proportion",
                   "Employed_sample.x",
                   "Employed_sample.y")]
  
  
  colnames(output)<-c("Population",
                      "Breakdown",
                      "pt_employed_proportion",
                      "FTPT_employed_sample",
                      "PT_employed_sample")
  
  output$time_period<-paste0(year)
  
  assign(paste0("Grad_pt_proportion_",year),output,envir = globalenv())


}

sapply(start_year:end_year,function(x)pt_prop_breakdown_year(x))





#6.-----Summary table:Average part-time  proportions ----

#Create  stats for each year (mean average of rates in each quarter)

grad_list_yr<-lapply(start_year:end_year,function(x)get(paste0("Grad_pt_proportion_",x)))
grad_yr_proportion<-Reduce(rbind,grad_list_yr) %>%
  mutate(graduate_type = "Graduate")            # label with graduate type

saveRDS(object = grad_yr_proportion,
        file = paste0(filepath, "Outputs_for_GLMS/EES_rds/grad_yr_proportion_",start_year,"_",end_year,".rds"))


#6.1 -----QA summary table----
list_Grad_yr<- lapply(start_year:end_year,function(x)get(paste0("join_grad_data_Qa_",x)))
print(Grad_yr<-Reduce(rbind,list_Grad_yr))

print(" Are there any Na's or zeros in the final Grad summary table ")
print(apply(grad_yr_proportion, 2, function(x) any((x==0))))
print(apply(grad_yr_proportion, 2, function(x) any(is.na(x))))

