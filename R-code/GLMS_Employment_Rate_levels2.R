#----load library----
library("reshape2")
library("rlang")

#----Employment rate by demographic:contents-----

#1.Headline_stats_function finds employment rates, samples sizes and populations
#  by graduate type and AgeGroup. (input modified quarterly datasets)
#2.Employment_rate function modifies datasets  for the Headline_stats function
#3.Headline stats:yearly dataset. combines headline stats for 1/4 datasets into yearly statsitics. The
#  mean rates and sums of samples/counts are found for a year.
#4.Datasets for Graduate_Breakdowns. In this section the 1/4 datasets are modified
#  for input into Graduate_Breakdowns function. The datasets are adapted by putting through
#  Employment_counts_by function and the variable splits function
#5.Graduate breakdown finds employment rates, samples sizes and populations
#  for specific graduate breakdowns (input modified quarterly datasets)
#6.Output:Graduate Breakdown for year combines Graduate Breakdowns for 1/4 datasets into statistics for a year. The
#  mean rates and sums of samples/counts are found

#----1. Headline stats ----
#The all datasets are quarterly datasets modified in 2.

  Headline_stats_function<-function(all,agebracket){

    #subset all data if needed into working age "16-64" and young "21-30"
    if(agebracket=="16-64"){all<- all}else{all<-all[which(all$AgeGroup=="21-30"),]}

    #find the sample size and no. of people in each graduate type population
    all %>%
      group_by(Graduate_Type)%>% summarise(total=sum(freq),sample=sum(sample))->full_pop

    #Make a table to label the different types of graduates
    types<-full_pop["Graduate_Type"]

    #Make in_employment table by taking all object select those in employment and group by grad type and find
    #sample size etc, then label grad type using merge with types table
    #. is the modified all dataset
    all[which(all$ILODEFR=="In employment"),] %>%
      group_by(Graduate_Type)%>% summarise(count=sum(freq),sample=sum(sample))%>%
      merge(x=types, y=., by=c("Graduate_Type"), all.x=TRUE)->in_employment

    #Make in_HSemployment table by taking all object select those in employment and group by grad type and find
    #sample size etc, then label grad type using merge with types table
    all[which(all$ILODEFR=="In employment" & all$SOCHE=="High Skilled Employment"),] %>%
      group_by(Graduate_Type)%>% summarise(count=sum(freq),sample=sum(sample))%>%
      merge(x=types, y=., by=c("Graduate_Type"), all.x=TRUE)->in_HSemployment


    #Make unemployment table by taking all object select those in employment and group by grad type and find
    #sample size etc, then label grad type using merge with types table
    all[which(all$ILODEFR=="ILO unemployed"),] %>%
      group_by(Graduate_Type)%>% summarise(count=sum(freq),sample=sum(sample))%>%
      merge(x=types, y=., by=c("Graduate_Type"), all.x=TRUE)->unemployed

    #Make inactive table by taking all object select those in employment and group by grad type and find
    #sample size etc, then label grad type using merge with types table
    all[which(all$ILODEFR=="Inactive"),] %>%
      group_by(Graduate_Type)%>% summarise(count=sum(freq),sample=sum(sample))%>%
      merge(x=types, y=., by=c("Graduate_Type"), all.x=TRUE)->inactive


    Headline_stats<-data.frame( in_employment$Graduate_Type,                              #names of population for each grad type
                                in_employment$count/full_pop$total,                       #Employment_rate each grad type
                                in_HSemployment$count/full_pop$total,                     # HsEmployment_rate each grad type
                                unemployed$count/(unemployed$count + in_employment$count), #unemployment_rate each grad type
                                inactive$count/full_pop$total,                                 #inactive rate each grad type
                                in_employment$sample,                                             # employment sample
                                in_HSemployment$sample,                                            #HSsemployment sample
                                unemployed$sample,                                             #unemployment sample
                                inactive$sample,                                                  #inactive sample
                                in_employment$count,                                     # employmentment total by grad type
                                in_HSemployment$count,                             # Hemployment total  grad type
                                unemployed$count,                                     # unemployment total grad type
                                inactive$count,                                           # inactive total by grad type
                                full_pop$total,                                          #full_population sample
                                full_pop$sample)                                         #full_population by grad type

  #full_pop$total total population for each grad type

    #labels age range for each grad type
    Headline_stats$Age_range<-agebracket



    #collumn names
    colnames(Headline_stats)<-c("Population","Employment_rate","High_skill_emp_rate","Unemployment_rate","Inactivity_rate",
                                "Employed sample","HS employed sample", "unemployed sample", "inactive sample","employed total","HS employed total",
                                "unemployed total","inactive total","full_population_total","full_population_sample", "Age_range")

    Headline_stats<-Headline_stats[c("Age_range","Population","Employment_rate","High_skill_emp_rate","Unemployment_rate","Inactivity_rate",
                                     "Employed sample","HS employed sample", "unemployed sample", "inactive sample","employed total","HS employed total",
                                     "unemployed total","inactive total","full_population_total","full_population_sample")]
  }

#----2. Employment_rate function: datasets for Head_line stats function----
#Employment_rate function makes datasets  (all) which are used in Headline_stats function
#the most recent people weight is picked and samples/counts for defined population is found

  Employment_rate<-function(quarter, year){


    #get the dataset from the global environment
    dataset<-get(paste0("Q",quarter,"_",year))
    #find the weight variable
    weight<-tail(sort(names(dataset[grep("PWT", names(dataset))])),1)
    dataset<-dataset[which(dataset[,weight]>0),]
  # count all people by age, graduate type, ILO employment status and SOC group: weight people by the weight variable  (such as PWT16)
  #also note sample size
  all<-dataset%>%
    group_by(AgeGroup,Graduate_Type,ILODEFR,SOCHE) %>%
    dplyr::summarise(freq=sum(!!sym(weight)), sample=n())


  Headline_stats<-Headline_stats_function(all,"16-64")

  #Headline stats for 21-30s
  Headline_stats_young<-Headline_stats_function(all,"21-30")

  headline_stats<-rbind(Headline_stats,Headline_stats_young)

  assign(paste0("Headline_Q",quarter,"_",year),headline_stats,envir = globalenv())

  #write.csv(headline_stats,paste0("Outputs for GLMS/Headline_Q",quarter,"_",year,".csv"))

}



#3.Headline stats:yearly dataset-------------------------

#quarterly datasets combined to yearly datasets, find mean rates and total sample sizes/ counts for a year
  year_average_employment_rate<-function(year){

  #select dataset: for example if quarter =1 and year =2016 this will get Q1_2016 and set this to 'dataset'
  dataset1<-get(paste0("Headline_Q1_",year))
  dataset2<-get(paste0("Headline_Q2_",year))
  dataset3<-get(paste0("Headline_Q3_",year))
  dataset4<-get(paste0("Headline_Q4_",year))

  year_data<-rbind(dataset1,dataset2,dataset3,dataset4)

  #group by age range and graduate type (i.e population), summarise the statistics for
  #the year
  year_data%>%
    group_by( Age_range, Population) %>%
    summarise(Employment_rate=mean.default(Employment_rate),
              High_skill_emp_rate= mean.default(High_skill_emp_rate),
              Unemployment_rate=mean.default(Unemployment_rate),
              inactivity_rate= mean.default(Inactivity_rate),
              employed_sample=sum(`Employed sample`),
              HS_employed_sample= sum(`HS employed sample`),
              unemployed_sample=sum(`unemployed sample`),
              inactive_sample=sum(`inactive sample`),
              employed_total=sum(`employed total`),
              HS_employed_total=sum(`HS employed total`),
              unemployed_total=sum(`unemployed total`),
              inactive_total=sum(`inactive total`),
              full_population_total=sum(`full_population_total`),
              full_population_sample=sum(`full_population_sample`)) -> year_average

  #label the year_avearge table with year
  year_average$year<-year

  assign(paste0("Headline_",year),year_average,envir = globalenv())
  #write.csv(year_average,paste0("Outputs for GLMS/Headline_",year,".csv"))

}

-----------------------------------------


#4. Datasets for Graduate_breakdown function----
#quarterly datasets are modified by passing to Employment_counts_by function
#then piping into variable_splits function

#Employment_counts_by function makes datasets for piping into variable splits function. The datasets finds counts and sample size
#by defined demographic i.e "AgeGroup", "SEX".

#note the Graduate_breakdown function pipes these datasets into the Graduate_breakdown function

Employment_counts_by<-function(dataset,demographic){
  weight<-tail(sort(names(dataset[grep("PWT", names(dataset))])),1)
  as.data.frame(
    dataset%>%
      group_by_(demographic,"Graduate_Type","ILODEFR","SOCHE")%>%
      dplyr::summarise(sample=n(),
                       population=sum(!!sym(weight))))
}


variable_splits<-function(dataset, variable_y){


#dataset has been modified by Employment_counts_by(dataset, demographic)
#i.e demographic = "SEX". Then counts, sample sizes and employment rates are found for various demographic
#breakdowns

    dataset %>%
    group_by_(variable_y)%>% summarise(total=sum(population), full_pop.sample=sum(sample))->full_pop

  dataset[which(dataset$ILODEFR=="In employment"),] %>%
    group_by_(variable_y)%>% summarise(employed.count=sum(population), employed.sample=sum(sample))->in_employment

  dataset[which(dataset$ILODEFR=="In employment" & dataset$SOCHE=="High Skilled Employment"),] %>%
    group_by_(variable_y)%>% summarise(HSemp.count=sum(population),HSemp.sample=sum(sample))->in_HSemployment

  dataset[which(dataset$ILODEFR=="ILO unemployed"),] %>%
    group_by_(variable_y)%>% summarise(Unemp.count=sum(population),Unemp.sample=sum(sample))->unemployed

  dataset[which(dataset$ILODEFR=="Inactive"),] %>%
    group_by_(variable_y)%>% summarise(Inactive.count=sum(population),Inactive.sample=sum(sample))->inactive

  list<-list(full_pop,in_employment,in_HSemployment,unemployed,inactive)


  total_count<-Reduce(function(x, y) merge(x, y, all.x=T,
                                           by=c(variable_y)), list, accumulate=F)



  Graduate_breakdown<-data.frame( total_count[variable_y],         #list what the demographic breakdown is i.e for demographic= SEX, breakdown is male, female
                                  total_count$employed.count/total_count$total, #employment rate by demographic breakdown
                                  total_count$employed.sample,                  #employment sample by demographic breakdown
                                  total_count$HSemp.count/total_count$total,
                                  total_count$HSemp.sample,
                                  total_count$Unemp.count/(total_count$Unemp.count + total_count$employed.count),
                                  total_count$Unemp.sample,
                                  total_count$Inactive.count/total_count$total,
                                  total_count$Inactive.sample,
                                  total_count$employed.count,
                                  total_count$HSemp.count,
                                  total_count$Unemp.count,
                                  total_count$Inactive.count,
                                  total_count$total,
                                  total_count$full_pop.sample)

  colnames(Graduate_breakdown)<-c("Breakdown","Employment_rate","Employed_sample",
                                  "High_skill_emp_rate","High_Skill_employed_sample",
                                  "Unemployment_rate","Unemployed_sample",
                                  "Inactivity_rate","Inactive_sample",
                                  "employment_total", "high_skill_employment_total",
                                  "unemployment_total", "inactive_total", "full_population_total", "full_population_sample")

  return(Graduate_breakdown)
}

#5.Graduate_breakdown-----
#provide Graduate employment rates, sample sizes, counts for quarterly datasets broken down
#by demographic.

Graduate_breakdown<-function(quarter,year){

  #year<-2019
  #quarter<-1

  #select dataset: for example if quarter =1 and year =2016 this will get Q1_2016 and set this to 'dataset'
  dataset<-get(paste0("Q",quarter,"_",year))

  #select  just graduates from dataset
  dataset<-dataset[which(dataset$Graduate_Type=="Graduate"),]

  #A 1/4 dataset is modified by Employment_counts function where demograhic = AgeGroup,  then the dataset is
  #piped into the variable split function where variable_y = "AgeGroup" . A table summarising statistics on graduates
  # by AgeGroup breakdown is made.
  Employment_counts_by(dataset,"AgeGroup")%>%  variable_splits(.,"AgeGroup")->Age_group_rates
  Age_group_rates$Population<-"AgeGroup"    #describes the population eg AgeGroups

  #demographic "SEX"
  Employment_counts_by(dataset,"SEX") %>% variable_splits(.,"SEX")->Gender_rates
  Gender_rates$Population<-"SEX" #describes the population eg sex

  #demographic "ETHNICITY"
  Employment_counts_by(dataset,"ETHNICITY") %>% variable_splits(.,"ETHNICITY")->Ethnicity_rates
  Ethnicity_rates$Population<-"ETHNICITY" #describes the population  "ETHNICITY"

  #demographic "DISEA"
  Employment_counts_by(dataset,"DISEA") %>% variable_splits(.,"DISEA")->Disability_rates
  Disability_rates$Population<-"DISEA" #describes the population eg "DISEA"

  #demographic "DEGCLS7"
  Employment_counts_by(dataset,"DEGCLS7") %>% variable_splits(.,"DEGCLS7")->Degree_class_rates
  Degree_class_rates$Population<-"DEGCLS7"

  #demographic "STEM"
  Employment_counts_by(dataset,"STEM") %>% variable_splits(.,"STEM")->STEM_rates
  STEM_rates$Population<-"STEM"

  #demographic="LEM"
  Employment_counts_by(dataset,"LEM") %>% variable_splits(.,"LEM")->LEM_rates
  LEM_rates$Population<-"LEM"

  #demographic OSSAH"
  Employment_counts_by(dataset,"OSSAH") %>% variable_splits(.,"OSSAH")->OSSAH_rates
  OSSAH_rates$Population<-"OSSAH"

  #demographic "GOVTOF2"
  Employment_counts_by(dataset,"GOVTOF2") %>% variable_splits(.,"GOVTOF2")->Region_rates
  Region_rates$Population<-"GOVTOF2"

  #comines all the rates in a table
  quarter<-rbind(Age_group_rates,Gender_rates,Ethnicity_rates, Disability_rates, Degree_class_rates,
                 STEM_rates, LEM_rates, OSSAH_rates,Region_rates)

  #labels the table with the group it belongs to
  quarter$Group<-"Graduates:16-64"

  #dataset selects just young graduates and find rates for each demographic
  dataset<-dataset[which(dataset$Graduate_Type=="Graduate" & dataset$AgeGroup=="21-30"),]


  Employment_counts_by(dataset,"SEX") %>% variable_splits(.,"SEX")->Gender_rates
  Gender_rates$Population<-"SEX"

  Employment_counts_by(dataset,"ETHNICITY") %>% variable_splits(.,"ETHNICITY")->Ethnicity_rates
  Ethnicity_rates$Population<-"ETHNICITY"

  Employment_counts_by(dataset,"DISEA") %>% variable_splits(.,"DISEA")->Disability_rates
  Disability_rates$Population<-"DISEA"

  Employment_counts_by(dataset,"DEGCLS7") %>% variable_splits(.,"DEGCLS7")->Degree_class_rates
  Degree_class_rates$Population<-"DEGCLS7"

  Employment_counts_by(dataset,"STEM") %>% variable_splits(.,"STEM")->STEM_rates
  STEM_rates$Population<-"STEM"

  Employment_counts_by(dataset,"LEM") %>% variable_splits(.,"LEM")->LEM_rates
  LEM_rates$Population<-"LEM"

  Employment_counts_by(dataset,"OSSAH") %>% variable_splits(.,"OSSAH")->OSSAH_rates
  OSSAH_rates$Population<-"OSSAH"

  Employment_counts_by(dataset,"GOVTOF2") %>% variable_splits(.,"GOVTOF2")->Region_rates
  Region_rates$Population<-"GOVTOF2"


  quarter2<-rbind(Age_group_rates,Gender_rates,Ethnicity_rates, Disability_rates, Degree_class_rates,
                  STEM_rates, LEM_rates, OSSAH_rates,Region_rates)

  quarter2$Group<-"Graduates:21-30"

  quarter<-rbind(quarter,quarter2)
  return(quarter)

}

#6.----Output:Graduate Breakdown for year----

Graduate_breakdown_year<-function(year){

  #year<-2019

   #select dataset: for example if quarter =1 and year =2016 this will get Q1_2016 and set this to 'dataset'
  dataset1<-Graduate_breakdown(1,year)
  dataset2<-Graduate_breakdown(2,year)
  dataset3<-Graduate_breakdown(3,year)
  dataset4<-Graduate_breakdown(4,year)

  year_data<- rbind(dataset1,dataset2,dataset3,dataset4)

  #find the mean of the rates for a year
  means<-ddply(year_data, .(Group,Population, Breakdown), colwise(mean, .(Employment_rate,
                                                                          High_skill_emp_rate,
                                                                          Unemployment_rate, Inactivity_rate),na.rm=TRUE))
  #find the  sample size and population totals for a year
  samples<-ddply(year_data, .(Group,Population, Breakdown),colwise(sum, .(Employed_sample,
                                                                          High_Skill_employed_sample,
                                                                          Unemployed_sample, Inactive_sample,
                                                                          employment_total, high_skill_employment_total,
                                                                          unemployment_total, inactive_total, full_population_total,full_population_sample),na.rm=TRUE))
  #put means and sample size, populations in a tables
  output<-merge(x=means, y=samples, by=c("Group","Population","Breakdown"), all=TRUE)

  output<-output[c("Group",
                   "Population",
                   "Breakdown",
                   "Employment_rate",
                   "Employed_sample",
                   "High_skill_emp_rate",
                   "High_Skill_employed_sample",
                   "Unemployment_rate",
                   "Unemployed_sample",
                   "Inactivity_rate",
                   "Inactive_sample",
                   "employment_total",
                   "high_skill_employment_total",
                   "unemployment_total",
                   "inactive_total",
                   "full_population_total",
                   "full_population_sample")]


  assign(paste0("Graduate_breakdown_",year),output,envir = globalenv())


  write.csv(output,
            paste0(filepath, "Outputs_for_GLMS/Graduate_breakdown_lev_", year, ".csv"))

  saveRDS(object = output,
          file = paste0(filepath, "Outputs_for_GLMS/EES_rds/Graduate_breakdown_lev_", year, ".rds"))



}
