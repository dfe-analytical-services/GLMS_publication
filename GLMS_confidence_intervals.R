#----Load Libraries-----
library("survey")
library("tidyr")

library("survival")
library("grid")
library("Matrix")

# See link below for a discussion of accounting for survey design and weighting
# https://www.ons.gov.uk/methodology/methodologicalpublications/generalmethodology/onsworkingpaperseries/onsmethodologyworkingpaperseriesno9guidetocalculatingstandarderrorsforonssocialsurveys#accounting-for-the-estimation-method

#options(survey.lonely.psu="fail") #default option
options(survey.lonely.psu="adjust") #lonley PSUs make no contribution to the variance.

confidence_intervals<-function(quarter,year,young){
  
  
 
  dataset_name<-paste0("Q",quarter,"_",year)
  dataset<-get(dataset_name)
  
  
  weight<-tail(sort(names(dataset[grep("PWT", names(dataset))])),1)
  dataset<-dataset[which(dataset[,weight]>=0),]
  
  #subest to 21-30s if young=TRUE
  if(young==TRUE){dataset<-dataset[which(dataset$AgeGroup=="21-30"),]}
  
  dataset$hhld_id<-substring(dataset$CASENO,1,13)
  
  #define graduates, post graduates and non_graduates
  dataset$Grad<-0
  dataset$Grad[dataset$Graduate_Type=="Graduate"]<-1
  
  dataset$PostGrad<-0
  dataset$PostGrad[dataset$Graduate_Type=="PostGrad"]<-1
  
  dataset$NonGrad<-0
  dataset$NonGrad[dataset$Graduate_Type=="Non Grad"]<-1
  
  #define employed graduates, post graduate and non graduates
  dataset$Emp_Grad<-0
  dataset$Emp_Grad[dataset$ILODEFR=="In employment" & dataset$Graduate_Type=="Graduate"]<-1
  
  dataset$Emp_PostGrad<-0
  dataset$Emp_PostGrad[dataset$ILODEFR=="In employment" & dataset$Graduate_Type=="PostGrad"]<-1
  
  dataset$Emp_NonGrad<-0
  dataset$Emp_NonGrad[dataset$ILODEFR=="In employment" & dataset$Graduate_Type=="Non Grad"]<-1
  
  #define graduates, post graduates and non graduates in high skilled employment
  
  dataset$HSEmp_Grad<-0
  dataset$HSEmp_Grad[dataset$ILODEFR=="In employment" & dataset$Graduate_Type=="Graduate"& 
                       dataset$SOCHE=="High Skilled Employment"]<-1
  
  dataset$HSEmp_PostGrad<-0
  dataset$HSEmp_PostGrad[dataset$ILODEFR=="In employment" & dataset$Graduate_Type=="PostGrad"& 
                           dataset$SOCHE=="High Skilled Employment"]<-1
  
  dataset$HSEmp_NonGrad<-0
  dataset$HSEmp_NonGrad[dataset$ILODEFR=="In employment" & dataset$Graduate_Type=="Non Grad"& 
                          dataset$SOCHE=="High Skilled Employment"]<-1
  
  #define Unemployed graduates, post graduate and non graduates
  dataset$Unemp_Grad<-0
  dataset$Unemp_Grad[dataset$ILODEFR=="ILO unemployed" & dataset$Graduate_Type=="Graduate"]<-1
  
  dataset$Unemp_PostGrad<-0
  dataset$Unemp_PostGrad[dataset$ILODEFR=="ILO unemployed" & dataset$Graduate_Type=="PostGrad"]<-1
  
  dataset$Unemp_NonGrad<-0
  dataset$Unemp_NonGrad[dataset$ILODEFR=="ILO unemployed" & dataset$Graduate_Type=="Non Grad"]<-1
  
  #define economically active graduates, post graduate and non graduates
  dataset$Act_Grad<-0
  dataset$Act_Grad[(dataset$ILODEFR=="ILO unemployed"| dataset$ILODEFR=="In employment")
                   & dataset$Graduate_Type=="Graduate"]<-1
  
  dataset$Act_PostGrad<-0
  dataset$Act_PostGrad[(dataset$ILODEFR=="ILO unemployed"| dataset$ILODEFR=="In employment") 
                       & dataset$Graduate_Type=="PostGrad"]<-1
  
  dataset$Act_NonGrad<-0
  dataset$Act_NonGrad[(dataset$ILODEFR=="ILO unemployed"| dataset$ILODEFR=="In employment")
                      & dataset$Graduate_Type=="Non Grad"]<-1
  
  
  #dataset<<-dataset
  #Define the survey design:
  #clusters- households
  #strata- low level area identifier (Unitary Local Authoritary)
  #weight- person weight
  weight_formula<-as.formula(paste0("~",weight))
  
  surveydesign<-svydesign(id=~hhld_id, data=dataset, strata=~UALA, weight=weight_formula,nest=TRUE)
  #surveydesign<<-surveydesign
  
  #work out the standard error for graduates
  #Note the programe saves the variance = SE^2
  
  ratio_and_confidence_int<-function(Numerator_input, Demoninator_input){
    Numerator<-as.formula(paste0("~",Numerator_input))
    Denominator<-as.formula(paste0("~",Demoninator_input))
    #calcualte mean and standard error
    ratio_output<-svyratio(numerator=Numerator,denominator=Denominator, design = surveydesign)
    #calcualte confidence intervals
    confidence_int<-confint(ratio_output,  level = 0.95,df =degf(surveydesign))
    #reformat to dataframe
    ratio_output<-data.frame(matrix(unlist(ratio_output),nrow=1, byrow=T),stringsAsFactors = FALSE)
    confidence_int<-data.frame(matrix(unlist(confidence_int),nrow=1, byrow=T),stringsAsFactors = FALSE)
    #We sqare root to get back to the standard error
    ratio_output$X2<-sqrt(as.numeric(as.character(ratio_output$X2)))
    
    #summarise as 1 dataframe
    output<-data.frame(cbind(ratio_output$X1, confidence_int, ratio_output$X2),stringsAsFactors = FALSE)
    
    numerator_type<-strsplit(Numerator_input,"_")[[1]][1]
    
    
    names(output)<-c(paste0(numerator_type,"_prop"),paste0(numerator_type,"_prop_lower"),
                     paste0(numerator_type,"_prop_upper"),paste0(numerator_type,"_SE"))
    
    output$population<-strsplit(Numerator_input,"_")[[1]][2]
    output$Quarter<-dataset_name
    
    return(output)
  }
  
  
  #graduates
  grad_emp<-ratio_and_confidence_int("Emp_Grad","Grad")
  grad_HSemp<-ratio_and_confidence_int("HSEmp_Grad","Grad")
  grad_Unemp<-ratio_and_confidence_int("Unemp_Grad","Act_Grad")
  
  list<-list(grad_emp, grad_HSemp, grad_Unemp)
  
  grad<-Reduce(function(x, y) merge(x, y, all.x=T, 
                                    by=c("population","Quarter")), list, accumulate=F)
  
  #postgraduates
  PostGrad_emp<-ratio_and_confidence_int("Emp_PostGrad","PostGrad")
  PostGrad_HSemp<-ratio_and_confidence_int("HSEmp_PostGrad","PostGrad")
  PostGrad_Unemp<-ratio_and_confidence_int("Unemp_PostGrad","Act_PostGrad")
  
  list<-list(PostGrad_emp, PostGrad_HSemp, PostGrad_Unemp)
  
  PostGrad<-Reduce(function(x, y) merge(x, y, all.x=T, 
                                        by=c("population","Quarter")), list, accumulate=F)
  
  #Non graduates
  NonGrad_emp<-ratio_and_confidence_int("Emp_NonGrad","NonGrad")
  NonGrad_HSemp<-ratio_and_confidence_int("HSEmp_NonGrad","NonGrad")
  NonGrad_Unemp<-ratio_and_confidence_int("Unemp_NonGrad","Act_NonGrad")
  
  list<-list(NonGrad_emp, NonGrad_HSemp, NonGrad_Unemp)
  
  NonGrad<-Reduce(function(x, y) merge(x, y, all.x=T, 
                                       by=c("population","Quarter")), list, accumulate=F)
  
  
  ratios<-rbind(grad,PostGrad,NonGrad)
  
  #rename to keep in sync with Graduate_Types
  ratios$population[ratios$population=="Grad"]<-"Graduate"
  ratios$population[ratios$population=="NonGrad"]<-"Non Grad"
  
  
  #Also want the sample and population sizes 
  dataset %>%
    group_by(ILODEFR,Graduate_Type)%>% 
    summarise(count=n(),weighted_pop=sum(!!sym(weight))) %>% 
    gather(variable, value, -(Graduate_Type:ILODEFR)) %>%
    unite(temp, ILODEFR, variable) %>%
    spread(temp, value)->samples
  
  dataset[which(dataset$SOCHE=="High Skilled Employment" & dataset$ILODEFR=="In employment"),] %>%
    group_by(Graduate_Type)%>% 
    summarise(HS_emp_count=n(),HS_emp_weighted_pop=sum(!!sym(weight))) ->sample2
  
  samples<-merge(x=samples, y=sample2, by=("Graduate_Type"))
  
  colnames(samples)[colnames(samples)=="Graduate_Type"]<-"population"
  
  ratios<-merge(x=ratios, y=samples, by=c("population"), all.x=TRUE)
  
  return(ratios)
  
}

#Find confidence intervals from start year to end year
output<-lapply(start_year:end_year, function(y){lapply(1:4,function(x)confidence_intervals(x,y,FALSE))})


#store as dataframe
output2<-Reduce(rbind,lapply(1:length(output),function(x)Reduce(rbind, output[[x]])))

output2$year<-substring(output2$Quarter,4,7)
output2$Quarter<-substring(output2$Quarter,2,2)

output2<-output2[c("population","year",names(output2)[2:22])]

#sort by population and quarter
output3<-output2[with(output2, order(population, year, Quarter)), ]
output3$age<-"16-64"

#repeat for young_graduates

#Find confidence intervals from start year to end year
output<-lapply(start_year:end_year, function(y){lapply(1:4,function(x)confidence_intervals(x,y,TRUE))})


#store as dataframe
output2<-Reduce(rbind,lapply(1:length(output),function(x)Reduce(rbind, output[[x]])))

output2$year<-substring(output2$Quarter,4,7)
output2$Quarter<-substring(output2$Quarter,2,2)

output2<-output2[c("population","year",names(output2)[2:22])]

#sort by population and quarter
output4<-output2[with(output2, order(population, year, Quarter)), ]
output4$age<-"21-30"

output5<-rbind(output3,output4)

write.csv(output5,
          paste0(filepath, "Outputs_for_GLMS/Emp_confidence_Intervals_",start_year,"_",end_year,".csv"))

saveRDS(object = output5,
        file = paste0(filepath, "Outputs_for_GLMS/EES_rds/Emp_confidence_Intervals_",start_year,"_",end_year,".rds"))


