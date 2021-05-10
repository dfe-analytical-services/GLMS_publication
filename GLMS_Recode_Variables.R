#----Load library----  
library(plyr)

#----Recode Variables----
#Recode variables for ease of grouping in the publication.

recode_variables<- function(quarter, year){
  
#fetch the dataset from the global environment: for example Q1_2016
#de-select any columns with _old in the column title.  Do not use any columns variables with _old. These columns 
#have been superseded
dataset<-get(paste0("Q",quarter,"_",year))%>%
  select(!contains('_old'))
 

hiqual_var<-names(dataset[grep("HIQUAL", names(dataset))])[1]
if(class(dataset[,c(hiqual_var)])=="numeric"){
  #add in error warning if not a factor/string variable
  paste0("Warning: dataset Q",quarter," ",year,"is missing factor levels in HIQUAL")
}


#which HIQUAL variable is in use?
#use 'highqual15' for LFS beyond 2014
#For pre 2014 use HIQUAL11<- 2012-2014, HIQUAL5 2006-2011
hiqual_var<-names(dataset[grep("HIQUAL", names(dataset))])[1]
if(class(dataset[,c(hiqual_var)])=="numeric"){
  #add in error warning if not a factor/string variable
  paste0("Warning: dataset Q",quarter," ",year,"is missing factor levels in HIQUAL")
}


# Check if data is the right type, if not convert to a factor with the correct value levels.
if("HIQUAL15" %in% names(dataset)& class(dataset$HIQUAL15)=="numeric"){ 
  dataset$HIQUAL15<- as.factor(dataset$HIQUAL15)
  levels(dataset$HIQUAL15)<-high_qual15
} 

#----Define Graduate Type----
#Define graduates, postgraduates and non-graduates 
dataset$Graduate_Type<-NA  

 
#Graduates are those with batchelor degrees only
#For 2006 Degree type is set by DEGREE4, not DEGREE7
if("DEGREE71" %in% names(dataset)){
dataset$Graduate_Type[(dataset[,c(hiqual_var)]=="First degree/foundation degree"|
                         dataset[,c(hiqual_var)]=="First/Foundation degree"
                       ) & (
                     dataset$DEGREE71=="first degree" |
                     dataset$DEGREE72=="first degree" |
                     dataset$DEGREE73=="first degree" |
                     dataset$DEGREE74=="first degree" |
                     dataset$DEGREE75=="first degree" )]<-"Graduate"}

if("DEGREE4" %in% names(dataset)){
  dataset$Graduate_Type[(dataset[,c(hiqual_var)]=="First degree/foundation degree"|
                           dataset[,c(hiqual_var)]=="First/Foundation degree"
                         ) & (
                      dataset$DEGREE4=="A first degree"  )]<-"Graduate"}

#For 2006 Degree type is set by DEGREE4, not DEGREE7

#Note this includes 'Other Post Grad degree or professional qual' 
dataset$Graduate_Type[dataset[,c(hiqual_var)]=="Higher degree" 
                      & dataset$HIGHO != "Dont know"]<-"PostGrad"


  #Note those who don't know what their higher degree is- but have a first degree are excluded from the analysis
dataset$Graduate_Type[dataset[,c(hiqual_var)]=="Level 8 Diploma"|
                        dataset[,c(hiqual_var)]=="Level 8 Certificate"|
                        dataset[,c(hiqual_var)]=="Level 8 Award"|
                        dataset[,c(hiqual_var)]=="Level 7 Certificate"|
                        dataset[,c(hiqual_var)]=="Level 7 Diploma"|
                        dataset[,c(hiqual_var)]=='Level 7 Award'|
                        dataset[,c(hiqual_var)]=='Level 6 Award'|
                        dataset[,c(hiqual_var)]=='Level 6 Certificate'|
                        dataset[,c(hiqual_var)]=='Level 6 Diploma'|
                        dataset[,c(hiqual_var)]=="Don.t know"|
                        dataset[,c(hiqual_var)]=="Don't know"|
                        dataset[,c(hiqual_var)]=="Did not know"|
                        (dataset[,c(hiqual_var)]=="Higher degree" & dataset$HIGHO =="Dont know")|
                        (dataset[,c(hiqual_var)]=="Higher degree" & is.na(dataset$HIGHO))
                      ]<-"Not in scope"

#Set everyone who isn't a graduate, post graduate or not in scope to a non graduate
dataset$Graduate_Type[is.na(dataset$Graduate_Type)]<-"Non Grad"


#----Define AgeGroup----
dataset$AgeGroup[dataset$AGE %in% 16:20]<-"16-20"
dataset$AgeGroup[dataset$AGE %in% 21:30]<-"21-30"
dataset$AgeGroup[dataset$AGE %in% 31:40]<-"31-40"
dataset$AgeGroup[dataset$AGE %in% 41:50]<-"41-50"
dataset$AgeGroup[dataset$AGE %in% 51:60]<-"51-60"
#Note error in 2016 (age 60 in last bracket)
dataset$AgeGroup[dataset$AGE %in% 61:64]<-"61-64"

#The remaining variables are only added to the latest datasets

if("SC10MMJ" %in% names(dataset)){

#Define skilled occupation groupings
SOCHE1<-c("1  'Managers, Directors And Senior Officials'"        
          ,"2  'Professional Occupations'"                        
          ,"3  'Associate Professional And Technical Occupations'")

SOCHE2<-c("4  'Administrative And Secretarial Occupations'"      
          ,"5  'Skilled Trades Occupations'"                      
          ,"6  'Caring, Leisure And Other Service Occupations'"  )

SOCHE3<-c("7  'Sales And Customer Service Occupations'"          
          ,"8  'Process, Plant And Machine Operatives'"           
          ,"9  'Elementary Occupations'"  )

dataset$SOCHE[dataset$SC10MMJ %in% SOCHE1]<-"High Skilled Employment"
dataset$SOCHE[dataset$SC10MMJ %in% SOCHE2]<-"Medium Skilled Employment"
dataset$SOCHE[dataset$SC10MMJ %in% SOCHE3]<-"Low Skilled Employment"
dataset$SOCHE[is.na(dataset$SOCHE)]<-"Else"
}


#----Define skilled occupation groupings----
if("SC2KMMJ" %in% names(dataset)){
SOCHE1<-c("1  'Managers and Senior Officials'"         
          ,"2  'Professional occupations'"                           
          ,"3  'Associate Professional and Technical'" )

SOCHE2<-c("4  'Administrative and Secretarial'"       
          ,"5  'Skilled Trades Occupations'"                     
          ,"6  'Personal Service Occupations'"  )

SOCHE3<-c("7  'Sales and Customer Service Occupations'"          
          ,"8  'Process, Plant and Machine Operatives'"           
          ,"9  'Elementary Occupations'"  )

dataset$SOCHE[dataset$SC2KMMJ %in% SOCHE1]<-"High Skilled Employment"
dataset$SOCHE[dataset$SC2KMMJ %in% SOCHE2]<-"Medium Skilled Employment"
dataset$SOCHE[dataset$SC2KMMJ %in% SOCHE3]<-"Low Skilled Employment"
dataset$SOCHE[is.na(dataset$SOCHE)]<-"Else"
}

if("ETH11EW" %in% names(dataset)){
#Define ethnic group
dataset$ETHNICITY<-dataset$ETH11EW

dataset$ETHNICITY[dataset$ETH11EW %in% c("Mixed / Multiple ethnic groups" ,"Chinese","Arab")]<-"Other ethnic group."   
}

#----Define Subject Type----
if("SNGDEGB" %in% names(dataset)){
dataset$STEM[dataset$SNGDEGB %in% 1:9]<- 1
#10.1~Economics, Law, Business
dataset$LEM[dataset$SNGDEGB %in% 11:12| (substr(dataset$FDSNGDEG,1,4)=="10.1" 
                                         & dataset$Graduate_Type=="Graduate")|
              (substr(dataset$SNGHD,1,4)=="10.1" 
               & dataset$Graduate_Type=="PostGrad")]<-1
#10.0~Social Studies
#10.2:10.9~Politics, Sociology, Social Policy, Social Work,Anthropology, 
#Human and Social Geography, Other Social Studies
#
dataset$OSSAH[dataset$SNGDEGB %in% 13:19|  
                 (substr(dataset$FDSNGDEG,1,4) %in% c("10.0","10.2","10.3","10.4","10.5",
                                                  "10.6","10.7","10.8","10.9")
                   & dataset$Graduate_Type=="Graduate")|
                (substr(dataset$SNGHD,1,4) %in% c("10.0","10.2","10.3","10.4","10.5",
                                                     "10.6","10.7","10.8","10.9")
                 & dataset$Graduate_Type=="PostGrad")]<-1
}

if("SC10MMJ" %in% names(dataset)){
#* Occupation.
dataset$Occupation<-dataset$SC10MMJ
dataset$Occupation<-factor(dataset$Occupation, levels=c(levels(dataset$Occupation),"10 'Medium Skilled Employment'","11 'Low Skilled Employment'"))
dataset$Occupation[dataset$SC10MMJ %in% c("4  'Administrative And Secretarial Occupations'",      
                                         "5  'Skilled Trades Occupations'",
                                         "6  'Caring, Leisure And Other Service Occupations'")]<-"10 'Medium Skilled Employment'"
dataset$Occupation[dataset$SC10MMJ %in% c("7  'Sales And Customer Service Occupations'" , "8  'Process, Plant And Machine Operatives'"           
                                          ,"9  'Elementary Occupations'" )]<-"11 'Low Skilled Employment'"
}
#----Labels For  Occupation----- 					
#1 'Managers, Directors and Senior Officials'
#2 'Professional Occupations'
#3	'Associate Professional and Technical Occupations'		
#10	'Medium Skilled Employment'							
#11	'Low Skilled Employment'.


assign(paste0("Q",quarter,"_",year),dataset,envir = globalenv())

}



