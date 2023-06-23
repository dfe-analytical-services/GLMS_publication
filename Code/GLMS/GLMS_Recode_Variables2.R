#----Load library----  
library(plyr)
#Employment Statistics
#Add in plyr library for nice frequency tables


## Note: 
## before section 3.1, make sure you have already manually run the clean data script to remove the symbols from the years 
# that have this in 

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
  


  
  ###############################
  ######## NOT IN SCOPE ##########
  ###############################
  
  
  
  
  ##note changes for 2022 data: new qualifications added, some old qualifications removed 
  ## e.g 2022 data contains RQF level qualifications. Check the LFS userguides for more guidance if interested
  dataset$Graduate_Type[(dataset[,c(hiqual_var)]=="Level 8 Diploma" & dataset$HIGHO != "first degree") |
                          (dataset[,c(hiqual_var)]=="Level 8 Diploma" & dataset$HIGHO != "higher degree") |
                          (dataset[,c(hiqual_var)]=="Level 8 Diploma" & is.na(dataset$HIGHO)) |
                          (dataset[,c(hiqual_var)]=="Level 8 Certificate"& dataset$HIGHO != "first degree") |
                          (dataset[,c(hiqual_var)]=="Level 8 Certificate"& dataset$HIGHO != "higher degree") |
                          (dataset[,c(hiqual_var)]=="Level 8 Certificate" & is.na(dataset$HIGHO)) |
                          (dataset[,c(hiqual_var)]=="Level 8 Award" & dataset$HIGHO != "first degree") |
                          (dataset[,c(hiqual_var)]=="Level 8 Award" & dataset$HIGHO != "higher degree") |
                          (dataset[,c(hiqual_var)]=="Level 8 Award" & is.na(dataset$HIGHO)) |
                          (dataset[,c(hiqual_var)]=="RQF level 8 qualifications" & dataset$HIGHO != "first degree") |
                          (dataset[,c(hiqual_var)]=="RQF level 8 qualifications" & dataset$HIGHO != "higher degree") |
                          (dataset[,c(hiqual_var)]=="RQF level 8 qualifications" & is.na(dataset$HIGHO)) |
                          (dataset[,c(hiqual_var)]=="Level 7 Certificate" & dataset$HIGHO != "first degree") |
                          (dataset[,c(hiqual_var)]=="Level 7 Certificate" & dataset$HIGHO != "higher degree") |
                          (dataset[,c(hiqual_var)]=="Level 7 Certificate" & is.na(dataset$HIGHO)) |
                          (dataset[,c(hiqual_var)]=="Level 7 Diploma" & dataset$HIGHO != "first degree")|
                          (dataset[,c(hiqual_var)]=="Level 7 Diploma" & dataset$HIGHO != "higher degree") |
                          (dataset[,c(hiqual_var)]=="Level 7 Diploma" & is.na(dataset$HIGHO)) |
                          (dataset[,c(hiqual_var)]=="RQF level 7 qualifications" & dataset$HIGHO != "first degree")|
                          (dataset[,c(hiqual_var)]=="RQF level 7 qualifications" & dataset$HIGHO != "higher degree") |
                          (dataset[,c(hiqual_var)]=="RQF level 7 qualifications" & is.na(dataset$HIGHO)) |
                          (dataset[,c(hiqual_var)]=='Level 7 Award' & dataset$HIGHO != "first degree") |
                          (dataset[,c(hiqual_var)]=='Level 7 Award'& dataset$HIGHO != "higher degree") |
                          (dataset[,c(hiqual_var)]=="Level 7 Award" & is.na(dataset$HIGHO)) |
                          (dataset[,c(hiqual_var)]=='Level 6 Award' & dataset$HIGHO != "first degree") |
                          (dataset[,c(hiqual_var)]=='Level 6 Award' & dataset$HIGHO != "higher degree") |
                          (dataset[,c(hiqual_var)]=="Level 6 Award" & is.na(dataset$HIGHO)) |
                          (dataset[,c(hiqual_var)]=='Level 6 Certificate'& dataset$HIGHO != "first degree") |
                          (dataset[,c(hiqual_var)]=='Level 6 Certificate'& dataset$HIGHO != "higher degree") |
                          (dataset[,c(hiqual_var)]=="Level 6 Certificate" & is.na(dataset$HIGHO)) |
                          (dataset[,c(hiqual_var)]=='Level 6 Diploma'& dataset$HIGHO != "first degree") |
                          (dataset[,c(hiqual_var)]=='Level 6 Diploma' & dataset$HIGHO != "higher degree")|
                          (dataset[,c(hiqual_var)]=="Level 6 Diploma" & is.na(dataset$HIGHO)) |
                          (dataset[,c(hiqual_var)]=="RQF level 6 qualifications" & dataset$HIGHO != "first degree") |
                          (dataset[,c(hiqual_var)]=="RQF level 6 qualifications" & dataset$HIGHO != "higher degree")|
                          (dataset[,c(hiqual_var)]=="RQF level 6 qualifications" & is.na(dataset$HIGHO)) |
                          dataset[,c(hiqual_var)]=="Don.t know"|
                          dataset[,c(hiqual_var)]=="Don't know"|
                          dataset[,c(hiqual_var)]=="Did not know"|
                          dataset[,c(hiqual_var)]=="Don¿t know"|
                          dataset[,c(hiqual_var)]=="DK"|
                          dataset[,c(hiqual_var)]=="-9"|
                          dataset[,c(hiqual_var)]=="-8"|
                          (dataset[,c(hiqual_var)]=="Higher degree" & dataset$HIGHO =="Dont know")| # don't knows
                          (dataset[,c(hiqual_var)]=="Higher degree" & dataset$HIGHO == "NA") | 
                          (dataset[,c(hiqual_var)]=="Higher degree" & dataset$HIGHO == "-9")| 
                          (dataset[,c(hiqual_var)]=="Higher Degree" & dataset$HIGHO =="Dont know")| # don't knows
                          (dataset[,c(hiqual_var)]=="Higher Degree" & dataset$HIGHO == "NA") | ## is.na 
                          (dataset[,c(hiqual_var)]=="Higher Degree" & dataset$HIGHO == "-9") 
                        
  ]<-"Not in scope"
  
  
  dataset$Graduate_Type[is.na(dataset[,c(hiqual_var)])] <- "Not in scope"
  dataset$Graduate_Type[(dataset[,c(hiqual_var)]=="Higher Degree" & is.na(dataset$HIGHO))] <- "Not in scope" 
  dataset$Graduate_Type[(dataset[,c(hiqual_var)]=="Higher degree" & is.na(dataset$HIGHO))] <- "Not in scope" 
  dataset$Graduate_Type[(dataset[,c(hiqual_var)]=="Don¿t know")] <- "Not in scope"
  
  # setting those we cannot be sure of level of qualification in relation to first/foundation degree to not in scope 
  dataset$Graduate_Type[(dataset[,c(hiqual_var)]=="First or Foundation Degree"  & dataset$DEGREE71 == "grad membership of prof institute")] <- "Not in scope"
  dataset$Graduate_Type[(dataset[,c(hiqual_var)]=="First or Foundation Degree"  & dataset$DEGREE71 == "grad membership of prof institute" & dataset$DEGREE72 == 'first degree')] <- "Graduate"
  dataset$Graduate_Type[(dataset[,c(hiqual_var)]=="First or Foundation Degree"  & dataset$DEGREE71 == "other")] <- "Not in scope"
  dataset$Graduate_Type[(dataset[,c(hiqual_var)]=="First or Foundation Degree"  & dataset$DEGREE71 == "dont know")] <- "Not in scope"
  dataset$Graduate_Type[(dataset[,c(hiqual_var)]=="First degree/foundation degree"  & dataset$DEGREE71 == "grad membership of prof institute")] <- "Not in scope"
  dataset$Graduate_Type[(dataset[,c(hiqual_var)]=="First degree/foundation degree"  & dataset$DEGREE71 == "other")] <- "Not in scope"
  dataset$Graduate_Type[(dataset[,c(hiqual_var)]=="First degree/foundation degree"  & dataset$DEGREE71 == "dont know")] <- "Not in scope"
  dataset$Graduate_Type[(dataset[,c(hiqual_var)]=="First degree/foundation degree"  & dataset$DEGREE71 == "grad membership of prof institute")] <- "Not in scope"
  dataset$Graduate_Type[(dataset[,c(hiqual_var)]=="First degree/foundation degree"  & dataset$DEGREE71 == "other")] <- "Not in scope"
  dataset$Graduate_Type[(dataset[,c(hiqual_var)]=="First degree/foundation degree"  & dataset$DEGREE71 == "dont know")] <- "Not in scope"
  dataset$Graduate_Type[(dataset[,c(hiqual_var)]=="Degree Apprenticsehip"  & dataset$DEGREE71 == "grad membership of prof institute")] <- "Not in scope"
  dataset$Graduate_Type[(dataset[,c(hiqual_var)]=="Degree Apprenticsehip"  & dataset$DEGREE71 == "other")] <- "Not in scope"
  dataset$Graduate_Type[(dataset[,c(hiqual_var)]=="Degree Apprenticsehip"  & dataset$DEGREE71 == "dont know")] <- "Not in scope"
  dataset$Graduate_Type[(dataset[,c(hiqual_var)]=="First/Foundation degree"  & dataset$DEGREE71 == "grad membership of prof institute")] <- "Not in scope"
  
  # but specifying grad if degree 71 is grad membership and degree 72 is first degree
  # this code chunk is here as if it was in the graduate section it would get overwritten 
 # dataset$Graduate_Type[(dataset[,c(hiqual_var)]=="First degree/foundation degree"  & dataset$DEGREE71 == "grad membership of prof institute" & dataset$DEGREE72 == "first degree")] <- "Graduate"
  
  
  # setting those with na's in degree71 with FD/foundation degree to not in scope 
  if("DEGREE71" %in% names(dataset)){
    dataset$Graduate_Type[(dataset[,c(hiqual_var)]=="First degree/foundation degree"|
                             dataset[,c(hiqual_var)]=="First/Foundation degree" |
                             dataset[,c(hiqual_var)]=="First or Foundation Degree" | #different wording in HIQUAL22
                             dataset[,c(hiqual_var)]=="Degree Apprenticsehip" | #spelling incorrect in dataset
                             dataset[,c(hiqual_var)]=="Degree Apprenticeship" |
                             dataset[,c(hiqual_var)]=="Graduate Apprenticsehip"
    ) & (
      is.na(dataset$DEGREE71)|
        dataset$DEGREE71=="-9"
    )]<-"Not in scope"}
  
  
  if("DEGREE4" %in% names(dataset)){
    dataset$Graduate_Type[(dataset[,c(hiqual_var)]=="First degree/foundation degree"|
                             dataset[,c(hiqual_var)]=="First/Foundation degree"
    ) & (
      dataset$DEGREE4=="NA" |
        dataset$DEGREE4=="-9")]<-"Not in scope"}
  
  
  
  
  
  
  

  
  ####################################
  ####### DEFINING POSTGRADUATE ########
  ####################################
  
  
  dataset$Graduate_Type[(dataset[,c(hiqual_var)]=="Higher degree" |
                          dataset[,c(hiqual_var)]=="Higher Degree") #watch out for spelling/capitalisation
                        & (dataset$HIGHO != "Dont know" |
                          dataset$HIGHO != "-9" | ## contingency for files that do not read in properly where -9 does not change to NA
                          dataset$HIGHO != "NA")]<-"PostGrad"
  
  dataset$Graduate_Type[(dataset[,c(hiqual_var)]=="Degree Apprenticsehip" | 
                          dataset[,c(hiqual_var)]=="Graduate Apprenticsehip"|
                         dataset[,c(hiqual_var)]=="Level 8 Diploma"|
                          dataset[,c(hiqual_var)]=="Level 8 Certificate"|
                          dataset[,c(hiqual_var)]=="Level 8 Award"|
                          dataset[,c(hiqual_var)]=="RQF level 8 qualifications"|
                          dataset[,c(hiqual_var)]=="Level 7 Certificate"|
                          dataset[,c(hiqual_var)]=="Level 7 Diploma"|
                          dataset[,c(hiqual_var)]=="RQF level 7 qualifications"|
                          dataset[,c(hiqual_var)]=='Level 7 Award'|
                          dataset[,c(hiqual_var)]=='Level 6 Award'|
                          dataset[,c(hiqual_var)]=='Level 6 Certificate'|
                          dataset[,c(hiqual_var)]=='Level 6 Diploma'|
                          dataset[,c(hiqual_var)]=="RQF level 6 qualifications")
                        & (dataset$DEGREE71=="higher degree" |
                          dataset$DEGREE72=="higher degree" |
                          dataset$DEGREE73=="higher degree" |
                          dataset$DEGREE74=="higher degree" |
                          dataset$DEGREE75=="higher degree")]<-"PostGrad"



####################################
#######  DEFINING GRADUATES ########
####################################


if("DEGREE71" %in% names(dataset)){
  dataset$Graduate_Type[(dataset[,c(hiqual_var)]=="First degree/foundation degree"|
                           dataset[,c(hiqual_var)]=="First/Foundation degree" |
                           dataset[,c(hiqual_var)]=="First or Foundation Degree" | #different wording in HIQUAL22
                           dataset[,c(hiqual_var)]=="Degree Apprenticsehip" | #spelling incorrect in dataset
                           dataset[,c(hiqual_var)]=="Degree Apprenticeship" |
                           dataset[,c(hiqual_var)]=="Graduate Apprenticsehip" |
                           dataset[,c(hiqual_var)]=="Level 8 Diploma"|
                           dataset[,c(hiqual_var)]=="Level 8 Certificate"|
                           dataset[,c(hiqual_var)]=="Level 8 Award"|
                           dataset[,c(hiqual_var)]=="RQF level 8 qualifications"|
                           dataset[,c(hiqual_var)]=="Level 7 Certificate"|
                           dataset[,c(hiqual_var)]=="Level 7 Diploma"|
                           dataset[,c(hiqual_var)]=="RQF level 7 qualifications"|
                           dataset[,c(hiqual_var)]=='Level 7 Award'|
                           dataset[,c(hiqual_var)]=='Level 6 Award'|
                           dataset[,c(hiqual_var)]=='Level 6 Certificate'|
                           dataset[,c(hiqual_var)]=='Level 6 Diploma'|
                           dataset[,c(hiqual_var)]=="RQF level 6 qualifications"
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


# setting those where hiqual is higher, but HIGO is NA, but any degree is first degree
  # this is because we dont have enough info to know what their higher degree is, but we know they at least have an UG degree 
dataset$Graduate_Type[(dataset[,c(hiqual_var)]=="Higher Degree" & is.na(dataset$HIGHO) & 
                           (dataset$DEGREE71 == "first degree" |
                           dataset$DEGREE72=="first degree" |
                           dataset$DEGREE73=="first degree" |
                           dataset$DEGREE74=="first degree" |
                           dataset$DEGREE75=="first degree"))]<-"Graduate"
                           
                          
                          
###############################
########### NON GRADS ###########
###############################
dataset$Graduate_Type[dataset[,c(hiqual_var)]== "HNC/HND Higher" |
                        dataset[,c(hiqual_var)]== "Teaching further education" |
                        dataset[,c(hiqual_var)]== "Teaching secondary education" |
                        dataset[,c(hiqual_var)]== "Teaching primary education" |
                        dataset[,c(hiqual_var)]== "Teaching foundation stage" |
                        dataset[,c(hiqual_var)]== "Teaching level not stated" |
                        dataset[,c(hiqual_var)]== "Nursing etc" |
                        dataset[,c(hiqual_var)]== "RSA/OCR/Cambridge technical Level 4" |
                        dataset[,c(hiqual_var)]== "Other higher education below degree level" |
                        dataset[,c(hiqual_var)]== "SVQ level 4" |
                        dataset[,c(hiqual_var)]== "RQF level 4 qualifications" |
                        dataset[,c(hiqual_var)]== "Higher Apprenticeship" |
                        dataset[,c(hiqual_var)]== "Modern, Technical, & Professional Apprenticeship" |
                        dataset[,c(hiqual_var)]== "Advanced Higher (Scotland)" |
                        dataset[,c(hiqual_var)]== "RQF level 3 qualifications" |
                        dataset[,c(hiqual_var)]== "SVQ level 3 (SCQF level 7)" |
                        dataset[,c(hiqual_var)]== "OND/ONC National" |
                        dataset[,c(hiqual_var)]== "Advanced or Progression Diploma (14-19)" |
                        dataset[,c(hiqual_var)]== "Advanced Welsh Baccalaureate" |
                        dataset[,c(hiqual_var)]== "International Baccalaureate" |
                        dataset[,c(hiqual_var)]== "Scottish Baccalaureate" |
                        dataset[,c(hiqual_var)]== "GNVQ/GSVQ Advanced" |
                        dataset[,c(hiqual_var)]== "RQF level 5 qualifications" |
                        dataset[,c(hiqual_var)]== "A level or equivalent" |
                        dataset[,c(hiqual_var)]== "RSA/OCR/Techni level 3" |
                        dataset[,c(hiqual_var)]== "Advanced Apprenticeship" |
                        dataset[,c(hiqual_var)]== "Foundation Apprenticeship" |
                        dataset[,c(hiqual_var)]== "Scottish 6th year certificate" |
                        dataset[,c(hiqual_var)]== "National Scottish Higher" |
                        dataset[,c(hiqual_var)]== "Access Qualifications" |
                        dataset[,c(hiqual_var)]== "AS level or equivalent" |
                        dataset[,c(hiqual_var)]== "Trade Apprenticeship" |
                        dataset[,c(hiqual_var)]== "RQF level 2 qualifications" |
                        dataset[,c(hiqual_var)]== "SVQ level 2" |
                        dataset[,c(hiqual_var)]== "Intermediate Apprenticeship" |
                        dataset[,c(hiqual_var)]== "Intermediate Welsh Bac" |
                        dataset[,c(hiqual_var)]== "National Scott Intermediate 2" |
                        dataset[,c(hiqual_var)]== "GNVQ/GSVQ Intermediate" |
                        dataset[,c(hiqual_var)]== "RSA/OCR/ Cambridge Technical level 2" |
                        dataset[,c(hiqual_var)]== "Higher Diploma (14-19)" |
                        dataset[,c(hiqual_var)]== "Scottish National level 5" |
                        dataset[,c(hiqual_var)]== "O Level/GCSE Grades A* - C and equivalents" |
                        dataset[,c(hiqual_var)]== "RQF Level 1 Qualifications" |
                        dataset[,c(hiqual_var)]== "SVQ Level 1" |
                        dataset[,c(hiqual_var)]== "Foundation Welsh Bac" |
                        dataset[,c(hiqual_var)]== "GNVQ/GSVQ Foundation Level" |
                        dataset[,c(hiqual_var)]== "RSA/OCR/ Cambridge Technical Level 1" |
                        dataset[,c(hiqual_var)]=="Scottish National Level 4" |
                        dataset[,c(hiqual_var)]== "GCSE < Grade C, CSE < Grade 1 and equivalents" |
                        dataset[,c(hiqual_var)]== "SCOTVEC National Certificate modules" |
                        dataset[,c(hiqual_var)]== "National Scottish Intermediate 1" |
                        dataset[,c(hiqual_var)]== "14-19 Diploma" |
                        dataset[,c(hiqual_var)]== "YT/YTP Certificate" |
                        dataset[,c(hiqual_var)]== "Scottish National Level 3" |
                        dataset[,c(hiqual_var)]== "Scottish National < Level 3" |
                        dataset[,c(hiqual_var)]== "Basic Skills Qualifications" |
                        dataset[,c(hiqual_var)]== "National Scottish Access Level" |
                        dataset[,c(hiqual_var)]== "Entry Level Qualifications" |
                        dataset[,c(hiqual_var)]== "Key/Core/ Essential/ functional Skills" |
                        dataset[,c(hiqual_var)]== "Other RSA/OCR/Cambridge Technicals" |
                        dataset[,c(hiqual_var)]== "Other Qualifications" |
                        dataset[,c(hiqual_var)]== "No qualifications" |
                        dataset[,c(hiqual_var)]== "NVQ level 5" |
                        dataset[,c(hiqual_var)]== "Other degree" | 
                        dataset[,c(hiqual_var)]== "Other Degree" |
                        dataset[,c(hiqual_var)]== "NVQ level 4" |
                        dataset[,c(hiqual_var)]== "Level 6 Diploma" |
                        dataset[,c(hiqual_var)]== "Level 6 Certificate" |
                        dataset[,c(hiqual_var)]== "Diploma in higher education" |
                        dataset[,c(hiqual_var)]== "Level 5 Diploma" |
                        dataset[,c(hiqual_var)]== "Level 5 Certificate" |
                        dataset[,c(hiqual_var)]== "HNC/HND/BTEC higher etc" |
                        dataset[,c(hiqual_var)]== "Teaching Ð further education" |
                        dataset[,c(hiqual_var)]== "Teaching Ð secondary education" |
                        dataset[,c(hiqual_var)]== "Teaching Ð primary education" |
                        dataset[,c(hiqual_var)]== "Teaching Ð foundation stage" |
                        dataset[,c(hiqual_var)]== "Teaching Ð level not stated" |
                        dataset[,c(hiqual_var)]== "Nursing etc" |
                        dataset[,c(hiqual_var)]== "RSA higher diploma" |
                        dataset[,c(hiqual_var)]== "Other higher education below degree" |
                        dataset[,c(hiqual_var)]== "Level 4 Diploma" |
                        dataset[,c(hiqual_var)]== "Level 4 Certificate" |
                        dataset[,c(hiqual_var)]== "Level 5 Award" |
                        dataset[,c(hiqual_var)]== "NVQ level 3" |
                        dataset[,c(hiqual_var)]== "Advanced/Progression (14-19) Diploma" |
                        dataset[,c(hiqual_var)]== "Level 3 Diploma" |
                        dataset[,c(hiqual_var)]== "A-level or equivalent" |
                        dataset[,c(hiqual_var)]== "RSA advanced diploma" |
                        dataset[,c(hiqual_var)]== "OND/ONC/BTEC/SCOTVEC National etc" |
                        dataset[,c(hiqual_var)]=="City & Guilds Advanced Craft/Part 1" |
                        dataset[,c(hiqual_var)]=="Scottish 6 year certificate/CSYS" |
                        dataset[,c(hiqual_var)]=="SCE higher or equivalent" |
                        dataset[,c(hiqual_var)]=="AS-level or equivalent" |
                        dataset[,c(hiqual_var)]=="Level 3 Certificate" |
                        dataset[,c(hiqual_var)]== "Level 4 Award" |
                        dataset[,c(hiqual_var)]== "NVQ level 2 or equivalent" |
                        dataset[,c(hiqual_var)]=="Intermediate Welsh Baccalaureate"|
                        dataset[,c(hiqual_var)]=="RSA diploma" |
                        dataset[,c(hiqual_var)]=="City & Guilds Craft/Part 2" |
                        dataset[,c(hiqual_var)]== "BTEC/SCOTVEC First or General diploma etc" |
                        dataset[,c(hiqual_var)]=="Higher (14-19) Diploma" |
                        dataset[,c(hiqual_var)]=="Level 2 Diploma" |
                        dataset[,c(hiqual_var)]=="Level 2 Certificate" |
                        dataset[,c(hiqual_var)]=="O-level, GCSE grade A*-C or equivalent" |
                        dataset[,c(hiqual_var)]== "Level 3 Award" | 
                        dataset[,c(hiqual_var)]== "Foundation Welsh Baccalaureate" |
                        dataset[,c(hiqual_var)]== "Foundation (14-19) Diploma" |
                        dataset[,c(hiqual_var)]== "Level 1 Diploma" |
                        dataset[,c(hiqual_var)]=="CSE below grade 1, GCSE below grade C" |
                        dataset[,c(hiqual_var)]== "BTEC/SCOTVEC First or General certificate" |
                        dataset[,c(hiqual_var)]=="SCOTVEC modules" |
                        dataset[,c(hiqual_var)]=="RSA other" |
                        dataset[,c(hiqual_var)]=="Scottish Nationals Level 3" |
                        dataset[,c(hiqual_var)]=="Scottish Nationals below Level 3" |
                        dataset[,c(hiqual_var)]=="City & Guilds foundation/Part 1" |
                        dataset[,c(hiqual_var)]== "Level 1 Certificate" |
                        dataset[,c(hiqual_var)]=="Level 2 Award" |
                        dataset[,c(hiqual_var)]== "skills qualification" |
                        dataset[,c(hiqual_var)]== "Basic skills qualification"|
                        dataset[,c(hiqual_var)]== "Entry level qualification"|
                        dataset[,c(hiqual_var)]=="Entry level Diploma" |
                        dataset[,c(hiqual_var)]=="Entry level Certificate" |
                        dataset[,c(hiqual_var)]=="Level 1 Award" |
                        dataset[,c(hiqual_var)]== "Entry level Award" |
                        dataset[,c(hiqual_var)]=="Other qualification" |
                        dataset[,c(hiqual_var)]== "NVQ level 1 or equivalent" |
                        dataset[,c(hiqual_var)]=="NVQ level 1 or equivalent" |
                        dataset[,c(hiqual_var)]== "RSA/OCR/Cambridge Technical level 3" |
                        dataset[,c(hiqual_var)]== "NVQ level 5 (old) qualifications" |
                        dataset[,c(hiqual_var)]== "O Level/GCSE A*-C and equivalents" |
                        dataset[,c(hiqual_var)]== "A Level or equivalent" |
                        dataset[,c(hiqual_var)]== "Other qualifications" |
                        dataset[,c(hiqual_var)]== "Diploma in HE" |
                        dataset[,c(hiqual_var)]== "Nursing" |
                        dataset[,c(hiqual_var)]== "Teaching - Further Education" |
                        dataset[,c(hiqual_var)]== "GCSE < C/CSE < 1 and equivalents" |
                        dataset[,c(hiqual_var)]== "Other qualifications" |
                        dataset[,c(hiqual_var)]== "Teaching - not stated" |
                        dataset[,c(hiqual_var)]== "Basic Skills qualifications" |
                        dataset[,c(hiqual_var)]== "AS Level or equivalent" |
                        dataset[,c(hiqual_var)]== "GNVQ/GSCQ Intermediate" |
                        dataset[,c(hiqual_var)]== "Key/Core/Essential/Functional Skills" |
                        dataset[,c(hiqual_var)]== "Teaching - Foundation Stage" |
                        dataset[,c(hiqual_var)]== "Other HE below Degree level" |
                        dataset[,c(hiqual_var)]== "RQF Level 1 qualifications" |
                        dataset[,c(hiqual_var)]== "RSA/OCR/Cambridge Technical level 1" |
                        dataset[,c(hiqual_var)]=="RSA/OCR/Cambridge Technical other" |
                        dataset[,c(hiqual_var)]== "Advanced or Progression Diploma" |
                        dataset[,c(hiqual_var)]== "RSA/OCR/Cambridge Technical level 2" |
                        dataset[,c(hiqual_var)]== "Trade apprenticeship" |
                        dataset[,c(hiqual_var)]== "GNVQ/GSVQ intermediate" |
                        dataset[,c(hiqual_var)]== "GNVQ/GSVQ advanced" |
                        dataset[,c(hiqual_var)]== "Level 2  Certificate" |
                        dataset[,c(hiqual_var)]== "Level 6 Award" |
                        dataset[,c(hiqual_var)]== "Teaching - Secondary Education" |
                        dataset[,c(hiqual_var)]== "Scottish 6th Year Certificate" |
                        dataset[,c(hiqual_var)]== "GNVQ/GSVQ foundation level" |
                        dataset[,c(hiqual_var)]== "Key skills qualification" |
                        dataset[,c(hiqual_var)]== "YT/YTP certificate" |
                        dataset[,c(hiqual_var)]== "Level 2  Diploma" |
                        dataset[,c(hiqual_var)]== "Level 1  Certificate" |
                        dataset[,c(hiqual_var)]== "Entry level  Certificate" |
                        dataset[,c(hiqual_var)]== "Scottish National Level 5" |
                        dataset[,c(hiqual_var)]== "Access qualifications" |
                        dataset[,c(hiqual_var)]== "Level 1  Diploma" |
                        dataset[,c(hiqual_var)]== "Level 2  Award" |
                        dataset[,c(hiqual_var)]== "Level 1  Award"|
                        dataset[,c(hiqual_var)]== "RSA/OCR/Cambridge Technical level 4" |
                        dataset[,c(hiqual_var)]== "Entry Level qualifications" |
                        dataset[,c(hiqual_var)]== "Teaching - Primary Education" |
                        dataset[,c(hiqual_var)]== "Diploma (14-19)" |
                        dataset[,c(hiqual_var)]== "SVQ level 2 qualifications" |
                        dataset[,c(hiqual_var)]== "SVQ level 3 qualifications" |
                        dataset[,c(hiqual_var)]== "Teaching ¿secondary education" |
                        dataset[,c(hiqual_var)]== "Teaching ¿primary education" |
                        dataset[,c(hiqual_var)]== "Teaching ¿level not stated" |
                        dataset[,c(hiqual_var)]== "Teaching ¿further education" |
                        dataset[,c(hiqual_var)]== "Teaching ¿foundation stage" |
                        dataset[,c(hiqual_var)]== "Scottish National below level 3" |
                        dataset[,c(hiqual_var)]== "Entry level  Diploma" |
                        dataset[,c(hiqual_var)]== "Entry level  Award" |
                        dataset[,c(hiqual_var)]== "Scottish National below level 3" |
                        dataset[,c(hiqual_var)]== "SVQ level 4 qualifications" |
                        dataset[,c(hiqual_var)]== "O level, GCSE grade A-C or equivalent" |
                        dataset[,c(hiqual_var)]== "No qualif" |
                        dataset[,c(hiqual_var)]== "CSE below grade1,GCSE below grade c" |
                        dataset[,c(hiqual_var)]== "Other qualif" |
                        dataset[,c(hiqual_var)]== "OND,ONC,BTEC etc, national" |
                        dataset[,c(hiqual_var)]== "City & Guilds advanced craft/part 1" |
                        dataset[,c(hiqual_var)]== "HNC,HND,BTEC etc higher" |
                        dataset[,c(hiqual_var)]== "Teaching, primary educ" |
                        dataset[,c(hiqual_var)]== "Teaching, further educ" |
                        dataset[,c(hiqual_var)]== "A,S level or equivalent" |
                        dataset[,c(hiqual_var)]== "City & Guilds craft/part 2" |
                        dataset[,c(hiqual_var)]== "Diploma in higher educ" |
                        dataset[,c(hiqual_var)]== "Other higher educ below degree" |
                        dataset[,c(hiqual_var)]== "Teaching, secondary educ" |
                        dataset[,c(hiqual_var)]== "YT,YTP certificate" |
                        dataset[,c(hiqual_var)]== "City & Guilds Foundation/Part 1" |
                        dataset[,c(hiqual_var)]== "BTEC,SCOTVEC first/general diploma etc" |
                        dataset[,c(hiqual_var)]== "GNVQ,GSVQ foundation level" |
                        dataset[,c(hiqual_var)]== "Basic Skills Qualif" |
                        dataset[,c(hiqual_var)]== "International Bac'te" |
                        dataset[,c(hiqual_var)]== "Teaching, level not stated" |
                        dataset[,c(hiqual_var)]== "Key Skills Qualif" |
                        dataset[,c(hiqual_var)]== "SCE Higher or equivalent" |
                        dataset[,c(hiqual_var)]== "BTEC,SCOTVEC first/general certificate" |
                        dataset[,c(hiqual_var)]== "Scottish CSYS" |
                        dataset[,c(hiqual_var)]== "Entry Level qualif" |
                        dataset[,c(hiqual_var)]== "Foundation Welsh Bac'te"
                        ]<-"Non Grad"

dataset$Graduate_Type[(dataset[,c(hiqual_var)]=="Teaching ¿secondary education")] <- "Non Grad"
dataset$Graduate_Type[(dataset[,c(hiqual_var)]=="Teaching ¿primary education")] <- "Non Grad"
dataset$Graduate_Type[(dataset[,c(hiqual_var)]=="Teaching ¿level not stated")] <- "Non Grad"
dataset$Graduate_Type[(dataset[,c(hiqual_var)]=="Teaching ¿further education")] <- "Non Grad"
dataset$Graduate_Type[(dataset[,c(hiqual_var)]=="Teaching ¿foundation stage")] <- "Non Grad"

# setting those with foundation in degree71 with FD/foundation degree to non grad
if("DEGREE71" %in% names(dataset)){
  dataset$Graduate_Type[(dataset[,c(hiqual_var)]=="First degree/foundation degree"|
                           dataset[,c(hiqual_var)]=="First/Foundation degree" |
                           dataset[,c(hiqual_var)]=="First or Foundation Degree" | #different wording in HIQUAL22
                           dataset[,c(hiqual_var)]=="Degree Apprenticsehip" | #spelling incorrect in dataset
                           dataset[,c(hiqual_var)]=="Degree Apprenticeship" |
                           dataset[,c(hiqual_var)]=="Graduate Apprenticsehip"
  ) & (
    dataset$DEGREE71=="foundation degree")]<-"Non Grad"}



#Setting those left to not in scope
dataset$Graduate_Type[is.na(dataset$Graduate_Type)]<-"Not in scope"




#----Define AgeGroup----
dataset$AgeGroup[dataset$AGE %in% 16:20]<-"16-20"
dataset$AgeGroup[dataset$AGE %in% 21:30]<-"21-30"
dataset$AgeGroup[dataset$AGE %in% 31:40]<-"31-40"
dataset$AgeGroup[dataset$AGE %in% 41:50]<-"41-50"
dataset$AgeGroup[dataset$AGE %in% 51:60]<-"51-60"
#Note error in 2016 (age 60 in last bracket)
dataset$AgeGroup[dataset$AGE %in% 61:64]<-"61-64"


# New section added in Feb 2022 (2021 data) to reflect the change to the SOC 2020 codes
# for the 4 four quarters of the 2021 LFS data using the SC20MMJ variable

if("SC20MMJ" %in% names(dataset)){
  
  #Define skilled occupation groupings
  SOCHE1<-c("Managers, Directors And Senior Officials"        
            ,"Professional Occupations"                        
            ,"Associate Professionaloccupations")
  
  SOCHE2<-c("Administrative And Secretarial Occupations"      
            ,"Skilled Trades Occupations"                      
            ,"Caring, Leisure And Other Service Occupations")
  
  SOCHE3<-c("Sales And Customer Service Occupations"          
            ,"Process, Plant And Machine Operatives"           
            ,"Elementary Occupations")
  
  dataset$SOCHE[dataset$SC20MMJ %in% SOCHE1]<-"High Skilled Employment"
  dataset$SOCHE[dataset$SC20MMJ %in% SOCHE2]<-"Medium Skilled Employment"
  dataset$SOCHE[dataset$SC20MMJ %in% SOCHE3]<-"Low Skilled Employment"
  dataset$SOCHE[is.na(dataset$SOCHE)]<-"Else"
}


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

# New section on Occupation using updated SOC 2020 code inserted below in Feb 2022 
if("SC20MMJ" %in% names(dataset)){
  #* Occupation.
  dataset$Occupation<-dataset$SC20MMJ
  dataset$Occupation<-factor(dataset$Occupation, levels=c(levels(dataset$Occupation),"Medium Skilled Employment","Low Skilled Employment"))
  dataset$Occupation[dataset$SC20MMJ %in% c("Administrative And Secretarial Occupations",      
                                            "Skilled Trades Occupations",
                                            "Caring, Leisure And Other Service Occupations")]<-"Medium Skilled Employment"
  dataset$Occupation[dataset$SC20MMJ %in% c("Sales And Customer Service Occupations","Process, Plant And Machine Operatives"           
                                            ,"Elementary Occupations" )]<-"Low Skilled Employment"
}
#----Labels For  Occupation----- 					
#Managers, Directors and Senior Officials
#Professional Occupations
#Associate Professional Occupations		
#Medium Skilled Employment							
#Low Skilled Employment


assign(paste0("Q",quarter,"_",year),dataset,envir = globalenv())


}
