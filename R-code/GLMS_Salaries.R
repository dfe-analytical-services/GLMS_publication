

#----Employment rate by demographic----

#Find the median, lower and upper quartiles of salaries by demographic characteristic, and reshape (melt) data to have the characteristic as a variable rather than
#column name


#uses a weighted average method to provide an unbiased estimate. (as in method 2 here: https://www.xycoon.com/quartiles.htm)

Average_salary_by<-function(dataset,demographic){
  #find the weight variable
  weight<-tail(sort(names(dataset[grep("PIWT", names(dataset))])),1)

  group_by_at

  dataset%>%
    group_by_at(demographic)%>%
    dplyr::summarise(p25=weighted.quantile(GRSSWK, !!sym(weight), probs=0.25, type =4, collapse = TRUE),
                     p90=weighted.quantile(GRSSWK, !!sym(weight), probs=0.90, type =4, collapse = TRUE),
                     median=weighted.median(GRSSWK,!!sym(weight), type = 4, collapse = TRUE),
                     mean=weighted.mean(GRSSWK,!!sym(weight)),
                     sample=n(),
                     population=sum(!!sym(weight)))%>%
    as.data.frame()%>%
    mutate(variable=paste0(demographic, collapse = "_"))

  #take a dataset; based on the demographic characeristic separate the dataet into groups; find the quartiles and median of GRSSWK (gross weekly pay) and weight on PIWT16
  #reshape dataset for ease of output- turn the demographic into a variable rather than a column heading
}



Average_Salaries<-function(year){

  #select dataset: for example if quarter =1 and year =2016 this will get Q1_2016 and set this to 'dataset'
  dataset1<-get(paste0("Q1_",year))
  dataset2<-get(paste0("Q2_",year))
  dataset3<-get(paste0("Q3_",year))
  dataset4<-get(paste0("Q4_",year))

  #rename weight variable
  reformat_dataset<-function(dataset){
  colnames<-c(tail(sort(names(dataset[grep("PIWT", names(dataset))])),1),
              "Graduate_Type",
              "AgeGroup",
              "SEX",
              "ILODEFR",
              "FTPTWK",
              "GRSSWK")

  dataset<- dataset[,colnames]

  weight_var<-tail(sort(names(dataset[grep("PIWT", names(dataset))])),1)
  weight_index<-match(colnames(dataset),weight_var,0)
  colnames(dataset)[colnames(dataset) %in% weight_var]  <- c("PIWT")
  return(dataset)
  }

  dataset1<-reformat_dataset(dataset1)
  dataset2<-reformat_dataset(dataset2)
  dataset3<-reformat_dataset(dataset3)
  dataset4<-reformat_dataset(dataset4)


  dataset<-rbind(dataset1, dataset2, dataset3, dataset4)

   #Restrict to just those in full time employment
 dataset<-dataset[which(dataset$FTPTWK=="Full-time" & dataset$ILODEFR=="In employment"),]

 #find the weight variable for the dataset
 weight<-tail(sort(names(dataset[grep("PIWT", names(dataset))])),1)

  #restrict to just those with an income weight. Those with 0 weight wouldn't be included in the weighted median anyway, but removing at this stage speeds up computation.
  dataset<-dataset[which(dataset[,weight]!=0),]



  #----Gross Weekly pay in main job----


  ##Split by Age, Gender and Graduate Type

  all_age_sex<-Average_salary_by(dataset,c("AgeGroup","SEX", "Graduate_Type"))

  ##Split by Age and Graduate Type

  all_age<-Average_salary_by(dataset,c("AgeGroup", "Graduate_Type"))

  #add extra variable to show this is for any gender
  all_age$SEX<-"Total"

  ##Split by Gender and Graduate Type

  all_sex<-Average_salary_by(dataset,c("SEX", "Graduate_Type"))
  #add extra variable to show this is for any age
  all_sex$AgeGroup<-"Total"

 #Just split by graduate type (i.e. across all agebands)
  #Use the function just defined above
  all_total<-  Average_salary_by(dataset,"Graduate_Type")

#Add in an 'agegroup' for the average weekly salaries by graduate type and rename the column headers: this way we can merge the datasets as they'll have the same headers
  all_total$AgeGroup<-"Total"
  all_total$SEX<-"All"

  #append the average salaries for people by graduate type and ageband with those for all agebands.
  all_sal<-rbind(all_age, all_total, all_sex , all_age_sex)
  all_sal$year<-year

  #rename the young_grad_sal dataset to reference the year and quarter it's run on and output to the global environment.
  assign(paste0("Sal_",year),all_sal,envir = globalenv())
  #Also write to an Excel file for ease of use by economics team.
  #write.csv(all_sal,paste0("Outputs for GLMS/SAL_",year,".csv"))
}

#-----Graduate Breakdown salaries(16-64) & Most recent year only------

#Restrict to just graduates
#rename weight variable
grad_dat_reform<-function(dataset){

#rename and pick the latest people income weight, select collumns needed
  colnames<-c(tail(sort(names(dataset[grep("PIWT", names(dataset))])),1),
              "Graduate_Type",
              "AgeGroup",
              "SEX",
              "ILODEFR",
              "FTPTWK",
              "GRSSWK",
              "ETHNICITY",
              "DISEA",
              "STEM",
              "LEM",
              "DEGCLS7",
              "OSSAH",
              "Occupation",
              "INDE07M",
              "GOVTOF2")

  dataset<- dataset[,colnames]

  weight_var<-tail(sort(names(dataset[grep("PIWT", names(dataset))])),1)
  weight_index<-match(colnames(dataset),weight_var,0)
  colnames(dataset)[colnames(dataset) %in% weight_var]  <- c("PIWT")
  return(dataset)
}


  Graduate_breakdown_salaries<-function(year){

    #select dataset: for example if quarter =1 and year =2016 this will get Q1_2016 and set this to 'dataset
    dataset1 <- get(paste0("Q1_",year))
    dataset2 <- get(paste0("Q2_",year))
    dataset3 <- get(paste0("Q3_",year))
    dataset4 <- get(paste0("Q4_",year))

    #modify datasets using grad_dat_reform func. This selects the most up to date income
    #weight and renames it
    dataset1 <- grad_dat_reform(dataset1)
    dataset2 <- grad_dat_reform(dataset2)
    dataset3 <- grad_dat_reform(dataset3)
    dataset4 <- grad_dat_reform(dataset4)

    #bind quarterly dataset to make yearly dataset
    dataset<-rbind(dataset1, dataset2, dataset3, dataset4)

    #Restrict to just those in full time employment
    dataset<-dataset[which(dataset$FTPTWK=="Full-time" & dataset$ILODEFR=="In employment"),]

    #find the weight variable for the dataset
    weight<-tail(sort(names(dataset[grep("PIWT", names(dataset))])),1)

    #restrict to just those with an income weight. Those with 0 weight wouldn't be included in the weighted median anyway, but removing at this stage speeds up computation.
    dataset<-dataset[which(dataset[,weight]!=0),]


  #restrict to just graduates
  dataset<-dataset[which(dataset$Graduate_Type=="Graduate"),]

  #Average salary by ageband
  age<-Average_salary_by(dataset,"AgeGroup")
  age<-dplyr::rename(age, Value=AgeGroup)

  #Repeat for average salary by gender
  sex<-Average_salary_by(dataset,"SEX")
  sex<-dplyr::rename(sex, Value=SEX)

  #Average salary by ethnicity
  ethnicity<-Average_salary_by(dataset,"ETHNICITY")
  ethnicity<-dplyr::rename(ethnicity, Value=ETHNICITY)

  #Average salary by disability status

  disability<-Average_salary_by(dataset,"DISEA")
  disability<-dplyr::rename(disability, Value=DISEA)

  #Average salary by degree classification
  degreeclass<-Average_salary_by(dataset,"DEGCLS7")
  degreeclass<-dplyr::rename(degreeclass, Value=DEGCLS7)

  #Average salary by STEM/non-STEM graduates
  stem<-Average_salary_by(dataset,"STEM")
  stem<-dplyr::rename(stem, Value=STEM)
  stem$Value<-as.character(stem$Value)

  #Average salary by LEM/non-LEM graduates
  lem<-Average_salary_by(dataset,"LEM")
  lem<-dplyr::rename(lem, Value=LEM)
  lem$Value<-as.character(lem$Value)

  #Average salary by OSSAH/non-OSSAH graduates
  ossah<-Average_salary_by(dataset,"OSSAH")
  ossah<-dplyr::rename(ossah, Value=OSSAH)
  ossah$Value<-as.character(ossah$Value)

  #Average salary by occupational group
  occupation<-Average_salary_by(dataset,"Occupation")
  occupation<-dplyr::rename(occupation, Value=Occupation)

#Average salary by industry category
  industry<-Average_salary_by(dataset,"INDE07M")
  industry<-dplyr::rename(industry, Value=INDE07M)

  #Average salary by region
  region<-Average_salary_by(dataset,"GOVTOF2")
  region<-dplyr::rename(region, Value=GOVTOF2)

  #Average salary by Sex and industry category
  industry_sex<-Average_salary_by(dataset,c("SEX","INDE07M"))
  industry_sex<-dplyr::rename(industry_sex, Value=SEX,Value1=INDE07M)

  #append datasets to each other to create one dataset of graduate salaries for different demographic characteristics
      grad_sal<-bind_rows(list(age,
                    sex,
                    ethnicity,
                    disability,
                    degreeclass,
                    stem,
                    lem,
                    ossah,
                    occupation,
                    industry,
                    region,
                    industry_sex))


  #rename the grad_sal dataset to reference the year and quarter it's run on and output to the global environment.
  assign(paste0("grad_sal_",year),grad_sal,envir = globalenv())
  #Also write to an Excel file for ease of use by economics team.

  write.csv(grad_sal,paste0(filepath, "Outputs_for_GLMS/GRAD_SAL_",year,".csv"))

  saveRDS(object = grad_sal,
          file = paste0(filepath, "Outputs_for_GLMS/EES_rds/grad_sal_", year, ".rds"))


  #----Graduate Break down Salaries (21-30):young graduates----

  dataset<-dataset[which(dataset$Graduate_Type=="Graduate" & dataset$AgeGroup=="21-30"),]

  #Average salary by ageband
  age<-Average_salary_by(dataset,"AgeGroup")
  age<-dplyr::rename(age, Value=AgeGroup)

  #Repeat for average salary by gender
  sex<-Average_salary_by(dataset,"SEX")
  sex<-dplyr::rename(sex, Value=SEX)

  #Average salary by ethnicity
  ethnicity<-Average_salary_by(dataset,"ETHNICITY")
  ethnicity<-dplyr::rename(ethnicity, Value=ETHNICITY)

  #Average salary by disability status

  disability<-Average_salary_by(dataset,"DISEA")
  disability<-dplyr::rename(disability, Value=DISEA)

  #Average salary by degree classification
  degreeclass<-Average_salary_by(dataset,"DEGCLS7")
  degreeclass<-dplyr::rename(degreeclass, Value=DEGCLS7)

  #Average salary by STEM/non-STEM graduates
  stem<-Average_salary_by(dataset,"STEM")
  stem<-dplyr::rename(stem, Value=STEM)
  stem$Value<-as.character(stem$Value)

  #Average salary by LEM/non-LEM graduates
  lem<-Average_salary_by(dataset,"LEM")
  lem<-dplyr::rename(lem, Value=LEM)
  lem$Value<-as.character(lem$Value)

  #Average salary by OSSAH/non-OSSAH graduates
  ossah<-Average_salary_by(dataset,"OSSAH")
  ossah<-dplyr::rename(ossah, Value=OSSAH)
  ossah$Value<-as.character(ossah$Value)

  #Average salary by occupational group
  occupation<-Average_salary_by(dataset,"Occupation")
  occupation<-dplyr::rename(occupation, Value=Occupation)

  #Average salary by industry category
  industry<-Average_salary_by(dataset,"INDE07M")
  industry<-dplyr::rename(industry, Value=INDE07M)

  #Average salary by region
  region<-Average_salary_by(dataset,"GOVTOF2")
  region<-dplyr::rename(region, Value=GOVTOF2)

  #Average salary by Sex and industry category
  industry_sex<-Average_salary_by(dataset,c("SEX","INDE07M"))
  industry_sex<-dplyr::rename(industry_sex, Value=SEX,Value1=INDE07M)

  #append datasets to each other to create one dataset of graduate salaries for different demographic characteristics
      young_grad_sal<-bind_rows(list(age,
                          sex,
                          ethnicity,
                          disability,
                          degreeclass,
                          stem,
                          lem,
                          ossah,
                          occupation,
                          industry,
                          region,
                          industry_sex))

  #rename the young_grad_sal dataset to reference the year and quarter it's run on and output to the global environment.
  assign(paste0("young_grad_sal_",year),young_grad_sal,envir = globalenv())
  #Also write to an Excel file for ease of use by economics team.

  write.csv(young_grad_sal,paste0(filepath, "Outputs_for_GLMS/YOUNG_GRAD_SAL_", year, ".csv"))

  saveRDS(object = young_grad_sal,
          file = paste0(filepath, "Outputs_for_GLMS/EES_rds/young_grad_sal_", year, ".rds"))

  }
