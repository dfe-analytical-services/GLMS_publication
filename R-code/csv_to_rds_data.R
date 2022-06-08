#----Convert .csv files into .rds files-----
#if the source("Code/GLMS_from_LFS_data.R") has identified  .rds datasets that has not read in well, manually convert these files 
#to .csv files, then Use this script to convert the .csv files into .rds files.

#This function will  read the csv, subset and filter as in the function above
#before saving as an RDS file.

#check that new .rds files have been created. It takes about an hour to run this script on 

Catch_remaining_datasets<-function(quarter, year){
  test <-read.csv(paste0(filepath, "CSV_datasets/", year," Q",quarter,".csv"))
  
  if(dim(test)[1]==0){#add in error warning 
    stop(return(paste0("Warning: dataset Q",quarter," ",year,"won't read from SPSS and doesn't exist as CSV:
                       action needed")))
  }
  
  colnames<-c("COUNTRY","SEX","AGE", names(test[grep("PWT", names(test))]), 
              names(test[grep("PIWT", names(test))]),
              names(test[grep("HIQUAL", names(test))]),"HIGHO",
              names(test[grep("DEGREE", names(test))]),"SC2KMMJ",
              "SC10MMJ","ETH11EW",
              "SNGDEGB","SNGHD","FDSNGDEG","ILODEFR","DISEA","DEGCLS7",
              "FTPTWK","GRSSWK", "INDE07M","UALA","CASENO","GOVTOF2","GRSSWK2","HOURPAY","YERQAL3","YERQAL2")
  
  #not all variables are in each dataset
  available_columns<-colnames[which(colnames %in% names(test))]
  
  test2<- test[,available_columns]
  
  
  #restrict to population in England aged 16-64
  test2<-test2[which(test2$COUNTRY=="England" & test2$AGE>15 & test2$AGE<65),]
  
  saveRDS(object = test2, file = paste0(filepath, "Rds_datasets/Q",quarter,"_",year,".rds"))
  assign(paste0("Q",quarter,"_",year),test2,envir = globalenv())
  }

#Put in the data sets that have been manually converted from .sav files to .csv files
sapply(2011:2011, function(y)sapply(3:4, function(x)Catch_remaining_datasets(x,y)))
sapply(2012:2012, function(y)sapply(1:2, function(x)Catch_remaining_datasets(x,y)))
sapply(2018:2018, function(y)sapply(1:2, function(x)Catch_remaining_datasets(x,y)))

