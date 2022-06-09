

#Older versions of HIQUAL15 don't have value labels- just numbers- but we can use the value labels
#of Q1_2016 to recode the numbers as text strings




HIQUAL15<-seq(1:85)
HIQUAL15_values<-levels(Q1_2016$HIQUAL15)

HIQUAL15_ref<-tibble(HIQUAL15_values,HIQUAL15)

Q1_2015<-left_join(Q1_2015,HIQUAL15_ref,by="HIQUAL15")
Q1_2015<-Q1_2015%>%select(-HIQUAL15)
setnames(Q1_2015,"HIQUAL15_values","HIQUAL15")
