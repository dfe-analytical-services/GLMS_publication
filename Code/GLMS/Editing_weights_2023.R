

# Drop the PWT22 columns 

Q1_2020_New <- subset(Q1_2020, select = -c(PWT22, PIWT22))
View(Q1_2020_New)

Q2_2020_New <- subset(Q2_2020, select = -c(PWT22, PIWT22))
View(Q2_2020_New)

Q3_2020_New <- subset(Q3_2020, select = -c(PWT22, PIWT22))
View(Q3_2020_New)

Q4_2020_New <- subset(Q4_2020, select = -c(PWT22, PIWT22))
View(Q4_2020_New)

Q1_2021_New <- subset(Q1_2021, select = -c(PWT22, PIWT22))
View(Q1_2021_New)

Q2_2021_New <- subset(Q2_2021, select = -c(PWT22, PIWT22))
View(Q2_2021_New)

Q3_2021_New <- subset(Q3_2021, select = -c(PWT22, PIWT22))
View(Q3_2021_New)

Q4_2021_New <- subset(Q4_2021, select = -c(PWT22, PIWT22))
View(Q4_2021_New)

# Save the RDS files in the Rds_datasets folder 

saveRDS(object = Q1_2020_New, 
        file = )

saveRDS(object = Q2_2020_New, 
        file =)

saveRDS(object = Q3_2020_New, 
        file = )

saveRDS(object = Q4_2020_New, 
        file = )



saveRDS(object = Q1_2021_New, 
        file = )

saveRDS(object = Q2_2021_New, 
        file = )

saveRDS(object = Q3_2021_New, 
        file =)

saveRDS(object = Q4_2021_New, 
        file = )