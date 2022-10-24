rm(list=ls())

### Load Libraries ####
library(tidyverse)
library(dplyr)

### Set Directory ####
user <- Sys.getenv("USERNAME")
clone <- paste0("C:/Users/", user, "/GitHub/EducationForEmployment")
setwd(clone)

## Read Files ##
contacts <- read.csv(paste0(clone, "/01_raw/ContactsCWK2.csv"))

## Clean the contacts file 
names(contacts)[28] <- "placement_grad"
names(contacts)[29] <- "placement_grad_data"
names(contacts)[30] <- "placement_3"
names(contacts)[31] <- "placement_3_data"
names(contacts)[32] <- "placement_6"
names(contacts)[33] <- "placement_6_data"
names(contacts)[34] <- "placement_9"
names(contacts)[35] <- "placement_9_data"
names(contacts)[36] <- "placement_12"
names(contacts)[37] <- "placement_12_data"

cols.num <- c(28:37)
contacts[cols.num] <- sapply(contacts[cols.num],as.numeric)

## Make collapse job placement month column
contacts <- contacts %>% mutate(months_job=ifelse(placement_grad_data==0 & placement_3_data==0 & 
                                                    placement_6_data==0 & placement_9_data==0 & placement_12_data==0, NA,
                                                  ifelse(placement_3==1 & placement_grad==0, 3,
                                                         
                                                         ifelse(placement_6==1 & placement_3 ==0 & placement_grad ==0, 6,
                                                                ifelse(placement_9==1 & placement_6==0 & placement_3 ==0 & placement_grad ==0, 9,
                                                                       ifelse(placement_12==1 & placement_9==0 & placement_6==0 & placement_3 ==0 & placement_grad ==0, 12, 
                                                                              ifelse(placement_grad ==1, 0, 
                                                                                     ifelse(placement_grad ==1 & placement_12==1 & placement_9==1 & placement_6==1 & placement_3 ==1, 0, 
                                                                                            ifelse(placement_12 == 0, 0, -9)))))))))


### Exported this part for Martin's ML Model
#write.csv(contacts, "Contacts_YW.csv")
## Rename some other columns to keep 
names(contacts)[25] <- "retained_days"
names(contacts)[86] <- "monthly_fam_inc"

# Keep columns of interest
contacts <- contacts[c(2,4:12,111,112,115)]

## Merge into employment data
employment <- read.csv(paste0(clone, "/01_raw/Employment Status Checks Blind - Employment Status Checks Blind.csv"))

## Make all combinations of "Status Type Check" and ContactID
emp_subset <- tidyr::complete(employment, ContactID, Employment.Status.Check.Type)

# Reorder
emp_subset <- emp_subset[,c(1,3,4,5,6,7,2, 8:140)]

#table(employment$Employment.Status.Check.Type)
## Some Cleaning
emp_subset <- emp_subset[c(1,2,6,7,11,12,14,16,19,20,27,29,49,50,51,55,57,58,59,60,61,62,63,64, 117,137)]

## Rename some stuff
emp_subset <- rename_with(emp_subset, tolower)
names(emp_subset)[3] <- "survey_date"
names(emp_subset)[4] <- "status_check_type"
names(emp_subset)[5] <- "currently_working"
names(emp_subset)[6] <- "current_position"
names(emp_subset)[7] <- "current_contract"
names(emp_subset)[8] <- "current_contract_length"
names(emp_subset)[9] <- "industry_emp_1"
names(emp_subset)[10] <- "industry_emp_2"
names(emp_subset)[11] <- "current_same_emp"
names(emp_subset)[12] <- "current_promoted"
names(emp_subset)[13] <- "satisfaction_growth"
names(emp_subset)[14] <- "satisfaction_supervisorrel"
names(emp_subset)[15] <- "income_monthly"
names(emp_subset)[16] <- "income_sufficient"
names(emp_subset)[17] <- "hoursworked"
names(emp_subset)[18] <- "satisfaction_hoursworked"
names(emp_subset)[19] <- "benefits"
names(emp_subset)[20] <- "benefits_other"
names(emp_subset)[21] <- "previous_secured_emp"
names(emp_subset)[22] <- "previous_position_type"
names(emp_subset)[23] <- "previous_emp_start_date"
names(emp_subset)[24] <- "previous_contract_type"
names(emp_subset)[25] <- "current_studying"
names(emp_subset)[26] <- "leavejob_mainreason"

# Remove Blank row 
emp_subset <- emp_subset %>% filter(status_check_type!="")

# Drop Duplicates (same contact on the same date)
emp_subset <-  emp_subset %>% distinct(contactid, status_check_type, .keep_all=TRUE)

## Only interested in status check below 6 months
#emp_subset$filter <- substr(emp_subset$status_check_type, 1, 1) 
#emp_subset <- emp_subset %>% filter(filter != "1" & filter != "9" & filter != "N")


## Rename NAs so that the dataset can be reshaped
emp_subset <- emp_subset %>% mutate_if(is.numeric , replace_na, replace = -9)
emp_subset <- emp_subset %>% mutate_if(is.character , replace_na, replace = "NA")
emp_subset[emp_subset==""] <- "NA"

## Modify column for reshape 
emp_subset$status_check_type <- substr(emp_subset$status_check_type, 1, 14) 
emp_subset$status_check_type <- str_replace(emp_subset$status_check_type, "-Month ", "_")
emp_subset$status_check_type <- str_replace(emp_subset$status_check_type, "-", "_")
emp_subset$status_check_type <- str_replace(emp_subset$status_check_type, "At Grad ", "0_")
emp_subset$status_check_type <- str_replace(emp_subset$status_check_type, "Placem", "place")
emp_subset$status_check_type <- str_replace(emp_subset$status_check_type, "Post_P", "post")

# Remove special characters
emp_subset <- emp_subset %>% mutate(across(everything(), ~gsub("[^[:alnum:][:blank:]+?&/\\-]", "", .x)))
emp_subset <- as.data.frame(gsub("[[:punct:]]", "", as.matrix(emp_subset))) 



## Reshape Wide 
emp_wide <- emp_subset[-c(2,3,27)]

emp_wide <- emp_wide %>% 
  pivot_wider(names_from = status_check_type, values_from = c(3:24))

## Create a new row ID
emp_wide <- emp_wide %>% mutate(rowid = row_number())

# After reshape, turn the NAs back to NAs
emp_wide[emp_wide == -9] <- NA 
emp_wide[emp_wide == "NA"] <- NA 

## Merge contacts and employment data
merged <- merge(emp_wide, contacts, by.x="contactid", by.y="ContactID")

# Reorder
merged <- merged[,c(1,200:212, 2:199)]
merged <- merged[,c(1:14, )]

# Make the "NA" coding consistent
merged[merged == "#NA"] <- NA 
