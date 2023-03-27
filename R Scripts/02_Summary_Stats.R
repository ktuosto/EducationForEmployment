##Education for Employment
## Last modified by YNW on 3/27/23

## Set up 
rm(list=ls())

library(tidyverse)

# Set wd
parent_dir <- dirname(getwd())

## Read file
df <- read.csv(paste0(parent_dir,"/ContactsProcessed.csv"))

# groups to summarize 
groups <- c("Gender", "Nationality", "Speaks.Arabic", 
            "Speaks.English", "Age", "Beneficiary.Specialization",
            "Level.of.Education")

# Create list of the retention groups so we can loop over outcomes by each of the group
retention_groups <- c("pl_grad", "pl_3", "pl_6", "pl_9", "pl_12")

# create mean by country by months of retention every time
calculate_mean <- function(df) {
 # for (group in retention_groups) {
  df %>%
    group_by(Country.of.Programming) %>%
    summarize_at(vars(groups), mean, na.rm = TRUE)
  
  #}
}

calculate_mean(df)
