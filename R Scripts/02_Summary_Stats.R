
##Education for Employment

library(tidyverse)


## Load Data from USB
user <- Sys.getenv("USERNAME")
df <- read.csv(paste0("C:/Users/", user,"/Documents/GitHub/EducationForEmployment/ContactsProcessed.csv"))


# groups to summarize 
groups <- colnames(df)
colnames(df)
# by country by months of retention everytime