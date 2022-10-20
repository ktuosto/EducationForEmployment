
##Education for Employment

library(tidyverse)


## Load Data from USB

setwd("D:/")

Apps<-read.csv("Applications Report.csv")
Contacts<-read.csv("Contacts Report.csv")
ESC<-read.csv("Employment Status Check Data.csv")
PrePost<-read.csv("JTP Pre-Post Training Data.csv")


##How many Unique IDs in the ESC Data?
n_distinct(ESC$Contact.Case.Safe.ID)

## Remove people not in ESC from Apps
AppsJoin<-Apps%>%
  semi_join(ESC,by="Contact.Case.Safe.ID")

n_distinct(AppsJoin$Contact.Case.Safe.ID)

##Fewer unique IDs in Apps than is ESC for some reason, so in future removals also remove those not in Apps in addition to those not in ESC

## Remove people not in ESC or Apps from Contacts
ContactsJoin<-Contacts%>%
  semi_join(ESC,by="Contact.Case.Safe.ID")%>%
  semi_join(AppsJoin,by="Contact.Case.Safe.ID")

n_distinct(ContactsJoin$Contact.Case.Safe.ID)


## Remove people not in ESC or Apps from PrePost
PrePostFinal<-PrePost%>%
  semi_join(ESC,by="Contact.Case.Safe.ID")%>%
  semi_join(AppsJoin,by="Contact.Case.Safe.ID")


n_distinct(PrePostFinal$Contact.Case.Safe.ID)


##Get most recent application, and add field that sums number of apps per person
AppsFinal<-AppsJoin%>%
  group_by(Contact.Case.Safe.ID)%>%
  mutate(NumApps=n())%>%
  mutate(Application..Created.Date=as.Date(Application..Created.Date, '%m/%d/%Y'))%>%
  group_by(Contact.Case.Safe.ID)%>%
  slice(which.max(Application..Created.Date))


##Join Most Recent App to Contacts
ContactsFinal<-ContactsJoin%>%
  left_join(AppsFinal,by="Contact.Case.Safe.ID")


##Write Final Data back to Folder on USB

setwd("D:/For Data Dive")

write.csv(ContactsFinal,"Contacts.csv")
write.csv(PrePostFinal,"PrePost Surveys.csv")
write.csv(ESC,"Employment Status Checks.csv")






