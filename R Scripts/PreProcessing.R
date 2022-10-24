
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


## Remove people not in  Apps from ESC
ESCJoin<-ESC%>%
  semi_join(AppsJoin,by="Contact.Case.Safe.ID")

n_distinct(ESCFinal$Contact.Case.Safe.ID)

#Create DeIdentifying key

ContactKey<-ESCJoin%>%
  group_by(Contact.Case.Safe.ID)%>%
  mutate(ContactID=cur_group_id())%>%
  group_by(Contact.Case.Safe.ID,ContactID)%>%
  summarise(n=n())%>%
  dplyr::select(1,2)

ClassKey<-ESCJoin%>%
  group_by(Class..Class.Name)%>%
  mutate(ClassID=cur_group_id())%>%
  group_by(Class..Class.Name,ClassID)%>%
  summarise(n=n())%>%
  dplyr::select(1,2)

ProjectKey<-PrePost%>%
  group_by(Class..Project.Name)%>%
  mutate(ProjectID=cur_group_id())%>%
  group_by(Class..Project.Name,ProjectID)%>%
  summarise(n=n())%>%
  dplyr::select(1,2)

CompanyKey<-ESCJoin%>%
  group_by(Current.Company.Name)%>%
  mutate(ComapnyID=cur_group_id())%>%
  group_by(Current.Company.Name,ComapnyID)%>%
  summarise(n=n())%>%
  dplyr::select(1,2)


##Remove Actual Contact ID from ESC
ESCFinal<-ESCJoin%>%
  left_join(ContactKey,by="Contact.Case.Safe.ID")%>%
  left_join(ClassKey,by="Class..Class.Name")%>%
  left_join(CompanyKey,"Current.Company.Name")%>%
  dplyr::select(-1)%>%
  dplyr::select(-Class..Class.Name)%>%
  dplyr::select(-Current.Company.Name)%>%
  dplyr::relocate("ContactID",before=1)

n_distinct(ESCFinal$ContactID)

ESCFinal$Current


## Remove people not in ESC or Apps from Contacts
ContactsJoin<-Contacts%>%
  semi_join(ESC,by="Contact.Case.Safe.ID")%>%
  semi_join(AppsJoin,by="Contact.Case.Safe.ID")

n_distinct(ContactsJoin$Contact.Case.Safe.ID)



## Remove people not in ESC or Apps from PrePost, remove real contact ID
PrePostFinal<-PrePost%>%
  semi_join(ESC,by="Contact.Case.Safe.ID")%>%
  semi_join(AppsJoin,by="Contact.Case.Safe.ID")%>%
  left_join(ContactKey,by="Contact.Case.Safe.ID")%>%
  left_join(ClassKey,by="Class..Class.Name")%>%
  left_join(ProjectKey,by="Class..Project.Name")%>%
  dplyr::select(-1,-3,-5)%>%
  dplyr::relocate("ContactID",before=1)


n_distinct(PrePostFinal$Contact.Case.Safe.ID)
n_distinct(PrePostFinal$ContactID)


##Get most recent application, and add field that sums number of apps per person
AppsFinal<-AppsJoin%>%
  group_by(Contact.Case.Safe.ID)%>%
  mutate(NumApps=n())%>%
  mutate(Application..Created.Date=as.Date(Application..Created.Date, '%m/%d/%Y'))%>%
  group_by(Contact.Case.Safe.ID)%>%
  slice(which.max(Application..Created.Date))

n_distinct(AppsFinal$Contact.Case.Safe.ID)



##Join Most Recent App to Contacts, remove real contact ID from final contacts
ContactsFinal<-ContactsJoin%>%
  left_join(AppsFinal,by="Contact.Case.Safe.ID")%>%
  left_join(ContactKey,by="Contact.Case.Safe.ID")%>%
  dplyr::select(-1)%>%
  dplyr::relocate("ContactID",before=1)

n_distinct(ContactsFinal$ContactID)

##Check there are no original contact IDs left - should all be NULL

unique(ContactsFinal$Contact.Case.Safe.ID)
unique(PrePostFinal$Contact.Case.Safe.ID)
unique(ESCFinal$Contact.Case.Safe.ID)


##Write Final Data back to Folder on USB








setwd("C:/Users/rcarder/Documents/")


write.csv(ContactsFinal,"Contacts.csv")
write.csv(PrePostFinal,"PrePost Surveys.csv")
write.csv(ESCFinal,"Employment Status Checks.csv")

##Write Lookup Keys

#setwd("D:/Lookup Keys")
write.csv(ContactKey,"ContactKey.csv")
write.csv(CompanyKey,"CompanyKey.csv")
write.csv(ProjectKey,"ProjectKey.csv")
write.csv(ClassKey,"ClassKey.csv")


