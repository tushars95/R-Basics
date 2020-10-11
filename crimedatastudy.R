# 1>........................................
kidnap39= read.csv('39_Specific_purpose_of_kidnapping_and_abduction.csv')
kidnap39$K_A_Grand_Total=as.numeric(as.character(kidnap39$K_A_Grand_Total))
str(kidnap39)
reason= kidnap39 %>% 
  filter(!Group_Name == "Kidnap - Total" & !Group_Name == "Kidnap - For Other Purposes") %>% 
  group_by(ï..Area_Name,Group_Name) %>% 
  summarise(Total=sum(K_A_Grand_Total,na.rm = T))%>%top_n(3)

View(reason)

ggplot(reason,aes(Group_Name,Total))+ 
  geom_bar(stat = 'Identity',width = .5, aes(fill= Group_Name)) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        axis.title.x = element_blank())+
  facet_wrap(~ï..Area_Name)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# 2>......................................
#offender relation with rape victims
offender= read.csv('21_Offenders_known_to_the_victim.csv')
col=c("Neigh","other_know","family_mem","Relative")
colnames(offender)[4:7]=col
#converting the same in long data
offender_long= gather(offender,Relationship,Cases,Neigh:Relative)
View(offender_long)

offender_final= offender_long %>% group_by(Area_Name,Relationship)%>% summarise(Total.case =sum(Cases,na.rm = T))
View(offender_final)

ggplot(offender_final,aes(Relationship,Total.case))+ 
  geom_bar(stat = 'Identity',width = .5, aes(fill= Relationship)) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        axis.title.x = element_blank())+
  facet_wrap(~Area_Name)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# 3>.....................
education= read.csv('18_01_Juveniles_arrested_Education.csv')
economic= read.csv('18_02_Juveniles_arrested_Economic_setup.csv')
family_bck= read.csv('18_03_Juveniles_arrested_Family_background.csv')

#education
col=c("Below_Matric","Illiterate","Above_Matric","Upto_Primary")
colnames(education)[4:7]=col
View(education)
education_long=gather(education,edu_level,Cases,Below_Matric,Illiterate,Above_Matric,Upto_Primary)
View(education_long)
education_final= education_long %>% group_by(edu_level)%>%
  summarise(Total.cases=sum(Cases))
View(education_final)

o=ggplot(education_final,aes(edu_level,Total.cases))+ 
  geom_bar(stat = 'Identity',width = .5, aes(fill= edu_level)) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        axis.title.x = element_blank())
#economic
col=c("no250001_to_50000","upto_25000","no100001_to_200000","no50001_to_100000","above_300000","no200001_to_300000")
colnames(economic)[4:9]=col
View(economic)
economic_long=gather(economic,Salary,no.of.cases,no250001_to_50000:no200001_to_300000 )
View(economic_long)
economic_final= economic_long %>% group_by(Salary)%>%
  summarise(Total.Cases=sum(no.of.cases))
View(economic_final)

t=ggplot(economic_final,aes(Salary,Total.Cases))+ 
  geom_bar(stat = 'Identity',width = .5, aes(fill= Salary)) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        axis.title.x = element_blank())
#family_bck
col=c("Homeless","With_Guardian","With_parents")
colnames(family_bck)[4:6]=col
View(family_bck)
family_bck_long=gather(family_bck,family_background,Cases,Homeless,With_Guardian,With_parents)
View(family_bck_long)
family_bck_final= family_bck_long %>% group_by(family_background)%>%
  summarise(Total.cases=sum(Cases))
View(family_bck_final)

th=ggplot(family_bck_final,aes(family_background,Total.cases))+ 
  geom_bar(stat = 'Identity',width = .5, aes(fill= family_background)) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        axis.title.x = element_blank())
install.packages("gridExtra")
library(gridExtra)
grid.arrange(o,t,th)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# 4>.....................
child_01_12= read.csv('03_District_wise_crimes_committed_against_children_2001_2012.csv')
child_01_12_tot= child_01_12 %>% group_by(STATE.UT)%>%
  summarise(Total.cases=sum(Total))

child_13= read.csv('03_District_wise_crimes_committed_against_children_2013.csv')
child_13_tot= child_13 %>% group_by(STATE.UT)%>%
  summarise(Total.cases=sum(Total))

women_01_12= read.csv('42_District_wise_crimes_committed_against_women_2001_2012.csv')
women_01_12_tot= women_01_12 %>% group_by(STATE.UT)%>%
  summarise(Total.cases=sum(Rape,Kidnapping.and.Abduction,Dowry.Deaths,Assault.on.women.with.intent.to.outrage.her.modesty,Insult.to.modesty.of.Women,Cruelty.by.Husband.or.his.Relatives,Importation.of.Girls,na.rm = T))

women_13= read.csv('42_District_wise_crimes_committed_against_women_2013.csv')
women_13_tot= women_13 %>% group_by(STATE.UT)%>%
  summarise(Total.cases=sum(Rape,Kidnapping.and.Abduction,Dowry.Deaths,Assault.on.women.with.intent.to.outrage.her.modesty,Insult.to.modesty.of.Women,Cruelty.by.Husband.or.his.Relatives,Importation.of.Girls,na.rm = T))

child_13_tot$STATE.UT = format(toupper(child_01_12_tot$STATE.UT))
View(child_01_12_tot)
View(child_13_tot)

women_13_tot$STATE.UT = format(toupper(women_01_12_tot$STATE.UT))
View(women_01_12_tot)
View(women_13_tot)
# rbind
women.1=rbind(women_13_tot,women_01_12_tot)
women.cases=women.1 %>% group_by(STATE.UT) %>% summarise(Total=sum(Total.cases))
View(women.cases)
str(women.cases)
child.cases=rbind(child_13_tot,child_01_12_tot)
View(women.cases)
t13=data_13 %>% group_by(STATE.UT) %>% summarise(total=sum(Total.cases))
View(t13)
data_01_12=rbind(women_01_12_tot,child_01_12_tot)
t01_12=data_01_12 %>% group_by(STATE.UT) %>% summarise(total=sum(Total.cases))
View(t01_12)

x = women_01_12_tot$STATE.UT

`.rowNamesDF<-`(women_13_tot, make.names = FALSE, x)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# 5>.........

complaint=read.csv("25_Complaints_against_police.csv")
max_complaint=complaint%>%group_by(ï..Area_Name)%>%summarise(total_cases=sum(CPA_._Cases_Registered))%>%arrange(desc(total_cases))%>%head(5)
View(max_complaint)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# 6>..........

min.crime01_12=read.csv("01_District_wise_crimes_committed_IPC_2001_2012.csv")
min.crime13=read.csv("01_District_wise_crimes_committed_IPC_2013.csv")
min.crime14=read.csv("01_District_wise_crimes_committed_IPC_2014.csv")


min.crime01to12= min.crime01_12 %>% group_by(STATE.UT) %>% summarise(Total=sum(TOTAL.IPC.CRIMES)) %>% arrange(Total)
min.crime_13= min.crime13 %>% group_by(STATE.UT) %>% summarise(Total=sum(TOTAL.IPC.CRIMES)) %>% arrange(Total)
min.crime_14= min.crime14 %>% group_by(States.UTs) %>% summarise(Total=sum(Total.Cognizable.IPC.crimes)) %>% arrange(Total)

colnames(min.crime_14)[1]=c("STATE.UT")

se=rbind(min.crime_14,min.crime_13)

min.crime = se %>% group_by(STATE.UT)%>%summarise(Total=sum(Total))%>%arrange(Total)
View(min.crime)
ggplot(min.crime,aes(STATE.UT,Total))+ 
  geom_bar(stat = 'Identity',width = .5, aes(fill= STATE.UT)) +
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        axis.title.x = element_blank())

View(min.crime01to12)
View(min.crime_13)
