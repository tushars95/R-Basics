install.packages("dplyr")

library(dplyr)


odi= read.csv('odi-batting.csv')
dim(odi)

# Filter

# data for Sachin tendulkar

View(odi)

Sachin = filter(odi, Player =='Sachin R Tendulkar')

dim(Sachin)
View(Sachin)

Pt = filter(odi, Player =='Sachin R Tendulkar' | Player == 'Ricky T Ponting')

dim(Pt)

hundreds= filter(odi, Player == 'Sachin R Tendulkar' , Versus== 'Australia' , Runs >=100)

dim(hundreds)

#####################################################

# Using %n% pipeline function

#method 1

Player_vec = c("Sachin R Tendulkar", "Ricky T Ponting" , "Sourav C Ganguly")
Filter_data = filter(odi, Player %in% Player_vec)
View(Filter_data)
dim(Filter_data)


#method 2
Filter_data_1 = filter(odi, Player %in%
                         c("Sachin R Tendulkar", "Ricky T Ponting" , "Sourav C Ganguly"))
dim(Filter_data_1)


###########################################################

#GROUPING

Top_10= odi %>% group_by(Player) %>%
  summarise(Total_Runs = sum(Runs, na.rm = T)) %>%
  arrange(-Total_Runs)%>% head(10)

dim(Top_10)

View(Top_10)


#top 10 ground based on no. of runs

Top_10_gd= odi %>% group_by(Ground) %>%
  summarise(Total_Runs = sum(Runs, na.rm = T)) %>%
  arrange(-Total_Runs)%>% head(10)

dim(Top_10_gd)

View(Top_10_gd)


#top 10 ground based on no. of matches

Top_10_gd_match= odi %>% group_by(Ground) %>%
  summarise(Total_Matches = n_distinct(MatchDate)) %>%
  arrange(-Total_Matches)%>% head(10)

dim(Top_10_gd_match)

View(Top_10_gd_match)


#top 10 player based on avg score rate and also find total runs

Top_10_gd_avg= odi %>% group_by(Player) %>%
  summarise(Avg = round(mean(ScoreRate),2), Total_Runs = sum(Runs, na.rm = T)) %>%
  arrange(-Avg)%>% head(10)

View(Top_10_gd_avg)

dim(Top_10_gd_avg)

###############################################################


# HR dataset - Total no. of employee, avg age, avg month income , female, male

hr= read.csv('HR Analytics.csv')
View(hr)

dim(hr)


Total_emp= hr %>% group_by(JobRole) %>%
  summarise(Total_emp = n_distinct(EmployeeNumber),
            Avg_age = round(mean(Age),2),
            Avg_month_income = round(mean(MonthlyIncome),2),
            Female = sum(Gender %in% 'Female'),
            Male = sum(Gender %in% 'Male'))
View(Total_emp)

#############Or###########

Total_emp= hr %>% group_by(JobRole) %>%
  summarise(Total_emp = n_distinct(EmployeeCount),
            Avg_age = round(mean(Age),2),
            Avg_month_income = round(mean(MonthlyIncome),2),
            No_of_Female = sum(Gender == 'Female'),
            No_of_Male = sum(Gender == 'Male'))
View(Total_emp)
#############################

#ODI DATE UNDERSTANDING

odi= read.csv('odi-batting.csv')
View(odi)

odi$Date = as.Date(odi$MatchDate, format = "%m-%d-%Y")
View(odi$Date)
odi$Month = format(odi$Date, "%b")
View(odi$Month)

months = odi$Month
days = odi$Date

month_wise = odi %>% group_by(Month) %>% 
  summarise(total_match= n_distinct(MatchDate)) %>%
  arrange(-total_match)
View(month_wise)


#LAST MATCH OF SACHIN IN WHICH HE SCORE CENTURY

Sachin_last_match = odi %>% filter(Player == "Sachin R Tendulkar", Runs> 99) %>%
  arrange(desc(Date)) %>% head(1)
View(Sachin_last_match)

#total match month wise

month_wise = odi %>% group_by(Month) %>% 
  summarise(total_match= n_distinct(MatchDate)) %>%
  arrange(-total_match)
View(month_wise)


#Ground on which sachin scored centuries

sachin_Ground = odi %>% filter(Player == "Sachin R Tendulkar", Runs>99) %>%
  group_by(Ground)%>% summarise(No.of.cent = n_distinct(Runs , na.rm = TRUE)) %>% arrange(-No.of.cent)

View(sachin_Ground)

#total matches, no. of cent, no. of 50s, no. of ducks

player_name = odi %>% group_by(Player) %>%
  summarise(total_matches= n_distinct(MatchDate), 
            No.of.cent = sum(Runs>99 , na.rm = TRUE), 
            no.of.50s = sum(Runs<99 & Runs>49 , na.rm = TRUE), 
            no.of.duck = sum(Runs ==0 ,na.rm = TRUE))
View(player_name)  

# OR

odi$Cent = ifelse(odi$Runs>99,1,0)
odi$half = ifelse(odi$Runs>49 & odi$Runs<100,1,0)
odi$duck = ifelse(odi$Runs == 0,1,0)
View(odi$Cent)
View(odi$half)
View(odi$duck)

player_wise = odi %>% group_by(Player) %>%
  summarise(No.of.matches=n(),
            coun.of.cent=sum(Cent, na.rm = TRUE),
            coun.of.half=sum(half,na.rm = TRUE),
            coun.of.duck=sum(duck,na.rm = TRUE))
            View(player_wise)


#performance of team india against all country

Team_india = odi %>%filter(Country == "India") %>% 
  group_by(Versus) %>%
  summarise(total_matches= n_distinct(MatchDate),
            total_runs=sum(Runs),
            total_cent=sum(Runs>99),
            Avg.runs= round((total_runs/total_matches),2)) 
            View(Team_india)            

#highest run by each player

player_name_list = odi %>% group_by(Player) %>%
  summarise(Highest_run = max(Runs,na.rm = TRUE))
  View(player_name_list)
  
  
  
# Group on multiple columns
  
  
double_grp=odi %>%
  group_by(Country,Player,Ground) %>%
  summarise(total_runss=sum(Runs,na.rm = TRUE))
  View(double_grp)
  
  
######################################################



#1st vector

Emp_id = c(101,102,103,104,105)


Emp_Name = c("Sathish", "Amit" ,"Rahul","Suraj","Ram" )

Emp_Salary = c(10000,20000,30000,40000,50000)

Emp_data = data.frame(Emp_id,Emp_Name,Emp_Salary)


View(Emp_data)

str(Emp_data)

#new Emp

Emp_id = c(106,107)

Emp_Name = c("suresh", "mohan")

Emp_Salary = c(60020,70200)

Emp_New_Data = data.frame(Emp_id,Emp_Name,Emp_Salary)

View(Emp_New_Data)

Final = rbind(Emp_data , Emp_New_Data)

View(Final)

#OR
Final_Data = rbind(Final,
                   data.frame(Emp_id = c(108,109),
                              Emp_Name = c("surh", "ohan"),
                              Emp_Salary = c(60020,70200)
                   ))
View(Final_Data)

Emp_location = data.frame(Emp_Name=c("Amit","suraj","Mohan", "Ram"),Location =c("UT","PN","KA","KE") )

View(Emp_location)

Emp_with_Location = cbind(Final_Data, Emp_location)





                      ###data column selection ########


# i want to take column from country to ground

odi_new = odi[,1:5]
dim(odi_new)

odi_new = odi[,c(1,2,4,5)]

# using dplyr

odi_new=odi%>% select(Country,Player, Ground)

# i want to get rid of cent URl

odi= odi%>% select(-c(URL))
View(odi_new)

# in select fn -pick those column start with a or b or d

odi_new=odi%>% select(matches())

lead = read.csv('Source_wise_lead.csv')

dim(lead)

addmission = read.csv('Source_wise_Addmission.csv')

dim(addmission)

# JOIN
lead_analysis= merge(x= lead, y = addmission, by = "Source.Bucket", all.x = TRUE)

dim(lead_analysis)
View(lead_analysis)

lead_analysis$conversion= round((lead_analysis$Enroll_count/lead_analysis$Lead.count)*100,2)

View(lead_analysis)

###########################



