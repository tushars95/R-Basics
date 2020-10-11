data1=mtcars
View(mtcars)

# how many cars have automatic transmission and manual in  each gears
cTable = table(data1$gear, data1$am)
cTable
#add the column sum 
ctable = rbind(cTable, Total =colSums(cTable))
ctable

# row sum
cTable=cbind(cTable, Total =rowSums(cTable))
cTable
# what % of car in 4 gear system are automatic
gTable = table(data1$gear ,data1$cyl, data1$am, data1$vs)
gTable
View(gTable)

wide = read.table(header = T, text ='
                  subject sex week1 week2 week3
                  kid1    M   7.9   12.3  10.7
                  kid2    F   6.3   10.6  11.1
                  kid3    F   9.5   13.4  13.8
                  kid4    M  11.5   13.4  12.9
                 ')
View(wide)
str(wide)
wide2 = read.table(header = T, text ='
                  subject sex week   weight
                  kid1    M   week1    9
                  kid2    F   week2    12
                  kid3    F   week3    7.3
                  ')
View(wide2)
library(tidyr)

long = gather(wide,week,weight,week1:week3)

View(long)

gTable= as.data.frame(gTable)
gTable_wide = spread(gTable, Var4 ,Freq)
View(gTable_wide)

gTable_2

install.packages('rpivotTable')
library(rpivotTable)
rpivotTable(data1)

data3=cars
rpivotTable(data3)


library(psych)
summary(odi_batting)
hist(odi_batting$Runs, xlab = "Run bucket" ,ylab="Frequency")
skew(odi_batting$Runs)

odi_batting$Runs_sqrt = sqrt(odi_batting$Runs)
View(odi_batting$Runs_sqrt)  

skew(odi_batting$Runs_sqrt,na.rm = T)
mean(odi_batting$Runs_sqrt,na.rm = T)

View(odi_batting)
###################
#calculation the quateriles
quantile(odi_batting$Runs, na.rm = T)
U.w = 32+ 1.5*(32-4)

#removing outlier
odi_batting$status =ifelse(odi_batting$Runs > U.w, "Outlier", "Normal")

library(dplyr)
t = filter(odi_batting ,status=="Normal")
mean(t$Runs, na.rm = T)

odi_batting %>% filter(status== "Normal") %>%
  summarise(Avg =mean(Runs, na.rm = T))

###################################

hr= read.csv('HR Analytics.csv')
View(hr)

#boxplot using ggplot


library(ggplot2)
data1 = mpg
View(mpg)

ggplot(data1, aes(class, cty)) + 
  geom_boxplot(aes(fill=as.factor(cyl)),outlier.color = "red", outlier.size = 2.5, notch =F)
data1$cyl = as.factor(data1$cyl)

T = filter(data1, class == "subcompact" & cyl =="8")
View(T)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#converting the variable to factor

names = c('Attrition','Education','EnvironmentSatisfaction','JobSatisfaction',
          'RelationshipSatisfaction','WorkLifeBalance','JobInvolvement')
hr[,names]= lapply(hr[,names] ,factor)
str(hr)
library(purrr)
library(tidyr)
t= hr[1:25] %>% keep(is.numeric) %>% gather()
View(t)
ggplot(t,aes(value)) + geom_histogram() +
  facet_wrap(~key,scales = "free")
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

                             ##HR ANYLASIS###
#Q1)the average age of the employees in each department excluding the outliers
ggplot(hr,aes(Department,Age))+
  geom_boxplot(outlier.color = "Orange", outlier.size = 2.5 )

#Q2)
att_rate= hr %>%
  filter(TotalWorkingYears<5,MonthlyIncome>=2000, MonthlyIncome<=4000) %>%
  select(EducationField,Attrition) %>%
  group_by(EducationField) %>%
  summarise(att=sum(Attrition==1),
            att_rat=(att/n())*100)
View(att_rate)

#Q3)average salary hike for the employees with work experience between 8 years to 10 years
ave_sala_hike=hr %>%
  filter(TotalWorkingYears>=8, TotalWorkingYears<=10) %>%
  summarise(perchike=mean(PercentSalaryHike),
            avghikes=mean(MonthlyIncome),
            ave_sala_hike=(avghikes*perchike)/100)
ave_sala_hike  

#Q4) Is number of companies worked normally distributed
#for the employees with work experience between 8 years and 10 years>>>>>>>>>>>
skew(hr$NumCompaniesWorked)
Avg_exp= hr %>% filter(NumCompaniesWorked>=8 & NumCompaniesWorked<=10)
View(Avg_exp)
skew(Avg_exp$NumCompaniesWorked, na.rm = T)

#Q5) 5.	Divided the whole data into two groups
#Group1: people with monthly salary more than the average salary of all employees
hr$Group =ifelse(hr$MonthlyIncome>mean(hr$MonthlyIncome),"high","low")
g1=filter(hr, Group =="high")
g2=filter(hr, Group =="low")

g1_table_1=g1 %>% group_by(JobRole) %>% summarise(Tot.emp=n())
g1_table_2=filter(g1,Attrition ==1) %>% group_by(JobRole) %>% summarise(Emp.Left=n())
g1_final = merge(g1_table_1,g1_table_2,by ="JobRole",all.x=T)
g1_final$Ratio = round((g1_final$Emp.Left/g1_final$Tot.emp),2)
g1_final[is.na(g1_final)]=0

g2_table_1=g2 %>% group_by(JobRole) %>% summarise(Tot.emp=n())
g2_table_2=filter(g2,Attrition ==1) %>% group_by(JobRole) %>% summarise(Emp.Left=n())
g2_final = merge(g2_table_1,g2_table_2,by ="JobRole",all.x=T)
g2_final$Ratio = round((g2_final$Emp.Left/g2_final$Tot.emp),2)
g2_final[is.na(g2_final)]=0

g_final=merge(g1_final,g2_final,by="JobRole")

g_final$attratio = g_final$Ratio.y-g_final$Ratio.x
View(g_final)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>.

                              ## BIVARIANT ANALYSIS ##
# N-N Relationship>>>>
View(mpg)
scatter_plot=ggplot(mpg,aes(x=cty,y=hwy)) + geom_point()
scatter_plot #Points are overlapping in this so now we use jitter plot

jitter_plot= ggplot(mpg,aes(x=cty,y=hwy)) + 
  geom_jitter(aes(col=class))
jitter_plot

# iWant to predict highway mileage
T = lm(hwy ~ cty, mpg) #lm = linear model
summary(T) # m or slope = 1.33746, c= 0.89204

hwy = 1.34 *20 + 0.89
T = lm(cty ~ hwy, mpg)
summary(T)

cty = 0.683* 27 + 0.84

jitter_plot= ggplot(mpg,aes(x=cty,y=hwy)) + 
  geom_jitter(aes(col=class, size = displ))
jitter_plot # now this become a bubble chart (Greater the bubble larger the displacement )

T = lm(hwy ~ cty+displ, mpg)
summary(T)
Y = aX1+bX2 +c
Y = 1.33*X1 - 0.03 *X2 + 1.15

#Corelation Matrix >>>>>
View(mtcars)
str(mtcars)

#let select only the numerical coloumn
new = mtcars %>% select("mpg","disp","hp","drat","wt","qsec")
View(new)
matrix = cor(new)
View(matrix)

install.packages("corrplot")
library(corrplot)
corrplot(matrix,method = "circle", type = "upper", order = "hclust")

View(hr)
summary(hr)
New= hr %>%
  select('Age','DistanceFromHome','HourlyRate','MonthlyIncome','MonthlyRate','PercentSalaryHike','TotalWorkingYears','TrainingTimesLastYear','YearsAtCompany','YearsInCurrentRole','YearsSinceLastPromotion','YearsWithCurrManager')
View(New)
matrixes= cor(New)
corrplot(matrixes,method = "pie", type = "lower", order = "hclust")
#plot.new()  and  dev.off() (this will remove all early plot)

#Numerical vs Numerical#
#>>>> scatter / jitter

                        ### Numerical vs Categorical ###
#hor/vert/lollipop
#ANOVA
#one way
#Two way
#(without/with replication)
dataset=chickwts
View(dataset)
describe.by(dataset$weight,group = dataset$feed, mat=T,digit =2)

model =aov(weight~feed,dataset)
summary(model)
TukeyHSD(model,conf.level= .95)


tyr= read.csv('tyre.csv')
View(tyr)

describe.by(tyr$Mileage,group = tyr$Brands, mat=T,digit =2)

modelss =aov(Mileage~Brands,tyr)
summary(modelss)
c=TukeyHSD(modelss,conf.level= .95)
plot(c, col="Red",las =1)


mond=aov(MonthlyIncome~JobRole,hr)
summary(mond)
TukeyHSD(mond,conf.level= .95,ordered = T)
plot(f, col="Red",las =1)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>.

                                      #TWO WAY ANOVA#
#Convert the same to long
student =read_excel("Students Marks.xlsx")
long = gather(student,Assesment_type,Total_marks,FA:IA) 
View(long)

#converting into factor
student$Student=as.factor(student$Student)
long$Assesment_type=as.factor(long$Assesment_type)

#null hypothesis
#1. nosignificant difference in mean (Totalmarks) of factor A
#2. nosignificant difference in mean (Totalmarks) of factor B(IA and FA)

#two way anova without replicaltion
model= aov(data = long,Total_marks~Student+Assesment_type)
summary(model) # null hypothesis accepted

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

local_news =read_excel("Advertising in Local Newspapers.xls")
adver_news = gather(local_news,news_coloum_type,Total_add,News:Sports) 
local_news$Day=as.factor(local_news$Day)
adver_news$news_coloum_type=as.factor(adver_news$news_coloum_type)
mod=aov(data = adver_news,Total_add~Day+news_coloum_type)
summary(mod)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

dataset=read.spss("iqdata.sav",to.data.frame = TRUE)
str(dataset)
model=aov(iq~group,dataset)
summary(model)
TukeyHSD(model,conf.level= .95,ordered = T)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

library(HSAUR2)
data=Forbes2000
a=boxplot(data$sales,data$profits,data$assets,data$marketvalue)
a$stats
IQR(Forbes2000$sales)
UW=9.54+1.5*IQR(data$sales)
quantile(data$sales)
quantile(data$profits,na.rm = T)

data$sales= ifelse(data$sales > UW,UW, data$sales )
boxplot(data$sales)

#cat wise sale

cat_wise_sale = data %>% group_by(category) %>% summarise(Total.Sales=sum(sales)) %>% arrange(-Total.Sales) %>% head(10)
cat_wise_sale
char1=ggplot(cat_wise_sale, aes(Category, Total.Sales )+
                geom_point(size =5)+
                theme(axis.text.x =element_text(angle = 90)))
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>....
data1=economics
View(economics)
glimpse(economics)

chart=ggplot(data1,aes(date,unemploy)) + geom_line()
chart
chart1=ggplot(data1,aes(date)) + geom_line(aes(y= unemploy,col="unemployment")) +
  geom_line(aes(y=pop,col="population"))
chart1
#My chart is showing date over a period of 10 years. iwant to show for each year

install.packages("lubridate")

brks = data1$date[seq(7,length(data1$date)-4, 12)] #no. of data point
lbls = lubridate::year(brks)

chart+ scale_x_date(labels =  lbls, breaks = brks) +theme(axis.text.x = element_text()) +
  geom_area(position="fill") +theme(axis.text.x =element_text(angle = 90))

#calculating z score ( odi$zscore= scale(odi$Run) ) then x=Match, y= zscore
install.packages("VIM")
install.packages("mice")
data_air=airquality
View(data_air)
summary(data_air)
md.pattern(data_air) # missing values observation

missing_plot =aggr(data_air, col(c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                                 labels(names(data_air),
                                 cex.axis=.7,gap=3,ylab=c("History of missing values","Pattern"))))

#imputing missing value using MICE
md.pattern(data_air)
data_imputed= mice(data_air, m=5, method = "pmm")
data_new=complete(data_imputed,4)
View(data_new)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
parliament= read.csv("parliament.csv")
View(parliament)
md.pattern(parliament)
missing_plot =aggr(parliament, col(c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                                 labels(names(parliament),
                                 cex.axis=.7,gap=3,ylab=c("History of missing values","Pattern"))))

parliament= mice(parliament, m=5, method = "pmm")
parliament_new=complete(parliament,4)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
library(arules)
library(arulesViz)
data(Groceries)
inspect(head(Groceries,3))
T=as(Groceries,"matrix")
T=as.data.frame(T)
tData= as(T,"transactions")
View(T[1:10,1:10])
size(head(Groceries))
itemFrequencyPlot(Groceries,topN=10,type="absolute",main="Item Frequency")
rules=apriori(Groceries,parameter = list(supp=0.001,conf=0.8))
options(digits = 2)
inspect(rules[1:5])
summary(rules)
