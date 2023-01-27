#to make vectores we use the fuction c (concatenate)

a <- c(1,4,3,3,2,5)
a
table(a) #to show number of components there are in vectors

#another way for table is 
b=c(china=3,usa=2,Netherlands=1)
b
names(b)

#sorting

index <- order(a)
index
a[index]

b[2]
install.packages("dslabs")
library(dslabs)
murders$total

sort(murders$total)


#important now we lets order cities depending on total of murder 
index <- order(murders$total)
index #we got this index and because we have the same order in the dataset we get to know the order by applying the index on the state column
murders$state[index]

i_max <-which.max(murders$total)
i_min <- which.min(murders$total)

i_max
i_min

print('the city with the max murder rate is:')
murders$abb[i_max]
print('the city with the min murder rate is:')
murders$abb[i_min]


murders$population[which.max(murders$population)] #all the thing between brackets is to get the number of the row index
murders$abb[which.max(murders$population)]

#arithmetic operations

#murder rate
murder_rate <- murders$total*100000/(murders$population)
murders$state[order(murder_rate,decreasing = 'True')]

#making data_frames
temp <- c(35, 88, 42, 84, 81, 30)
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")
city_temps <- data.frame(city = city, temperature = temp,Murder_Rate=murders$total[1:6])
city_temps


#excercise1: Mandi, Amy, Nicole, and Olivia all ran different distances in different time intervals. Their distances (in
#miles) and times (in minutes) are as follows:

name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)

#Write a line of code to convert time to hours. Remember there are 60 minutes in an hour. Then write a line of code to calculate the speed of each runner in miles per hour. Speed is distance divided by time.
time_hour <- time/60
speed= distance/time_hour
time_hour
speed

help('dplyr')

#indexing

index <- murders$rate <= 1
murders$state[index]
murders$region[murders$rate <= 1]


#logical operations end up with index
murders$state[index]

#(And & ) and  (OR | )
west <- murders$region == 'west'
safe <- murders$rate <= 1

index <- (murders$rate <= 1) & (murders$region == 'west')     
index
murders$state[index]

#which-which(!)=which not: shows where do we have True (not 1) and brings index

x <- c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE)
x <- c(0,1,0,1,1,0)
which(x)

# find the index of Massachusetts


index <- which(murders$state=='Massachusetts')
index
#cevap=22
murder_rate[index]

index <- which(!murders$rate>= 0.71)
index
murders$rate[index]

#make dataframe for the ones <=0.71 with name of city
index <- which(!murders$rate>= 0.71)
average <- murders$rate[(index)]
city <- murders$state[index]

less_0.71_frame <-data.frame(city=city,average=average)
less_0.71_frame

#Match:"match" looks for entries in a vector and returns
#the index needed to access them.

index_m <- match(c("New York", "Florida", "Sana'a"),murders$state)
index_m
#this will give us the index in the murders$state that matches the name of the states
murders$population[index_m]
murders$population[match(c("New York", "Florida", "Washington"),murders$state)]

# %in%
#The last function we're going to look at is perhaps the most useful.
#And it's the %in% operator.
#If rather than an index, we want to know whether or not each element of a first
#vector is in a second vector, we use the function %in%.

#example
c("Boston", "Dakota", "Washington") %in% murders$state

#تصحيح الريت
murder_rate <- murders$total / murders$population * 100000
murder_rate
murders$state[murder_rate <= 1] 

# Store the murder rate per 100,000 for each state, in `murder_rate`
murder_rate <- murders$total/murders$population*100000

# Store the `murder_rate < 1` in `low` 
low <- murder_rate < 1
low

# Create a vector ind for states in the Northeast and with murder rates lower than 1. 

which(low & murders$region == "Northeast")
#مهم لاداعي لعمل 

# Names of states in `ind` 
murders$state[low & murders$region == "Northeast"]
murders$state(max)
murders$state[order(murder_rate)]     

head(murders)

#countries under average calculation

# Store the murder rate per 100,000 for each state, in murder_rate
murder_rate <- murders$total/murders$population*100000


# Compute the average murder rate using `mean` and store it in object named `avg`
avg <- mean(murder_rate)
les_av <- which(murders$total < avg)
les_av

# How many states have murder rates below avg ? Check using sum 
murders$state[les_av]

#example

# Store the 5 abbreviations in `abbs`. (remember that they are character vectors)
abbs <- c("MA", "ME", "MI", "MO", "MU") 

# Use the `which` command and `!` operator to find out which index abbreviations are not actually part of the dataset and store in 'ind' 
ind <- which(!abbs%in%murders$abb) #الاندكس الخارج للأول
ind
# Names of abbreviations in `ind` 
abbs[ind]

#Data wrangling
#adding or exchanging columns _ we use mutate()
mutate(dataframe,new column)

#to filter data with sub-setting rows we use 'filter()'
filter(dataframe,condition)

#to subset the data 
#by selecting specific columns we use 'select()'
select()

#more than one operation with pipe operator.sending the result of one function into another function
%>%

#add the murder_rate into our dataframe 
install.packages('dplyr')
library(dplyr)
murders
murders <- mutate(murders,rate=murder_rate)
head(murders)

#filter the data table to only show murder_rate under 0.71.
murder_less_0.71 <- filter(murders,rate <= 0.71)
murder_less_0.71

#select state region and rate from the data.then show murder_rate under 0.71.
new_selected <- select(murders,state,region,rate)
new_selected
filter(new_selected,rate<=0.71)

#with pipe do select and filter togather

murders_pipe <- murders %>% select(state,region,rate) %>% filter(rate<=0.71)

murders_pipe

#creating dataframes
data.frame()

#make a data frame for students grades for exam1 and exam 2 

result <- data.frame(names=c('Abdulrhman','Ahmed','Anis'),
           exam1=c(100,90,85),
           exam2=c(95,80,70),
            stringsAsFactors = 'False')
result

#data.frame by defult turns charecters into factors

#exercises

# Loading data
library(dslabs)
data(murders)

# Loading dplyr
library(dplyr)

murders

# Redefine murders so that it includes a column named rate with the per 100,000 murder rates
mutate(murders,population=population/10^5)
murders

#Note that we can write population rather than murders$population. The function mutate knows we are grabing columns from murders.
library(dplyr)

#exercise
# Defining rate
rate <-  murders$total/ murders$population * 100000
rate
x <- c(100,200,333,444,555,666,777,888,999,1000,1100)
rank(x)
# Redefine murders to include a column named rank
# with the ranks of rate from highest to lowest
rank <- rank(-rate)
murders <- mutate(murders,rank)
murders$rank


#to find number of rows we can use nrow()

#önemli

#Use one line of code to create a new data frame, called my_states, that has murder rate and rank columns (with the rank ordered from highest to lowest), considers only states in the Northeast or West which have a murder rate lower than 1, and contain only the state, rate, and rank columns. The line should have four components separated by three %>% operators:
my_state <- mutate(murders,rate=murders$total/ murders$population * 100000 ,rank=rank(-rate)) %>% filter(region %in% c('Northeast','West'),rate<1) %>% select(state,rate,rank)
my_state
nrow(my_state)
#to reset data we write data(murders)

#ploting data

population_in_millions <- murders$population/10^6
total_gun_murders <- murders$total

plot(population_in_millions,total_gun_murders)
murders$state[which.min(murders$rank)]
boxplot(rate~region,data=murders)

murders
#summarizing data(median - mean - standard deviation) 
#first we make groups(split data) then we summarize (group_by(), summarize())
#arrange() helps us examine data after sorting

#summarize
      
#compute the minimum , median and maximum murder rate for states in western region

#first we split data or make groups using filter or select depends on question
install.packages("dplyr")
library('dplyr')

s <- murders %>% filter(region=="West") %>% summarize(minimum=min(rate),
                                                      median=median(rate),
                                                      maximum=max(rate))
s
#önemli
# average rate not adjusted by population size
mean(murders$rate)

# average rate adjusted by population size
us_murder_rate <- murders %>% 
  summarize(rate = sum(total) / sum(population) * 10^5)
us_murder_rate
s
#we can obtain the min, median, and maximum with just one line
#of code using the quantiles function. 

murders %>% 
  filter(region == "West") %>%
  summarize(range = quantile(rate, c(0, 0.5, 1)))


#functions
#turn row to colunm with data.frame

my_quantile <- function(x){
  r <- quantile(x,c(0,0.5,1))
  data.frame(minimum=r[1],median=r[2],maximim=r[3])
  
}
murders %>% 
  filter(region == "West") %>%
  summarize(my_quantile(rate))

#function als can be like

my_quantile <- function(x){
  quantile(x,c(0,0.5,1))
}
murders %>% 
  filter(region == "West") %>%
  summarize(my_quantile(rate))

#another equation

equation <- function(x){
  x^2-4*x+5
  
}
equation(5)
  

#pull and dot to access columns (from dataframes or vektors to numeric)
us_murder_rate

murders %>%
  select(population)   #output is in dataframe format

murders %>% 
  pull(population)       #the same as select but the output is in vector format

murders %>%
  .$population
#group then summarize 

#A common operation in data exploration
#is the first split data into groups and then compute summaries for each group.
murders %>% group_by(region)

#it will not look different down but it is splitted

#examining datasets with sorting data
#for odering entire table we use arrange()

murders %>%
  arrange(population)%>%
  head()

murders %>%
  arrange(desc(population))%>%
  head()

#Similarly, a third column can be used to break
#ties between first and second columns, and so on.

murders %>%
  arrange(region,population,rank)%>%
  head()

murders %>% top_n(5,total)

#data tabel

# install the data.table package before you use it!
install.packages("data.table")

# load data.table package
library(data.table)

# load other packages and datasets
install.packages("tidyverse")
library(data.table)

library(tidyverse)
library(dplyr)
library(dslabs)
data(murders)

murders

# convert the data frame into a data.table object
murders <- setDT(murders)

murders

# selecting in dplyr
select(murders, state, region)
murders <- setDT(murders)

# selecting in data.table - 2 methods
murders[, c("state", "region")] |> head()
murders[, .(state, region)] |> head()

# adding or changing a column in dplyr
murders <- mutate(murders, rate = total / population * 10^5)

# adding or changing a column in data.table
murders[, rate := total / population * 100000]
head(murders)
murders[, ":="(rate = total / population * 100000, rank = rank(population))]
head(murders)
head(murders)

# y is referring to x and := changes by reference
boodschapen <- data.table(veg = c("tomaat","aardappels","uinen"),Fruits=c("banaan","Mango","appels"),others=c("rice","chicken","mlek"))
boodschapen

x=data.table(a=1)
y <- x
y
x[,a := 2]    #mutation
y

y[,a := 1]
x

# use copy to make an actual copy
x <- data.table(a = 1)
y <- copy(x)
x[,a := 2]
y

# subsetting in dplyr
filter(murders, rate <= 0.7)

# subsetting in data.table
murders[rate <= 0.7]

# combining filter and select in data.table
murders[rate <= 0.7, .(state, rate)]
murders[rate <=0.7,c("state","rate")]

# combining filter and select in dplyr
murders %>% filter(rate <= 0.7) %>% select(state, rate)

# summarizing in dplyr
s <- heights %>% 
  summarize(average = mean(height), standard_deviation = sd(height))
s

library(data.table)

library(tidyverse)
library(dplyr)
library(dslabs)
data("heights")

# summarizing in data.table
heights <- setDT(heights) #first change the format of data from dataframe into datatable

s <- heights[,.(average = mean(height), standard_deviation = sd(height))]
s

# subsetting and then summarizing in dplyr
s <- heights %>% 
  filter(sex == "Female") %>%
  summarize(average = mean(height), standard_deviation = sd(height))

# subsetting and then summarizing in data.table
heights <- setDT(heights) #first change the format of data from dataframe into datatable
s <- heights[sex == "Female",.(average = mean(height), standard_deviation = sd(height))]
s
# previously defined function
median_min_max <- function(x){
  r <- quantile(x, c(0, 0.5, 1))
  data.frame(minimum = r[1], median = r[2], maximum = r[3])
}

# multiple summaries in data.table
heights[, .(median_min_max(height))]

# grouping then summarizing in data.table
heights[, .(average = mean(height), standard_deviation = sd(height)), by = sex]

# order by population
setDT(murders)
murders[order(population)] |> head()

# order by population in descending order
murders[order(population, decreasing = TRUE)] 
murders[order(-population)] 

# order by region and then population
murders[order(region, population, decreasing = TRUE)]


# Önemli First, determine the average height in this dataset. Then create a logical vector ind with the indices for those individuals who are above average height.
library(dslabs)
data(heights)
options(digits = 3)

average <- heights %>%
  select(height) %>%
  summarize(average=mean(height))

average

ind <- heights %>%
  filter(height > 68.3)
ind
nrow(ind)

ind
ind_2 <-filter(ind,sex=="Female")
nrow(ind_2)

ind_3 <-filter(ind,sex=="Male")
nrow(ind_3)

str(heights)
fem_por <- filter(heights,sex=="Female")
nrow(fem_por)
mean <- heights %>%
  filter(sex=="Female")%>%
  summarize(mean=mean())
#What proportion of individuals in the data frame are female?

index <- which(heights$sex=='Female')
index
mean(index)

min=min(heights$height)
min
which.min(heights$height)
data("heights")
heights[which.min(heights$height)]
heights[1032]               #here there is a mistake because you cannot bring one hole row from dataframe you have to specify which column as below
heights$sex[1032]

setDT(heights)
heights[1032]               #here there is no mistake because you can bring one hole row from datatable.

match(heights[1032],heights[1])
heights
x <- 50:82
!(x %in% heights$height)    #the outcome is True fulse
which(!(x %in% heights$height))   #which(true,false)=numbers
heights[c(7,8,33)]          #data table it is possible 
sum(!(x %in% heights$height))
heights2 <- heights %>%
  mutate(ht_cm=height*2.54)
heights2

mean(heights2$ht_cm)

# find the average height is in cm for ladys
heights2 %>% filter(sex=='Female')%>%
  summarize(average=mean(ht_cm))

mean <- heights2 %>%
  filter(sex=="Female")%>%
  summarize(mean=mean(ht_cm))
mean

#visualization
heights2
data.frame(heights2)
plot(heights2$sex,heights2$ht_cm)
boxplot(heights2$ht_cm~heights2$sex,data=heights2) #y~x numeric~category

library(dslabs)
data(olive)
head(olive)
plot(olive$palmitic , olive$palmitoleic )
hist(olive$eicosenoic)
boxplot(palmitoleic~region ,data=olive)

#if conditional
library(dslabs)
data(murders)
murder_rate <- murders$total / murders$population*100000
murder_rate
ind <- which.min(murder_rate)
ind
if(murder_rate[ind]<=0.5){
  print(murders$state[ind])
}else{
  print('there is no state with this low murder rate')
}
#another one If the height in cm is less or equal to 150cm report us with the gender
data.frame(heights2)         #if can not be done on tables only with frames
ind <- which.min(heights2$ht_cm)
ind

if(heights2$ht_cm[ind]<=150){
  print(heights2$sex[ind])
}else{
  print('there is no state with this low height')
}



plot(murders$state)
