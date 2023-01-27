#unique and table function
#Use the table function to compute the frequencies of each unique height value. Because we are using the resulting frequency table in a later exercise we want you to save the results into an object and call it tab.
library(dslabs)
data(heights)
x <- heights$height
x
table(x)
x
plot(table(x))
y<- table(heights$sex)
y
plot(y)
hist(y)
plot(heights$sex)

#In the previous exercise we    computed the variable tab which reports the number of times each unique value appears. For values reported only once tab will be 1. Use logicals and the function sum to count the number of times this happens.
library(dslabs)
data(heights)
tab <- table(heights$height)
sum(table(heights$height)==1)

#Describe Heights to ET
#CDF can be calculated and plotted like this

#sapply() function in R Language takes list, vector or data frame as input and gives output in vector or matrix

a <- seq(min(heights$height), max(heights$height), length = 100)    # define range of values spanning the dataset
a
cdf_function <- function(z) {    # computes prob. for a single value
  mean(heights$height <= z)
}
cdf_values <- sapply(a, cdf_function)
cdf_values
plot(a, cdf_values)

#sapply on murders total

b <- seq(min(murders$total), max(murders$total), length = 50)    # define range of values spanning the dataset
b
cdf_function2 <- function(z) {    # computes prob. for a single value
  mean(murders$total <= z)
}
cdf_values2 <- sapply(b, cdf_function2)
cdf_values2

plot(b, cdf_values2)

#CDF2

a <- seq(min(murders$rate), max(murders$rate), length = 100)    # define range of values spanning the dataset
cdf_function <- function(x) {    # computes prob. for a single value
  mean(murders$rate <= x)
}
cdf_values <- sapply(a, cdf_function)
cdf_values
a
plot(a, cdf_values)



#Smooth density plots can be thought of as histograms where the bin width is extremely or infinitely small. The smoothing function makes estimates of the true continuous trend of the data given the available sample of data points.
#The degree of smoothness can be controlled by an argument in the plotting function. (We will learn functions for plotting later.)
#While the histogram is an assumption-free summary, the smooth density plot is shaped by assumptions and choices you make as a data analyst.
#The y-axis is scaled so that the area under the density curve sums to 1. This means that interpreting values on the y-axis is not straightforward. To determine the proportion of data in between two values, compute the area under the smooth density curve in the region between those values.
#An advantage of smooth densities over histograms is that densities are easier to compare visually.

#normal dist depends on average and standard deviation and look at 100 screenshot
# define x as vector of male heights (2/-2)
library(tidyverse)
library(dslabs)
data(heights)
index <- which(heights$sex=="Male")   #the outcome is True or Fulse that can be 
x <- heights$height[index]
x <- heights$height[heights$sex=="Male"] #[] accepts inside either numbers or true false
x

# calculate the mean and standard deviation manually
average1 <- sum(x)/length(x)
SD1 <- sqrt(sum((x - average)^2)/length(x))
average1
SD1


# built-in mean and sd functions - note that the audio and printed values disagree
average <- mean(x)
SD <- sd(x)
c(average = average, SD = SD)
average

# calculate standard units (The standard unit of a value tells us how many standard deviations away
#from the average this value is.) z=(x-average)/SD
z <- scale(x)
z

# calculate proportion of values within 2 SD of mean
mean(abs(z) >=2)
#Using pnorm to calculate probabilities
#find the cumulative distribution on normal dis 1-pnorm(a,mean(),sd())
#what is the probability to find a Male student with 70.5 inches height
index <- heights$sex == "Male"
x <- heights$height[index]
CD_of_SD <- 1-pnorm(70.5,mean(x), sd(x))
CD_of_SD

#The normal distribution has a mathematically defined CDF which can be computed in R with the function pnorm().
#pnorm(a, avg, s) gives the value of the cumulative distribution function  for the normal distribution defined by average avg and standard deviation s.
#We say that a random quantity is normally distributed with average avg and standard deviation s if the approximation pnorm(a, avg, s) holds for all values of a.
#If we are willing to use the normal approximation for height, we can estimate the distribution simply from the mean and standard deviation of our values.
#If we treat the height data as discrete rather than categorical, we see that the data are not very useful because integer values are more common than expected due to rounding. This is called discretization.
#With rounded data, the normal approximation is particularly useful when computing probabilities of intervals of length 1 that include exactly one integer.

#Discretization and the normal approximation

# plot distribution of exact heights in data # plot(prop.table(table(x))
x
table(x)
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")
prop.table(table(x))
?xlab


# probabilities in actual data over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)


# probabilities in normal approximation match well
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

# probabilities in actual data over other ranges don't match normal approx as well because the interval is not 1 
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))

#Histograms and density plots provide excellent summaries of a distribution. But can we summarize even further? We often see the average and standard deviation used as summary statistics: a two number summary! To understand what these summaries are and why they are so widely used, we need to understand the normal distribution.
#The normal distribution, also known as the bell curve and as the Gaussian distribution, is one of the most famous mathematical concepts in history. A reason for this is that approximately normal distributions occur in many situations. Examples include gambling winnings, heights, weights, blood pressure, standardized test scores, and experimental measurement errors. Often data visualization is needed to confirm that our data follows a normal distribution.
#Here we focus on how the normal distribution helps us summarize data and can be useful in practice.
#One way the normal distribution is useful is that it can be used to approximate the distribution of a list of numbers without having access to the entire list. We will demonstrate this with the heights dataset.

#What proportion of the data is between 69 and 72 inches (taller than 69 but shorter or equal to 72)? A proportion is between 0 and 1.
#exact calculation
library(dslabs)
data(heights)
x <- heights$height[heights$sex == "Male"]
mean(x > 69 & x <= 72)


#pnorm(value, mean, sd). Notice that this is the CDF for the normal distribution
#approximation calculated we do not need data in pnorm all we need is avg and sd
#for one value = 1- pnorm
library(dslabs)
data(heights)
x <- heights$height[heights$sex=="Male"]
avg <- mean(x)
stdev <- sd(x)
pnorm(72, avg, stdev) - pnorm(69, avg, stdev)

# approx and exact

library(dslabs)
data(heights)
x <- heights$height[heights$sex == "Male"]
exact <- mean(x > 79 & x <= 80)
exact
avg <-mean(x)
SD <- sd(x)
approx <- pnorm(80,avg,SD)-pnorm(79,avg,SD)
exact
approx
exact/approx


#Definition of quantiles
#Quantiles are cutoff points that divide a dataset into intervals with set probabilities. The qth quantile is the value at which q% of the observations are equal to or less than that value.

#Given a dataset data and desired quantile q, you can find the qth quantile of data with:

quantile(data,q)

#Percentiles are the quantiles that divide a dataset into 100 intervals each with 1% probability. You can determine all percentiles of a dataset data like this:
p <- seq(0.01, 0.99, 0.01)
quantile(data, p)

#Quarties divide a dataset into 4 parts each with 25% probability. They are equal to the 25th, 50th and 75th percentiles. The 25th percentile is also known as the 1st quartile, the 50th percentile is also known as the median, and the 75th percentile is also known as the 3rd quartile.
#The summary() function returns the minimum, quartiles and maximum of a vector.

library(dslabs)
data(heights)

#Use summary() on the heights$height variable to find the quartiles:
summary(heights$height)


#Find the percentiles of heights$height
#observed quantile
p <- seq(0.01, 0.99, 0.01)
percentiles <- quantile(heights$height, p)
percentiles
heights
names(percentiles) == "25%"

percentiles[names(percentiles) == "25%"]
percentiles[names(percentiles) == "75%"]

#example 
#show us if the normal dist is good for murder rate values dataset using qqplot

x<-murders$total
p<- seq(0.01, 0.99, 0.01)
p


#Theoretical quantiles qnorm()

p <- seq(0.01, 0.99, 0.01)
theoretical_quantiles <- qnorm(p, 69, 3) #qnorm(p,mean,sd)
theoretical_quantiles

# find the 60th percentile if mean is defined as 500 and standard deviation 100
qnorm(0.6,500,100)

#The qnorm() function gives the theoretical value of a quantile with probability p of observing a value equal to or less than that quantile value given a normal distribution with mean mu and standard deviation sigma:
#qnorm() and pnorm() are inverse functions:
qnorm(p, mu, sigma)
pnorm(-1.96)
qnorm(0.025)
qnorm(pnorm(0.025))

#By default, mu=0 and sigma=1. Therefore, calling qnorm() with no arguments gives quantiles for the standard normal distribution.


# define x and z
library(tidyverse)
library(dslabs)
data(heights)
index <- heights$sex=="Male"
x <- heights$height[index]
z <- scale(x) #standard unites how many sd are there between the average and the data
z
boxplot(z~x,data=heights)

# proportion of data below 69.5
mean(x <= 69.5)

# calculate observed and theoretical quantiles
p <- seq(0.05, 0.95, 0.05)
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm(p, mean = mean(x), sd = sd(x))
observed_quantiles
theoretical_quantiles
# make QQ-plot
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

# make QQ-plot with scaled values.an easier method
observed_quantiles <- quantile(z, p)
theoretical_quantiles <- qnorm(p)
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)


#example 
#show us if the normal dist is good for murder rate values dataset using qqplot

x<-murders$total
p<- seq(0.01, 0.99, 0.01)
z <- scale(x) #standard unites how many sd are there between the average and the data
observed_quantiles_murder <- quantile(z,p)
theoretical_quantiles_murder <- qnorm(p)
plot(theoretical_quantiles_murder,observed_quantiles_murder)
abline(0,1)
#answer is the norm dis for murder is not good because the points are far from the line. in this case we take the alternative (histogram- smooth density plot) which will be so useful for us
#boxplot is very usefull when we want to calculate more than one distribution


#Create two five row vectors showing the 10th, 30th, 50th, 70th, and 90th percentiles for the heights of each sex called these vectors female_percentiles and male_percentiles.
#Then create a data frame called df with these two vectors as columns. The column names should be female and male and should appear in that order. As an example consider that if you want a data frame to have column names names and grades, in that order, you do it like this:
x<-heights$height[heights$sex == "Female"]
p<- seq(0.01, 0.99, 0.01)
z <- scale(x)
observed_quantiles_heights_Female <- quantile(z,p)
theoretical_quantiles_heights_Female <- qnorm(p)
plot(theoretical_quantiles_heights_Female,observed_quantiles_heights_Female)
abline(0,1)

#median absolute deviation and median is resistant to flaws in datasets 
#mean and sd are not resistant

##ggplot function grammar of graphics. easy and simple but it is exclusively designed to deal with data tables
library(tidyverse)
#other usefull packages grid, lattice

#summarize the Us murders data set
#graph component
#1-US data table summarized (data component)
#2-the plot design is scatter plot (geometry component)(defult=gemo_point,barplot,hist,smooth density,qqplots,boxplots)
#3-axis (x,y), colors ...... (aesthetic mapping component)
#4-##note!!! (x,y) range defined by ranged data on ''log scale'' (scale component)
#5- Labels,title,like the one in economist magazine 

#GG2 construction code

library(tidyverse)
library(dslabs)
data(murders)

ggplot(data = murders) #In this case, a blank slate since no geometry has been defined.
#The only style we see is a gray background. and becuase it was not assigned it was automatically evaluated

murders %>% ggplot() #we can also use pipe

p <- ggplot(data = murders)
class(p)
print(p)   # this is equivalent to simply typing p
p
help('ggplot')

#Layers
#DATA %>% ggplot() + LAYER_1(geometry(geom_point(aes(x=population/10^6,y=total)))) + LAYER_2 + ... + LAYER_N
#Aes(aesthetic mapping) will be one of the functions that you will most use. The function connects data with what we see on the graph.the outcome of this function is often used as the argument of a geometry function.

  murders %>% ggplot() +
    geom_point(aes(x = population/10^6, y = total)) +       #geom_plot = scatter
 
                                                             #geom_label = scatter but instead of point you have label try in the example
#second:lets add labels for each layer geo_label, geom_text

    
  murders %>% ggplot() + 
    geom_point(aes(x = population/10^6, y = total),size=3) +
    geom_text(aes(population/10^6,total,label=abb),nudge_x = 1)
  
#we are repeating pop and total over and over again to avoid that we can make a global aes
#If we define a mapping in ggplot, then all the geometries that are added as layers will default to this mapping.
    murders %>% ggplot(aes(population/10^6,total,label=abb)) + 
      geom_point(size=3) +            #best method
      geom_text(nudge_x = 1.5)

#adjust labels , scales and add some color and a line
    #the scale_x_continuous function.
   # We can use this to edit the behavior of scales.

    murders %>% ggplot(aes(population/10^6,total,label=abb)) + 
      geom_point(size=3) +            
      geom_text(nudge_x=0.075)+
      scale_x_continuous(trans='log10')+
      scale_y_continuous(trans='log10')
    
   
    
    murders %>% ggplot(aes(population/10^6,total,label=abb)) + 
      geom_point(size=3) +            
      geom_text(nudge_x=0.075)+
      scale_x_log10()+        # another way
      scale_y_log10()
    
  #  let us add some labels and titles
    library(dslabs)
    library(dplyr)
    library(tidyverse)
    murders %>% ggplot(aes(population/10^6,total,label=abb)) + 
      geom_point(size=3,col='blue') +            
      geom_text(nudge_x=0.075)+
      scale_x_log10()+        # another way
      scale_y_log10()+
      xlab('population in milions(log scale)')+
      ylab('total murders(log scale)')+
      ggtitle("US gun murders 2010")
    
    #making colors for each region(when we add color the legend comes automatically) because there are categories we need to make a mapping
    murders %>% ggplot(aes(population/10^6,total,label=abb)) + 
      geom_point(aes(col=region),size=3) +            
      geom_text(nudge_x=0.075)+
      scale_x_log10()+        # best way for color
      scale_y_log10()+
      xlab('population in milions(log scale)')+
      ylab('total murders(log scale)')+
      ggtitle("US gun murders 2010")
    
    murders %>% ggplot(aes(population/10^6,total,label=abb,col=region)) + 
      geom_point(size=3) +            
      geom_text(nudge_x=0.075)+
      scale_x_log10()+        # another way for color but the colors are bad
      scale_y_log10()+
      xlab('population in milions(log scale)')+
      ylab('total murders(log scale)')+
      ggtitle("US gun murders 2010")
    r
  #average Murder rate for the entire country (line) geom_plot(intercept_a,slope_b) default line has slope 1 and intercept 0 so we have to define the intercept
    r <- murders %>% 
      summarize(rate= sum(total)/sum(population)*10^6) %>%
      .$rate
    r
    
    murders %>% ggplot(aes(population/10^6,total,label=abb)) + 
      geom_point(aes(col=region),size=3) +            
      geom_text(nudge_x=0.075)+
      scale_x_log10()+        # best way for color
      scale_y_log10()+
      xlab('population in milions(log scale)')+
      ylab('total murders(log scale)')+
      ggtitle("US gun murders 2010")+
      geom_abline(intercept = log10(r),slope = 1,lty=2,color='darkgrey')
    
  #capitalize the first letter of the legend
    
    murders %>% ggplot(aes(population/10^6,total,label=abb)) + 
      geom_point(aes(col=region),size=3) +            
      geom_text(nudge_x=0.075)+
      scale_x_log10()+        # best way for color
      scale_y_log10()+
      xlab('population in milions(log scale)')+
      ylab('total murders(log scale)')+
      ggtitle("US gun murders 2010")+
      geom_abline(intercept = log10(r),slope = 1,lty=2,color='darkgrey')+
      scale_color_discrete(name="Region")
      
  #final Touch using add on packages ggthemes, ggrepel

  #1-themes
    #ds_themes_set is a theme in dslabs package other themes can be added using ggthemes like(theme_economist)
    install.packages('ggthemes')
    library(ggthemes)
    library(dplyr)
    library(dslabs)
    library(tidyverse)
    murders %>% ggplot(aes(population/10^6,total,label=abb)) + 
      geom_point(aes(col=region),size=3) +            
      geom_text(nudge_x=0.075)+
      scale_x_log10()+        # best way for color
      scale_y_log10()+
      xlab('population in milions(log scale)')+
      ylab('total murders(log scale)')+
      ggtitle("US gun murders 2010")+
      geom_abline(intercept = log10(r),slope = 1,lty=2,color='red')+
      scale_color_discrete(name="Region")+
      theme_hc()
      # theme_stata()#white background with blacklines inside and outside is like economist
     
       # theme_tufte() #minimal theme
      # theme_economist()
     #or theme_fivethirtyeight()
    #add on package ggrepel adds labels and ensures that they dont fall on top of each other.So all we need to do is change the geom_text layer with a geom_text_repel layer after loading the ggrepel package
    install.packages( 'ggrepel') 
    library(ggrepel)
    
  
    murders %>% ggplot(aes(population/10^6,total,label=abb)) + 
      geom_point(aes(col=region),size=3) +            
      geom_text_repel()+
      scale_x_log10()+        # best way for color
      scale_y_log10()+
      xlab('population in milions(log scale)')+
      ylab('total murders(log scale)')+
      ggtitle("US gun murders 2010")+
      geom_abline(intercept = log10(r),slope = 1,lty=2,color='red')+
      scale_color_discrete(name="Region")+
      theme_economist()
     # theme_fivethirtyeight()
  
    #examples
    #hist
    heights$height[heights$sex == "Male" ] # this filter technique is only used for one column
    heights %>% filter(sex=="Male") %>% #this is used to filter a dataset with many columns
      ggplot(aes(x=height))+
      geom_histogram(binwidth =1,fill='blue',col='black' )+
      xlab("male height in inches")+
      ggtitle('Histgram for male heights in inches')+
      theme_economist()
    #smooth density
    
    heights %>% filter(sex=="Male") %>%
      ggplot(aes(x=height))+
      geom_density(fill='blue' )+
      xlab("male height in inches")+
      ggtitle('Smooth debsity plot for male heights in inches')+
      theme_economist()
    
    # geom_points
    #we cant use for heights dataset because it does not have a y numerical column
    
    
    #for qq plots we use geom_qq() instead of x in aes(x,y) we have aes(sample=) qq plot is a plot for normal dist with average=0 and sd =1
    
    heights %>% filter(sex=="Male") %>%
      ggplot(aes(sample=height))+
      geom_qq()+
      xlab("male height in inches")+
      ggtitle('Histgram for male heights in inches')
     # theme_economist()
    
    
    #we change parameters(mean,sd) of qq using summarize
    params<- heights %>% filter(sex=="Male") %>%
      summarize(mean=mean(height),sd=sd(height))
      
    heights %>% filter(sex=="Male") %>%
      ggplot(aes(sample=height))+
      geom_qq(dparams=params)+                #harder
      xlab("male height in inches")+
      ggtitle('Histgram for male heights in inches')+
      theme_economist()+
      geom_abline()
    
    
    heights %>% filter(sex=="Male") %>%
      ggplot(aes(sample=scale(height)))+
      geom_qq()+                            #easier
      xlab("male height in inches")+
      ggtitle('Histgram for male heights in inches')+
      theme_economist()+
      geom_abline()
    
    #lets grid the data
    install.packages("gridExtra")
    library('gridExtra')
    
    p <- heights %>% filter(sex=="Male") %>%
      ggplot(aes(x=height))
    p1 <- p+geom_histogram(binwidth = 1,fill='blue',col='black')
    p2 <- p+geom_histogram(binwidth = 2,fill='blue',col='black')
    p3 <- p+geom_histogram(binwidth = 3,fill='blue',col='black')
    p4 <- p+geom_histogram(binwidth = 4,fill='blue',col='black')
    grid.arrange(p1,p2,p3,p4,ncol=2)
    
    #make a histogram for male and female heights
    
    ## add the group argument then a layer with +
    p1 <- heights %>% filter(sex=="Female") %>%
      ggplot(aes(height))
    p2 <- heights %>% filter(sex=="Male") %>%   # more than one plot in grid
      ggplot(aes(height))
    g1 <- p1+geom_histogram(binwidth = 1,fill='blue',col='black')+ggtitle("Female heights")
    g2 <- p2+geom_histogram(binwidth = 1,fill='blue',col='black')+ggtitle("Male heights")
    grid.arrange(g1,g2,ncol=2)
  
    
   #very important more than one plot in one page
    heights %>% 
      ggplot(aes(height, group = sex)) + 
      geom_density()+                 #NOT DISTinguished with color
      theme_economist()
      
    #very important more than one plot in one page distinguished with color
    heights %>% 
      ggplot(aes(height, color = sex)) +      #a better graph
      geom_density()+
      theme_economist()
    
    #very important more than one plot in one page distinguished with color fill
    heights %>% 
      ggplot(aes(height, fill = sex)) + 
      geom_density(alpha=0.2) +
      theme_economist()
    
    
    install.packages("ggrepels")
    library(ggrepel)
    
    
    
#Trends in World Health and Economics.
#is it fair to say the world that the world is divided into rich western side and poor eastern and latin side
#has income inequality worsened during the last 40 years
library(dslabs)
library(tidyverse)
library(dplyr)                  # zkr11
library(gridExtra)
    
data(gapminder)
head(gapminder)

#review in infant_mortality across different countries in 2015 so we need to filter year and country names then selcet the infant_mortality columns to work on. for or in logics we use %in% 
data_1 <- gapminder %>%
  filter(year == 2015 & country %in% c("Sri Lanka","Turkey") ) %>%
  select(country,infant_mortality)
data_1
data_2 <- gapminder %>%
  filter(year == 2015 & country %in% c("Poland","South Korea") ) %>%
  select(country,infant_mortality)
data_2

data_3 <- gapminder %>%
  filter(year == 2015 & country %in% c("Malaysia","Russia") ) %>%
  select(country,infant_mortality)
data_3

data_4 <- gapminder %>%
  filter(year == 2015 & country %in% c("Pakistan","Vietnam") ) %>%
  select(country,infant_mortality)
data_4

data_5 <- gapminder %>%
  filter(year == 2015 & country %in% c("Thailand","South Africa") ) %>%
  select(country,infant_mortality)
data_5
p_1 <- data_1 %>%ggplot(aes(country,infant_mortality)) +      #x= and y= are optional
  geom_point(aes(col=country))+
  theme_economist()
p_1

p_2 <- data_2 %>%ggplot(aes(x=country,y=infant_mortality)) +
  geom_point(aes(col=country))+
  theme_economist()
p_3 <- data_3 %>%ggplot(aes(x=country,y=infant_mortality)) +
  geom_point(aes(col=country))+
  theme_economist()
p_4 <- data_4 %>%ggplot(aes(x=country,y=infant_mortality)) +
  geom_point(aes(col=country))+
  theme_economist()
p_5 <- data_5 %>%ggplot(aes(x=country,y=infant_mortality)) +
  geom_point(aes(col=country))+
  theme_economist()

grid.arrange(p_1,p_2,p_3,p_4,p_5,ncol=3)

#Is it true that the nations of the west has smaller families with longer age and families in east have shorter age with larger families
#we will proof if this is true or wrong using life expectancy and fertilty dataset 

head(gapminder)

ds_theme_set()    # set plot theme
gapminder %>% filter( year == 2015) %>%
    ggplot(aes(fertility, life_expectancy,col=continent)) +
    geom_point()    

#faceting: side by side plots using function facet_grid(row~column) which automatically separates the plots
#facet function has two variables one for columns and the other for rows

# facet by continent and year
gapminder %>% filter( year %in% c(1962, 2015)) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_grid(continent~year)

gapminder %>%filter( year %in% c(1962,1972,1985,1999,2005, 2015)) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_grid(.~year)       #not very beautiful


gapminder %>% filter( year %in% c(1962,1972,1985,1999,2005, 2015)& continent %in% c('Asia','Europe')) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +                     #when we have so much columns or rows better 
  facet_wrap(year~.)                 #facet_wrap is the best because it automatically makes a fixed axises

#time_seriers_plot(time,measurement of interest)
#which countries improve more, which countries improve less, was improvement constant or there was any acceleration to asnwer all these questions time series plot is helpful

# scatterplot of US fertility by year
library(dslabs)
library(tidyverse)
library(dplyr)                  # zkr11
library(gridExtra)


gapminder %>%
  filter(country == "United States") %>%
  ggplot(aes(year, fertility)) +
  geom_point()


# line plot of US fertility by year
gapminder %>%
  filter(country == "United States") %>%   #one country
  ggplot(aes(year, fertility)) +
  geom_line()


# line plot fertility time series for two countries- only one line (incorrect)
countries <- c("South Korea", "Germany")
gapminder %>% filter(country %in% countries) %>%    #
  ggplot(aes(year, fertility,col=country)) +
  geom_line()




# life expectancy time series - lines colored by country and labeled, no legend
countries <- c("South Korea", "Germany")
labels <- data.frame(country = countries, x = c(1975, 1965), y = c(60, 72))
labels
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, life_expectancy, col = country)) +
  geom_line() +
  geom_text(data = labels, aes(x, y, label = country), size = 5) +
  theme(legend.position = "none")


#transformations
#wealth distribution across the world has become worse poor become poorer and wealthy has become wealthier

# add dollars per day variable
gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365)
gapminder
# histogram of dollars per day
library(ggthemes)
gapminder %>%
  filter(year == 1970 & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, fill='blue',col = "red")+
  theme_economist()

#the previous example shows multiplicative change to change it into additive we use log transformations

gapminder %>%
  filter(year == 1970 & !is.na(gdp)) %>%
  ggplot(aes(log2(dollars_per_day))) +
  geom_histogram(binwidth = 1, fill='blue',col = "red")+  
  theme_economist()
#mode:the value of highest frequency or average for normal dist. tepeler
#we can log the values before ploting or use log the scales before plotting them.

#scale_x_continuous() # scale transformed to log

gapminder %>%
  filter(year == 1970 & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, fill='blue',col = "red")+   #daha advantage direct dollars
  scale_x_continuous(trans='log2')+                  #big numbers like population we use log10
  theme_economist()

#when we have so many regions like 22 the histogram or smoothplots are not usefull so we tend to use boxplot

gapminder %>%
  filter(year == 1970 & !is.na(gdp)) %>%
  ggplot(aes(region,dollars_per_day,fill=continent)) +
  geom_boxplot()+   
  #scale_x_continuous(trans='log2')+                  
  theme_economist()+ 
  theme(axis.text.x=element_text(angle=90,hjust=1) )    #when we cannot read elements names we can rotate them

#we find that the elements are not orderd well to do so let's do it  reorder()
#level() shows the order alphabet
#reorder(region(char),dollars(number),Fun=median)
gapminder
p <- gapminder %>%
  filter(year == 1970 & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%    # reorder
  ggplot(aes(region, dollars_per_day, fill = continent)) +    # color by continent
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
p+ scale_y_continuous(trans = "log2")

#geom_bar is hist with morethan one parameter    

library(dplyr)
library(ggplot2)
library(dslabs)
data(us_contagious_diseases)
us_contagious_diseases
dat <- us_contagious_diseases %>% filter(year == 1967 & disease=="Measles" & count>0 & !is.na(population)) %>%
  mutate(rate = count / population * 10000 * 52 / weeks_reporting) %>%
  mutate(state=reorder(state,rate,FUN=median))
dat %>% ggplot(aes(state, rate)) +
  geom_bar(stat="identity")+
 # theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_flip()                      #flip names and graph in graphs good technique
  
  dat %>% ggplot(aes(state, rate)) +
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  # coord_flip()                      #flip names and graph in graphs good technique
dat
#comparing distribution
# define Western countries
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
#mute a column to be in groups 
gapminder %>%
  filter(year %in% c(1970,2010) & !is.na(gdp)) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day,fill=continent)) +
  geom_histogram(binwidth = 1 ) +
  scale_x_continuous(trans = "log2") +
  facet_grid(year~ group)

#we find that in 2010 more countries than in 1970 due to sovieyt union and data unavailable
date_seventies <-gapminder %>% 
  filter(year==1970 & !is.na(gdp)) %>% .$country #.$ selects one column and shows it as a vector or list
date_seventies
  
date_new <-gapminder %>% 
  filter(year==2010 & !is.na(gdp)) %>% .$country #.$ selects one column and shows it as a vector or list
data_new

country_list <- intersect(date_seventies,date_new)   #intersect show common elements in two lists 
country_list

  
gapminder %>%
  filter(year %in% c(1970, 2010) & country %in% country_list) %>%    # keep only selected countries
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day,fill=continent)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ group)

#lets do that in box plot

p <- gapminder %>%
  filter(year %in% c(1970, 2010) & country %in% country_list) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +   #makes text of x scale horizontal to be readable
  xlab("") + 
  scale_y_continuous(trans = "log2")

p + geom_boxplot(aes(region, dollars_per_day, fill = continent)) +
  facet_grid(year ~.)

#ease comparision 
#it is easier to compare plots when they are togather by puting them next to each other

# arrange matching boxplots next to each other, colored by year
p <- gapminder %>%
  filter(year %in% c(1970, 2010) & country %in% country_list) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot(aes(region, dollars_per_day, fill = factor(year))) +   #the grouping must be either charecter or factor(numerical)
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + scale_y_continuous(trans = "log2")

p + geom_boxplot()

#desity plots

#smooth density plots - area under each curve adds to 1
gapminder %>%
  filter(year == 1970 & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>% group_by(group) %>%
  summarize(n = n()) %>% knitr::kable()



# smooth density plots - variable counts on y-axis because by default even though devloped are much more the density plot acts as if they have the same so we add y= ..count..



p <- gapminder %>%
  filter(year %in% c(1970,2010) & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day, y = ..count.., fill = group)) 
  scale_x_continuous(trans = "log2")
p + geom_density(alpha = 0.5, bw = 0.75) + facet_grid(year ~ .)+theme_classic()

#case_when:show key regions separately
# add group as a factor, grouping regions
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "West",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    TRUE ~ "Others"))
gapminder$group

# reorder factor levels
gapminder <- gapminder %>%
  mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))
gapminder

gapminder %>%
  filter(year %in% c(1970, 2010) & country %in% country_list) %>%
  #group_by(year) %>%
  mutate(weight = population/sum(population*2)) %>%
  ungroup() %>%
  ggplot(aes(dollars_per_day, fill = group, weight = weight)) +
  scale_x_continuous(trans = "log2") +
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") + facet_grid(year ~ .) #stack for stacking plots in top of each other

#ecological fallacy
# define gapminder
library(tidyverse)
library(dslabs)
data(gapminder)

# add additional cases
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "The West",
    .$region == "Northern Africa" ~ "Northern Africa",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region == "Southern Asia" ~ "Southern Asia",
    .$region %in% c('Western Asia',"Central Asia","Southern Europe","Eastern Europe")~"other",
    .$region %in% c("Central America", "South America", "Caribbean") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"))
gapminder

# define a data frame with group average income and average infant survival rate
surv_income <- gapminder %>%
  filter(year %in% c(2010) & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
  group_by(group) %>%                                  #official group_by() and summarize()
  summarize(income = sum(gdp)/sum(population)/365,
            infant_survival_rate = 1 - sum(infant_mortality/1000*population)/sum(population))
surv_income %>% arrange(income)

library(ggthemes)
surv_income %>% ggplot(aes(income,infant_survival_rate))+
  geom_point(aes(col=group))+
  theme_economist()


# plot infant survival versus income, with transformed axes
surv_income %>% ggplot(aes(income, infant_survival_rate, label = group, color = group)) +
  scale_x_continuous(trans = "log2", limit = c(0.25, 150)) +
  scale_y_continuous(trans = "logit", limit = c(0.875, .9981),
                     breaks = c(.85, .90, .95, .99, .995, .998)) +
  geom_text(size = 3, show.legend = FALSE)+
  theme_economist()

#Vietnam war's effect in life expectancy
library(dplyr)
library(dslabs)
data(gapminder)
gapminder %>% 
  filter(year %in% c(1990:2010), country%in% c("Netherlands","United Kingdom"))%>%
  ggplot(aes(year,life_expectancy,color=country))+
  geom_point()

# your code herelibrary(dplyr)
library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)
gapminder_Africa_2010 <- gapminder %>%
  filter(year==2010,country=="Ethiopia",!is.na(gdp))%>%
  mutate(dollars_per_day = gdp/population/365)%>%
  ggplot(aes(dollars_per_day,infant_mortality,color=region)) +
  geom_point()
gapminder_Africa_2010




#data visuallization principles
#unprefered plots----how to improve them----use these as principles. we compare and contrast those that follow the principles to those that do not

#Encoding Data Using Visual Cues: position, aligned lengths, angles, area, brightness,color hue

# dot plot showing the data
heights %>% ggplot(aes(sex, height)) + geom_point()
heights %>% plot(sex,height)

#blended point plot
heights %>% ggplot(aes(sex, height)) + geom_jitter(width = 0.1, alpha = 0.2)
heights

color_blind_friendly_cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

p1 <- data.frame(x = 1:8, y = 1:8, col = as.character(1:8)) %>%
  ggplot(aes(x, y,color=col)) +
  geom_point(size = 5)
p1 + scale_color_manual(values = color_blind_friendly_cols)


#group_by #summarize # plot

library(dplyr)
library(ggplot2)
library(dslabs)
data("murders")
new_murders <- murders %>% mutate(rate = total/population*100000,region = factor(region)) %>%
  group_by(region) %>%
  summarize(avg = mean(rate)) %>%
  ggplot(aes(region, avg)) +
  geom_bar(stat="identity") +
  ylab("Murder Rate Average")
new_murders

#What is the main problem with this interpretaion?It does not show all the data. We do not see the variability within a region and it's possible that the safest states are not in the West.

#To further investigate whether moving to the western region is a wise decision, let's make a box plot of murder rates by region, showing all points.

library(dplyr)
library(ggplot2)
library(dslabs)
data("murders")
new_murders <- murders %>% mutate(rate = total/population*100000) %>% mutate(region=reorder(region,rate,FUN=median)) %>% ggplot(aes(region,rate,color=region))+geom_boxplot()
mew_murders

#slope charts:when you are comparing variables of the same type but at different time points and for a relatively small number of comparison. for comparing two different variables scatter plot(geom_points was good)
#one variable at different time points like income(one variable) in 1970 and 2010(two different time points)

#we use geom_lines to make slope chart

library(tidyverse)
library(dslabs)
data(gapminder)            #connecting lines

west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

dat <- gapminder %>%
  filter(year %in% c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10^7)

dat %>%
  mutate(location = ifelse(year == 2010, 1, 2),
         location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"),
                           location + 0.22, location),
         hjust = ifelse(year == 2010, 1, 0)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(year, life_expectancy, group = country)) +
  geom_line(aes(color = country), show.legend = FALSE) +
  geom_text(aes(x = location, label = country, hjust = hjust), show.legend = FALSE) +
  xlab("") +
  ylab("Life Expectancy") 


#Bland-Altman plot:Since what we're interested in is in differences, it makes sense to dedicate one of our axes to differences.
#TUKEY MEAN DIFFERENCE PLOT,MA PLOT


library(ggrepel)
dat %>%
  filter(year %in% c(2010,2015) & ! is.na(life_expectancy))%>%
  mutate(year = paste0("life_expectancy_", year)) %>% #past0 does not change the name of the column only maniupulates column content  #In R, the paste0() function is used to concatenate vectors after converting to character vectors.
  select(country, year, life_expectancy) %>%
  spread(year, life_expectancy) %>% #combines two columns :makes first column(year) as  columnnames and second column(life_expectency) as column content.previous column names get deleted. 
  mutate(average = (life_expectancy_2015 + life_expectancy_2010)/2,
         difference = life_expectancy_2015 - life_expectancy_2010) %>%
  ggplot(aes(average, difference, label = country)) +
  geom_point(aes(col=country),size=3) +
  geom_text(nudge_x=0.075) 
 # geom_abline(lty = 2) +
 # xlab("Average of 2010 and 2015") +
 # ylab("Difference between 2015 and 2010")

#encoding 3rd variable.

#case study: vaccines

# import data and inspect
library(tidyverse)
library(dslabs)
data(us_contagious_diseases)
str(us_contagious_diseases)
us_contagious_diseases
# assign dat to the per 10,000 rate of measles, removing Alaska and Hawaii and adjusting for weeks reporting
the_disease <- "Measles"
dat <- us_contagious_diseases %>%
  filter(!state %in% c("Hawaii", "Alaska") & disease == the_disease) %>%
  mutate(rate = count / population * 10000 * 52/weeks_reporting) %>%
  mutate(state = reorder(state, rate))
dat

# plot disease rates per year in California
dat %>% filter(state == "California" & !is.na(rate)) %>%
  ggplot(aes(year, rate)) +
  geom_line() +
  ylab("Cases per 10,000") +
  geom_vline(xintercept=1960, col = "red")+
  geom_hline(yintercept=100, col = "red")


# tile plot of disease rate by state and year
dat %>% ggplot(aes(year, state, fill=rate)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"), trans = "sqrt") +
  geom_vline(xintercept = 1966, col = "blue") +
  theme_minimal() + theme(panel.grid = element_blank()) +
  ggtitle('Measles')
  

#colors
library(RColorBrewer)
display.brewer.all(type="seq")
display.brewer.all(type='div')


#compute US average measles rate by year
avg <- us_contagious_diseases %>%
  filter(disease == the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm = TRUE)/sum(population, na.rm = TRUE)*10000)
avg

# make line plot of measles rate by year by state
dat %>%
  filter(!is.na(rate)) %>%
  ggplot() +
  geom_line(aes(year, rate, group = state), color = "grey50",       #this makes the gray multiple lines 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate), data = avg, size = 1, col = "black") +    #this makes the black line of the rate
  geom_text(data = data.frame(x = 1955, y = 50),
            mapping = aes(x, y, label = "US average"), color = "black") +
  geom_vline(xintercept = 1963, col = "blue")


#Avoid Pseudo and Gratuitous 3D Plots
#line is better than three dimentional figure. color drug type then one dimension for survival and other one for dose number.
#avoid 3d plots
#Avoid Too Many Significant Digits with signif and round number of rounded numbers can be shown by option(digits=n)



#tile plot for smallpox

library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(dslabs)
data(us_contagious_diseases)

the_disease = "Smallpox"


dat <- us_contagious_diseases %>% 
  filter(!state%in%c("Hawaii","Alaska") & disease == "Smallpox" & weeks_reporting>=10) %>% 
  mutate(rate = count / population * 10000) %>% 
  mutate(state = reorder(state, rate))
dat

dat %>% ggplot(aes(year, state, fill = rate)) + 
  geom_tile(color = "grey50") + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") + 
  theme_minimal() + 
  theme(panel.grid = element_blank()) + 
  geom_vline(xintercept=1941,col="blue")+
  ggtitle(the_disease) + 
  ylab("") + 
  xlab("")



#Time series plot #group_by is important for 3d time_series plots


library(dplyr)
library(ggplot2)
library(dslabs)
library(RColorBrewer)
data(us_contagious_diseases)
the_disease = "Smallpox"

dat <- us_contagious_diseases %>%
  filter(!state%in%c("Hawaii","Alaska") & disease == the_disease & weeks_reporting >= 10) %>%
  mutate(rate = count / population * 10000) %>%
  mutate(state = reorder(state, rate))

avg <- us_contagious_diseases %>%
  filter(disease==the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm=TRUE)/sum(population, na.rm=TRUE)*10000)

avg

dat %>% ggplot() +
  geom_line(aes(year, rate, group = state),  color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate),  data = avg, size = 1, color = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5,25,125,300)) + 
  ggtitle("Cases per 10,000 by state") + 
  xlab("") + 
  ylab("") +
  geom_text(data = data.frame(x=1955, y=50), mapping = aes(x, y, label="US average"), color="black") + 
  geom_vline(xintercept=1963, col = "blue")



#Time series plot - all diseases in California

library(dplyr)
library(ggplot2)
library(dslabs)
library(RColorBrewer)
data(us_contagious_diseases)

new_con <- us_contagious_diseases %>% filter(state=="California" & weeks_reporting>=10) %>% 
  group_by(year, disease) %>%
  summarize(rate = sum(count)/sum(population)*10000) %>%
  ggplot(aes(year, rate, color = disease)) + 
  geom_line()
  
new_con
  

#Time series plot - all diseases in the United States

library(dplyr)
library(ggplot2)
library(dslabs)
library(RColorBrewer)
data(us_contagious_diseases)

us_contagious_diseases %>% filter(!is.na(population)) %>% 
  group_by(year, disease) %>%
  summarize(rate = sum(count)/sum(population)*10000) %>%
  ggplot(aes(year, rate, color = disease)) + 
  geom_line()


#vis exercise 

install.packages("titanic")
library(titanic)

#barplots with percentage of each one rather than count using geom_bar(position="fill")

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

titanic %>% filter(!is.na(Age) &!is.na(Survived)) %>%
  ggplot(aes(Pclass,fill=Survived))+geom_bar(position="fill")

#bar plots to show count of each P class

titanic %>% filter(!is.na(Age) &!is.na(Survived)) %>%
  ggplot(aes(Pclass,fill=Survived))+geom_bar()

#jitter plot with boxplot

titanic %>% select(Age,Sex) %>%
  ggplot(aes(Sex,Age,color=Sex))+geom_jitter(width = 0.1, alpha = 0.2)+
  geom_boxplot(alpha=0.1)


#density plots for more than one is by making a color = survival
titanic %>%
  ggplot(aes(Age, y = ..count..,fill=Survived)) +
  geom_density(alpha = 0.2)

#Create a grid of density plots for age, filled by survival status, with count on the y-axis, faceted by sex and passenger class.

library(dslabs)
library(tidyverse)
library(dbplyr)                  # zkr11
library(gridExtra)

titanic %>%
  ggplot(aes(Age, y = ..count..,fill=Survived)) +
  geom_density(alpha = 0.2)+facet_grid(Sex~Pclass)


options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)
str(titanic_train)



###########################
#creating maps in R
##########################
library(ggplot2)
library(dplyr)
require(maps)
require(viridis)    #necessary libraries for mapping
theme_set(
  theme_void()
)
#######################
EUvax <- read.csv("C:/Users/Abdulrhman Al-Tabali/Downloads/EUvaccine.csv") #####this reads in the data file I made you will need to change the path to your computer
View(EUvax)

mapdata <- map_data("world") ##ggplot2
View(mapdata)
mapdata <- left_join(mapdata, EUvax, by="region")
View(mapdata)

mapdata1<-mapdata %>% filter(!is.na(mapdata$Perc_vaccinated))
View(mapdata1)

map1 <- mapdata1 %>% ggplot(aes( x = long, y = lat, group=group)) +
  geom_polygon(aes(fill = Perc_vaccinated), color = "black")
#here we can't lable every single place it will cause clutter
map1           #this is enough 
#####################################
#if you want to add colors 
map2 <- map1 + scale_fill_gradient(name = "% vaccinated", low = "red", high =  "green", na.value = "grey50")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        rect = element_blank())
map2



install.packages("plotrix" )
######################################
#make a map for murders total filled with region and shapes show murderer total
#####################################
library(plotrix )
library(ggplot2)
library(dplyr)
library(dslabs)
require(maps)
require(viridis)    #necessary libraries for mapping

theme_set(
  theme_void()
)
data("murders")
colnames(murders)[3] <- "BigRegion"
colnames(murders)[1] <- "region"
murders$region <- tolower(murders$region)         # here we change columns names to match with the state and use by="region" 

view(murders)


mapdata= map_data('state')
view(mapdata)
mapdata_murders=left_join(mapdata,murders,by='region')
view(mapdata_murders)

mapdata_murders_new <- mapdata_murders %>% filter(!is.na(total))

mapdata_murders_new %>% ggplot(aes( x = long, y = lat, group=group,color=BigRegion)) +
  geom_polygon(aes(fill = total), color = "black")+
  ggtitle("Total murders in each state")+
  theme(plot.title = element_text(hjust = 0.5))
  
mapdata_murders_new
