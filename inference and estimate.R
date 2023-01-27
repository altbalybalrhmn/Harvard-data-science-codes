#Code: Function for taking a random draw from a specific urn
library(tidyverse)
library(dslabs)
take_poll(25)    # draw 25 beads


#For loop in R

# The vector `p` contains 100 proportions of Democrats ranging from 0 to 1 using the `seq` function
p <- seq(0, 1, length = 100)

# The vector `sample_sizes` contains the three sample sizes
sample_sizes <- c(25, 100, 1000)

# Write a for-loop that calculates the standard error `se` for every value of `p` for each of the three samples sizes `N` in the vector `sample_sizes`. Plot the three graphs, using the `ylim` argument to standardize the y-axis across all three plots.
for(N in sample_sizes){
  se <- sqrt(p*(1-p)/N)
  plot(p, se, ylim = c(0,0.5/sqrt(25)))
}



#Code: Computing the probability of X_hat being within .01 of p
N=25
X_hat <- 0.48
se_hat <- sqrt(X_hat*(1-X_hat)/N) #se hat is estimated SE where we have X hats in p places
se_hat
pnorm(0.01/se_hat) - pnorm(-0.01/se_hat) #the answer is probably 8%


#Code: Monte Carlo simulation using a set value of p
p <- 0.45    # unknown p to estimate     urn of 25 beeds (12blue and 13red) we want X_hat as the mean of blue x random variable
N <- 1000

# simulate B polls of size N and determine average x_hat
B <- 10000    # number of replicates
N <- 1000    # sample size per replicate
x_hat <- replicate(B, {
  x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p)) #urn of 25 beeds (12blue and 13red) we want X_hat as the mean of blue x random variable
  mean(x)
})
x_hat
mean(x_hat)

#Code: Histogram and QQ-plot of Monte Carlo results
library(tidyverse)
library(gridExtra)
p1 <- data.frame(x_hat=x_hat) %>%
  ggplot(aes(x_hat)) +
  geom_histogram(binwidth = 0.005, color = "black")
p2 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(sample=x_hat)) +          #sample makes here default sd and mean 
  stat_qq(dparams = list(mean = mean(x_hat), sd = sd(x_hat))) +
  geom_abline() +
  ylab("X_hat") +
  xlab("Theoretical normal")
grid.arrange(p1, p2, nrow=1)
#the problem that we dont really know what p is but for various P and sample sizes N



#Code: Plotting margin of error in an extremely large poll over a range of values of p
library(tidyverse)
N <- 100000
p <- seq(0.35, 0.65, length = 100)
SE <- sapply(p, function(x) 2*sqrt(x*(1-x)/N))# What is sapply in R? The sapply function in R is a vectorized function of the apply family that allows you to iterate over a list or vector without the need of using the for loop, that is known to be slow in R.
SE
data.frame(p = p, SE = SE) %>%
  ggplot(aes(p, SE)) +
  geom_line()



#function that computes the mean of random variables (x_hat)
take_sample <- function(N,p){
  x <- sample(c(0,1),size=N,replace=TRUE,prob=c(1-p,p))
  mean(x)
}

B=10000
N=100
p <- 0.45
#error= p-x_hat

error <- p-take_sample(N,p)
error


#the difference between X and X_bar
p <- 0.45
N <- 100
set.seed(1)
# Define `X` as a random sample of `N` voters with a probability of picking a Democrat ('1') equal to `p`
X <- sample(c(0,1),size=N,replace=TRUE,prob=c(1-p,p))
# Define `X_bar` as the average sampled proportion
X_bar=mean(X)
X_bar
# Calculate the standard error of the estimate. Print the result to the console.
SE<-sqrt(X_bar*(1-X_bar)/N)
SE

N <- seq(100, 5000, len = 100)
p <- 0.5
se <- sqrt(p*(1-p)/N)
se




#confidence interval curve
data("nhtemp")
nhtemp
data.frame(year = as.numeric(time(nhtemp)), temperature = as.numeric(nhtemp)) %>%
  ggplot(aes(year, temperature)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Average Yearly Temperatures in New Haven")
nhtemp

#Code: Monte Carlo simulation of confidence intervals
p <- 0.45
N <- 1000
X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))    # generate N observations
X_hat <- mean(X)    # calculate X_hat
SE_hat <- sqrt(X_hat*(1-X_hat)/N)    # calculate SE_hat, SE of the mean of N observations
c(X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # build interval of 2*SE above and below mean

#Monte carlo simulation on confidence interval Premium ********* مهم جداً
p<-0.45
N<-1000
B <- 10000
inside <- replicate(B, {
  X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  between(p, X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # TRUE if p in confidence interval
})
inside
mean(inside)


#excercise1:
        #Assume there are only two candidates and construct a 95% confidence interval for the election night proportion . find confidence interval.
        
        # Load the data
        library(dslabs)
        data("polls_us_election_2016")
        
        # Generate an object `polls` that contains data filtered for polls that ended on or after October 31, 2016 in the United States
        polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.") 
        polls
        # How many rows does `polls` contain? Print this value to the console.
        nrow(polls)
        
        # Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
        N <- polls$samplesize[1]   #***********NEW CODE*********
        N
        
        # For the first poll in `polls`, convert the percentage to a proportion of Clinton voters and assign it to a variable called `X_hat`. Print this value to the console.
        X_hat <- polls$rawpoll_clinton[1]/100         #***********new code***************
        X_hat
        
        # Calculate the standard error of `X_hat` and save it to a variable called `se_hat`. Print this value to the console.
        se_hat <- sqrt(X_hat*(1-X_hat)/N)
        se_hat
        
        # Use `qnorm` to calculate the 95% confidence interval for the proportion of Clinton voters. Save the lower and then the upper confidence interval to a variable called `ci`.
        ci<- c(X_hat - qnorm(0.977)*se_hat, X_hat + qnorm(0.977)*se_hat)       #(x_hat-2*se_hat) in qnorm(0.95) doesnt equall 2 so we make qnorm(0.977) which is equal to 2
        ci
#excercise 2:# Create a new object called `pollster_results` that contains columns for pollster name, end date, X_hat, se_hat, lower confidence interval, and upper confidence interval for each poll.
        head(polls)
        #First mutate then select
        pollster_results <- polls %>% 
          mutate(X_hat = rawpoll_clinton/100, se_hat = sqrt(X_hat*(1-X_hat)/samplesize), lower = X_hat - qnorm(0.975)*se_hat, upper = X_hat + qnorm(0.975)*se_hat) %>% 
          select(pollster, enddate, X_hat, se_hat, lower, upper)
        pollster_results
        

#Code: Simulating polls
d <- 0.039  #difference in vote 
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p <- (d+1)/2

# calculate confidence intervals of the spread
confidence_intervals <- sapply(Ns, function(N){         #sapply applies a function on a vector
  X <- sample(c(0,1), size=N, replace=TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  2*c(X_hat, X_hat - 2*SE_hat, X_hat + 2*SE_hat) - 1
})

# generate a data frame storing results
polls <- data.frame(poll = 1:ncol(confidence_intervals),
                    t(confidence_intervals), sample_size = Ns)
names(polls) <- c("poll", "estimate", "low", "high", "sample_size")
polls

# Code: Calculating the spread of combined polls
d_hat <- polls %>%          #estimated difference(estimated spread)
  summarize(avg = sum(estimate*sample_size) / sum(sample_size)) %>%
  .$avg    #.$ and pull gets a vector 
d_hat
p_hat <- (1+d_hat)/2
moe <- 2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size))   
round(d_hat*100,1)
round(moe*100, 1)



#Code: Generating simulated poll data

        library(dslabs)
        data(polls_us_election_2016)
        names(polls_us_election_2016)
        
        # keep only national polls from week before election with a grade considered reliable
        polls <- polls_us_election_2016 %>%
          filter(state == "U.S." & enddate >= "2016-10-31" &
                   (grade %in% c("A+", "A", "A-", "B+") | is.na(grade)))
        
        # add spread estimate
        polls <- polls %>%
          mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
        
        # compute estimated spread for combined polls
        d_hat <- polls %>%
          summarize(d_hat = sum(spread * samplesize) / sum(samplesize)) %>%
          .$d_hat
        
        # compute margin of error
        p_hat <- (d_hat+1)/2
        moe <- 1.96 * 2 * sqrt(p_hat*(1-p_hat)/sum(polls$samplesize))
        
        # histogram of the spread
        polls %>%
          ggplot(aes(spread)) +
          geom_histogram(color="black", binwidth = .01)   #the graph is not normally distributed so what we do is choose pollsters that have done polls more than one
    
        # number of polls per pollster in week before election
        polls %>% group_by(pollster) %>% summarize(n())
        
        # plot results by pollsters with at least 6 polls
        polls %>% group_by(pollster) %>%
          filter(n() >= 6) %>%
          ggplot(aes(pollster, spread)) +
          geom_point() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1))
        polls
        # standard errors within each pollster
        polls %>% group_by(pollster) %>%
          filter(n() >= 6) %>%
          summarize(se = 2 * sqrt(p_hat * (1-p_hat) / median(samplesize)))
        polls

        
        
#Data driven models        
      # collect last result before the election for each pollster
      library(dplyr)
      library(tidyverse)        
      one_poll_per_pollster <- polls %>% group_by(pollster) %>%
        filter(enddate == max(enddate)) %>%      # keep latest poll
        ungroup()
      one_poll_per_pollster
      
      # histogram of spread estimates
      one_poll_per_pollster %>%
        ggplot(aes(spread)) + geom_histogram(binwidth = 0.01)
      
      
      # construct 95% confidence interval
      pnorm(1.96) #equals 97.5% 
      pnorm(2) # 97.7% 
      results <- one_poll_per_pollster %>%
        summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
        mutate(start = avg - 1.96*se, end = avg + 1.96*se)
      round(results*100, 1)

#excercise important 1: 
      # Load the 'dslabs' package and data contained in 'heights'
      library(dslabs)
      data(heights)
      library(dplyr)
      
      # Make a vector of heights from all males in the population
      x <- heights %>% filter(sex == "Male") %>%
        .$height
      
      # Calculate the population average. Print this value to the console.
      mean(x)   #population average is different from sample average look down to see sample average
      
      # Calculate the population standard deviation. Print this value to the console.
      sd(x)
      # The vector of all male heights in our population `x` has already been loaded for you. You can examine the first six elements using `head`.
      head(x)
      
      # Use the `set.seed` function to make sure your answer matches the expected result after random sampling
      set.seed(1)
      
      # Define `N` as the number of people measured
      N <- 50
      
      # Define `X` as a random sample from our population `x`
      X <- sample(x,N,replace=TRUE)
      X
      # Calculate the sample average. Print this value to the console.
      mean(X)    #X_hat
      
      # Calculate the sample standard deviation. Print this value to the console.
      sd(X)
      # Define `se` as the standard error of the estimate. Print this value to the console.
      se <- sd(X)/sqrt(N)
      # Construct a 95% confidence interval for the population average based on our sample. Save the lower and then the upper confidence interval to a variable called `ci`.
      ci <- c(mean(X)-qnorm(0.95)*se,mean(X)+qnorm(0.95)*se)
      ci
      # Define `B` as the number of times to run the model
      B <- 10000
      mu <- mean(x)
      
      # Define with monte carlo simulation an object `res` that contains a logical vector for simulated intervals that contain mu
      res <- replicate(B,{
        X<- sample(x,N,replace = TRUE)
        X_hat<- mean(X)
      })
      # Calculate the proportion of results in `res` that include mu. Print this value to the console.
        
      res <- replicate(B,{
        X<- sample(x,N,replace = TRUE)
        X_hat<- mean(X)
        between(mu,ci[1], ci[2])
      }) 
      mean(res)
      
#exercise 2 important visualizing the bias    
      
      # Load the libraries and data you need for the following exercises
      library(dslabs)
      library(dplyr)
      library(ggplot2)
      data("polls_us_election_2016")
      
      # These lines of code filter for the polls we want and calculate the spreads
      polls <- polls_us_election_2016 %>% 
        filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research","The Times-Picayune/Lucid") &
                 enddate >= "2016-10-15" &
                 state == "U.S.") %>% 
        mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) 
      
      # Make a boxplot with points of the spread for each pollster
      polls %>% ggplot(aes(pollster,spread))+
        geom_boxplot()
      
      
      
      
  #Bayesian statistics 
      
      prev <- 0.00025    # disease prevalence
      N <- 100000    # number of tests
      outcome <- sample(c("Disease", "Healthy"), N, replace = TRUE, prob = c(prev, 1-prev))
      
      N_D <- sum(outcome == "Disease")    # number with disease
      N_H <- sum(outcome == "Healthy")    # number healthy
      N_D/N
      
      
      # for each person, randomly determine if test is + or -
      accuracy <- 0.99
      test <- vector("character", N)   #prepares empty 100000 ("") vector
      test[outcome == "Disease"] <- sample(c("+", "-"), N_D, replace=TRUE, prob = c(accuracy, 1-accuracy))
      test[outcome == "Healthy"] <- sample(c("-", "+"), N_H, replace=TRUE, prob = c(accuracy, 1-accuracy))
      table(outcome, test)
      
#Excerise 1 Bayes: Pr(1|2)=(Pr(2|1)*Pr(1))/Pr(2) 
      #question find the probability that the mother is a murderer given that two children were found dead with no evidence.
      
      # Define `Pr_1` as the probability of the first son dying of SIDS
      Pr_1 <- 1/8500
      
      # Define `Pr_2` as the probability of the second son dying of SIDS
      Pr_2 <- 1/100
      
      # Define `Pr_B` as the probability of both sons dying of SIDS
      Pr_B <- Pr_1*Pr_2
      
      # Define Pr_A as the rate of mothers that are murderers
      Pr_A <- 1/10000009
      
      # Define Pr_BA as the probability that two children die without evidence of harm, given that their mother is a murderer
      Pr_BA <- 0.50
      
      # Define Pr_AB as the probability that a mother is a murderer, given that her two children died with no evidence of physical harm. Print this value to the console.
      
      Pr_AB <- (Pr_BA*Pr_A)/Pr_B
      Pr_AB
      
      
#excercise 2 Calculate a spread average and provide an estimate of the standard error.
      
      # Load the libraries and poll data
      library(dplyr)
      library(dslabs)
      data(polls_us_election_2016)
      
      # Create an object `polls` that contains the spread of predictions for each candidate in Florida during the last polling days
      polls <- polls_us_election_2016 %>% 
        filter(state == "Florida" & enddate >= "2016-11-04" ) %>% 
        mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
      
      # Examine the `polls` object using the `head` function
      head(polls)
      
      # Create an object called `results` that has two columns containing the average spread (`avg`) and the standard error (`se`). Print the results to the console.
      results <- polls %>% summarize(avg = mean(spread),se=sd(spread)/sqrt(length(spread)))
      results
      
  #excercise follow before Estimate the Posterior Distribution
      
      # The results` object has already been loaded. Examine the values stored: `avg` and `se` of the spread
      results
      
      # Define `mu` and `tau`  ** these two values are given according to results of previous actual elections
      mu <- 0
      tau <- 0.01
      
      
      # Define a variable called `Y` that contains the average in the object `results`
      Y <- results$avg
      Y    
      
      # Define a variable called `sigma` that contains the standard error in the object `results`
      sigma <- results$se
      sigma
      

      # Define a variable `B` using `sigma` and `tau`. Print this value to the console.
      B <- (sigma^2)/((sigma^2)+(tau^2))
      B
      
      # Calculate the expected value of the posterior distribution
      E_Post <-mu+(1-B)*(Y-mu)
      E_Post
      # Compute the standard error of the posterior distribution. Print this value to the console.
      SE_Post <- sqrt(1/((1/sigma^2)+(1/tau^2)))
      
      # Construct the 95% credible interval based on posterior E and SE . Save the lower and then the upper confidence interval to a variable called `ci`.
      ci <- c(E_Post-qnorm(0.975)*SE_Post,E_Post+qnorm(0.975)*SE_Post)
      ci
      
      # Using the `pnorm` function, calculate the probability that the actual spread was less than 0 
      pnorm(0,E_Post,SE_Post)
      
      # Define a variable `taus` as different values of tau
      taus <- seq(0.005, 0.05, len = 100)
      
      # Create a function called `p_calc` that generates `B` and calculates the probability of the spread being less than 0
      p_calc <- function(tau){                  # فرق بين tau و taus(tau is like x the variable)
        B <- (sigma^2)/((sigma^2)+(tau^2))
        E_Post <-mu+(1-B)*(Y-mu)
        SE_Post <- sqrt(1/((1/sigma^2)+(1/tau^2))) 
        pnorm(0,E_Post,SE_Post)
      }
      
      # Create a vector called `ps` by applying the function `p_calc` across values in `taus`
      ps <- p_calc(taus)
      ps
      
      # Plot `taus` on the x-axis and `ps` on the y-axis
      plot(taus,ps)
      
      
#This code from previous videos defines the results object used for empirical Bayes election forecasting.
      
      library(tidyverse)
      library(dslabs)
      polls <- polls_us_election_2016 %>%
        filter(state == "U.S." & enddate >= "2016-10-31" &
                 (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
        mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
      
      one_poll_per_pollster <- polls %>% group_by(pollster) %>%
        filter(enddate == max(enddate)) %>%
        ungroup()
      
      results <- one_poll_per_pollster %>%
        summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
        mutate(start = avg - 1.96*se, end = avg + 1.96*se)
      Note that to compute an exact 95% credible interval, we would use qnorm(.975) instead of 1.96.
      
      #Code: Computing the posterior mean, standard error, credible interval and probability
      #Note that to compute an exact 95% credible interval, we would use qnorm(.975) instead of 1.96.
      
      mu <- 0                #0 because we dont know who is winning         
      tau <- 0.035            # 0.035 the sd according to previous elections
      
      Y <- results$avg  # (x_hat)
      sigma <- results$se#(se_hat)      from recent polls
      
      B <- sigma^2 / (sigma^2 + tau^2)
      posterior_mean <- mu+(1-B)*(Y-mu)   #posterior expected value
      posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2)) #posterior standard error
      
      posterior_mean
      posterior_se
      
      # 95% credible interval
      posterior_mean + c(-1.96, 1.96)*posterior_se
      
      # probability of p > 0
      1 - pnorm(0, posterior_mean, posterior_se)
      
#Mathematical representation
      
      J <- 6   #samples from polls
      N <- 2000
      d <- .021
      p <- (d+1)/2
      X <- d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
      
      
      I <- 5   #pollsters
      J <- 6   #samples from polls
      N <- 2000
      d <- .021
      p <- (d+1)/2
      X <- sapply(1:I, function(i){
        d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
      })
      
      
      I <- 5     #pollsters
      J <- 6    #samples from polls
      N <- 2000
      d <- .021
      p <- (d+1)/2
      h <- rnorm(I, 0, 0.025)    # assume standard error of pollster-to-pollster variability is 0.025(house effect)
      X <- sapply(1:I, function(i){
        d + h[i] + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
      })
      X
      
# Calculating probability of  with general bias
      #Note that sigma now includes an estimate of the variability due to general bias sigma_b <- 0.025
     
      mu <- 0        #we dont know who is winning
      tau <- 0.035    #previous elections sd 
      
      sigma <- sqrt(results$se^2 + .025^2)
      Y <- results$avg
      B <- sigma^2 / (sigma^2 + tau^2)
      
      posterior_mean <- B*mu + (1-B)*Y
      posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))
      
      1 - pnorm(0, posterior_mean, posterior_se)
      
#  Calculating 95% confidence intervals with the t-distribution
      z <- qt(0.975, nrow(one_poll_per_pollster) - 1)
      one_poll_per_pollster %>%
        summarize(avg = mean(spread), moe.
                  = z*sd(spread)/sqrt(length(spread))) %>%
        mutate(start = avg - moe, end = avg + moe)
      
      # quantile from t-distribution versus normal distribution
      qt(0.975, 14)    # 14 = nrow(one_poll_per_pollster) - 1
      qnorm(0.975)
      
      
#Association tests
      # Research funding rates example
      # load and inspect research funding rates object
      library(tidyverse)
      library(dslabs)
      data(research_funding_rates)
      research_funding_rates
      
      # compute totals that were successful or not successful
      totals <- research_funding_rates %>%
        select(-discipline) %>%
        summarize_all(funs(sum)) %>%
        summarize(yes_men = awards_men,
                  no_men = applications_men - awards_men,
                  yes_women = awards_women,
                  no_women = applications_women - awards_women)
      
      
      # compare percentage of men/women with awards
      totals %>% summarize(percent_men = yes_men/(yes_men + no_men),
                           percent_women = yes_women/(yes_women + no_women))
      
      # Two-by-two table and p-value for the Lady Tasting Tea problem
      tab <- matrix(c(3,1,1,3), 2, 2)
      tab
      rownames(tab) <- c("Poured Before", "Poured After")
      colnames(tab) <- c("Guessed Before", "Guessed After")
      tab
      
      # p-value calculation with Fisher's Exact Test
      fisher.test(tab, alternative = "greater")
      
      
#Chi-squared test
      
      # compute overall funding rate
      funding_rate <- totals %>%
        summarize(percent_total = (yes_men + yes_women) / (yes_men + no_men + yes_women + no_women)) %>%
        .$percent_total
      funding_rate
      
      # construct two-by-two table for observed data
      two_by_two <- tibble(awarded = c("no", "yes"),
                           men = c(totals$no_men, totals$yes_men),
                           women = c(totals$no_women, totals$yes_women))
      two_by_two
      
      # compute null hypothesis two-by-two table
      tibble(awarded = c("no", "yes"),
             men = (totals$no_men + totals$yes_men) * c(1-funding_rate, funding_rate),
             women = (totals$no_women + totals$yes_women) * c(1-funding_rate, funding_rate))
      tibble
      
      # chi-squared test
      chi_sq_test <- two_by_two %>%
        select(-awarded) %>%
        chisq.test()
      chisq_test$p.value  #We see that the p-value is 0.051. this means that the probability of seeing a deviation like the one we see or bigger under the null that funding is assigned at random is 0.051.
      
      #Odds ratio
      # odds of getting funding for men
      odds_men <- (two_by_two$men[2] / sum(two_by_two$men)) /
        (two_by_two$men[1] / sum(two_by_two$men))
      odds_men
      # odds of getting funding for women
      odds_women <- (two_by_two$women[2] / sum(two_by_two$women)) /
        (two_by_two$women[1] / sum(two_by_two$women))
      odds_women
      # odds ratio - how many times larger odds are for men than women
      odds_men/odds_women
      
      
      #p-value and odds ratio responses to increasing sample size
      # multiplying all observations by 10 decreases p-value without changing odds ratio
      two_by_two %>%
        select(-awarded) %>%               #     p value changes but odd ratio stays the same
        mutate(men = men*10, women = women*10) %>%
        chisq.test()