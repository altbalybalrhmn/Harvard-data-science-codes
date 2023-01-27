#which is the mathematical foundation of statistical inference.

#permutation order maters and for combination order is not important 
#permmutation= nPr(n=number of hole,r number of the part)=n!/(n-r)!
#combination= nCr=n!/((n-r)!*r!)
#combination = ncr=nPr/r!
install.packages('dslabs')
library(dslabs)
rep("Rahman",times=3)
beads <- rep(c("red", "blue"), times = c(2,3))    # create an urn with 2 red, 3 blue
beads    # view beads object
sample(beads, 1)    # sample 1 bead at random

#as if we are doing the expirment forever replicate Monte carlo

B <- 10000    # number of times to draw 1 bead
events <- replicate(B, sample(beads, 1))    # draw 1 bead, B times
events
tab <- table(events)    # make a table of outcome counts
tab    # view count table

prop.table(tab)    # view table of outcome proportions

library(dslabs)
prop.table(table(sample(beads,10000,replace=TRUE))) #works instead of replicate


#seeding

set.seed(1)
set.seed(1, sample.kind="Rounding") 


#mean of prop
#To find the probability of drawing a blue bead at random
mean(beads=="blue")


#dependency

#Dependent

beads <- rep(c("red", "blue"), times = c(2,3))    # create an urn with 2 red, 3 blue
beads    # view beads object
x <- sample(beads, 5)    # sample 1 bead at random
x

x[2:5]


#independent 

y <- sample(beads, 5,replace="True")
y
y[2:5]

#exercise
#one ball will be drawn at random from a box containing: 3 cyan balls, 5 magenta balls, and 7 yellow balls.

What is the probability that the ball will be cyan?
balls <- rep(c("ceyan", "mag","yellow"), times = c(3,5,7)) 
mean(balls!='ceyan')


# joining strings with paste
number <- "Three"
suit <- "Hearts"
paste(number, suit)

# joining vectors element-wise with paste
paste(letters[1:5], as.character(1:5))

# generating combinations of 2 vectors with expand.grid
expand.grid(pants = c("blue", "black"), shirt = c("white", "grey", "plaid"))

#Generating a deck of cards
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck
deck_1<- paste(deck$number, deck$suit)
deck_1
data.frame(deck_1)

# probability of drawing a king
kings <- paste("King", suits)
kings
mean(deck_1 %in% kings)   #4/52


#permutation and compination 
install.packages("gtools")
library(gtools)

permutations(10,3,v=0:9)  #choose 3 numbers out of 10 cards  # ways to choose 2 numbers in order from 1:5
all_phone_numbers <- permutations(10, 7, v = 0:9)
all_phone_numbers
n <- nrow(all_phone_numbers)
n

index <- sample(n, 5)
index
all_phone_numbers[index,]

permutations(3,2)    # order matters 1  2 and 2  1 are OK 
combinations(3,2)    # order does not matter if 1  2 apears then no need to mention 2  1 


#king given that one king is drawn                
hands <- permutations(52,2, v = deck_1)
hands
first_card <- hands[,1]     #first column              #please do not mix this with [2,] 
second_card <- hands[,2]    #second column            #[2,]row index, [,2] column index
sum(first_card %in% kings)  
sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)
  

permutations(3,2)    # order matters
combinations(3,2)    # order does not matter


# Probability of a natural 21 in blackjack


aces <- paste("Ace", suits) #single with list directly paste
facecard <- c("King", "Queen", "Jack", "Ten") 
facecard <- expand.grid(number = facecard, suit = suits) #two lists or vectors first we make two lists dataframe
facecard <- paste(facecard$number, facecard$suit)

hands_c <- combinations(52, 2, v=deck_1) # all possible hands  #focus we used comb
hands_c
# probability of a natural 21 given that the ace is listed first in `combinations`
mean(hands_c[,1] %in% aces & hands_c[,2] %in% facecard)  #no need to mention vise vers

# probability of a natural 21 checking for both ace first and ace second
hands_p <- permutations(52, 2, v=deck_1)  #focus we used permutations
hands_p
mean((hands_p[,1] %in% aces & hands_p[,2] %in% facecard)|(hands_p[,2] %in% aces & hands_p[,1] %in% facecard))



#Monte Carlo simulation of natural 21 in blackjack

# code for one hand of blackjack
hand <- sample(deck, 2)
hand

# code for B=10,000 hands of blackjack
B <- 10000
results <- replicate(10000, {               #not important
  hand <- sample(deck, 2)          #choose two cards 10000 times
  (hand[1] %in% aces & hand[2] %in% facecard) | (hand[2] %in% aces & hand[1] %in% facecard)
})
mean(results)



#permitations/first    compinations net

#50 student class 2 have same birthday

# checking for duplicated bdays in one 50 person group
n <- 50
bdays <- sample(1:365, n, replace = TRUE)    # generate n random birthdays
any(duplicated(bdays))    # check if any birthdays are duplicated #any have only one answer True or False(Yes or no question does not give vector answer)

index <- which(duplicated(bdays))
bdays[index]


# Monte Carlo simulation with B=10000 replicates
B <- 10000
results <- replicate(B, {    # returns vector of B logical values
  bdays <- sample(1:365, 50, replace = TRUE)
  any(duplicated(bdays))
})
results
mean(results)    # calculates proportion of groups with duplicated bdays


# function to calculate probability of shared bdays across n people
# function to calculate probability of shared bdays across n people
compute_prob <- function(n, B = 10000) {
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

n <- seq(1, 60)
sapply(1:10,sqrt)

prob <- sapply(n, compute_prob)  #permits us to perform element wise operations on any functions
prob
plot(n, prob)  #plot monte carlos results

1+1


#how big B should be

B <- 10^seq(1, 5, len = 100)    # defines vector of many B values
compute_prob <- function(B, n = 22){    # function to run Monte Carlo simulation with each B
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

prob <- sapply(B, compute_prob)    # apply compute_prob to many values of B
plot(log10(B), prob, type = "l")    # plot a line graph of estimates 

#monty hall


B <- 1000000
stick <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)    # open door with no prize that isn't chosen
  stick <- my_pick    # stick with original door
  stick == prize_door    # test whether the original door has the prize
})
mean(stick)    # probability of choosing prize door when sticking




switch <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen first
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)    # open door with no prize that isn't chosen
  switch <- doors[!doors%in%c(my_pick, show)]    # switch to the door that wasn't chosen first or opened
  switch == prize_door    # test whether the switched door has the prize
})
mean(switch)    # probability of choosing prize door when switching






#continuous probability

#cumulative dist function


#Define x as male heights from the dslabs heights dataset:
  
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)   #.$ and pull selects one column but returns results as a vector
x
Given a vector x, we can define a function for computing the CDF of x using:
  
f_P <- function(a) mean(x <= a)
f_P(70)    # probability of male taller than 70 inches

1-f_P(70)

# probability that a man is between 72 and 70
f_P(72)-f_P(70)

install.packages("tidyverse")
library(tidyverse)
install.packages('dplyr ')
library(dplyr)
data(heights)
seq(1,5,len=100)

#what is the probability that a male student between 70 and 50
#emprical cdf
x <- heights %>% filter(sex=="Male") 
x

#another for selection (.$ and pull) (column) makes vektor
y <- heights %>% filter(sex=="Male") %>% select(height)
y   #select does not make vektor
str(y)  #dataframe
x <- heights %>% filter(sex=="Male") %>% .$height
x <- heights %>% filter(sex=="Male") %>% pull(height)
x
str(x)  #numeric vektor [1:812]
F <- function(a) mean(x <= a)
F(70)-F(66)


#theorical dist

#in R we use pnorm(a,avg,s) to define the cumulative dist for normal dist
x<-heights$height[heights$sex=="Male"]
x
pnorm(72,mean(x),sd(x))-pnorm(70,mean(x),sd(x)) probabilty of #code 269 #men between 70,72


pnorm(70,mean(x),sd(x)) #the probabilty of Men <= 70
1-pnorm(70,mean(x),sd(x))  #the probabilty of Men > 70

# plot distribution of exact heights in data 
table(x)
prop.table(table(x))
# plot distribution of exact heights in data
plot(prop.table(table(x)), xlab = " Height in inches", ylab = "Proportion")  #table counts the number of each one and writes under(calculates the proportion of each one)
plot(table(x), xlab = " Height in inches", ylab = "number of males")

# probabilities in actual data over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

# probabilities in normal approximation match well
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))



# probabilities in actual data over other ranges don't match normal approx as well (discretization)
mean(x <= 70.9) - mean(x <= 70.1) #actual
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))#approx do not use actuall data because we have sd


#probability density dnorm() calculates the area under curve integral

library(tidyverse)
x <- seq(-4, 4, length = 100)
y<- data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x, f)) +
  geom_line()
y


#Monte carlos using normal dist
#rnorm(n,mean,sd): the R function that simulates random variables having a specified normal distribution

# define x as male heights from dslabs data
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)  #x as a vector
n<-length(x)
avg<-mean(x)
sd<-sd(x)
mon_car_sim_length_norm_dist<- rnorm(n,avg,sd)
mon_car_sim_length_norm_dist

data.frame(mon_car_sim_length_norm_dist)%>% #in order to plot any list we should change it into dataframe first
  ggplot(aes(mon_car_sim_length_norm_dist))+  
  geom_histogram(color="blue",binwidth=2)




#Code: Monte Carlo simulation of tallest person over 7 feet

B <- 10000
tallest <- replicate(B, {
  simulated_data <- rnorm(800, avg, sd)    # generate 800 normally distributed random heights
  max(simulated_data)    # determine the tallest height out of the 800 and picks it
})
tallest   #10000 has been picked 
mean(tallest >= 7*12)    # proportion of times that tallest person exceeded 7 feet (84 inches)




#density function for normal dist

x <- seq(-4, 4, length.out = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x,f)) +
  geom_line()


#Quiz
# If we pick a female at random, what is the probability that she is 5 feet or shorter? 5*12=60 inches
y <- heights %>% filter(sex=="Female")%>%pull(height)
avg_fem <- mean(y)
sd_fem<- sd(y)
pnorm(60,avg_fem,sd_fem)

#If we pick a female at random, what is the probability that she is taller than 6 feet ? 6*12=72 inches 

1-pnorm(72,avg_fem,sd_fem)

#if we pick a female at random, what is the probability that she is between 61 and 67 inches?

pnorm(67,avg_fem,sd_fem)-pnorm(61,avg_fem,sd_fem)

#Compute the probability that the height of a randomly chosen female is within 1 SD from the average height.
taller <- avg_fem+sd_fem
shorter<-avg_fem-sd_fem
pnorm(taller,avg_fem,sd_fem)-pnorm(shorter,avg_fem,sd_fem)

#Quiz 2: Imagine the distribution of male adults is approximately normal with an average of 69 inches and a standard deviation of 3 inches. How tall is a male in the 99th percentile?

qnorm(0.99,69,3)

#quiz 3:The distribution of IQ scores is approximately normally distributed. The average is 100 and the standard deviation is 15. Suppose you want to know the distribution of the person with the highest IQ in your school district, where 10,000 people are born each year.
#Generate 10,000 IQ scores 1,000 times using a Monte Carlo simulation. Make a histogram of the highest IQ scores.
# Use the `set.seed` function to make sure your answer matches the expected result after random number generation.
set.seed(1)

highestIQ <- replicate(B,{
  rand<-rnorm(10000,100,15)
  max(rand)
})
#hist(highestIQ)    #hist and plot functions can plot vectors 
data.frame(highestIQ)%>%ggplot(aes(highestIQ))+geom_histogram(color="blue",binwidth = 3)  #first we convert the vector outcome of norm into dataframe




#random variables: outcomes resulting from random process.
#statistical inference is to be able to quantify the uncertainty resulting from random operations.

# define random variable x to be 1 if blue, 0 otherwise
beads <- rep(c("red", "blue"), times = c(2, 3))
x <- ifelse(sample(beads, 1) == "blue", 1, 0)

# demonstrate that the random variable is different every time
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)



#We build a sampling model for the random variable S that represents the casino's total winnings. 
# sampling model 1: define urn, then sample
color <- rep(c("Black", "Red", "Green"), c(18, 18, 2)) # define the urn for the sampling model
n <- 1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)
X[1:10]
# sampling model 2: define urn inside sample function by noting probabilities
n=1000
x <- sample(c(-1, 1), n, replace = TRUE, prob = c(9/19, 10/19))    # 1000 independent draws
S <- sum(x)    # total winnings = sum of draws
S


#We use the sampling model to run a Monte Carlo simulation and use the results to estimate the probability of the casino losing money.
n <- 1000    # number of roulette players
B <- 10000    # number of Monte Carlo experiments
S <- replicate(B, {
  X <- sample(c(-1,1), n, replace = TRUE, prob = c(9/19, 10/19))    # simulate 1000 spins
  sum(X)    # determine total profit
})

mean(S < 0)    # probability of the casino losing money
hist(S)


#We can plot a histogram of the observed values of S as well as the normal density curve based on the mean and standard deviation of S.
library(tidyverse)
s <- seq(min(S), max(S), length = 100)    # sequence of 100 values across range of S
s
normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S))) # generate normal density for S
data.frame (S = S) %>%    # make data frame of S for histogram
  ggplot(aes(S, ..density..)) +
  geom_histogram(color = "black", binwidth = 10) +
  ylab("Probability") +
  geom_line(data = normal_density, mapping = aes(s, f), color = "blue")


#Expected value(mean)
B=10^6
X <- sample(c(-1,1),B,replace=TRUE,prob=c(9/19,10/19))
E <- mean(X)

#for Expected value, standard error and central limit theorem look at the notebook.



#Quiz:Create a model to predict your winnings from betting on green one time.

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green=1-p_green

# Create a model to predict the random variable `X`, your winnings from betting on green. Sample one time.
B=1
X <- sample(c(1,-1),B,replace=TRUE,prob=c(p_green,p_not_green))  #random variable

# Print the value of `X` to the console
X


#Quiz:

#Now, compute the expected value of , the random variable you generated previously.(Expected value(E)=ap+b(1-p)) a,b consequences of both draws

# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Calculate the expected outcome if you win $17 if the ball lands on green and you lose $1 if the ball doesn't land on green

a=17
b=-1
a*p_green+b*p_not_green

#Now, compute the standard error of that random variable, which represents a single outcome after one spin of the roulette wheel.

# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Compute the standard error of the random variable
sqrt(1)*abs(17-(-1))*sqrt(p_green*p_not_green) #sqrt(n)*abs(b-a)*sqrt(p*(1-p))


#Quiz: What is the expected value of S for outcomes if you try 1000 times ?

# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Define the number of bets using the variable 'n'
n <- 1000

# Calculate the expected outcome of 1,000 spins if you win $17 when the ball lands on green and you lose $1 when the ball doesn't land on green
1000*(17*p_green+(-1)*p_not_green)




#Quiz:You generated the expected value of , the outcomes of 1,000 bets that the ball lands in the green pocket, in the previous exercise.

#What is the standard error of S outcomes ?

# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Define the number of bets using the variable 'n'
n <- 1000

# Compute the standard error of the sum of 1,000 outcomes
sqrt(n)*abs(17-(-1))*sqrt(p_green*p_not_green)






#Quiz 1: compare avg with expected value and standard deviation with standard error


# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Define the number of bets using the variable 'n'
n <- 100

# The variable `B` specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Create an object called `S` that replicates the sample code for `B` iterations and sums the outcomes.
S <- replicate(B,{
  outcomes <- sample(c(17,-1),100,replace=TRUE,prob=c(p_green,p_not_green))
  sum(outcomes)
})



# Compute the average value for 'S'
#E<- n*(17*p_green+(-1)*p_not_green)
avg_s =mean(S)
avg_s
#E
# Calculate the standard deviation of 'S'
#SE <- sqrt(n)*abs(17-(-1))*sqrt(p_green*p_not_green)
sd_s<- sd(S)
sd_s
#SE



#the big short

#Code: Interest rate sampling model
n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02
defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE)
sum(defaults * loss_per_foreclosure)


#Code: Interest rate Monte Carlo simulation
B <- 10000
losses <- replicate(B, {
  defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE) 
  sum(defaults * loss_per_foreclosure)
})
hist(losses)
library(tidyverse)
data.frame(losses_in_millions = losses/10^6) %>%
  ggplot(aes(losses_in_millions)) +
  geom_histogram(binwidth = 0.6, col = "black")

#code using central limit theorem.

E <-n*(p*loss_per_foreclosure + (1-p)*0)    # expected value 
SE<- sqrt(n)*abs(loss_per_foreclosure-0)*sqrt(p*(1-p))    # standard error

#pnorm(,E,SE)

#Code: Calculating interest rate for 1% probability of losing money
l <- loss_per_foreclosure #200000
z <- qnorm(0.01)    #how much do you want the probability to be
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p))) #profit
x/180000   #180000 is the loan amount # interest rate
loss_per_foreclosure*p + x*(1-p)    # expected value of the profit per loan
n*(loss_per_foreclosure*p + x*(1-p)) # expected value of the profit over n loans


#Code: Monte Carlo simulation for 1% probability of losing money


B <- 100000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)    # expected value of the profit over n loans
mean(profit<0)    # probability of losing money




#Code: Expected value with higher default rate and interest rate
p <- 0.04
loss_per_foreclosure <- -200000
r <- 0.05   #interest rate
x <- r*180000   #interest per loan
loss_per_foreclosure*p + x*(1-p)

#the big short determination of n
#if we know the percentage we want to achieve then it is easy to find n

z <- qnorm(0.01)
l <- loss_per_foreclosure
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)
n    # number of loans required
n*(loss_per_foreclosure*p + x * (1-p))    # expected profit over n loans         So the budget we need is 

#confirming previous with monte carlos(Code: Monte Carlo simulation with known default probability)

B <- 10000
p <- 0.04
x <- 0.05 * 180000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)
mean(profit<0)   



#Code: Monte Carlo simulation with unknown default probability of default we estimate that p is between 3-5%
p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-new_p, new_p), replace = TRUE)
  sum(draws)
})
hist(profit/10^6)
mean(profit)    # expected profit
mean(profit < 0)    # probability of losing money
mean(profit < -10000000)    # probability of losing over $10 million



