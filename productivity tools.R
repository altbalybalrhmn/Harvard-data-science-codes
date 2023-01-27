install.packages(c("tiddverse","dslabs"))   #install more than one
#tools then install packages is good if you don't remember the package name properly 
installed.packages()
library(tidyverse)         #use the library
library(dslabs)

data(murders)

murders %>% 
  ggplot(aes(population,total,label=abb,fill=region))+
  geom_label()
murders
