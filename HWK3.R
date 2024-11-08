options(scipen=77)
library(tidyverse)
library(mosaic)
library(moderndive)

#Gas and Presidential Approval


approval = approval %>% 
  mutate(gas10 = gas/10)

#linear regression model
lm1 = lm(approval~gas10, data = approval)


#scatter plot
approval %>% 
  ggplot(aes(x=gas, y=approval)) +
  geom_point() +
  geom_smooth(method='lm', se = F) +
  labs(x= "gas prices (in dollars)", y = "approval rating", 
       title = "Association between gas prices and approval ratings")
lm1


#confidence interval
confint(lm1)

#proportion test (model fit statisic)
rsquared(lm1)
sigma(lm1)

#Signifcance
confint(lm1)


#The Bechdtal Test
#Part A

ggplot(films) + 
  geom_histogram(aes(x=votes)) + 
  facet_wrap(~test, nrow=2) +
  labs(x= " number of votes", 
       title = " number of votes based on bechdal test")
 
#sample mean estimates
#NEEDS HELP
#You have to compare the two means of the average number of votes for fail 
#and average number of votes for pass
newFilms = films %>% 
summarize(meanVotes = mean(votes~test))
#ya i don't know



#95% large sample inference
t.test(votes~test, data = films)

#Part B
#Bar plot
ggplot(films) +
  geom_bar(aes(x = test)) +
  facet_wrap(~R_rated) +
  labs(x= "Bechdal Test Results", 
       title = " Rate of bechdal test results based on film rating")
#Plot Caption
xtabs(~test, data = films) %>% 
prop.table()
prop(~test, data = films)
 #fail test: .5578

prop(test~R_rated, data = films)
#R Rated films that fail the test is .57247
#Not R-rated films that failed the test is .5461

#confidence interval
prop.test(test~R_rated, data = films)

#Manufacturing flaws in circuit boards
# Slide 1

#Regression Model
lm(Skips~Solder + Size, data = ATT)
  lm2 = lm(Skips~Solder + Size, data = ATT)


get_regression_table(lm2)

get_regression_table(lm2) %>% 
  select(term, estimate, lower_ci, upper_ci)

#Slide 2

#boxplots
ggplot(ATT) + 
  geom_boxplot(aes(x = Size, y = Skips))+
  labs(title = "Relationship of Skips based on Alloy Size")


#Density Histogram
ggplot(ATT) + 
  geom_histogram(aes(x=Skips, y=..density..), binwidth=3) 
  + facet_wrap(~Solder)



