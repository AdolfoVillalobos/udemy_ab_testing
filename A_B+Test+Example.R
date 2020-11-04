
############################################################################################################


################## Background - 

# Decco is an online retailer that sells home decor items. They recently added ‘Lamps’ 
# as a new category of products which they were not selling before. In order to generate 
# awareness and boost sales, they want to do a promotion through their App. Their 
# notifications have had good success in the past and they are considering to send a message 
# making users aware of this new category, through in-app notification. But atthe same time, 
# they want to be judicious about any features or releases when it comes to their app because 
# they know that LTV of a customer who has installed their app is much higher. They want to 
# be careful so as not to drive users to uninstall the app. 
# 
# Can they run an A/B test in this scenario?


############################## Hypothesis - 

# Because we have seen good success through  in-app notifications in the past, if we send an
# in-app notification with a promotional offer for Lamps, then the % of users that purchase 
# from the Lamp Category will increase. 


# Primary Metrics - Transaction Rate i.e. % of users that will make a purchase
# Secondary metrics are - Purchase Value
# Other metrics - Uninstall rate

############################################################################################################

############################### Sample Size Calculation 

install.packages("pwr")
library(pwr)


control = 0.101
uplift = .2
variant = (1 + uplift)*control
effect_size <- ES.h(control, variant)
sample_size_output <- pwr.p.test(h = effect_size,
                                 n = ,
                                 sig.level = 0.05,
                                 power = 0.8)

sample_size_output <- ceiling(sample_size_output$n)
sample_size_output #1896


#################################  Understanding the data

getwd()
setwd("~/Downloads")

df = read.csv("\\AB_Test_Data.csv")


str(df)

# Treatment indicator - allocation
# Response variables - addtocart_flag, transaction_flag, purchase_value
# Baseline variables - active_6m, days_since
# Other - uninstall_flag

# Numerical variable stats
summary(df[,c("active_6m", "addtocart_flag","transaction_flag", "uninstall_flag", 
              "purchase_value", "days_since")])

install.packages("ggplot2")
library(ggplot2)
library(dplyr)

df %>% ggplot(aes(x = purchase_value)) +  
  geom_histogram( color="#e9ecef", fill = "#E69F00", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(fill="")

df %>% ggplot(aes(x = purchase_value)) +  
  geom_density( color="#E69F00")

df %>% ggplot(aes(x = days_since)) +  
  geom_histogram( color="#e9ecef", fill = "#56B4E9", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(fill="")

df %>% ggplot(aes(x = days_since)) +  
  geom_density( color="#56B4E9")

# Categorical variable stats
table(df$allocation)
table(df$allocation)/nrow(df)

# ############################### Check for randomization - baseline variables

df %>% group_by(allocation) %>% summarise(mean(active_6m), mean(days_since))
# Group means are similar between groups


# Compare distribution across the two groups

df %>% ggplot(aes(x = days_since, fill = allocation)) +  
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(fill="")


################################ Treatment Effects

# Compare performance of response variables across the two groups
df %>% group_by(allocation) %>% summarise(mean(addtocart_flag), mean(transaction_flag), 
                                              mean(purchase_value, na.rm = TRUE))

prop.test(xtabs(~ allocation + transaction_flag, data = df)[,2:1])


# This is telling us that the treatment group performed between 7.8% and 6.9% better than the control group.
# i.e. if we were to repeat this test 20 times with 20 different samples, the difference between the
# performance of the two groups will lie somewhere between these two numbers, at least 19 times.
# i.e those differences are statistically significant.

prop.test(xtabs(~ allocation + addtocart_flag, data = df)[,2:1])


t.test(purchase_value ~ allocation, data = df)

#Checking for Uninstalls

df %>% group_by(allocation) %>% summarise(mean(uninstall_flag))


prop.test(xtabs(~ allocation + uninstall_flag, data = df)[,2:1])


# One limitation of these tests is that they can only handle tests with two variations. In that case you can either
# repeat the analysis using different combinations or pairs. You can also use Logistic or linear regression 



