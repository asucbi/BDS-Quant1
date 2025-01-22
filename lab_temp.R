library(dslabs); library(tidyverse)

data("gapminder", package = "dslabs") 

gapminder_2015 <- gapminder %>% 
  filter(year == 2015, !is.na(infant_mortality))

ggplot(gapminder_2015) +
  geom_histogram(aes(x = life_expectancy), color = "black")  +
  xlab("Infant mortality per 1,000 live births") +
  ylab("Number of countries")

library(infer)
library(cowplot)

sample_5 <- rep_sample_n(gapminder_2015, size = 100, reps = 10000) %>% 
  group_by(replicate) %>% 
  summarise(mean_infant_mortality = mean(life_expectancy)) %>% 
  mutate(n = 5)

sample_30 <- rep_sample_n(gapminder_2015, size = 130, reps = 10000) %>% 
  group_by(replicate) %>% 
  summarise(mean_infant_mortality = mean(life_expectancy)) %>% 
  mutate(n = 30)

sample_100 <- rep_sample_n(gapminder_2015, size = 150, reps = 10000) %>% 
  group_by(replicate) %>% 
  summarise(mean_infant_mortality = mean(life_expectancy)) %>% 
  mutate(n = 100)

all_samples <- bind_rows(sample_5, sample_30, sample_100)

ggplot(all_samples) +
  geom_histogram(aes(x = mean_infant_mortality), color = "black")  +
  xlab("Infant mortality per 1,000 live births") +
  ylab("Number of countries") +
  xlim(c(60, 80)) +
  facet_wrap(~n, ncol = 1)

df <- tibble(rlnorm(2000))
  
ggplot(df) +
  geom_histogram(aes(x = value), color = "black")  +
  xlab("Value") +
  ylab("Frequency")

# First, plot the data -- what can you say about the distribution of the data?

sample_250 <- rep_sample_n(df, size = 250, reps = 10000) %>% 
  group_by(replicate) %>% 
  summarise(mean_value = mean(value)) %>% 
  mutate(n = 250)

sample_500 <- rep_sample_n(df, size = 500, reps = 10000) %>% 
  group_by(replicate) %>% 
  summarise(mean_value = mean(value)) %>% 
  mutate(n = 500)

sample_1000 <- rep_sample_n(df, size = 1000, reps = 10000) %>% 
  group_by(replicate) %>% 
  summarise(mean_value = mean(value)) %>% 
  mutate(n = 1000)

all_samples_large <- bind_rows(sample_250, sample_500, sample_1000)

ggplot(all_samples_large) +
  geom_histogram(aes(x = mean_value), color = "black")  +
  xlab("Value") +
  ylab("Frequency") +
  facet_wrap(~n, ncol = 1)

# How do these distributions compare to the original distribution? What is this an illustration of?
# Are the HW distributions more or less similar than the samples of 5, 30, and 100 from lab? Why do you think this is the case?


# NOTE TO SELF: MAKE THE LAB NUMBERS MORE SIMILAR AND THEN MAKE THE HOMEWORK NUMBERS LARGER NUMBERS OVERALL BUT THE SAME DIFFERENCE BETWEEN THEM 
# Lab: do 10, 30, 50

# Quiz Q1: Law of Large numbers says that what is true? [CHANGE THIS ONE?]
# Quiz Q2: The Central Limit Theorem states that as your sample size increases, the sampling distribution of the mean approaches a ________ distribution.
# Quiz Q3: As sample size increase, the SEM: increases, decreases, stays the same, disappears altogether
# Quiz Q4: What is the correct interpretation of a confidence interval? [add choices]
# Quiz Q5: The p-value tells us the probability of getting a value at least as extreme as our observed sample mean if __________ is true.
# BONUS: What is your favorite animal?


sample_5 <- rep_sample_n(df3, size = 5, reps = 10000) %>% 
  group_by(replicate) %>% 
  summarise(mean_dt = mean(rawDT)) %>% 
  mutate(n = 5)

sample_30 <- rep_sample_n(df3, size = 30, reps = 10000) %>% 
  group_by(replicate) %>% 
  summarise(mean_dt = mean(rawDT)) %>% 
  mutate(n = 30)

sample_100 <- rep_sample_n(df3, size = 100, reps = 10000) %>% 
  group_by(replicate) %>% 
  summarise(mean_dt = mean(rawDT)) %>% 
  mutate(n = 100)

all_samples <- bind_rows(sample_5, sample_30, sample_100)

ggplot(all_samples) +
  geom_histogram(aes(x = mean_dt), color = "black")  +
  xlab("Value") +
  ylab("Frequency") +
  facet_wrap(~n, ncol = 1)

