library(tidyverse)
library(janitor)
library(skimr)

df <- read.csv("Utah_Religions_by_County.csv") %>% 
  clean_names()

skim(df)





#1. "Does population of a county correlate with the proportion of any specific religious group in that county?"

dat <- pivot_longer(data = df, cols = -c(1:4), names_to = "religion", values_to = "proportion")
# Makes the data frame so I can look at multiple religions at the same time much easier.

dat %>% 
  ggplot(aes(y=proportion,x=pop_2010)) + 
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(~religion,scales="free")
# I see that there are some lines with slope but large standard error.
# Does not seem like any specific religious group has a clear correlation to population size.
# It looks like most of the counties have population sizes less than 25,000. I can look at that.

df %>% 
  filter(pop_2010 < 25000) %>%
  pivot_longer(cols = -c(1:4), names_to = "religion", values_to = "proportion") %>% 
  ggplot(aes(y=proportion,x=pop_2010)) + 
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(~religion,scales="free")
# Looks like standard error is smaller. In general most religious groups have a positive slope.
# How about pop size greater than 25,000?

df %>% 
  filter(pop_2010 > 25000) %>%
pivot_longer(cols = -c(1:4), names_to = "religion", values_to = "proportion") %>% 
  ggplot(aes(y=proportion,x=pop_2010)) + 
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(~religion,scales="free")
# Larger standard error. Religions that have small proportions show up more in larger pop sizes.

dat %>% 
  ggplot(aes(x = reorder(county,pop_2010),y = proportion)) + 
  geom_point() +
  facet_wrap(~religion,scales="free") +
  theme(axis.text.x = element_blank())
# One dot for each county in order of increasing population
# Yep proportion seems pretty random except that religions that have zero proportion in most counties have more people identifying with the religion in the counties with larger pop size.
# There are certain counties that have seemingly significantly higher proportions of certain religions, but not in clear relation to population size.

filter(dat, dat$proportion < 0.006) %>% 
  ggplot(aes(x = reorder(county, pop_2010), y = log(pop_2010))) + 
  geom_col(aes(fill = proportion)) +
  scale_fill_continuous(type = "viridis") +
  facet_wrap(~religion) +
  theme(axis.text.x = element_blank())
# Looking at only the religions with very small proportion
# Each column represents a county in order of increasing population
# Can see that proportions are generally higher in higher populations (but only by a couple people for every thousand people)
# I used log(pop_2010) to see all the columns better, but it might be misleading because population size is very different between counties and this does not visually show that.

## Overall, I do not think that any specific religious group correlates with a certain county because of the county's population size.





#2. “Does proportion of any specific religion in a given county correlate with the proportion of non-religious people?”

dat %>% 
  ggplot(aes(y=proportion,x=non_religious)) + 
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(~religion,scales="free")
# It seems that the Latter-day Saint proportion decreases as the proportion of non-religious people increases
# A few religious groups might increase as the proportion of non-religious people increases (Evangelical and Episcopal Church perhaps)

dat %>% 
  ggplot(aes(x = reorder(county,non_religious),y = proportion)) + 
  geom_point() +
  facet_wrap(~religion,scales="free") +
  theme(axis.text.x = element_blank())
# Yep looks like a negative correlation with the Latter-day Saint proportion and non-religious proportion.
# Evangelical, Episcopal Church, and United Methodist Church do look like they could have a positive correlation.

df %>%
select(c(county,pop_2010,religious,non_religious,lds)) %>% 
  pivot_longer(cols = -c(1:4), names_to = "religion", values_to = "proportion") %>% 
  ggplot(aes(x = reorder(county, non_religious), y = proportion)) + 
  geom_col(aes(fill = proportion)) +
  scale_fill_continuous(type = "viridis") +
  facet_wrap(~religion,scales = "free") +
  theme(axis.text.x = element_blank())
# Similar graph with columns instead of dots for each county in order of increasing proportion of non-religious people

df %>%
  select(c(county,pop_2010,religious,non_religious,evangelical,episcopal_church,united_methodist_church)) %>% 
  pivot_longer(cols = -c(1:4), names_to = "religion", values_to = "proportion") %>% 
  ggplot(aes(x = reorder(county, non_religious), y = proportion)) + 
  geom_col(aes(fill = proportion)) +
  scale_fill_continuous(type = "viridis") +
  facet_wrap(~religion,scales = "free") +
  theme(axis.text.x = element_blank())
# Looks like there is some correlation between these three religions and an increase in proportion of non_religious people

## Clear negative correlation between Latter-day Saint proportion and non-religious proportion
## Less clear positive correlation between Evangelical, Episcopal Church, and United Methodist Church proportions and non-religious proportion

