library(tidyverse)
dat <- read_csv("BioLog_Plate_Data.csv")

#1 Clean data into tidy (long) form
dat <- dat %>% 
  pivot_longer(
    cols = starts_with("Hr_"),
    names_to = "Time",
    names_prefix = "Hr_",
    values_to = "Absorbance",
    names_transform = as.numeric
  )

#2 Create a new column specifying whether a sample is from soil or water
dat <- dat %>% 
  mutate(Type = if_else(startsWith(dat$`Sample ID`, "Soil"), "Soil", "Water"))

#3 Generate a plot that matches the one in the instructions (just plotting dilution == 0.1)
library(ggplot2)
D_0.1 <- subset(dat, dat$Dilution == 0.1)
ggplot(D_0.1, aes(x=Time, y=Absorbance, color = Type)) + 
  geom_smooth(se = FALSE) +
  facet_wrap(~ Substrate) +
  theme_minimal() +
  labs(subtitle = "Just dilution 0.1")

#4 Generate an animated plot that matches the one in the instructions
library(gganimate)
IA <- subset(dat, dat$Substrate == "Itaconic Acid") #just showing values for the substrate “Itaconic Acid”
IA2 <- aggregate(IA$Absorbance, by = list(IA$Dilution, IA$`Sample ID`, IA$Time), FUN = mean) #absorbance values are mean of all 3 replicates for each group
IA2 <- IA2 %>% 
  rename("Dilution" = "Group.1",
         "Sample ID" = "Group.2",
         "Time" = "Group.3",
         "Mean_absorbance" = "x")

ggplot(IA2, aes(x=Time, y=Mean_absorbance, color=`Sample ID`)) +
  geom_line() +
  facet_wrap(~ Dilution) +
  transition_reveal(Time) +
  theme_minimal()
