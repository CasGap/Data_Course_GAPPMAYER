library(tidyverse)

#I.
# Read the cleaned_covid_data.csv file into an R data frame.
dat <- read.csv("cleaned_covid_data.csv")

#II.
# Use grepl() function to search for matches to a pattern (states that begin with the letter "A") within each element of a character vector
class(dat$Province_State) # character
grepl("^A",dat$Province_State,ignore.case = FALSE, perl = FALSE,
      fixed = FALSE, useBytes = FALSE) # ^A is the character string must be enclosed in quotes. ^ is an operator that means first letter but might not actually matter because only first letters are capitalized anyway.

# Subset the data set to just show states that begin with "A" and save this as an object called A_states.
class(dat)
A_states <- subset.data.frame(dat,grepl("^A",dat$Province_State,ignore.case = FALSE, perl = FALSE,
                             fixed = FALSE, useBytes = FALSE),drop=FALSE) # drop everything that isn't true in grepl() function output.

#III.
# Create a plot _of that subset_ showing Deaths over time, with a separate facet for each state.
ggplot(A_states, aes(x = Last_Update, 
                     y = Deaths)) +
  geom_point() + # Create a scatterplot
  geom_smooth(se = FALSE) + # Add loess curves WITHOUT standard error shading
  facet_wrap(~A_states$Province_State, scales = "free") + # Keep scales "free" in each facet
  labs(x="Time")

#IV.
# Back to the full dataset
# Find the "peak" of Case_Fatality_Ratio for each state. Save this as a new data frame object called state_max_fatality_rate.
state_max_fatality_rate <- aggregate(.~dat$Province_State, dat, FUN=max)

# Rename the Case_Fatality_Ratio column to Maximum_Fatality_Ratio.
colnames(state_max_fatality_rate)[colnames(state_max_fatality_rate) == "Case_Fatality_Ratio"] ="Maximum_Fatality_Ratio"

# Only include the "Province_State" & "Maximum_Fatality_Ratio" columns.
state_max_fatality_rate <- select(state_max_fatality_rate,Province_State,Maximum_Fatality_Ratio)

# Arrange the new data frame in descending order by Maximum_Fatality_Ratio.
state_max_fatality_rate <- state_max_fatality_rate[order(state_max_fatality_rate$Maximum_Fatality_Ratio,decreasing = TRUE),]

#V.
# Make Province_State column in the new data frame a factor.
state_max_fatality_rate$Province_State <- as.factor(state_max_fatality_rate$Province_State)

# Use that new data frame from task IV to create another plot.
state_max_fatality_rate %>%
  mutate(Province_State = fct_reorder(state_max_fatality_rate$Province_State, desc(state_max_fatality_rate$Maximum_Fatality_Ratio))) %>%
  ggplot(aes(state_max_fatality_rate, x=Province_State, y=Maximum_Fatality_Ratio)) +
  geom_col() + #bar plot with y column as values instead of count
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))

#VI.
# Using the FULL data set, plot cumulative deaths for the entire US over time.

#?group_by(dat,)
#?summarise()
# You'll need to read ahead a bit and use the dplyr package functions group_by() and summarize() to accomplish this.
  
  
  
  
  