library(tidyverse)

dtm <- read.csv("data_for_project.csv")

ggplot(dtm, aes(x=X,y=Family.Collocates)) +
  geom_col() +
  labs(title="Frequency of Neighboring Words", subtitle="of the word 'family'", y="Frequency", x="") +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
