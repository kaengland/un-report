library(tidyverse)
getwd()

gapminder_data <- read_csv("data/gapminder_data.csv")

summarize(gapminder_data, averageLifeExp = mean(lifeExp), medianLifeExp=median(lifeExp)) #can take many arugments

# Learning to pipe
# Assumes first arugment in function is what is piped to it
gapminder_summary <- gapminder_data%>%
  summarize(averageLifeExp=mean(lifeExp))

gapminder_summary

# filtering-- filters out certain rows from the frame
gapminder_summary_2007<-gapminder_data %>%
  filter(year == 2007)%>%
  summarize(average = mean(lifeExp))

# finding average gdpPerCap for the first year in the dataset
gapminder_data%>%
  summarize(firstYear=min(year)) #tells me the first year in data set

gapminder_data%>%
  filter(year==min(year))%>%
  summarize(Average_GDP = mean(gdpPercap)) 

# Using group_by()
gapminder_data%>%
  group_by(year, continent)%>%
  summarize(average = mean(lifeExp),
            error = sd(lifeExp))

# Mutate function-- creates a new column, row by row
gapminder_data%>%
  mutate(gdp = pop * gdpPercap)

# Mutate a new column which is population in millions
gapminder_data%>%
  mutate(popInMillions = pop/1000000)

# Select function-- selects certain columns from the frame
gapminder_data%>%
  select(pop, year)

gapminder_data%>%
  select(-continent) # REMOVES the column with the name

# Pivot_wider-- pivots data value to make it a column heading
gapminder_data%>%
  select(country, continent, year, lifeExp)%>%
  pivot_wider(names_from = year, values_from = lifeExp)%>%
  View()








