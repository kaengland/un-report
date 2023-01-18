library(tidyverse)
library(ggprism)

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
# Usually this is just for sharing a table//readability..not so much for data analysis
gapminder_data%>%
  select(country, continent, year, lifeExp)%>%
  pivot_wider(names_from = year, values_from = lifeExp)%>%
  View()



#Working with messy data
co2_emissions_dirty <- read_csv("data/co2-un-data.csv", skip = 2, #skips the first row, which seems to just be the title for the excel table, and the  column title row
         col_names = c("region", "country", "year", "series", "value", "footnotes", "source")) # create new column names
co2_emissions <- co2_emissions_dirty%>%
  select(country, year, series, value)%>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)"="per_capita_emissions"))%>%    #recode(column, if this, then replace with this)
  pivot_wider(names_from=series, values_from=value)%>%
  filter(year==2005)%>% # filter for year 2005
  select(-year) # select to remove the year column

# Bringing in 2007 population data
gapminder_data_2007<-read_csv("data/gapminder_data.csv")%>%
  filter(year==2007)%>%
  select(country, pop, lifeExp, gdpPercap)

# Let's join the tables based on country name
# inner join discards data if there are not matching rows
inner_join(co2_emissions, gapminder_data_2007, by = "country")

#anti-join displays rows that are not shared
#look at first data table, looking for rows that share values with the second data table, and KEEPS the FIRST data table values
anti_join(co2_emissions, gapminder_data_2007)

#full join-- combines tables and puts NA where data doesn't exist
full_join(co2_emissions, gapminder_data_2007)

co2_emissions%>%
  left_join(gapminder_data_2007)

joined_co2_pop <- inner_join(co2_emissions, gapminder_data_2007)

#Writing a CSV
write_csv(joined_co2_pop, file = "data/joined_co2_pop.csv")

# read in CSV
joined_pop_co2 <- read_csv("data/joined_co2_pop.csv")

# gdpPercap histogram and LifeExp histogram
joined_pop_co2%>%
  ggplot(aes(x=gdpPercap))+  ##aes inside of ggplot() function is more common
  geom_histogram()

gdp_co2_plot<-joined_pop_co2%>%
  ggplot(aes(x=gdpPercap, y=per_capita_emissions))+
  geom_point()+
  geom_smooth(method = 'lm', se = FALSE)+ #'lm' is linear and se = FALSE is SE
  labs(x = "GDP per Capita", y = "CO2 Emissions per Capita (metric tons)", title = "Comparing per capita CO2 emissions and GDP") +
  theme_classic() +
  ggpubr::stat_regline_equation(aes(label = after_stat(rr.label))) # Gets us the R^2 value
  
ggsave(gdp_co2_plot, filename = 'figures/gdp_vs_co2_plot.png',
       height=4, width=6, units = "in", dpi=300)

install.packages("ggpubr")



