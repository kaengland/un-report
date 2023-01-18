# Analyze life expectancy and CO2 emissions vs. population with gapminder
# Date:  20230117
# Author: Kevin England

# load in packages necessary for analysis
library("tidyverse")
library("readr")
library("ggprism")

#Read in data for analysis
gapminder_1997 <- read_csv("gapminder_1997.csv")

# Plotting data for vixualization

ggplot(data = gapminder_1997) +
  aes(x = gdpPercap) +
  labs(x = "GDP Per Capita") +
  aes(y = lifeExp) +
  labs(y = "Life Expectancy (yrs)") +
  geom_point() + # "Shape = 19" if you want to assign all the same shape
  labs(title = "Do people in wealthy countries live longer?") +
  aes(color = continent) +
  scale_color_brewer(palette = "Set1") +
  aes(size = pop/1000000) +
  labs(size = "Population (in millions)") +
  aes(shape = continent)

# Short handed ggplot
ggplot(data = gapminder_1997,
       aes(x = gdpPercap, y = lifeExp, color = continent, 
           shape = continent, size = pop)) + 
  labs (x = "GDP Per Capita", y = "Life Expectancy",
        title = "Do people in wealthy countries live longer?",
        size = "Population (in millions)") +
  geom_point()

# Read in all of the data from gapminder (more years than 1997!)
gapminder_data <- read_csv("gapminder_data.csv")

view(gapminder_data)
dim(gapminder_data)
head(gapminder_data)
tail(gapminder_data)

# Challenge:  Predicting the output
ggplot(data = gapminder_data) +
  aes(x=year, y=lifeExp, color=continent, group = country) +
  geom_line() #Had to add "group = country" to the previous line

# learn about data
str(gapminder_data) # Alternatively, can click the blue arrow in the environment panel

# Box plots
ggplot(data = gapminder_1997) +
  aes(x=continent, y=lifeExp) +
  geom_boxplot()

# Violin plot
ggplot(data = gapminder_1997) +
  aes(x=continent, y = lifeExp, color = continent) +
  geom_violin() + 
  geom_jitter(aes(size = pop)) # This adds points OVER TOP of the violin.  The aes() only applies to this geom

# Now, let's look at two continuous variables!  Histogram!

ggplot(gapminder_1997) +
  aes(x = lifeExp) + 
  geom_histogram(bins = 20) + # binwidth includes 20 units on the X-axis, bins is number of bins
  theme_prism() # Many different themes to explore ggtheme package, ggprism is like prism


ggplot(gapminder_1997) + 
  aes(x = gdpPercap, y = lifeExp) +
  geom_point() +
  facet_grid(rows = vars(continent))
  #facet_wrap(vars(continent)) Chooses its own dimmesions for the 2D grid

# Saves the most recent plot
ggsave("figures/awesome_plot.jpg", device = "jpg", width = 6, height = 4) # Can also save by plots tab -> export -> save as image...

