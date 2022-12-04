library(tidyverse)
library("ggplot2")
library("dplyr")
# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}
df <- read.csv("~/Documents/info201/data/incarceration_trends.csv")
total_jail_pop <- get_data()
View(total_jail_pop)
## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#
max_male_pop <- df %>%
  filter(male_pop_15to64 == max(male_pop_15to64)) %>%
  pull(male_pop_15to64)
max_female_pop <- df %>%
  filter(female_pop_15to64 == max(female_pop_15to64)) %>%
  pull(female_pop_15to64)
male_female_difference <- abs(max_male_pop - max_female_pop)
  
  
## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
  total_pop <- df %>%
    group_by(year) %>%
    filter(total_jail_pop != "NA") %>%
    summarise(total_jail_pop = sum(total_jail_pop))
    return(total_pop)   
}
get_year_jail_pop()
# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  ggplot(get_year_jail_pop(), aes(x = year, y = total_jail_pop)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "The Distribution of Jail Population in the U.S. (1970 - 2018)",
      x = "Year",
      y = "Total Jail Population")
  
} 
plot_jail_pop_for_us()
## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
get_jail_pop_by_states <- function(states) {
  df2 <- df %>% 
    filter(state %in% states) %>%
    group_by(state, year) %>%
    summarise(total_jail_pop = sum(total_jail_pop, na.rm = TRUE))
  return(df2)
}
plot_jail_pop_by_states <- function(states) {
  x <- length(states) 
  print(x)
  if(x < 3) {
    return("Not enough states")
  }
  if (x > 10) {
    return("Too many states")
  }
  return(ggplot(get_jail_pop_by_states(states), aes(x = year, y = total_jail_pop, colour = state)) +
         geom_point(size = 0.8, alpha = 0.09) +
         geom_smooth(size = 2) +
           theme_minimal() + labs(title = "Jail Population Distribution in the U.S. By State (1970-2018)", x = "Year", y = "Total Jail Population")
           )
}
plot_jail_pop_by_states(c("WA", "CA", "NY", "NV"))
## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
race_in_jail <- function() {
  df3 <- data.frame(black_population = df$black_pop_15to64,
                    white_population = df$white_pop_15to64)
  return(df3)
}
View(race_in_jail())
race_plot <- function() {
  scatterplot <- ggplot(race_in_jail(), aes(x = black_population, y = white_population)) +
    geom_point() +
    ggtitle("Black versus White Jail Population(ages 15-64)") +
    labs(caption = "This plot shows the correlation between white and black population in the jail system.")
  return(scatterplot)
}


race_plot()
## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#


# Create a blank map of U.S. states
df4 <- data.frame("State" = df$state,
                  "black_pop" = df$black_jail_pop,
                  "Total_pop" = df$total_pop)

ratio_diff <- df4 %>% summarize(state = State, ratio = total_pop/total_pop)



map_total <- ratio_diff %>%
  group_by(state) %>%
  summarise(total_pop_per_state = mean(ratio, na.rm = TRUE))
map_total[is.na(map_total)] = 0
map_total[sapply(map_total, is.infinite)] <- 0

map_total2 <- map_total %>%
  mutate(full_name = tolower(state.name[match(map_total$state, state.abb)])) %>%
  rename(abbr = state) %>%
  rename(state = full_name)


state_shape <- map_data("state") %>%
  rename(state = region) %>%
  left_join(map_total2, by = "state")


plot_state <- function() {
  ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = total_pop_per_state),
    color = "white", # show state outlines
    size = .1        # thinly stroked
  ) +
  coord_map() +# use a map-based coordinate system
  scale_fill_continuous(low = "#D8D7D7", high = "#F0A8A8", limits = c(0,1)) +
    labs(fill = "Ratio",
         caption = "The population of total immates across America",
         x = "",
         y = "") +
    theme(plot.caption = element_text(hjust = 0.5)) +
    ggtitle("Distribution of total population of immates across America")
}
plot_state()


