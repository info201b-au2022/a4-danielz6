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
           theme_minimal() + labs(title = "Jail Population Distribution in the U.S. (1970-2018)", x = "Year", y = "Total Jail Population")
           )
}
## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
race_in_jail <- function() {
  df3 <- data.frame(AAPI_Population = sum(df$aapi_pop_15to64, na.rm = TRUE),
                    black_population = sum(df$black_pop_15to64, na.rm = TRUE),
                    latinx_population = sum(df$latinx_pop_15to64, na.rm = TRUE),
                    native_population = sum(df$native_pop_15to64, na.rm = TRUE),
                    white_population = sum(df$white_pop_15to64, na.rm = TRUE))
  return(df3)
}
View(race_in_jail())
race_plot <- function() {
  scatterplot <- ggplot(race_in_jail(), aes(x = ))
}
## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


