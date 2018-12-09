
library (readr)
library (dplyr)
library (ggplot2)
# uploading data
forest <- read_csv ("forestfires.csv")

#factoring of months order during a year and days order during a week
forest <- forest %>%
  mutate(month = factor(month, levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")))
forest <- forest %>%
  mutate(day = factor(day, levels = c("sun", "mon", "tue", "wed", "thu", "fri", "sat")))

#grouping and summarising of fires by months and by days of a week
forest_number <- forest %>% 
  group_by (month, day) %>%
  summarise(Number = n())

#bar plotting of fires number per month
ggplot (data = forest_number) +
  aes (x = month, y = Number) +
  labs (title = "Forest fires number per month") +
  geom_bar(stat = "identity")
  
#bar plotting of fires number per day of week
  ggplot (data = forest_number) +
    aes (x = day, y = Number) +
    labs (title = "Forest fires number per day") +
    geom_bar(stat = "identity")

# new function to plot a boxplot from values of varios categories
create_boxplot <- function(x, y) {
  ggplot(data = forest) + 
    aes_string(x = x, y = y) +
    geom_boxplot() +
    theme(panel.background = element_rect(fill = "white"))
}

# per month [3]
x_month <- names(forest)[3]
y_var <- names(forest)[5:12]
map2(x_month, y_var, create_boxplot)

# per day of week [4]
x_day <- names (forest)[4]
y_var <- names(forest)[5:12]
map2(x_day, y_var, create_boxplot)

# new function to plot a scatterplot to define a dependance of fire areas from other categories
create_scatterplot <- function(x, y) {
  ggplot (data = forest) +
    aes_string (x = x, y = y) +
    ylim (1,300) +
    geom_point (alpha = 0.3)
}

x_var <- names (forest)[5:12]
y_var <- names (forest)[13]
map2(x_var, y_var, create_scatterplot)
