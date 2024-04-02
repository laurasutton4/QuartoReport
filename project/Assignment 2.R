install.packages("tidyverse")
library(tidyverse)
install.packages("plotly")
library(plotly)

#Data
unicef_indicator_1 <- read_csv("unicef_indicator_1.csv")
unicef_metadata <- read_csv("unicef_metadata.csv")

#rename
metadata <- unicef_metadata %>%
  rename(population = `Population, total`,
         LifeExp = `Life expectancy at birth, total (years)`,
         gdpPercap = `GDP per capita (constant 2015 US$)`)

#Filter and join
metadata_2018 <- filter(metadata, year == 2018)
data_join_2011 <- filter(data_join, time_period == 2011)


data_join <- full_join(unicef_indicator_1, metadata, by = c("time_period" = "year", "country" = "country"))
data_join_2018 <- filter(data_join, time_period == 2018)
data_join_2018_countries <- filter(data_join_2018, 
                                   country %in% c("Cameroon", "Costa Rica", "Tunisia", "Zambia", "Gambia", 
                                                  "Kiribati", "Iraq", "Guinea", "Madagascar", "Nigeria", 
                                                  "Mali", "Mongolia", "Suriname", "Kyrgyzstan"))

data_join_2011 <- filter(data_join, 
                         time_period == 2011,
                         country %in% c("Mozambique", "Côte d'Ivoire", "Ivory Coast", "Honduras"))                                                  


data_join_countries2 <- filter(data_join, 
                               country %in% c("Cameroon", "Costa Rica", "Tunisia", "Zambia", "Gambia", 
                                              "Kiribati", "Iraq", "Guinea", "Madagascar", "Nigeria", 
                                              "Mali", "Mongolia", "Suriname", "Kyrgyzstan"))

map_world <- map_data("world")
1
options(scipen = 999)


# map 
map_join_2018 <- full_join(data_join_2018, map_world, by = c("country" = "region"))

ggplot(map_join_2018) +
  aes(x = long, y = lat, group = group, fill = population) +
  geom_polygon() +
  scale_fill_gradient(low = "orange", high = "pink", na.value = "grey") +
  labs(x = "Longitude",
       y = "Latitude",
       title = "The population of countries featuring child deprivation in 2018") +
  theme_classic() +
  theme(text = element_text(family = "serif"))

# timeseries1

timeseries_plot1 <- data_join_countries2 %>%
  ggplot() +
  aes(time_period, LifeExp, group = country, colour = country) +
  geom_line() +
  labs(x = "Year",
       y = "Life Expectancy",
       title = "Life Expectancy from 1960 to 2020") +
  theme_classic() +
  theme(
    text = element_text(family = "serif"),
  )

ggplotly(timeseries_plot1)


#timeseries2

timeseries_plot2 <- data_join_countries2 %>%
  ggplot() +
  aes(time_period, population, group = country, colour = country) +
  geom_line() +
  labs(x = "Year",
       y = "Population",
       title = "Population from 1960 to 2020") +
  theme_classic() +
  theme(
    text = element_text(family = "serif"),
  )
ggplotly(timeseries_plot2)

# scatterplot

scatterplot <- ggplot(data_join_2018_countries) +
  aes(LifeExp, obs_value, color = country, size = population ) +
  geom_point(alpha = 0.5) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE) +
  labs(x = "Life expectancy at birth",
       y = "% of children suffering at least one deprivation",
       title = "Life Expectancy V % of children suffering deprivation (2018)") +
  theme_classic() +
  theme(text = element_text(family = "serif")) +
  guides(size = FALSE)

ggplotly(scatterplot)

# bar chart

barchart <- data_join_2018_countries %>%
  group_by(country) %>%
  summarise(m_obsvalue = mean(obs_value, na.rm = TRUE)) %>%
  ggplot() +
  aes(reorder(country, m_obsvalue), m_obsvalue, fill = country) +
  geom_col() +
  labs(x = "country",
       y = "Avg. % of children suffering at least one deprivation",
       title = "Average rate of child deprivation in 2018") +
  theme_classic() +
  theme(
    text = element_text(family = "serif"),
    axis.text.x = element_blank()
  )

ggplotly(barchart)

#barchart 2

barchart2 <- data_join_2011 %>%
  group_by(country) %>%
  summarise(m_obsvalue = mean(obs_value, na.rm = TRUE)) %>%
  ggplot() +
  aes(reorder(country, m_obsvalue), m_obsvalue, fill = country) +
  geom_col() +
  labs(x = "",
       y = "Avg. % of children suffering at least one deprivation",
       title = "Average rate of child deprivation in 2011") +
  theme_classic() +
  theme(
    text = element_text(family = "serif"),
    axis.text.x = element_blank()
  ) +
  scale_fill_manual(values = c("pink", "orange","yellow", "grey"))

ggplotly(barchart2)

#colours

custom_colors <- c("#FFDAB9", "#FF7F50", "#FA8072", "#FFD700", "#FF007F", "#E6E6FA", "#FDD5B1", "#87CEEB", "#E0B0FF", "#FFBF00", "#DC143C", "#40E0D0", "#DA70D6", "#C8A2C8")

barchart <- data_join_2018_countries %>%
  group_by(country) %>%
  summarise(m_obsvalue = mean(obs_value, na.rm = TRUE)) %>%
  ggplot() +
  aes(reorder(country, m_obsvalue), m_obsvalue, fill = country) +
  geom_col(position = position_dodge(width = 0.1))
  labs(x = "country",
       y = "Avg. % of children suffering at least one deprivation",
       title = "Average rate of child deprivation in 2018") +
  theme_classic() +
  theme(
    text = element_text(family = "serif"),
    axis.text.x = element_blank()
  ) +
  scale_fill_manual(values = custom_colors)

ggplotly(barchart)

#pastel

pastel_colors <- c("#FFDAB9", "#FFA07A", "#FFC0CB", "#87CEEB", "#98FB98", "#FFB6C1", "#FFD700", "#FF69B4", "#ADD8E6", "#FF6347", "#B0E0E6", "#FFE4B5", "#FFDAB9", "#C0C0C0")

barchart <- data_join_2018_countries %>%
  group_by(country) %>%
  summarise(m_obsvalue = mean(obs_value, na.rm = TRUE)) %>%
  ggplot() +
  aes(reorder(country, m_obsvalue), m_obsvalue, fill = country) +
  geom_col() +
  labs(x = "Country",
       y = "Avg. % of Children Suffering at Least One Deprivation",
       title = "Average Rate of Child Deprivation in 2018") +
  theme_classic() +
  theme(
    text = element_text(family = "serif"),
    axis.text.x = element_blank()
  ) +
  scale_fill_manual(values = pastel_colors)

ggplotly(barchart)

# Define pastel colors
pastel_colors <- c("#FFDAB9", "#FFA07A", "#FFC0CB", "#87CEEB", "#98FB98", "#FFB6C1", "#FFD700", "#FF69B4", "#ADD8E6", "#FF6347", "#B0E0E6", "#FFE4B5", "#FFDAB9", "#C0C0C0")

barchart <- data_join_2018_countries %>%
  group_by(country) %>%
  summarise(m_obsvalue = mean(obs_value, na.rm = TRUE)) %>%
  ggplot() +
  aes(reorder(country, m_obsvalue), m_obsvalue, fill = country) +
  geom_col() +
  labs(x = "Country",
       y = "Avg. % of Children Suffering at Least One Deprivation",
       title = "Average Rate of Child Deprivation in 2018") +
  theme_classic() +
  theme(
    text = element_text(family = "serif"),
    axis.text.x = element_blank()
  ) +
  scale_fill_manual(values = pastel_colors)

ggplotly(barchart)

# Define pastel colors
pastel_colors <- c("#FFDAB9", "#FFA07A", "#FFC0CB", "#87CEEB", "#98FB98", "#FFB6C1", "#FFD700", "#FF69B4", "#ADD8E6", "#FF6347", "#B0E0E6", "#FFE4B5", "#FFDAB9", "#C0C0C0")

barchart <- data_join_2018_countries %>%
  group_by(country) %>%
  summarise(m_obsvalue = mean(obs_value, na.rm = TRUE)) %>%
  ggplot() +
  aes(reorder(country, m_obsvalue), m_obsvalue, fill = country) +
  geom_col() +
  labs(x = "Country",
       y = "Avg. % of Children Suffering at Least One Deprivation",
       title = "Average Rate of Child Deprivation in 2018") +
  theme_classic() +
  theme(
    text = element_text(family = "serif"),
    axis.text.x = element_blank()
  ) +
  scale_fill_manual(values = pastel_colors)

ggplotly(barchart)
