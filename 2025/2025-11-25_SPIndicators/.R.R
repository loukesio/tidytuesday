library(readr)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)

spi_indicators <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-25/spi_indicators.csv')


world <- ne_countries(scale = "medium", returnclass = "sf")

df_map <- world %>%
  left_join(spi_indicators, by = c("iso_a3_eh" = "iso3c"))

ggplot(df_map) +
  geom_sf(aes(fill = overall_score), size = 0.2, color = "white") +
  scale_fill_viridis_c(option = "viridis", na.value = "gray90") +
  labs(title = "Overall Score by Country",
       fill = "SPI Score") +
  theme_minimal()




install.packages("treemap")   # if not installed

library(readr)
library(dplyr)
library(treemap)

spi_indicators <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-25/spi_indicators.csv"
)

# keep one year (e.g. 2023)
spi_2023 <- spi_indicators %>%
  filter(year == 2023)

treemap(
  spi_2023,
  index   = c("region", "country"),  # hierarchy: region > country
  vSize   = "population",            # area of the rectangles
  vColor  = "region",                # color by region
  type    = "categorical",           # use vColor as categories
  title   = "Countries treemap by region (area = population)"
)




install.packages("treemapify")   # if not installed

library(treemapify)
library(ggplot2)
library(dplyr)
library(readr)

spi_indicators <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-25/spi_indicators.csv"
)

spi_2023 <- spi_indicators %>%
  filter(year == 2023)

ggplot(spi_2023,
       aes(area = population,
           fill = region,
           label = iso3c,
           subgroup = region)) +
  geom_treemap(color = "white") +
  geom_treemap_subgroup_border(color = "black") +
  geom_treemap_text(colour = "white", place = "centre", grow = TRUE) +
  labs(title = "Treemap of countries by population",
       subtitle = "Color = region, area = population") +
  theme_minimal()

