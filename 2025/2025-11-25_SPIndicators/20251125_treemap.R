#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# 🌍 REGION-FOCUSED: Treemap with vivid regions, subtle country labels
#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

library(treemapify)
library(ggplot2)
library(dplyr)
library(readr)
library(MetBrewer)
library(ggtext)
library(showtext)
library(sysfonts)

# Font setup
font_add(family = "fa-brands", regular = "fa-brands-400.ttf")
font_add_google(name = "Merriweather", family = "merriweather")
font_add_google(name = "Roboto", family = "roboto")
font_add_google(name = "Roboto Condensed", family = "roboto-condensed")
showtext_auto()

# Social caption
social_caption <- paste0(
  "<span style='font-size:8pt;'><strong>Source:</strong> #TidyTuesday - Social Progress Index<br/>",
  "<strong>Graphic:</strong> ",
  "<span style='font-family:\"fa-brands\";'>&#xf09b;</span> loukesio ",
  "<span style='font-family:\"fa-brands\";'>&#xf08c;</span> Loukas Theodosiou ",
  "<span style='font-family:\"fa-brands\";'>&#xe671;</span> @bioinformatician.bsky.social</span>"
)

# Load data
spi_indicators <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-25/spi_indicators.csv')


spi_2023 <- spi_indicators %>%
  filter(year == 2023) %>%
  filter(!is.na(population) & population > 0) %>%
  # Only label larger countries to avoid clutter
  mutate(
    label_display = if_else(population > 15000000, iso3c, "")
  )

# Enhanced color palette
region_colors <- met.brewer("Hiroshige", n = length(unique(spi_2023$region)))


#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# VERSION 1: Vivid regions with faded country codes (RECOMMENDED)
#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

plot_region_focus <- ggplot(
  spi_2023,
  aes(
    area = population,
    fill = region,
    label = label_display,
    subgroup = region
  )
) +
  # Main treemap tiles with high alpha for vivid colors
  geom_treemap(color = "white", size = 2, alpha = 0.95) +

  # Thick region borders for strong definition
  geom_treemap_subgroup_border(color = "grey5", size = 5) +

  # COUNTRY CODES - Small, subtle, fading into background
  geom_treemap_text(
    colour = "white",
    place = "centre",
    grow = TRUE,
    reflow = TRUE,
    family = "roboto-condensed",
    fontface = "plain",  # Not bold - more subtle
    alpha = 0.25,  # Very transparent - fading effect
    min.size = 6
  ) +

  # REGION LABELS - Large, bold, vivid - THE MAIN FOCUS
  geom_treemap_subgroup_text(
    place = "centre",
    grow = FALSE,
    alpha = 0.95,  # Nearly opaque - vivid and clear
    colour = "#333333",
    fontface = "bold",
    family = "roboto",
    size = 16,  # Much larger than country codes
    min.size = 8
  ) +

  scale_fill_manual(values = region_colors, name = "Region") +

  labs(
    title = "Global Population Distribution by Region (2023)",
    subtitle = "Regional groupings emphasized | Individual countries shown in background | Rectangle size proportional to population",
    caption = social_caption
  ) +

  theme_minimal(base_family = "merriweather") +
  theme(
    plot.title = element_text(
      hjust = 0.5, face = "bold", size = 20,
      margin = margin(b = 8, t = 12)
    ),
    plot.subtitle = element_text(
      hjust = 0.5, size = 11, color = "grey35",
      margin = margin(b = 16, t = 5), lineheight = 1.2
    ),
    plot.caption = element_textbox_simple(
      lineheight = 1.3, padding = margin(t = 10),
      margin = margin(t = 16), halign = 0, size = 8
    ),
    plot.caption.position = "plot",
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.9, "cm"),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(18, 18, 18, 18)
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

plot_region_focus

ggsave("region_focus_treemap.png", plot_region_focus,
       width = 16, height = 12, dpi = 320, bg = "white")

print("✅ Region-focused treemap created!")


#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# VERSION 2: Even more subtle countries (ultra-minimal background)
#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

spi_2023_minimal <- spi_2023 %>%
  mutate(
    # Only show very large countries
    label_minimal = if_else(population > 30000000, iso3c, "")
  )

plot_ultra_minimal <- ggplot(
  spi_2023_minimal,
  aes(
    area = population,
    fill = region,
    label = label_minimal,
    subgroup = region
  )
) +
  geom_treemap(color = "white", size = 2, alpha = 0.95) +
  geom_treemap_subgroup_border(color = "grey5", size = 5) +

  # Country codes - extremely faded
  geom_treemap_text(
    colour = "white",
    place = "centre",
    grow = TRUE,
    reflow = TRUE,
    family = "roboto-condensed",
    fontface = "plain",
    alpha = 0.15,  # Even more transparent
    min.size = 7
  ) +

  # Region labels - dominant
  geom_treemap_subgroup_text(
    place = "centre",
    grow = FALSE,
    alpha = 1.0,  # Fully opaque
    colour = "white",
    fontface = "bold",
    family = "roboto",
    size = 18,  # Even larger
    min.size = 8
  ) +

  scale_fill_manual(values = region_colors, name = "Region") +

  labs(
    title = "Global Population by Region (2023)",
    subtitle = "Regional distribution emphasized | Major countries (>30M) subtly indicated",
    caption = social_caption
  ) +

  theme_minimal(base_family = "merriweather") +
  theme(
    plot.title = element_text(
      hjust = 0.5, face = "bold", size = 20,
      margin = margin(b = 8, t = 12)
    ),
    plot.subtitle = element_text(
      hjust = 0.5, size = 11, color = "grey35",
      margin = margin(b = 16, t = 5)
    ),
    plot.caption = element_textbox_simple(
      lineheight = 1.3, padding = margin(t = 10),
      margin = margin(t = 16), halign = 0, size = 8
    ),
    plot.caption.position = "plot",
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.9, "cm"),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(18, 18, 18, 18)
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

plot_ultra_minimal

ggsave("ultra_minimal_treemap.png", plot_ultra_minimal,
       width = 16, height = 12, dpi = 320, bg = "white")

print("✅ Ultra-minimal treemap created!")


#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# VERSION 3: No country labels at all - pure region focus
#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

plot_pure_regions <- ggplot(
  spi_2023,
  aes(
    area = population,
    fill = region,
    subgroup = region
  )
) +
  geom_treemap(color = "white", size = 1.8, alpha = 0.95) +
  geom_treemap_subgroup_border(color = "grey5", size = 6) +

  # Only region labels - large and prominent
  geom_treemap_subgroup_text(
    place = "centre",
    grow = FALSE,
    alpha = 1.0,
    colour = "white",
    fontface = "bold",
    family = "roboto",
    size = 18,
    min.size = 8
  ) +

  scale_fill_manual(values = region_colors, name = "Region") +

  labs(
    title = "Global Population Distribution by Region (2023)",
    subtitle = "Each tile is a country | Size represents population | Color indicates region",
    caption = social_caption
  ) +

  theme_minimal(base_family = "merriweather") +
  theme(
    plot.title = element_text(
      hjust = 0.5, face = "bold", size = 20,
      margin = margin(b = 8, t = 12)
    ),
    plot.subtitle = element_text(
      hjust = 0.5, size = 11, color = "grey35",
      margin = margin(b = 16, t = 5)
    ),
    plot.caption = element_textbox_simple(
      lineheight = 1.3, padding = margin(t = 10),
      margin = margin(t = 16), halign = 0, size = 8
    ),
    plot.caption.position = "plot",
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.9, "cm"),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(18, 18, 18, 18)
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

plot_pure_regions

ggsave("pure_regions_treemap.png", plot_pure_regions,
       width = 16, height = 12, dpi = 320, bg = "white")

print("✅ Pure regions treemap created!")


#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# VERSION 4: Balanced - small subtle countries, vivid regions
#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

spi_2023_balanced <- spi_2023 %>%
  mutate(
    label_balanced = if_else(population > 20000000, iso3c, "")
  )

plot_balanced <- ggplot(
  spi_2023_balanced,
  aes(
    area = population,
    fill = region,
    label = label_balanced,
    subgroup = region
  )
) +
  geom_treemap(color = "white", size = 2, alpha = 0.95) +
  geom_treemap_subgroup_border(color = "grey5", size = 5) +

  # Country codes - small, subtle
  geom_treemap_text(
    colour = "white",
    place = "centre",
    grow = FALSE,  # Fixed small size
    family = "roboto-condensed",
    fontface = "plain",
    size = 7,  # Small fixed size
    alpha = 0.3  # Faded
  ) +

  # Region labels - vivid and bold
  geom_treemap_subgroup_text(
    place = "centre",
    grow = FALSE,
    alpha = 0.95,
    colour = "white",
    fontface = "bold",
    family = "roboto",
    size = 17,
    min.size = 8
  ) +

  scale_fill_manual(values = region_colors, name = "Region") +

  labs(
    title = "Global Population Distribution by Region (2023)",
    subtitle = "Regions highlighted | Major countries (>20M) labeled subtly in background",
    caption = social_caption
  ) +

  theme_minimal(base_family = "merriweather") +
  theme(
    plot.title = element_text(
      hjust = 0.5, face = "bold", size = 20,
      margin = margin(b = 8, t = 12)
    ),
    plot.subtitle = element_text(
      hjust = 0.5, size = 11, color = "grey35",
      margin = margin(b = 16, t = 5)
    ),
    plot.caption = element_textbox_simple(
      lineheight = 1.3, padding = margin(t = 10),
      margin = margin(t = 16), halign = 0, size = 8
    ),
    plot.caption.position = "plot",
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.9, "cm"),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(18, 18, 18, 18)
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

plot_balanced

ggsave("balanced_treemap.png", plot_balanced,
       width = 16, height = 12, dpi = 320, bg = "white")

print("✅ Balanced treemap created!")

print("\n📊 Four region-focused versions created:")
print("1. region_focus_treemap.png - Vivid regions, faded countries (alpha=0.25)")
print("2. ultra_minimal_treemap.png - Even more subtle countries (alpha=0.15)")
print("3. pure_regions_treemap.png - No country labels at all (cleanest)")
print("4. balanced_treemap.png - Small fixed-size country labels (size=7)")
print("\n💡 Key changes: Region labels size 16-18, bold, alpha 0.95-1.0")
print("   Country labels size 7, plain font, alpha 0.15-0.3 (fading)")
