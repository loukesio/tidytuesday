#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# 📦 Load Required Libraries
#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
library(palmerpenguins)   # Dataset: Palmer Archipelago (Antarctica) penguins
library(dplyr)            # Tidy data manipulation
library(ggplot2)          # Data visualization
library(ggforce)          # For annotated ellipses (geom_mark_ellipse)
library(MetBrewer)        # Elegant color palettes (e.g. "Hokusai1")
library(ggtext)           # Rich text rendering in ggplot2
library(sysfonts)         # Font handling
library(showtext)         # Render fonts in plots
library(glue)             # String interpolation

#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# 🖋 Register Fonts (One-time setup required)
#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

# Register FontAwesome Brands font (icon font); assumes the .ttf file is in your working directory
font_add(family = "fa-brands", regular = "fa-brands-400.ttf")

# Add a Google font for text (e.g. “Merriweather”)
font_add_google(name = "Merriweather", family = "merriweather")

# Activate the showtext rendering system for ggplot
showtext_auto()

#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# 💬 Create Caption with Social Media Links
#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

social_caption <- paste0(
  "<strong>Source:</strong> R datasets<br/>",
  "<strong>Graphic:</strong> ",
  "<span style='font-family:\"fa-brands\";'>&#xf09b;</span><span style='font-family:\"merriweather\";'>loukesio</span> ",
  "<span style='font-family:\"fa-brands\";'>&#xf08c;</span><span style='font-family:\"merriweather\";'>Loukas Theodosiou</span> ",
  "<span style='font-family:\"fa-brands\";'>&#xe671;</span><span style='font-family:\"merriweather\";'>@bioinformatician.bsky.social</span>"
)

#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# 🧼 Prepare and Clean Penguin Data for PCA
#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

penguins_clean <- penguins %>%
  select(species, island, bill_length_mm:body_mass_g) %>%
  na.omit()  # Remove rows with missing values

# Standardize numeric variables
pca_matrix <- scale(penguins_clean %>% select(-species, -island))

# Run Principal Component Analysis (PCA)
pca_result <- prcomp(pca_matrix, center = TRUE, scale. = FALSE)

# Extract first two principal components and reattach species/island
pca_df <- as.data.frame(pca_result$x[, 1:2]) %>%
  mutate(
    species = penguins_clean$species,
    island  = penguins_clean$island
  )

# Extract % variance explained by PC1 and PC2
var_exp <- summary(pca_result)$importance[2, 1:2] * 100

# Use MetBrewer palette
pal <- ltc("trio1", length(unique(pca_df$species)))
str(pal)
#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# 📊 Create PCA Plot with Caption
#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
tweak_subtitle <- "PCA for <span style='color:#0E7175'><i>Pygoscelis adeliae</i></span>,
<span style='color:#FD7901'><i>Pygoscelis chinstrap</i></span>, and
<span style='color:#C35BCA'><i>Pygoscelis gentoo</i></span> reveals that
<span style='color:#C35BCA'><i>P. gentoo</i></span> clusters distinctly compared to the other two species, which overlap overall."

plot.pca <- ggplot(pca_df, aes(PC1, PC2, fill = species)) +
  geom_point(size = 4, alpha = 0.75, shape = 21, color = "white") +
  geom_mark_ellipse(aes(label = species), color = NA) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.7) +
  scale_fill_manual(values = pal) +
  scale_x_continuous(limits = c(-4, 4)) +
  scale_y_continuous(limits = c(-4, 4)) +
  labs(
    title   = "PCA of Penguin Morphometrics by Species",
    x       = paste0("PC1 (", round(var_exp[1], 1), "%)"),
    y       = paste0("PC2 (", round(var_exp[2], 1), "%)"),
    caption = social_caption,
    subtitle = tweak_subtitle
  ) +
  theme_bw(base_family = "merriweather") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = ggtext::element_textbox_simple(
      hjust = 0.5,
      size = 9,
      lineheight = 0.5,
      margin = margin(b = 5)
    ),
    plot.caption.position = "plot",
    plot.caption = element_textbox_simple(
      lineheight = 1.4,
      padding    = margin(t = 2, l = 0, r = 0, b = 0),
      margin     = margin(t = 10, b = 5),
      halign     = 0  # left-align
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      linetype = "dashed",
      color = scales::alpha("#616569", 0.18)
    ),
    legend.position = "none"
  )

plot.pca


