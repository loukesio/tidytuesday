#_____________________________________________________
#
# ▗▖   ▗▄▄▄▖▗▄▄▖ ▗▄▄▖  ▗▄▖ ▗▄▄▖ ▗▄▄▄▖▗▄▄▄▖ ▗▄▄▖
# ▐▌     █  ▐▌ ▐▌▐▌ ▐▌▐▌ ▐▌▐▌ ▐▌  █  ▐▌   ▐▌
# ▐▌     █  ▐▛▀▚▖▐▛▀▚▖▐▛▀▜▌▐▛▀▚▖  █  ▐▛▀▀▘ ▝▀▚▖
# ▐▙▄▄▖▗▄█▄▖▐▙▄▞▘▐▌ ▐▌▐▌ ▐▌▐▌ ▐▌▗▄█▄▖▐▙▄▄▖▗▄▄▞▘
#
#_____________________________________________________

library(ggtern)
library(dplyr)
library(ggplot2)
library(ltc)

#_______________________________________________________
#
# ┌┬┐┌─┐┌┬┐┌─┐  ┌┐┌┌─┐┬─┐┌┬┐┌─┐┬  ┬┌─┐┌─┐┌┬┐┬┌─┐┌┐┌
#  ││├─┤ │ ├─┤  ││││ │├┬┘│││├─┤│  │┌─┘├─┤ │ ││ ││││
# ─┴┘┴ ┴ ┴ ┴ ┴  ┘└┘└─┘┴└─┴ ┴┴ ┴┴─┘┴└─┘┴ ┴ ┴ ┴└─┘┘└┘
#_______________________________________________________

pokemon_normalized <- pokemon_df %>%
  mutate(total = hp + attack + defense) %>%
  mutate(
    hp_prop = hp / total,
    attack_prop = attack / total,
    defense_prop = defense / total,
    dominant_stat = case_when(
      hp_prop >= attack_prop & hp_prop >= defense_prop ~ "hp",
      attack_prop >= hp_prop & attack_prop >= defense_prop ~ "attack",
      defense_prop >= hp_prop & defense_prop >= attack_prop ~ "defense"
    )
  )

pokemon_normalized

#___________________________________________________________________
#
# ┌┬┐┌─┐┌─┐┬┌┐┌┌─┐  ┌┬┐┌─┐┌┬┐┬┌┐┌┌─┐┌┐┌┌┬┐  ┬─┐┌─┐┌─┐┬┌─┐┌┐┌┌─┐
#  ││├┤ ├┤ ││││├┤    │││ ││││││││├─┤│││ │   ├┬┘├┤ │ ┬││ ││││└─┐
# ─┴┘└─┘└  ┴┘└┘└─┘  ─┴┘└─┘┴ ┴┴┘└┘┴ ┴┘└┘ ┴   ┴└─└─┘└─┘┴└─┘┘└┘└─┘
#___________________________________________________________________

# Triangle corners
A <- c(1, 0, 0)  # Attack
B <- c(0, 1, 0)  # Defense
C <- c(0, 0, 1)  # HP
center <- c(1/3, 1/3, 1/3)

# Midpoints
AB_mid <- c(0.5, 0.5, 0)
AC_mid <- c(0.5, 0, 0.5)
BC_mid <- c(0, 0.5, 0.5)

# Create polygons
region1 <- as.data.frame(rbind(C, AC_mid, center, BC_mid))  # HP
region2 <- as.data.frame(rbind(A, AB_mid, center, AC_mid))  # Attack
region3 <- as.data.frame(rbind(B, BC_mid, center, AB_mid))  # Defense
colnames(region1) <- colnames(region2) <- colnames(region3) <- c("x", "y", "z")

region_colors <- setNames(
  c("#1A5B5B", "#F4AB5C", "#D1422F"),  # HP, Attack, Defense
  c("hp", "attack", "defense")
)

#______________________
# ┬  ┌─┐┌┐ ┌─┐┬  ┌─┐
# │  ├─┤├┴┐├┤ │  └─┐
# ┴─┘┴ ┴└─┘└─┘┴─┘└─┘
#_______________________

extremes <-
  bind_rows(
    pokemon_normalized %>% slice_max(hp_prop),
    pokemon_normalized %>% slice_max(attack_prop),
    pokemon_normalized %>% slice_max(defense_prop)
  ) %>%
  distinct()

#_______________
# ╔═╗╦  ╔═╗╔╦╗
# ╠═╝║  ║ ║ ║
# ╩  ╩═╝╚═╝ ╩
#________________

# Nudge settings for label placement
nv <- 0.05
pn <- position_nudge_tern(y = nv, x = -nv / 2, z = -nv / 2)

ggtern(data = pokemon_normalized,
       aes(x = attack_prop, y = defense_prop, z = hp_prop)) +

  # Background trapezoids
  geom_polygon(data = region1, aes(x = x, y = y, z = z),
               fill = region_colors[["hp"]], alpha = 0.25, inherit.aes = FALSE) +
  geom_polygon(data = region2, aes(x = x, y = y, z = z),
               fill = region_colors[["attack"]], alpha = 0.25, inherit.aes = FALSE) +
  geom_polygon(data = region3, aes(x = x, y = y, z = z),
               fill = region_colors[["defense"]], alpha = 0.25, inherit.aes = FALSE) +

  # Points colored by dominant stat
  geom_point(aes(color = dominant_stat, size = weight), alpha = 0.6) +
  scale_color_manual(values = region_colors) +

  # Label extreme points
  geom_text(
    data = extremes,
    aes(label = pokemon),
    position = pn,
    color = "#333333",
    size = 3
  ) +
  labs(
    title = "Ternary Plot Using Attack, Defense, and HP for Each Pokémon",
    subtitle = "Each dot's size reflects the Pokémon's weight. Background color and point color show \n the dominant stat (HP, Attack, or Defense) based on relative proportions.",
    caption = "Made by Loukas Theodosiou • @bioinformatician.bsky.social",
    x = "Attack",
    y = "Defense",
    z = "HP"
  ) +
  theme_arrowlarge() +
  theme_nomask() +
  theme_hidegrid_minor()+
  theme(
    tern.axis.arrow = element_line(size = 1),
    legend.position = "none",
    text = element_text(family = "Avenir"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust=0.5),
    plot.caption = element_text(hjust=0.5)
  )

