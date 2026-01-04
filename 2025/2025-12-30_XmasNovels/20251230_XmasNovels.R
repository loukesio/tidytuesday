# Load visNetwork
library(visNetwork)
library(tidyverse)
library(igraph)
library(ggraph)
library(patchwork)
library(scales)
library(tidytext)

# Load data directly from URLs
authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-30/christmas_novel_authors.csv')
christmas_novel_text <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-30/christmas_novel_text.csv')
novels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-30/christmas_novels.csv')

novels
christmas_novel_text
authors


# Define color palette inspired by Picasso's "Les femmes d'Alger"
picasso_colors <- c(
  "Early Romantic" = "#1A5B5B",    # Dark teal
  "Victorian" = "#ACC8BE",          # Light sage
  "Late Victorian" = "#F4AB5C",     # Warm orange
  "Edwardian+" = "#D1422F"          # Terracotta red
)

# Count books per author
author_counts <- novels %>%
  count(gutenberg_author_id, name = "n_books") %>%
  left_join(authors, by = "gutenberg_author_id") %>%
  mutate(author_short = str_extract(author, "^[^,]+"))

# Get vocabulary per author from full text
cat("Processing text data (this may take a moment)...\n")

author_vocab <- christmas_novel_text %>%
  inner_join(novels, by = "gutenberg_id") %>%
  filter(!is.na(text)) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  filter(str_length(word) > 3) %>%
  group_by(gutenberg_author_id, word) %>%
  summarize(n = n(), .groups = "drop") %>%
  group_by(gutenberg_author_id) %>%
  slice_max(n, n = 200) %>%
  summarize(words = list(word), .groups = "drop")

cat("Vocabulary extracted for", nrow(author_vocab), "authors\n")

# Calculate similarity between authors
cat("Calculating author similarities...\n")

author_pairs <- expand.grid(
  id1 = author_vocab$gutenberg_author_id,
  id2 = author_vocab$gutenberg_author_id,
  stringsAsFactors = FALSE
) %>%
  filter(id1 < id2)

similarity_edges <- author_pairs %>%
  left_join(author_vocab, by = c("id1" = "gutenberg_author_id")) %>%
  rename(words_1 = words) %>%
  left_join(author_vocab, by = c("id2" = "gutenberg_author_id")) %>%
  rename(words_2 = words) %>%
  mutate(
    jaccard = map2_dbl(words_1, words_2, function(w1, w2) {
      shared <- length(intersect(w1, w2))
      total <- length(union(w1, w2))
      if (total == 0) return(0)
      shared / total
    })
  ) %>%
  filter(jaccard > 0.15) %>%
  dplyr::select(from = id1, to = id2, weight = jaccard)

cat("Number of similarity edges:", nrow(similarity_edges), "\n")

# Try lower threshold if no edges found
if (nrow(similarity_edges) == 0) {
  cat("Trying lower threshold (>10%)...\n")
  similarity_edges <- author_pairs %>%
    left_join(author_vocab, by = c("id1" = "gutenberg_author_id")) %>%
    rename(words_1 = words) %>%
    left_join(author_vocab, by = c("id2" = "gutenberg_author_id")) %>%
    rename(words_2 = words) %>%
    mutate(
      jaccard = map2_dbl(words_1, words_2, function(w1, w2) {
        shared <- length(intersect(w1, w2))
        total <- length(union(w1, w2))
        if (total == 0) return(0)
        shared / total
      })
    ) %>%
    filter(jaccard > 0.10) %>%
    dplyr::select(from = id1, to = id2, weight = jaccard)
  cat("Found", nrow(similarity_edges), "edges\n")
}

# Create interactive network
if (nrow(similarity_edges) > 0) {
  g <- graph_from_data_frame(similarity_edges, directed = FALSE)

  # Calculate network metrics
  node_degree <- as.numeric(degree(g))
  node_betweenness <- betweenness(g)

  # Prepare nodes dataframe with colors directly assigned
  nodes <- tibble(
    id = as.numeric(V(g)$name),
    label = author_counts$author_short[match(id, author_counts$gutenberg_author_id)],
    title = paste0(
      "<b>", author_counts$author[match(id, author_counts$gutenberg_author_id)], "</b><br>",
      "Born: ", authors$birthdate[match(id, authors$gutenberg_author_id)],
      " | Died: ", authors$deathdate[match(id, authors$gutenberg_author_id)], "<br>",
      "Era: ", cut(
        authors$birthdate[match(id, authors$gutenberg_author_id)],
        breaks = c(0, 1830, 1850, 1870, 2000),
        labels = c("Early Romantic", "Victorian", "Late Victorian", "Edwardian+")
      ), "<br>",
      "Books: ", author_counts$n_books[match(id, author_counts$gutenberg_author_id)], "<br>",
      "<b>Network Position:</b><br>",
      "Connections: ", node_degree, "<br>",
      "Betweenness: ", round(node_betweenness, 1), "<br>",
      "<i>(Node size = number of connections)</i>"
    ),
    value = node_degree * 3,  # Size by connections - THIS IS WHAT CONTROLS NODE SIZE
    birthdate = authors$birthdate[match(id, authors$gutenberg_author_id)],
    group = cut(
      authors$birthdate[match(id, authors$gutenberg_author_id)],
      breaks = c(0, 1830, 1850, 1870, 2000),
      labels = c("Early Romantic", "Victorian", "Late Victorian", "Edwardian+")
    ),
    # Assign colors directly based on group - FIXED TO MATCH LEGEND
    color = case_when(
      group == "Early Romantic" ~ "#1A5B5B",  # Dark teal
      group == "Victorian" ~ "#ACC8BE",        # Light sage
      group == "Late Victorian" ~ "#F4AB5C",   # Warm orange
      group == "Edwardian+" ~ "#D1422F",       # Terracotta red
      TRUE ~ "#999999"
    ),
    shape = "dot",
    font.size = 14,           # FIXED: Simple values instead of list
    font.color = "#000000",   # FIXED: Black labels for readability
    borderWidth = 2,
    borderWidthSelected = 4
  )

  # Prepare edges dataframe - VARIABLE INTENSITY based on similarity
  edges <- similarity_edges %>%
    mutate(
      width = weight * 5,
      opacity = 0.2 + (weight * 0.8),
      color = paste0("rgba(0, 0, 0, ", round(opacity, 2), ")"),
      title = paste0(
        "<b>Vocabulary Similarity: ", round(weight * 100, 1), "%</b><br>",
        "Shared words in their top 200"
      )
    )

  # Print edge weight distribution
  cat("\nEdge weight distribution:\n")
  cat("  Min similarity:", round(min(edges$weight), 3), "\n")
  cat("  Max similarity:", round(max(edges$weight), 3), "\n")
  cat("  Mean similarity:", round(mean(edges$weight), 3), "\n")

  # Create interactive network with BETTER SPACING
  p2 <- visNetwork(nodes, edges, height = "900px", width = "100%") %>%
    visNodes(
      shape = "dot",
      scaling = list(min = 15, max = 60),  # Bigger range for better visibility
      font = list(size = 14, face = "arial", color = "#000000"),
      shadow = list(enabled = TRUE, size = 5, x = 3, y = 3),
      borderWidth = 2
    ) %>%
    visEdges(
      scaling = list(min = 1, max = 5),
      smooth = list(enabled = TRUE, type = "continuous", roundness = 0.5),
      hoverWidth = 2  # Thicker on hover
    ) %>%
    visLegend(
      main = list(text = "Literary Era",
                  style = "font-family:arial;font-weight:bold;font-size:16px;"),
      position = "right",
      width = 0.2,
      useGroups = FALSE,  # Don't use groups to avoid mismatch
      addNodes = list(
        list(label = "Early Romantic (<1830)", shape = "dot",
             size = 20, color = "#1A5B5B"),
        list(label = "Victorian (1830-1850)", shape = "dot",
             size = 20, color = "#ACC8BE"),
        list(label = "Late Victorian (1850-1870)", shape = "dot",
             size = 20, color = "#F4AB5C"),
        list(label = "Edwardian+ (1870+)", shape = "dot",
             size = 20, color = "#D1422F")
      )
    ) %>%
    visOptions(
      highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE,
                              labelOnly = FALSE, hideColor = "rgba(200,200,200,0.2)"),
      selectedBy = list(
        variable = "group",
        main = "Select by Era:",
        style = "font-family:arial;font-size:14px;"
      ),
      nodesIdSelection = list(
        enabled = TRUE,
        main = "Select Author:",
        style = "font-family:arial;font-size:14px;"
      )
    ) %>%
    visInteraction(
      navigationButtons = TRUE,
      dragNodes = TRUE,
      dragView = TRUE,
      zoomView = TRUE,
      hover = TRUE,
      tooltipDelay = 200,
      tooltipStay = 300,
      keyboard = TRUE,
      multiselect = TRUE
    ) %>%
    visPhysics(
      enabled = TRUE,  # ENABLE physics for better distribution
      solver = "forceAtlas2Based",
      forceAtlas2Based = list(
        gravitationalConstant = -80,      # Stronger repulsion - spreads nodes apart
        centralGravity = 0.005,           # Weak center pull
        springLength = 150,               # Longer springs = more space
        springConstant = 0.05,            # Weaker springs
        damping = 0.6,                    # More damping = settles faster
        avoidOverlap = 0.8                # Strong overlap avoidance
      ),
      stabilization = list(
        enabled = TRUE,
        iterations = 2000,                # More iterations for better layout
        updateInterval = 50
      )
    ) %>%
    visLayout(
      randomSeed = 42,
      improvedLayout = TRUE,
      hierarchical = FALSE
    )

  print(p2)
}

####################
# Save a snapshot
####################
library(htmlwidgets)
library(webshot2)

# Save the network widget as HTML
saveWidget(p2, "network.html", selfcontained = TRUE)

# Take a screenshot
webshot("network.html",
        file = "network_snapshot.png",
        vwidth = 1200,    # Width in pixels
        vheight = 900,    # Height in pixels
        delay = 3)        # Wait 3 seconds for rendering

