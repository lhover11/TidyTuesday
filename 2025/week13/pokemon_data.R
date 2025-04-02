# Week 13 TidyTuesday

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(readxl)
library(ggpubr)
library(showtext)
library(ggtext)
library(grid)


# Parameters

# Theme for plots
my_theme <- function() {
  theme_minimal(base_size = 30) +
    theme(
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),  # Remove panel border
      axis.text = element_text(size = 32, color = "black", family = my_font, face = "bold"),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
      axis.title = element_text(size = 34, face = "bold", family = my_font),  # Bold axis titles
      plot.title = element_text(size = 44, face = "bold", hjust = 0.5, family = my_font, color = "black"),  # Centered, bold title
      legend.position = "none",  # no legend
      plot.caption = element_markdown(hjust= 0, margin=margin(10,0,0,0), size=22, color="black", lineheight = 1.2),
      # Set background color
      plot.background = element_rect(fill = "white", color = NA),  
      panel.background = element_rect(fill = "white", color = NA)  
    )
}


# Add cool fonts to use in plots
font_add_google("Quicksand", "quicksand")
my_font <- "quicksand"
showtext_auto()


# Plot caption
data_source = "Data: pokemon R package"
caption = paste0(data_source, "; Created by ", "<span style='font-family:fa-brands;'>&#xf16d;</span> the_plots_thicken")


# Read in data:
data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-01/pokemon_df.csv')
head(data)

# Look at data
str(data)
summary(data)

# Order pokemon types by median attack points
order <- data %>% select(type_1, attack) %>%
  group_by(type_1) %>%
  summarize(med = median(attack)) %>%
  arrange(med) %>%
  pull(type_1)

attack <- data %>% select(type_1, attack, color_1)
attack$type_1 <- factor(attack$type_1, levels = order)

# get overall median for attack points
median(data$attack)

# Plot attack points by Primary Pokemon types
ggplot(attack, aes(x = type_1, y = attack, color = color_1, fill = color_1)) +
  geom_violin() +
    geom_jitter(width = 0.2, color = "black", size = 0.5) +
  scale_color_identity() +
  scale_fill_identity() +
  theme(legend.position = "none") +
  scale_y_continuous(expand = c(0.05,0)) +
  geom_hline(yintercept = 75, linetype = "dashed", size = 1, color = "black") +
  labs(x = "", 
       y = "Attack Points",
       title = "Attack Points by Pokemon Primary Type",
       caption = caption) +
  my_theme()
ggsave("../results/Pokemon_attack_points_primary_type.png", w = 6, h = 3)
