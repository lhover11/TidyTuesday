# Week 11 TidyTuesday

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
  theme_minimal(base_size = 24) +
    theme(
      panel.grid.major.y = element_blank(),  # Remove horizontal gridlines
      panel.grid.major.x = element_blank(),  # Remove vertical grid lines
      panel.border = element_blank(),  # Remove panel border
      axis.text = element_text(size = 26, color = "black", family = my_font),
      # axis.text.x = element_text(size = 22, color = "black"),  # Bold x-axis labels
      # axis.text.y = element_text(size = 22, color = "black"),  # Readable y-axis labels
      axis.title = element_text(size = 30, face = "bold", family = my_font),  # Bold axis titles
      plot.title = element_text(size = 44, face = "bold", hjust = 0.5, family = my_font, color = "#0B6E4F"),  # Centered, bold title
      legend.position = "none",  # no legend
      # legend.title = element_text(size = 14, face = "bold"),  # Bold legend title
      # legend.text = element_text(size = 14),  # Readable legend labels
      plot.caption = element_markdown(hjust= 0, margin=margin(10,0,0,0), size=16, color="black", lineheight = 1.2),
      panel.grid = element_line(color = alpha("black", 0.4)),
      # Set background color
      plot.background = element_rect(fill = "ivory", color = NA),  
      panel.background = element_rect(fill = "ivory", color = NA)  
    )
}


# Add cool fonts to use in plots
font_add_google("Quicksand", "quicksand")
my_font <- "quicksand"
showtext_auto()

# Import font awesome to use icons
font_add('fa-reg', '../../fontawesome/otfs/Font Awesome 6 Free-Regular-400.otf')
font_add('fa-brands', '../../fontawesome/otfs/Font Awesome 6 Brands-Regular-400.otf')
font_add('fa-solid', '../../fontawesome/otfs/Font Awesome 6 Free-Solid-900.otf')


# Custom color palette
# green_cols <- rev(c("#344E41","#3A5A40", "#588157", "#5A9F68"))
green_cols <- rev(c("#073B3A", "#0B6E4F", "#08A045", "#6BBF59"))


# Plot caption
data_source = "Data: palmtrees R package"
caption = paste0(data_source, "; Created by ", "<span style='font-family:fa-brands;'>&#xf16d;</span> the_plots_thicken")


# Read in data:
data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-18/palmtrees.csv')
head(data)

# Look at data
## How many palm tribes and subfamilies are there?
head(data)
unique(data$acc_genus) # > 180 genus
unique(data$palm_tribe)  # > 20
unique(data$palm_subfamily) # 5 subfamilies


# Let's look at the number of leaves per subfamily

# summarize max number of leaves per subfamily
med_leaves <- data %>%
  group_by(palm_subfamily) %>%
  summarize(med_leaves = median(max_leaf_number, na.rm = TRUE),
            total_trees = n())

# create order for plot based on median
order <- med_leaves %>% arrange(med_leaves) %>% pull(palm_subfamily)
data$palm_subfamily <- factor(data$palm_subfamily, levels = order)

# remove Nypoideae subfamily, only 1 data point
data <- data %>%
  filter(palm_subfamily != "Nypoideae")


# Plot data
ggplot(data, aes(palm_subfamily, max_leaf_number,
                 fill = palm_subfamily, color = palm_subfamily)) +
  geom_boxplot(alpha = 0.3, width = 0.5, outlier.shape = NA) +
  geom_jitter(width=0.15, alpha=0.8)+
  scale_fill_manual(values = green_cols) +
  scale_color_manual(values = green_cols) +
  labs(x = "Palm Subfamily", 
       y = "Max Leaf Number",
       title = "Max Leaf Number by Palm Subfamily",
       caption = caption) +
  my_theme()
ggsave("../results/palm_trees_max_leaves.png", w = 4.5, h = 4)
