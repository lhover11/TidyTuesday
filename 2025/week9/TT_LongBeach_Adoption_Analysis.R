# Week 9 TidyTuesday

# Load libraries
library(tidyverse)
library(ggridges)
library(RColorBrewer)
library(ggpubr)

# Parameters

# Theme for plots
my_theme <- function() {
  theme_minimal(base_size = 16) +
    theme(
      #panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"),  # Subtle horizontal grid
      panel.grid.major.x = element_blank(),  # Remove vertical grid lines
      axis.text.x = element_text(size = 12, color = "black"),  # Bold x-axis labels
      axis.text.y = element_text(size = 12, color = "black"),  # Readable y-axis labels
      axis.title = element_text(size = 12, face = "bold"),  # Bold axis titles
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Centered, bold title
      legend.position = "top",  # Move legend to the top
      legend.title = element_text(size = 14, face = "bold"),  # Bold legend title
      legend.text = element_text(size = 14),  # Readable legend labels
      panel.background = element_rect(fill = "transparent", color = NA),  # Transparent background
      plot.background = element_rect(fill = "transparent", color = NA)  # Transparent plot background
    )
}

# Custom color palette
color_pal <- c("#f2d45d", "#ee7c0e" ,"#a23b3c", "#800080", "#6950a1", "#0053a1", "#7ba0a0" ,"#006e46")


# Read in data:
data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-04/longbeach.csv')
head(data)


#### PART I: Data Exploration and Cleaning ####

# Data exploration
dim(data)
# 29787 rows and 22 columns

# Let's examine the types of data in each column
str(data)

# Remove any duplicated rows
data <- distinct(data)

# check for duplicate entries for animals with the same ID and intake date
any(duplicated(data[, c("animal_id", "intake_date")])) # TRUE
dups <- unique(data$animal_id[duplicated(data[, c("animal_id", "intake_date")])])

# Look at some of these manually
check <- data %>% filter(animal_id %in% dups)

# Let's remove any rows where the animal ID and intake date is the same, but the animal has different intake values or different outcomes, we can't tell which values are true
nrow(data)
data_cleaned <- data %>%
  group_by(animal_id, intake_date) %>%
  filter(n_distinct(outcome_subtype, intake_type, outcome_date) == 1) %>%
  ungroup()

# Check again for duplicates
any(duplicated(data_cleaned[, c("animal_id", "intake_date")]))
dups <- unique(data_cleaned$animal_id[duplicated(data_cleaned[, c("animal_id", "intake_date")])])
check <- data_cleaned %>% filter(animal_id %in% dups)

# The only remaining duplicated animal id only has a different 'crossing' value, let's just keep the first instance for this animal
data_cleaned <- data_cleaned %>%
  group_by(animal_id) %>%
  filter(animal_id != "A718113" | row_number() == 1) %>%
  ungroup()


# What types of animals do we have:
data_cleaned %>%
  group_by(animal_type) %>%
  summarize(count = n()) %>%
  arrange(-count)

# dataset contains by far the most dogs and cats, let's create groupings to get this down to major groupings
data_cleaned <- data_cleaned %>%
  mutate(animal_category = case_when(
    animal_type == "reptile" ~ "reptile_amphibian",
    animal_type == "amphibian" ~ "reptile_amphibian",
    animal_type == "guinea pig" ~ "other",
    animal_type == "livestock" ~ "other",
    TRUE ~ animal_type))



# What types of intake do we have:
data_cleaned %>%
  group_by(intake_type) %>%
  summarize(count = n()) %>%
  arrange(-count)

# convert "adopted animal return" to return
data_cleaned$intake_type <- gsub("adopted animal return", "return", data_cleaned$intake_type)



# What types of outcome types do we have:
data_cleaned %>%
  group_by(outcome_type) %>%
  summarize(count = n()) %>%
  arrange(-count)


# merge foster and foster to adopt
data_cleaned$outcome_type <- gsub("foster to adopt", "foster", data_cleaned$outcome_type)

# remove animals with NA outcome
data_cleaned <- data_cleaned %>%
  filter(!is.na(outcome_type))


# format date columns
data_cleaned$dob <- ymd(data_cleaned$dob)
data_cleaned$intake_date <- ymd(data_cleaned$intake_date)
data_cleaned$outcome_date <- ymd(data_cleaned$outcome_date)

# add intake and outcome year and month columns
data_cleaned$outcome_month <- month(data_cleaned$outcome_date)
data_cleaned$outcome_year <- year(data_cleaned$outcome_date)
data_cleaned$intake_month <- month(data_cleaned$intake_date)
data_cleaned$intake_year <- year(data_cleaned$intake_date)


## Calculate duration of time at the shelter in days
data_cleaned$duration <- as.numeric(data_cleaned$outcome_date - data_cleaned$intake_date)


#### PART II: Analysis and Visualization ####

# Subset to adopted animal data
adoptions <- data_cleaned %>%
  filter(outcome_type == "adoption")

## Adoptions per animal type
adoptions_n <- adoptions %>%
  filter(outcome_type == "adoption") %>%
  count(animal_type, name = "count") %>%
  ungroup() %>%
  mutate(percentage = (count / sum(count)) * 100) # get percentage of all adoptions by animal type


# Order of most commonly adopted animals
animal_order <- adoptions_n %>%
  arrange(-percentage) %>%
  pull(animal_type)


# Create a named vector for mapping colors to the categories
animal_colors <- setNames(color_pal, animal_order)


# Percentages for all animals below rabbits are so low, let's combine those into 1 group
adoptions_n <- adoptions_n %>%
  mutate(animal_type = ifelse(animal_type %in% c("dog", "cat", "rabbit"), animal_type, "other")) %>%
  group_by(animal_type) %>%
  summarise(count = sum(count), percentage = sum(percentage))

adoptions_n$animal_type_ordered <- factor(adoptions_n$animal_type, levels = c("cat", "dog", "rabbit", "other"))

# Plot 1: adoption percentage by animal type

p1 <- ggplot(adoptions_n, aes(x = animal_type_ordered, y = percentage, fill = animal_type_ordered)) +
  geom_col(width = 0.5, color = "black") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            vjust = -0.5, size = 4, fontface = "bold") +  # Add % above bars
  scale_fill_manual(values = animal_colors) +
  xlab("") +
  ylab("% of all Adoptions") +
  scale_y_continuous(limits=c(0,60), expand = c(0,0)) + # use expand = c(0,0) to reduce space between axes and plot
  scale_x_discrete(expand = c(0, 0)) +
  my_theme() +
  theme(legend.position = 'none',
        axis.text.y = element_blank())
p1

## Adoptions per month for all animals, dogs and cats 

# Create column with month and year
adoptions <- adoptions %>%
  mutate(intake_month = paste0(intake_month, "-", intake_year), 
         outcome_month = paste0(outcome_month, "-", outcome_year)
  )

# Create order for month-year
month_order <- expand.grid(month = 1:12, year = 2017:2024) %>%
  mutate(label = paste(month, year, sep = "-")) %>%
  pull(label)

# Adoptions for all animals combined
all_combined <- adoptions %>%
  filter(outcome_type == "adoption") %>%
  select(animal_type, outcome_month) %>%
  group_by(outcome_month) %>%
  count() %>%
  rename(adoptions = n, date = outcome_month) %>%
  mutate(animal_type = "all")

all_combined$date <- factor(all_combined$date, levels = month_order)

# Plot 2: Plot adoptions for each month (all animals combined)
p2 <- ggplot(all_combined %>% filter(animal_type == "all"), aes(x = date, y = adoptions, color = animal_type)) +
  geom_line(aes(group = animal_type), size = 1, color = "black") +
  ylab("Number of Adoptions")+
  xlab("") +
  scale_x_discrete(breaks = paste0("1-", 2017:2024)) +  # Only show January of each year
  theme(axis.text.x = element_text(angle = 90)) +
  geom_vline(xintercept = '3-2020', linetype = "dashed") +
  annotate("text", x = "6-2020", y = 200, label = "Covid", size = 4, fontface = "bold") +
  my_theme() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90))
p2

## How long do adopted animals stay at the shelter?

# Update ordered animal type column for all animals
adoptions$animal_type_ordered <- factor(adoptions$animal_type, levels = animal_order)

p3 <- ggplot(adoptions, aes(x = duration, y = animal_type_ordered, fill = animal_type_ordered)) +
  geom_density_ridges() +
  scale_fill_manual(values = animal_colors) +
  xlab("Days in Shelter before Adoption") +
  ylab("") +
  #scale_y_discrete(expand = c(0,2)) +
  geom_vline(xintercept = 30, linetype = "dashed", size = 1) +
  annotate("text", x = 200, y = 8.75, label = "1 month", size = 4, fontface = "bold") +
  my_theme()+
  theme(legend.position = 'none')
p3


# combine plots together 

combined_plot <- ggarrange(
  p2,                # First row with line plot
  # Second row with combined bar and ridge plot
  ggarrange(p1, p3, ncol = 2), 
  nrow = 2 
) 

# Add title to combined plot
annotate_figure(combined_plot, top = text_grob("Analysis of Adopted Animals \n Long Beach Animal Shelter, 2017-2014", face = "bold", size = 16))

ggsave("../results/TT9_LongBeach_Adoption_Plot.png", w = 8, h = 6)
