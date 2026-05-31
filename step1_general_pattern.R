# AIM: Vizualise participation and topics discussed in the 
# worshop: Shaping the Workflow: Co-designing Ecosystem Indicators 

# 0. Call packages and functions ----
library(tidyverse)       # To manipulate data and plotting
library(readxl)          # For reading data in Excel format
library(RColorBrewer)    # Palette of colors
library(scales)          # For scaling and formatting axes, labels, and percentages in plots
library(patchwork)       # To combine ggplots
library(ggforce)

# 1. Bring the data -----
participant <- read_excel("input/EIW_FeedbackWorkshops_Miro_Content.xls", sheet = 1)
str(participant) 

familiarity <- read_excel("input/EIW_FeedbackWorkshops_Miro_Content.xls", sheet = 2)
str(familiarity) 

# Define palette
ecosystem_palette <- c(
  "#0B3C5D",  # dark blue
  "#2F5D7C",  # mid blue
  "#6FA8DC",  # light blue
  "#9FC5E8",  # light blue
  "#6AA84F",  # green
  "#F1C232",  # yellow
  "#E69138"   # orange
)


# 2. Participation profile -----
plot_career <- participant |>
  drop_na() |>
  filter(career_stage != "Other, ARDC Program Manager") |>
  count(career_stage) |>
  mutate(percent = round(n / sum(n) * 100, 0)) |>
  ggplot(aes(x = percent, y = reorder(career_stage, percent))) +
  geom_col(fill =  "#6FA8DC") +
  geom_text(aes(label = paste0(percent, "%")), hjust = -0.2) +
  labs(x = "Percentage", y = NULL, title = "Career stage") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  theme_minimal()

plot_affiliation <- participant |>
  drop_na() |>
  filter(affiliation != "Other, ARDC project partner") |>
  count(affiliation) |>
  mutate(percent = round(n / sum(n) * 100, 0)) |>
  ggplot(aes(x = percent, y = reorder(affiliation, percent))) +
  geom_col(fill =  "#6AA84F") +
  geom_text(aes(label = paste0(percent, "%")), hjust = -0.2) +
  labs(x = "Percentage", y = NULL, title = "Affiliation") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  theme_minimal()

plot_realm <- participant |>
  drop_na() |>
  mutate(realm2 = case_when(
    realm == "Saline wetlands (e.g. estuaries, mangroves)" ~ "Saline wetlands",
    TRUE ~ realm
  )) |>
  count(realm2) |>
  mutate(percent = round(n / sum(n) * 100, 0)) |>
  ggplot(aes(x = percent, y = reorder(realm2, percent))) +
  geom_col(fill =  "#F1C232") +
  geom_text(aes(label = paste0(percent, "%")), hjust = -0.2) +
  labs(x = "Percentage", y = NULL, title = "Realm") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  theme_minimal()

plot_application <- participant |>
  drop_na() |>
  mutate(purpose2 = case_when(
    purpose %in% c("Assessing ecosystem recovery or integrity", "Other, Green Status of Ecosystems - Ecosystem recovery") ~ "Recovery",
    purpose %in% c("Assessing risk of ecosystem collapse") ~ "Risk",
    purpose %in% c("Evaluating ecosystem condition") ~ "Condition",
    purpose %in% c("Ecosystem accounting (extent and condition)") ~ "Accounting",
    purpose %in% c("Research / method development") ~ "Research",
    purpose %in% c("State of the environment reporting") ~ "Reporting",
    purpose %in% c("Other, ARDC development of enduring research infrastructure - observer only for this event",
                   "Other, ARDC project partner") ~ "Infraestructure",
    purpose %in% c("Other, I work across almost all of the roles and applications mentioned above.") ~ "Multiple",
    TRUE ~ purpose
  )) |>
  count(purpose2) |>
  mutate(percent = round(n / sum(n) * 100, 0)) |>
  ggplot(aes(x = percent, y = reorder(purpose2, percent))) +
  geom_col(fill = "#E69138") +
  geom_text(aes(label = paste0(percent, "%")), hjust = -0.2) +
  labs(x = "Percentage", y = NULL, title = "Application") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  theme_minimal()

# Combine the plots
((plot_affiliation + plot_career) | (plot_realm + plot_application)) +
  plot_annotation(
    title = "Participation profile showing the percentage distribution of respondents by career stage, affiliation, ecosystem realm, and application.",
    subtitle = "Values are calculated as proportions of the total sample (excluding missing data)."
  ) &
  theme(
    plot.title = element_text(size = 12, hjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 10, hjust = 0)
  )


ggsave("output/plot_participation.png", width = 20, height = 5)

# 3. Familiarity ----
## 3.1 Base data frame ----
my_familiarity <-familiarity |>
  # Tidy up a little bit the items mentioned by participants
  mutate(item_tidy = case_when(
    item %in% c("Multi-nat imagery providers (e.g. ESA, USGS, JAXA, etc.)",
                "ebird", "BOM Data", "Q-spatial",
                "other_ARDC Nectar Research Cloud", "other_AODN",
                "other_Research Data Australia", 
                "CLoud Optimised formats (Zarr, Parquet)") ~ "Other",
    item %in% c("SEED NSW", "The LIST", "SA Enviro Data") ~ "Regional",
    TRUE ~ item
    
  )) |>
  # Order the familiarity levels
  mutate(
    quadrant = factor(
      quadrant,
      levels = c(
        "COMFORT ZONE",
        "LEARNING ZONE",
        "STRETCHING MY LIMITS",
        "BEYOND MY LIMITS"
      ),
      labels = c(
        "Conform zone",
        "Learning zone",
        "Stretching my limits",
        "Beyond my limits"
      )
    )
  ) |>
  mutate(
    boad_name = factor(
      boad_name,
      levels = c(
        "analytical_tools",
        "data_collection",
        "data_formats",
        "platform",
        "programming_languages"
      ),
      labels = c(
        "Analytical tools",
        "Data collections",
        "Data formats",
        "Platforms",
        "Programming languages"
      )
    )
  ) |>
  # Calculate totals and percentage
  count(boad_name, quadrant, item_tidy) |>
  group_by(boad_name, item_tidy) |>
  mutate(percent = round(n / sum(n) * 100, 0)) |>
  ungroup()


head(my_familiarity)

## 3.2 Labels ----
# Generate labels with the number of times a given item was mentioned in the Miro board
labels_languages <- my_familiarity |>
  filter(boad_name == "Programming languages") |>
  filter(n != 1) |>
  group_by(item_tidy, quadrant) |>
  summarise(
    percent = sum(percent, na.rm = TRUE),
    n = sum(n, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(item_tidy) |>
  # total mentions per item
  mutate(
    total_n = sum(n) 
  ) |>
  distinct(item_tidy, total_n)

labels_platforms <- my_familiarity |>
  filter(boad_name == "Platforms") |>
  filter(n != 1) |>
  group_by(item_tidy, quadrant) |>
  summarise(
    percent = sum(percent, na.rm = TRUE),
    n = sum(n, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(item_tidy) |>
  # total mentions per item
  mutate(
    total_n = sum(n) 
  ) |>
  distinct(item_tidy, total_n)

labels_formats <- my_familiarity |>
  filter(boad_name == "Data formats") |>
  filter(n != 1) |>
  group_by(item_tidy, quadrant) |>
  summarise(
    percent = sum(percent, na.rm = TRUE),
    n = sum(n, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(item_tidy) |>
  # total mentions per item
  mutate(
    total_n = sum(n) 
  ) |>
  distinct(item_tidy, total_n)

labels_collections <- my_familiarity |>
  filter(boad_name == "Data collections") |>
  filter(n != 1) |>
  group_by(item_tidy, quadrant) |>
  summarise(
    percent = sum(percent, na.rm = TRUE),
    n = sum(n, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(item_tidy) |>
  # total mentions per item
  mutate(
    total_n = sum(n) 
  ) |>
  distinct(item_tidy, total_n)


labels_tool <- my_familiarity |>
  filter(boad_name == "Analytical tools") |>
  filter(n != 1) |>
  group_by(item_tidy, quadrant) |>
  summarise(
    percent = sum(percent, na.rm = TRUE),
    n = sum(n, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(item_tidy) |>
  # otal mentions per item
  mutate(
    total_n = sum(n) 
  ) |>
  distinct(item_tidy, total_n)


## 3.3 Half circle -----

ecosystem_palette2 <- c(
  "#440154",  # dark purple
  "#31688e",  # blue
  "#35b779",  # green
  "#fde725"   # yellow
)

### Analytical tools-----
my_familiarity |>
  filter(boad_name == "Analytical tools") |>
  group_by(item_tidy, quadrant) |>
  summarise(percent = sum(percent, na.rm = TRUE), .groups = "drop") |>
  group_by(item_tidy) |>
  arrange(quadrant) |>
  mutate(
    id = row_number(),
    start = 0,
    end   = pi,
    # Normalize thickness so all the tools have the same size
    thickness_raw = percent / 100,
    thickness = thickness_raw / sum(thickness_raw),  
    r0 = cumsum(lag(thickness, default = 0)),
    r  = cumsum(thickness)
  ) |>
  ggplot() +
  geom_arc_bar(
    aes(
      x0 = 0, y0 = 0,
      r0 = r0,
      r = r,
      start = start,
      end = end,
      fill = quadrant,
    ),
    color = "grey95", linewidth = 0.6
  ) +
  facet_wrap(~item_tidy) +
  scale_fill_manual(values = ecosystem_palette2) +
  coord_fixed(clip = "off") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_void() +
  geom_text(
    data = labels_tool,
    aes(
      x = 0,
      # slightly above the outer ring
      y = 1.15,   
      label = paste0("n = ", total_n)
    ),
    size = 3,
    fontface = "italic"
  ) +
  labs(x = "Percentage", y = NULL, fill = "Familiarity",
       title = "How familiar are participants with different analytical tools?" ,
       subtitle = "Each panel shows the distribution of responses across learning zones for a given tool.\nRing thickness represents the relative share of responses within each tool.") +
  theme(
    #panel.spacing = unit(0.5, "lines"),
    #plot.margin = margin(t = 10, r = 10, b = 10, l = 5),
    plot.title = element_text(size = 12, hjust = 0, face = "bold", margin = margin(b = 6)),
    plot.subtitle = element_text(size = 10, hjust = 0, margin = margin(b = 10))
  )

ggsave("output/plot_familiarity_analytical_tool.png", width = 7, height = 10)

### Data collections ----

my_familiarity |>
  filter(boad_name == "Data collections") |>
  filter(n != 1) |>
  group_by(item_tidy, quadrant) |>
  summarise(percent = sum(percent, na.rm = TRUE), .groups = "drop") |>
  group_by(item_tidy) |>
  arrange(quadrant) |>
  mutate(
    id = row_number(),
    start = 0,
    end   = pi,
    # Normalize thickness so all the tools have the same size
    thickness_raw = percent / 100,
    thickness = thickness_raw / sum(thickness_raw),  
    r0 = cumsum(lag(thickness, default = 0)),
    r  = cumsum(thickness)
  ) |>
  ggplot() +
  geom_arc_bar(
    aes(
      x0 = 0, y0 = 0,
      r0 = r0,
      r = r,
      start = start,
      end = end,
      fill = quadrant,
    ),
    color = "grey95", linewidth = 0.6
  ) +
  coord_fixed(clip = "off") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~item_tidy) +
  scale_fill_manual(values = ecosystem_palette2) +
  theme_void() +
  geom_text(
    data = labels_collections,
    aes(
      x = 0,
      # slightly above the outer ring
      y = 1.15,   
      label = paste0("n = ", total_n)
    ),
    size = 3,
    fontface = "italic"
  ) +
  labs(x = "Percentage", y = NULL, fill = "Familiarity",
       title = "How familiar are participants with different data collections?" ,
       subtitle = "Each panel shows the distribution of responses across learning zones for a given data collection.\nRing thickness represents the relative share of responses within each collection.") +
  theme(
    plot.title = element_text(size = 12, hjust = 0, face = "bold", margin = margin(b = 6)),
    plot.subtitle = element_text(size = 10, hjust = 0, margin = margin(b = 10))
  )

ggsave("output/plot_familiarity_data_collections.png", width = 7, height = 10)


### Data formats -----
my_familiarity |>
  filter(boad_name == "Data formats") |>
  filter(n != 1) |>
  group_by(item_tidy, quadrant) |>
  summarise(percent = sum(percent, na.rm = TRUE), .groups = "drop") |>
  group_by(item_tidy) |>
  arrange(quadrant) |>
  mutate(
    id = row_number(),
    start = 0,
    end   = pi,
    # Normalize thickness so all the tools have the same size
    thickness_raw = percent / 100,
    thickness = thickness_raw / sum(thickness_raw),  
    r0 = cumsum(lag(thickness, default = 0)),
    r  = cumsum(thickness)
  ) |>
  ggplot() +
  geom_arc_bar(
    aes(
      x0 = 0, y0 = 0,
      r0 = r0,
      r = r,
      start = start,
      end = end,
      fill = quadrant,
    ),
    color = "grey95", linewidth = 0.6
  ) +
  facet_wrap(~item_tidy) +
  scale_fill_manual(values = ecosystem_palette2) +
  coord_fixed(clip = "off") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_text(
    data = labels_formats,
    aes(
      x = 0,
      # slightly above the outer ring
      y = 1.15,   
      label = paste0("n = ", total_n)
    ),
    size = 3,
    fontface = "italic"
  ) +
  theme_void() +
  labs(x = "Percentage", y = NULL, fill = "Familiarity",
       title = "How familiar are participants with different data formats?" ,
       subtitle = "Each panel shows the distribution of responses across learning zones for a given data format.\nRing thickness represents the relative share of responses within each format.") +
  theme(
    plot.title = element_text(size = 12, hjust = 0, face = "bold", margin = margin(b = 6)),
    plot.subtitle = element_text(size = 10, hjust = 0, margin = margin(b = 10))
  )

ggsave("output/plot_familiarity_data_format.png", width = 7, height = 10)

### Platforms -----
my_familiarity |>
  filter(boad_name == "Platforms") |>
  filter(n != 1) |>
  group_by(item_tidy, quadrant) |>
  summarise(percent = sum(percent, na.rm = TRUE), .groups = "drop") |>
  group_by(item_tidy) |>
  arrange(quadrant) |>
  mutate(
    id = row_number(),
    start = 0,
    end   = pi,
    # Normalize thickness so all the tools have the same size
    thickness_raw = percent / 100,
    thickness = thickness_raw / sum(thickness_raw),  
    r0 = cumsum(lag(thickness, default = 0)),
    r  = cumsum(thickness)
  ) |>
  ggplot() +
  geom_arc_bar(
    aes(
      x0 = 0, y0 = 0,
      r0 = r0,
      r = r,
      start = start,
      end = end,
      fill = quadrant,
    ),
    color = "grey95", linewidth = 0.6
  ) +
  facet_wrap(~item_tidy) +
  scale_fill_manual(values = ecosystem_palette2) +
  coord_fixed(clip = "off") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_text(
    data = labels_platforms,
    aes(
      x = 0,
      # slightly above the outer ring
      y = 1.15,   
      label = paste0("n = ", total_n)
    ),
    size = 3,
    fontface = "italic"
  ) +
  theme_void() +
  labs(x = "Percentage", y = NULL, fill = "Familiarity",
       title = "How familiar are participants with different platforms?" ,
       subtitle = "Each panel shows the distribution of responses across learning zones for a given platform.\nRing thickness represents the relative share of responses within each platform.") +
  theme(
    plot.title = element_text(size = 12, hjust = 0, face = "bold", margin = margin(b = 6)),
    plot.subtitle = element_text(size = 10, hjust = 0, margin = margin(b = 10))
  )

ggsave("output/plot_familiarity_platform.png", width = 7, height = 10)

### Programming languages -----
my_familiarity |>
  filter(boad_name == "Programming languages") |>
  filter(n != 1) |>
  group_by(item_tidy, quadrant) |>
  summarise(percent = sum(percent, na.rm = TRUE), .groups = "drop") |>
  group_by(item_tidy) |>
  arrange(quadrant) |>
  mutate(
    id = row_number(),
    start = 0,
    end   = pi,
    # Normalize thickness so all the tools have the same size
    thickness_raw = percent / 100,
    thickness = thickness_raw / sum(thickness_raw),  
    r0 = cumsum(lag(thickness, default = 0)),
    r  = cumsum(thickness)
  ) |>
  ggplot() +
  geom_arc_bar(
    aes(
      x0 = 0, y0 = 0,
      r0 = r0,
      r = r,
      start = start,
      end = end,
      fill = quadrant,
    ),
    color = "grey95", linewidth = 0.6
  ) +
  facet_wrap(~item_tidy) +
  scale_fill_manual(values = ecosystem_palette2) +
  coord_fixed(clip = "off") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_void() +
  geom_text(
    data = labels_languages,
    aes(
      x = 0,
      # slightly above the outer ring
      y = 1.15,   
      label = paste0("n = ", total_n)
    ),
    size = 3,
    fontface = "italic"
  ) +
  labs(x = "Percentage", y = NULL, fill = "Familiarity",
       title = "How familiar are participants with different programming languages?" ,
       subtitle = "Each panel shows the distribution of responses across learning zones for a given languages.\nRing thickness represents the relative share of responses within each language.") +
  theme(
    plot.title = element_text(size = 12, hjust = 0, face = "bold", margin = margin(b = 6)),
    plot.subtitle = element_text(size = 10, hjust = 0, margin = margin(b = 10))
  )

ggsave("output/plot_familiarity_programming_languages.png", width = 7, height = 10)


# Develop a two year workflow development roadmap that will identify
# Outline user requirements for ecosystem workflows
# Document high level workflows requirements and constraints
# Document detailed use cases describing who are the end users, how they would use the outputs of the project, and the impact of these outputs
# Detail the priorisation of the functionality to define the first tranche of ecosystem indicator workflows to be delivered in the first 12 months
# Detail the priorisation of functionality to define the second tranche of ecosystem indicator workflows to be delivered in the following 12 months
# Review the initial second tranche functionality requirements of ecosystem indicator workflows in the 4th quarter of year 1 to be delivered in year 2 and update if necessary.



