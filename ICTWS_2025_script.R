# Load packages ####

# Before loading the packages, you will need to install them. Use:
# install.packages("")
# and include the name of the package in the quotes. 

library(palmerpenguins)
library(tidyverse)

# Load the Palmer Penguins dataset ####

data(package = "palmerpenguins")

p <- penguins_raw

p

# Tibbles and data frames ####

class(p)

p_df <- as.data.frame(p)

p_df
head(p_df)

p <- as_tibble(p_df)

p
head(p)

# Learning tidyverse functions ####

## Selecting specific columns ####

select(p, 
       "Species", 
       "Individual ID",
       "Culmen Length (mm)",
       "Culmen Depth (mm)",
       "Flipper Length (mm)",
       "Body Mass (g)")

## Renaming columns ####

p <- rename(p, 
            study_name = studyName,
            sample = "Sample Number",
            species = Species,
            region = Region,
            island = Island,
            age_class = Stage,
            ind_id = "Individual ID",
            clutch_comp = "Clutch Completion",
            date_egg = "Date Egg",
            culmen_length_mm = "Culmen Length (mm)",
            culmen_depth_mm = "Culmen Depth (mm)",
            flipper_length_mm = "Flipper Length (mm)",
            body_mass_g = "Body Mass (g)",
            sex = Sex,
            delta_15_n = "Delta 15 N (o/oo)",
            delta_13_c = "Delta 13 C (o/oo)",
            comments = Comments)

## Concatenating multiple commands: the pipe ####

morph <- p_df %>%
  as_tibble() %>% 
  rename(study_name = studyName,
         sample = "Sample Number",
         species = Species,
         region = Region,
         island = Island,
         age_class = Stage,
         ind_id = "Individual ID",
         clutch_comp = "Clutch Completion",
         date_egg = "Date Egg",
         culmen_length_mm = "Culmen Length (mm)",
         culmen_depth_mm = "Culmen Depth (mm)",
         flipper_length_mm = "Flipper Length (mm)",
         body_mass_g = "Body Mass (g)",
         sex = Sex,
         delta_15_n = "Delta 15 N (o/oo)",
         delta_13_c = "Delta 13 C (o/oo)",
         comments = Comments) %>% 
  select(sample,
         ind_id,
         species,
         sex,
         age_class,
         culmen_length_mm,
         culmen_depth_mm,
         flipper_length_mm, 
         body_mass_g)

## Dropping columns ####

morph <- morph %>% 
  select(-sample)

## Finding unique values ####

ind_info <- p %>% 
  # Columns we want to find unique combinations of
  select(ind_id, species, sex) %>% 
  distinct()

## Extracting single columns as vectors ####

# Let's check how many unique individuals we have in the data
p %>% 
  pull(ind_id) %>% 
  unique()

# We only have 190 unique IDs, but the table of unique combinations of
# ID, species, and sex has 298 rows. This means we have repeated IDs! 

## Filtering data ####

# Let's look, for example, at individual N11A1.

# Filtering based on one logical condition
ind_info %>% 
  filter(ind_id == "N11A1")

# The same ID was assigned to two individuals of different species.

## Grouping and doing operations by group ####

# How can we find out which individual IDs are repeated in the data?

# For each individual ID, we can count the number of rows it appears in
ind_info %>% 
  group_by(ind_id) %>% 
  tally()

## Sorting results ####

# How many times at most is an ID repeated?
ind_info %>% 
  group_by(ind_id) %>% 
  tally() %>% 
  arrange(n)

# Reverse order
ind_info %>% 
  group_by(ind_id) %>% 
  tally() %>% 
  arrange(desc(n))
# At most, an ID is repeated 3 times

# Now we can find the repeated IDs, which will be those with n > 1
ind_info %>% 
  group_by(ind_id) %>% 
  tally() %>% 
  filter(n > 1)

# This command is equivalent:
ind_info %>% 
  group_by(ind_id) %>% 
  tally() %>% 
  filter(n %in% c(2, 3))

## Creating new columns ####

# We can create a new column to assign unique IDs to each individual. For 
# example, we can create a column with the prefix "ID" and an incremental number 
ind_info <- ind_info %>% 
  mutate(new_id = paste0("ID", 1:nrow(.)))

## Conditional value assignment ####

# Alternatively, if we wanted something more sophisticated and we did not want
# to lose the original ID that was assigned to each individual, we can append
# to it the initial of the species and the initial of the sex.

unique(ind_info$species)

ind_info <- ind_info %>% 
  mutate(species_initial = case_when(
    species == "Adelie Penguin (Pygoscelis adeliae)" ~ "A",
    species == "Gentoo penguin (Pygoscelis papua)" ~ "G",
    species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "C"
  ),
  sex_initial = case_when(
    sex == "MALE" ~ "M",
    sex == "FEMALE" ~ "F",
    is.na(sex) ~ "U"
  )) %>% 
  mutate(new_id = paste(ind_id, species_initial, sex_initial, sep = "_")) %>% 
  select(-species_initial, -sex_initial)

length(unique(ind_info$new_id)) == nrow(ind_info)

## Joining tables ####

# How can we make sure that the new individual ID appears in the morphometrics
# table, too?

# We can either do the same thing we did above and recreate the ID, or we can
# attach the ID from the ind_info table based on matches of ind_id, sex, and 
# species
left_join(morph, ind_info)
left_join(morph, ind_info, by = "ind_id")
morph <- left_join(morph, ind_info, by = c("ind_id", "species", "sex"))

## Relocating columns ####

# We want the new ID that we created to appear at the beginning of the table,
# not at the end.

morph %>% 
  relocate(new_id, .after = ind_id)

morph <- morph %>% 
  relocate(new_id, .before = ind_id)

## Limiting results and subsetting rows by position ####

morph %>% 
  slice(1)

morph %>% 
  slice(1:3)

morph %>% 
  slice(c(1, 3, 10, 25))

morph %>% 
  group_by(species) %>% 
  slice(1)

morph %>% 
  group_by(species, sex) %>% 
  slice(1)

# Putting it all together ####

## Task 1 ####

# Count how many individuals for each combination of species and sex weigh more
# than 4 kg. 

over4kg <- morph %>% 
  mutate(body_mass_kg = body_mass_g/1000) %>% 
  filter(body_mass_kg > 4) %>% 
  group_by(species, sex) %>% 
  tally()

## Task 2 ####

# What proportion of the total number of individuals is that?
n_by_species_sex <- morph %>% 
  group_by(species, sex) %>% 
  tally()

over4kg %>% 
  left_join(n_by_species_sex, by = c("species", "sex")) %>% 
  rename(n_4kg = n.x, n = n.y) %>% 
  mutate(prop_4kg = n_4kg/n)

## Task 3 ####

# It looks like Gentoo penguins are generally much bigger than Adelie and 
# Chinstrap. How to they compare in terms of flipper length? What is the 
# minimum, mean, and maximum flipper length for each species?
morph %>% 
  group_by(species) %>% 
  summarize(min = min(flipper_length_mm),
            mean = mean(flipper_length_mm),
            max = max(flipper_length_mm))

morph %>% 
  group_by(species) %>% 
  summarize(min = min(flipper_length_mm, na.rm = TRUE),
            mean = mean(flipper_length_mm, na.rm = TRUE),
            max = max(flipper_length_mm, na.rm = TRUE))

# Data visualization with ggplot2 ####

## 1. Boxplots of body mass by species ####

ggplot(p, aes(y = body_mass_g, x = species, color = species)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  theme_bw() +
  labs(x = "", y = "Body mass (g)", color = "Species") +
  theme(legend.position = "none") + 
  scale_color_brewer(palette = "Accent")

## 2. Density plot of flipper length by species ####

ggplot(p, aes(x = flipper_length_mm, fill = species)) +
  geom_density(alpha = 0.5) + 
  theme_bw() +
  labs(x = "Flipper length (mm)", y = "Density", fill = "Species") + 
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Accent")

## 3. Scatterplot of isotopic values by species and island ####

ggplot(p, aes(x = delta_13_c, y = delta_15_n, color = species)) +
  geom_point() +
  facet_wrap(~ island) +
  labs(x = "Delta 13C", y = "Delta 15N", color = "Species") +
  theme_bw() +
  scale_color_brewer(palette = "Accent")

## 4. Scatterplot of culmen depth and length by body mass and species ####

ggplot(p, aes(x = culmen_length_mm, y = culmen_depth_mm, 
              color = species, size = body_mass_g)) +
  geom_point() +
  scale_color_brewer(palette = "Accent") +
  labs(x = "Culmen length (mm)", y = "Culmen depth (mm)",
       color = "Species", size = "Body mass (g)") +
  theme_bw()

## 5. Add regression lines to previous plot ####

mod <- lm(culmen_depth_mm ~ culmen_length_mm * species, data = p)

new_data <- expand.grid(species = factor(unique(p$species)),
            culmen_length_mm = seq(min(p$culmen_length_mm, na.rm = TRUE),
                                   max(p$culmen_length_mm, na.rm = TRUE),
                                   length.out = 100))

preds <- predict(mod, newdata = new_data, se.fit = TRUE)

res <- new_data %>% 
  mutate(culmen_depth_mm = preds$fit,
         culmen_depth_lwr_ci = preds$fit - preds$se.fit * 1.96,
         culmen_depth_upr_ci = preds$fit + preds$se.fit * 1.96)

ggplot(p, aes(x = culmen_length_mm, y = culmen_depth_mm, color = species)) +
  geom_point(aes(size = body_mass_g)) +
  geom_ribbon(data = res, 
              mapping = aes(ymin = culmen_depth_lwr_ci,
                            ymax = culmen_depth_upr_ci,
                            fill = species),
              #fill = "gray80", 
              alpha = 0.3) +
  geom_line(data = res) +
  scale_color_brewer(palette = "Accent") +
  scale_fill_brewer(palette = "Accent") +
  labs(x = "Culmen length (mm)", y = "Culmen depth (mm)",
       color = "Species", fill = "Species", size = "Body mass (g)") +
  theme_bw()

## 6. Scatterplot of flipper and culmen length by body mass and sex, Adelie only ####

p %>% 
  filter(grepl("Adelie", species) &
           !is.na(sex)) %>% 
  ggplot(aes(x = culmen_length_mm, y = flipper_length_mm, 
              color = body_mass_g)) +
  geom_point(size = 2) +
  # geom_hline(yintercept = mean(p$flipper_length_mm, na.rm = TRUE), 
  #            lty = "dashed") +
  # geom_vline(xintercept = mean(p$culmen_length_mm, na.rm = TRUE), 
  #            lty = "dashed") +
  facet_wrap(~ sex) +
  scale_color_viridis_c() +
  labs(x = "Culmen length (mm)", y = "Flipper length (mm)",
       color = "Body mass (g)") +
  theme_bw()
