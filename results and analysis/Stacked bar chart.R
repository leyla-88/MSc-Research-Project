setwd("/Users/leyla/Documents/Education/Imperial/MSc Environmental Technology/Research project/Coastal_Vulnerability_Data/Geomorphology")

library(dplyr)
library(tidyr)
library(ggplot2)

# Assuming your data is in habitat_data DataFrame
habitat_data <- read.csv("/Users/leyla/Documents/Education/Imperial/MSc Environmental Technology/Research project/Results and analysis/Mean hab role 2050.csv")

# Define a constant to reverse the scale
constant_value <- 5

# Transform the Contribution values to reverse the scale
habitat_data_reversed <- habitat_data %>%
  mutate_at(vars(Dunes:Estuaries), list(~ constant_value - .))

head(habitat_data_reversed)

# Select the habitat columns
habitat_columns <- c("Dunes", "Seagrass", "Forest", "Marshes", 
                     "Heathland.and.Moorland", "Lakes", 
                     "Lagoons", "Peat.bogs", "Grassland", 
                     "Watercourses", "Estuaries")

# Calculate row sums
row_sums <- habitat_data_reversed %>%
  select(habitat_columns) %>%
  rowSums(na.rm = TRUE)

# Divide each habitat column by the corresponding row sum
habitat_data_percent <- habitat_data_reversed %>%
  mutate_at(vars(habitat_columns), ~ . / row_sums)

head(habitat_data_percent)
write.csv(habitat_data_percent, file = "habitat_data_percent.csv", row.names = FALSE)

# Reshape the data for plotting
habitat_data_long <- habitat_data_percent %>%
  pivot_longer(cols = Dunes:Estuaries, names_to = "Habitat", values_to = "Protective_Role")

habitat_colors <- c(
  Dunes = "lightyellow",
  Seagrass = "turquoise4",
  Forest = "darkolivegreen2",
  Marshes = "mediumaquamarine",
  Heathland.and.Moorland = "thistle2",
  Lakes = "deepskyblue1",
  Lagoons = "deepskyblue3",
  Peat.bogs = "orange4",
  Grassland = "springgreen",
  Watercourses = "skyblue1",
  Estuaries = "cyan"
)

dodge <- position_dodge(width = 0.5)

# Create a reversed stacked bar plot using ggplot2
ggplot(habitat_data_long, aes(x = X, y = Protective_Role, width=.5, fill = Habitat)) +
  geom_bar(stat = "identity", position = dodge) +
  labs(x = "Jurisdiction", y = "Protective Role") +
  theme_classic() +
  scale_fill_manual(values = habitat_colors) +
  geom_col(colour = "gray25") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,1.0001)) 
