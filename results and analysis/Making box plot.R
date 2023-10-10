data <- read.csv("Coastal_Vulnerability_Data/Workspace/coastal_exposure_2050_3d.csv", header = TRUE, stringsAsFactors = FALSE)

library(ggplot2)

# Define the binning categories and labels
bins <- list(
  list(jurisdiction = "Szczecin", start = 0, end = 87),
  list(jurisdiction = "Koszalin", start = 88, end = 189),
  list(jurisdiction = "Słupsk", start = 190, end = 258),
  list(jurisdiction = "Gdańsk", start = 259, end = 538)
)

# Assign jurisdiction names based on the binning categories
for (bin in bins) {
  data$jurisdiction[data$shore_id >= bin$start & data$shore_id <= bin$end] <- bin$jurisdiction
}

data$jurisdiction <- factor(data$jurisdiction, levels = c("Szczecin", "Koszalin", "Słupsk", "Gdańsk"))

# Single box plot

# p <- ggplot(data, aes(x = jurisdiction, y = exposure)) +
#     geom_boxplot(outlier.shape = 4) +  # Use outlier.shape = 4 for a cross
#    labs(x = "Jurisdiction", y = "Exposure") +
#    ggtitle("Exposure by District Court Jurisdiction") +
#    theme_minimal()
# 
# p + 
#   stat_summary(fun = mean, geom = "point", shape = 23, size = 3) +
#   guides(shape = guide_legend(title = "Statistic", override.aes = list(shape = 23)))



# # Double box plots for EI with and without habitat

# # Create a new data frame for double box plotting
# double_box_data <- rbind(
#   transform(data, Habitat_Scenario = "With habitats", value = exposure),
#   transform(data, Habitat_Scenario = "Without Habitats", value = exposure_no_habitats)
# )
# 
# # Create the double box plot using ggplot2
# p <- ggplot(double_box_data, aes(x = jurisdiction, y = value, fill = Habitat_Scenario)) +
#   geom_boxplot(position = position_dodge(width = 0.75), width = 0.6, outlier.shape = 4) +
#   labs(x = "Jurisdiction", y = "Exposure") +
#   # ggtitle("Coastal Vulnerability in Poland for 2050") +
#   scale_fill_manual(values = c("With habitats" = "lightgreen", "Without Habitats" = "lightpink")) +
#   theme_classic() +
#   scale_y_continuous(expand = c(0,0), limits = c(1, 5), breaks = seq(0, 5, by = 1)) +
#   geom_vline(xintercept = c(1.5, 2.5, 3.5), color = "darkgray", linetype = "dashed", linewidth = 0.5)
# 
# p


# # Double box plots for 2050 and 2100 EIs
# data <- read.csv("Results and analysis/2100 and 2050 exposures.csv", header = TRUE, stringsAsFactors = FALSE)
# 
# bins <- list(
#   list(jurisdiction = "Szczecin", start = 0, end = 87),
#   list(jurisdiction = "Koszalin", start = 88, end = 189),
#   list(jurisdiction = "Słupsk", start = 190, end = 258),
#   list(jurisdiction = "Gdańsk", start = 259, end = 538)
# )
# 
# # Assign jurisdiction names based on the binning categories
# for (bin in bins) {
#   data$jurisdiction[data$shore_id >= bin$start & data$shore_id <= bin$end] <- bin$jurisdiction
# }
# 
# data$jurisdiction <- factor(data$jurisdiction, levels = c("Szczecin", "Koszalin", "Słupsk", "Gdańsk"))
# 
# # Create a new data frame for double box plotting
# double_box_data <- rbind(
#   transform(data, Scenario = "2050", value = exposure_2050),
#   transform(data, Scenario = "2100", value = exposure_2100)
# )
# 
# # Create the double box plot using ggplot2
# p <- ggplot(double_box_data, aes(x = jurisdiction, y = value, fill = Scenario)) +
#   geom_boxplot(position = position_dodge(width = 0.75), width = 0.6, outlier.shape = 4) +
#   labs(x = "Jurisdiction", y = "Exposure") +
#   scale_fill_manual(values = c("2050" = "lightyellow", "2100" = "#9999CC")) +
#   theme_classic() +
#   scale_y_continuous(expand = c(0,0), limits = c(1, 4), breaks = seq(0, 4, by = 1)) +
#   geom_vline(xintercept = c(1.5, 2.5, 3.5), color = "darkgray", linetype = "dashed", linewidth = 0.5)
# 
# p

# Double box plots for 2050 and 2100 habitat roles


# Create the double box plot using ggplot2
double_box_data <- rbind(
  transform(data, Habitat_role = "2050", value = habitat_role_2050),
  transform(data, Habitat_role = "2100", value = habitat_role_2100)
)

p <- ggplot(double_box_data, aes(x = jurisdiction, y = value, fill = Habitat_role)) +
  geom_boxplot(position = position_dodge(width = 0.75), width = 0.6, outlier.shape = 4) +
  labs(x = "Jurisdiction", y = "Habitat role") +
  scale_fill_manual(values = c("2050" = "lightyellow", "2100" = "#9999CC")) +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0), limits = c(0.25, 1), breaks = seq(0.25, 1, by = 0.25)) +
  geom_vline(xintercept = c(1.5, 2.5, 3.5), color = "darkgray", linetype = "dashed", linewidth = 0.5)

p
