# Load necessary libraries
library(ggplot2)
library(dplyr)
 
# 2050 exposure

data <- read.csv("Coastal_Vulnerability_Data/Workspace/coastal_exposure_2050_3d.csv", header = TRUE, stringsAsFactors = FALSE)

custom_breaks <- seq(0, 4, by = 0.125)
custom_labels <- seq(0, 4, by = 0.125)

# Histogram
ggplot(data, aes(x = exposure)) +
  geom_histogram(binwidth = 0.125, fill = "lightblue", color = "black") +
  scale_x_continuous(breaks = custom_breaks, labels = custom_labels) +
  labs(x = "Exposure index",
       y = "Frequency") +
  theme_classic() +
  scale_y_continuous(expand = c(0,0))

# Q-Q Plot
qq_data <- data %>%
  arrange(exposure) %>%
  mutate(rank = seq(1, n())) %>%
  mutate(normal_quantile = qnorm((rank - 0.5) / n()))

ggplot(qq_data, aes(x = normal_quantile, y = exposure)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(x = "Theoretical Normal Quantiles",
       y = "Sample Quantiles") +
  theme_classic() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,4.0001))

# 2100 exposure

data2 <- read.csv("Coastal_Vulnerability_Data/Workspace/coastal_exposure_2100_3d.csv", header = TRUE, stringsAsFactors = FALSE)

# Histogram
ggplot(data2, aes(x = exposure)) +
  geom_histogram(binwidth = 0.125, fill = "lightblue", color = "black") +
  labs(x = "Exposure index",
       y = "Frequency") +
  theme_classic() +
  scale_y_continuous(expand = c(0,0))

# Q-Q Plot
qq_data2 <- data2 %>%
  arrange(exposure) %>%
  mutate(rank = seq(1, n())) %>%
  mutate(normal_quantile = qnorm((rank - 0.5) / n()))

ggplot(qq_data2, aes(x = normal_quantile, y = exposure)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(x = "Theoretical Normal Quantiles",
       y = "Sample Quantiles") +
  theme_classic() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,4.0001))
