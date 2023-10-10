# Load required library
library(tidyverse)

setwd("/Users/leyla/Documents/Education/Imperial/MSc Environmental Technology/Research project")


# Checking if there is any significant difference between 2050 and 2100

data <- read.csv('Results and analysis/2100 and 2050 exposures.csv')

# Calculate variances to see if they are similar enough to use student's t-test
variance_2050 <- var(data$exposure_2050)
variance_2100 <- var(data$exposure_2100)

cat("Variance for 2050:", variance_2050, "\n")
cat("Variance for 2100:", variance_2100, "\n")

if (variance_2050 / variance_2100 >= 0.5 && variance_2050 / variance_2100 <= 2) {
  cat("Variances are approximately equal.")
} else {
  cat("Variances are not approximately equal. Consider using the Welch Two Sample t-test.")
}

# Perform Student's t-test
t_test_result <- t.test(data$exposure_2050, data$exposure_2100, var.equal = TRUE)

print(t_test_result)


# Checking if there is any significant difference between 2050 and 2100 without habitats
data <- read.csv('Results and analysis/2100 and 2050 exposures.csv')

t_test_nohab50100 <- t.test(data$exposure_no_habitats_2050, data$exposure_no_habitats_2100, var.equal = TRUE)
print(t_test_nohab50100)

# Checking to see if habitats had a significant role

data2050 <- read.csv("Coastal_Vulnerability_Data/Workspace/coastal_exposure_2050_3d.csv", header = TRUE, stringsAsFactors = FALSE)
data2100 <- read.csv("Coastal_Vulnerability_Data/Workspace/coastal_exposure_2050_3d.csv", header = TRUE, stringsAsFactors = FALSE)

t_test_result2050 <- t.test(data2050$exposure, data2050$exposure_no_habitats, var.equal = TRUE)
print(t_test_result2050)

t_test_result2100 <- t.test(data2100$exposure, data2100$exposure_no_habitats, var.equal = TRUE)
print(t_test_result2100)


# Checking if there is a difference in habitat role for 2050 and 2100

t_test_resulthabrole <- t.test(data$habitat_role_2050, data$habitat_role_2100, var.equal = TRUE)
print(t_test_resulthabrole)

# Checking for regional difference between 2050 and 2100

Słupsk2050 <- read.csv("Results and analysis/jurisdictions/2050_Słupsk.csv")
Słupsk2100 <- read.csv("Results and analysis/jurisdictions/2100_Słupsk.csv")

t_test_Słupsk <- t.test(Słupsk2050$exposure, Słupsk2100$exposure, var.equal = TRUE)
print(t_test_Słupsk)

Gdańsk2050 <- read.csv("Results and analysis/jurisdictions/2050_Gdańsk.csv")
Gdańsk2100 <- read.csv("Results and analysis/jurisdictions/2100_Gdańsk.csv")

t_test_Gdańsk <- t.test(Gdańsk2050$exposure, Gdańsk2100$exposure, var.equal = TRUE)
print(t_test_Gdańsk)

Szczecin2050 <- read.csv("Results and analysis/jurisdictions/2050_Szczecin.csv")
Szczecin2100 <- read.csv("Results and analysis/jurisdictions/2100_Szczecin.csv")

t_test_Szczecin <- t.test(Szczecin2050$exposure, Szczecin2100$exposure, var.equal = TRUE)
print(t_test_Szczecin)

Koszalin2050 <- read.csv("Results and analysis/jurisdictions/2050_Koszalin.csv")
Koszalin2100 <- read.csv("Results and analysis/jurisdictions/2100_Koszalin.csv")

t_test_Koszalin <- t.test(Koszalin2050$exposure, Koszalin2100$exposure, var.equal = TRUE)
print(t_test_Koszalin)

Hel <- read.csv("Results and analysis/jurisdictions/Hel 2100 and 2050 exposures.csv")
t_test_Hel_years <- t.test(Hel$exposure_2050, Hel$exposure_2100, var.equal = TRUE)
print(t_test_Hel_years)


# Comparing Gdansk vulnerability
Gdansk_city2050 <- read.csv("Data analysis/cv_2050_gdansk_city.csv")
noGdansk_2050 <- read.csv("Data analysis/coastal_exposure_2050_3d_no_Gdansk_city.csv")

Gdansk_city2100 <- read.csv("Data analysis/cv_2100_gdansk_city.csv")
noGdansk_2100 <- read.csv("Data analysis/coastal_exposure_2100_3d_no_Gdansk_city.csv")

# Calculate variances to see if they are similar enough to use student's t-test
variance_Gdansk_city <- var(Gdansk_city2050$exposure)
variance_noGdansk_2050 <- var(noGdansk_2050$exposure)

cat("Variance for Gdansk city:", variance_Gdansk_city, "\n")
cat("Variance for else:", variance_noGdansk_2050, "\n")

if (variance_Gdansk_city / variance_noGdansk_2050 >= 0.5 && variance_Gdansk_city / variance_noGdansk_2050 <= 2) {
  cat("Variances are approximately equal.")
} else {
  cat("Variances are not approximately equal. Consider using the Welch Two Sample t-test.")
}

library(stats)

# Perform the F-test
f_test_result <- var.test(noGdansk_2100$exposure, Gdansk_city2100$exposure)
print(f_test_result)

t_test_Gdansk_city <- t.test(noGdansk_2100$exposure, Gdansk_city2100$exposure, var.equal = FALSE)
print(t_test_Gdansk_city)


# Testing jurisdictions

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

# Calculate variance for jurisdiction "Koszalin"
variance_koszalin <- var(data$exposure_2050[data$jurisdiction == "Koszalin"])
variance_Gdańsk <- var(data$exposure_2050[data$jurisdiction == "Gdańsk"])

cat("Variance for Koszalin:", variance_koszalin, "\n")
cat("Variance for Gdańsk:", variance_Gdańsk, "\n")

if (variance_koszalin / variance_Gdańsk >= 0.5 && variance_koszalin / variance_Gdańsk <= 2) {
  cat("Variances are approximately equal.")
} else {
  cat("Variances are not approximately equal. Consider using the Welch Two Sample t-test.")
}


