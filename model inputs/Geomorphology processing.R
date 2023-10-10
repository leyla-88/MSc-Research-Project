# Exporting a CSV list of all the coastal types

library(sf)

shapefile <- st_read("original.shp")

# Group the data by CoastType and collect unique values of Morpho in a list
grouped_data <- aggregate(shapefile$Morpho, by = list(shapefile$CoastType), unique)
print(grouped_data)

# Convert the grouped data into a data frame
grouped_data_df <- data.frame(CoastType = grouped_data$Group.1, Morpho = grouped_data$x)
print(grouped_data_df)

# Export the data frame as a CSV file
write.csv(grouped_data_df, file = "coastal types ranking.csv", row.names = FALSE)


# Filling the 'rank' field in the "EMOD Coastal Type.shp" shapefile with the ranking values from the "coastal types ranking.csv" file 

library(dplyr)

grouped_data <- read.csv("coastal types ranking.csv")

# Merge the shapefile with the grouped data based on 'Morpho'
merged_shapefile <- left_join(shapefile, grouped_data, by = "Morpho")

colnames(merged_shapefile)

# Create a new 'CoastType' field by combining 'CoastType.x' and 'CoastType.y'
merged_shapefile$CoastType <- ifelse(is.na(merged_shapefile$CoastType.x), 
                                     merged_shapefile$CoastType.y, 
                                     merged_shapefile$CoastType.x)

# Remove 'CoastType.x' and 'CoastType.y' fields
merged_shapefile$CoastType.x <- NULL
merged_shapefile$CoastType.y <- NULL

updated_sf <- merged_shapefile %>% 
  relocate(Morpho, CoastType)
colnames(updated_sf)
head(updated_sf)

for (col in names(updated_sf)) {
  if (is.character(updated_sf[[col]])) {
    updated_sf[[col]] <- iconv(updated_sf[[col]], from = "UTF-8", to = "UTF-8")
  }
}

updated_sf$rank <- as.integer(updated_sf$rank)

# Save the updated shapefile
st_write(updated_sf, "EMOD Coastal Type 3.shp")
