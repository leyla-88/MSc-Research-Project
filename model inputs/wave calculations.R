# Where X=[0,22,45,67,90,112,135,157,180,202,225,247,270,292,315,337]
# Calculating WavP_X, representing the highest 10% of wave power values in 16 equiangular directions
# Calculating WavPPCTX, corresponding to the proportion of the highest 10% wave power values which are centered on the main sector direction X

data <- read.csv("balticsea_extremewave2.csv", header=TRUE, stringsAsFactors=FALSE)
# head(data)

# Create a new data frame data2 with one row for each unique lat and lon combination
unique_coords <- unique(data[, c("lat", "lon")])
data2 <- data.frame(lat = numeric(nrow(unique_coords)),
                    lon = numeric(nrow(unique_coords)))

# Define the bin boundaries
bins <- seq(0, 360, by = 22.5)

# Iterate over the unique lat and lon combinations
for (i in 1:nrow(unique_coords)) {
  lat <- unique_coords[i, "lat"]
  lon <- unique_coords[i, "lon"]
  
  # Subset the data for the current lat and lon
  subset_data <- data[data$lat == lat & data$lon == lon, ]
  
  # Initialize a list to store the average wave powers for each bin
  avg_powers <- list()
  
  # Iterate over the bins
  for (j in 1:length(bins)) {
    bin_lower <- bins[j]
    bin_upper <- ifelse(j == length(bins), bins[1], bins[j+1])  # Handle last bin wrapping around
    bin_name <- paste("WavP_", bin_lower, sep = "")
    
    # Subset the data within the bin boundaries
    bin_data <- subset_data[subset_data$wave_from_direction >= bin_lower & subset_data$wave_from_direction < bin_upper, ]
    
    # Check if there are any non-numeric values in the wave_power column
    if (any(!is.numeric(bin_data$wave_power))) {
      avg_power <- NA  # Set average power to NA if non-numeric values exist
    } else {
      # Calculate the average wave_power for the bin
      avg_power <- mean(as.numeric(bin_data$wave_power), na.rm = TRUE)
    }
    
    # Store the average power in the list
    avg_powers[[bin_name]] <- avg_power
  }
  
  # Calculate the sum of average powers
  sum_avg_powers <- sum(unlist(avg_powers), na.rm = TRUE)
  
  if (length(avg_powers) == 0) {
    # If no valid average powers are calculated, set all percent values to 0
    for (j in 1:length(bins)) {
      bin_lower <- bins[j]
      ppctx_name <- paste("WavPPCT", bin_lower, sep = "")
      data2[i, ppctx_name] <- 0
    }
  } else {
    # Iterate over the bins again to calculate the percent values
    for (j in 1:length(bins)) {
      bin_lower <- bins[j]
      bin_upper <- ifelse(j == length(bins), bins[1], bins[j+1])  # Handle last bin wrapping around
      bin_name <- paste("WavP_", bin_lower, sep = "")
      ppctx_name <- paste("WavPPCT", bin_lower, sep = "")
      
      # Get the average power for the bin
      avg_power <- avg_powers[[bin_name]]
      
      # Check if the average power is NA
      if (is.na(avg_power)) {
        ppctx <- 0  # Set percent value to 0 if average power is NA
      } else {
        # Calculate the percent value
        ppctx <- avg_power / sum_avg_powers
      }
      
      # Add the percent value to data2
      data2[i, ppctx_name] <- ppctx
    }
  }
  
  # Add the lat and lon to data2
  data2[i, "lat"] <- lat
  data2[i, "lon"] <- lon
  data2[i, names(avg_powers)] <- unlist(avg_powers)
}

head(data2)

library(dplyr)
data2fix <- data2 %>% select(-WavPPCT360, -WavP_360)
head(data2fix)

names(data2fix) <- c("lat", "lon", "WavPPCT0", "WavPPCT22", "WavPPCT45", "WavPPCT67", "WavPPCT90", "WavPPCT112",
                     "WavPPCT135", "WavPPCT157", "WavPPCT180", "WavPPCT202", "WavPPCT225", "WavPPCT247",
                     "WavPPCT270", "WavPPCT292", "WavPPCT315", "WavPPCT337","WavP_0", "WavP_22", "WavP_45", "WavP_67", "WavP_90", "WavP_112",
                     "WavP_135", "WavP_157", "WavP_180", "WavP_202", "WavP_225", "WavP_247",
                     "WavP_270", "WavP_292", "WavP_315", "WavP_337")

data2fix <- lapply(data2fix, function(x) replace(x, is.nan(x), 0))

my_dataframe2 <- do.call(data.frame, data2fix)
head(my_dataframe2)

wave_data_fixed <- my_dataframe2 %>% 
  relocate(lat, lon, WavP_0, WavP_22, WavP_45, WavP_67, WavP_90, WavP_112,
           WavP_135, WavP_157, WavP_180, WavP_202, WavP_225, WavP_247,
           WavP_270, WavP_292, WavP_315, WavP_337)

head(wave_data_fixed)

names(wave_data_fixed) <- c("lat", "lon", "WavP_0", "WavP_22", "WavP_45", "WavP_67", "WavP_90", "WavP_112",
                            "WavP_135", "WavP_157", "WavP_180", "WavP_202", "WavP_225", "WavP_247",
                            "WavP_270", "WavP_292", "WavP_315", "WavP_337", "Wav_PPCT0", "Wav_PPCT22", "Wav_PPCT45", "Wav_PPCT67", "Wav_PPCT90", "Wav_PPCT112",
                            "Wav_PPCT135", "Wav_PPCT157", "Wav_PPCT180", "Wav_PPCT202", "Wav_PPCT225", "Wav_PPCT247",
                            "Wav_PPCT270", "Wav_PPCT292", "Wav_PPCT315", "Wav_PPCT337")
                   
write.csv(wave_data_fixed, "wave_data.csv", row.names = FALSE)


