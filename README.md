# MSc-Research-Project
Code used for my MSc research project, modelling coastal resilience provided natural habitats for the Polish Baltic coast under SSP2 levels of climate change for 2050 and 2100. 

The highest 10% of wind and wave data collected from Copernicus Marine Service for the Baltic sea was filtered and exported. As the wind and wave data sources were different, the coordinates were different, so a nearest neighbour algorithm was used to force the wave data points onto the nearest wind data points to allow for storm conditions to be modelled (where wind and wave data was needed for each point).
