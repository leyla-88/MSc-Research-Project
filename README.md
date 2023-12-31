# MSc-Research-Project
Code used for my MSc research project, modelling coastal resilience provided natural habitats for the Polish Baltic coast under SSP2 levels of climate change for 2050 and 2100. The model inputs were prepared by formatting them in the correct format and making the necessary calculations for input into the InVEST Coastal Vulnerability model. 

The highest 10% of wind and wave data collected from Copernicus Marine Service for the Baltic sea was filtered and exported. As the wind and wave data sources were different, the coordinates were different so a join was not possible (due to no overlapping coordinates). Therefore a nearest neighbour algorithm was used to force the wave data points onto the nearest wind data points to allow for storm conditions to be modelled (where wind and wave data was needed for each point).

As no spatial data of sand dune distribution in Poland could be found, a shapefile of Natura 2000 habitats was filtered to only the sites containing dune habitats.
