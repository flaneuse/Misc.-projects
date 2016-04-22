
# load packages -----------------------------------------------------------

library(rgdal)
library(maptools)
library(rgeos)
library(dplyr)
library(ggplot2)



# Import shapefile data --------------------------------------------------------
# The process of converting any shapefile --> lat/lon points follows the same format for all:
# 1. use readOGR to read in the shapefile
# 2. pull out the row names and make into an id variable
# 3. use fortify to convert convert
# 4. merge the lat/lons with associated data from the original shapefile
# 5. save to .csv

# Here, I've made a little function that you can use to do this for you.

shp2csv = function(workingDir = getwd(),
                   layerName,
                   exportData = TRUE,
                   fileName = layerName){
  
  # Change directory to the file folder containing the shape file
  setwd(workingDir)
  
  # the dsn argument of '.' says to look for the layer in the current directory.
  rawShp = rgdal::readOGR(dsn=".", layer = layerName)
  
  # pull out the row names from the data and save it as a new column called 'id'
  rawShp@data$id = rownames(rawShp@data)
  
  # Convert the shape polygons into a series of lat/lon coordinates.
  poly_points = ggplot2::fortify(rawShp, region="id")
  
  # Merge the polygon lat/lon points with the original data
  df = dplyr::left_join(poly_points, rawShp@data, by="id")
  
  # if the 'exportData' option is selected, save the lat/lon coordinates as a .csv
  if (exportData == TRUE){
    write.csv(df, paste0(workingDir, '/', fileName, '.csv'))
  }
  
  # Return the dataframe
  return(df)
}


# Basic plot function to check that the data look correct -----------------
# Note: if you have lots of complex polygons, it'll take awhile to plot.  
# In that case, it's easier to save the plot to a variable and export the plot as a .pdf
# Don't be alarmed if the function takes a minute or two to render
plotMap = function(df, 
                   exportPlot = FALSE,
                   fileName = 'map.pdf',
                   plotWidth = 6,
                   plotHeight = 6){
  
  p = ggplot(df, aes(x = long, 
                     y = lat, 
                     group = group, 
                     fill = id)) +
    geom_polygon() +
    theme_void() + 
    coord_equal() +
    theme(legend.position = 'none')
  
  if (exportPlot == TRUE) {
    ggsave(filename = fileName,
           width = plotWidth, height = plotHeight,
           bg = 'transparent',
           paper = 'special',
           units = 'in',
           useDingbats=FALSE,
           compress = FALSE,
           dpi = 300)
  }
  
  return(p)
  
}


# Cambodia lakes/rivers data ------------------------------------------------------
lakes_df = shp2csv(workingDir = '~/Documents/USAID/mini projects/tableaupolygonsfromshapefiles/khm_WatrcrsA_wfp',
                  layerName = "khm_WatrcrsA_wfp",
                  exportData = TRUE)

plotMap(lakes_df)


# Cambodia Adm1 data ------------------------------------------------------
adm1_df = shp2csv(workingDir = '~/Documents/USAID/mini projects/tableaupolygonsfromshapefiles/khm_admbnda_adm1_gov',
                  layerName = "khm_admbnda_adm1_gov",
                  exportData = TRUE)

plotMap(adm1_df)

# Cambodia Adm2 data ------------------------------------------------------
adm2_df = shp2csv(workingDir = '~/Documents/USAID/mini projects/tableaupolygonsfromshapefiles/khm_admbnda_adm2_gov',
                  layerName = "khm_admbnda_adm2_gov",
                  exportData = TRUE)

plotMap(adm2_df)

# Cambodia Adm3 data ------------------------------------------------------
adm3_df = shp2csv(workingDir = '~/Documents/USAID/mini projects/tableaupolygonsfromshapefiles/khm_admbnda_adm3_gov',
                  layerName = "khm_admbnda_adm3_gov",
                  exportData = TRUE)

plotMap(adm3_df)
