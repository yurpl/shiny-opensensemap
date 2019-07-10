# Copyright 2019 John Murzaku
#    
#    Permission is hereby granted, free of charge, to any person obtaining 
#    a copy of this software and associated documentation files (the "Software"), 
#    to deal in the Software without restriction, including without limitation the 
#    rights to use, copy, modify, merge, publish, distribute, sublicense, and/or 
#    sell copies of the Software, and to permit persons to whom the Software is 
#    furnished to do so, subject to the following conditions:
#    
#    The above copyright notice and this permission notice shall be included in 
#    all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE 
# SOFTWARE.
#
# Function to get sensebox data and find influential values
#
# @param phenom Returns boxes with given phenomen
# @param bbox Specify bounding box using st_bbox to get boxes within a region
# @return Filtered box dataframe measurements with data anomalies
# @example get_region_anomalies("Temperatur", st_bbox(st_multipoint(matrix(c(5.8664, 9.4623, 50.3276, 52.5325), 2, 2))))
# Returns temperature anomalies in the region of Nordrhein-Westfalen


library(tidyverse)
library(lubridate)
library(sf)
library(sp)
library(units)
library(opensensmapr)

find_defective <<- function(model, df, region){
  
  cooksd <- cooks.distance(model)
  influential <- as.numeric(names(cooksd)[(cooksd > (4/nrow(df)))])
  influential_df <<- data.frame(value = df$value[influential], box_id = region$X_id[df$sensorId[influential]], lat = df$lat[influential], lon = df$lon[influential])
  influential_boxes <<- unique(influential_df)

  return(influential_boxes)
}

find_potential_anomaly <<- function(clean_model, clean_df, region){
  
  clean_cooks <<- cooks.distance(clean_model)
  clean_influential <- as.numeric(names(clean_cooks)[(clean_cooks > (4/nrow(clean_df)))])
  local_anomaly_df <<- data.frame(value=clean_df$value[clean_influential], box_id = region$X_id[clean_df$sensorId[clean_influential]], lat = clean_df$lat[clean_influential], lon = clean_df$lon[clean_influential])
  local_anomaly_boxes <<- unique(local_anomaly_df)
  return(local_anomaly_df)
  
 
}


get_region_anomalies <- function(phenom, bbox){
  #Get all the boxes
  boxes <- osem_boxes()
  
  #Filter boxes to region
  region_boxes <<- boxes %>%
    dplyr::filter(lon >= bbox$xmin & lon <= bbox$xmax & lat >= bbox$ymin & lat <= bbox$ymax) %>%
    dplyr::filter(!is.na(lastMeasurement))
  
  #Get phenom data frame  
  phenom_df <<- osem_measurements(
    bbox,
    exposure="outdoor",
    phenomenon = phenom,
    from = now() - minutes(5),
    to = now()
  )
  
  #Create linear model
  overall_model <- lm(value ~ createdAt, data = phenom_df, na.action=na.omit)
  find_influential(overall_model, phenom_df, region_boxes)
  
  clean_data <<- phenom_df %>%
    filter(!phenom_df$value %in% influential_df$value) %>%
    select(value, createdAt, sensorId, lat, lon)

  clean_model <- lm(value ~ createdAt, data=clean_data)
  find_potential_anomaly(clean_model, clean_data, region_boxes)
  
  normal_temp <<- clean_data %>%
    filter(!clean_data$value %in% local_anomaly_df$value) %>%
    select(value, createdAt, sensorId, lat, lon)
  
  normal_temp_df <<- data.frame(value = normal_temp$value, box_id = region_boxes$X_id[normal_temp$value], lat = normal_temp$lat, lon = normal_temp$lon)
  
}

get_region_anomalies("Temperatur", st_bbox(st_multipoint(matrix(c(5.8664, 9.4623, 50.3276, 52.5325), 2, 2))))
