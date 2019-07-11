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

#Load required libraries
library(tidyverse)
library(lubridate)
library(sf)
library(sp)
library(units)
library(opensensmapr)




find_outliers_iqr_sd <<- function(df){
  range_iqr <- 1.5 * IQR(df$value)
  upper_iqr <- quantile(df$value, .75) + range_iqr
  lower_iqr <- quantile(df$value, .25) - range_iqr
  
  outliers <<- df$value[df$value > upper_iqr | df$value < lower_iqr]
  
  defective_boxes_iqr <<- data.frame(value = outliers, box_id = region_boxes$X_id[phenom_df$sensorId[phenom_df$value %in% outliers]], lon = df$lon[df$value %in% outliers], lat = df$lat[df$value %in% outliers])
  
  clean_defective_boxes_iqr <<- phenom_df %>%
    filter(!phenom_df$value %in% defective_boxes_iqr$value) %>%
    select(value, lon, lat)
  
  sd_outliers <<- clean_defective_boxes_iqr$value[(clean_defective_boxes_iqr$value > mean(clean_defective_boxes_iqr$value) + 2 * sd(clean_defective_boxes_iqr$value)) | (clean_defective_boxes_iqr$value < mean(clean_defective_boxes_iqr$value) - 2 * sd(clean_defective_boxes_iqr$value))]
  
  potential_anomalies_iqr <<- data.frame(value = sd_outliers, box_id = region_boxes$X_id[df$sensorId[df$value %in% sd_outliers]], lon = df$lon[df$value %in% sd_outliers], lat = df$lat[df$value %in% sd_outliers])
  
  normal_iqr_values <<- clean_defective_boxes_iqr %>%
    filter(!clean_defective_boxes_iqr$value %in% potential_anomalies_iqr$value) %>%
    select(value, lon, lat)
  
  normal_iqr_values_df <<- data.frame(value = normal_iqr_values$value, box_id = region_boxes$X_id[normal_iqr_values$value], lat = normal_iqr_values$lat, lon = normal_iqr_values$lon)
}


#Find defective boxes in a region based on a linear regression model and cooks distance
find_defective <<- function(model, df, region){
  
  cooksd <- cooks.distance(model)
  influential <- as.numeric(names(cooksd)[(cooksd > (4/nrow(df)))])
  influential_df <<- data.frame(value = df$value[influential], box_id = region$X_id[df$sensorId[influential]], lat = df$lat[influential], lon = df$lon[influential])
  influential_boxes <<- unique(influential_df)

  return(influential_boxes)
}


#Find potential data anomalies in a region based on a clean linear regression model and cooks distance
find_potential_anomaly <<- function(clean_model, clean_df, region){
  
  clean_cooks <<- cooks.distance(clean_model)
  clean_influential <- as.numeric(names(clean_cooks)[(clean_cooks > (4/nrow(clean_df)))])
  local_anomaly_df <<- data.frame(value=clean_df$value[clean_influential], box_id = region$X_id[clean_df$sensorId[clean_influential]], lat = clean_df$lat[clean_influential], lon = clean_df$lon[clean_influential])
  local_anomaly_boxes <<- unique(local_anomaly_df)
  return(local_anomaly_df)
  
 
}

#Get and return anomalies for a whole region
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
  find_outliers_iqr_sd(phenom_df)
  
  #Create linear model
  overall_model <- lm(value ~ createdAt, data = phenom_df, na.action=na.omit)
  find_defective(overall_model, phenom_df, region_boxes)
  
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


#Find and create data frames for IQR and standard deviation detection
#Detects outliers using 1.5 * IQR rule and then uses standard deviation detection finding data 2 standard deviations away from the mean

get_region_anomalies("Temperatur", st_bbox(st_multipoint(matrix(c(5.8664, 9.4623, 50.3276, 52.5325), 2, 2))))
