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
# Function to get sensebox data and find influential values using cook's distance method and IQR method
# get_bbox_data(phenom, bbox)
# @param phenom: The phenomena you are looking for. For example: "Temperatur", "PM2.5", etc.
# @param bbox: An st_bbox of the data boundaries. For example: st_bbox(st_multipoint(matrix(c(5.8664, 9.4623, 50.3276, 52.5325), 2, 2))) creates a bounding box of all the boxes in the Nordrhein Westfalen region
# @returns: data frames of clean data, potential anomalies, and defective boxes within the bounding box using IQR and cook's distance methods.

#Load required libraries
library(tidyverse)
library(lubridate)
library(sf)
library(sp)
library(units)
library(opensensmapr)

get_bbox_data <- function(phenom, bbox) {
  
  boxes <- osem_boxes(cache = getwd())
  
  #Filter boxes to region
  region_boxes <<- boxes %>%
    dplyr::filter(lon >= bbox$xmin & lon <= bbox$xmax & lat >= bbox$ymin & lat <= bbox$ymax) %>%
    dplyr::filter(!is.na(lastMeasurement))
  
  
  phenom_df <<- osem_measurements(
    bbox,
    phenomenon = phenom,
    from = now() - minutes(30),
    to = now()
  )
  
  model <- lm(value ~ createdAt, data = phenom_df, na.action=na.omit)
  
  cooksd <- cooks.distance(model)
  influential <- as.numeric(names(cooksd)[(cooksd > (4/nrow(phenom_df)))])
  influential_df <<- data.frame(value = phenom_df$value[influential], box_id = region_boxes$X_id[phenom_df$sensorId[influential]], lat = phenom_df$lat[influential], lon = phenom_df$lon[influential])
  influential_boxes <<- unique(influential_df)
  
  clean_data <<- phenom_df %>%
    filter(!phenom_df$value %in% influential_df$value) %>%
    select(value, createdAt, sensorId, lat, lon)
  
  clean_model <- lm(value ~ createdAt, data=clean_data)
  
  clean_cooks <<- cooks.distance(clean_model)
  clean_influential <- as.numeric(names(clean_cooks)[(clean_cooks > (4/nrow(clean_data)))])
  local_anomaly_df <<- data.frame(value=clean_data$value[clean_influential], box_id = region_boxes$X_id[clean_data$sensorId[clean_influential]], lat = clean_data$lat[clean_influential], lon = clean_data$lon[clean_influential])
  local_anomaly_boxes <<- unique(local_anomaly_df)
  
  normal_temp <<- clean_data %>%
    filter(!clean_data$value %in% local_anomaly_df$value) %>%
    select(value, createdAt, sensorId, lat, lon)
  
  normal_temp_df <<- data.frame(value = normal_temp$value, box_id = region_boxes$X_id[normal_temp$sensorId], lat = normal_temp$lat, lon = normal_temp$lon)
  
  range_iqr <- 1.5 * IQR(phenom_df$value)
  upper_iqr <- quantile(phenom_df$value, .75) + range_iqr
  lower_iqr <- quantile(phenom_df$value, .25) - range_iqr
  
  outliers <<- phenom_df$value[phenom_df$value > upper_iqr | phenom_df$value < lower_iqr]
  
  defective_boxes_iqr <<- data.frame(value = outliers, box_id = region_boxes$X_id[phenom_df$sensorId[phenom_df$value %in% outliers]], lon = phenom_df$lon[phenom_df$value %in% outliers], lat = phenom_df$lat[phenom_df$value %in% outliers])
  
  clean_defective_boxes_iqr <<- phenom_df %>%
    filter(!phenom_df$value %in% defective_boxes_iqr$value) %>%
    select(value, lon, lat, sensorId)
  
  sd_outliers <<- clean_defective_boxes_iqr$value[(clean_defective_boxes_iqr$value > mean(clean_defective_boxes_iqr$value) + 2 * sd(clean_defective_boxes_iqr$value)) | (clean_defective_boxes_iqr$value < mean(clean_defective_boxes_iqr$value) - 2 * sd(clean_defective_boxes_iqr$value))]
  
  potential_anomalies_iqr <<- data.frame(value = sd_outliers, box_id = region_boxes$X_id[phenom_df$sensorId[phenom_df$value %in% sd_outliers]], lon = phenom_df$lon[phenom_df$value %in% sd_outliers], lat = phenom_df$lat[phenom_df$value %in% sd_outliers])
  
  normal_iqr_values <<- clean_defective_boxes_iqr %>%
    filter(!clean_defective_boxes_iqr$value %in% potential_anomalies_iqr$value) %>%
    select(value, lon, lat, sensorId)
  
  normal_iqr_values_df <<- data.frame(value = normal_iqr_values$value, box_id = region_boxes$X_id[normal_iqr_values$sensorId], lat = normal_iqr_values$lat, lon = normal_iqr_values$lon)
}

get_bbox_data("Temperatur", st_bbox(st_multipoint(matrix(c(5.8664, 9.4623, 50.3276, 52.5325), 2, 2))))

