#Find and create data frames for IQR and standard deviation detection
#Detects outliers using 1.5 * IQR rule and then uses standard deviation detection finding data 2 standard deviations away from the mean

find_outliers_iqr_sd <- function(df){
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

  normal_iqr_values <<- data.frame(value = normal_iqr_values$value, box_id = region_boxes$X_id[normal_iqr_values$value], lat = normal_iqr_values$lat, lon = normal_iqr_values$lon)
  
}

find_outliers_iqr_sd(phenom_df)