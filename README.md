# shiny-opensensemap
A shiny interface to display temperature data anomalies in Nordrhein Westfalen(Germany) collected from opensensmapR using Leaflet. Defective boxes and potential data anomalies are displayed on the map with box ID and the temperature value. Anomalies are found using different outlier detection methods. The two options for outlier detection in the app are cook's distance and IQR (interquartile range). 

Cook's distance anomaly detection: Cook's distance is a measure of the influence of a data point shen performing a least squares regression. In the app, this works by first creating a linear regression model of the temperature versus created at values. The a point of influence is defined as a point that has a cook's distance higher than (4 / number of observations). These points of influence are then marked as defective boxes because they have a very high cook's distance. After this, a model of the clean data is created and the cook's distance is used again to find points of influence. These points of influence are weather anomalies in the region. 

To see Cook's distance in action or to learn more check these links:
https://pdfs.semanticscholar.org/c278/532e84557bb54fdb8757d0fa5b7693fd4ca3.pdf
https://demonstrations.wolfram.com/InfluentialPointsInRegression/


IQR anomaly detection: IQR, or interquartile range, is the difference between the 75th percentile of data and the 25th percentile of the data (IQR = Q3 - Q1). To find outliers using IQR, the app finds the IQR of the temperature values in Nordrhein Westfalen. It then uses the (1.5 * IQR) to determine which values are outliers. If an observation is less than the 25th percentile of data minus (1.5 * IQR), then it is a low outlier(Q1 - (1.5 * IQR)). If an observation is more than the 75th percentile of data plus (1.5 * IQR), then it is a high outlier(Q3 + (1.5 * IQR)). This method found the defective boxes which were extreme low outliers. After cleaning the outliers from the data, the app finds values more than 2 standard deviations away from the mean of the temperature values which are then flagged as potential data anomalies. 


Link to the shiny app is https://jammurz.shinyapps.io/shiny-opensensemap/
