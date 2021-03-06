# Grid based Accident Prediction using DNN


A road traffic accident is one of the serious problems related to normal daily life. The frequency of accidents is more in certain areas as compared with other areas due to various landmarks around it or the location of the area. The field of computer science has improved a lot in the recent area and it has opened various paths for solving the various problems in different fields. The grid-based approach for the prediction of locational data has proven to be a successful algorithm so we will focus on the integration of grid-based models with Deep Neural Network. Also, the previous frequency of incidence can predict the accident rate in an area that can classify the area as accidental prone area so we will also use the number of incidences in the previous time for training the model corresponding to each area.

## Pre-requirement 

* [R](https://www.r-project.org/) - Used to create DNN model
* [Google Maps Static Api ](https://developers.google.com/maps/documentation) - Used for final plotting of predicted hotspots


## Step to run
1. Pull the code and Setup R studio
2. Change Path for files in code (one for Data file and other for using Map shape file)
3. Download the libraries that are not installed in R Like ggmap, h2o, and any other library that is required.
4. Activate the Google Map Api from google console and run this command in Console
```R
register_google(key = "[your key]", write = TRUE)
```
5. Run the Code and Loading of Maps could take some time.

## Los Angeles MAP
![Not Found](Images/LA_map_AND_Grid.png)

## Intersection of Grid and Los Angeles MAP
![Not Found](Images/Intersection_LA_MAP.png)

## Remove the points with no accidents in the entire data
![Not Found](Images/Filtered_Intersection_LA_MAP.png)

## Model Design
Used h2o Library for running the deeplearning model with 7 Hidden Layer 100 Neuron each.
Learning Rate is 0.001 and activation is "RectifierWithDropout"

![Not Found](Images/DNN_Model.PNG)

## Total 8 Models:
1. Around 60% Training and 40% Testing.
2. Around 75% Training and 25% Testing.
3. Around 80% Training and 20% Testing.
4. Around 83% Training and 17% Testing.
5. Around 85% Training and 15% Testing.
6. Around 87.5% Training and 12.5% Testing.
7. Around 88.88% Training and 11.12% Testing.
8. Around 90% Training and 10% Testing.

### It is important to note that the number of testing data has to be kept the same for an interval because we have a predefined number of points on the map which can't be changed.

The total number of points on the maps is 3975 which can become a hotspot. 

## Derivation from Confusion Matrix for all the models 
![Not Found](Images/Confusion_Matrix_Results.PNG)

## Derivation from Confusion Matrix for all the models using Random Forest
![Not Found](Images/Confusion_Matrix_Results_Random%20Forest.PNG)

### Although the accuracy of Random Forest is good but still DNN's accuracy is better

## Predicted Hotspot for Model 1
![Not Found](Images/MAP.png)

## Real Hotspot for Model 1
![Not Found](Images/MAP_actual.png)

## Training Loss for Model 1
![Not Found](Images/Training%20Loss.png)

