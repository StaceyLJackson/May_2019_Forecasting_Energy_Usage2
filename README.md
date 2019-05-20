May_2019_Forecasting_Energy_Usage2

Forecasting energy usage: forecasting energy usage


PROJECT GOAL

Use time series regression modeling to forecasting energy usage in a smart home. And use Shiny to produce a prototype of a dashboard that will enable homeowners to manage their usage.

SUPPLIED DATA

Data Source: http://archive.ics.uci.edu/ml/datasets/Individual+household+electric+power+consumption

TECHNICAL APPROACH

LANGUAGE USED: R

1. INITIAL EXPLORATION
SQL queries
Identify missing values and replace them with averages for that variable

2. TIME SERIES 
Adjust granualrities and group by day, week and month
Produce time series and their decompositions into season, trend and remainder
Visualise these to see patterns

3. FORECASTING
Holt Winters model
Arima model

4. MAKE PREDICTIONS AND ANALYSE ACCURACY OF THE MODELS

5. USE SHINY TO PRODUCE A PROTYPE DASHBOARD
