# EnergyForecast
This is `Time Series Analysis` project with a visualization dashboard using R Shiny. The focus is to predict when will the Renewable Energy generation in the United States grow up to a certain percentage of the total energy production. However the dashboard can be used to draw multiple other conclusions and insights. <br>
The data I used for this project is directly gathered as a csv file from US Energy Information Administration. <br>
I made the basic probabilistic assumptions of `stationarity`, that is, *_the future will probabilistically remain same as the past or the present,_* necessary to fit any time series model. <br> 
I have tried to explore some basic and some major time series models. These models range from `Simple Trend` and `Seasonality` up to `Holt-winter` and `ARIMA`. <br>
With the kind of global focus on renewable energy sources, the growth of renewable energy production is exponentially on the rise. <br>
There are rda and csv files of the same dataset. The code has been run to work on both of them. <br>
The code for loading and running with the csv file has been commented out in `tsa.R` file. If the csv file is loaded two lines needed to be uncommented. One for loading the csv file and another for changing the date format of the `date` column in the csv file.<br>
Here is a link to view the dashboard directly. <br>
* https://aranya-kundu.shinyapps.io/us_energy_forecast/
