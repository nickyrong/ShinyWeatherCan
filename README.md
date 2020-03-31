---
output: 
  html_document: 
    keep_md: yes
---


<br/>

**Authors:** <br/>
Nick Rong ([nickyrong](https://github.com/nickyrong)) <br/>
Nate Smith ([WraySmith](https://github.com/WraySmith)) <br/><br/>
Last Update 03/28/2020 <br/>
Last Change: added interactive available data plot using plotly <br/>

### Overview
This app allows the user to enter a station ID to access climate data from:
[Environment and Climate Change Canada (ECCC)](https://climate.weather.gc.ca/historical_data/search_historic_data_e.html).
<br/>

Currently the ECCC website only allows data downloads of one month at a time. The current scope of this app is to minimize repetition for downloading data.
<br/>

Functionality in this ShinyApp is provided by [weathercan package](https://github.com/ropensci/weathercan). This R package is developed and maintained by members of the rOpenSci (main contributors include Dr. Steffi LaZerte and Sam Albers, Data Scientist in BC Goverment's Data Science & Analytics Unit).

<br/>
<img src="https://github.com/ropensci/weathercan/raw/master/inst/assets/weathercan_logo.png" width="10%" />
<br/>


### Usage:

Currently the app is hosted on [shinyapps.io](https://nickrongkp.shinyapps.io/WeatherCan/) with a free account. Please be mindful of others and close the app/browser tab as soon as possible when you are done to conserve resources for others, thank you.

**Data are downloaded on-the-fly from ECCC. Please be patient as the downloads can be fairly large (especially for hourly and daily data).**

Check the Station Map to identify potential stations and their IDs. If the map is not visible, it is still being loaded.

Enter station ID once the station meta-data are loaded (the `Enter ID` box will appear)

The `Data Table` tab presents a preview of data. Users may download the data as a .csv for further processing and analysis.
<br/>

### Future updates

Data visualization & screening may be included in future updates.
<br/>

### Feedback & Suggestions

Please contact [Nick](https://github.com/nickyrong) or [Nate](https://github.com/WraySmith).
<br/>
