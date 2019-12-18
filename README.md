
<br/> **Authors:** <br/> Nick Rong (Junior Scientist at *Knight Piesold
Consulting*) <br/> Nate Smith (Senior Engineer at *Knight Piesold
Consulting*) <br/><br/> Last Update 12/17/2019 <br/> Last Change:
Updated ID enter to be case-insensitive; added data availability
screening <br/>

### Overview

This app allows the user to enter a station ID to access climate data
from: [Environment and Climate Change Canada (ECCC)
website](https://climate.weather.gc.ca/historical_data/search_historic_data_e.html)
<br/>

Currently the ECCC’s website only allows data download of each month at
a time. The current scope of this app is to minimize repetition for
downloading the long term data. Future update might include quick data
visualization and analysis. <br/>

Functionality in this ShinyApp is provided by [weathercan
package](https://github.com/ropensci/weathercan). This R package is
developed and maintained by the Provincial Government of British
Columbia. <br/>
<img src="https://github.com/ropensci/weathercan/raw/master/inst/assets/weathercan_logo.png" width="10%" />
<br/>

### Usage:

**Data are downloaded on-the-fly from ECCC. Please be patient as the
downloads can be fairly large (especially for hourly and daily data).**

Check Station Map to identify potential stations and retrieve their IDs.
If map is not presented, that means it is still loaded in the
background.

Enter station ID once the station meta-data are loaded (the `Enter ID`
box will appear)

`Data Table` presents a preview of the data table. Users may download
the data as `.csv` for further processing and analysis. <br/>

### Future updates

`weathercan{}` has built-in interpolation functions to estimate weather
in between stations. It has not been realized in this ShinyApp but is on
Nick’s to-do list.

Plotting will be included in the future updates. <br/>

### Feedback & Suggestions

Please contact [Nick](https://github.com/nickyrong). <br/>
