## Authors 

* [Nick Rong](https://github.com/nickyrong)
* [Nate Smith](https://github.com/WraySmith)
<br/>

## Overview
This app allows the user to enter a station ID to access climate data from:
[Environment and Climate Change Canada (ECCC)](https://climate.weather.gc.ca/historical_data/search_historic_data_e.html).
<br/>

Currently the ECCC website only allows data downloads of one month at a time. The current scope of this app is to minimize repetition for downloading data.
<br/>

Functionality in this ShinyApp is provided by [weathercan package](https://github.com/ropensci/weathercan). This R package is developed and maintained by members of the rOpenSci (main contributors include Dr. Steffi LaZerte and Sam Albers, Data Scientist in BC Goverment's Data Science & Analytics Unit).

<br/>
<img src="https://github.com/ropensci/weathercan/raw/master/inst/assets/weathercan_logo.png" width="6%" />
<br/>


## Usage:

Currently the app is hosted on [shinyapps.io](https://nickrongkp.shinyapps.io/WeatherCan/) with a free account. Please be mindful of others and close the app/browser tab as soon as possible when you are done to conserve resources for others, thank you.

**Data are downloaded on-the-fly from ECCC. Please be patient as the downloads can be fairly large (especially for hourly and daily data).**

Check the Station Map to identify potential stations and their Climate IDs. Enter the Climate ID for targeted station on the sidebar. Alternatively, WMO or TC IDs can be used if they are known to users.

The `Data Table` tab presents a preview of data. Users may download the data as a .csv/.xls/.pdf file for further processing and analysis.

The `Data Completeness` tab presents a quick overview of the completeness of data for each variable in each year. The plot is interactive and can be zoomed in or out.

<br/>

## License
Copyright © 2021 [Nick Rong](https://github.com/nickyrong) & [Nate Smith](https://github.com/WraySmith)

Released under the [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an “AS IS” BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
<br/>

## Contributing

Pull requests and stars are always welcome. For bugs and feature requests, [please create an issue](https://github.com/nickyrong/ShinyWeatherCan/issues).

Please note that this project was created under a learning objective and it is provided in the event it is useful to others. Accordingly, it is not necessarily under active development. 
<br/>
<br/>
