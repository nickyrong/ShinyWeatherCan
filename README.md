## Authors 

* [Nick Rong](https://github.com/nickyrong)
* [Nate Smith](https://nathan-smith.net)
<br/>

## Overview
ShinyWeatherCan is currently available at: 
- https://nickrongkp.shinyapps.io/WeatherCan/

This app allows the user to enter a station ID to access climate data from:
[Environment and Climate Change Canada (ECCC)](https://climate.weather.gc.ca/historical_data/search_historic_data_e.html).
<br/>

Currently the ECCC website only allows data downloads of one month at a time. The current scope of this app is to minimize repetition for downloading data. Functionality to download data from ECCC within this app is provided by the [weathercan package](https://github.com/ropensci/weathercan) which is part of the [rOpenSci project](http://ropensci.org) and authored by Dr. Steffi LaZerte.  
<br/>
<img src="https://github.com/ropensci/weathercan/raw/master/inst/assets/weathercan_logo.png" width="6%" />
<br/>

## Usage:

The app is hosted on [shinyapps.io](https://nickrongkp.shinyapps.io/WeatherCan/) with a free account. Please be mindful of this and close the app/browser tab as soon as possible when you are done, thank you. The app is also hosted on a free tier AWS account at https://app.waterexplorer.net/shinyWeather/. If one of the services is not working, it is recommended to try the other.  

**Data are downloaded on-the-fly from ECCC. Please be patient as the downloads can be fairly large (especially for hourly and daily data).**

Currently the app allows the user to:
- Check the Station Map to identify potential stations and their Climate IDs (non-searchable map). 
- Access hourly, daily, or monthly climate data from ECCC for a selected Climate Station ID. WMO or TC IDs can be used if they are known to the user.
- Use the `Data Table` tab to preview the accessed data. Users may download the data as a `.csv` file for further processing and analysis.
- View the `Data Completeness` tab which provides an overview of the completeness of data variables in each year. The plot is interactive and can be zoomed in or out.
- Use the `Infill Data` tab to fill gaps in climate records using quantile mapping from nearby stations. This feature helps create more complete datasets by leveraging data from neighboring stations while preserving the statistical characteristics of the target station.
<br/>

## License
Copyright © 2023 [Nick Rong](https://github.com/nickyrong) & [Nate Smith](https://github.com/WraySmith)

Released under the [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an “AS IS” BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
<br/>

## Contributing

Pull requests and stars are always welcome. For bugs and feature requests, [please create an issue](https://github.com/nickyrong/ShinyWeatherCan/issues).

Please note that this project is released with a [Contributor Code of Conduct](https://github.com/nickyrong/ShinyWeatherCan/blob/main/CODE_OF_CONDUCT.md).By participating in this project you agree to abide by its terms.

Also note that this project was created under a learning objective and it is provided in the event it is useful to others. Accordingly, it is not necessarily under active development. 
<br/>
<br/>
