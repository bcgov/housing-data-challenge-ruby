# BC Housing Data Visualization

<!-- badges: start -->
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
<!-- badges: end -->

This app is a solution for Data Visualization Challenge to provide
improved data visualization capacity and usefullness of Crown data with
regards to the housing market and condiotions in BC.

## About the app

The Province of British Columbia and other agencies collect a wealth of
administrative data that relates to housing, including the market for
housing, as a consequence of their regulatory and administrative
authorities.

The British Columbia Government wanted to develop statistics that will
provide greater certainty about the state of housing in the province,
including the role of foreign ownership, real estate as an investment or
business strategy (rather than home ownership), and insights into the
regional impact of these issues. For more information see
<https://github.com/bcgov/housing-data-visualization-project>.

This application is the result of the *Data Visualization Challenge*,
launched by the [Innovate BC (former BC Innovation
Council)](https://innovatebc.ca) in partnership with [BC
Stats](https://www2.gov.bc.ca/gov/content/data/about-data-management/bc-stats)
in December 2016.

The challenge aimed to develop a tool to visualize BC housing data in a
more meaningful and impactful way. BCIC and BCStats invited innovators
around the province to submit their proposed solutions to the challenge.
A review panel comprised of the BC Innovation Council, BC Developers’
Exchange and BC Stats selected the five finalists who have been selected
to present their prototypes at the 2017 [BCTech
Summit](https://bctechsummit.ca/), the largest tech event in BC and a
joint initiative between the Province and BC Innovation Council. [Ruby
Industries Inc.](https://rubyind.com) was announced as the winner of the
Challenge and got the opportunity to continue working on the project and
develop the solution.

## Installation instructions

### System Requirements

Local installation requires R statistical software to be installed.

### Installation

You can install the app directly from this GitHub repository using the
[remotes](https://cran.r-project.org/package=remotes) package:


```
install.packages("remotes")
remotes::install_github("bcgov/housing-data-challenge-ruby")
library(bchousing)
```

To run the app, execute the following in the R console:

```
bchousing::run_app()
```

### Vignettes

  - [Getting property transfer tax
    data](https://bcgov.github.com/housing-data-challenge-ruby/articles/getting-ptt-data.html)
  - [Getting census
    data](https://bcgov.github.com/housing-data-challenge-ruby/articles/getting-census-data.html)

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an
[issue](https://github.com/bcgov/housing-data-challenge-ruby/issues/).

## Data Sources

BC Housing Data Visualization Platform uses the following datasets: 

- Census Canada (years 2016, 2011 and 2006). 
- Property Transfer Tax (2017
and 2016).

### Census Canada

Census data is provided by [Statistics
Canada](http://www.statcan.gc.ca/) and obtained through [CensusMapper
API](https://censusmapper.ca/api) using [cancensus R
package](https://github.com/mountainMath/cancensus). - Statistics
Canada. No date. 2006 Census of Canada (Provinces, Census Divisions,
Municipalities) (database). - Statistics Canada. No date. 2011 Census of
Canada (Provinces, Census Divisions, Municipalities) (database). -
Statistics Canada. No date. 2016 Census of Canada (Provinces, Census
Divisions, Municipalities) (database).

Statistics Canada provides a wealth of detail about the information
collected in the Census. Some of the key pages are:

  - [Census Program
    homepage](http://www12.statcan.gc.ca/census-recensement/index-eng.cfm)

  - [Data products, 2016
    Census](http://www12.statcan.gc.ca/census-recensement/2016/dp-pd/index-eng.cfm)
    
      - [Data tables, 2016
        Census](http://www12.statcan.gc.ca/census-recensement/2016/dp-pd/dt-td/index-eng.cfm),
        grouped by topic, variable, and map
    
      - [Reference materials, 2016
        Census](http://www12.statcan.gc.ca/census-recensement/2016/ref/index-eng.cfm):
        concepts and definitions, reference guides and other materials
    
      - [Reference materials, 2016 Census:
        Housing](http://www12.statcan.gc.ca/census-recensement/2016/ref/98-501/98-501-x2016007-eng.cfm)

### Property Transfer Tax

Property transfer tax data relates to market transactions within the
province.

It is provided by Province of British Columbia Ministry of Finance,
licensed under [Open Government LicenCe - British
Columbia](http://www.data.gov.bc.ca/local/dbc/docs/license/OGL-vbc2.0.pdf).

The data is available for download at [B.C. Data Catalogue administered
by
DataBC](https://catalogue.data.gov.bc.ca/organization/property-taxation).

## Credits

The platform is developed using [R statistical
software](https://www.r-project.org/), and realized as R
[Shiny™](http://shiny.rstudio.com/) app with the help of the following
open-source R packages:

  - [Shiny™](https://github.com/rstudio/shiny) - Interactive web
    applications with R.
  - [Plotly](https://github.com/ropensci/plotly) - R implementation of
    plotly.js interactive graphing library.
  - [Readr](https://github.com/tidyverse/readr) - Read flat files (csv,
    tsv, fwf) into R.
  - [Stringr](https://github.com/tidyverse/stringr) - String
    manipulation in R.
  - [Magrittr](https://github.com/tidyverse/magrittr) - Improve the
    readability of R code with the pipe operator.
  - [Lubridate](https://github.com/tidyverse/lubridate) - Working with
    dates in R.
  - [Tidyr](https://github.com/tidyverse/tidyr) - Tidy data with spread
    and gather functions.
  - [Dplyr](https://github.com/tidyverse/dplyr) - A grammar of data
    manipulation.
  - [Cancensus](https://github.com/mountainMath/cancensus) - R wrapper
    for calling [CensusMapper APIs](https://censusmapper.ca/api).
  - [Leaflet](https://github.com/rstudio/leaflet/) - R Interface to
    leaflet.js maps.
  - [RGDAL](https://r-forge.r-project.org/projects/rgdal/) - R bindings
    to Geospatial Data Abstraction Library.
  - [Rgeos](https://r-forge.r-project.org/projects/rgeos/) - Tools for
    interfacing the C++ translation of the Java Topology Suite.
  - [Sf](https://r-spatial.github.io/sf/) - Simple Features for R.
  - [rmapshaper](https://github.com/ateucher/rmapshaper) - An R wrapper
    for the mapshaper javascript library.
  - [Htmlwidgets](https://github.com/ramnathv/htmlwidgets) - HTML
    Widgets for R.
  - [DT](https://github.com/rstudio/DT) - R Interface to the jQuery
    Plug-in DataTables.
  - [SunburstR](https://github.com/timelyportfolio/sunburstR) - R
    htmlwidget for sequence sunburst charts.
  - [Treemap](https://github.com/mtennekes/treemap) - R package for
    treemap visualisation.
  - [ShinyJS](https://github.com/daattali/shinyjs) - Improve the user
    experience of your Shiny™ apps.
  - [Shinycssloaders](https://github.com/andrewsali/shinycssloaders) -
    CSS loader animations for Shiny™ outputs.
  - [bsplus](https://github.com/ijlyttle/bsplus) - Shiny™ and R Markdown
    addons to Bootstrap 3.

### How to Contribute

If you would like to contribute to the package, please see our
[CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

### License

Copyright 2018 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the “License”); you may
not use this file except in compliance with the License. You may obtain
a copy of the License at

<http://www.apache.org/licenses/LICENSE-2.0>

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an “AS IS” BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
