# BC Stats / BCIC Housing Data Visualization

This app is a solution for BC Government Data Visualization Challenge to provide 
improved data visualization capacity and usefullness of Crown data with regards 
to the housing market and condiotions in BC.

App is publicly available at http://bcdevx.rubyind.com/housing/.

## Table of Contents

  * [ BC Stats / BCIC Housing Data Visualization](#bc-stats-bcic-housing-data-visualization)
      * [Installation Instructions](#installation-instructions)
          * [System Requirements](#system-requirements)
          * [Installation](#installation)
          * [Using Docker Image](#using-docker-image)
          * [Credits](#credits)


## Installation instructions

### System Requirements
Local installation requires R statistical software to be installed.
Following is the R session info with required packages.

```
> sessionInfo()
R version 3.3.2 (2016-10-31)
Platform: x86_64-apple-darwin13.4.0 (64-bit)
Running under: macOS Sierra 10.12.3

locale:
[1] en_CA.UTF-8/en_CA.UTF-8/en_CA.UTF-8/C/en_CA.UTF-8/en_CA.UTF-8

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] plotly_4.5.6.9000  ggplot2_2.2.1.9000 crosstalk_1.0.1    tidyr_0.6.1        rgeos_0.3-22      
 [6] DT_0.2             htmlwidgets_0.8    dplyr_0.5.0        maps_3.1.1         leaflet_1.0.2.9010
[11] rgdal_1.2-5        sp_1.2-4           shiny_1.0.0       

loaded via a namespace (and not attached):
 [1] Rcpp_0.12.9        colourpicker_0.3   RColorBrewer_1.1-2 plyr_1.8.4         base64enc_0.1-3   
 [6] tools_3.3.2        digest_0.6.12      jsonlite_1.2       tibble_1.2         gtable_0.2.0      
[11] lattice_0.20-34    viridisLite_0.1.3  DBI_0.5-1          yaml_2.1.14        httr_1.2.1        
[16] grid_3.3.2         R6_2.2.0           purrr_0.2.2        magrittr_1.5       scales_0.4.1      
[21] htmltools_0.3.5    assertthat_0.1     mime_0.5           xtable_1.8-2       colorspace_1.3-2  
[26] httpuv_1.3.3       miniUI_0.1.1       lazyeval_0.2.0     munsell_0.4.3     
```

### Installation
In terminal, type the following:
```
shiny::runGitHub("housing-data-challenge-ruby", "bcgov")
```
This will download the app localy in your current working directory and run the app.


### Using Docker image
App can also be installed using Docker image (it is required that docker is installed 
on the server or local machine).

There is a separate repo forked from [simple-R-shiny](https://github.com/bcgov/simple-R-shiny)
which can be utilized to build Docker image with all required libraries and packages.

Use the following code to use it.

```
git clone https://github.com/sasha-ruby/simple-R-shiny.git
cd simple-R-shiny
./dev.sh
```

### Credits
Data sources:

- [Property Transfer Tax Data 2016](https://catalogue.data.gov.bc.ca/dataset/property-transfer-tax-data-2016) (Open Government License - British Columbia)
- Statistics Canada. 2017. Population and Dwelling Count Highlight Tables, 2016 Census. "Population and Dwelling Count Highlight Tables, 2016 Census" "2016 Census: Release topics." Census. Statistics Canada Catalogue no. 98-402-X2016001. Ottawa, Ontario. February 8.
http://www12.statcan.gc.ca/census-recensement/2016/dp-pd/hlt-fst/pd-pl/index-eng.cfm (accessed March 3, 2017.) (Statistics Canada Open Licence Agreement)

It is built open source software tools and packages, namely:

- R statistical software (GPL-2 | GPL-3)
- shiny (GPL-3)
- rgdal (GPL-2 | GPL-3)
- leaflet (GPL-3)
- maps (GPL-2)
- dplyr (MIT)
- htmlwidgets (MIT)
- DT (GPL-3)
- rgeos (GPL-2 | GPL-3)
- tidyr (MIT)
- crosstalk (MIT)
- plotly (MIT)


