
# BC Stats / BCIC Housing Data Visualization

This app is publicly available at http://bcdevx.rubyind.com/housing/.

## Installation instructions on local environment

### Requirements
Local installation requires R statistical software to be installed.
Following is the R session info with required packages.

```
> sessionInfo()
R version 3.3.2 (2016-10-31)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Debian GNU/Linux 8 (jessie)

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=en_US.UTF-8       
 [4] LC_COLLATE=en_US.UTF-8     LC_MONETARY=en_US.UTF-8    LC_MESSAGES=C             
 [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                  LC_ADDRESS=C              
[10] LC_TELEPHONE=C             LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] shiny_1.0.0     htmlwidgets_0.8 dplyr_0.5.0     maps_3.1.1      leaflet_1.0.1   rgdal_1.2-5    
[7] sp_1.2-4       

loaded via a namespace (and not attached):
 [1] Rcpp_0.12.9        magrittr_1.5       munsell_0.4.3      xtable_1.8-2       colorspace_1.3-2  
 [6] lattice_0.20-34    R6_2.2.0           plyr_1.8.4         tools_3.3.2        grid_3.3.2        
[11] DBI_0.5-1          htmltools_0.3.5    yaml_2.1.14        lazyeval_0.2.0     assertthat_0.1    
[16] digest_0.6.12      tibble_1.2         RColorBrewer_1.1-2 mime_0.5           scales_0.4.1      
[21] jsonlite_1.2       httpuv_1.3.3      
```

### Installation
In terminal, type the following:
```
shiny::runGitHub("housing-data-challenge-ruby", "bcgov")
```
This will download the app localy in your current working directory, and run the app.


### Using Docker image
Docker image is in preparation, so it will be possible to install the app
by pulling the Docker image.
