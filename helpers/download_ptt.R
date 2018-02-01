library(rvest)
library(dplyr)

# Prepare data dir
dataDir <- './data'
pttDir = "ptt"
destinationDir <- file.path(dataDir, pttDir)
if (!file.exists(destinationDir)){
    dir.create(file.path(dataDir, pttDir))
}

# Scrape PTT catalogue page to obtain csv links
pttUrl <- "https://catalogue.data.gov.bc.ca/dataset/9c9b8d35-d59b-436a-a350-f581ea71a798"
catalogue <- read_html(x = pttUrl)
urls <- catalogue %>%
    html_nodes(".resource-url-analytics") %>%
    html_attr("href")
links <- catalogue %>%
    html_nodes(".resource-url-analytics") %>%
    html_text()
catalogueUrls <- data.frame(links = links, urls = urls, stringsAsFactors = FALSE)

# Filter out unwanted links
csvs <- filter(catalogueUrls, grepl("csv", urls, fixed = TRUE))
csvs <- csvs$urls

# Download files
fileNames <- file.path(destinationDir, basename(csvs))
for (cnt in seq_along(fileNames)) {
    fileName <- fileNames[[cnt]]
    curl_download(csvs[[cnt]], fileName, mode = "w")
}

# @TODO REPROCESS PTT FILES TO RECREATE RDS