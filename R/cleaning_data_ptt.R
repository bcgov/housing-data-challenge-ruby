#' @title Add Transaction Period Columns
#' @description  Add transaction period and transaction period label columns to be used for charts
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom forcats as_factor
#' @importFrom lubridate month
#' @importFrom stringr str_c
#' @importFrom stringr str_pad

#'
#' @param data data frame
#' @return modified data frame
#'
#' @export
AddTransactionPeriodColumns <- function(data) {

  # Rename year and month columns if needed
  cols <- colnames(data)
  if ('Year' %in% cols) {
    data <- data %>%
      dplyr::rename(year = Year)
  }
  if ('Month' %in% cols) {
    data <- data %>%
      dplyr::rename(month = Month)
  }

  data <- data %>%
    dplyr::mutate(

      # Transaction period (yyyy-mm-dd)
      trans_period = as.Date(
        stringr::str_c(
          year,
          '-',
          stringr::str_pad(month, width = 2, side = 'left', pad = '0'),
          '-01'
        )
      ),

      # Transaction period label yyyy-m
      trans_period_label = forcats::as_factor(
        stringr::str_c(
          year,
          '-',
          lubridate::month(trans_period, label = TRUE)
        ),

      )
    ) %>%
    # Reorder columns and drop year and month columns
    dplyr::select(trans_period, trans_period_label, everything(), -year, -month)

  return(data)
}

#' @title Wrangles PTT data
#'
#' @description Wrapper to perform all wrangling required on PTT data. This includes
#'              adding transaciton period columnns function call, changing the case
#'              of region names, etc.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom forcats as_factor
#' @importFrom stringr str_to_title
#' @importFrom stringr str_replace
#' @importFrom stringr str_to_upper
#'
#' @param data data frame
#' @param calculate_percentages Whether to calculate percentage columns for foreign transactions
#' @return modified data frame
#'
#' @export
WranglePttData <- function(data, calculate_percentages = FALSE, region_name_split = FALSE) {

  cols <- colnames(data)

  if ('DevelopmentRegion' %in% cols) {
    data <- data %>%
      dplyr::mutate(
        # Transform region names to UPPER CASE so we can safely do the filtering
        DevelopmentRegion = stringr::str_to_upper(DevelopmentRegion)
      ) %>%
      dplyr::filter(
        # Filter out the observations that we can't join to polygon regions
        !DevelopmentRegion %in% c('REST OF PROVINCE', 'UNKNOWN', 'UNKNOWN/RURAL') & !is.na(DevelopmentRegion)
      ) %>%
      dplyr::mutate(
        # Transform region names to Title Case
        DevelopmentRegion = stringr::str_to_title(DevelopmentRegion)
      ) %>%
      dplyr::mutate(
        # Convert to factor
        DevelopmentRegion = forcats::as_factor(DevelopmentRegion)
      )
  }

  if ('RegionalDistrict' %in% cols) {

    # RegionalDistrict is prefixed with the sequence number and dash separator, e.g. "04 - Cariboo". We keep only the name
    if (region_name_split) {
      data <- data %>%
        dplyr::mutate(
          RegionalDistrict = SplitBySeparator(RegionalDistrict, sep = ' - ', export_part = 2)
        )
    }

    # Convert to Title Case
    data <- data %>%
      dplyr::mutate(
        RegionalDistrict = stringr::str_to_title(RegionalDistrict)
      )

    # Convert to factor
    data <- data %>%
      dplyr::mutate(
        RegionalDistrict = forcats::as_factor(RegionalDistrict)
      )
  }

  if ('Municipality' %in% cols) {

    data <- data %>%
      dplyr::mutate(
        Municipality = stringr::str_to_upper(Municipality),
        Municipality = stringr::str_replace(Municipality, 'CITY OF ', ''),
        Municipality = stringr::str_replace(Municipality, 'DISTRICT OF ', ''),
        Municipality = stringr::str_replace(Municipality, 'TOWN OF ', ''),
        Municipality = stringr::str_replace(Municipality, 'VILALGE OF ', ''),
        Municipality = stringr::str_to_title(Municipality)
      )

    # Municipality is prefixed with the sequence number and dash separator, e.g. "391 - District of Wells". We keep only the name
    if (region_name_split) {
      data <- data %>%
        dplyr::mutate(
          Municipality = SplitBySeparator(Municipality, sep = ' - ', export_part = 2)
        )
    }

    # Convert to factor
    data <- data %>%
      dplyr::mutate(
        Municipality = forcats::as_factor(Municipality)
      )
  }

  data <- data %>%
    AddTransactionPeriodColumns()

  if (calculate_percentages) {
    data <- data %>%
      dplyr::mutate(
        n_foreign_tran_nona = as.numeric(replace_na(n_foreign_res, 0)),
        sum_FMV_foreign_nona = as.numeric(replace_na(sum_FMV_foreign_res, 0)),
        no_foreign_perc = round(n_foreign_tran_nona / tot_mkt_trans * 100, 2),
        sum_FMV_foreign_perc = round(sum_FMV_foreign_nona / sum_FMV * 100, 2)
      ) %>%
      dplyr::select(-c(n_foreign_tran_nona, sum_FMV_foreign_nona))
  }

  return(data)
}

#' @title Split string by a separator
#' @description  Function to split string by separator and return one part
#'
#' @importFrom stringr str_split
#' @importFrom stringr str_trim
#'
#' @param v String to be split
#' @param sep Separtor
#' @param trim_ws Trim whitespace or not
#' @param export_part The part of string to extract
#'
#' @return First part of the split string
#'
#' @export
SplitBySeparator <- function(v, sep, trim_ws = TRUE, export_part = 1) {
  splitV <- stringr::str_split(v, sep, simplify = TRUE)
  part <- splitV[, export_part]
  if (trim_ws) {
    part <- stringr::str_trim(part, side = "both")
  }

  return(part)
}

#' @title Wrangle Shape files
#' @description  Add transaction period and transaction period label columns to be used for charts
#'
#' @importFrom dplyr filter
#' @importFrom dplyr if_else
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom sf st_as_sf
#' @importFrom sf st_read
#' @importFrom stringr str_replace
#' @importFrom rmapshaper ms_simplify
#'
#' @param data sp object with shapes for the particular geographic level
#' @param id_column Column which identifies the ID of the record (e.g. CSDUID)
#' @param name_column Column which identifies the name of the record (e.g. CSDNAME)
#' @param pr_uid Province ID (e.g. 59 for British Columbia)
#' @return sf data frame
#'
#' @export
WrangleShapeFiles <- function(data, id_column, name_column, pr_uid = 59) {
  shapes <- sf::st_as_sf(
    sf::st_read(data, stringsAsFactors = FALSE)
  )

  # Simplify polygons
  shapes <- rmapshaper::ms_simplify(shapes, keep = 0.02, keep_shapes = TRUE)

  #
  shapes <- shapes %>%
    dplyr::filter(PRUID == pr_uid) %>%
    dplyr::rename("GeoUID" = id_column) %>%
    dplyr::mutate(
      # Preserve location names in English
      GeoName = bchousing:::SplitBySeparator({{ name_column }}, "/"),
      PRNAME = bchousing:::SplitBySeparator(PRNAME, "/")
    )

    # Development Region fixes
    if (id_column == 'ERUID') {
      shapes <- shapes %>%
        dplyr::mutate(
          GeoName = stringr::str_replace(GeoName, '--', '/'),
          # Align region names for joining to PTT files
          GeoName = dplyr::if_else(GeoName == 'Lower Mainland/Southwest', 'Mainland/Southwest', GeoName),
          GeoName = dplyr::if_else(GeoName == 'Vancouver Island and Coast', 'Vancouver Island/Coast', GeoName),
          # Make union of Nechako and North Coast because they are joined in PTT data
          GeoName = dplyr::if_else(GeoName %in% c('Nechako', 'North Coast'), 'Nechako & North Coast', GeoName),
          GeoUID = dplyr::if_else(GeoUID %in% c(5960, 5970), '5960', GeoUID)
        )
    }

    # Regional District fixes
    if (id_column == 'CDUID') {
      shapes <- shapes %>%
        dplyr::mutate(
          # Align region names for joining to PTT files
          GeoName = stringr::str_replace(GeoName, 'Kootenay Boundary', 'Kootenay-Boundary'),
          GeoName = dplyr::if_else(GeoName == 'Greater Vancouver', 'Metro Vancouver', GeoName),
          GeoName = dplyr::if_else(GeoName == 'Central Coast', 'Central Coast-Mount Waddington', GeoName),
          GeoName = dplyr::if_else(GeoName == 'Stikine', 'Rest Of Province', GeoName),
          # Make union of  because they are joined in PTT data
          GeoName = dplyr::if_else(GeoName %in% c('Skeena-Queen Charlotte', 'Central Coast'), 'North Coast', GeoName),
          GeoUID = dplyr::if_else(GeoUID %in% c(5947, 5945), '5947', GeoUID),
          GeoName = dplyr::if_else(GeoName %in% c('Northern Rockies', 'Peace River'), 'Northern Rockies Rm-Peace River', GeoName),
          GeoUID = dplyr::if_else(GeoUID %in% c(5959, 5955), '5959', GeoUID)
        )
    }

    # Municipality fixes
    if (id_column == 'CSDUID') {

    }

    # Reorder columns
    shapes <- shapes %>%
      dplyr::select(GeoUID, GeoName, everything(), -{{ name_column }})

  return(shapes)
}

#' @title Join PTT and Shape files
#' @description Join PTT and Shape files, and remove duplicate rows due to grouped geometries
#'
#' @param ptt_data PTT data frame
#' @param shapes  Shapes sf object
#' @param geo_name Column to join on
#'
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr inner_join
#' @importFrom dplyr mutate
#' @importFrom dplyr summarise_if
#' @importFrom dplyr ungroup
#' @importFrom forcats as_factor
#' @importFrom sf st_transform
#'
#' @return sf data frame
#' @export
JoinPttShapes <- function(ptt_data, shapes, geo_name) {
  ptt_sf <- dplyr::inner_join(
    shapes,
    ptt_data,
    by = c("GeoName" = geo_name)
  ) %>%
    dplyr::group_by(GeoUID, GeoName, trans_period, trans_period_label, PRUID, PRNAME) %>%
    dplyr::summarise_if(is.numeric, mean, na.rm = TRUE) %>%
    dplyr::filter(row_number() == 1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      GeoUID = forcats::as_factor(GeoUID),
      GeoName = forcats::as_factor(GeoName),
      PRUID = forcats::as_factor(PRUID),
      PRNAME = forcats::as_factor(PRNAME)
    )

  # https://github.com/r-spatial/mapview/issues/72
  ptt_sf <- ptt_sf %>%
    sf::st_transform(crs = 4326)

  return(ptt_sf)
}
