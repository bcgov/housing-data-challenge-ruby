#' @importFrom tibble tibble
NULL

#' PTT Development Region-level
#'
#' Development Region-level property transfer tax provided by the Ministry of Finance, Government of British Columbia.
#'
#' @format A data frame with many variables: \code{trans_period}, \code{trans_period_label},
#'   \code{GeoUID}, \code{GeoName} and \code{prop} (\code{n}.
"ptt_dr_sf"

#' PTT Regional District-level
#'
#' Regional District-level property transfer tax provided by the Ministry of Finance, Government of British Columbia.
#'
#' @format A data frame with many variables: \code{trans_period}, \code{trans_period_label},
#'   \code{GeoUID}, \code{GeoName} and \code{prop} (\code{n}.
"ptt_rd_sf"

#' PTT Municipality-level
#'
#' Municipality-level property transfer tax provided by the Ministry of Finance, Government of British Columbia.
#'
#' @format A data frame with many variables: \code{trans_period}, \code{trans_period_label},
#'   \code{GeoUID}, \code{GeoName} and \code{prop} (\code{n}.
"ptt_mun_sf"

#' PTT boundaries - Development Region-level
#'
#' Development Region-level boundaries used for PTT.
#'
#' @format An sf data frame
"shapes_dr"


#' PTT boundaries - Regional District-level
#'
#' Regional District-level boundaries used for PTT.
#'
#' @format An sf data frame
"shapes_rd"

#' PTT boundaries - Municipality-level
#'
#' Municipality-level boundaries used for PTT.
#'
#' @format An sf data frame
"shapes_mun"

#' PTT dashboard data - Province-level
#'
#' Province-level boundaries used for PTT-related data on the dashboard.
#'
#' @format A data frame
"ptt_prov_dash"
