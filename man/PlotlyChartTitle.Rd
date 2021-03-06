% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/app_helpers.R
\name{PlotlyChartTitle}
\alias{PlotlyChartTitle}
\title{Format title for plotly charts}
\usage{
PlotlyChartTitle(title_text = "Chart title", x = 0,
  font_family = "Arial", font_color = "#393939", font_size = 16)
}
\arguments{
\item{title_text}{Chart title text}

\item{x}{Horizontal alignment}

\item{font_family}{Font family}

\item{font_color}{Font color}

\item{font_size}{Font size}
}
\value{
Formatted chart title for Plotly charts
}
\description{
Format title for plotly charts
}
\examples{
PlotlyChartTitle('My chart title', x = 0.5, font_family = "Arial", font_color = '#e6e6e6', font-size = 18)
}
