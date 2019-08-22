# Chart formatting
fontFamily <- "Myriad-Pro, Calibri, Arial, 'sans serif'"
tickfontBl <- list(family = fontFamily,
                   size = 12,
                   color = "black")

tickfontRd = list(family = fontFamily,
                  size = 12,
                  color = "#C40C0C")

axisFormat <- list(title = "",
                   showticklabels = TRUE,
                   # tickangle = 45,
                   tickfont = tickfontBl)

legendFormat <- list(
  font = list(
    family = fontFamily,
    size = 11,
    color = "#696969"
  ),
  bordercolor = "#e6e6e6",
  borderwidth = 1,
  bgcolor = "rgba(255, 255, 255, 0.5)",
  orientation = 'h',
  # xanchor = "center",  # use center of legend as anchor
  x = 0
)

marginFormat <- list(
  l = 50,
  r = 50,
  b = 100,
  t = 50,
  pad = 4
)

marginFormatMonthly <- list(
  l = 150,
  r = 50,
  b = 50,
  t = 50,
  pad = 4
)

# color schema
colResidential <- "#80b1d3"
colSingleFam <- "#4292c6"
colMultiFam <- "#9ecae1"
colStrata <- "#abdda4"
colNonStrataRental <- "#c7eae5"
colCommercial <- "#fdae61"
colRecreational <- "#80cdc1"
colFarms <- "#fee08b"
colUnknown <- "#d9d9d9"
colAcreage <- "#dfc27d"
colC16 <- colCanadian <- "#c40c0c"
colC11 <- colForeign <- "#3eb4f0"
