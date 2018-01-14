tabPanel("Home",
         fluidPage(
           jumbotron(
             header = "BC Housing Market Data Visualization project",
             popPerc = c16Prov$Population.Change,
             popInc = TRUE,
             dwellPerc = c16Prov$Total.Private.Dwellings.Change,
             dwellInc = TRUE,
             trans_period = maxTransPeriod,
             no_mkt_trans = ptProvMth[ptProvMth$trans_period == maxTransPeriod, "no_mkt_trans"],
             no_foreign_perc = ptProvMth[ptProvMth$trans_period == maxTransPeriod, "no_foreign_perc"] ,
             sum_FMV = ptProvMth[ptProvMth$trans_period == maxTransPeriod, "sum_FMV"],
             sum_FMV_foreign_perc = ptProvMth[ptProvMth$trans_period == maxTransPeriod, "sum_FMV_foreign_perc"]
           )
         ))
