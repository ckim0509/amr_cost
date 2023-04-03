# ----------------------------------------------------------------------------
# inflation.R
#
# applying inflation function
# Purchasing Power Parity exchange rates
# Local currency unit exchange rates
# GDP deflation indices
# -----------------------------------------------------------------------------
# load libraries
library(data.table)
library(dplyr)
library(countrycode)
library(wbstats)

# remove all objects from workspace
rm (list = ls ())


# source functions
source ("functions.R")

# move to base directory (run code from source directory)
source_wd <- getwd ()
setwd ("../")


for (i in 1:nrow(primary.cost)){
  primary.cost[i, mean_i := inflation_exchange_PPP(2010,2019,primary.cost[i], 
                                                   "mean_cost",inf_xch_4function)]
  primary.cost[i, high95_i := inflation_exchange_PPP(2010,2019,primary.cost[i], 
                                                     "high95",inf_xch_4function)]
  primary.cost[i, low95_i := inflation_exchange_PPP(2010,2019,primary.cost[i], 
                                                    "low95",inf_xch_4function)]
  primary.cost[i, SD_i := inflation_exchange_PPP(2010,2019,primary.cost[i], 
                                                 "SD",inf_xch_4function)]
}


