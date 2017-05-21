#####################
# Download Macro data

# inflation, monthly
getSymbols.FRED("CPIAUCSL",env =globalenv(), return.class = "xts") # CPI
getSymbols.FRED("PCE",env =globalenv(), return.class = "xts") # PCE

# GDP, quarterly
getSymbols.FRED("GDPPOT",env =globalenv(), return.class = "xts") # potential GDP, quarterly, REAL
getSymbols.FRED("NGDPPOT",env =globalenv(), return.class = "xts") # potential GDP, quarterly, NOMINAL
getSymbols.FRED("GDP",env =globalenv(), return.class = "xts") # GDP, quarterly, NOMINAL
getSymbols.FRED("GDPC1",env =globalenv(), return.class = "xts") # GDP, quarterly, REAL

