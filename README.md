### Background
I am primarily using Electricity prices(in dollars per kilowatt hours) data from fred.st.louisfed.org which represents average electricity prices calculated with CPI to provide estimates based on the Department of Energy’s collection of mail questionnaires of 75 urban areas in the US. This dataset is observed monthly and spans from Nov 1978 to Jan 2020. I also make use of api data from eia.gov for the prices of natural gas sold to commercial consumers (dollars per thousand cubic feet) observed monthly from October 1983 to December 2019.

### Goal
I am aiming to create my own model for a time series containing dynamics of trend, seasonality, cycles, and potential volatility clustering. I will be comparing this model to many others and optimizing the forecasts (h = 12 ahead) for possible usability in predicting urban U.S electricity prices. A potential use could be for large businesses which use heavy amounts of electricity to allocate certain activities to months where electricity prices will be expected to drop. It could also potentially provide some information on aspects of energy sources such as fossil fuels and renewable energy as they will be correlated to some extent. Going with this theory, I introduce natural gas prices as another working dataset to later form a Vector Autoregression (VAR) model which makes use of correlated lag values in order to get information about both time series.

### Conclusion
The 12 step forecasts from the created model performed better than auto.arima, ETS, Holt-Winters, Combination, and VAR models in the recent years with the lowest MAPE.

### Future Work
There may be more testing that could be done to validate more forecasts beyond the recent 12 steps. There could be improvements made on the efficiency of MAPE function as well as considering more models in trend, seasonality, and cyclical components. As time passes, it may be a good idea to take into account the increasing portion of renewable energy and electricity use over gas, coal, and other contemporary sources.

### References:
https://fred.stlouisfed.org/series/APU000072610

https://www.bls.gov/cpi/factsheets/average-prices.htm

https://www.eia.gov/opendata/qb.php?category=461217&sdid=NG.N3020US3.M

https://www.eia.gov/energyexplained/electricity/electricity-in-the-us.php
