This repository contains the data behind [COVID-19 Tournament](https://predictions.uwaterloo.ca/tournament) of the [Forecasting Collaborative](https://predictions.uwaterloo.ca/).

## Data

* `dat_for_analyses.csv` contains a harmonized file used for main analyses of forecasting accuracy.
* `dat_long.csv` contains a file with predictions of forecasting teams in a long format, used for plotting estimates of each team.
* `sim folder` contains R files used to simulate naive benchmarks
* `historical_data.csv` contains historical data provided to participants for domains they chose to make forecasts for, as well as ground truth scores for each domain, along with information on a key conditional factor - COVID-19 infections and deaths in the US.
* `dat_for_analyses.csv` contains a harmonized file used for main analyses of forecasting accuracy.
* `wave1.scores.csv` contains the estimates in the First Tournament (May 2020) for each month per domain, ranked in terms of inaccuracy (MASE scores) of Forecasting Teams.
* `wave2.scores.csv` contains the estimates in the Second Tournament (Nov 2020) for each month per domain, ranked in terms of inaccuracy (MASE scores) of Forecasting Teams.
* `top.t1.csv.csv` contains the estimates of the best performing teams in each domain in the First Tournament (May 2020) per month, ranked in terms of inaccuracy (MASE scores) of Forecasting Teams, along with the forecasting approach, and team characteristics.
* `top.t2.csv.csv` contains the estimates of the best performing teams in each domain in the Second Tournament (Nov 2020) per month, ranked in terms of inaccuracy (MASE scores) of Forecasting Teams, along with the forecasting approach, and team characteristics.
* `Columns reference.xlsx` contains the legend for all variable names in the dataset.

## Method

* [Recruitment](https://predictions.uwaterloo.ca/tournament/)
* [Tournament Instructions](https://predictions.uwaterloo.ca/instructions/)
* [Codebook for model complexity](https://github.com/grossmania/Forecasting-Tournament/blob/main/ModelType+ComplexityCodebook_2020-07-21-MTG-LT-57.docx?raw=true)
* [Codebook for counterfactuals](https://github.com/grossmania/Forecasting-Tournament/blob/main/TournamentCounterfactualWorldviewCodebook_2021-07-12.docx?raw=true)


## Analyses

* [R Markdown file of the main analyses, in the order they appear in the manuscript](https://github.com/grossmania/Forecasting-Tournament/blob/8db528c9055e227d7258d1fdcf836398f9f3d09e/Wave%201+2%20Analyses%20FINAL%20FOR%20MANUSCRIPT.Rmd)
* [R Markdown file of analyses of a correlation-based index of inaccuracy reported in the supplement](https://github.com/grossmania/Forecasting-Tournament/blob/d9b9c6eea457a6c7cddeea2274bad3ea4cb25ff5/Correlation%20Coefficients%20by%20Teams%20and%20Tasks.Rmd)
