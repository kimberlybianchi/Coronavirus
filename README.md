# Coronavirus-Dashboard

The purpose of this dashboard is to track the Coronavirus Pandemic and all the statistics available with it such as number of Negative and Positive tests, Hospitalizations, Seven-day rolling averages of Positive Cases, and more. Data came from The COVID Tracking Project (https://covidtracking.com/data/api) and is displayed as recorded. As of 03/07/2021, the data has stopped being updating. Columns of data have been deleted causing visulatizations to become misleading, however the general idea of the dashbaord is still portrayed.

### Map

This tab displays a map of the United States and each states current status for New York States Quarantine Restrictions. States that were considered unsafe to travel to are noted by a red color and must have a testing positivity rate higher than 10% over a seven-day rolling average or a positive test rate higher than 10 per 100,000 residents over a seven-day rolling average. States that were considered cautionary to travel to are noted by a yellow color and must have a testing positivity rate between 9% - 10% over a seven-day rolling average or a positive test rate between 8 per 100,000 and 10 per 100,000 residents over a seven-day rolling average. States that were considered safe to travel to are noted by a green color and must have a testing positivity rate below 9% over a seven-day rolling average and a positive test rate below 8 per 100,000 residents over a seven-day rolling average.

### Trends

This tab displays more descriptive data for each individual state. After selecting a specific state, the number of current hospitalizations, ICU patients, positive tests, negative tests, and days since the first recorded COVID cases are dispayed. There is also a graph that shows the number of cumulative cases over time and another graph that shows the daily positive and negative cases.

### Quarantine

This tab displays the two metrics New York State used for travel and quarantine restictions. The top half shows the testing positivity rate over a seven-day rolling average and the bottom half shows the positive test rate for residents over a seven-day rolling average. There are horizontal lines representing the thresholds that the states need to be under in order to be safe to travel to and from. There are also information boxes on the right side that give you the current numbers, how much it has changed since the day prior, and the threshold the metric needs to be under.
