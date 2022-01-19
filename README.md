# Desafio Seazone

This is a technical challenge from Seazone for the position of data analyst.

## Description

The aim is to answer some established questions involving two datasets: one including Airbnb listings and other with the prices of some of those listings and whether they were rented during a time period. The questions asked are:

1. Sort neighbourhoods by ascending order of listings
2. Sort neighbourhoods by ascending order of average revenue of listings
3. Look for correlations between listings's characteristics and revenue
4. How far in advance do guests book?

Each R script answers a specific question and so they are written to work independently from one another.

## Packages used

Each question is answered in a specific R script. The following R packages were used: 
* [tidyverse](https://www.tidyverse.org/) to clean and organize the data.
* [chron](https://cran.r-project.org/web/packages/chron/index.html) to treat time series.
* [data.table](https://github.com/Rdatatable/data.table) was used to read .csv files quicker.
* [corrplot](https://github.com/taiyun/corrplot) to produce correlation plots.
* [wordcloud](https://cran.r-project.org/web/packages/wordcloud/index.html), [wordcloud2](https://cran.r-project.org/web/packages/wordcloud2/index.html), and [tm](https://cran.r-project.org/web/packages/tm/index.html) for text mining and look for most used words.
* [patchwork](https://cran.r-project.org/web/packages/patchwork/index.html) easily generated combined graphs from ggplot2.