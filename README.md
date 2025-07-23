# Forecasting COVID-19, influenza, and RSV hospitalisations over winter 2023/24 in England

Code related to the publication (Forecasting COVID-19, influenza, and RSV hospitalisations over winter 2023/24 in England)[https://academic.oup.com/ije/article/54/3/dyaf066/8156944] published in the International Journal of Epidemiology, doi: 10.1093/ije/dyaf066.

This is a one-way push of the relevant contents of the operational repository used to deliver forecasts within UKHSA in winter 2023/24.

The repository represents the state of code at the end of the 2023/24 season.



## Key notes

1. The code is not presented with accompanying data due to the large scale of data and sharing restrictions placed upon the targets and indicators.
2. This code is not actively being developed, and is intended as a archive of past work, rather than being representative of current production code.
3. To protect data & cyber security data infrastructure artifacts have been removed.
4. Non-modelling scripts (e.g. exploratory analyses) have been removed from the repository.

We have open sourced this code to support the transparency of the project with the epidemic forecasting community.

## Structure

Each disease has it's own directory, with each directory there is a modelling directory for each metric.

Common functions across models are accessed via a `box` module for this repository, as well as common functions used across the team (e.g. for data access).

## Changes between 2023/24 & 2024/25

Over summer 2024 there was a large refactor of code to streamline operations. The primary changes were:

1. Development of a single database table for all forecast targets.
2. Coupling forecast pipelines to databases rather than AWS S3 file locations.
3. Introduction of `targets` pipelines for respiratory modelling.
4. Addition of `lintr` into the repository.

We hope to release this updated code at the end of the 24/25 season.
