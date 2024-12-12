# 2024 NFLPA Case Competition

## Introduction
What constitutes the middle class in the National Football League? And how has it changed in the last 10 years? The following code will be used to help answer these questions.

## Code
### Script 1 - Defining the middle class
[Link](https://github.com/Atuav10/2024-NFLPA-case-competition/blob/main/01-Determining-middle-class.R)

This R script goes through how we defined and analyzed the middle class over time. This includes how to scrape the data, how to define the different classes, and some introductory data analysis on the final data.

### Script 2 - Preparing the model
[Link](https://github.com/Atuav10/2024-NFLPA-case-competition/blob/main/02-Preparing-model.R)

This R script creates a model based on the outputs from script 1. The goal is to determine a player's contract based on their statistics from the previous two seasons. This script involves joining the contract data with player statistics and organizing/cleaning it. 

### Script 3 - Running the model
[Link](https://github.com/Atuav10/2024-NFLPA-case-competition/blob/main/03-Actual-model.R)

In this R script, we actually conduct our model. We also do some supplementary analysis and introduce the "Salary over Expected" metric. This metric tells us if a player was overpaid or underpaid based on their statistics.

## Data
[Link](https://github.com/Atuav10/2024-NFLPA-case-competition/tree/main/Data)

The [drafted_players](https://github.com/Atuav10/2024-NFLPA-case-competition/blob/main/Data/drafted_players.csv) data is used in script 1. All other datasets are used in script 2.
