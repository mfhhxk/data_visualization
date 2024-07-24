# README: Interactive Visualization for Data Exploration Project (DEP)

## Overview

This report is a continuation of the Data Exploration Project (DEP). It details the design process and implementation of interactive visualizations that present key findings from DEP using R Shiny. The visualizations aim to answer sub-question 1: How do flight routes correlate with the likelihood of cancellations or delays? This question explores the relationship between flight routes and the punctuality of Australian domestic flights, measured by cancellation and delay rates. The exploration extends from routes to include the Departing Port of flights.

## Key Findings from DEP

- **Cancellation Rates**: 
  - Melbourne has the highest cancellation rate.
  - The Melbourne-Sydney route has the highest cancellation rate among all routes.
- **Delay Rates**: 
  - Sydney, Melbourne, and Brisbane are the top three ports with the highest delay rates.
  - Potential seasonal effects were suggested, influencing cancellation and delay rates.

## Target Audience

The target audience for this project includes:
- **Airline Companies**: To gain insights regarding flight performance and help in decision-making.
- **Travelers**: To assist in planning and making informed travel decisions.

## Data Processing

While the majority of data wrangling and cleansing were performed during DEP, additional processing steps were undertaken to prepare the data for visualization:
- **Filtering Incomplete Records**: 
  - Removed incomplete records over years and months to ensure full coverage for analysis.

Details of these processing steps are explained in the technical implementation section of the report.

## Visualization Design and Implementation

### Design Layout

The visualization design follows a Five Sheet design layout:
1. **Overall Summary**: Overview of cancellation and delay rates.
2. **Route Analysis**: Detailed visualization of cancellation and delay rates for different routes.
3. **Port Analysis**: Visualization of punctuality metrics for different departure ports.
4. **Seasonal Analysis**: Exploration of potential seasonal effects on flight performance.
5. **Interactive Map**: Geographical representation of routes and their performance metrics.

### Implementation using R Shiny

- **Goal**: To create a useful and informative webpage, allowing users to interactively explore patterns and findings between different routes and ports.
- **Tools and Technologies**: 
  - R Shiny for interactive web applications.
  - Data processing and wrangling performed in R.

## Conclusion

This report focuses on the visualization design using the Five Sheet design layout and its implementation using R Shiny. The goal is to create an informative and interactive webpage that allows users to explore patterns and findings related to flight routes and departure ports. The visualizations provide valuable insights for airline companies and travelers, aiding in decision-making processes.
