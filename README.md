# Data-analytics
 # Association Rule Mining with R

# Author
- **Name**: Michael Tefera
- **ID**: 79451
- University of Cassino – Economics with Data Science
- 
## Project Overview

This project applies Association Rule Mining using the Apriori algorithm in R to analyze transactional recipe data.

The objective is to discover frequent itemsets and strong association rules based on support, confidence, and lift.

## Dataset
The dataset contains 
- Total transactions: 6,750
- Unique items: 29
- Average items per transaction: 5.39
- 
## Requirements
- R (version 4.0+)
- Required packages:
  - tidyverse
  - arules
  - arulesViz
  - RColorBrewer

## Installation
```R
install.packages(c("tidyverse", "arules", "arulesViz", "RColorBrewer"))

## Methods Used

- Frequent 1-itemsets, 2-itemsets, 3-itemsets
- Apriori algorithm
- Support, Confidence, Lift metrics
- Rule visualization:
  - Graph plot
  - Scatter plot
  - Matrix plot

---
