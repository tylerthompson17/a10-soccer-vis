# A10 Men's Soccer Visualization

This project explores the relationship between **team performance** and **scholarship resources** for Atlantic 10 (A10) men's soccer programs.

The analysis uses R to create two visualizations:

1. **CPCT vs Scholarship Spending**
2. **CPCT vs Number of Scholarships**

Each team is represented by its **logo** in the plots using the `ggimage` package.

## Data

Data is pulled from a public Google Sheets dataset containing:

* School
* Conference Points Percentage (CPCT)
* Scholarship dollars spent
* Number of scholarships

## Files

```
a10soccer.R        # Main analysis script
A10 Logos/         # Team logo images used in plots
figures/           # Generated plot images
README.md
```

## How to Run

Install required packages:

```
install.packages(c("tidyverse", "ggimage", "googlesheets4", "scales"))
```

Run the script:

```
source("a10soccer.R")
```

This will download the dataset and generate the visualizations.

## Author

Tyler Thompson
Davidson College — Mathematics & Computer Science
