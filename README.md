# ShinyGamble

**rShiny Gambler: Simulated Quality-Adjusted Life Year Interviews**

An interactive R Shiny teaching tool that walks users through the interview methods used in pharmacoeconomics to elicit health utilities for Quality-Adjusted Life Years (QALYs). It takes about 15 minutes.

**Live app:** https://thayes.shinyapps.io/StandardGamble/

## What it does

Users rank three health conditions, then value each one using three common utility-elicitation methods:

1. **Visual Analog Scale (VAS):** rate each condition on a 0–100 scale.
2. **Standard Gamble (SG):** choose between living with a condition for certain and a gamble between perfect health and death, adjusting the odds until indifferent.
3. **Time Trade-Off (TTO):** choose between a full life expectancy with a condition and fewer years in perfect health.

The final tab shows the results from all three methods side by side so users can compare how the methods differ.

The age and gender inputs are used only to look up a realistic remaining life expectancy for the TTO interview. That lookup uses the U.S. Social Security Administration 2020 period life table (`SSA2020LifeTable.csv`).

## Repository layout

```
Code/
├── StandardGamble/   # Current app: VAS, Standard Gamble, and Time Trade-Off interviews
│   ├── app.R
│   └── SSA2020LifeTable.csv
└── TimeTradeOff/     # Older standalone Time Trade-Off prototype (superseded)
    ├── app.R
    └── SSA2020LifeTable.csv
```

> **Note:** `Code/TimeTradeOff/` is an older, standalone version of the Time Trade-Off interview, written before it was folded into the main app. It's kept for reference only. The current Time Trade-Off interview is the third interview in `Code/StandardGamble/`, which is the app deployed at the link above.

## Running locally

Requires R with these packages:

```r
install.packages(c("shiny", "plotly", "shinyWidgets", "sortable", "shinyjs", "DT"))
```

Then from the repository root:

```r
shiny::runApp("Code/StandardGamble")
```

## Acknowledgements

This is an R Shiny remake of the original *Automated Tool for Health Utility Assessments: The Gambler II* by Adejare and Eckman ([PubMed](https://pubmed.ncbi.nlm.nih.gov/32215320/)).

Life expectancy data: U.S. Social Security Administration, 2020 Period Life Table.

## License

[MIT](LICENSE) © 2024 Tristan Hayes
