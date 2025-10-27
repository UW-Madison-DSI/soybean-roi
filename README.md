# Fungicide ROI Calculator for White Mold of Soybean

## Overview

This Shiny application helps soybean growers evaluate the economic return on investment (ROI) of different fungicide programs for white mold management. The tool calculates expected net benefits, confidence intervals, and breakeven disease severity thresholds based on field conditions and treatment costs.

## Features

- **Net Benefit Analysis**: Calculate expected returns for 8 different fungicide programs
- **Confidence Intervals**: View 95% confidence intervals for net benefit estimates
- **Breakeven Analysis**: Determine the disease severity threshold where each treatment becomes profitable
- **Customizable Inputs**: Adjust yield expectations, soybean prices, disease severity, and treatment costs
- **Interactive Visualizations**: Dynamic charts and tables for easy comparison
- **Mobile Responsive**: Works on desktop, tablet, and mobile devices
- **Data Export**: Download results as CSV files

## Installation

### Prerequisites

- R (version 4.0 or higher)
- RStudio (recommended)

### Required R Packages

```r
install.packages(c(
  "shiny",
  "ggplot2",
  "scales",
  "shinythemes",
  "DT",
  "dplyr",
  "shinyWidgets",
  "shinyBS",
  "shinycssloaders",
  "purrr",
  "tibble",
  "magrittr"
))
```

### Data Files

Ensure the following files are in the `simulations/` directory:
- `sampled_beta_values.rds` - Simulated treatment efficacy data
- `sampled_exponential_values.rds` - Simulated error distributions

## Running the Application

1. Clone or download the repository
2. Set your working directory to the app folder
3. Run in R console:

```r
shiny::runApp()
```

Or in RStudio, open `app.R` and click "Run App"

## Usage Guide

### Input Parameters

**Farm Conditions:**
- **Soybean Yield**: Expected yield in bushels per acre (20-100 bu/acre)
- **Soybean Price**: Current market price per bushel ($8-$18/bu)
- **Disease Severity Index**: Estimated white mold disease pressure (0-50%)

**Treatment Costs:**
- **Base Application Cost**: Cost per application (default: $10/acre)
- **Individual Program Costs**: Adjust specific fungicide product costs

### Fungicide Programs Included

1. **Cobra R1** (8 fl oz/a) - Single reproductive application
2. **Cobra V4/V5** (8 fl oz/a) - Vegetative application
3. **Delaro Complete R3** (8 fl oz/a) - R3 application
4. **Endura R1 fb R3** (8 fl oz/a) - Sequential applications
5. **Endura R3** (8 fl oz/a) - Single R3 application
6. **Omega R1 fb Miravis Neo R3** (16+16 fl oz/a) - Sequential program
7. **Omega R3** (16 fl oz/a) - Single R3 application
8. **Miravis Neo R3** (16 fl oz/a) - Single R3 application

### Interpreting Results

**Net Benefits Tab:**
- Point estimates show expected return per acre
- Horizontal error bars represent 95% confidence intervals
- Programs ranked by profitability
- Red line at $0 indicates breakeven point

**Breakeven Disease Tab:**
- Shows minimum disease severity needed for profitability
- Red dashed line indicates your current disease index
- Programs above the line are profitable at current conditions

**Breakeven Probability:**
- Percentage chance that the treatment will be profitable
- Based on 10,000 Monte Carlo simulations
- Accounts for uncertainty in efficacy and disease response

## Model Methodology

The calculator uses a linear response model:

```
Yield Response = β₀ + β₁ × (1 - θ) × DIX
```

Where:
- β₀ = 0.0733 (baseline response)
- β₁ = 0.55 (disease response coefficient)
- θ = program-specific efficacy coefficient
- DIX = Disease Severity Index (as decimal)

Net benefit is calculated as:
```
Net Benefit = (Yield × Price × Response) - Treatment Cost
```

## Technical Details

### File Structure

```
├── app.R                 # Main application file
├── ui.R                  # User interface definition
├── server.R              # Server logic
├── auxiliar.R            # Helper functions and data
└── simulations/          # Simulation data
    ├── sampled_beta_values.rds
    └── sampled_exponential_values.rds
```

### Key Functions

- `compute_netbenefit()`: Calculates net benefit for all programs
- `compute_probability_single()`: Estimates breakeven probability using simulations
- `parse_string_vector()`: Parses stored simulation data
- `get_cost_vector()`: Retrieves user-input costs safely

## Customization

### Adding New Fungicide Programs

Edit the `fungicides_specs` list in `auxiliar.R`:

```r
list(
  fungicide_program = "Program-Name",
  alias = "Display Name",
  rate = "Application rate",
  product_and_application_cost = 50.00,
  application = 'Reproductive',
  theta = 0.50  # Efficacy coefficient
)
```

### Modifying Model Parameters

Update global parameters in `server.R`:
```r
b0 <- 0.0733      # Intercept
b1 <- 0.55        # Slope
b0_se <- 0.029    # Standard error of intercept
b1_se <- 0.204    # Standard error of slope
```

## Deployment

### Deploying to shinyapps.io

```r
library(rsconnect)
rsconnect::deployApp()
```

### Local Server Deployment

Use Shiny Server or RStudio Connect for production hosting.

## Citation

If you use this tool in research or publications, please cite:

```
University of Wisconsin–Madison, Department of Plant Pathology
Fungicide ROI Calculator for White Mold of Soybean (2025)
Developed in collaboration with the Data Science Institute
```

## License

© Copyright 2025 University of Wisconsin–Madison

## Support & Contact

For questions, issues, or suggestions:
- Department of Plant Pathology, UW-Madison
- Data Science Institute: https://dsi.wisc.edu

## Version History

- **v1.0** (2025) - Initial release
  - 8 fungicide programs
  - Net benefit and breakeven analysis
  - Mobile-responsive design
  - Monte Carlo simulation integration

## Disclaimer

This tool provides economic estimates based on research data and should be used as a decision support aid. Actual results may vary based on environmental conditions, disease pressure, application timing, and other factors. Always consult with local extension specialists and consider multiple information sources when making treatment decisions.