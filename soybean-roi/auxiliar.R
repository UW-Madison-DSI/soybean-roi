# Load required libraries
library(shiny)
library(ggplot2)
library(scales)
library(shinythemes)
library(DT)
library(dplyr)
library(shinyWidgets)  # For enhanced UI elements
library(shinyBS)       # For tooltips
library(shinycssloaders) # For loading spinners

# Global Definitions ------------------------------------------------------
fungicides_specs <- list(
  #list(
  #  fungicide_program    = "Aproach-R1-Aproach-R3",
  #  mean_yield_increase  =  1.8934,
  #  se_yield_increase    =  2.6301,
  #  alias                = "Aproach R1 fb R3",
  #  rate = "9 fl oz/a",
  #  product_and_application_cost = 60.92,
  #  application = 'Reproductive'
  #),
  list(
    fungicide_program    = "Cobra-R1",

    alias                = "Cobra R1",
    rate = "8 fl oz/a",
    product_and_application_cost = 12.7,
    application = 'Reproductive',
    theta = 0.498969
  ),
  list(
    fungicide_program    = "Cobra-V4/V5",

    alias                = "Cobra V4/V5",
    rate = "8 fl oz/a",
    product_and_application_cost = 12.70,
    application = 'Vegetative',
    theta = 0.8233
  ),
  list(
    fungicide_program    = "DelaroComplete-R3",

    alias                = "Delaro Complete R3",
    rate = "8 fl oz/a",
    product_and_application_cost = 37.03,
    application = 'Reproductive',
    theta = 0.7076
  ),
  list(
    fungicide_program    = "Endura-R1-Endura-R3",
    mean_yield_increase  =  5.6391,
    se_yield_increase    =  1.3731,
    alias                = "Endura R1 fb R3",
    rate = "8 fl oz/a",
    product_and_application_cost = 102.64,
    application = 'Reproductive',
    theta = 0.35
  ),
  list(
    fungicide_program    = "Endura-R3",

    alias                = "Endura R3",
    rate = "8 fl oz/a",
    product_and_application_cost = 51.32,
    application = 'Reproductive',
    theta = 0.4755
  ),
  list(
    fungicide_program    = "Lektivar/Omega-R1-MiravisNeo-R3",

    alias                = "Omega R1 fb Miravis Neo R3",
    rate = "16+16 fl oz/a",
    product_and_application_cost = 106.96,
    application = 'Reproductive',
    theta = 0.6861
  ),
  list(
    fungicide_program    = "Lektivar/Omega-R3",

    alias                = "Omega R3",
    rate = "16 fl oz/a",
    product_and_application_cost = 69.04,
    application = 'Reproductive',
    theta = 0.6305
  ),
  list(
    fungicide_program    = "MiravisNeo-R3",
    alias                = "Miravis Neo R3",
    rate = "16 fl oz/a",
    product_and_application_cost = 37.92,
    application = 'Reproductive',
    theta = 0.826
  )
)


# ------------------------------------------------------------------------------
# Utility function: compute a tibble of (Program, NetBenefit) for a given rp, dix
# and the current set of input‐costs in your Shiny app
# ------------------------------------------------------------------------------
compute_netbenefit <- function(input, fungicides_specs, rp, dix_pct, input_vals, b0, b1) {
  # fungicides_specs : a list of lists/data‐frames, each with elements $alias and $theta
  # rp               : revenue potential (numeric)
  # dix_pct          : current DIX as a fraction (e.g. input$disease_index/100)
  # input_vals       : the entire 'input' object from Shiny
  # b0, b1           : fixed intercept/slope values used in your model
  #
  purrr::map_dfr(fungicides_specs, function(f) {
    # make.names() ensures that alias is a valid input‐ID suffix
    alias_id <- make.names(f$alias)
    cost_val  <- input_vals[[paste0("cost_", alias_id)]]
    theta_val <- f$theta
    
    if(alias_id=='Omega R1 fb Miravis Neo R3' || alias_id=='Oxidate 2.0 R1 fb R3'){
      cost_val2<-cost_val+2*input$application_cost
    }else{
      cost_val2<-cost_val+input$application_cost
    }
    
    netb_pt <- rp * (b0 + b1 * (1 - theta_val) * dix_pct) - cost_val2
    
    tibble::tibble(
      Program    = f$alias,
      NetBenefit = netb_pt
    )
  })
}


#' Build a named vector of program costs, safely pulling from input
#' @param input   The shiny input list
#' @param specs   List of fungicide specs, each with $alias and $product_and_application_cost
#' @return named numeric vector of costs
get_cost_vector <- function(input, specs) {
  purrr::map_dbl(specs, function(f) {
    id   <- paste0("cost_", make.names(f$alias))
    val  <- input[[id]]
    
    # if it's NULL or not numeric, fall back to the spec's default cost
    if (is.null(val) || !is.numeric(val)) {
      return(f$product_and_application_cost)
    }
    
    # otherwise coerce to numeric in case it's e.g. a string
    as.numeric(val)
  }) %>%
    magrittr::set_names(purrr::map_chr(specs, "alias"))
}


custom_css2 <- "
/* Change navbar background */
.navbar-default {
  background-color: #006939;
  border-color: #006939;
}

/* Change navbar text color */
.navbar-default .navbar-brand,
.navbar-default .navbar-nav > li > a {
  color: white;
}

/* Change navbar text color on hover */
.navbar-default .navbar-nav > li > a:hover {
  color: #6c757d;
}

/* Change primary button color */
.btn-primary {
  background-color: #006939;
  border-color: #006939;
  color: white;
}

.btn-primary:hover,
.btn-primary:focus {
  background-color: #6c757d;
  border-color: #006939;
  color: white;
}
"


# ------------------------------------------------------------------------------
# Utility function: reading simulations
# ------------------------------------------------------------------------------

compute_probability_single <- function(fungicide, Xt_list, exponential_data, alpha, beta, xnt, adjusted_cost, rev_potential) {
  if (!fungicide %in% names(Xt_list)) {
    stop(paste("Fungicide", fungicide, "not found in Xt_list."))
  }
  
  # Parse string to numeric vector
  Xt_string <- Xt_list[[fungicide]]
  Xt_array <- parse_string_vector(Xt_string)
  
  # Read exponential values
  reading_simulations <- exponential_data$exponential_values
  
  # Match lengths
  n <- min(length(Xt_array), length(reading_simulations))
  Xt_array_trimmed <- Xt_array[1:n]
  reading_sim_trimmed <- reading_simulations[1:n]
  
  # Compute expression values
  expression_values <- rev_potential * (alpha + beta * (xnt - Xt_array_trimmed)) + reading_sim_trimmed
  
  # Compute probability
  prob <- mean(expression_values >= adjusted_cost)
  
  # Return result
  data.frame(
    fungicide = fungicide,
    probability = prob,
    stringsAsFactors = FALSE
  )
}

# Helper to parse "[1.1, 2.2, 3.3]" to numeric vector
parse_string_vector <- function(x) {
  as.numeric(strsplit(gsub("\\[|\\]", "", x), ",")[[1]])
}




