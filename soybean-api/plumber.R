# plumber.R
library(plumber)
library(dplyr)
library(purrr)

source("auxiliar.R")


# ──────────────────────────────────────────────────
#* @apiTitle Soybean Program Fungicide Profitability API
#* @apiDescription Provides expected net‐benefit, 95% CI, and breakeven probability for each fungicide program applied on SSR.
#* @apiVersion 1.1.0
# ──────────────────────────────────────────────────

#* Health check
#* @get /health
function() {
  list(
    status    = "healthy",
    timestamp = Sys.time(),
    version   = "1.1.0"
  )
}

#* List all available fungicides
#* @get /program_applications
function() {
  purrr::map(FUNGICIDES_SPECS, function(f) {
    list(
      alias = f$alias,
      program = f$fungicide_program,
      rate = f$rate,
      default_cost = f$product_and_application_cost,
      application = f$application,
      proportional_disease_reduction = f$proportional_disease_reduction
    )
  })
}

#* Evaluate fungicide profitability
#* @param yield numeric Yield in bushels per acre (0–200)
#* @param soybean_price numeric Price per bushel (0–50)
#* @param disease_index numeric Disease index percent (0–100)
#* @param programs string Optional comma‐separated list of aliases to evaluate
#* @get /soy_profitability
function(req, res, yield = NULL, soybean_price = NULL, disease_index = NULL, programs = NULL) {
  tryCatch({
    yield <- if (is.null(yield)) NULL else as.numeric(yield)
    soybean_price <- if (is.null(soybean_price)) NULL else as.numeric(soybean_price)
    disease_index <- if (is.null(disease_index)) NULL else as.numeric(disease_index)
    
    requested_programs <- NULL
    if (!is.null(programs) && nchar(programs) > 0) {
      requested_programs <- trimws(strsplit(programs, ",")[[1]])
    }
    
    validation <- validate_inputs(yield, soybean_price, disease_index)
    if (!validation$valid) {
      res$status <- 400
      return(list(
        error       = "Input validation failed",
        details     = validation$errors,
        example_url = "/soy_profitability?yield=50&soybean_price=12.50&disease_index=25"
      ))
    }
    
    dix_frac <- disease_index / 100
    rp       <- yield * soybean_price
    
    # Filter specs if the user requested specific aliases
    fungicides_to_process <- if (!is.null(requested_programs)) {
      FUNGICIDES_SPECS[
        purrr::map_lgl(FUNGICIDES_SPECS, ~ .$alias %in% requested_programs)
      ]
    } else {
      FUNGICIDES_SPECS
    }
    
    if (length(fungicides_to_process) == 0) {
      res$status <- 400
      return(list(
        error              = "No valid fungicide programs found",
        available_programs = purrr::map_chr(FUNGICIDES_SPECS, ~ .$alias)
      ))
    }
    
    results <- purrr::map(fungicides_to_process, ~ process_fungicide(., rp, dix_frac, costs_user = NULL))
    results <- results[order(purrr::map_dbl(results, ~ .$expected_net_benefit), decreasing = TRUE)]
    
    list(
      inputs = list(
        yield             = yield,
        soybean_price     = soybean_price,
        disease_index     = disease_index,
        revenue_potential = round(rp, 2)
      ),
      summary = list(
        programs_evaluated   = length(results),
        best_program         = if (length(results) > 0) results[[1]]$alias else NULL,
        best_expected_benefit = if (length(results) > 0) results[[1]]$expected_net_benefit else NULL
      ),
      results  = results,
      metadata = list(
        timestamp     = Sys.time(),
        model_version = "1.1.0"
      )
    )
  }, error = function(e) {
    res$status <- 500
    list(
      error     = "Internal server error",
      message   = as.character(e),
      timestamp = Sys.time()
    )
  })
}

#* Enable CORS for all routes
#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET,POST,OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  } else {
    plumber::forward()
  }
}
