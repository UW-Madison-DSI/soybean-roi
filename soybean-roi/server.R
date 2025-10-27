library(ggplot2)
library(scales)
library(shinythemes)
library(DT)
library(dplyr)
library(shinyWidgets) 
library(shinyBS)      
library(shinycssloaders)

source("auxiliar.R")

#################################### Global parameters
b0 <- 0.0733 
b1 <- 0.55 
b0_se <- 0.029
b1_se <- 0.204

# Load data
dis <- readRDS("simulations/sampled_beta_values.rds")
Xt_list <- setNames(dis$list_of_simulated_dix_treatment, dis$program)
normal_errors <- readRDS("simulations/sampled_exponential_values.rds")


server <- function(input, output, session) {
  
  # Create reactive values to store calculation state
  values <- reactiveValues(
    calculate_triggered = 0,
    last_results = NULL
  )
  
  # Observe the calculate button
  observeEvent(input$calculate, {
    values$calculate_triggered <- values$calculate_triggered + 1
  })
  
  # Reset costs to default
  observeEvent(input$reset_costs, {
    for (f in fungicides_specs) {
      input_id <- paste0("cost_", make.names(f$alias))
      updateNumericInput(
        session,
        input_id,
        value = f$product_and_application_cost
      )
    }
  })
  
  output$fungicide_cost_inputs <- renderUI({
    # 1) sort your specs by alias
    sorted_costs <- fungicides_specs[
      order(sapply(fungicides_specs, `[[`, "alias"))
    ]
    
    # 2) split into chunks of 2
    specs_by_row <- split(
      sorted_costs,
      ceiling(seq_along(sorted_costs) / 2)
    )
    
    # 3) build one fluidRow per chunk
    rows <- lapply(specs_by_row, function(chunk) {
      cols <- lapply(chunk, function(f) {
        input_id <- paste0("cost_", make.names(f$alias))
        column(
          width = 6,
          style = "margin-bottom: 20px;",
          div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            div(class = "cost-input-label", f$alias),
            div(style = "font-size: 14px; color: #6c757d;", paste0("Rate: ", f$rate))
          ),
          div(
            style = "width: 100%; margin-top: 5px;",
            div(
              style = "position: relative;",
              numericInput(
                inputId = input_id, label = NULL,
                value = f$product_and_application_cost,
                min   = 0, width = "100%"
              ),
              tags$span(
                style = "position: absolute; top: 6px; right: 10px;", "$"
              )
            )
          )
        )
      })
      # if only one in this row, add an empty column so layout stays consistent
      if (length(cols) == 1) cols <- c(cols, column(6))
      fluidRow(cols)
    })
    
    do.call(tagList, rows)
  })
  
  # Shared reactive expression
  netbenefit_df <- reactive({
    req(input$yield, input$disease_index, input$soybean_price)
    
    purrr::map_dfr(fungicides_specs, function(f) {
      alias_id <- make.names(f$alias)
      
      theta    <- f$theta
      rp       <- input$yield * input$soybean_price
      app_cost <- ifelse(grepl("fb", f$alias), 20, 10)
      cost     <- input[[paste0("cost_", alias_id)]]+app_cost
      
      netb_pt <- rp * (b0 + b1 * (1 - theta) * input$disease_index / 100) - cost
      low_netb_pt <- rp * ((b0 - 1.9 * b0_se) + (b1 - 1.9 * b1_se) * (1 - theta) * input$disease_index / 100) - cost
      up_netb_pt  <- rp * ((b0 + 1.9 * b0_se) + (b1 + 1.9 * b1_se) * (1 - theta) * input$disease_index / 100) - cost
      # Compute for one fungicide
      
      break_probab <- compute_probability_single(
        fungicide = f$alias,
        Xt_list = Xt_list,
        exponential_data = normal_errors,
        alpha = b0,
        beta = b1,
        xnt = input$disease_index/100,
        adjusted_cost = cost,
        rev_potential = rp
      )
      
      tibble::tibble(
        Program                  = f$alias,
        `Application Cost`       = dollar(input[[paste0("cost_", alias_id)]] + app_cost),
        `Net Benefit`            = dollar(round(netb_pt, 2)),
        `Lower 95% CI`           = dollar(round(low_netb_pt, 2)),
        `Upper 95% CI`           = dollar(round(up_netb_pt, 2)),
        `Breakeven Probability` = paste0(round(100 * break_probab$probability, 2), "%")
      )
      
    })
  })
  
  # Render DataTable
  output$netbenefit_table <- DT::renderDataTable({
    DT::datatable(
      netbenefit_df(),
      rownames = FALSE,
      options = list(
        pageLength = 10,
        dom = 'tip',
        ordering = TRUE
      )
    )
  })
  
  # Download Handler
  output$download_netbenefit <- downloadHandler(
    filename = function() {
      paste0("net_benefit_summary_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(netbenefit_df(), file, row.names = FALSE)
    }
  )
  
  output$summary_text <- renderText({
    req(input$yield, input$soybean_price, input$disease_index)
    
    # 1) Compute revenue potential (rp) and current DIX as a fraction
    rp       <- input$yield * input$soybean_price
    dix_pct  <- input$disease_index / 100
    
    # 2) Call the helper function to get a data.frame of (Program, NetBenefit)
    df_nb <- compute_netbenefit(
      input,
      fungicides_specs = fungicides_specs,
      rp = rp,
      dix_pct = dix_pct,
      input_vals = input,
      b0 = b0,
      b1 = b1
    )
    
    # 3) Identify best and worst
    best_row  <- df_nb %>% dplyr::filter(NetBenefit == max(NetBenefit))
    worst_row <- df_nb %>% dplyr::filter(NetBenefit == min(NetBenefit))
    
    # 4) Format dollar amounts
    best_val  <- scales::dollar(best_row$NetBenefit)
    worst_val <- scales::dollar(worst_row$NetBenefit)
    rp_str    <- scales::dollar(rp)
    dix_pct_str <- scales::percent(dix_pct, accuracy = 1)
    
    # 5) Build summary sentence
    paste0(
      "At a Disease Index of ", dix_pct_str, " and revenue potential of ",
      rp_str, ", the program with the highest expected net benefit per acre is “",
      best_row$Program, "” (", best_val, "). The lowest is “",
      worst_row$Program, "” (", worst_val, ") given its respective application costs per acre."
    )
  })
  
  output$netbenefit_curve <- renderPlot({
    req(input$yield, input$disease_index, input$soybean_price)
    
    df <- purrr::map_dfr(fungicides_specs, function(f) {
      alias_id <- make.names(f$alias)
      cost <- input[[paste0("cost_", alias_id)]]
      theta <- f$theta
      
      # revenue potential (bu * $/bu)
      rp <- input$yield * input$soybean_price
      
      # point estimate of net benefit at current DIX
      netb_pt <- rp * (b0 + b1 * (1 - theta) * input$disease_index/100) - cost
      
      # lower and upper bounds of net benefit (approx. 95% CI using ±1.9*SE)
      low_netb_pt <- rp * (
        (b0 - 1.9 * b0_se) +
          (b1 - 1.9 * b1_se) * (1 - theta) * input$disease_index/100
      ) - cost
      
      up_netb_pt <- rp * (
        (b0 + 1.9 * b0_se) +
          (b1 + 1.9 * b1_se) * (1 - theta) * input$disease_index/100
      ) - cost
      
      tibble::tibble(
        Program = f$alias,
        NetBenefit = netb_pt,
        NetBenefit_lo = low_netb_pt,
        NetBenefit_hi = up_netb_pt
      )
    })
    
    # reorder so that the program with the largest point estimate appears at the top
    df$Program <- reorder(df$Program, df$NetBenefit)
    
    ggplot(df, aes(y = Program)) +
      
      # 1. Horizontal error bars representing 95% CI
      geom_errorbarh(
        aes(xmin = NetBenefit_lo, xmax = NetBenefit_hi),
        height = 0.2,
        color = "steelblue",
        size = 1
      ) +
      
      # 2. Point for the estimated net benefit
      geom_point(
        aes(x = NetBenefit),
        size = 2.5,
        color = "darkred"
      ) +
      
      # 3. Reference line at Net Benefit = 0
      geom_vline(
        xintercept = 0,
        linetype = "dashed",
        color = "grey60"
      ) +
      
      # 4. Format x-axis as currency and expand for label spacing
      scale_x_continuous(
        labels = scales::dollar_format(),
        expand = expansion(mult = c(0, 0.2))
      ) +
      
      # 5. Plot labels and caption
      labs(
        title    = "Net Benefit Estimates ($/acre) with 95% Confidence Intervals",
        subtitle = sprintf("Revenue Potential: %d bu × $%g/bu", input$yield, input$soybean_price),
        x        = "Net Benefit ($/acre)",
        y        = NULL,
        caption  = "Horizontal lines represent approximate 95% confidence intervals."
      ) +
      
      # 6. Minimal clean theme
      theme_minimal(base_size = 15) +
      theme(
        plot.title       = element_text(face = "bold", size = 16),
        plot.subtitle    = element_text(color = "grey40", size = 13),
        axis.text.y      = element_text(size = 12),
        axis.title.x     = element_text(size = 13),
        panel.grid.major.y = element_blank()
      )
    
  })
  
  output$breakeven_plot <- renderPlot({
    req(input$disease_index, input$yield, input$soybean_price)
    
    # 1) Compute the current revenue potential:
    rp_current <- input$yield * input$soybean_price
    
    # 2) For each program, compute its break‐even DIX at rp_current:
    df_be_current <- purrr::map_dfr(fungicides_specs, function(f) {
      alias_id <- make.names(f$alias)
      cost     <- input[[paste0("cost_", alias_id)]]
      theta    <- f$theta
      
      # Raw break‐even DIX:
      dix_raw <- ((cost / rp_current) - b0) / (b1 * (1 - theta))
      
      # Clamp to [0, 0.8]:
      dix_clamped <- pmin(pmax(dix_raw, 0), 0.8)
      
      tibble::tibble(
        Program      = f$alias,
        BreakEvenDIX = dix_clamped
      )
    })
    
    # 3) Reorder so that highest BE-DIX is at top:
    df_be_current$Program <- reorder(df_be_current$Program, df_be_current$BreakEvenDIX)
    
    # 4) Draw horizontal bars, plus a line at current DIX:
    ggplot(df_be_current, aes(x = Program, y = BreakEvenDIX)) +
      geom_col(fill = "steelblue", width = 0.7) +
      coord_flip() +
      # Add a horizontal line at current disease index:
      geom_hline(
        yintercept = input$disease_index / 100,
        linetype   = "dashed",
        color      = "red",
        size       = 0.8
      ) +
      scale_y_continuous(
        labels = scales::percent_format(accuracy = 1),
        limits = c(0, 0.8)
      ) +
      labs(
        title = "Break‐Even Disease Severity Index %",
        subtitle = paste0(
          "Red dashed line = your current DIX (",
          scales::percent(input$disease_index / 100, accuracy = 1),
          ")"
        ),
        x     = NULL,
        y     = "Break‐Even Disease Severity Index %"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title   = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey40"),
        axis.text.y  = element_text(size = 12),
        axis.title.y = element_blank()
      )
  })
}