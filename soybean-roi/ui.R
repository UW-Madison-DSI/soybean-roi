# Mobile-Responsive Dashboard UI
library(shiny)
source("auxiliar.R")

ga_id <- Sys.getenv("analytics")

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Custom header with title
  div(
    class = "navbar navbar-default navbar-static-top",
    style = "background-color: #006939; border: none; margin-bottom: 0;",
    div(
      class = "container-fluid",
      div(
        class = "navbar-header",
        span(
          class = "navbar-brand",
          style = "color: white; font-size: clamp(16px, 4vw, 24px); font-weight: bold;",
          "Fungicide ROI Calculator for White Mold of Soybean"
        )
      )
    )
  ),
  
  # Enhanced responsive CSS
  header = tagList(
    tags$head(
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
      tags$style(HTML(paste0(
        custom_css2,
        "
        /* Mobile-first responsive design */
        @media (max-width: 768px) {
          .container-fluid {
            padding: 0 5px !important;
          }
          
          .control-panel {
            margin-bottom: 20px !important;
            padding: 10px !important;
          }
          
          .input-section {
            margin-bottom: 15px !important;
          }
          
          .section-header h4 {
            font-size: 18px !important;
          }
          
          .navbar-brand {
            font-size: 16px !important;
          }
          
          .form-control {
            font-size: 16px !important; /* Prevents zoom on iOS */
          }
          
          .btn-lg {
            padding: 10px 15px !important;
            font-size: 16px !important;
          }
          
          /* Fix for mobile charts */
          .chart-panel {
            overflow-x: auto;
            -webkit-overflow-scrolling: touch;
          }
          
          /* Mobile-friendly tables */
          .dataTables_wrapper {
            font-size: 12px !important;
          }
          
          .dataTables_wrapper .dataTables_filter input {
            font-size: 14px !important;
          }
        }
        
        @media (max-width: 576px) {
          .row {
            margin: 0 !important;
          }
          
          .col-md-4, .col-md-8 {
            padding: 0 5px !important;
          }
          
          .navbar-nav {
            margin: 0 !important;
          }
          
          .nav-tabs {
            font-size: 14px !important;
          }
          
          .nav-tabs .nav-link {
            padding: 8px 12px !important;
          }
        }
        
        /* Tablet adjustments */
        @media (min-width: 769px) and (max-width: 1024px) {
          .container-fluid {
            padding: 0 15px !important;
          }
          
          .control-panel {
            padding: 15px !important;
          }
        }
        
        /* Control Panel Section Styling */
        .control-panel-section {
          background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
          border-bottom: 3px solid #006939;
          position: relative;
        }
        
        .control-panel-section::before {
          content: '';
          position: absolute;
          top: 0;
          left: 0;
          right: 0;
          height: 4px;
          background: linear-gradient(90deg, #006939 0%, #004a28 100%);
        }
        
        .control-card {
          transition: transform 0.3s ease, box-shadow 0.3s ease;
        }
        
        .control-card:hover {
          transform: translateY(-2px);
          box-shadow: 0 6px 20px rgba(0,0,0,0.15) !important;
        }
        
        .results-section {
          background: #ffffff;
          padding: 40px 0;
        }
        
        /* Card headers */
        .card-header {
          border-bottom: 2px solid #f8f9fa;
          margin-bottom: 20px;
          padding-bottom: 15px;
        }
        
        /* Better button styling */
        .btn-primary {
          background: linear-gradient(135deg, #006939 0%, #004a28 100%);
          border: none;
          border-radius: 6px;
          transition: all 0.3s ease;
        }
        
        .btn-primary:hover {
          transform: translateY(-2px);
          box-shadow: 0 4px 15px rgba(0, 105, 57, 0.3);
        }
        
        /* Improved form controls */
        .form-control, .form-control:focus {
          border-radius: 6px;
          border: 2px solid #dee2e6;
          transition: border-color 0.3s ease;
        }
        
        .form-control:focus {
          border-color: #006939;
          box-shadow: 0 0 0 0.2rem rgba(0, 105, 57, 0.25);
        }
        
        /* Results section improvements */
        .results-summary {
          background: linear-gradient(135deg, #e8f5e8 0%, #d4edda 100%);
          border-radius: 8px;
          padding: 20px;
          margin-bottom: 20px;
          border-left: 4px solid #006939;
        }
        
        .chart-panel {
          background: white;
          border-radius: 8px;
          padding: 20px;
          box-shadow: 0 2px 10px rgba(0,0,0,0.1);
          margin-bottom: 20px;
        }
        
        /* Footer adjustments */
        .my-footer {
          margin-top: 40px !important;
        }
        
        @media (max-width: 768px) {
          .my-footer {
            text-align: center !important;
            padding: 15px !important;
          }
          
          .my-footer .row > div {
            text-align: center !important;
            margin-bottom: 10px;
          }
        }
        "
      )))
    ),
    tags$script(HTML(sprintf(
      "<!-- Google tag (gtag.js) -->
      <script async src='https://www.googletagmanager.com/gtag/js?id=%s'></script>
      <script>
        window.dataLayer = window.dataLayer || [];
        function gtag(){dataLayer.push(arguments);}
        gtag('js', new Date());
        gtag('config', '%s');
      </script>",
      ga_id, ga_id
    )))
  ),
  
  # Control Panel Section
  # Left Sidebar Layout
  div(class = "container-fluid",
      div(class = "row",
          
          # Control Panel (Left Sidebar)
          div(class = "col-md-4",
              div(class = "control-panel-section",
                  style = "height: 100%; background: #f8f9fa; padding: 20px; 
                     box-shadow: 2px 0 10px rgba(0,0,0,0.05); 
                     border-right: 3px solid #006939;",
                  
                  # Farm Conditions
                  h4("Farm Conditions", style = "color: #006939; margin-top: 25px;"),
                  numericInput("yield", "Soybean Yield (bu/acre):", value = 40, min = 20, max = 100),
                  sliderInput("soybean_price", "Soybean Price ($/bushel):", min = 8, max = 18, value = 12, step = 0.1),
                  numericInput("disease_index", "Disease Severity Index %:", value = 30, min = 0, max = 50),
                  tags$small(textOutput("severity_level"), style = "font-weight: bold;"),
                  
                  # Treatment Costs
                  h4("Treatment Costs", style = "color: #006939; margin-top: 25px;"),
                  p("Consult your local retailers for program costs as product prices vary across regions",
                    style = "font-style: italic; font-size: 0.85em; color: #444;"),
                  numericInput("application_cost", "Base Application Cost ($/acre):", value = 10, min = 0, max = 50),
                  uiOutput("fungicide_cost_inputs"),
                  actionButton("reset_costs", "Reset Costs", class = "btn btn-sm btn-outline-secondary"),
                  
                  # Action
                  br(), br(),
                  actionButton("calculate", "Calculate Profitability", class = "btn btn-lg btn-primary btn-block"),
                  conditionalPanel(
                    condition = "input.calculate > 0",
                    tags$small("Last updated: ", textOutput("last_calc_time", inline = TRUE), style = "color: #666; font-style: italic;")
                  )
              )
          ),
          
          # Main Panel (Right Side)
          div(class = "col-md-8",
              div(class = "results-section",
                  conditionalPanel(
                    condition = "input.calculate > 0",
                    div(class = "results-summary",
                        h4("Analysis Summary"),
                        textOutput("summary_text")
                    )
                  ),
                  
                  tabsetPanel(
                    id = "main_tabs", type = "tabs",
                    tabPanel("Net Benefits",
                             div(class = "chart-panel",
                                 withSpinner(plotOutput("netbenefit_curve", height = "400px")),
                                 downloadButton("download_netbenefit", "Download CSV", class = "btn btn-success btn-sm"),
                                 DT::dataTableOutput("netbenefit_table")
                             )
                    ),
                    tabPanel("Breakeven Disease",
                             div(class = "chart-panel",
                                 withSpinner(plotOutput("breakeven_plot", height = "400px"))
                             )
                    )
                  )
              )
          )
      )
  ),
  
  # Responsive footer
  tags$footer(
    class = "my-footer",
    style = "background-color: #006939; color: white; padding: 20px; margin-top: 30px;",
    div(class = "container-fluid",
        div(class = "row",
            div(class = "col-md-6 col-12",
                HTML("&copy; Copyright 2025 University of Wisconsin&ndash;Madison<br/>
               Department of Plant Pathology")
            ),
            div(class = "col-md-6 col-12", style = "text-align: right;",
                HTML("Developed in collaboration with the <a href='https://dsi.wisc.edu' target='_blank' style='color: white; text-decoration: underline;'>Data Science Institute</a><br/>
               <small style='color: white;'>Version 1.0</small>")
            )
        )
    )
  )
)