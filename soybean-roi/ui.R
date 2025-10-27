library(shiny)
library(shinythemes)
library(shinycssloaders)
library(DT)

source("auxiliar.R")

ga_id <- Sys.getenv("analytics")

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Head: meta + GA + styles
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    
    if (nzchar(ga_id)) tagList(
      tags$script(
        async = NA,
        src = sprintf("https://www.googletagmanager.com/gtag/js?id=%s", ga_id)
      ),
      tags$script(HTML(sprintf(
        "window.dataLayer = window.dataLayer || [];
         function gtag(){dataLayer.push(arguments);}
         gtag('js', new Date());
         gtag('config', '%s');", ga_id)))
    ),
    
    tags$style(HTML(paste0(
      custom_css2,
      "/* Add extra responsive rules here if needed */"
    )))
  ),
  
  # Navbar
  div(
    class = "navbar navbar-default navbar-static-top",
    style = "background-color: #006939; border: none; margin-bottom: 0;",
    div(class = "container-fluid",
        div(class = "navbar-header",
            span(class = "navbar-brand",
                 style = "color: white; font-size: clamp(16px, 4vw, 24px); font-weight: bold;",
                 "Fungicide ROI Calculator for White Mold of Soybean")
        )
    )
  ),
  
  # Main layout
  div(class = "container-fluid",
      div(class = "row",
          
          # Sidebar
          div(class = "col-md-4",
              div(class = "control-panel-section",
                  style = "height: 100%; background: #f8f9fa; padding: 20px;
                           box-shadow: 2px 0 10px rgba(0,0,0,0.05);
                           border-right: 3px solid #006939;",
                  
                  h4("Farm Conditions", style = "color: #006939; margin-top: 25px;"),
                  numericInput("yield", "Soybean Yield (bu/acre):", value = 40, min = 20, max = 100),
                  sliderInput("soybean_price", "Soybean Price ($/bushel):", min = 8, max = 18, value = 12, step = 0.1),
                  numericInput("disease_index", "Disease Severity Index %:", value = 30, min = 0, max = 50),
                  tags$small(textOutput("severity_level"), style = "font-weight: bold;"),
                  
                  h4("Treatment Costs", style = "color: #006939; margin-top: 25px;"),
                  p("Consult your local retailers for program costs as product prices vary across regions",
                    style = "font-style: italic; font-size: 0.85em; color: #444;"),
                  numericInput("application_cost", "Base Application Cost ($/acre):", value = 10, min = 0, max = 50),
                  uiOutput("fungicide_cost_inputs"),
                  actionButton("reset_costs", "Reset Costs", class = "btn btn-sm btn-outline-secondary"),
                  
                  br(), br(),
                  actionButton("calculate", "Calculate Profitability", class = "btn btn-lg btn-primary btn-block"),
                  conditionalPanel(
                    condition = "input.calculate > 0",
                    tags$small("Last updated: ", textOutput("last_calc_time", inline = TRUE),
                               style = "color: #666; font-style: italic;")
                  )
              )
          ),
          
          # Main Panel
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
  
  # Survey section — just a link
  div(class = "container-fluid",
      style = "margin-top: 30px; text-align: center;",
      tags$p(
        "Help us improve the tool! Take our quick survey:",
        tags$br(),
        tags$a(
          href = "https://uwmadison.co1.qualtrics.com/jfe/form/SV_3wwtihliQDB3lbM",
          target = "_blank",
          style = "font-weight: bold; font-size: 16px; color: #006939; text-decoration: underline;",
          "Open Survey"
        )
      )
  ),
  
  # Footer
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
