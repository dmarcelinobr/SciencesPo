library('shiny')

shinyUI(fluidPage(

  # titlePanel('Bayesian inference for normal mean (known variance)'),

  sidebarLayout(
    sidebarPanel(

      numericInput("y_data", label = "Observed data:", value = 45, step = 1L),
      numericInput("data_sigma", label = "Known variance of data:", value = 18, min = 0, step = 1L),
      hr(),
      numericInput("prior_mu", label = "Prior μ:", value = 61, step = 1L),
      numericInput("prior_sigma", label = "Prior σ:", value = 10, min = 0, step = 1L)

    ),

    mainPanel(
      plotOutput("dist_plot")
    )
  )

))
