library(shiny)

distns <- list("Binomial"="bin",
               "Geometric"="geo",
               "Negative Binomial"="nbin",
               "Hypergeometric"="hyp",
               "Poisson"="pois")
               
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Discrete Distributions"),

  # Sidebar with a slider input for number of observations
  sidebarPanel(
  
  radioButtons("typeDist",label="",choices=distns),
    conditionalPanel(condition = "input.typeDist == 'bin'",
        br(),
        numericInput("num_bin", "Number of trials (n):", min = 1, max = 100, value = 10),
        numericInput("prob_bin", "Probability of success (p):", min = 0, max = 1, value = 0.5, step=0.01)),
    conditionalPanel(condition = "input.typeDist == 'geo'",
        br(),
        numericInput("prob_geo", "Probability of success (p):", min = 0, max = 1, value = 0.5, step=0.01),
        numericInput("max_x_geo", "Maximum y:", min = 1, max = 100, value = 10)),
    conditionalPanel(condition = "input.typeDist == 'nbin'",
        br(),
        numericInput("num_suc_trials", "Number of successes (r):", min = 1, max = 50, value = 10),            
        numericInput("prob_nbin", "Probability of success (p):", min = 0, max = 1, value = 0.5, step=0.01),
        br(),
        numericInput("num_trials", "Maximum y:", min = 1, max = 100, value = 50), 
        helpText("Remember: The number of trials (y) must be greater than or equal to the number of successes (r).")),
    conditionalPanel(condition = "input.typeDist == 'hyp'",
        br(),
        numericInput("red_hyp", "Number of 'red' elements in the population (r):", min = 1, max = 100, value = 50),
        numericInput("sample_hyp", "Sample size (n):", min = 1, max = 100, value = 30),
        br(),
        numericInput("pop_hyp", "Population size (N):", min = 10, max = 200, value = 100),
        helpText("Remember: The sample size (n) must be less than or equal to the population size (N).  Also, the number of red elements (r) must be less than or equal to the population size (N).")),
    conditionalPanel(condition = "input.typeDist == 'pois'",
    	br(),
        numericInput("lambda", "Lambda:", min = 0, max = 20, value = 10, step=0.1),
        numericInput("max_x_pois", "Maximum y:", min = 1, max = 100, value = 50)),
        
	    br(),
	    checkboxInput("showMean", "Show mean on the probability histogram", FALSE),
	    checkboxInput("show1SD", "Show one standard deviation to either side of the mean", FALSE),
	   	checkboxInput("show2SD", "Show two standard deviations to either side of the mean", FALSE)
    ),

  mainPanel(
    tabsetPanel(
      tabPanel("Probability Histogram", plotOutput("ph_plot")), 
      tabPanel("Probability Mass Function", plotOutput("pmf_plot")), 
      tabPanel("Cumulative Distribution Function", plotOutput("cdf_plot"))
    )
  )
))

