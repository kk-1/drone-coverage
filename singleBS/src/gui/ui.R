
########################################################################################################
# Define UI for app
########################################################################################################
ui <- fluidPage(
  
  #Change the font size for paper images!!!
  #Comment these lines for the user demo run
  tags$style(type = "text/css",
             "label { font-size: 20px; }",
             "select { font-size: 20px; }",
             "input { font-size: 20px; }",
             "output { font-size: 20px; }",
             "#initSolnPlot{  font-size: 20px;}",
             "#CovPlot{  font-size: 20px;}",
             "#runSim{  font-size: 20px;}",
             "#algo{  font-size: 20px;}",
             "#useInitSolSW{  font-size: 20px;}",
            ),
  
  # App title ----
  titlePanel("Drone Coverage - V2"),
  
  fluidRow(
    column(6, p("Initial Solution", style = "font-size:20px"), plotOutput(outputId = "initSolnPlot")),
    
    column(6, p("Final Solution", style = "font-size:20px"), plotOutput(outputId = "CovPlot")),
  ),
  
  hr(),
  
 
  fluidRow(
    column(3,
           actionButton("runSim", label = "Run Simulation"),
           
           sliderInput(inputId = "ndrones",
                       label = "Drones:",
                       min = 2,
                       max = 12,
                       step=2,
                       value = 6
           ),
           
           sliderInput(inputId = "MaxIteration",
                       label = "Max Iteration:",
                       min = 20,
                       max = 200,
                       step = 10,
                       value = 30
           ),
           
           
           
           
           
    ),
    
    column(3,
           
           
           selectInput("algo", "Choose algorithm:",
                       list("GA", "genoud", "DEoptim", "GenSA","optimParallel")
           ),
           
           switchInput(inputId = "useInitSolSW", label = "Use init Soln",  value = FALSE),
           
    ),
    
    
    
    column(3,
          
           
           sliderInput(inputId = "xwCoverage",
                       label = "Coverage weight +:",
                       min = 0,
                       max = 4,
                       step = 0.5,
                       value = 1.5
           ),
           
           sliderInput(inputId = "xwOverflow",
                       label = "Overflow weight -:",
                       min = -4,
                       max = 0,
                       step = 0.5,
                       value = -1
           ),
           
          
           
          
    ),
    
    
    
    column(3,
           
            sliderInput(inputId = "xwDist",
                       label = "Dist diff weight +:",
                       min = 0,
                       max = 4,
                       step = 0.5,
                       value = 1.5
           ),  
           
         
           
           sliderInput(inputId = "xwOverlap",
                       label = "Overlap weight -:",
                       min = -4,
                       max = 0,
                       step = 0.5,
                       value = -1
           ),
           
          
          
    ),
    
    
    
  ),
  
   hr(),
  
   fluidRow(
    column(5,verbatimTextOutput("msgInit")),
    
    column(5,verbatimTextOutput("msgFinal"))
  ),
  
  hr(),
  fluidRow(
    column(6, htmlOutput("info")),
  ), 

)

########################################################################################################
