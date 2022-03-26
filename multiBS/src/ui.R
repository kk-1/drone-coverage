
########################################################################################################
ui <- fluidPage(
  # tags$style(type='text/css', ".selectize-input { font-size: 32px; line-height: 32px;} 
  #                              .selectize-dropdown { font-size: 28px; line-height: 28px; }"),
  # 
  # Change the font size.
 # tags$style(type='text/css', "#select {font-size: 64px !important} "),
  
  tags$style(type = "text/css",
             "label { font-size: 20px; }",
             "select { font-size: 20px; }",
             "input { font-size: 20px; }",
             "#startPoly{  font-size: 20px;}",
             "#startBS{  font-size: 20px;}",
             "#clearSelectionRegion{  font-size: 20px;}",
             "#closePolygon{  font-size: 20px;}",
             "#DroneNumber{  font-size: 20px;}",
             "#DroneAlt{  font-size: 20px;}",
             "#DroneNumberMsg{  font-size: 20px;}",
             "#showVoronoi{  font-size: 20px;}",
             "#showInitSoln{  font-size: 20px;}",
             "#findCov{  font-size: 20px;}",
             "#myMsg{  font-size: 20px;}",
             "#clickMsg{  font-size: 20px;}",
             "#mouseCoord{  font-size: 20px;}",
             "#saveMap{  font-size: 20px;}",
             "#dl{  font-size: 20px;}",
             ),
  
  # tags$head(tags$style("#clearSelectionRegion{color: red;
  #                                font-size: 20px;
  #                                font-style: italic;
  #                                }"
  # )
  # ),
  # 
  useShinyjs(),
  # App title ----
  titlePanel("Region Coverage - V20"),
  
  
  sidebarLayout(
    
    sidebarPanel(
      
      div(style="display: inline-block;vertical-align:top;",
          switchInput(inputId = "startPoly", label = "Select Region Pts",  value = FALSE)),
      div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")),
      div(style="display: inline-block;vertical-align:top;",
          switchInput(inputId = "startBS", label = "Select BS Pos",  value = FALSE)),
      
      
      
      
      #  column(5,actionButton("startSelection", label = "Start Selection")),
      
      hr(),
      
      actionButton("clearSelectionRegion", label = "Clear Selection"),
      
      div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")),
      actionButton("closePolygon", label = "Close Polygon"), hr(),
      
      numericInput("DroneNumber", "Total drones[1-100]:", min = 1, value = totDroneReq, max = 100, step = 1),
      #verbatimTextOutput("OutDroneNumber"), hr(),
      sliderInput("DroneAlt", "Avg Drone Alt for Coverage:", min = hmin, value = hmed, max = hmax, step = 1),
      verbatimTextOutput("DroneNumberMsg",placeholder = TRUE),
      
       hr(),
      
      #actionButton("xshowBS", label = "DEBUG:Show all BS"), hr(),
      
      actionButton("showVoronoi", label = "Show Voronoi"),
      actionButton("showInitSoln", label = "Show Init Soln"),
      actionButton("findCov", label = "Find Coverage"), hr(),
      
      h5("Msg:"),
      verbatimTextOutput("myMsg",placeholder = TRUE),
      hr(),
      
      h5("Click msg:"),
      verbatimTextOutput("clickMsg",placeholder = TRUE),
      hr(),
      
      h5("Mouse coord:"),
      verbatimTextOutput("mouseCoord",placeholder = TRUE),
      hr(),
      
      actionButton("saveMap", "Save Map"),
      downloadButton("dl", "Download Map")
    ),
    
    mainPanel(
      # # 1. js to get width/height of map div
      # tags$head(tags$script('
      #                   var dimension = [0, 0];
      #                   $(document).on("shiny:connected", function(e) {
      #                   dimension[0] = document.getElementById("mymap").clientWidth;
      #                   dimension[1] = document.getElementById("mymap").clientHeight;
      #                   Shiny.onInputChange("dimension", dimension);
      #                   });
      #                   $(window).resize(function(e) {
      #                   dimension[0] = document.getElementById("mymap").clientWidth;
      #                   dimension[1] = document.getElementById("mymap").clientHeight;
      #                   Shiny.onInputChange("dimension", dimension);
      #                   });
      #                   ')),
      leafletOutput(outputId = "mymap",width="1200", height="750")
    )
  )
)


########################################################################################################

