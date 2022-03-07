#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(rgdal)
library(shiny)
library(raster)
library(geosphere)  


world <- shapefile('./ne_10m_admin_0_countries.shp')
world$AREA <- area(world) / 1000000

centroids <- shapefile('./country_centroids.shp')



#===============================================================================
#===============================================================================


choose.diff <- function(world, level) {
  
  if(level == 1){
    world = world[world$AREA > 1001058,]
  }
  else if(level == 2){
    world = world[world$AREA > 399901,]
  }
  else if(level == 3){
    world = world[world$AREA > 198990,]
  }
  else if(level == 4){
    world = world[world$AREA > 56863,]
  }
  
  return(world)
}

#===============================================================================
#===============================================================================


calculate.dist <- function(name.guess, country.name, centroids){
  
  point.guess <- centroids[centroids$NAME_EN == name.guess,]
  point.country <- centroids[centroids$NAME_EN == country.name,]

  two.pts <- rbind(point.guess, point.country)
  
  dist.km <- distGeo(two.pts) / 1000

  return(dist.km[1])}

#===============================================================================
#===============================================================================

calc.celestial.dir <- function(name.guess, country.name, centroids){
  
  point.guess = centroids[centroids$NAME_EN == name.guess,]
  point.country = centroids[centroids$NAME_EN == country.name,]

  x.guess = coordinates(point.guess)[1]
  x.country = coordinates(point.country)[1]
  y.guess = coordinates(point.guess)[2]
  y.country = coordinates(point.country)[2]
  
  if(x.country > x.guess){
    west.east <- "East"}else{west.east <- "West"}
  if (y.country > y.guess){
    north.south <- "North"}else{north.south <- "South"}
  
  location <- paste0(north.south,"-",west.east)
  
  return(location)
}


#===============================================================================
#===============================================================================

guess.check <- function(country.name, name.guess, centroids){
  
  guess.isTrue <- country.name == name.guess
  if (guess.isTrue){
    message <- "Aweeeeh!!! That's absolutely correct :)"
  }else{
    
    dist.km <- calculate.dist(name.guess, country.name, centroids)
    direction <- calc.celestial.dir(name.guess, country.name, centroids)
    
    message <- paste(strwrap(paste0(
                      "That's not correct :( The land you are looking for is ",
                      round(dist.km),
                      "km away and lies in the direction of  ",
                      direction), 25),
                      collapse="\n")
  }
  return(message)
}



#===============================================================================
#===============================================================================

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("WORLDLY"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "level", label = "Choose a level and click enter: ",
        min = 1, value = 1, max = 5
      ),
      actionButton("submit", label = "Enter"),
      br(),br(),
      selectizeInput("name.guess", "Which country do you see?", world$NAME_EN),
      actionButton("submit.2", label = "Enter"),
      br(),br(),
      verbatimTextOutput("results"),
      actionButton("submit.3", label = "No clue? Want an anser? Click here"),
      br(),br(),
      verbatimTextOutput("answer")
    ),
    mainPanel(
      plotOutput("outputPlot"),
      br(),br(),
      verbatimTextOutput("citation")
    )
  )
)  


# Define server logic required to draw a histogram
server <- function(input, output) {

  res.reactive <- eventReactive( input$submit, {
    world <- choose.diff(world, input$level)
    country <- world[sample(1:nrow(world), 1),]
    country.name <- country$NAME_EN
    citation <- paste(strwrap("Made with Natural Earth. Free vector and raster map data @ naturalearthdata.com.", 200),
      collapse="\n")

    list(country, country.name, citation)
  })
  
  res.reactive.2 <- eventReactive( input$submit.2, {
    message <- guess.check(res.reactive()[[2]], input$name.guess, centroids)
  })
  
  
  res.reactive.3 <- eventReactive( input$submit.3, {
    res.reactive()[[2]]
  })
  
  
  output$outputPlot <- renderPlot({
    plot(res.reactive()[[1]], col="tomato4")
  })
  
  output$citation <- renderText({
    res.reactive()[[3]]
  })
     
  output$results <- renderText({
    res.reactive.2()
  })
  
  output$answer <- renderText({
    res.reactive.3()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

