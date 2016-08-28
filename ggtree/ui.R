library(shiny)
library(shinyAce)
library(shinyURL)

shinyUI(
  fluidPage(
    tags$head(
      tags$script(type="text/javascript", src = "d3.v3.js"),
      tags$script(type="text/javascript", src ="d3.tip.js"),
      tags$script(type="text/javascript", src ="ggtree.js"),
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'ggtree.css')
    ),
    
    titlePanel("Visualizing ggplot2 internals"),
    
    sidebarLayout(
      sidebarPanel(
        shinyURL.ui(ZeroClipboard.swf = "//cdn.jsdelivr.net/zeroclipboard/2.2.0/ZeroClipboard.swf")
      ),
      
    mainPanel(
        tabsetPanel(
          tabPanel('Tree',HTML("<div id=\"d3\" class=\"d3plot\"><svg /></div>")),
          tabPanel("Table", DT::dataTableOutput('.table'))
        )
      )
    )
  )
)