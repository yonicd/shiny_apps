library(shiny)
library(ggplot2)
library(grid)
library(dplyr)
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

ui=fluidPage(
  checkboxGroupInput(inputId = 'PlotType',label = 'plottype',choices = c('a','b','c','d'),selected='c'),
  selectInput(inputId = 'varx',label = 'varx',choices = names(mtcars),selected = 'cyl'),
  selectInput(inputId = 'vary',label = 'vary',choices = names(mtcars),selected = 'mpg'),
  checkboxInput(inputId = 'factor',label='Var X Factor',value = F),
  sliderInput(inputId = 'slide',label = 'Filter',min = min(mtcars$mpg),max = max(mtcars$mpg),value = c(10,20),step=.1),
  plotOutput("ggplot")
)

server=function(input, output, session) {

  
  p=reactive({
    varx.in=input$varx
    vary.in=input$vary
    if(input$factor) varx.in=paste0('factor(',varx.in,')')
    aes.in=aes_string(x=varx.in,y=vary.in)
    p=vector('list',4)
    names(p)=letters[1:4]
    m=mtcars%>%mutate(lbl=rownames(mtcars),id=1:nrow(mtcars))
    m=m%>%filter(mpg>=input$slide[1]&mpg<=input$slide[2])
    p0=m%>%ggplot()
    p[[1]] <- p0+geom_line(aes(x=id,y=disp))
    p[[2]] <- p0+geom_density(aes(x=mpg))
    p[[3]] <- p0+geom_boxplot(aes.in)
    p[[4]] <- p0+geom_text(aes(x=mpg,y=disp,label=lbl))
    return(p)}
  )
  
  output$ggplot=renderPlot(expr = {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(1,length(input$PlotType))))
    x=input$PlotType
    for(i in 1:length(x))
      print(p()[[x[i]]], vp = vplayout(1, i))
  })
  
}


shinyApp(ui = ui, server = server)
