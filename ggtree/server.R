library(shiny)
library(ggplot2)
library(shinyAce)
library(dplyr)
library(rvest)
# recursive approach! http://stackoverflow.com/questions/12818864/how-to-write-to-json-with-children-from-r
makeList <- function(x) {
  idx <- is.na(x[,2])
  if (ncol(x) > 2 && sum(idx) != nrow(x)){
    listSplit <- split(x[-1], x[1], drop=T)
    lapply(names(listSplit), function(y){list(name = y, children = makeList(listSplit[[y]]))})
  } else {
    nms <- x[,1]
    lapply(seq_along(nms), function(y){list(name = nms[y], value = x[,"value"][y])})
  }
}

# thanks Jeroen http://stackoverflow.com/questions/19734412/flatten-nested-list-into-1-deep-list
renquote <- function(l) if (is.list(l)) lapply(l, renquote) else enquote(l)

gg2tree <- function(gg) {
  la <- lapply(unlist(renquote(gg)), eval)
  # capture the output of the list values as one long character string
  vals <- lapply(la, function(x) paste(utils::capture.output(x), collapse = "<br>"))
  #names(vals) <- gsub("\\.", "_", names(vals))
  # preallocate matrix
  lvls <- strsplit(names(vals), "\\.")
  d <- max(sapply(lvls, length)) #maximum depth of the list
  m <- matrix(NA, nrow = length(lvls), ncol = d)
  for (i in seq_len(d)) m[,i]  <- sapply(lvls, function(x) x[i])
  m <- data.frame(m, value = as.character(vals))
  list(name = "ggplot", children = makeList(m))
}

p=mtcars%>%ggplot(aes(x=mpg,y=wt,colour=vs))+geom_point()+facet_wrap(~cyl)

shinyServer(function(input, output, session) {
  shinyURL.server()
  
  output$d3 <- reactive({
    return(list(root = gg2tree(p), layout = 'collapse'))
  })
  
  
  output$.table <- DT::renderDataTable(mtcars,
                                   extensions = c('Buttons',
                                                  'Scroller',
                                                  'ColReorder',
                                                  'FixedColumns'), 
                                   filter='top',
                                   options = list(  deferRender = TRUE,
                                                    dom='t',
                                                    scrollX = TRUE,
                                                    pageLength = 50,
                                                    scrollY = 500,
                                                    scroller = TRUE,
                                                    dom = 'Bfrtip',
                                                    colReorder=TRUE,
                                                    fixedColumns = TRUE,
                                                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis')
                                   ))
    
})