#We only did plot_graph function here


source("graph.R")



plot_graph <- function(graph)
{
  # Check if the graph is valid
  if(!is_valid(graph)) stop("error")
  # transform the graph into a matrix(for some degree of convenience)
  g <- graph_into_matrix(graph)
  # the graph will be plotted differently according to the weights
  weights <- NULL
  for (i in 1:ncol(g))
  {
    weights <- c(weights, g[i,g[i,]!=0])
  }
  
  
  
  # If all the weights are equal to 1, we simply plot them.
  if (length(unique(weights)) == 1 && unique(weights) == 1)
  {
    # create points and marked their names
    x <- runif(ncol(g),min=0,max=50)
    y <- runif(ncol(g),min=0,max=50)
    plot(x, y, xlab = "", ylab = "", xaxt = "n", yaxt = "n", pch = ".")
    text(x, y, labels = colnames(g))   
    for (i in 1:ncol(g))
    {
      flag <- c()
      # Check if the vertex has edges
      if (length(graph[[i]]$edges) > 0)
      {
        for (j in graph[[i]]$edges)
        {
          flag <- c(flag,j)
        }
      }
      lines(x[c(i,flag)],y[c(i,flag)])
    }
  } 
  
  
  # If the edges are weighted, include weight in the lines
  # Unlike the above situation, we can't improve this to plot a graph
  # Instead we simply print all the lines
  else
  {
    for (i in 1:ncol(g))
    {
      # Check if the vertex has edges
      if (length(graph[[i]]$edges) > 0)
      {
        for ( j in 1:length(graph[[i]]$edges) )
        {
          from <- colnames(g)[i]      
          to <- colnames(g)[as.numeric(graph[[i]]$edges[j])]
          weight <- as.numeric(g[i,g[i,]!=0][j])
          line <- paste0(from, " -> ",to, " " ,"[weight=", weight, "]")
          print(line)
        }
      } else
      {
        from <- colnames(g)[i]
        line <- paste0(from,";")
        print(line)
      }
    }
  }
}
