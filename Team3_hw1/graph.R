graph1 = list(A = list(edges   = c(2L),
                       weights = c(1 )),
              B = list(edges   = c(3L),
                       weights = c(1 )),
              C = list(edges   = c(5L),
                       weights = c(1 )),
              D = list(edges   = c(2L),
                       weights = c(1 )),
              E = list(edges   = c(4L,6L),
                       weights = c(1,1  )),
              F = list(edges   = c(),
                       weights = c())
)

graph2 = list(A = list(edges   = c(2L),
                       weights = c(14)),
              B = list(edges   = c(3L,4L),
                       weights = c(23,13)),
              D = list(edges   = c(1L),
                       weights = c(5) ),
              F = list(edges   = c(1L,5L),
                       weights = c(43,33)),
              N = list(edges   = c(1L,2L,4L),
                       weights = c(33,22,11))
)

is_valid <- function(g){
###############################################
# The is_valid function takes a graph a determines if it is valid
#
#  Args
#   g - a structure to be tested
#
# Output
#   Returns a boolean.  TRUE if g is a valid graph, false otherwise
##############################################
  # check that g is a list and objects in g are lists
  if(any(c(class(g),sapply(g,class)) != "list")){
    return(FALSE)
  }
  # Here we test if the names of the sublists are unique
  if (length(names(g)) != length(unique(names(g))) ||
      length(names(g)) == 0 ) {
    return(FALSE)  
  }

  # Here we test whether second lists contain only edges and weights
  if(any(sapply(g, function(x) {sort(names(x))[1] != "edges" ||
                                sort(names(x))[2] != "weights" || 
                                # are the sublists named correctly
                                length(x) != 2}))){
                                # are their only two sublists
    return(FALSE)
  }
  checkList <- function(list){
  ###################################################
  # This function determines if the edges list is valid
  #
  # Input
  #   An edges vector
  #
  # Output
  #    TRUE/FALSE, whether edges list is valid
  ###################################################

    if (length(list) == 0){ # empty list is fine
      return(TRUE)
    } else {
      if(any(sapply(list, function(x){!is.integer(x)}))){
        return(FALSE)
      }
    }
    return(TRUE)
  } 
    checkWeights <- function(list){
    ###################################################
    # This function determines whether a list of edges
    # is valid.
    #
    # Input
    #   A list of weights
    #
    # Output
    #   TRUE/FALSE, whether weights are valid
    ###################################################

      if (length(list) != 0){
        # the empty list is ok.  If a list has length 0
        # it is automatically valid
        # further checks are only run if length > 0
        if (mode(list) != "numeric")
        # the weights should be numeric
        # but need not be integers
          return(FALSE)
      } 
      return(TRUE)
    } 
  # edges are appropariate type
  if(any(!sapply(g, function(x){checkList(x$edges)}))){
    return(FALSE)
  }
  # weights are of appropraite type
  if(any(!sapply(g, function(x){checkWeights(x$weights)}))){
    return(FALSE)
  }
  # Are there any NAs?  Is the list empty?
  if(any(sapply(g, function(x){any(is.na(x$edges)) & length(x$edges) > 0}))){
    return(FALSE)
  }
  # This section of code checks for edges to non-existant vertices
  if(any(sapply(g, function(x){any(x$edges < 0 | x$edges > length(g)) & length(x$edges) > 0}))){
    return(FALSE)
  }
  # are there multiple edges to same vertice?
  if (any(sapply(g, function(x) {length(unique(x$edges)) != length(x$edges)}))){
    return(FALSE)
  }
  # any weights negative or NA?
  if (any(sapply(g, function(x){any(x$weights <= 0 | is.na(x$weights)) & length(x$weights) > 0}))){
    return(FALSE)
  }
  # is there a weight for every edge
  if (any(sapply(g, function(x){length(x$edges) != length(x$weights)}))){
    return(FALSE)
  }
  # If graph has passed all above tests, it is valid
  # return TRUE
  return(TRUE)
}

### Test

###########################################################################
# This function will turn the graph list into a matrix, in which the rows #
# represent the starting vertex, and the colomn represent the ending      #
# vertex. 0 represents there is no path, while non-zero represents there  #
# is a path. The is_undirected, is_connected, is_isomorphic and shortest_ #
# path function is base on the graph_into_matrix function.                              #
###########################################################################
graph_into_matrix <- function(graph){# this function requires a list as an input
  vertex <- names(graph)# get the names of the elements in the list
  map <- matrix(0,length(vertex),length(vertex))# create an empty matrix
  rownames(map) <- vertex # change the row names to vertices
  colnames(map) <- vertex # change the column names to vertices
  for (i in 1:length(graph)){# the ith element in the list is the ith row in the matrix
    for (j in 1:length(graph)){# index the jth vertex, the jth column in the matrix
      if (j %in% graph[[i]][[1]]){# decide which vertex in edges
        map[i,j] <- graph[[i]][[2]][match(j,graph[[i]][1][[1]])] 
        # put the corresponding weight into the matrix
      }
    }
  }
  return(map)# return the matrix
}

###############################################################################
# This function will Check if the graph object is undirected, this is true if #
# all directed edges have a complementary directed edge with the same weight  #
# in the opposite direction. It requires a list, and return TRUE or FALSE.    #                       
###############################################################################
is_undirected <- function(g){# this function requires a list as an input
  stopifnot(is_valid(g)) # if the graph is not valid, an error will be returned
  map_graph <- graph_into_matrix(g)
  return(isSymmetric(map_graph))# return the matrix
}


is_isomorphic <- function(g1,g2){
  stopifnot(is_valid(g1) & is_valid(g2))
  if (length(g1) != length(g2)){
    return(FALSE)
  }
  map1 <- graph_into_matrix(g1)
  
  map2 <- graph_into_matrix(g2)
  
  o1 <- order(colnames(map1)) #Find out which order the colnames have
  map1 <- as.matrix(map1[o1,]) #Rearrange rows according to new order
  map1 <- map1[,o1] #Rearrange columns according to new order
  o2 <- order(colnames(map2)) #Find out which order the colnames have
  map2 <- as.matrix(map2[o2,]) #Rearrange rows according to new order
  map2 <- map2[,o2] #Rearrange columns according to new order
  
  if (all(map1 == map2)){ # All values in the matrix must be equivalent
    return(TRUE) # For the two matrices to be isomoprhic
  } else {
    return(FALSE)
  }
}


####################################################
# This function determines if any two vertices of a graph is connected
#
# Input
#   a graph, two vertices(need to specify the start and the end points)
#
# Output
#    TRUE/FALSE/ERROR, whether they are connected
###################################################
is_connected <- function(g, v1, v2){
  # check if the graph is valid, if not, stop
  if(!is_valid(g)) stop("validation error")
  # check if inputs are characters
  if(!is.character(v1) | !is.character(v2)) stop("input error")
  if(nchar(v1) > 1 | nchar(v2) > 1) stop("input error")
  if(!(v1 %in% names(g)) | !(v2 %in% names(g))) stop("input error")
  g <- graph_into_matrix(g)  # transform the graph into matrix
  # find out the corresponding index for v1
  for(i in 1:ncol(g)){
    if(colnames(g)[i] == v1){
      v1 <- i
    }
  }
  # find out the corresponding index for v2
  for(i in 1:ncol(g)){
    if(colnames(g)[i] == v2){
      v2 <- i
    }
  }
  visited <- c()  # initialize the visited queue
  num <- 0  # indicate the element(row) you are about to check
  i <- v1 
  while(num < ncol(g)){
    if(all(g[i,] == 0)){  #it means there is no connection to i
      return(FALSE)
    }
    for(j in setdiff(1:ncol(g),visited)){
      if(g[i,j] != 0 & j == v2){  # it indicates that we find the connection
        return(TRUE)
      }else if(g[i,j] != 0){
        visited <- c(visited,j) # we simply record those vertices that are connected to the current vertex
      }
    }
    num <- num + 1 
    i <- visited[num]  #go to the next recorded vertex
    if(is.na(i)){  #if there is no new vertex we return False
      return(FALSE)
    }
  }
  return(FALSE)
}

########################################################################
# This function find the shortest path from vertex v1 to vertex v2     #
# using the edges of graph g. The output will be a vector of the names #
# of vertices that make up the shortest path, in order. If there is no #
# path between the vertices then return an empty vector.               #
########################################################################
shortest_path <- function(g,v1,v2){
  # use the map function to get the matrix form of the graph
  map <- graph_into_matrix(g)
  
  # Initialization
  d <- c() # 
  pred <- c() # predecessor recording the previous node in optimal path from source
  vertex <- names(g)
  
  # if the graph is not valid, return an error 
  stopifnot(is_valid(g))
  
  # if v1 and v2 are not graph vertex, or they are not even vertex names, return an error
  stopifnot((v1 %in% vertex)&(v2 %in% vertex))
  
  # check if v1 and v2 are of length 1
  stopifnot((length(v1) == 1) & (length(v2) == 1))
  
  if (v1 == v2){
    for (i in vertex){
      d[i] <- ifelse(map[v1,i] != 0,map[v1,i],Inf) # initial distance from source to vertex v is set to infinite
      pred[i] <- v1 # Previous node in optimal path from source
    }
  }else{
     for (i in vertex){
       d[i] <- Inf # initial distance from source to vertex v is set to infinite
       pred[i] <- "" # initial node in optimal path from source
     } 
     d[v1] <- map[v1,v1]
  }
  
  # below are the dynamic recorder in the relaxation process
  dist <- d 
  Q <- vertex
  neighbour <- map[v1,]
  neighbour <- neighbour[which(ifelse(neighbour!=0,TRUE,FALSE))]
  neighbour <- names(neighbour)
  
  # main loop
  while (length(Q) != 0){ 
    Q <- Q[-which.min(dist)]
    for (i in neighbour) {
      alt <- min(dist) + map[names(which.min(dist)),i] # add the distance between vertex and vertex to its previous d 
      if (alt < dist[i]){ # if alt is smaller, replace it in dist 
        dist[i] <- alt
        pred[i] <- names(which.min(dist)) # the predecessor record the previous vertex
      }
    }
    
    d[names(dist)] <- dist[names(dist)] # replace d with what is in dist, because dist now it the shortest path so far
    dist <- dist[-which.min(dist)]
    neighbour <- map[names(which.min(dist)),names(dist)]
    neighbour <- neighbour[which(ifelse(neighbour != 0,TRUE,FALSE))]
    neighbour <- names(neighbour)
  }
  
  # three conditions to output routes 
  if ((d[v2] != Inf) & (v1 != v2)){ # this is when there is a shortest path from v1 to v2, and v1 is not the same as v2
    route <- c(pred[v2])
    # the while loop is consistently finding the previous vertex, and put it into route, until find the source vertex
    while (route[1] != v1){
      route <- c(pred[route[1]],route)
    }
    route <- c(route,v2) # complete the path by adding the destination vertex
  }else if(is_connected(g,v1,v2)&(v1 == v2)){ # this is when v1 and v2 are connected, and they are the same 
     if (d[v1] >= map[v1,v1]){ # first have to decide wether directly from v1 to itself is the shortest path
       route<-c(v1,v2) 
     }else if(d[v1] < map[v1,v1]){route <- c(pred[v2]) # if the path from v1 to itself is not the shortest, then find the shortest path using while loop 
        while (route[1] != v1){
          route <- c(pred[route[1]],route)
        }
        route <- c(route,v2)
      }
  }else{
     route <- c() # this is when v1 and v2 are the same, but not connected, return an empty vector
  } 
  
  return(route=unname(route)) # get rid of the name of the elements in the route vector
}
