library(igraph)
library(ggplot2)
library(optrees)
library(tictoc)

tic("sleeping")
# your data
mat <- as.matrix(read.table(text=
                              "node Atlanta Chicago St.Louis Springfield 
                            Atlanta 0 312 408 508 
                            Chicago 468 0 234 342
                            St.Louis 816 312 0 208
                            Springfield 508 228 104 0 ", header= TRUE))
suppressWarnings(as.numeric(mat))
# prepare data for graph functions - set NA to zero to indicate no direct edge
nms <- mat[,1]
mat <- mat[, -1]
ncol(mat)
colnames(mat) <- rownames(mat) <- nms
mat[is.na(mat)] <- 0


# create graph from adjacency matrix
g <- graph.adjacency(mat, weighted=TRUE)
plot(g)

# Get all path distances
(s.paths <- shortest.paths(g, algorithm = "dijkstra"))

toc()

tic("sleeping")
#Bellman_Ford Algorithm
nodes <- 1:4
arcs <- matrix(c(1,2,873, 1,3,778, 1,4,1356, 
                 2,1,636, 2,3,552, 2,4,960, 
                 3,1,678, 3,2,597, 3,4,548,
                 4,1,642, 4,2,585, 4,3,338), ncol = 3, byrow = TRUE)
# Shortest path tree
getShortestPathTree(nodes, arcs, algorithm = "Bellman-Ford",
                    source.node = 1, directed=TRUE, show.data = TRUE, 
                    show.graph = TRUE, show.distances = TRUE)
getShortestPathTree(nodes, arcs, algorithm = "Bellman-Ford",
                    source.node = 2, directed=TRUE, show.data = TRUE, 
                    show.graph = TRUE, show.distances = TRUE)

getShortestPathTree(nodes, arcs, algorithm = "Bellman-Ford", 
                    source.node = 3, directed=TRUE, show.data = TRUE, 
                    show.graph = TRUE, show.distances = TRUE)

getShortestPathTree(nodes, arcs, algorithm = "Bellman-Ford", 
                    source.node = 4, directed=TRUE, show.data = TRUE, 
                    show.graph = TRUE, show.distances = TRUE)

toc()


#Dijkstra's Algorithm
S=matrix(999,7,7)
S = mat
### List of input parameters for function
n=length(S[,1]) #number of nodes
v=1 #source node
dest=n #destination node
cost=S #distance matrix

### Dijkstra's algorithm
dijkstra=function(n,v,cost,dest){
  
  #create empty variables to store data
  dest = numeric(n)
  flag = numeric(n)
  prev = numeric(n)
  
  # for every node in the network
  for(i in 1:n){
    prev[i] = -1
    dest[i] = cost[v,i] #= distance from start node v to every other node i in the network
  }
  
  #initialise counter which keeps track of number of steps through network
  count=2
  
  # until we have reached our destination node n
  while(count <= n){
    min=999
    
    # loop over each node
    for(w in 1:n){
      #if the new path is less long than the existing smallest one and flag[w] is equal to zero (aka we've not already incuded that node in route)
      if(dest[w] < min && !flag[w]){
        # overwrite the minimum with the new shortest path and update counter
        min=dest[w]
        u=w
      }
    }
    flag[u] = 1 #indicate that we go to this site
    count = count+1
    
    # loop over each node again keeping in mind where we have already been
    for(w in 1:n){
      #if the new route is shorter than the previous route
      if((dest[u]+cost[u,w] < dest[w]) && !flag[w]){
        dest[w]=dest[u]+cost[u,w] #update the distance to destination
        prev[w]=u #keep track of the node visited
      }
    }
  }
  return(prev)
}

### create function which returns path
savepath = function(f,x){
  path=x
  while(f[x] != -1){
    path=c(path,f[x])
    x=f[x]
    savepath(f,x)
  }
  path=c(path,1)
  return(path)
}

### Run Dijkstra's algorithm with our distance matrix
prev = dijkstra(n,v,cost,dest)
path = savepath(prev,dest)

### Print path
path


# Libraries Used
library(reshape2)
library(igraph)
library(optrees)
library(Rfast)
library(tictoc)

# Reading the 1st data (travelling cost)
data <- read.csv("Data.csv")
# pre processing the data into more usable format
data$Source.Destination <- as.character(data$Source.Destination)     # converting factor to character
data$Source.Destination <- gsub("\\s", "", data$Source.Destination)  # removing whitespaces

# adding other required rows
data[nrow(data) + 1,] = list('Atlanta(ATL)-Atlanta(ATL)', 0, 0)
data[nrow(data) + 1,] = list('Chicago(ORD)-Chicago(ORD)', 0, 0)
data[nrow(data) + 1,] = list('Springfield(SPI)-Springfield(SPI)', 0, 0)
data[nrow(data) + 1,] = list('St.Louis(STL)-St.Louis(STL)', 0, 0)

# Calculating the total cost of travel per person
data['Total.Travel.Cost.per.person'] <- rowSums(data[,2:3], na.rm = TRUE)

# Splitting the source and destination into two columns
Source.Destination <- data.frame(do.call('rbind', strsplit(data$Source.Destination, "[-?]")))
data <- cbind(Source.Destination, data[,2:4])
names(data) <- c("Source", "Destination", "Round.Trip.cost.per.person", 
                 "Cab.Fare", "Total.Travel.Cost.per.person")

# Converting the data into matrix format with rows as Source and columns as destination
traveldf <- xtabs(Total.Travel.Cost.per.person~Source+Destination, data)
traveldf[,1] <- traveldf[1,] 
traveldf[2:4,2] <- traveldf[2,2:4]
traveldf[3:4,3] <- traveldf[3,3:4]

# Storing the matrix above into dataframe
traveldf2 <- as.data.frame.matrix(traveldf)

# storing the above matrix in long format
traveldf3 <- as.data.frame(traveldf)

# Removing rows where source and destination are same
traveldf3 <- traveldf3[traveldf3$Freq >0,]
names(traveldf3) <- c(names(traveldf3)[1:2], "Travel.cost")
#traveldf3 <- traveldf3[!duplicated(data.frame(t(apply(traveldf3,1,sort)))),]

# Reading the 2nd dataset consisting of Meal and Lodging cost
data_2 <- read.csv("data_2.csv")
data_2['Total.cost.per.person'] <- rowSums(data_2[,3:4])  # calculating the total cost per person per day

# Adding the Meal & Lodging cost per person and number of persons visiting from source city 
traveldf3['Meal.and.Loging.cost.per.person'] = 0
traveldf3['Numberof.person.visiting'] = 0

for (i in 1:nrow(traveldf3)){
  if (traveldf3$Destination[i] == 'Atlanta(ATL)') {
    traveldf3$Meal.and.Loging.cost.per.person[i] = 135
  }else if (traveldf3$Destination[i] == 'Chicago(ORD)') {
    traveldf3$Meal.and.Loging.cost.per.person[i] = 162
  }else if (traveldf3$Destination[i] == 'Springfield(SPI)') {
    traveldf3$Meal.and.Loging.cost.per.person[i] = 85
  }else if (traveldf3$Destination[i] == 'St.Louis(STL)') {
    traveldf3$Meal.and.Loging.cost.per.person[i] = 117
  }
  
  if (traveldf3$Source[i] == 'Atlanta(ATL)') {
    traveldf3$Numberof.person.visiting[i] = 2
  }else if (traveldf3$Source[i] == 'Chicago(ORD)') {
    traveldf3$Numberof.person.visiting[i] = 3
  }else if (traveldf3$Source[i] == 'Springfield(SPI)') {
    traveldf3$Numberof.person.visiting[i] = 2
  }else if (traveldf3$Source[i] == 'St.Louis(STL)') {
    traveldf3$Numberof.person.visiting[i] = 4
  }
  
}

# Calculate the Total cost incured for people attending conference at the destination city when
# traveling from their respective source cities (for 2day, 3day and 4day conference)
traveldf3['Total.cost.for.1day.conf'] <- traveldf3$Travel.cost*traveldf3$Numberof.person.visiting + traveldf3$Meal.and.Loging.cost.per.person*traveldf3$Numberof.person.visiting
traveldf3['Total.cost.for.2day.conf'] <- traveldf3$Travel.cost*traveldf3$Numberof.person.visiting + traveldf3$Meal.and.Loging.cost.per.person*traveldf3$Numberof.person.visiting*2
traveldf3['Total.cost.for.3day.conf'] <- traveldf3$Travel.cost*traveldf3$Numberof.person.visiting + traveldf3$Meal.and.Loging.cost.per.person*traveldf3$Numberof.person.visiting*3
traveldf3['Total.cost.for.4day.conf'] <- traveldf3$Travel.cost*traveldf3$Numberof.person.visiting + traveldf3$Meal.and.Loging.cost.per.person*traveldf3$Numberof.person.visiting*4

# Applying the Dijkstra and Johnson Algorithm for Shortest path for a 2day conference
#tic and toc is used to calculate the time taken by the algorithm.
# ________________________________________________________________
tic("Sleeping")
traveldf4 <- xtabs(Total.cost.for.2day.conf~Source+Destination, traveldf3)
g <- graph.adjacency(as.matrix(traveldf4), weighted = TRUE)
plot(g)
shortest.paths(g, algorithm = "dijkstra")

shortest.paths(g, algorithm = "johnson")
toc()
# 'Johnson' considers the direction of the path and gives the short weighted value of the path
# while 'Dijkstra' does not consider the direction and just gives the least weighted values in 
# a path.

# Applying the Floyd-Warshall Algorithm on 2 day conference.
# _________________________________________________________

floyd_path <- floyd(traveldf4)
rownames(floyd_path) <- rownames(traveldf4)
colnames(floyd_path) <- colnames(traveldf4)
floyd_path

# Applying 'Kruskal' and 'Prim' Min Spanning Tree methods for 2 day conference
# ________________________________________________________
# Changing the levels
traveldf5 <- traveldf3[,c(1,2,6)]
levels(traveldf5$Source) <- 1:length(levels(traveldf5$Source))
traveldf5$Source <- as.numeric(traveldf5$Source)
levels(traveldf5$Destination) <- 1:length(levels(traveldf5$Destination))
traveldf5$Destination <- as.numeric(traveldf5$Destination)

# for a 2 day conference
getMinimumSpanningTree(1:4, as.matrix(traveldf5), algorithm = 'Kruskal')
getMinimumSpanningTree(1:4, as.matrix(traveldf5), algorithm = 'Prim')

# Kruskal and prims are minimum spanning tree algorithm they will not give the correct output.

# Sensitivity Analysis
# _____________________

# As all nodes are interconnected (i.e. we can fly from every location to other) it is hard to
# make sense of values obtained, therefore, we take the travel cost (between cities) and break
# one link for example here I am breaking the link between Atlanta to Chicago

# Original Paths
traveldf6 <- xtabs((Travel.cost*Numberof.person.visiting)~Source+Destination, traveldf3)
g <- graph.adjacency(as.matrix(traveldf6), weighted = TRUE)
plot(g)
traveldf6
shortest.paths(g, algorithm = "dijkstra")
shortest.paths(g, algorithm = "johnson")

# New Paths (assuming we cannot travel from atlanta to Chicago)
traveldf6 <- xtabs((Travel.cost*Numberof.person.visiting)~Source+Destination, traveldf3)
traveldf6[1,2] <- 0
#traveldf6[2,1] <- 0
traveldf6
g <- graph.adjacency(as.matrix(traveldf6), weighted = TRUE)
plot(g)
shortest.paths(g, algorithm = "dijkstra")
shortest.paths(g, algorithm = "johnson")
