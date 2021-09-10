#' Dijkstra's algorithm
#'
#' @param graph A data frame that contains the edges of the graph and the weight of the edge
#' @param init_node A numeric scalar that exists in the graph
#' @return The shortest distance from \code{init_node} to every other node
#' @examples
#' dijkstra(wiki_graph, 1)
#' dijkstra(wiki_graph, 3)
#' @references \url{https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm}
#' @export
dijkstra <- function(graph, init_node) {
  nodes <- unique(graph[,1])
  dist <- rep(Inf, length(nodes))
  unvisited <- rep(TRUE, length(nodes))
  res <- data.frame(nodes, dist, unvisited)

  res[init_node,2] <- 0

  while(sum(res[,3], na.rm = TRUE) > 0) {
    u <- res[which(res[,3] == TRUE),][which.min(res[which(res[,3] == TRUE),][,2]),1]
    res[u,3] <- FALSE
    neighbors <- graph[which(graph[,2] == u),1]

    for(neighbor in neighbors) {
      alt <- res[u,2] + graph[which(graph[,1] == u & graph[,2] == neighbor),3]
      if(alt < res[neighbor,2]) {
        res[neighbor,2] <- alt
      }
    }
  }
  return(res[,2])
}
