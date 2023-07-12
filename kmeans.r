gen_rand_graph <- function (numOfPoints = 5000, srange = 1, nrange = 10000) {
  data_points <- sample(srange:nrange, numOfPoints*2, replace = TRUE)

  data <- matrix(data_points, ncol = 2)

  return (data)
}


k_means <- function (data, k, plotbool = FALSE, max_iters = 100) {

  dist <- function (x1, x2, y1, y2) {
    return (sqrt((x1-x2)^2+(y1-y2)^2))
  }

  plot_kmeans <- function (centroids, clusters, data, k) {
    plot(data, col = clusters, pch = 16, main = "K-means Clustering", xlab = "X-Axis", ylab = "Y-Axis")
    points(centroids, col = 2:k+1, pch = 17, cex = 2)
  }

  arr_dim <- dim(data)
  
  rows <- sample(1:arr_dim[1], k)
  
  initial_centers <- data[rows, ]
  
  suppressWarnings({
    for (i in 1:max_iters) {
      dists <- matrix(0, nrow = nrow(data), ncol = k)
  
      for (j in 1:nrow(data)) {
        x <- data[j, ][1]
        y <- data[j, ][2]
  
        for (n in 1:nrow(initial_centers)) {
          x1 <- initial_centers[n, ][1]
          y1 <- initial_centers[n, ][2]
  
          dis <- dist(x, x1, y, y1)
  
          dists[j, n] <- dis
        }
        
      }
  
      cluster_assignments <- apply(dists, 1, which.min)
  
      updated_centers <- matrix(0, nrow = k, ncol = ncol(data))
      for (i in 1:k) {
        updated_centers[i, ] <- colMeans(data[cluster_assignments == i, ])
      }
  
      if (all(rowSums((updated_centers - initial_centers)^2)) < 1e-6) {
        break
      }
  
      initial_centers <- updated_centers
    }
  })

  if (plotbool) {
    plot_kmeans(initial_centers, cluster_assignments, data, k)
  }
  
  result <- list(centroids = initial_centers, clusters = cluster_assignments)
  
  return (result)
}

#library(stats)

#k <- 6
#result <- kmeans(data, centers = k)

#plot(data, col = result$cluster, pch = 16, main="K-means Clustering", xlab="X-Axis", ylab="Y-Axis")
#points(result$centers, col = 2:k+1, pch = 17, cex = 2)