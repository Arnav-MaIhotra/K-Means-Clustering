source("kmeans.r")

k <- 6

data <- gen_rand_graph(10000, 1, 10000)

result <- k_means(data, k, TRUE)