#Estimating min points threshold for DB & HDB
estimate_minPts <- function(X, minPts = NULL) {
  if (!is.null(minPts)) return(minPts)
  2 * (ncol(X)+1)
}

#Estimating epsilon for DB
estimate_eps <- function(X, minPts, prob = 0.9) {
  dists <- kNNdist(X, k = minPts)
  as.numeric(quantile(dists, probs = prob))
}

#Returns clustered graphs & ARI scores
run_methods <- function(
    data,
    truth,
    k,
    db_eps = NULL,
    db_minPts = NULL,
    hdb_minPts = NULL,
    eps_quantile = 0.9,
    scale_data=TRUE
) {
  X <- if (scale_data) scale(data) else data
  
  if (is.null(db_minPts)){
    db_minPts <- estimate_minPts(X, 2*ncol(X))
  }
  if (is.null(db_eps)) {
    db_eps <- estimate_eps(X, minPts = db_minPts, prob = eps_quantile)}
  
  hdb_minPts <- estimate_minPts(X, hdb_minPts)
  
  single <- agnes(X, method = "single")
  single_cut <- cutree(single, k)
  
  spectral <- specc(X, centers = k)
  
  db <- dbscan(X, eps = db_eps, minPts = db_minPts)
  hdb <- hdbscan(X, minPts = hdb_minPts)
  
  results <- data.frame(
    method = c("single", "spectral", "dbscan", "hdbscan"),
    ari = c(
      ARI(truth, single_cut),
      ARI(truth, spectral),
      ARI(truth, db$cluster),
      ARI(truth, hdb$cluster)
    )
  )
  
  scatterplot3d(X, color = single_cut, pch = 19)
  scatterplot3d(X, color = as.numeric(spectral), pch = 19)
  scatterplot3d(X, color = db$cluster + 1, pch = 19)
  scatterplot3d(X, color = hdb$cluster + 1, pch = 19)
  
  list(
    params = list(
      db_eps = db_eps,
      db_minPts = db_minPts,
      hdb_minPts = hdb_minPts,
      eps_quantile = eps_quantile
    ),
    clusters = list(
      single = single_cut,
      spectral = spectral,
      dbscan = db$cluster,
      hdbscan = hdb$cluster
    ),
    results = results
  )
}



