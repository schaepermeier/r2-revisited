library(tidyverse)

theme_set(theme_bw())

d <- 2L
fid <- 2L
iid <- 1L

fn <- smoof::makeBiObjBBOBFunction(d, fid, iid)
fn_data <- moleopt:::generateBiObjBBOBData(d, fid, iid)

ideal <- fn_data$ideal_point

n <- 1e3

X <- sapply(1:n, function(x) {
  moleopt::runif_box(smoof::getLowerBoxConstraints(fn),
                     smoof::getUpperBoxConstraints(fn))
})

y <- apply(X, 2, fn)


compute_ideal_weights <- function(y) {
  ideal_prop <- y[2] / y[1]
  w1 <- ideal_prop / (1 + ideal_prop)
  
  c(w1, 1 - w1)
}

continuous_r2 <- function(y, ideal) {
  nondom <- ecr::nondominated(y)
  y_nondom <- y[,nondom,drop = F]
  ord <- order(y_nondom[1,])
  y_ordered <- y_nondom[,ord]
  
  n_pf <- sum(nondom)
  
  y_transformed <- t(y_ordered - ideal)
  
  # ideal_props <- y_transformed[,1] / y_transformed[,2]
  # ideal_w1 <- 1 - ideal_props / (1 + ideal_props)
  
  # compute_ideal_weights(y_transformed[1,])

  lapply(1:n_pf, function(idx) {
    y1 <- y_transformed[idx,1]
    y2 <- y_transformed[idx,2]
    
    if (idx == 1) {
      lower_ws <- c(1,0)
    } else {
      lower_ws <- compute_ideal_weights(pmax(y_transformed[idx - 1,],
                                             y_transformed[idx,]))
    }
    
    mid_ws <- compute_ideal_weights(y_transformed[idx,])
    
    if (idx == n_pf) {
      upper_ws <- c(0,1)
    } else {
      upper_ws <- compute_ideal_weights(pmax(y_transformed[idx,],
                                             y_transformed[idx + 1,]))
    }
    
    # lower ws - mid ws: y1 is critical in max(w1 * y1, w2 * y2)
    # mid ws - upper ws: y2 is critical in max(w1 * y1, w2 * y2)
    
    (lower_ws[1]**2 - mid_ws[1]**2) / 2 * y1 + (upper_ws[2]**2 - mid_ws[2]**2) / 2 * y2
  }) %>% unlist %>% sum
}

