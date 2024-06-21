library(tidyverse)

d <- 2L
fid <- 1L
iid <- 1L

fn <- smoof::makeBiObjBBOBFunction(d, fid, iid)
fn_data <- moleopt:::generateBiObjBBOBData(d, fid, iid)

ideal <- fn_data$ideal_point
nadir <- fn_data$ref_point


fn <- smoof::makeDTLZ1Function(2, 2)
ideal <- c(0,0)
nadir <- c(11,11)


n <- 1e3
cutoffs <- 1:n

mean_r2s <- lapply(1:10, function(i) {
  X <- sapply(1:n, function(x) {
    moleopt::runif_box(smoof::getLowerBoxConstraints(fn),
                       smoof::getUpperBoxConstraints(fn))
  })
  
  y <- apply(X, 2, fn)
  
  r2s <- sapply(cutoffs, function(c) {
    r2 <- continuous_r2(y[,1:c,drop = F], ideal)
    nd <- sum(ecr::nondominated(y[,1:c,drop = F]))
    
    print(paste(r2, nd))
    
    r2
    
    # ecr::computeHV(y[,1:c,drop = F], nadir)
  })
  
  r2s
}) %>% Reduce(rbind, .) %>% colMeans

cbind.data.frame(budget = cutoffs, r2 = (mean_r2s - min(mean_r2s))) %>% 
  ggplot(aes(budget, r2)) +
  geom_step() +
  scale_x_log10() +
  scale_y_log10()

