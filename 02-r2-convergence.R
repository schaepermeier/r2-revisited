library(tidyverse)

d <- 2L
fid <- 10L
iid <- 1L

fn <- smoof::makeBiObjBBOBFunction(d, fid, iid)
fn_data <- moleopt:::generateBiObjBBOBData(d, fid, iid)

ideal <- fn_data$ideal_point

n <- 1e4

X <- sapply(1:n, function(x) {
  moleopt::runif_box(smoof::getLowerBoxConstraints(fn),
                     smoof::getUpperBoxConstraints(fn))
})

y <- apply(X, 2, fn)

y_df <- as.data.frame(t(y))
colnames(y_df) <- c("y1", "y2")

y_df$nondom <- ecr::nondominated(y)

sum(y_df$nondom)

y_df %>% 
  ggplot(aes(y1, y2, color = nondom)) +
  geom_point(aes(alpha = ifelse(nondom, 1, 0.01))) +
  geom_point(aes(x = ideal[1], y = ideal[2]),
             shape = "+", color = "black", size = 5)

Ns <- c(1, 2, 3, 5, 10, 20, 30, 50, 100,
        200, 300, 500, 1000,
        2000, 3000, 5000, 10000,
        20000, 30000, 50000, 100000,
        200000, 300000, 500000, 1000000)

r2s <- sapply(Ns, function(N) {
  weights <- rbind((0:N)/N, 1 - (0:N)/N)
  emoa::unary_r2_indicator(y[,y_df$nondom], weights, ideal)
})

true_r2 <- continuous_r2(y, ideal)

cbind.data.frame(N = Ns, r2 = r2s) %>% 
  ggplot(aes(N, r2)) +
  geom_abline(slope = 0, intercept = true_r2, linetype = "dashed", alpha = 0.5) +
  geom_point() +
  scale_x_log10()

cbind.data.frame(N = Ns, r2 = true_r2 - r2s) %>% 
  ggplot(aes(N, r2)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()
