library(tidyverse)

theme_set(theme_bw())

fn = function(x) {
  c1 = c(0.5, 0)
  c2 = c(-0.5, 0)
  
  c(sum((x - c1)**2), sum((x - c2)**2))
}

ideal <- c(0, 0)
nadir <- c(1, 1)

n <- 1e5

X <- rbind(seq(-0.5, 0.5, by = 1e-3), 0)

y <- apply(X, 2, fn)

# y_norm <- rbind(
#   (y[1,] - ideal[1]) / (nadir[1] - ideal[1]),
#   (y[2,] - ideal[2]) / (nadir[2] - ideal[2])
# )

y_df <- as.data.frame(t(y))
colnames(y_df) <- c("y1", "y2")

y_df$nondom <- ecr::nondominated(y)

sum(y_df$nondom)

y_df %>% 
  ggplot(aes(y1, y2)) +
  geom_point(aes(color = nondom), alpha = ifelse(y_df$nondom, 1, 0.01)) +
  geom_point(x = ideal[1], y = ideal[2],
             shape = "+", color = "black", size = 5)

y_df %>% 
  filter(nondom == TRUE) %>% 
  ggplot(aes(y1, y2)) +
  geom_point(shape = "+") +
  geom_point(x = ideal[1], y = ideal[2],
             shape = "+", color = "black", size = 5) +
  geom_ribbon(aes(ymin = y2, ymax = max(filter(y_df, nondom == TRUE)$y2)), alpha = 0.2) +
  labs(x = expr(y[1]), y = expr(y[2]))

ggsave("weights-pf-illu.pdf", width = 3, height = 3)

Ns <- c(1, 2, 3, 5, 10, 20, 30, 50, 100,
        200, 300, 500, 1000,
        2000, 3000, 5000, 10000,
        20000, 30000, 50000, 100000,
        200000, 300000, 500000, 1000000)

r2s <- sapply(Ns, function(N) {
  weights <- rbind((0:N)/N, 1 - (0:N)/N)
  emoa::unary_r2_indicator(y[,y_df$nondom], weights, ideal)
})

# true_r2 <- continuous_r2((y - ideal) / (nadir - ideal), c(0, 0))
true_r2 <- continuous_r2(y + 1e-8, ideal)

cbind.data.frame(N = Ns, r2 = r2s) %>% 
  ggplot(aes(N, r2)) +
  geom_abline(slope = 0, intercept = true_r2, linetype = "dashed", alpha = 0.5) +
  geom_line(linetype = "dotted") +
  geom_point(shape = "+", size = 5) +
  scale_x_log10() +
  labs(x = "|W|", y = "R2 Indicator Value")

ggsave("weights-vs-R2.pdf", width = 3, height = 3)

cbind.data.frame(N = Ns, r2 = true_r2 - r2s) %>% 
  ggplot(aes(N, r2)) +
  geom_line() +
  geom_point(shape = "+", size = 5) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "|W|", y = "Discrete R2 Approximation Error")

ggsave("weights-vs-R2-error.pdf", width = 3, height = 3)


