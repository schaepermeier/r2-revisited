source("03-continuous-r2.R")

eps <- 1e-6
len <- 1e4

y1 <- seq(eps,1 - eps, length.out = len)
# y <- rbind(y1, (1 - sqrt(y1))**2) # convex
# y <- rbind(y1, sqrt(1 - y1**2)) # concave
# y <- rbind(y1, (1 - y1)) # linear
# y <- rbind(y1, c(rep(1, len - 1), 0)) # nadir
y <- rbind(y1, c(1, rep(0, len - 1))) # ideal
ideal <- c(0,0)

ggplot(aes(x = y[1,], y = y[2,]), data = data.frame()) +
  geom_step() +
  geom_ribbon(aes(ymin = y[2,], ymax = 1), alpha = 0.2) +
  coord_fixed() +
  labs(x = expr(f[1]),
       y = expr(f[2])) +
  scale_x_continuous(breaks = c(0, 0.5, 1)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  geom_point(aes(x = 0, y = 0), shape = "+", size = 5)

# ggsave("convex-schematic.pdf", width = 2, height = 1.9)
# ggsave("concave-schematic.pdf", width = 2, height = 1.9)
# ggsave("linear-schematic.pdf", width = 2, height = 1.9)
# ggsave("nadir-schematic.pdf", width = 2, height = 1.9)
ggsave("ideal-schematic.pdf", width = 2, height = 1.9)

continuous_r2(y, ideal)

weights <- rbind((0:N)/N, 1 - (0:N)/N)
emoa::unary_r2_indicator(rbind(1, 1), weights, c(0,0))

ecr::computeHV(y, c(1,1))
