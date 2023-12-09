library(moPLOT)
library(tidyverse)

fn <- makeAsparFunction()

design <- generateDesign(fn, 500**2)

gradients <- computeGradientFieldGrid(design)
divergence <- computeDivergenceGrid(gradients$multi.objective, design$dims, design$step.sizes)
less <- localEfficientSetSkeleton(design, gradients, divergence, integration = "fast")

ggplotPLOT(design$dec.space, design$obj.space, less$sinks, less$height) +
  coord_fixed()


for (x in seq(0, 1, 0.1)) {
  # x <- 0.1
  obj.weights <- c(x, 1 - x)
  
  mod.obj.space <- t(t(design$obj.space) * obj.weights)
  height <- apply(mod.obj.space, 1, max)
  
  ggplotHeatmap(cbind.data.frame(design$dec.space, height = height)) +
    coord_fixed()
  
  ggsave(paste0("aspar-", x, "-dec.png"), width = 6, height = 6)
  
  ggplotObjectiveSpace(cbind.data.frame(design$obj.space, height = height)) +
    geom_point(data = as.data.frame(design$obj.space[which.min(height),,drop = F]), mapping = aes(y1, y2)) +
    coord_fixed()
  
  ggsave(paste0("aspar-", x, "-obj.png"), width = 6, height = 6)
}
