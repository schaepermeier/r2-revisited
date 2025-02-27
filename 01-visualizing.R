library(moPLOT)
library(tidyverse)

if (!reticulate::virtualenv_exists("moPLOT")) {
  reticulate::virtualenv_create("moPLOT")
}

reticulate::use_virtualenv("moPLOT")

if (!reticulate::py_numpy_available()) {
  reticulate::py_install("numpy")
}

# fn <- makeAsparFunction()
fn <- makeBiObjMPM2Function()

design <- generateDesign(fn, 300**2)

gradients <- computeGradientFieldGrid(design)
divergence <- computeDivergenceGrid(gradients$multi.objective, design$dims, design$step.sizes)
less <- localEfficientSetSkeleton(design, gradients, divergence, integration = "fast")

ggplotPLOT(design$dec.space, design$obj.space, less$sinks, less$height) +
  coord_fixed()


plot_ly(z = matrix(height, nrow = design$dims[1], ncol = design$dims[2])) %>% add_surface()

for (x in seq(0, 1, 0.01)) {
  # x <- 0.1
  obj.weights <- c(x, 1 - x)
  
  mod.obj.space <- t(t(design$obj.space) * obj.weights)
  height <- apply(mod.obj.space, 1, max)
  
  idx <- which.min(height)
  
  ggplotHeatmap(cbind.data.frame(design$dec.space, height = height)) +
    scale_fill_viridis_c() +
    geom_point(aes(x = design$dec.space[idx,1],
                   y = design$dec.space[idx,2]),
               size = 10,
               color = "black",
               shape = "+") +
    theme(legend.position = "none") +
    coord_fixed()
  
  ggsave(paste0("mpm2-dec-", 100 * x, ".png"), width = 6, height = 6)
  
  # ggplotObjectiveSpace(cbind.data.frame(design$obj.space, height = height)) +
  #   geom_point(data = as.data.frame(design$obj.space[which.min(height),,drop = F]), mapping = aes(y1, y2)) +
  #   scale_color_viridis_c() +
  #   theme(legend.position = "none") +
  #   coord_fixed()
  # 
  # ggsave(paste0("mpm2-obj-", x, ".png"), width = 6, height = 6)
}
