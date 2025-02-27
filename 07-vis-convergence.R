library(tidyverse)
library(reticulate)

# necessary if we use smoof and r2_incremental simultaneously
Sys.setenv(KMP_DUPLICATE_LIB_OK = "True")

# reticulate::py_install("sortedcontainers")
inc <- reticulate::import_from_path("r2_incremental")

theme_set(theme_bw())

fn = function(x) {
  c1 = c(0.5, 0)
  c2 = c(-0.5, 0)
  
  c(sum((x - c1)**2), sum((x - c2)**2))
}

ideal <- c(0, 0)
nadir <- c(1, 1)

# feval_list <- lapply(1:1e5, function(i) {
#     x <- runif(1)
#     c(x, 1 - x)
# })

set.seed(0xC0FFEE)

ids <- unique(as.integer(10**(seq(0, 5, length.out = 2000))))

dfs <- lapply(1:100, function(i) {
    print(i)
    feval_list <- lapply(1:1e5, function(j) {
        x <- c(runif(1, -5, 5), runif(1, -5, 5)) / 5
        fn(x)
    })

    result <- inc$compute_indicators_archive(feval_list, ideal = ideal, ref = nadir)
    r2_history <- unlist(result[[1]])
    hv_history <- unlist(result[[2]])

    sum(diff(r2_history) != 0)
    sum(diff(hv_history) != 0)

    df <- cbind(
        feval = 1:length(r2_history),
        R2 = r2_history,
        HV = hv_history
    ) %>% as.data.frame() %>%
        filter(feval %in% ids)
    df$run <- i

    df
})

hv_ideal = 5/6
r2_ideal = (3 * pi - 8) / 16

df <- Reduce(rbind, dfs) %>%
    group_by(feval) %>%
    summarize(R2 = mean(R2), HV = mean(HV)) %>%
    ungroup()

df %>%
    mutate(R2 = R2 - r2_ideal, HV = hv_ideal - HV) %>%
    pivot_longer(
        cols = c("R2", "HV"),
        names_to = "indicator", values_to = "value"
    ) %>%
    ggplot(aes(feval, value)) +
    facet_wrap(vars(indicator), scales = "free") +
    scale_x_log10() +
    scale_y_log10() +
    labs(x = "Function Evaluations", y = "Indicator Gap") +
    geom_step() + 
    theme(axis.text.x = element_text(hjust=0.6))

ggsave("iterative-indicator-2.pdf", width = 4, height = 2.5)
