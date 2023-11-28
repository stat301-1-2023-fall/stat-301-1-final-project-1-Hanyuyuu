### Numeric variable plots

for (var in traffic |> select(is.numeric) |> colnames()) {
  label <- rlang::englue("A histogram of {{var}}")
  numeric_plot <- 
    ggplot(traffic, aes(x = !!sym(var))) +
    geom_histogram(bins = 20)
  ggsave(paste(var, "_distribution.png", sep = ""), 
         numeric_plot,
         path = "./plots/numeric")
}

### Factor variable plots

for (var in convert_to_factor) {
  label <- rlang::englue("A barchart of {{var}}")
  factor_plot <- 
    ggplot(traffic, aes(x = !!sym(var))) +
    geom_bar() +
    coord_flip() +
    labs(title = label)
  ggsave(paste(var, "_distribution.png", sep = ""),
         factor_plot,
         path = "./plots/factor")
}


## Datetime variable plots

for (interval in levels(traffic$delay_bins)){
  delay_plot <- traffic |> 
    filter(delay_bins == interval) |> 
    ggplot(aes(delay)) +
    geom_histogram()
  ggsave(paste(interval, "_distribution.png", sep = ""),
         delay_plot,
         path = "./plots")
}

#ggplot(traffic, aes(delay)) +
#  geom_histogram(bins = 250) +
#  coord_cartesian(ylim = c(0, 25))
