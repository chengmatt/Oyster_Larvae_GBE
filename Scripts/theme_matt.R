# Custom theme!

theme_matt <- function() {
  theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          strip.text = element_text(size = 17, hjust = 0.5, color = "White"),
          strip.background = element_rect(fill = "black", color = NA),
          axis.text = element_text(size = 15, color = "black"),
          axis.title = element_text(size = 19),
          legend.position = "top",
          legend.title = element_text(size = 17),
          legend.text = element_text(size = 14))
}