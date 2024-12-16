pc_strength <- function(real, predicted) {
  structure(
    list(
      real = real,
      predicted = predicted
    ),
    class = "pc_strength"
  )
} 