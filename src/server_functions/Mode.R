Mode <- function(x, threshold = 0.5) {
  x <- x[!(is.na(x) | x == "NA")]

  if (length(x) == 0) return(NA_character_)

  ux <- unique(x)
  tabulated <- tabulate(match(x, ux))
  max_count <- max(tabulated)
  n <- length(x)

  # Strict majority: most common value must represent more than half
  if (max_count / n > threshold) {
    qualifying <- ux[tabulated / n > threshold]
    qualifying <- qualifying[qualifying != "do_not_know"]
    if (length(qualifying) == 0) return("do_not_know")
    return(qualifying[1])
  }

  return("NC")
}
