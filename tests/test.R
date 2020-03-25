dots <- getDots(n_motifs = 4, t = 200, evalue_thresh = 40)
dict <- c("n_motifs" = "m",
          "evalue_thresh" = "e")
dotsToArgs(dots, dict)

getDots(x = T, y = F) %>%
  dotsToArgs()


