dots <- getDots(n_motifs = 4, t = 200, evalue_thresh = 40)
dict <- c("n_motifs" = "m",
          "evalue_thresh" = "e")
dotsToArgs(dots, dict)

system2("cut", c("-f 1", "-d\t", " <(echo hello\tworld)"))

system2("echo", "hello world", input = "test.txt") -> x

myFun <- function(file, ...){
  args <- getDots(...) %>%
    dotsToArgs

  system2("wc", c(args, file))
}

system2("echo", "hello", stdout = "myFile.txt")

myFun("myFile.txt", l = T)


getDots(m = "test") -> r

dotsToArgs(r)

getDots(d = "tess") -> d

dotsToArgs(d)

## ISSUE WITH GLOBAL ENVIRONMENT: WHY???
