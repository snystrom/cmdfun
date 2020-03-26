library(magrittr)
dots <- getDots(n_motifs = 4, t = 200, evalue_thresh = 40)
dict <- c("n_motifs" = "m",
          "evalue_thresh" = "e")
dotsToArgs(dots, dict)

getDots(x = T, y = F) %>%
  dotsToArgs()




dots <- getDots(n_motifs = 4, e = c(4,3,4,5), b = T, c = F)
dotsToArgs(dots)

myAlist <- paste0(names(dict), "=", names(dict), collapse = ", ")

theArgs <- names(dict)
names(theArgs) <- names(dict)
myFun <- as.function(alist(deparse(myAlist)))


#x <- as.function(alist(a = 2, b = b, a + b))

#
fun_gen <- function(argsDict, ...){
  #myFun <- as.function(alist(...))
  myFun <- function(eval(...)){
    
  }
  return(myFun)
}


myFun <- fun_gen(dict, n_motifs = 2, evalue_thresh = 40)
myFun()


cutDict <- c("sep" = "d", "field" = "f")

####

shellCut_alias <- function(text, ...){

  argsDict <- c("sep" = "d")
    
	args <- getDots(...) %>%
		dotsToArgs(argsDict)

	system2("echo", c(text, "|", "cut", args))
}

shellCut_alias("hello_world", f = 2, sep = "_") 

### MACROS

make_param_names <- function(params){
  param_names <- names(params)
  if (is.null(param_names))
      param_names <- rep("", length(params))
  
  for (i in seq_along(param_names)){
    if (param_names[i] == "") {
      param_names[i] <- paste(params[[i]])
    }
  }
  return(param_names)
}

make_macro <- function(..., body){
  params <- eval(substitute(alist(...)))
  body <- substitute(body)
  
  # Construct macro
  f <- eval(substitute(
    function() eval(substitute(body), parent.frame())
  ))
  
  # Set macro arugments
  #param_names <- make_param_names(params)
  param_names <- params
  names(params) <- param_names
  params <- as.list(params)
  formals(f) <- params
  
  return(f)
}

make_macro(body(a + b)) -> x

set_na_val <- make_macro(df, var, na_val,
                         body = {
                           df$var[df$var == na_val] <- NA
                           return(df)
                           }
                         )

# would have to create environment to not edit d in place...
d <- data.frame(x = c(1,-9,3,4), y = c(1,2,-9,-9))

set_na_val(d, "x", -9)
d

######
  

dotsToArgs(list(l = 2, l = 2))
