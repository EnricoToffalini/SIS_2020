###################################
####    Auxiliary functions    ####
###################################

# Include auxiliary functions used in the analysis


#----    inclusion_interval    ----

# Evaluates if <evaluate_value> is included or not in the interval defined by <lower_bound>
# and <upper_bound>. Option <type> define if we evaluate inclusion or exclusion.
# The function returns a logical values TRUE FALSE

inclusion_interval <- function(lower_bound, upper_bound, evaluate_value, type=c("inclusion", "exclusion")){

  answer = (evaluate_value >= lower_bound) & (evaluate_value <= upper_bound)

  if (type=="inclusion"){
    return(answer)
  } else if (type=="exclusion"){
    return(!answer)
  } else {
    stop("type has to be 'inclusion' or 'exclusion'")
  }
}

#----    inclusion_interval_vectorized    ----

# Vectorized version of the function inclusion_interval()

inclusion_interval_vectorized <- Vectorize(inclusion_interval, vectorize.args = c("lower_bound","upper_bound"))

#----    make_labels    ----

make_labels <- function(original_names, new_names){
  labels = new_names

  names(labels) = original_names

  return(labels)
}


#-----

