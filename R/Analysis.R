#######################
####   Analysis    ####
#######################

#----    Load    -----

source("R/Settings.R")            # Load packages
source("R/Functions.R")           # Load functions
source("R/Auxiliary_functions.R") # Load auxiliary functions
# source("R/Plan.R")                # Load analysis plan


#----    Models   ----

# Define the four models

# 1) Model for Maximum Likelihood (include indirect effect)
model_ML<-'
  METACOGN ~ a*NEUROT
  SLEEP ~ b*METACOGN + NEUROT
  indirect := a*b
'

# 2) DEFAULT BAYESIAN WITH BLAVAAN
model_Bayes_default<-'
  METACOGN ~ NEUROT
  SLEEP ~ METACOGN + NEUROT
'

# 3) PLAUSIBLE/DIRECTIONAL BAYESIAN WITH BLAVAAN

model_Bayes_infI<-'
  METACOGN ~ prior("normal(.20,.50)")*NEUROT
  SLEEP ~ prior("normal(-.20,.50)")*METACOGN + prior("normal(-.20,.50)")*NEUROT
'

# 4) EXPERT-INFORMED BAYESIAN WITH BLAVAAN

model_Bayes_infII<-'
  METACOGN ~ prior("normal(.20,.20)")*NEUROT
  SLEEP ~ prior("normal(-.40,.20)")*METACOGN + prior("normal(-.15,.20)")*NEUROT
'

#----    Analysis    ----

# define the number of iteration fo each condition as 1:<number of iteration>
plan_simulation = expand.grid(iter=1:5,n=c(15,20))

tictoc::tic()
profvis::profvis({
  res = apply_simultaion(plan_simulation, n_cores = 6, gc = TRUE)
  })
tictoc::toc()

#-----  summary res    ----

res%>%
  filter(parameter=="METACOGN~NEUROT")%>%
  group_by(n_sample,method)%>%
  summarise(relative_mean_bias = relative_bias(estimates_vector = est, true_value = .20, FUN = mean),
            relative_median_bias = relative_bias(estimates_vector = est, true_value = .20, FUN = median),
            mean_squared_error = mean_squared_error(estimates_vector = est, true_value = .20),
            coverage = coverage(lb_vector = ci.lower, ub_vector = ci.upper, true_value = .20),
            power = power(lb_vector = ci.lower, ub_vector = ci.upper))

# #----    Check   -----
#
# # Configure the analysis plan
# config <- drake::drake_config(plan)
#
# # Plot the analysis plan
# vis_drake_graph(config, font_size = 16, targets_only = T)
#
# #----    Make    ----
#
# # Delate the analysis cache
# # drake::clean(destroy = TRUE)
#
# # Run the analysis
# make(plan)
#
# # Plot the analysis plan
# vis_drake_graph(config, font_size = 16, targets_only = T)
#
# #----    Load elements    ----
# # With drake you load the results with the function:
# # - loadd() to load the object in the environment
# # - readd() print the object
