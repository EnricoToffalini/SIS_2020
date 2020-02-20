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

#----    Simulation    ----

# define the number of iteration fo each condition as 1:<number of iteration>
plan_simulation = expand.grid(iter=1:5,n=c(15,20))

tictoc::tic()
profvis::profvis({
  res = apply_simultaion(plan_simulation, n_cores = 6, gc = TRUE)
  })
tictoc::toc()

#----    Results    ----

# load results

res <- read.csv(file = "Data/res_1000.csv", header = T, sep = ",", stringsAsFactors = F)%>%
  mutate(n_sample = factor(n_sample),
         method = factor(method, levels = c("ML","Bayes_default","Bayes_infI","Bayes_infII")),
         parameter = as.factor(parameter))

parameter_values <- data.frame(parameter = c("METACOGN~NEUROT","SLEEP~METACOGN","SLEEP~NEUROT"),
                               true_value = c(.205, -.363, -.129), stringsAsFactors = F)

# Table with all the results
table_results = results_table(parameter_values, res, return_list = FALSE)
table_results

# Plot estimates distribution
distribution_estimates(res, parameter = "METACOGN~NEUROT", true_value = parameter_values[1,2])
distribution_estimates(res, parameter = "SLEEP~METACOGN", true_value = parameter_values[2,2])
distribution_estimates(res, parameter = "SLEEP~NEUROT", true_value = parameter_values[3,2])

# Boxplot estimates
plot_boxplots(res, parameter = "METACOGN~NEUROT", true_value=parameter_values[1,2])
plot_boxplots(res, parameter = "SLEEP~METACOGN", true_value = parameter_values[2,2])
plot_boxplots(res, parameter = "SLEEP~NEUROT", true_value = parameter_values[3,2])

plot_boxplots_all(res, method_labels = c("ML", "Bayesian default", "Reasonable priro","Experts prior"))

# Plot recovery
plot_recovery(table_results, criteria = "relative_mean_bias")
plot_recovery(table_results, criteria = "relative_median_bias")
plot_recovery(table_results, criteria = "mean_squared_error")
plot_recovery(table_results, criteria = "coverage")
plot_recovery(table_results, criteria = "power")



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
