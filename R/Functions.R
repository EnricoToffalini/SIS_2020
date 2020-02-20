#########################
####    Functions    ####
#########################

# Include main functions used in the analysis

##########################
####    Simulation    ####
##########################

#----    sample_population    ----

# Sample <n_sample> observations of variables NEUROT, METACOGN, and SLEEP.
# Covariance structure is defined according to example model.
# The function returns a data.frame with columns NEUROT, METACOGN, and SLEEP.

sample_population <- function(n_sample=5e4){

  # CREATE SIMULATED POPULATION
  # (from real data on 122 people)
  Sigma<-lav_matrix_lower2full(c(
    + 1,
    + .205, 1,
    + -.204, -.390, 1)
  )

  pop<-data.frame(mvrnorm(n=n_sample,mu=rep(0,3),Sigma=Sigma))
  colnames(pop)<-c("NEUROT","METACOGN","SLEEP")

  return(pop)
}



#----    summary_ML    ----

# Get estimates information of the parameters METACOGN~NEUROT, SLEEP~METACOGN,
# and SLEEP~NEUROT from a Maximum Likelihood model.
# Return data.frame with method (estimation method "ML"), parameter (name of the parameter),
# est, ci.lower, and ci.upper.

summary_ML <- function(fit_ML){
  parameterEstimates(fit_ML)%>%
    mutate(parameter=paste0(lhs,op,rhs),
           method="ML")%>%
    filter(parameter %in% c("METACOGN~NEUROT","SLEEP~METACOGN","SLEEP~NEUROT"))%>%
    select(method,parameter, est, ci.lower, ci.upper)
}

#----    summary_Bayes    ----

# Get estimates information of the parameters METACOGN~NEUROT, SLEEP~METACOGN,
# and SLEEP~NEUROT from a Bayesian model.
# Return data.frame with method (estimation method <method_est>), parameter (name of the parameter),
# est, ci.lower, and ci.upper.

summary_Bayes <- function(fit_Bayes, method_est){
  hpd = data.frame(blavaan::blavInspect(fit_Bayes, what="hpd"))%>%
    filter(rownames(.) %in% c("METACOGN~NEUROT","SLEEP~METACOGN","SLEEP~NEUROT"))%>%
    rename("ci.lower" = "lower", "ci.upper" = "upper")
  parameterEstimates(fit_Bayes)%>%
    mutate(parameter=paste0(lhs,op,rhs),
           method=method_est)%>%
    filter(parameter %in% c("METACOGN~NEUROT","SLEEP~METACOGN","SLEEP~NEUROT"))%>%
    select(method,parameter, est)%>%
    cbind(hpd)
}

#----    summary_4_fits    ----

# Obtain summary for the 4 model conditions of each simulation replication.
# Return data.frame with method (estimation method), parameter (name of the parameter),
# est, ci.lower, and ci.upper.

summary_4_fits <- function(fit_ML, fit_Bayes_default, fit_Bayes_infI, fit_Bayes_infII){
  rbind(summary_ML(fit_ML),
        summary_Bayes(fit_Bayes_default,"Bayes_default"),
        summary_Bayes(fit_Bayes_infI,"Bayes_infI"),
        summary_Bayes(fit_Bayes_infII,"Bayes_infII"))
}

#----    simulation_one    ----

# Single simulation replication loop.
# Define the <plan_simulation> (iteration number and sample size) and the four model formulas.
# Return data.frame with iter (number iteration), n_sample (sample size) and summary of the models
# given by summary_4_fits() function.

simulation_one <- function(plan_simulation, model_ML=model_ML, model_Bayes_default=model_Bayes_default, model_Bayes_infI, model_Bayes_infII){
  iter = plan_simulation[1]
  n_sample = plan_simulation[2]

  d = sample_population(n_sample = n_sample)

  # fit models
  fit_ML = sem(model=model_ML,data=d)
  fit_Bayes_default = bsem(model=model_Bayes_default,data=d)
  fit_Bayes_infI = bsem(model=model_Bayes_infI,data=d)
  fit_Bayes_infII = bsem(model=model_Bayes_infII,data=d)

  res = data.frame(iter=iter, n_sample = n_sample,
        summary_4_fits(fit_ML, fit_Bayes_default, fit_Bayes_infI, fit_Bayes_infII))

  return(res)
}


#----    apply_simulation    ----

# Apply single simulation replication loop in parallel with future.apply.
# Define the <plan_simulation> (iteration number and sample size), <n_cores> (n core used),
# <gc> (if TRUE call garbage cleaning after each completition).

apply_simultaion <- function(plan_simulation, n_cores, gc=TRUE){

  future::plan(multiprocess, workers = n_cores, gc=gc)
  res = future_apply(plan_simulation, MARGIN = 1,
              FUN = function(x) simulation_one(x, model_ML, model_Bayes_default, model_Bayes_infI, model_Bayes_infII))

  res = do.call("rbind", res)

  return(res)
}




########################
####    criteria    ####
########################

#----    relative_bias    ----

# Get the relative bias

relative_bias <- function(estimates_vector, true_value, FUN){

  estimate = FUN(estimates_vector)
  bias = (estimate-true_value)/true_value

  return(bias)
}


#----    mean_squared_error    ----

# Get MSE

mean_squared_error <- function(estimates_vector, true_value){

  sigma = sd(estimates_vector)
  average = mean(estimates_vector)
  MSE = sigma^2 + (average - true_value)^2

  return(MSE)
}

#----    coverage    ----

# Get coverage

coverage <- function(lb_vector, ub_vector, true_value){

  included = inclusion_interval_vectorized(lower_bound = lb_vector,
                                           upper_bound = ub_vector,
                                           evaluate_value = true_value,
                                           type = "inclusion")

  return(mean(included))
}

#----    power    ----

# Get power

power <- function(lb_vector, ub_vector){

  included = inclusion_interval_vectorized(lower_bound = lb_vector,
                                           upper_bound = ub_vector,
                                           evaluate_value = 0,
                                           type = "exclusion")

  return(mean(included))
}




#######################
####    results    ####
#######################

#----    summarize_results   ----

summarize_results <- function(res, name_parameter, true_value) {
  name_parameter = enquo(name_parameter)

  res%>%
    filter(parameter == !!name_parameter)%>%
    group_by(n_sample, method, parameter)%>%
    summarize(relative_mean_bias = relative_bias(estimates_vector = est, true_value = true_value, FUN = mean),
              relative_median_bias = relative_bias(estimates_vector = est, true_value = true_value, FUN = median),
              mean_squared_error = mean_squared_error(estimates_vector = est, true_value = true_value),
              coverage = coverage(lb_vector = ci.lower, ub_vector = ci.upper, true_value = true_value),
              power = power(lb_vector = ci.lower, ub_vector = ci.upper))
}

#----    table_results    ----

results_table <- function(parameter_values, res, return_list=FALSE){

  parameter = parameter_values[,1]
  true_value =  parameter_values[,2]

  table = pmap(list(parameter, true_value),
             function(a, b) summarize_results(res, name_parameter = a, true_value = b))

  if(return_list==FALSE){
    table = do.call("rbind", table)
  }

  return(table)
}


#####################
####    Plots    ####
#####################

#----    diagram_model    ----

# Diagram of the mediation model used as example

diagram_model <- function(){
  DiagrammeR::grViz("digraph {

  graph[layout = dot, rankdir = LR]
  node [shape = box]

  a [label = 'Neuroticism']
  b [label = 'Metacognitive\\n thoughts']
  c [label = 'Sleep quality']

  a -> b [label = '&beta;@_{1} = .20' ]
  b -> c [label = '&beta;@_{2} = -.36' ]
  a -> c [label = '&beta;@_{3} = -.13' ]
  }")
}

#----    plot_prior    ----

# Plot of the different prior settings

plot_prior <- function(){

  seq_grid = seq(from=-2, to=2, length.out = 200)
  df_plot = data.frame(Prior = factor(rep(c("Default","Reasonable", "Experts"), each = 3*200),levels = c("Default","Reasonable", "Experts")),
                       parameter = rep(c("$\\beta_1$","$\\beta_2$","$\\beta_3$"), each=200),
                       x = rep(seq_grid, 9),
                       density = c(rep(dnorm(seq_grid, mean = 0, sd = 10),3),
                                 dnorm(seq_grid, mean = .2, sd = .5),
                                 rep(dnorm(seq_grid, mean = -.2, sd = .5),2),
                                 dnorm(seq_grid, mean = .2, sd = .2),
                                 dnorm(seq_grid, mean = -.4, sd = .2),
                                 dnorm(seq_grid, mean = -.2, sd = .2)))


  df_plot %>%
    ggplot() +
    geom_line(aes(x=x, y=density, color=Prior, linetype=Prior), size=.9)+
    facet_grid(.~parameter)+
    scale_linetype_manual(values=c(1,2,4))+
    theme(legend.position = c(.5,-0.23),
          legend.direction = "horizontal",
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          plot.margin = margin(0,0,20,0))+
    labs(x="Value", y="")

}





#----    plot_boxplots    ----

plot_boxplots <- function(res, parameter, true_value){
  name_parameter = parameter
  parameter = enquo(parameter)

  res %>%
    filter(parameter == !! parameter) %>%
    ggplot()+
    geom_boxplot(aes(x=n_sample, y=est, fill=method), alpha= .7)+
    theme(legend.position = "top")+
    geom_hline(yintercept=true_value, linetype="dashed")+
    labs(x="Sample size", y="Value")

}


#----    distribution_estimates    ----

distribution_estimates <- function(res, parameter, true_value){

  name_parameter = parameter
  parameter = enquo(parameter)

  res %>%
    filter(parameter == !! parameter) %>%
    ggplot()+
    geom_density(aes(x=est, fill=method), alpha=.3)+
    geom_vline(xintercept = true_value, linetype="dashed")+
    facet_grid(.~ n_sample)+
    labs(y = "", x = name_parameter)+
    theme(legend.position = "top",
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank())

}

#----    plot_recovery    ----

plot_recovery <- function(results_table, criteria){

  n_col = match(criteria,names(results_table))

  results_table %>%
    ggplot(aes(x=as.numeric(as.character(n_sample)), y=results_table[[n_col]], color = method))+
    geom_line()+
    geom_point()+
    labs(x="Sample size", y = criteria)+
    scale_x_continuous(breaks = c(30,50,100,500), trans = "log")+
    facet_grid(.~ parameter)+
    theme(legend.position = "top")

}



#------
