#########################
####    Functions    ####
#########################

# Include main functions used in the analysis


#----    sample_population    ----

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



#------


#----    relative_bias    ----

relative_bias <- function(estimates_vector, true_value, FUN){

  estimate = FUN(estimates_vector)
  bias = (estimate-true_value)/true_value

  return(bias)
}


#----    mean_squared_error    ----

mean_squared_error <- function(estimates_vector, true_value){

  sigma = sd(estimates_vector)
  average = mean(estimates_vector)
  MSE = sigma^2 + (average - true_value)^2

  return(MSE)
}

#----    inclusion_interval    ----

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

inclusion_interval_vectorized <- Vectorize(inclusion_interval, vectorize.args = c("lower_bound","upper_bound"))

#----    coverage    ----

coverage <- function(lb_vector, ub_vector, true_value){

  included = inclusion_interval_vectorized(lower_bound = lb_vector,
                                           upper_bound = ub_vector,
                                           evaluate_value = true_value,
                                           type = "inclusion")

  return(mean(included))
}

#----    power    ----

power <- function(lb_vector, ub_vector){

  included = inclusion_interval_vectorized(lower_bound = lb_vector,
                                           upper_bound = ub_vector,
                                           evaluate_value = 0,
                                           type = "exclusion")

  return(mean(included))
}



#----    summary_ML    ----

summary_ML <- function(fit_ML){
  parameterEstimates(fit_ML)%>%
    mutate(parameter=paste0(lhs,op,rhs),
           method="ML")%>%
    filter(parameter %in% c("METACOGN~NEUROT","SLEEP~METACOGN","SLEEP~NEUROT"))%>%
    select(method,parameter, est, ci.lower, ci.upper)
}

#----    summary_Bayes    ----

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

summary_4_fits <- function(fit_ML, fit_Bayes_default, fit_Bayes_infI, fit_Bayes_infII){
  rbind(summary_ML(fit_ML),
        summary_Bayes(fit_Bayes_default,"Bayes_default"),
        summary_Bayes(fit_Bayes_infI,"Bayes_infI"),
        summary_Bayes(fit_Bayes_infII,"Bayes_infII"))
}

####-----

#----    simulation_one    ----

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


#-----    apply_simulation    ----


apply_simultaion <- function(plan_simulation, n_cores, gc=TRUE){

  future::plan(multiprocess, workers = n_cores, gc=gc)
  res = future_apply(plan_simulation, MARGIN = 1,
              FUN = function(x) simulation_one(x, model_ML, model_Bayes_default, model_Bayes_infI, model_Bayes_infII))

  res = do.call("rbind", res)

  return(res)
}

#------    diagram_model    ----

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

#------    plot_prior    ----

plot_prior <- function(){

  seq_grid = seq(from=-2, to=2, length.out = 200)
  df_plot = data.frame(Prior = factor(rep(c("Default","Reasonable", "Experts"), each = 3*200),levels = c("Default","Reasonable", "Experts")),
                       parameter = rep(c("$\\beta_1$","$\\beta_2$","$\\beta_3$"), each=200),
                       x = rep(seq_grid, 9),
                       density = c(rep(dnorm(seq_grid, mean = 0, sd = 10),3),
                                 dnorm(seq_grid, mean = .2, sd = .5),
                                 rep(dnorm(seq_grid, mean = .2, sd = .5),2),
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


