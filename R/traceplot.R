#' Traceplots of Nimble Output
#'
#' @param x A \code{nimbleUI} object
#' @param parameters A vector of names (as characters) of parameters to plot.
#'  Parameter names must match parameters included in the model. Calling
#'  non-scalar parameters without subsetting (e.g. \code{alpha}) will plot all
#'  values of \code{alpha}. If \code{parameters=NULL}, all parameters will be
#'  plotted.
#' @param Rhat_min If provided, only plot parameters with Rhat values that
#'  exceed the provided value. A good min value to start with is 1.05.
#' @param per_plot Maximum number of parameters to include on each plot.
#' @param ask If \code{TRUE}, ask user for confirmation before generating
#'  each new plot; the default is to ask when output is going to the screen,
#'  not when it is going to a file.
#'
#' @return Nothing; called for its side effects.
#' @importFrom grDevices dev.interactive rainbow
#' @importFrom graphics par matplot legend mtext
#' @importFrom coda gelman.diag
#' @export
traceplot <- function(x, parameters=NULL, Rhat_min=NULL,
                      per_plot=9, ask=NULL){

  if(is.null(ask))
    ask <- grDevices::dev.interactive(orNone = TRUE)
  plot_info <- get_plot_info(x, parameters, per_plot, ask, Rhat_min)

  #Handle par()
  old_par <- graphics::par(plot_info$new_par)
  on.exit(graphics::par(old_par))

  #Generate plot
  n <- length(plot_info$params)
  for (i in 1:n){
    m_labels <- (i %% plot_info$per_plot == 0) || (i==n)
    param_trace(x, plot_info$params[i], m_labels=m_labels)
  }

}

#Traceplot for single parameter
param_trace <- function(x, parameter, m_labels=FALSE){

  #Get samples and Rhat values
  vals <- mcmc_to_mat(x$samples, parameter)

  Rhat <- sapply(1:ncol(x$samples[[1]]), function(i){
    coda::gelman.diag(x$samples[,i], autoburnin=FALSE)$psrf[1,1]
  })
  Rhat <- sprintf("%.3f",round(Rhat,3))

  #Draw plot
  cols <- grDevices::rainbow(ncol(vals))
  graphics::matplot(1:nrow(vals), vals, type='l', lty=1, col=cols,
                 xlab='Iterations', ylab='Value',
                 main=paste('Trace of',parameter))

  #Add Rhat value
  graphics::legend('bottomright', legend=bquote(hat(R) == .(Rhat)),
                   bty='o', bg='white', cex=1.2)

  #Add margin labels if necessary
  if(m_labels){
    graphics::mtext("Iteration", side=1, line=1.5, outer=TRUE)
    graphics::mtext("Value", side=2, line=1.5, outer=TRUE)
  }
}


#General function for setting up plots
get_plot_info <- function(x, parameters, per_plot, ask, Rhat_min=NULL){

  #Expand non-scalar parameters and check they exist
  all_params <- param_names(x$samples)
  if(!is.null(parameters)){
    #Expand bracketed parameter names
    parameters <- expand_params(parameters)
    #Check parameters are in output
    parameters <- match_params(parameters, all_params)
    if(is.null(parameters)){
      stop("None of the provided parameters were found in the output")
    }
  } else{
    parameters <- all_params
  }

  #If rhat_min, check parameters against it
  if(!is.null(Rhat_min)){
    Rhats <- x$summary[parameters, 'Rhat']
    parameters <- parameters[Rhats >= Rhat_min]
    if(length(parameters)==0) stop("No parameters > Rhat_min")
  }

  #Reduce max panels per plot if larger than number of parameters
  if(length(parameters) <= per_plot){
    per_plot <- length(parameters)
    ask=FALSE
  }

  #Set up new par settings
  new_par <- list(mar=c(1.5,1.5,2.5,1), oma=c(3,3,0,0), ask=ask)
  if(per_plot > 1)
    new_par$mfrow <- c(ceiling(sqrt(per_plot)), round(sqrt(per_plot)))

  list(params=parameters, new_par=new_par, per_plot=per_plot)
}

