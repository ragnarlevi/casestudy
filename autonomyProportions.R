

#------------------ Autonomy Change functions ---------------------------

# Returns proportions of autonomy for each year and qtr as a data frame
autonomyRate <- function(time.frame, func, param, init, delay = NULL){
  # Load deSolve package
  # time frame, 
  # func - list of functions
  # param - list of parameters corresponding to the functions
  # init - initial starting proportions
  # delay, vector of when the new model take over, if only one model set D
  
  
  time.frame <- seq(min(time.frame), max(time.frame), by = 0.25)
  # number of parameter changes/models
  number.of.models <- length(param)
  out <- list() # output stored in a list
  
  if(is.null(delay)){
    delay <- c(min(time.frame),max(time.frame))
  }
  
  
  
  
  for(i in 1:number.of.models){
    # get right time interval
    t <- time.frame[ delay[i] <= time.frame & time.frame <= delay[i+1]]
    # calculate
    out[[i]] <- ode(y = init, times = t, func = func[[i]], parms = param[[i]])
    # data frames are nice to work with
    out[[i]] <- as.data.frame(out[[i]])
    # next initial value is this models end point
    init <- out[[i]][dim(out[[i]])[1], ]
    init <- init[, 2:dim(init)[2]]
    
    
    # init as to be numeric with the names of the variables
    namesOfVariables <- names(init)
    init <- as.numeric(init)
    names(init) <- namesOfVariables
  }
  
  for (i in 1:(number.of.models-1)){
    last <- dim(out[[i]])[1]
    out[[i]] <- out[[i]][-last,]
    
  }
  
  
  # change the list to a data frame
  out <- do.call(rbind, out)
  # do A nice graph
  endTime <- max(time.frame)
  melt.out <- melt(data = out, id.vars = c("time"))
  autonomyPlot <- ggplot(data = melt.out) + geom_line(mapping = aes(x = time, y = value, color = variable)) +
    ggtitle("Projected change in the Autonomy Classes") + ylab("Proportion") +
    scale_x_continuous(name = "Years", labels = seq(from = 2019, to = endTime + 2019, by = 5)) # numeric so continous x-scale
  #autonomyPlot
  
  ret <- list()
  ret$out <- out
  ret$plot <- autonomyPlot
  
  
  
  return(ret)  
  
}
# we need to remove the last row in all except the last calclulations

# ----- Personal ----

# create functions, Remember List
func1 <- function(time, state, parameters) {
  
  with(as.list(c(state, parameters)), {
    
    # change = Flow In - FlowOut
    dA0 <- -A0toA1*A0 - A0toA2*A0 - A0toA3*A0 - A0toA4*A0 - A0toA5*A0 + 500*exp(0.05*time)
    dA1 <- A0toA1*A0 - A1toA2*A1 - A1toA3*A1 - A1toA4*A1 - A1toA5*A1  + 500*exp(0.05*time)
    dA2 <- A0toA2*A0 + A1toA2*A1 - A2toA3*A2 - A2toA4*A2 - A2toA5*A2 
    dA3 <- A0toA3*A0 + A1toA3*A1 + A2toA3*A2 - A3toA4*A3 - A3toA5*A3 
    dA4 <- A0toA4*A0 + A1toA4*A1 + A2toA4*A2 + A3toA4*A3 - A4toA5*A4 
    dA5 <- A0toA5*A0 + A1toA5*A1 + A2toA5*A2 + A3toA5*A3 + A4toA5*A4 
    
    return(list(c(dA0, dA1, dA2, dA3, dA4, dA5)))
  })
}


# Add to the list
func.personal <- list()
func.personal[[1]] <- func1
func.personal[[2]] <- func1
func.personal[[3]] <- func1
func.personal[[4]] <- func1

# define parameters they have to have the correct names
delay.personal <- 0
param1 <- c(A0toA1 = 0.02,
            A0toA2 = 0.001,
            A0toA3 = 0.008,
            A0toA4 = 0.1,
            A0toA5 = 0.8,
            A1toA2 = 0,
            A1toA3 = 0,
            A1toA4 = 0,
            A1toA5 = 0,
            A2toA3 = 0,
            A2toA4 = 0,
            A2toA5 = 0,
            A3toA4 = 0,
            A3toA5 = 0,
            A4toA5 = 0)
delay.personal <- c(delay.personal, 5)
param2 <- c(A0toA1 = 0.01,
            A0toA2 = 0.001,
            A0toA3 = 0.001,
            A0toA4 = 0.001,
            A0toA5 = 0,
            A1toA2 = 0.001,
            A1toA3 = 0,
            A1toA4 = 0,
            A1toA5 = 0,
            A2toA3 = 0,
            A2toA4 = 0,
            A2toA5 = 0,
            A3toA4 = 0,
            A3toA5 = 0,
            A4toA5 = 0)

delay.personal <- c(delay.personal, 10)
param3 <- c(A0toA1 = 0.01,
            A0toA2 = 0.001,
            A0toA3 = 0.001,
            A0toA4 = 0.001,
            A0toA5 = 0.0005,
            A1toA2 = 0.004,
            A1toA3 = 0.004,
            A1toA4 = 0.004,
            A1toA5 = 0.004,
            A2toA3 = 0,
            A2toA4 = 0,
            A2toA5 = 0,
            A3toA4 = 0,
            A3toA5 = 0,
            A4toA5 = 0)

delay.personal <- c(delay.personal, 15)
param4 <- c(A0toA1 = 0.1,
            A0toA2 = 0.01,
            A0toA3 = 0.01,
            A0toA4 = 0.01,
            A0toA5 = 0.01,
            A1toA2 = 0.004,
            A1toA3 = 0.004,
            A1toA4 = 0.004,
            A1toA5 = 0.004,
            A2toA3 = 0,
            A2toA4 = 0,
            A2toA5 = 0,
            A3toA4 = 0,
            A3toA5 = 0,
            A4toA5 = 0)
delay.personal <- c(delay.personal, max(time.frame))

# Finnally add to list the paranm
param.personal <- list()
param.personal[[1]] <- param1
param.personal[[2]] <- param2
param.personal[[3]] <- param3
param.personal[[4]] <- param4











# ----- Commercial ----

# ----- Personal

# create functions, Remember List
func1 <- function(time, state, parameters) {
  
  with(as.list(c(state, parameters)), {
    
    # change = Flow In - FlowOut
    dA0 <- -A0toA1*A0 - A0toA2*A0 - A0toA3*A0 - A0toA4*A0 - A0toA5*A0
    dA1 <- A0toA1*A0 - A1toA2*A1 - A1toA3*A1 - A1toA4*A1 - A1toA5*A1
    dA2 <- A0toA2*A0 + A1toA2*A1 - A2toA3*A2 - A2toA4*A2 - A2toA5*A2 
    dA3 <- A0toA3*A0 + A1toA3*A1 + A2toA3*A2 - A3toA4*A3 - A3toA5*A3 
    dA4 <- A0toA4*A0 + A1toA4*A1 + A2toA4*A2 + A3toA4*A3 - A4toA5*A4 
    dA5 <- A0toA5*A0 + A1toA5*A1 + A2toA5*A2 + A3toA5*A3 + A4toA5*A4 
    
    return(list(c(dA0, dA1, dA2, dA3, dA4, dA5)))
  })
}


# Add to the list
func.commercial <- list()
func.commercial[[1]] <- func1
func.commercial[[2]] <- func1
func.commercial[[3]] <- func1
func.commercial[[4]] <- func1

# define parameters they have to have the correct names
delay.commercial <- 0
param1 <- c(A0toA1 = 0.2,
            A0toA2 = 0.001,
            A0toA3 = 0.008,
            A0toA4 = 0.1,
            A0toA5 = 0.8,
            A1toA2 = 0,
            A1toA3 = 0,
            A1toA4 = 0,
            A1toA5 = 0,
            A2toA3 = 0,
            A2toA4 = 0,
            A2toA5 = 0,
            A3toA4 = 0,
            A3toA5 = 0,
            A4toA5 = 0)
delay.commercial <- c(delay.commercial, 5)
param2 <- c(A0toA1 = 0.01,
            A0toA2 = 0.001,
            A0toA3 = 0.001,
            A0toA4 = 0.001,
            A0toA5 = 0,
            A1toA2 = 0.001,
            A1toA3 = 0,
            A1toA4 = 0,
            A1toA5 = 0,
            A2toA3 = 0,
            A2toA4 = 0,
            A2toA5 = 0,
            A3toA4 = 0,
            A3toA5 = 0,
            A4toA5 = 0)

delay.commercial <- c(delay.commercial, 10)
param3 <- c(A0toA1 = 0.01,
            A0toA2 = 0.001,
            A0toA3 = 0.001,
            A0toA4 = 0.001,
            A0toA5 = 0.0005,
            A1toA2 = 0.004,
            A1toA3 = 0.004,
            A1toA4 = 0.004,
            A1toA5 = 0.004,
            A2toA3 = 0,
            A2toA4 = 0,
            A2toA5 = 0,
            A3toA4 = 0,
            A3toA5 = 0,
            A4toA5 = 0)

delay.commercial <- c(delay.commercial, 15)
param4 <- c(A0toA1 = 0.1,
            A0toA2 = 0.01,
            A0toA3 = 0.01,
            A0toA4 = 0.01,
            A0toA5 = 0.01,
            A1toA2 = 0.004,
            A1toA3 = 0.004,
            A1toA4 = 0.004,
            A1toA5 = 0.004,
            A2toA3 = 0,
            A2toA4 = 0,
            A2toA5 = 0,
            A3toA4 = 0,
            A3toA5 = 0,
            A4toA5 = 0)
delay.commercial <- c(delay.commercial, max(time.frame))

# Finnally add to list the paranm
param.commercial <- list()
param.commercial[[1]] <- param1
param.commercial[[2]] <- param2
param.commercial[[3]] <- param3
param.commercial[[4]] <- param4
