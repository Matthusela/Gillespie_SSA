# Loading the required R-packages
library(GillespieSSA)
# Defining the nu vector as a 6 row 10 column matrix, 6 rows for the 6 
# different states, and 10 columns for the 10 reactions.

nu <- matrix(c(-1,1,0,0,0,0,0,0,0,0,
        1,-1,-1,1,0,0,0,0,0,0,
        0,0,1,-1,-1,1,0,0,0,0,
        0,0,0,0,1,-1,-1,1,0,0,
        0,0,0,0,0,0,1,-1,-1,1,
        0,0,0,0,0,0,0,0,1,-1), nrow=6,ncol=10,byrow=T)

# Choose the initial state vector to be the queue being empty, therefore only 
# state zero is populated

x0 <- c(state0=1,state1=0,state2=0,state3=0,state4=0,state5=0)
x0

# The propensity values can be retrieved from the rate matrix \Lambda given
# in the report, the propensity values are given in the following length 10
# vector

a <- c("1*state0","0.5*state1","1*state1","1*state2","1*state2",
       "1.5*state3","1*state3","1.5*state4","1*state4","1.5*state5")
propensities <- as.vector(a,mode = "any")
propensities

# Using the SSA function to simulate the queuing process, tf argument 
# determines the maximum number of time units in the simulation, in arbitrary 
# units.
simulation <- ssa(x0,propensities,nu,tf = 100,simName = "M/M/3/5 Queue",
                  method = ssa.d())
# Finding the number of transitions the simulation went through
number_of_entries <- length(simulation$data[,1])
# Checking if there are more than 100 transitions in the simulation, if
# not increase the value of tf in the ssa function
number_of_entries > 100
# This retrieves the states from the output of the simulation and converts
# it to one vector which indicates the state the queue is in after each
# transition
state_vector <- replicate(number_of_entries,0) +simulation$data[,3]+2*simulation$data[,4]+
  3*simulation$data[,5]+4*simulation$data[,6]+5*simulation$data[,7]
# Removing the last entry as it is replicated by the simulation
state_vector <- state_vector[1:number_of_entries-1]
times <- times[1:number_of_entries-1]
# Extracting the times of each transition from the simulation
times <- simulation$data[,1]
#considering first 100 transitions only
state_vector100 <- state_vector[1:100]
times100 <- times[1:100]
# Plotting the state vector against time
plot(times100,state_vector100,xlab = "Time", ylab = "Queue Size",pch=20)
lines(times100,state_vector100,lwd=0.5,col="red")
# Using the R "diff" function to easily compute the differences between the
# entries in the times vector in order to determine the time steps between
# transitions.
time_steps <- diff(times)
# The 99 time steps corresponding to the first 100 transitions
time_steps99 <- time_steps[1:99]
# Histogram of time steps
# Ordering the time steps vector into ascending order 
ordered_timesteps <- time_steps99[order(time_steps99)]
hist(x=ordered_timesteps, breaks=6,freq = F,main="",xlab="")
title(main="Histogram of Time Steps",xlab="Time Step Values")
?title
lines(ordered_timesteps,
      dexp(ordered_timesteps,rate = 1/mean(ordered_timesteps)),
      col = "red",lwd = 1.5)
# Performing 10 longer simulations of the queue in order to assess
# the probability of a time step/transition being longer than 2 minutes

simulation_long1 <- ssa(x0,propensities,nu,tf = 100000,method = ssa.d())
simulation_long2 <- ssa(x0,propensities,nu,tf = 100000,method = ssa.d())
simulation_long3 <- ssa(x0,propensities,nu,tf = 100000,method = ssa.d())
simulation_long4 <- ssa(x0,propensities,nu,tf = 100000,method = ssa.d())
simulation_long5 <- ssa(x0,propensities,nu,tf = 100000,method = ssa.d())
simulation_long6 <- ssa(x0,propensities,nu,tf = 100000,method = ssa.d())
simulation_long7 <- ssa(x0,propensities,nu,tf = 100000,method = ssa.d())
simulation_long8 <- ssa(x0,propensities,nu,tf = 100000,method = ssa.d())
simulation_long9 <- ssa(x0,propensities,nu,tf = 100000,method = ssa.d())
simulation_long10 <- ssa(x0,propensities,nu,tf = 100000,method = ssa.d())
# Extracting the different times from each long simulation and truncating
# by removing the very last duplicated entry
times_long1 <- head(simulation_long1$data[,1],-1)
times_long2 <- head(simulation_long2$data[,1],-1)
times_long3 <- head(simulation_long3$data[,1],-1)
times_long4 <- head(simulation_long4$data[,1],-1)
times_long5 <- head(simulation_long5$data[,1],-1)
times_long6 <- head(simulation_long6$data[,1],-1)
times_long7 <- head(simulation_long7$data[,1],-1)
times_long8 <- head(simulation_long8$data[,1],-1)
times_long9 <- head(simulation_long9$data[,1],-1)
times_long10 <- head(simulation_long10$data[,1],-1)
# Finding the time steps for each time vector
time_steps_long1 <- diff(times_long1)
time_steps_long2 <- diff(times_long2)
time_steps_long3 <- diff(times_long3)
time_steps_long4 <- diff(times_long4)
time_steps_long5 <- diff(times_long5)
time_steps_long6 <- diff(times_long6)
time_steps_long7 <- diff(times_long7)
time_steps_long8 <- diff(times_long8)
time_steps_long9 <- diff(times_long9)
time_steps_long10 <- diff(times_long10)
# Finding the number of time steps greater than 2 from each time step vector
num_greater_than_2_vec1 <- length(time_steps_long1[time_steps_long1>2])
num_greater_than_2_vec2 <- length(time_steps_long2[time_steps_long2>2])
num_greater_than_2_vec3 <- length(time_steps_long3[time_steps_long3>2])
num_greater_than_2_vec4 <- length(time_steps_long4[time_steps_long4>2])
num_greater_than_2_vec5 <- length(time_steps_long5[time_steps_long5>2])
num_greater_than_2_vec6 <- length(time_steps_long6[time_steps_long6>2])
num_greater_than_2_vec7 <- length(time_steps_long7[time_steps_long7>2])
num_greater_than_2_vec8 <- length(time_steps_long8[time_steps_long8>2])
num_greater_than_2_vec9 <- length(time_steps_long9[time_steps_long9>2])
num_greater_than_2_vec10 <- length(time_steps_long10[time_steps_long10>2])
# Finding the proportion of time steps greater than 2 from each simulation
proportion_long_sim1 <- num_greater_than_2_vec1/length(time_steps_long1)
proportion_long_sim2 <- num_greater_than_2_vec2/length(time_steps_long2)
proportion_long_sim3 <- num_greater_than_2_vec3/length(time_steps_long3)
proportion_long_sim4 <- num_greater_than_2_vec4/length(time_steps_long4)
proportion_long_sim5 <- num_greater_than_2_vec5/length(time_steps_long5)
proportion_long_sim6 <- num_greater_than_2_vec6/length(time_steps_long6)
proportion_long_sim7 <- num_greater_than_2_vec7/length(time_steps_long7)
proportion_long_sim8 <- num_greater_than_2_vec8/length(time_steps_long8)
proportion_long_sim9 <- num_greater_than_2_vec9/length(time_steps_long9)
proportion_long_sim10 <- num_greater_than_2_vec10/length(time_steps_long10)
# Taking the mean of these 10 proportions
mean_greater_than_2<-(proportion_long_sim1+proportion_long_sim2+
                        proportion_long_sim3+proportion_long_sim4+
  proportion_long_sim5+proportion_long_sim6+
  proportion_long_sim7+proportion_long_sim8+proportion_long_sim9+
  proportion_long_sim10)/10
mean_greater_than_2
### Ignore Below ###
simulation_long1$data
length(simulation_long1$data[,2][simulation_long1$data[,2] == 1])/184868
number_of_entries_long <- length(simulation_long1$data[,1])
state_vector_long <- replicate(number_of_entries_long,0) +
  simulation_long1$data[,3]+2*simulation_long1$data[,4]+
        3*simulation_long1$data[,5]+4*simulation_long1$data[,6]+
  5*simulation_long1$data[,7]
length_long <- length(state_vector_long)
length(state_vector_long[state_vector_long == 0])/length_long
length(state_vector_long[state_vector_long == 1])/length_long
length(state_vector_long[state_vector_long == 2])/length_long
length(state_vector_long[state_vector_long == 3])/length_long
length(state_vector_long[state_vector_long == 4])/length_long
length(state_vector_long[state_vector_long == 5])/length_long
0.06960642+0.2086462+0.2783824+0.2301264+0.1520166+0.06122206
length(state_vector[state_vector==0])/length(state_vector)


# Very long simulation

simulation_vlong1 <- ssa(x0,propensities,nu,tf = 1000000,method = ssa.otl())
number_of_entries_vlong <- length(simulation_vlong1$data[,1])
number_of_entries_vlong
state_vector_vlong <- replicate(number_of_entries_vlong,0) +
  simulation_vlong1$data[,3]+2*simulation_vlong1$data[,4]+
  3*simulation_vlong1$data[,5]+4*simulation_vlong1$data[,6]+
  5*simulation_vlong1$data[,7]
length(state_vector_vlong[state_vector_vlong == 0])/number_of_entries_vlong
state_vector_vlong
tim <- simulation_vlong1$data[,1]
tim
mean(diff(tim)) * number_of_entries_vlong
plot(simulation_vlong1$data[,1],state_vector_vlong)
