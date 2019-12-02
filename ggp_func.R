ggp_func<-function(p1=mlt.fit.calendar[[1]], MC=FALSE, COMPETITION=FALSE, GGP1=GGPRCP26CHL100, NTRIALS=10, NYEARS=93, SD.KRILL.RDEV=0.7, FISHING.OPTION=NULL, GAMMA=0.093, START.YR=1, STOP.YR=NULL, BT.ABUND.VAR=FALSE){
# Function to run the Monte Carlo scenarios along with climate change inputs to krill Gross Growth Potential (GGP)
# This code also updates predator foraging and fishery distributions as of 2017
#
# define a second parameter set to be used for future simulations  
p2 <- p1
#
###### UPDATED PREDATOR FORAGING -- ESK Oct 2017 #########################
# Update foraging matrix for SSMUs 1-8
fp2 <- c(0.053, 0.014, 0.156, 0.027, 0.184, 0.298, 0.007, 0.101, 0.019, 0.001, 0.001, 0.001, 0.002, 0, 0.001, 0.121, 0.007, 0.006)
fp3 <- c(0.122, 0.021, 0.211, 0.058, 0.079, 0.048, 0.020, 0.000, 0.053, 0.003, 0.003, 0.002, 0.007, 0, 0.002, 0.337, 0.019, 0.017)
fp4 <- c(0.140, 0.019, 0.170, 0.061, 0.028, 0.050, 0.023, 0.000, 0.061, 0.003, 0.003, 0.002, 0.008, 0, 0.002, 0.388, 0.022, 0.019)
fp5 <- c(0.138, 0.019, 0.175, 0.061, 0.034, 0.050, 0.023, 0.000, 0.060, 0.003, 0.003, 0.002, 0.008, 0, 0.002, 0.383, 0.022, 0.019)
fp6 <- c(0.055, 0.012, 0.129, 0.025, 0.169, 0.327, 0.007, 0.112, 0.020, 0.001, 0.001, 0.001, 0.002, 0, 0.001, 0.125, 0.007, 0.006)
fp7 <- c(0.142, 0.018, 0.166, 0.061, 0.023, 0.050, 0.023, 0.000, 0.061, 0.003, 0.003, 0.002, 0.008, 0, 0.002, 0.393, 0.022, 0.020)
fp8 <- c(0.018, 0, 0.001, 0, 0.184, 0.582, 0, 0.214, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
#
# p2$PENGS$ssmu.1$foraging.matrix[2,] <- fp1
p2$PENGS$ssmu.2$foraging.matrix[2,] <- fp2
p2$PENGS$ssmu.3$foraging.matrix[2,] <- fp3
p2$PENGS$ssmu.4$foraging.matrix[2,] <- fp4
p2$PENGS$ssmu.5$foraging.matrix[2,] <- fp5
p2$PENGS$ssmu.6$foraging.matrix[2,] <- fp6
p2$PENGS$ssmu.7$foraging.matrix[2,] <- fp7
p2$PENGS$ssmu.8$foraging.matrix[2,] <- fp8
#
fs3 <- c(0.165,0.017,0.089,0.012,0.035,0.017,0.020,0.000,0.113,0.021,0.002,0.000,0.046,0.010,0.011,0.378,0.054,0.008)
fs4 <- c(0.165,0.017,0.089,0.012,0.035,0.017,0.020,0.000,0.113,0.021,0.002,0.000,0.046,0.010,0.011,0.378,0.054,0.008)
fs7 <- c(0.165,0.017,0.089,0.012,0.035,0.017,0.020,0.000,0.113,0.021,0.002,0.000,0.046,0.010,0.011,0.378,0.054,0.008)
#			
p2$SEALS$ssmu.3$foraging.matrix[2,] <- fs3
p2$SEALS$ssmu.4$foraging.matrix[2,] <- fs4
p2$SEALS$ssmu.7$foraging.matrix[2,] <- fs7
#
fw1 <- c(0.247,0.283,0.000,0.000,0.066,0.016,0.001,0.003,0.099,0.000,0.000,0.000,0.013,0.000,0.000,0.231,0.013,0.030)
#
p2$WHALES$ssmu.1$foraging.matrix[2,] <- fw1
#
## Update QQmax for whales and seals, only in SSMUs where they are modeled - ESK 9/22/2016
seal_q_const <- 0.867441
#
p2$SEALS$ssmu.3$QQmax[2] <- p2$SEALS$ssmu.3$QQmax[2] * seal_q_const
p2$SEALS$ssmu.4$QQmax[2] <- p2$SEALS$ssmu.4$QQmax[2] * seal_q_const
p2$SEALS$ssmu.7$QQmax[2] <- p2$SEALS$ssmu.7$QQmax[2] * seal_q_const
#
whale_q_const <- 0.407784266
#
p2$WHALES$ssmu.1$QQmax[2] <- p2$WHALES$ssmu.1$QQmax[1] * whale_q_const
#
# #######################################################################
#
# run set up during the calendar period to ensure parameters produce historical dynamics
p1saved<-ssmu.ss(param.list=p1, GGP=NULL, nyears=38, random.Rkrill=FALSE, single.sim=FALSE, suppress.messages=TRUE)
#
# plot fit to calendar to view results
plot.ss.calendar(p1saved)
#
# now change p2 values for forward simulation from the saved state
# use final value of the ENV.INDEX for all future time steps
p2$ENV.INDEX <- p1$ENV.INDEX[length(p1$ENV.INDEX)] 
#

###### UPDATED CATCH, 2009-2015 -- ESK Oct 2017 ######################### 

# Current fishing distribution with 25% caught in 48.1 (as per current CM 51-07)
hstry <- c(412051518.3, 16604439083, 12496675610, 7097563613, 67434467285,33636345931, 2348027520, 584104438.7, 2141151199, 2.40158E+11, 6334799420, 253122957.2, 85597135.3, 7438391058, 1.6543E+11)
p2$HISTORICAL.CATCH<-hstry
# # 
# # # Update for new seasonality of fishery
s1 <- c(0.7852,0.4421,0.4466,0.7591,0.3015,0.3706,0.8346,0.7322,0.8575,0.4819,0.9631,0.5266,0.7429,0.0713,0.0114)
s2 <- c(0.2148,0.5579,0.5534,0.2409,0.6985,0.6294,0.1654,0.2678,0.1425,0.5181,0.0369,0.4734,0.2571,0.9287,0.9886)
catch.setup <- matrix(0, nrow = 2, ncol = 15)
catch.setup[1,] <- s1
catch.setup[2,] <- s2

p2$CATCH.SETUP <- catch.setup

############################################
#
# now for bathtubs
# set bathtub abundance equal to final abundance from spin-up simulations
tt.tubs <- numeric(p2$NTUBS)
for(i in 1:length(tt.tubs)){
  tt.tubs[i]<-p1$BATHTUB.ABUNDANCE[[i]][length(p1$BATHTUB.ABUNDANCE[[i]])]
}
tt.tubs <- data.frame(t(tt.tubs))
p2$BATHTUB.ABUNDANCE<-tt.tubs
#
#
# optionally use the function bt.abund.var to set random krill abundance in the bathtubs for future years
if(BT.ABUND.VAR){
  p2$BATHTUB.ABUNDANCE<-bt.abund.var(btmeans=tt.tubs, nyears=NYEARS, ntrials=NTRIALS, sd.krill.Rdev=SD.KRILL.RDEV)
  #print(str(p2$BATHTUB.ABUNDANCE))
}
#
if(COMPETITION){
  # adjust competitive ability of one or more groups
  # note that setting one group to 3 and all others implies that group is 3x more competitive. 
  # values will be normalized to sum to 1 within ssmu.ss
  # For example - 
  # p2$COMPETITION.MATRIX[,3]<-3 #make whales superior competitors 
}
# 
# run some simulations, either monte carlo (stochastic) or not.
if(MC==FALSE){
  # run deterministic simulations to understand basic dynamics
  #
  # run a base case
  p2proj<-ssmu.ss(param.list=p2, safe.mode=FALSE, saved.state=p1saved,  GGP=NULL, nyears=NYEARS, random.Rkrill=FALSE, sd.krill.Rdev=SD.KRILL.RDEV, fishing.option=NULL, actual.gamma=GAMMA, start.fishing.yr=START.YR, stop.fishing.yr=STOP.YR, suppress.messages=TRUE)
  # run a sim with a GGP object to drive krill weight. 
  p2proj1<-ssmu.ss(param.list=p2, safe.mode=FALSE, saved.state=p1saved, GGP=GGP1, nyears=NYEARS, random.Rkrill=FALSE, sd.krill.Rdev=SD.KRILL.RDEV, fishing.option=NULL, actual.gamma=GAMMA, start.fishing.yr=START.YR, stop.fishing.yr=STOP.YR, suppress.messages=TRUE)
  # run a third sim with fising only to compare trajectories
  p2proj2<-ssmu.ss(param.list=p2, safe.mode=FALSE, saved.state=p1saved, GGP=NULL, nyears=NYEARS, random.Rkrill=FALSE, sd.krill.Rdev=SD.KRILL.RDEV, fishing.option=FISHING.OPTION, actual.gamma=GAMMA, start.fishing.yr=START.YR, stop.fishing.yr=STOP.YR, suppress.messages=TRUE)
  # run a final simulation with fishing and GGP
  p2proj3<-ssmu.ss(param.list=p2, safe.mode=FALSE, saved.state=p1saved, GGP=GGP1, nyears=NYEARS, random.Rkrill=FALSE, sd.krill.Rdev=SD.KRILL.RDEV, fishing.option=FISHING.OPTION, actual.gamma=GAMMA, start.fishing.yr=START.YR, stop.fishing.yr=STOP.YR, suppress.messages=TRUE)
  # assemble all sims into a list for output
  out<-list(Base=p2proj, GGP=p2proj1, Fishing=p2proj2, GGP_Fishing=p2proj3)
} else {
  # run tochastic simulations to understand variability
  #
  # run the base case
  p2proj<-ssmu.mc(param.list=p2, safe.mode=FALSE, saved.state=p1saved,  ggp=NULL, ntrials=NTRIALS, nyears=NYEARS, random.Rkrill=TRUE, sd.krill.Rdev=SD.KRILL.RDEV, fishing.option=NULL, actual.gamma=GAMMA, start.fishing.yr=START.YR, stop.fishing.yr=STOP.YR, plot.now=FALSE) 
  # run a sim with a GGP object to drive krill weight. 
  p2proj1<-ssmu.mc(param.list=p2, safe.mode=FALSE, saved.state=p1saved, ggp=GGP1, ntrials=NTRIALS, nyears=NYEARS, random.Rkrill=TRUE, sd.krill.Rdev=SD.KRILL.RDEV, fishing.option=NULL, actual.gamma=GAMMA, start.fishing.yr=START.YR, stop.fishing.yr=STOP.YR, plot.now=FALSE)
  # run a third sim with fising only to compare trajectories
  p2proj2<-ssmu.mc(param.list=p2, safe.mode=FALSE, saved.state=p1saved, ggp=NULL, ntrials=NTRIALS, nyears=NYEARS, random.Rkrill=TRUE, sd.krill.Rdev=SD.KRILL.RDEV, fishing.option=FISHING.OPTION, actual.gamma=GAMMA, start.fishing.yr=START.YR, stop.fishing.yr=STOP.YR, plot.now=FALSE)
  # run a final simulation with fishing and GGP 
  p2proj3<-ssmu.mc(param.list=p2, safe.mode=FALSE, saved.state=p1saved, ggp=GGP1, ntrials=NTRIALS, nyears=NYEARS, random.Rkrill=TRUE, sd.krill.Rdev=SD.KRILL.RDEV, fishing.option=FISHING.OPTION, actual.gamma=GAMMA, start.fishing.yr=START.YR, stop.fishing.yr=STOP.YR, plot.now=FALSE)
  # assemble all sims into a list for output
  out<-list(Base=p2proj, GGP=p2proj1, Fishing=p2proj2, GGP_Fishing=p2proj3)
}
#graphics.off()
out
}