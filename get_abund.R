get_abund<-function(tt=list(nst_l_f1_mc, nst_h_f1_mc, mst_l_f1_mc, mst_h_f1_mc, nlt_l_f1_mc, nlt_h_f1_mc, mlt_l_f1_mc, mlt_h_f1_mc)){
  # function to output the abundance differences by simulation, averaged across scenario/parameterizations

  n.tt<-length(tt)
  SPP<-c("krill", "seals", "pengs","whales", "fish")
  SIM=c("Base", "GGP", "Fishing", "GGP_Fishing")
  SCENARIO<-c("nst_l_f1", "nst_h_f1", "mst_l_f1", "mst_h_f1", "nlt_l_f1", "nlt_h_f1", "mlt_l_f1", "mlt_h_f1")
  MOVE.SCENARIO<-rep(c("No_movement","No_movement","Movement","Movement"),2)
  STABLE.SCENARIO<-c(rep("Stable",4), rep("Linear",4))
  RCP<-rep(c("RCP2.6","RCP8.5"),4)
  #
  # KRILL BIOMASS 
  #create a null data frame to append data to
  out<-data.frame(Spp=NA, SSMU=NA, Scenario=NA, Simulation=NA, Trial=NA, Movement=NA, Stable=NA, RCP=NA, Biomass=NA)
  # summarize by SSMU
  for(j in 1:15){
    # j indexes the SSMUs
    for(k in 1:n.tt){
      # k indexes the different parameter/ggp/fishing scenarios
      # establish number of trials - assumes all objects in tt used the same set up
      n.trials<-tt[[1]][[1]]$setup$ntrials
      # now loop over all simulations (N=4)
      # create a marix to house results for simple statistics
      # out_r<-matrix(0, nrow=n.trials, ncol=4)  #For r output
      out_N<-matrix(0, nrow=4, ncol=n.trials)
      for(m in 1:4){
        # m indexes the base, ggp, fish, and gpp_fish simulations
        Yall<-numeric()
        r_base<-numeric()
        for(ii in 1:n.trials){
          #ii indexes the trials in the mc simulation
          # pull out the last value of the trajectory
          ttt<-tt[[k]] # use just one of the scenarios
          # change abundance of krill to biomass
          n.x<-length(ttt[[m]]$N[[1]][,j,ii])
          krill.bm<-ttt[[m]]$N$krill[5:n.x, ,]*ttt[[m]]$wbar
          init.bm<-krill.bm[1,j,ii]
          Yt<-krill.bm[,j,ii]
#### For 30-yr averages #####
            last.bm<-(length(Yt)-60):length(Yt) # length for last 60 seasons/30 years
            Yt<-Yt[last.bm] # subset out last 30 years
#             Yt<-Yt[seq(from=1, by=2, to=60)] # subset only summer values
            Yt<-Yt[seq(from=2, by=2, to=60)] # subset only winter values
            Yt<-mean(Yt)            # average levels over last 30 years
            Yall[ii]<-Yt            # pass out the result
#############################            
          
         }
        out_N[m,]<-Yall
      }
# 
      base<-(out_N[1,])        # Base krill biomass at end of trials
      sim1<-(out_N[2,]/out_N[1,])   # End krill biomass for GGP sims
      sim2<-(out_N[3,]/out_N[1,])   # ... for Fishing Only sims
      sim3<-(out_N[4,]/out_N[1,])   # ... for Fishing + GGP sims
#      
      biomass<-c(base, sim1, sim2, sim3)
#
# now output the data
       out.m<-data.frame(Spp=rep(SPP[1], 4*n.trials), SSMU=rep(j, 4*n.trials), Scenario=rep(SCENARIO[k], 4*n.trials),  
                        Simulation=rep(SIM, each=n.trials), Trial=rep(1:n.trials, 4), Movement=rep(MOVE.SCENARIO[k],4*n.trials), 
                        Stable=rep(STABLE.SCENARIO[k], 4*n.trials), RCP=rep(RCP[k], 4*n.trials), Biomass=biomass)
      out<-rbind(out, out.m)
      out<-out[-1,]  # remove NAs
    }
    krill_B <- out
  }
  # keep the output for krill
  assign("krill_biom", krill_B,.GlobalEnv)

  
### FOR ALL SPECIES ABUNDANCES ###
  out<-data.frame(Spp=NA, SSMU=NA, Scenario=NA, Simulation=NA, Trial=NA, Movement=NA, Stable=NA, RCP=NA, Biomass=NA)
  for(i in 1:5){
    # i indexes the predator group
    # summarize by SSMU
    for(j in 1:15){
      # j indexes teh SSMUs
      # now for each trail within each simulation in the input list, pull out the approproate marginal r
      # house output for each simulation
      for(k in 1:n.tt){
         #k indexes the differnt parameter/ggp/fishing scenarios
        # establish number of trials - assumes all objects in tt used the same set up
        n.trials<-tt[[1]][[1]]$setup$ntrials
        # now loop over all simulations (N=4)
        # create a marix to house results for simple statistics
        out_r<-matrix(0, nrow=n.trials, ncol=4)
        out_N<-matrix(0, nrow=4, ncol=n.trials)
        for(m in 1:4){
          # m indexes the base, ggp, fish, and gpp_fish simulations
          Yall<-numeric()
          r_base<-numeric()
          for(ii in 1:n.trials){
            # ii indexes the trials in the mc simulation
            # pull out the last value of the trajectory
            ttt<-tt[[k]] # use just one of the scenarios
            n.x<-length(ttt[[m]]$N[[i]][,j,ii])
            init.n<-ttt[[m]]$N[[i]][,j,ii][1]
            n.trials<-ttt[[1]]$setup$ntrials
#### for 30 year averages ######
            Yt<-ttt[[m]]$N[[i]][,j,ii][(n.x-60):n.x] # pull out the last 30 years of abundances (60 time steps)
#             Yt<-Yt[seq(from=1, by=2, to=60)]# subset just the summer values
              Yt<-Yt[seq(from=2, by=2, to=60)]# subset just the winter values
            Yall[ii]<-mean(Yt)# average abundance during last 30 years
################################
          }
          out_N[m,]<-Yall
        }
       base<-(out_N[1,])       # Base sim final abundance
       sim1<-(out_N[2,]/out_N[1,])  # End abundance of GGP only sims
       sim2<-(out_N[3,]/out_N[1,])  # ... of Fishing Only sims
       sim3<-(out_N[4,]/out_N[1,])  # ... of Fishing + GGP sims
       biomass<-c(base, sim1, sim2, sim3)
# now output the data
        out.m<-data.frame(Spp=rep(SPP[i], 4*n.trials), SSMU=rep(j, 4*n.trials), Scenario=rep(SCENARIO[k], 4*n.trials),
                Simulation=rep(SIM, each=n.trials), Trial=rep(1:n.trials, 4), Movement=rep(MOVE.SCENARIO[k],4*n.trials),
                Stable=rep(STABLE.SCENARIO[k], 4*n.trials), RCP=rep(RCP[k], 4*n.trials), Biomass=biomass)
        out<-rbind(out, out.m)
        out<-out[-1,]  # remove NAs
      }
    }
  }
  # keep the output for preds
  assign("preds_abund", out,.GlobalEnv)
}






     