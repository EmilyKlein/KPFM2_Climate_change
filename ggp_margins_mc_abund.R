ggp_margins_mc_abund<-function(tt=list(nst_l_f1_mc, nst_h_f1_mc, mst_l_f1_mc, mst_h_f1_mc, nlt_l_f1_mc, nlt_h_f1_mc, mlt_l_f1_mc, mlt_h_f1_mc), MAP=FALSE, MARGIN="GGP"){
 ## NOTE: 'MARGIN' will designate which simulation to plot when mapping.  
  
  require(lattice)
  library(sp)
  library(mapproj)
  library(RColorBrewer)
  n.tt<-length(tt)
  # function to compute and plot the change in biomass (krill) and abdunance (predators) from MC runs
  # Note: Changes in biomass/abund are compared to base case end biomass/abundance 
  # UPDATED FROM ggp_margins_mc() BY ESK 17 OCT 2017
  # 
  Col=c("red","green", "blue")
  SPP<-c("krill", "seals", "pengs","whales","fish")
  SIM=c("GGP", "Fishing", "GGP_Fishing")
  SCENARIO<-c("nst_l_f1", "nst_h_f1", "mst_l_f1", "mst_h_f1", "nlt_l_f1", "nlt_h_f1", "mlt_l_f1", "mlt_h_f1")
  MOVE.SCENARIO<-rep(c("No_movement","No_movement","Movement","Movement"),2)
  STABLE.SCENARIO<-c(rep("Stable",4), rep("Linear",4))
  RCP<-rep(c("RCP2.6","RCP8.5"),4)
  #create a null data frame to append data to
  out<-data.frame(Spp=NA, SSMU=NA, Scenario=NA, Simulation=NA, Trial=NA, Movement=NA, Stable=NA, RCP=NA, Biomass=NA)
  #start with krill alone
    # summarize by SSMU
    for(j in 1:15){
      # j indexes the SSMUs
      # house output for each simulation
      for(k in 1:n.tt){
        #k indexes the different parameter/ggp/fishing scenarios
        # establish number of trials - assumes all objects in tt used the same set up
        n.trials<-tt[[1]][[1]]$setup$ntrials
        # now loop over all simulations (N=4)
        # create a marix to house results for simple statistics
        out_N<-matrix(0, nrow=4, ncol=n.trials)
        for(m in 1:4){
          # m indexes the base, ggp, fish, and gpp_fish simulations
          Yall<-numeric()
          for(ii in 1:n.trials){
            #ii indexes the trials in the mc simulation
            # pull out the last value of the trajectory
            ttt<-tt[[k]] # use just one of the scenarios
            # change abundance of krill to biomass
            n.x<-length(ttt[[m]]$N[[1]][,j,ii])   # n.x to reference last value, by sim (m), SSMU (j, 1-15), and trial (ii, 1-30) 
            krill.bm<-ttt[[m]]$N$krill[5:n.x, ,]*ttt[[m]]$wbar  #translate krill abundances to biomass via wbar
            init.bm<-krill.bm[1,j,ii]   # intial biomass by sim, SSMU, and trial
            Yt<-krill.bm[,j,ii]         # vector of krill biomass by SSMU and trial
            # last.bm<-length(Yt)         # last.bm to reference final value in krill biomass vectors
#             Yt<-Yt[last.bm]             # last value in krill biomass vectors = winter season (replace Yt from line 41)
#             Yall[ii]<-Yt                # Fills in Yall with last value of krill biomass (winter season)
#             Ys<-Yt[(last.bm-1)]         # last value in krill biomass vectors = summer season (replace Yt from line 41)
#             Yall[ii]<-Ys                # Fills in Yall with last value of krill biomass (summer season)
#
##### For 30-yr averages #####
            last.bm<-(length(Yt)-60):length(Yt) # length for last 60 seasons/30 years
            Yt<-Yt[last.bm] # subset out last 30 years
            # Yt<-Yt[seq(from=1, by=2, to=60)] # subset only summer values
            Yt<-Yt[seq(from=2, by=2, to=60)] # subset only winter values
            Yt<-mean(Yt)            # average levels over last 30 years
            Yall[ii]<-Yt            # pass out the result
#############################            
            }
          out_N[m,]<-Yall
        }
        # Changes in abundance  ESK 17 Oct 2017
        base<-(out_N[1,])        # Base abundance at end of trials
        sim1<-((out_N[2,])/base) # Effect of ggp only
        sim2<-((out_N[3,])/base) # Effect of fishing only
        sim3<-((out_N[4,])/base) # Effect of fishing and ggp

        # Impact of GGP on abundance relative to impact of fishing alone   ESK 27 Oct 2017
        #sim4<-(sim3/sim2) # Effect of ggp+fishing proportional to impact of fishing alone
        
        biomass<-c(sim1, sim2, sim3)
        
        # now output the data
        out.m<-data.frame(Spp=rep(SPP[1], 3*n.trials), SSMU=rep(j, 3*n.trials), Scenario=rep(SCENARIO[k], 3*n.trials),  
                          Simulation=rep(SIM, each=n.trials), Trial=rep(1:n.trials, 3), Movement=rep(MOVE.SCENARIO[k],3*n.trials), 
                          Stable=rep(STABLE.SCENARIO[k], 3*n.trials), RCP=rep(RCP[k], 3*n.trials), Biomass=biomass)
        out<-rbind(out, out.m)
      }
    }
  bm.out<-out[-1,]
  rm(out)
  #for(ii in 1:5){
  # create a lattice plot of r organized by simulation type and and species
  #out.spp<-out[out$Spp==SPP[ii],]
  bm.out$SSMUf<-as.factor(bm.out$SSMU)
  #out.spp$Marginal_r<-ifelse(is.na(out.spp$Marginal_r),0, out.spp$Marginal_r)
  windows()
  MAIN="Krill biomass"
  print(bwplot(Biomass~SSMUf|Simulation*RCP, data=bm.out, strip=TRUE, cex=0.75, pch="|", 
        col="black", fill="gray", box.width=0.9, main=MAIN, ylim=c(0, 2.0), xlab="SSMU", ylab="Biomass", 
        scales=list(alternating=FALSE), 
                 panel=function(...){
                   panel.abline(h=0, col="black", lty=1, lwd=1)
                   panel.abline(h=0.75, col="red", lty=1, lwd=1)
                   panel.abline(h=1.0, col="green", lty=1, lwd=1)
                   panel.bwplot(...)
                 }))
  
  #create maps of changes in krill biomass
  if(MAP){
    # formate out.m for use in a mapping exercies, where a single value to represent each ssmu is necessary
    Biom<-data.frame(SSMU=c("APPA", "APW","APDPW","APDPE","APBSW", "APBSE","APEI","APE","SOPA","SOW","SONE","SOSE","SGPA","SGW","SGE"))
    bm.ggp<-bm.out[bm.out$Simulation==MARGIN,]    
    Biom<-cbind(Biom, tapply(bm.ggp$Biom, list(bm.ggp$SSMU, bm.ggp$RCP), median.func)) 
    bt.data<-data.frame(SSMU=c("BT1", "BT2", "BT3"), RCP2.6=rep(NA, 3), RCP8.5=rep(NA, 3))
    Biom<-rbind(Biom, bt.data)
    plot_basic_polygons(SPP=bm.out$Spp[1], r.data=Biom)
    
    
  }

  
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
        #create a marix to house results for simple statistics
#       out_r<-matrix(0, nrow=n.trials, ncol=4)
        out_N<-matrix(0, nrow=4, ncol=n.trials)
        for(m in 1:4){
          # m indexes the base, ggp, fish, and gpp_fish simulations
          Yall<-numeric()
#           r_base<-numeric()
          for(ii in 1:n.trials){
            #ii indexes the trials in the mc simulation
            # pull out the last value of the trajectory
            ttt<-tt[[k]] # use just one of the scenarios
            n.x<-length(ttt[[m]]$N[[i]][,j,ii])    # n.x to reference last value, by sim (m), SSMU (j, 1-15), and trial (ii, 1-30)  
            init.n<-ttt[[m]]$N[[i]][,j,ii][1]      # intial biomass by sim, SSMU, and trial
#             Yt<-ttt[[m]]$N[[i]][,j,ii][n.x]        # final abundance values by SSMU and trial - winter season
#             Yall[ii]<-Yt                
#             Ys<-ttt[[m]]$N[[i]][,j,ii][(n.x-1)]    # final abundance values by SSMU and trial - summer season
#             Yall[ii]<-Ys           
#            
### for 30 year averages ######            
            Yt<-ttt[[m]]$N[[i]][,j,ii][(n.x-60):n.x] # pull out the last 30 years of abundances (60 time steps)
            # Yt<-Yt[seq(from=1, by=2, to=60)]# subset just the summer values
              Yt<-Yt[seq(from=2, by=2, to=60)]# subset just the winter values
            Yall[ii]<-mean(Yt)# average abundance during last 30 years
################################
          }
#           out_r[,m]<-r_base
          out_N[m,]<-Yall
          
        }
        # now get the impacts on abundance of each
        base<-out_N[1,]
        sim1<-((out_N[2,])/base) # effect of ggp only
        sim2<-((out_N[3,])/base) # effect of fishing only
        sim3<-((out_N[4,])/base) # effect of fishing and ggp effect
        
        # Impact of GGP on abundance relative to impact of fishing alone   ESK 27 Oct 2017
        # sim4<-(sim3/sim2) # Effect of ggp+fishing proportional to impact of fishing alone
        
        biomass<-c(sim1, sim2, sim3)
        
        # now output the data
        out.m<-data.frame(Spp=rep(SPP[i], 3*n.trials), SSMU=rep(j, 3*n.trials), 
                          Scenario=rep(SCENARIO[k], 3*n.trials),  Simulation=rep(SIM, each=n.trials), 
                          Trial=rep(1:n.trials, 3), Movement=rep(MOVE.SCENARIO[k],3*n.trials), 
                          Stable=rep(STABLE.SCENARIO[k], 3*n.trials), RCP=rep(RCP[k], 3*n.trials), 
                          Biomass=biomass)
        out<-rbind(out, out.m)
      }
    }
  }
  # remove first row of NA data from out.
  out<-out[-1,]
  for(ii in 1:5){
    # create a lattice plot of r organized by simulation type and and species
    out.spp<-out[out$Spp==SPP[ii],]
    out.spp$SSMUf<-as.factor(out.spp$SSMU)
    #out.spp$Marginal_r<-ifelse(is.na(out.spp$Marginal_r),0, out.spp$Marginal_r)
    windows()
    MAIN=SPP[ii]
    print(bwplot(Biomass~SSMUf|Simulation*RCP, data=out.spp, strip=TRUE, cex=0.75, pch="|", 
      col="black", fill="gray", box.width=0.9, main=MAIN, ylim=c(0, 1.5), xlab="SSMU", ylab="Change in Abundance", 
      scales=list(alternating=FALSE), 
    panel=function(...){
      panel.abline(h=0, col="black", lty=1, lwd=1)
      panel.abline(h=0.75, col="red", lty=1, lwd=1)
      panel.abline(h=1.0, col="green", lty=1, lwd=1)
      panel.bwplot(...)
    }))
    #windows()
    #MAIN=SPP[ii]
    #print(bwplot(Total_r~SSMUf|Simulation*RCP, data=out.spp, strip=TRUE, cex=0.75, pch="|", col="black", fill="gray", box.width=0.9, main=MAIN, xlab="SSMU", ylab="Total r", scales=list(alternating=FALSE), 
    #      panel=function(...){
    #        panel.abline(h=0, col="black", lty=1, lwd=1)
    #        panel.bwplot(...)
    #      }))
    if(MAP){
      # formate out.m for use in a mapping exercies, where a single value to represent each ssmu is necessary
      Biom<-data.frame(SSMU=c("APPA", "APW","APDPW","APDPE","APBSW", "APBSE","APEI","APE","SOPA","SOW","SONE","SOSE","SGPA","SGW","SGE"))
      out.ggp<-out.spp[out.spp$Simulation==MARGIN,]  #make sure to set this in call
      Biom<-cbind(Biom, tapply(out.ggp$Biomass, list(out.ggp$SSMU, out.ggp$RCP), median.func))
      # for whales, fill in coastal SSMUs with marginal for respective pelagic areas
      print(MAIN)
      # if(MAIN=="whales"){   # Use this script if filling in coastal SSMUs with pelagic data
        #   Biom[2:8,2]<-Biom[1,2]
        #   Biom[2:8,3]<-Biom[1,3]
        #   Biom[10:12,2]<-Biom[4,2]
        #   Biom[10:12,3]<-Biom[4,3]
      # }

      bt.data<-data.frame(SSMU=c("BT1", "BT2", "BT3"), RCP2.6=rep(NA, 3), RCP8.5=rep(NA, 3))
      Biom<-rbind(Biom, bt.data)
      plot_basic_polygons(SPP=MAIN, Nyears=n.x/2, r.data=Biom)
    }
  }
  out
}