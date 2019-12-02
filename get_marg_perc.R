get_marg_perc<-function(krill_marg, pred_marg, SSMU=FALSE){
#   Function to pull out proportion of simulations below 0.75 - ESK 11 JAN 2017
#   Input is output of krill biomass (1) and predator abundance (2) from function get_abund
#   User sets whether to get output by SSMU  

  ## Create matrix to hold all results
  perc_out <- matrix(0,5,3)  
  nms <-c("GGP", "Fish", "GGP_Fish")
  colnames(perc_out) <- nms
  rownames(perc_out) <- c("krill(B)", "pengs", "seals", "whales", "fish")
#   
#   #KRILL BIOMASS IS FIRST INPUT
  
  #Subet out the RCP scenario
  krill_margB <- subset(krill_marg, RCP=="RCP8.5")

  #Subset out by simulation, in this case GGP, Fishing, and GGP + Fishing
  GGP_out<-krill_margB$Biomass[which(krill_margB$Simulation=="GGP")]
  Fish_out<-krill_margB$Biomass[which(krill_margB$Simulation=="Fishing")]
  GGPF_out<-krill_margB$Biomass[which(krill_margB$Simulation=="GGP_Fishing")]

  #New matrix to combine marginal biomass for all three simulations
  rows <- length(GGPF_out)  # change this to GGP if need the next lines of code to make same length
  # rowsB <- length(Fish_out)
  # rowsC <- length(GGPF_out)
  # Fish_out<-Fish_out[(rowsB-rows):rows]
  # GGPF_out<-GGPF_out[(rowsc-rows):rows]
  all_marg <- matrix(0, rows, 3)
  colnames(all_marg) <- nms
  
  all_marg[,1] <- GGP_out
  all_marg[,2] <- Fish_out
  all_marg[,3] <- GGPF_out
  
  # Calculate the percent of simulations with marginal biomass below 0.75
  for (i in 1:3){
    total <- length(all_marg[,i])
    below <- sum(all_marg[,i]<0.75)
    perc <- below/total
  
  # report as percentages
  # perc <- paste(round(perc*100), "%", sep = "")
    perc <- round(perc, 4)
    perc_out[1,i] <- perc
  }
  
# # PREDATOR ABUNDANCES NEXT
  
  # subset out RCP 8.5
  pred_margB <- subset(pred_marg, RCP=="RCP8.5")
  
  # subset by predators
  zz1 <- subset(pred_margB, Spp=="pengs")
  zz2 <- subset(pred_margB, Spp=="seals")
  zz3 <- subset(pred_margB, Spp=="whales")
  zz4 <- subset(pred_margB, Spp=="fish")

  # PENGUINS
  GGP_out_pengs<-zz1$Biomass[which(zz1$Simulation=="GGP")]
  Fish_out_pengs<-zz1$Biomass[which(zz1$Simulation=="Fishing")]
  GGPF_out_pengs<-zz1$Biomass[which(zz1$Simulation=="GGP_Fishing")]
 
  rows <- length(GGPF_out_pengs) #change this to GGP if need the next lines of code to make same length
  # rowsB <- length(Fish_out)
  # rowsC <- length(GGPF_out)
  # Fish_out<-Fish_out[(rowsB-rows):rows]
  # GGPF_out<-GGPF_out[(rowsc-rows):rows]
  nms <-c("GGP", "Fish", "GGP_Fish")
  all_marg_pengs <- matrix(NA, rows, 3)
  colnames(all_marg_pengs) <- nms
  
  all_marg_pengs[,1] <- GGP_out_pengs
  all_marg_pengs[,2] <- Fish_out_pengs
  all_marg_pengs[,3] <- GGPF_out_pengs
  
  
   for (i in 1:3){
    yy<- all_marg_pengs[,i]
    yy <-  yy[!is.na(yy)]   # remove NAs
    total <- length(yy)
    below <- sum(yy<0.75)
    perc <- below/total

    # report as percentages
    # perc <- paste(round(perc*100), "%", sep = "")
    perc <- round(perc, 4)
    perc_out[2,i] <- perc
  }
  
# SEALS
  GGP_out_seals<-zz2$Biomass[which(zz2$Simulation=="GGP")]
  Fish_out_seals<-zz2$Biomass[which(zz2$Simulation=="Fishing")]
  GGPF_out_seals<-zz2$Biomass[which(zz2$Simulation=="GGP_Fishing")]

  rows <- length(GGPF_out_seals) 
  nms <-c("GGP", "Fish", "GGP_Fish")
  all_marg_seals <- matrix(NA, rows, 3)
  colnames(all_marg_seals) <- nms
  
  all_marg_seals[,1] <- GGP_out_seals
  all_marg_seals[,2] <- Fish_out_seals
  all_marg_seals[,3] <- GGPF_out_seals
  
  
  #Calculate the percent of simulations with marginal biomass below 0.75
  for (i in 1:3){
    yy<- all_marg_seals[,i]
    yy <-  yy[!is.na(yy)]   # remove NAs
    total <- length(yy)
    below <- sum(yy<0.75)
    perc <- below/total

    # report as percentages
    # perc <- paste(round(perc*100), "%", sep = "")
    perc <- round(perc, 4)
    perc_out[3,i] <- perc
  }
  
# WHALES
  GGP_out_whales<-zz3$Biomass[which(zz3$Simulation=="GGP")]
  Fish_out_whales<-zz3$Biomass[which(zz3$Simulation=="Fishing")]
  GGPF_out_whales<-zz3$Biomass[which(zz3$Simulation=="GGP_Fishing")]
  
  rows <- length(GGPF_out_whales)
  nms <-c("GGP", "Fish", "GGP_Fish")
  all_marg_whales <- matrix(NA, rows, 3)
  colnames(all_marg_whales) <- nms
  
  all_marg_whales[,1] <- GGP_out_whales
  all_marg_whales[,2] <- Fish_out_whales
  all_marg_whales[,3] <- GGPF_out_whales
  
  
  #Calculate the percent of simulations with marginal biomass below 0.75
  for (i in 1:3){
    yy<- all_marg_whales[,i]
    yy <-  yy[!is.na(yy)]   # remove NAs
    total <- length(yy)
    below <- sum(yy<0.75)
    perc <- below/total

    # report as percentages
    # perc <- paste(round(perc*100), "%", sep = "")#     
    perc <- round(perc, 4)
    perc_out[4,i] <- perc
  }
  
# FISH  
  GGP_out_fish<-zz4$Biomass[which(zz4$Simulation=="GGP")]
  Fish_out_fish<-zz4$Biomass[which(zz4$Simulation=="Fishing")]
  GGPF_out_fish<-zz4$Biomass[which(zz4$Simulation=="GGP_Fishing")]
  
  rows <- length(GGPF_out_fish)
  nms <-c("GGP", "Fish", "GGP_Fish")
  all_marg_fish <- matrix(NA, rows, 3)
  colnames(all_marg_fish) <- nms
  
  all_marg_fish[,1] <- GGP_out_fish
  all_marg_fish[,2] <- Fish_out_fish
  all_marg_fish[,3] <- GGPF_out_fish
  
  
  #Calculate the percent of simulations with marginal biomass below 0.75
  for (i in 1:3){
    yy<- all_marg_fish[,i]
    yy <-  yy[!is.na(yy)]   # remove NAs
    total <- length(yy)
    below <- sum(yy<0.75) 
    perc <- below/total

    # report as percentages
    # perc <- paste(round(perc*100), "%", sep = "")#     
    perc <- round(perc, 4)
    perc_out[5,i] <- perc
  }
  assign("all_perc", perc_out, .GlobalEnv)  
  
  
## FOR PERCENTAGES BY SSMU ##
  
  if(SSMU==TRUE){
    #code to subset results by SSMU
    
    ## Create new matrix to hold all results
    perc_out_SSMU <- matrix(NA,15,4)  
    nms <-c("SSMU", "GGP", "Fish", "GGP_Fish")
    colnames(perc_out_SSMU) <- nms
    perc_out_SSMU[,1] <- c(1:15)
    
    #KRILL BIOMASS IS FIRST INPUT
    
    #Subset out by simulation, in this case GGP, Fishing, and GGP + Fishing
    krill_SSMU_GGP<-subset(krill_margB, Simulation=="GGP")
    krill_SSMU_Fish<-subset(krill_margB, Simulation=="Fishing")
    krill_SSMU_GGPF<-subset(krill_margB, Simulation=="GGP_Fishing")
    
    for (i in 1:15){
      k_ssmu <- krill_SSMU_GGP$Biomass[which(krill_SSMU_GGP$SSMU==i)]
      total <- length(k_ssmu)
      below <- sum(k_ssmu<0.75)
      perc_ssmu_ggp <- below/total

      # report as percentages
      # perc_ssmu_ggp <- paste(round(perc_ssmu_ggp*100), "%", sep = "")
      perc_ssmu_ggp <- round(perc_ssmu_ggp, 4)
      perc_out_SSMU[i,2] <- perc_ssmu_ggp    
      
      k_ssmu <- krill_SSMU_Fish$Biomass[which(krill_SSMU_Fish$SSMU==i)]
      total <- length(k_ssmu)
      below <- sum(k_ssmu<0.75)
      perc_ssmu_fish <- below/total
      
#       perc_ssmu_fish <- paste(round(perc_ssmu_fish*100), "%", sep = "")
      perc_ssmu_fish <- round(perc_ssmu_fish, 4)
      perc_out_SSMU[i,3] <- perc_ssmu_fish    
      
      k_ssmu <- krill_SSMU_GGPF$Biomass[which(krill_SSMU_GGPF$SSMU==i)]
      total <- length(k_ssmu)
      below <- sum(k_ssmu<0.75)
      perc_ssmu_ggpf <- below/total
      
      # perc_ssmu_ggpf <- paste(round(perc_ssmu_ggpf*100), "%", sep = "")
      perc_ssmu_ggpf <- round(perc_ssmu_ggpf, 4)
      perc_out_SSMU[i,4] <- perc_ssmu_ggpf    
    }
    assign("krill_ssmu_perc", perc_out_SSMU, .GlobalEnv)  
    
    
## PENGUINS
    pengs_SSMU_GGP<-subset(zz1, Simulation=="GGP")
    pengs_SSMU_Fish<-subset(zz1, Simulation=="Fishing")
    pengs_SSMU_GGPF<-subset(zz1, Simulation=="GGP_Fishing")
 
   for (i in 1:15){
      pengs_ssmu <- pengs_SSMU_GGP$Biomass[which(pengs_SSMU_GGP$SSMU==i)]
      total <- length(pengs_ssmu)
      below <- sum(pengs_ssmu<0.75)
      perc_ssmu_ggp <- below/total
      
      # report as percentages
      # perc_ssmu_ggp <- paste(round(perc_ssmu_ggp*100), "%", sep = "")
      perc_ssmu_ggp <- round(perc_ssmu_ggp, 4)
      perc_out_SSMU[i,2] <- perc_ssmu_ggp    
      
      pengs_ssmu <- pengs_SSMU_Fish$Biomass[which(pengs_SSMU_Fish$SSMU==i)]
      total <- length(pengs_ssmu)
      below <- sum(pengs_ssmu<0.75)
      perc_ssmu_fish <- below/total
      
      # perc_ssmu_fish <- paste(round(perc_ssmu_fish*100), "%", sep = "")
      perc_ssmu_fish <- round(perc_ssmu_fish, 4)
      perc_out_SSMU[i,3] <- perc_ssmu_fish    
      
      pengs_ssmu <- pengs_SSMU_GGPF$Biomass[which(pengs_SSMU_GGPF$SSMU==i)]
      total <- length(pengs_ssmu)
      below <- sum(pengs_ssmu<0.75)
      perc_ssmu_ggpf <- below/total
      
      # perc_ssmu_ggpf <- paste(round(perc_ssmu_ggpf*100), "%", sep = "")
      perc_ssmu_ggpf <- round(perc_ssmu_ggpf, 4)
      perc_out_SSMU[i,4] <- perc_ssmu_ggpf    
    }
    assign("pengs_ssmu_perc", perc_out_SSMU, .GlobalEnv)  
    
    
## SEALS
    seals_SSMU_GGP<-subset(zz2, Simulation=="GGP")
    seals_SSMU_Fish<-subset(zz2, Simulation=="Fishing")
    seals_SSMU_GGPF<-subset(zz2, Simulation=="GGP_Fishing")
    
    for (i in 1:15){
      seals_ssmu <- seals_SSMU_GGP$Biomass[which(seals_SSMU_GGP$SSMU==i)]
      total <- length(seals_ssmu)
      below <- sum(seals_ssmu<0.75)
      perc_ssmu_ggp <- below/total
      
      # report as percentages
      # perc_ssmu_ggp <- paste(round(perc_ssmu_ggp*100), "%", sep = "")
      perc_ssmu_ggp <- round(perc_ssmu_ggp, 4)
      perc_out_SSMU[i,2] <- perc_ssmu_ggp    
      
      seals_ssmu <- seals_SSMU_Fish$Biomass[which(seals_SSMU_Fish$SSMU==i)]
      total <- length(seals_ssmu)
      below <- sum(seals_ssmu<0.75)
      perc_ssmu_fish <- below/total
      
      # perc_ssmu_fish <- paste(round(perc_ssmu_fish*100), "%", sep = "")
      perc_ssmu_fish <- round(perc_ssmu_fish, 4)
      perc_out_SSMU[i,3] <- perc_ssmu_fish    
      
      seals_ssmu <- seals_SSMU_GGPF$Biomass[which(seals_SSMU_GGPF$SSMU==i)]
      total <- length(seals_ssmu)
      below <- sum(seals_ssmu<0.75)
      perc_ssmu_ggpf <- below/total
      
      # perc_ssmu_ggpf <- paste(round(perc_ssmu_ggpf*100), "%", sep = "")
      perc_ssmu_ggpf <- round(perc_ssmu_ggpf, 4)
      perc_out_SSMU[i,4] <- perc_ssmu_ggpf    
    }
    assign("seals_ssmu_perc", perc_out_SSMU, .GlobalEnv) 
    
## WHALES
    whales_SSMU_GGP<-subset(zz3, Simulation=="GGP")
    whales_SSMU_Fish<-subset(zz3, Simulation=="Fishing")
    whales_SSMU_GGPF<-subset(zz3, Simulation=="GGP_Fishing")
    
    for (i in 1:15){
      whales_ssmu <- whales_SSMU_GGP$Biomass[which(whales_SSMU_GGP$SSMU==i)]
      total <- length(whales_ssmu)
      below <- sum(whales_ssmu<0.75)
      perc_ssmu_ggp <- below/total
      
      # report as percentages
      # perc_ssmu_ggp <- paste(round(perc_ssmu_ggp*100), "%", sep = "")
      perc_ssmu_ggp <- round(perc_ssmu_ggp, 4)
      perc_out_SSMU[i,2] <- perc_ssmu_ggp    
      
      whales_ssmu <- whales_SSMU_Fish$Biomass[which(whales_SSMU_Fish$SSMU==i)]
      total <- length(whales_ssmu)
      below <- sum(whales_ssmu<0.75)
      perc_ssmu_fish <- below/total
      
      # perc_ssmu_fish <- paste(round(perc_ssmu_fish*100), "%", sep = "")
      perc_ssmu_fish <- round(perc_ssmu_fish, 4)
      perc_out_SSMU[i,3] <- perc_ssmu_fish    
      
      whales_ssmu <- whales_SSMU_GGPF$Biomass[which(whales_SSMU_GGPF$SSMU==i)]
      total <- length(whales_ssmu)
      below <- sum(whales_ssmu<0.75)
      perc_ssmu_ggpf <- below/total
      
      # perc_ssmu_ggpf <- paste(round(perc_ssmu_ggpf*100), "%", sep = "")
      perc_ssmu_ggpf <- round(perc_ssmu_ggpf, 4)
      perc_out_SSMU[i,4] <- perc_ssmu_ggpf    
    }
    assign("whales_ssmu_perc", perc_out_SSMU, .GlobalEnv)  
    
## FISH
    fish_SSMU_GGP<-subset(zz4, Simulation=="GGP")
    fish_SSMU_Fish<-subset(zz4, Simulation=="Fishing")
    fish_SSMU_GGPF<-subset(zz4, Simulation=="GGP_Fishing")
    
    for (i in 1:15){
      ## GGP
      fish_ssmu <- fish_SSMU_GGP$Biomass[which(fish_SSMU_GGP$SSMU==i)]
      total <- length(fish_ssmu)
      below <- sum(fish_ssmu<0.75)
      perc_ssmu_ggp <- below/total
      
      # report as percentages
      # perc_ssmu_ggp <- paste(round(perc_ssmu_ggp*100), "%", sep = "")
      perc_ssmu_ggp <- round(perc_ssmu_ggp, 4)
      perc_out_SSMU[i,2] <- perc_ssmu_ggp    
      
      ## Fishing
      fish_ssmu <- fish_SSMU_Fish$Biomass[which(fish_SSMU_Fish$SSMU==i)]
      total <- length(fish_ssmu)
      below <- sum(fish_ssmu<0.75)
      perc_ssmu_fish <- below/total
      
      # perc_ssmu_fish <- paste(round(perc_ssmu_fish*100), "%", sep = "")
      perc_ssmu_fish <- round(perc_ssmu_fish, 4)
      perc_out_SSMU[i,3] <- perc_ssmu_fish    
      
      ##GGPF
      fish_ssmu <- fish_SSMU_GGPF$Biomass[which(fish_SSMU_GGPF$SSMU==i)]
      total <- length(fish_ssmu)
      below <- sum(fish_ssmu<0.75)
      perc_ssmu_ggpf <- below/total
      
      # perc_ssmu_ggpf <- paste(round(perc_ssmu_ggpf*100), "%", sep = "")
      perc_ssmu_ggpf <- round(perc_ssmu_ggpf, 4)
      perc_out_SSMU[i,4] <- perc_ssmu_ggpf    
    }
    assign("fish_ssmu_perc", perc_out_SSMU, .GlobalEnv)  
    
  }
}


# rm(yy, zz, perc, rows, nms, m, i, GGP_out_pengs, GGP_out_seals, GGPF_out_pengs, GGPF_out_seals, 
#    Fish_out_pengs, Fish_out_seals, below, perc, zz1, zz2, zz3, zz4, perc_out, all_marg_pengs, 
#    all_marg_seals, krill_marg, pred_marg, total, all_marg)