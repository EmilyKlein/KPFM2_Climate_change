get_stats<-function(krill_marg, pred_marg, SSMU=FALSE){
  #   Function to pull out the mean proportion of base case - ESK 11 JAN 2017
  #   Input is output of krill biomass (1) and predator abundance (2) from function get_abund
  #   User sets whether to get output by SSMU  
  
  
  ## Create matrix to hold total krill results
  krill_stats <- matrix(0,6,3)  
  nms <-c("GGP", "Fish", "GGP_Fish")
  colnames(krill_stats) <- nms
  rownames(krill_stats) <- c("mean", "median", "var", "sd", "quant25", "quant75")
  
  
### KRILL BIOMASS IS FIRST INPUT ###
  #Subet out the RCP scenario
  krill_margB <- subset(krill_marg, RCP=="RCP8.5")
  
  #Subset out by simulation, in this case GGP, Fishing, and GGP + Fishing
  GGP_out<-krill_margB$Biomass[which(krill_margB$Simulation=="GGP")]
  Fish_out<-krill_margB$Biomass[which(krill_margB$Simulation=="Fishing")]
  GGPF_out<-krill_margB$Biomass[which(krill_margB$Simulation=="GGP_Fishing")]
  
  # Get median, mean, and variance for each simulation
  # GGP 
  krill_stats[1,1]<- round(mean(GGP_out),3)
  krill_stats[2,1]<- round(median(GGP_out),3)
  krill_stats[3,1]<- round(var(GGP_out),3)
  krill_stats[4,1]<- round(sd(GGP_out),3)
  qs <- quantile(GGP_out)
  krill_stats[5,1]<- round(qs[2], 3)
  krill_stats[6,1]<- round(qs[4], 3)
  
  # Fishing
  krill_stats[1,2]<- round(mean(Fish_out),3)
  krill_stats[2,2]<- round(median(Fish_out),3)
  krill_stats[3,2]<- round(var(Fish_out),3)
  krill_stats[4,2]<- round(sd(Fish_out),3)
  qs <- quantile(Fish_out)
  krill_stats[5,2]<- round(qs[2], 3)
  krill_stats[6,2]<- round(qs[4], 3)
 
  # GGP_Fishing
  krill_stats[1,3]<- round(mean(GGPF_out),3)
  krill_stats[2,3]<- round(median(GGPF_out),3)
  krill_stats[3,3]<- round(var(GGPF_out),3)
  krill_stats[4,3]<- round(sd(GGPF_out),3)
  qs <- quantile(GGPF_out)
  krill_stats[5,3]<- round(qs[2], 3)
  krill_stats[6,3]<- round(qs[4], 3)
  
  assign("krill_stats", krill_stats, .GlobalEnv)  

  
#### PREDATOR ABUNDANCES NEXT ####
  
  ## Create matrix to hold total predator results
  pred_stats <- matrix(0,24,3)  
  colnames(pred_stats) <- nms
  rownames(pred_stats) <- c("pengs_mean", "pengs_median", "pengs_var", "pengs_sd", "pengs_quant25","pengs_quant75",
                            "seals_mean", "seals_median", "seals_var", "seals_sd", "seals_quant25","seals_quant75",
                            "whales_mean", "whales_median", "whales_var", "whales_sd","whales_quant25",
                            "whales_quant75", "fish_mean", "fish_median","fish_var", "fish_sd", "fish_quant25","fish_quant75")

### subset out RCP scenario
  pred_margB <- subset(pred_marg, RCP=="RCP8.5")
    
  #subset by predators
  zz1 <- subset(pred_margB, Spp=="pengs")
  zz2 <- subset(pred_margB, Spp=="seals")
  zz3 <- subset(pred_margB, Spp=="whales")
  zz4 <- subset(pred_margB, Spp=="fish")
  
  
### PENGUINS ###
  GGP_out_pengs<-zz1$Biomass[which(zz1$Simulation=="GGP")]
  Fish_out_pengs<-zz1$Biomass[which(zz1$Simulation=="Fishing")]
  GGPF_out_pengs<-zz1$Biomass[which(zz1$Simulation=="GGP_Fishing")]
  
  # Get median, mean, and variance for each simulation
  # GGP 
  GGP_out_pengs <-  GGP_out_pengs[!is.na(GGP_out_pengs)]   # remove NAs
  pred_stats[1,1]<- round(mean(GGP_out_pengs),3)
  pred_stats[2,1]<- round(median(GGP_out_pengs),3)
  pred_stats[3,1]<- round(var(GGP_out_pengs),3)
  pred_stats[4,1]<- round(sd(GGP_out_pengs),3)
  qs <- quantile(GGP_out_pengs)
  pred_stats[5,1]<- round(qs[2], 3)
  pred_stats[6,1]<- round(qs[4], 3)
  
  # Fishing
  Fish_out_pengs <-  Fish_out_pengs[!is.na(Fish_out_pengs)]   # remove NAs
  pred_stats[1,2]<- round(mean(Fish_out_pengs),3)
  pred_stats[2,2]<- round(median(Fish_out_pengs),3)
  pred_stats[3,2]<- round(var(Fish_out_pengs),3)
  pred_stats[4,2]<- round(sd(Fish_out_pengs),3)
  qs <- quantile(Fish_out_pengs)
  pred_stats[5,2]<- round(qs[2], 3)
  pred_stats[6,2]<- round(qs[4], 3)
  
  # GGP_Fishing
  GGPF_out_pengs <-  GGPF_out_pengs[!is.na(GGPF_out_pengs)]   # remove NAs
  pred_stats[1,3]<- round(mean(GGPF_out_pengs),3)
  pred_stats[2,3]<- round(median(GGPF_out_pengs),3)
  pred_stats[3,3]<- round(var(GGPF_out_pengs),3)
  pred_stats[4,3]<- round(sd(GGPF_out_pengs),3)
  qs <- quantile(GGPF_out_pengs)
  pred_stats[5,3]<- round(qs[2], 3)
  pred_stats[6,3]<- round(qs[4], 3)
  
  
### SEALS ###
  GGP_out_seals<-zz2$Biomass[which(zz1$Simulation=="GGP")]
  Fish_out_seals<-zz2$Biomass[which(zz1$Simulation=="Fishing")]
  GGPF_out_seals<-zz2$Biomass[which(zz1$Simulation=="GGP_Fishing")]
  # GGP 
  GGP_out_seals <-  GGP_out_seals[!is.na(GGP_out_seals)]   # remove NAs
  pred_stats[7,1]<- round(mean(GGP_out_seals),3)
  pred_stats[8, 1]<- round(median(GGP_out_seals),3)
  pred_stats[9,1]<- round(var(GGP_out_seals),3)
  pred_stats[10,1]<- round(sd(GGP_out_seals),3)
  qs <- quantile(GGP_out_seals)
  pred_stats[11,1]<- round(qs[2], 3)
  pred_stats[12,1]<- round(qs[4], 3)
  
  # Fishing
  Fish_out_seals <-  Fish_out_seals[!is.na(Fish_out_seals)]   # remove NAs
  pred_stats[7,2]<- round(mean(Fish_out_seals),3)
  pred_stats[8,2]<- round(median(Fish_out_seals),3)
  pred_stats[9,2]<- round(var(Fish_out_seals),3)
  pred_stats[10,2]<- round(sd(Fish_out_seals),3)
  qs <- quantile(Fish_out_seals)
  pred_stats[11,2]<- round(qs[2], 3)
  pred_stats[12,2]<- round(qs[4], 3)
  
  # GGP_Fishing
  GGPF_out_seals <-  GGPF_out_seals[!is.na(GGPF_out_seals)]   # remove NAs
  pred_stats[7,3]<- round(mean(GGPF_out_seals),3)
  pred_stats[8,3]<- round(median(GGPF_out_seals),3)
  pred_stats[9,3]<- round(var(GGPF_out_seals),3)
  pred_stats[10,3]<- round(sd(GGPF_out_seals),3)
  qs <- quantile(GGPF_out_seals)
  pred_stats[11,3]<- round(qs[2], 3)
  pred_stats[12,3]<- round(qs[4], 3)
  
  
### WHALES ### 
  GGP_out_whales<-zz3$Biomass[which(zz1$Simulation=="GGP")]
  Fish_out_whales<-zz3$Biomass[which(zz1$Simulation=="Fishing")]
  GGPF_out_whales<-zz3$Biomass[which(zz1$Simulation=="GGP_Fishing")]
  
  # GGP 
  GGP_out_whales <-  GGP_out_whales[!is.na(GGP_out_whales)]   # remove NAs
  pred_stats[13,1]<- round(mean(GGP_out_whales),3)
  pred_stats[14,1]<- round(median(GGP_out_whales),3)
  pred_stats[15,1]<- round(var(GGP_out_whales),3)
  pred_stats[16,1]<- round(sd(GGP_out_whales),3)
  qs <- quantile(GGP_out_whales)
  pred_stats[17,1]<- round(qs[2], 3)
  pred_stats[18,1]<- round(qs[4], 3)
  
  # Fishing
  Fish_out_whales <-  Fish_out_whales[!is.na(Fish_out_whales)]   # remove NAs
  pred_stats[13,2]<- round(mean(Fish_out_whales),3)
  pred_stats[14,2]<- round(median(Fish_out_whales),3)
  pred_stats[15,2]<- round(var(Fish_out_whales),3)
  pred_stats[16,2]<- round(sd(Fish_out_whales),3)
  qs <- quantile(Fish_out_whales)
  pred_stats[17,2]<- round(qs[2], 3)
  pred_stats[18,2]<- round(qs[4], 3)
  
  # GGP_Fishing
  GGPF_out_whales <-  GGPF_out_whales[!is.na(GGPF_out_whales)]   # remove NAs
  pred_stats[13,3]<- round(mean(GGPF_out_whales),3)
  pred_stats[14,3]<- round(median(GGPF_out_whales),3)
  pred_stats[15,3]<- round(var(GGPF_out_whales),3)
  pred_stats[16,3]<- round(sd(GGPF_out_whales),3)
  qs <- quantile(GGPF_out_whales)
  pred_stats[17,3]<- round(qs[2], 3)
  pred_stats[18,3]<- round(qs[4], 3)
  
  
### FISH ###
  GGP_out_fish<-zz4$Biomass[which(zz1$Simulation=="GGP")]
  Fish_out_fish<-zz4$Biomass[which(zz1$Simulation=="Fishing")]
  GGPF_out_fish<-zz4$Biomass[which(zz1$Simulation=="GGP_Fishing")]
  
  # GGP 
  GGP_out_fish <-  GGP_out_fish[!is.na(GGP_out_fish)]   # remove NAs
  pred_stats[19,1]<- round(mean(GGP_out_fish),3)
  pred_stats[20,1]<- round(median(GGP_out_fish),3)
  pred_stats[21,1]<- round(var(GGP_out_fish),3)
  pred_stats[22,1]<- round(sd(GGP_out_fish),3)
  qs <- quantile(GGP_out_fish)
  pred_stats[23,1]<- round(qs[2], 3)
  pred_stats[24,1]<- round(qs[4], 3)
  
  # Fishing
  Fish_out_fish <-  Fish_out_fish[!is.na(Fish_out_fish)]   # remove NAs
  pred_stats[19,2]<- round(mean(Fish_out_fish),3)
  pred_stats[20,2]<- round(median(Fish_out_fish),3)
  pred_stats[21,2]<- round(var(Fish_out_fish),3)
  pred_stats[22,2]<- round(sd(Fish_out_fish),3)
  qs <- quantile(Fish_out_fish)
  pred_stats[23,2]<- round(qs[2], 3)
  pred_stats[24,2]<- round(qs[4], 3)
  
  # GGP_Fishing
  GGPF_out_fish <-  GGPF_out_fish[!is.na(GGPF_out_fish)]   # remove NAs
  pred_stats[19,3]<- round(mean(GGPF_out_fish),3)
  pred_stats[20,3]<- round(median(GGPF_out_fish),3)
  pred_stats[21,3]<- round(var(GGPF_out_fish),3)
  pred_stats[22,3]<- round(sd(GGPF_out_fish),3)
  qs <- quantile(GGPF_out_fish)
  pred_stats[23,3]<- round(qs[2], 3)
  pred_stats[24,3]<- round(qs[4], 3)
  
  assign("pred_stats", pred_stats, .GlobalEnv)  
  
  
##### FOR STATISTICS BY SSMU ######
  
  if(SSMU){
    
    ## Create new matrix to hold results
    stats_SSMU <- matrix(NA,15,7)  
    nms <-c("SSMU", "mean", "median", "var", "sd", "q25", "q75")
    colnames(stats_SSMU) <- nms
    stats_SSMU[,1] <- c(1:15)
  
    
### KRILL BIOMASS FIRST ###
    
    #Note: have to re-subset by simulation to preserve headings, then by biomass
    krill_SSMU_GGP<-subset(krill_margB, Simulation=="GGP")
    krill_SSMU_Fish<-subset(krill_margB, Simulation=="Fishing")
    krill_SSMU_GGPF<-subset(krill_margB, Simulation=="GGP_Fishing")
    
    # GGP 
    for (i in 1:15){
      k_ssmu <- krill_SSMU_GGP$Biomass[which(krill_SSMU_GGP$SSMU==i)]
      stats_SSMU[i,2]<- round(mean(k_ssmu),3)
      stats_SSMU[i,3]<- round(median(k_ssmu),3)
      stats_SSMU[i,4]<- round(var(k_ssmu),3)
      stats_SSMU[i,5]<- round(sd(k_ssmu),3)
      qs <- quantile(k_ssmu)
      stats_SSMU[i,6]<- round(qs[2], 3)
      stats_SSMU[i,7]<- round(qs[4], 3)
    }
    assign("krill_SSMU_GGPstats", stats_SSMU, .GlobalEnv)  
  
    # Fishing
    for (i in 1:15){
      k_ssmu <- krill_SSMU_Fish$Biomass[which(krill_SSMU_Fish$SSMU==i)]
      stats_SSMU[i,2]<- round(mean(k_ssmu),3)
      stats_SSMU[i,3]<- round(median(k_ssmu),3)
      stats_SSMU[i,4]<- round(var(k_ssmu),3)
      stats_SSMU[i,5]<- round(sd(k_ssmu),3)
      qs <- quantile(k_ssmu)
      stats_SSMU[i,6]<- round(qs[2], 3)
      stats_SSMU[i,7]<- round(qs[4], 3)
    }
    assign("krill_SSMU_Fishstats", stats_SSMU, .GlobalEnv)  
    
    # GGP_Fishing
    for (i in 1:15){
      k_ssmu <- krill_SSMU_GGPF$Biomass[which(krill_SSMU_GGPF$SSMU==i)]
      stats_SSMU[i,2]<- round(mean(k_ssmu),3)
      stats_SSMU[i,3]<- round(median(k_ssmu),3)
      stats_SSMU[i,4]<- round(var(k_ssmu),3)
      stats_SSMU[i,5]<- round(sd(k_ssmu),3)
      qs <- quantile(k_ssmu)
      stats_SSMU[i,6]<- round(qs[2], 3)
      stats_SSMU[i,7]<- round(qs[4], 3)
    }
    assign("krill_SSMU_GGPFstats", stats_SSMU, .GlobalEnv)  
  
## PENGUINS
    pengs_SSMU_GGP<-subset(zz1, Simulation=="GGP")
    pengs_SSMU_Fish<-subset(zz1, Simulation=="Fishing")
    pengs_SSMU_GGPF<-subset(zz1, Simulation=="GGP_Fishing")
    
    # GGP 
    for (i in 1:15){
      p_ssmu <- pengs_SSMU_GGP$Biomass[which(pengs_SSMU_GGP$SSMU==i)]
      p_ssmu <- p_ssmu[!is.na(p_ssmu)]   # remove NAs
      stats_SSMU[i,2]<- round(mean(p_ssmu),3)
      stats_SSMU[i,3]<- round(median(p_ssmu),3)
      stats_SSMU[i,4]<- round(var(p_ssmu),3)
      stats_SSMU[i,5]<- round(sd(p_ssmu),3)
      qs <- quantile(p_ssmu)
      stats_SSMU[i,6]<- round(qs[2], 3)
      stats_SSMU[i,7]<- round(qs[4], 3)
    }
    assign("pengs_SSMU_GGPstats", stats_SSMU, .GlobalEnv)  
    
    # Fishing
    for (i in 1:15){
      p_ssmu <- pengs_SSMU_Fish$Biomass[which(pengs_SSMU_Fish$SSMU==i)]
      p_ssmu <- p_ssmu[!is.na(p_ssmu)]   # remove NAs
      stats_SSMU[i,2]<- round(mean(p_ssmu),3)
      stats_SSMU[i,3]<- round(median(p_ssmu),3)
      stats_SSMU[i,4]<- round(var(p_ssmu),3)
      stats_SSMU[i,5]<- round(sd(p_ssmu),3)
      qs <- quantile(p_ssmu)
      stats_SSMU[i,6]<- round(qs[2], 3)
      stats_SSMU[i,7]<- round(qs[4], 3)
    }
    assign("pengs_SSMU_Fishstats", stats_SSMU, .GlobalEnv)  
    
    # GGP_Fishing
    for (i in 1:15){
      p_ssmu <- pengs_SSMU_GGPF$Biomass[which(pengs_SSMU_GGPF$SSMU==i)]
      p_ssmu <- p_ssmu[!is.na(p_ssmu)]   # remove NAs
      stats_SSMU[i,2]<- round(mean(p_ssmu),3)
      stats_SSMU[i,3]<- round(median(p_ssmu),3)
      stats_SSMU[i,4]<- round(var(p_ssmu),3)
      stats_SSMU[i,5]<- round(sd(p_ssmu),3)
      qs <- quantile(p_ssmu)
      stats_SSMU[i,6]<- round(qs[2], 3)
      stats_SSMU[i,7]<- round(qs[4], 3)
    }
    assign("pengs_SSMU_GGPFstats", stats_SSMU, .GlobalEnv) 
    
### SEALS
    seals_SSMU_GGP<-subset(zz2, Simulation=="GGP")
    seals_SSMU_Fish<-subset(zz2, Simulation=="Fishing")
    seals_SSMU_GGPF<-subset(zz2, Simulation=="GGP_Fishing")
    
    # GGP 
    for (i in 1:15){
      s_ssmu <- seals_SSMU_GGP$Biomass[which(seals_SSMU_GGP$SSMU==i)]
      s_ssmu <- s_ssmu[!is.na(s_ssmu)]   # remove NAs
      stats_SSMU[i,2]<- round(mean(s_ssmu),3)
      stats_SSMU[i,3]<- round(median(s_ssmu),3)
      stats_SSMU[i,4]<- round(var(s_ssmu),3)
      stats_SSMU[i,5]<- round(sd(s_ssmu),3)
      qs <- quantile(s_ssmu)
      stats_SSMU[i,6]<- round(qs[2], 3)
      stats_SSMU[i,7]<- round(qs[4], 3)
    }
    assign("seals_SSMU_GGPstats", stats_SSMU, .GlobalEnv)  
    
    
    # Fishing
    for (i in 1:15){
      s_ssmu <- seals_SSMU_Fish$Biomass[which(seals_SSMU_Fish$SSMU==i)]
      s_ssmu <- s_ssmu[!is.na(s_ssmu)]   # remove NAs
      stats_SSMU[i,2]<- round(mean(s_ssmu),3)
      stats_SSMU[i,3]<- round(median(s_ssmu),3)
      stats_SSMU[i,4]<- round(var(s_ssmu),3)
      stats_SSMU[i,5]<- round(sd(s_ssmu),3)
      qs <- quantile(s_ssmu)
      stats_SSMU[i,6]<- round(qs[2], 3)
      stats_SSMU[i,7]<- round(qs[4], 3)
    }
    assign("seals_SSMU_Fishstats", stats_SSMU, .GlobalEnv)  
    
    # GGP_Fishing
    for (i in 1:15){
      s_ssmu <- seals_SSMU_GGPF$Biomass[which(seals_SSMU_GGPF$SSMU==i)]
      s_ssmu <- s_ssmu[!is.na(s_ssmu)]   # remove NAs
      stats_SSMU[i,2]<- round(mean(s_ssmu),3)
      stats_SSMU[i,3]<- round(median(s_ssmu),3)
      stats_SSMU[i,4]<- round(var(s_ssmu),3)
      stats_SSMU[i,5]<- round(sd(s_ssmu),3)
      qs <- quantile(s_ssmu)
      stats_SSMU[i,6]<- round(qs[2], 3)
      stats_SSMU[i,7]<- round(qs[4], 3)
    }
    assign("seals_SSMU_GGPFstats", stats_SSMU, .GlobalEnv) 
    
### WHALES
    whales_SSMU_GGP<-subset(zz3, Simulation=="GGP")
    whales_SSMU_Fish<-subset(zz3, Simulation=="Fishing")
    whales_SSMU_GGPF<-subset(zz3, Simulation=="GGP_Fishing")
    
    # GGP 
    for (i in 1:15){
      w_ssmu <- whales_SSMU_GGP$Biomass[which(whales_SSMU_GGP$SSMU==i)]
      w_ssmu <- w_ssmu[!is.na(w_ssmu)]   # remove NAs
      stats_SSMU[i,2]<- round(mean(w_ssmu),3)
      stats_SSMU[i,3]<- round(median(w_ssmu),3)
      stats_SSMU[i,4]<- round(var(w_ssmu),3)
      stats_SSMU[i,5]<- round(sd(w_ssmu),3)
      qs <- quantile(w_ssmu)
      stats_SSMU[i,6]<- round(qs[2], 3)
      stats_SSMU[i,7]<- round(qs[4], 3)
    }
    assign("whales_SSMU_GGPstats", stats_SSMU, .GlobalEnv)  
    
    # Fishing
    for (i in 1:15){
      w_ssmu <- whales_SSMU_Fish$Biomass[which(whales_SSMU_Fish$SSMU==i)]
      w_ssmu <- w_ssmu[!is.na(w_ssmu)]   # remove NAs
      stats_SSMU[i,2]<- round(mean(w_ssmu),3)
      stats_SSMU[i,3]<- round(median(w_ssmu),3)
      stats_SSMU[i,4]<- round(var(w_ssmu),3)
      stats_SSMU[i,5]<- round(sd(w_ssmu),3)
      qs <- quantile(w_ssmu)
      stats_SSMU[i,6]<- round(qs[2], 3)
      stats_SSMU[i,7]<- round(qs[4], 3)
    }
    assign("whales_SSMU_Fishstats", stats_SSMU, .GlobalEnv)  
    
    # GGP_Fishing
    for (i in 1:15){
      w_ssmu <- whales_SSMU_GGPF$Biomass[which(whales_SSMU_GGPF$SSMU==i)]
      w_ssmu <- w_ssmu[!is.na(w_ssmu)]   # remove NAs
      stats_SSMU[i,2]<- round(mean(w_ssmu),3)
      stats_SSMU[i,3]<- round(median(w_ssmu),3)
      stats_SSMU[i,4]<- round(var(w_ssmu),3)
      stats_SSMU[i,5]<- round(sd(w_ssmu),3)
      qs <- quantile(w_ssmu)
      stats_SSMU[i,6]<- round(qs[2], 3)
      stats_SSMU[i,7]<- round(qs[4], 3)
    }
    assign("whales_SSMU_GGPFstats", stats_SSMU, .GlobalEnv) 
    
### FISH
    fish_SSMU_GGP<-subset(zz4, Simulation=="GGP")
    fish_SSMU_Fish<-subset(zz4, Simulation=="Fishing")
    fish_SSMU_GGPF<-subset(zz4, Simulation=="GGP_Fishing")
    
    # GGP 
    for (i in 1:15){
      f_ssmu <- fish_SSMU_GGP$Biomass[which(fish_SSMU_GGP$SSMU==i)]
      f_ssmu <- f_ssmu[!is.na(f_ssmu)]   # remove NAs
      stats_SSMU[i,2]<- round(mean(f_ssmu),3)
      stats_SSMU[i,3]<- round(median(f_ssmu),3)
      stats_SSMU[i,4]<- round(var(f_ssmu),3)
      stats_SSMU[i,5]<- round(sd(f_ssmu),3)
      qs <- quantile(f_ssmu)
      stats_SSMU[i,6]<- round(qs[2], 3)
      stats_SSMU[i,7]<- round(qs[4], 3)
    }
    assign("fish_SSMU_GGPstats", stats_SSMU, .GlobalEnv)  
    
    # Fishing
    for (i in 1:15){
      f_ssmu <- fish_SSMU_Fish$Biomass[which(fish_SSMU_Fish$SSMU==i)]
      f_ssmu <- f_ssmu[!is.na(f_ssmu)]   # remove NAs
      stats_SSMU[i,2]<- round(mean(f_ssmu),3)
      stats_SSMU[i,3]<- round(median(f_ssmu),3)
      stats_SSMU[i,4]<- round(var(f_ssmu),3)
      stats_SSMU[i,5]<- round(sd(f_ssmu),3)
      qs <- quantile(f_ssmu)
      stats_SSMU[i,6]<- round(qs[2], 3)
      stats_SSMU[i,7]<- round(qs[4], 3)
    }
    assign("fish_SSMU_Fishstats", stats_SSMU, .GlobalEnv)  
    
    # GGP_Fishing
    for (i in 1:15){
      f_ssmu <- fish_SSMU_GGPF$Biomass[which(fish_SSMU_GGPF$SSMU==i)]
      f_ssmu <- f_ssmu[!is.na(f_ssmu)]   # remove NAs
      stats_SSMU[i,2]<- round(mean(f_ssmu),3)
      stats_SSMU[i,3]<- round(median(f_ssmu),3)
      stats_SSMU[i,4]<- round(var(f_ssmu),3)
      stats_SSMU[i,5]<- round(sd(f_ssmu),3)
      qs <- quantile(f_ssmu)
      stats_SSMU[i,6]<- round(qs[2], 3)
      stats_SSMU[i,7]<- round(qs[4], 3)
    }
    assign("fish_SSMU_GGPFstats", stats_SSMU, .GlobalEnv) 
  }
}
