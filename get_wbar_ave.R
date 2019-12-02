get_wbar_ave<-function(tt=mlt_h_f1_mc_70){
# Function for pulling out average wbar across trials
# NOTE: This has not yet been updated to  run over all parameterizations, so it needs to be updated each time  
# ESK March 2017
#  
  n.y<-tt[[1]]$setup$ntimes  #186 2 seasons per year
  n.x<-tt[[1]]$setup$nareas   #18 15 SSMUs + 3 bathtubs
  out_wbar_TS<-matrix(0, nrow=n.y, ncol=n.x)      # matrix for time series of wbar averages across trials
  out_wbar_ave <- matrix(0, nrow=15, ncol=3)  # DF for final wbar, averaged over last 30 years 
  colnames(out_wbar_ave) <- c("SSMU", "wbar", "stddev")
  for(j in 1:18){       # j indexes the SSMUs
    for(ii in 1:n.y){
      x <- round(mean(tt$GGP$wbar[ii,j,]),6)   # get mean of wbar across trials for each year and SSMU
      out_wbar_TS[ii, j] <- x
    }
  }
  for(j in 1:15){    
    end_wbar<-out_wbar_TS[((n.y-59):n.y),j] # subset out last 30 years (2xseasons)
    # end_wbar<-end_wbar[seq(from=1, by=2, to=60)]  # subset only summer values
    end_wbar<-end_wbar[seq(from=2, by=2, to=60)]    # subset only winter values
    out_wbar_ave[j,2]<-round(mean(end_wbar),3)      # average over last 30 years
    out_wbar_ave[j,3]<-round(sd(end_wbar),3)        # average over last 30 years
    out_wbar_ave[j,1]<-j            
  }
  write.table(out_wbar_ave, "C:/Users/Emily Klein/Desktop/SWFSC/FOOSA/GGP/GGP_out_all_mar2017/mlt_h_f1_mc.txt", sep="\t")
  
}