plot.ss.calendar<-function(ss.object, plot.bars=TRUE, page.layout = c(4, 4), Nylimits = c(-1, 4))
{
	# auxiliary function to plot abundance from single run of predator-prey model
	# and OVERLAY trajectories onto implied abundance changes from the CALENDAR
	# George Watters
	# code last edited 29 February 2008
	# code based on plot.ss.N()
	#
	# ss.object ("single sim object") should have the following components to work with this function:
	# krill, krill.Rage, seals, seals.maxRage, pengs, pengs.maxRage, whales, whales.maxRage, fish, fish.maxRage
  #
  season.vector <- rep(1:ss.object$setup$nseasons, length.out = ss.object$setup$ntimes)
  year.vector <- rep(1:(ss.object$setup$ntimes/ss.object$setup$nseasons),each=ss.object$setup$nseasons)
  time.label<-"year"
  connector <- 1
  keepers <- (season.vector == connector)
  #
  plot.time <- c(0,year.vector[keepers])
  krill.denom <- ss.object$N$krill[connector, ]
  seals.denom <- ss.object$N$seals[connector, ]
  pengs.denom <- ss.object$N$pengs[connector, ]
  whales.denom <- ss.object$N$whales[connector, ]
  #fish.denom <- ss.object$N$fish[connector, ]
  #
  krill.y <- ss.object$N$krill[((ss.object$R$maxRage$krill+1):(ss.object$R$maxRage$krill + ss.object$setup$ntimes))[keepers], ]
  seals.y <- ss.object$N$seals[((ss.object$R$maxRage$seals+1):(ss.object$R$maxRage$seals + ss.object$setup$ntimes))[keepers], ]
  pengs.y <- ss.object$N$pengs[((ss.object$R$maxRage$pengs+1):(ss.object$R$maxRage$pengs + ss.object$setup$ntimes))[keepers], ]
  whales.y <- ss.object$N$whales[((ss.object$R$maxRage$whales+1):(ss.object$R$maxRage$whales + ss.object$setup$ntimes))[keepers], ]
  #fish.y <- ss.object$N$fish[((ss.object$R$maxRage$fish+1):(ss.object$R$maxRage$fish + ss.object$setup$ntimes))[keepers], ]
  #
  title.prefix <- paste("Nsummer",connector,"/Nsummer",connector,"[yr1]",sep="")
  #
  # here are the benchmarks in the calendar
  cal.pengs.midratio <- c(NA,rep(1.66,7),NA,rep(1.66,3),NA,rep(1.00,2))
  # lo and hi midratios for ssmus 14 and 15 are NA because calendar posits no change (and a range is not given)
  cal.pengs.midratio.lo <- c(NA,rep(1.41,7),NA,rep(1.41,3),rep(NA,3))
  cal.pengs.midratio.hi <- c(NA,rep(1.95,7),NA,rep(1.95,3),rep(NA,3))
  # funky addition of 1 is to account for "time 0" in plot.time
  cal.pengs.midtimes <- c(rep(8+1,12),rep(11+1,3))
  cal.pengs.endratio <- c(NA,rep(0.42,7),NA,rep(0.42,3),NA,rep(0.55,2))
  cal.pengs.endratio.lo <- c(NA,rep(0.29,7),NA,rep(0.29,3),NA,rep(0.5,2))
  cal.pengs.endratio.hi <- c(NA,rep(0.59,7),NA,rep(0.59,3),NA,rep(0.6,2))
  # funky subtraction of 1 is to account for "time 0" in plot.time
  cal.pengs.endtimes <- rep(length(plot.time)-1,15)
  #
  cal.seals.midratio <- c(rep(NA,2),rep(29.5214,2),rep(NA,2),29.5214,rep(NA,6),rep(7.286088411,2))  # OK
  cal.seals.midratio.lo <- c(rep(NA,2),rep(10.83,2),rep(NA,2),10.83,rep(NA,6),rep(5.56,2))  # OK
  cal.seals.midratio.hi <- c(rep(NA,2),rep(32.92,2),rep(NA,2),32.92,rep(NA,6),rep(12.38,2)) # OK   
  cal.seals.midtimes <- c(rep(26+1,13),rep(19+1,2))
  cal.seals.endratio <- c(rep(NA,2),rep(29.5214,2),rep(NA,2),29.5214,rep(NA,6),rep(22.4606,2)) # OK
  cal.seals.endratio.lo <- c(rep(NA,2),rep(10.83,2),rep(NA,2),10.83,rep(NA,6),rep(14.65,2)) # OK
  cal.seals.endratio.hi <- c(rep(NA,2),rep(32.92,2),rep(NA,2),32.92,rep(NA,6),rep(52.01,2)) # OK
  cal.seals.endtimes <- rep(length(plot.time)-1,15)
  #
  cal.whales.endratio <- c(7.54,rep(NA,7),7.79,rep(NA,6))
  # since we use a growth rate in the calendar that is already outside the range of growth rates given by the calendar
  # but the calendar spans 1% per year -- i assume this same range to get lo and hi here
  cal.whales.endratio.lo <- c(6.33,rep(NA,7),6.54,rep(NA,6))
  cal.whales.endratio.hi <- c(8.98,rep(NA,7),9.28,rep(NA,6))
  cal.whales.endtimes <- rep(length(plot.time)-1,15)
  #
  cal.pengs.dataratio <- c(NA,rep(1.05,2),0.92,1.08,1.15,1.66,1.15,NA,rep(1.26,3),NA,rep(1.0,2))
  cal.pengs.datatimes <- c(rep(18,3),21,2,16,8,16,rep(14,4),rep(8,3))+1
  #
  cal.seals.dataratio <- c(rep(NA,2),rep(29.5214,2),rep(NA,2),29.5214,rep(NA,6),rep(8.70,2))
  cal.seals.datatimes <- c(rep(33+1,12), rep(22+1,3))
  #
  cal.whales.dataratio <- c(5.15,rep(NA,7),5.28,rep(NA,6))
  cal.whales.datatimes <- rep(31+1,15)
  windows()
  origpar <- par(no.readonly=TRUE)
	#par(oma = c(0, 0, 2, 0), mfrow = page.layout)
	par(oma = c(4, 2, 4, 3), mar=c(4,4,1,0)+0.1, mgp=c(2,0.75,0), xpd=FALSE, mfrow = page.layout)
	panel.count <- 1
	left.col.panels <- seq(from=1,to=page.layout[1]*page.layout[2],by=page.layout[2])
	bottom.panels <- (1:(page.layout[1]*page.layout[2]))[max(left.col.panels):((page.layout[1]*page.layout[2]))]
	for(i in 1:ss.object$setup$nssmus) {
		if(panel.count > (page.layout[1] * page.layout[2])) {
			panel.count <- 1
			par(origpar)
			windows()
			#par(oma = c(0, 0, 2, 0), mfrow = page.layout)
			par(oma = c(4, 2, 4, 3), mar=c(4,4,1,0)+0.1, mgp=c(2,0.75,0), xpd=FALSE, mfrow = page.layout)
		}
		if(is.element(panel.count,left.col.panels)){ylabel<-"ln(relative abundance)"}else{ylabel<-""}
  	if(is.element(panel.count,bottom.panels)){xlabel<-time.label}else{xlabel<-""}
		plot(plot.time, log(c(krill.denom[i],krill.y[,i])/krill.denom[i]), type = "l", xlab = xlabel, ylab =
			ylabel, ylim = Nylimits,axes=FALSE)
    if(!plot.bars){
      points(cal.pengs.midtimes[i],log(cal.pengs.midratio[i]),col="blue",pch=16,cex=0.8)
      points(cal.pengs.endtimes[i],log(cal.pengs.endratio[i]),col="blue",pch=16,cex=0.8)
      points(cal.seals.midtimes[i],log(cal.seals.midratio[i]),col="red",pch=16,cex=0.8)
      points(cal.seals.endtimes[i],log(cal.seals.endratio[i]),col="red",pch=16,cex=0.8)
      points(cal.whales.endtimes[i],log(cal.whales.endratio[i]),col="green",pch=16,cex=0.8)
    }
    else {
      lines(rep(cal.pengs.midtimes[i],2),log(c(cal.pengs.midratio.lo[i],cal.pengs.midratio.hi[i])),lwd=2,lend=2,col="blue")
      points(cal.pengs.midtimes[i],log(cal.pengs.midratio[i]),col="white",pch=15,cex=1)
      lines(rep(cal.pengs.endtimes[i],2),log(c(cal.pengs.endratio.lo[i],cal.pengs.endratio.hi[i])),lwd=2,lend=2,col="blue")
      points(cal.pengs.endtimes[i],log(cal.pengs.endratio[i]),col="white",pch=15,cex=1)
      lines(rep(cal.seals.midtimes[i],2),log(c(cal.seals.midratio.lo[i],cal.seals.midratio.hi[i])),lwd=2,lend=2,col="red")
      points(cal.seals.midtimes[i],log(cal.seals.midratio[i]),col="white",pch=15,cex=0.5)
      lines(rep(cal.seals.endtimes[i],2),log(c(cal.seals.endratio.lo[i],cal.seals.endratio.hi[i])),lwd=2,lend=2,col="red")
      points(cal.seals.endtimes[i],log(cal.seals.endratio[i]),col="white",pch=15,cex=0.5)
      lines(rep(cal.whales.endtimes[i],2),log(c(cal.whales.endratio.lo[i],cal.whales.endratio.hi[i])),lwd=2,lend=2,col="green")
      points(cal.whales.endtimes[i],log(cal.whales.endratio[i]),col="white",pch=15,cex=1)
    }
    points(cal.pengs.datatimes[i],log(cal.pengs.dataratio[i]),col="blue",pch=4,cex=0.8)
    points(cal.seals.datatimes[i],log(cal.seals.dataratio[i]),col="red",pch=4,cex=0.8)
    points(cal.whales.datatimes[i],log(cal.whales.dataratio[i]),col="green",pch=4,cex=0.8)
    lines(plot.time, log(c(seals.denom[i],seals.y[,i])/seals.denom[i]), col = "red")
		lines(plot.time, log(c(pengs.denom[i],pengs.y[,i])/pengs.denom[i]), col = "blue")
		lines(plot.time, log(c(whales.denom[i],whales.y[,i])/whales.denom[i]), col = "green")
		#lines(plot.time, log(c(fish.denom[i],fish.y[,i])/fish.denom[i]), lty = 4)
    box()
		axis(1,cex.axis=0.8)
		axis(2,cex.axis=0.8)
    title(main=paste("SSMU ", i, sep = ""), outer = FALSE, line = 0.5, cex.main = 0.9)
		panel.count <- panel.count + 1
		if(panel.count > (page.layout[1] * page.layout[2])) {
			#mtext(paste(title.prefix, " -- krill (black), seals (red), pengs (blue), whales (green), fish (dash)",sep=""), outer
			#	 = TRUE, line = 1, cex = 0.75)
      mtext(paste(title.prefix, " -- krill (black), seals (red), pengs (blue), whales (green) -- calendar (bars/dots), data (x)",sep=""), outer
				 = TRUE, line = 1, cex = 0.75)
		}
	}
	#mtext(paste(title.prefix," -- krill (black), seals (red), pengs (blue), whales (green), fish (dash)",sep=""), outer = TRUE, line = 1,
	#	cex = 0.75)
  mtext(paste(title.prefix," -- krill (black), seals (red), pengs (blue), whales (green) -- calendar (bars/dots), data (x)",sep=""), outer = TRUE, line = 1,
		cex = 0.75)
  par(origpar)
}