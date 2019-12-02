plot_basic_polygons<-function(SPP="Hodag", N.Colors=100, Nyears=93, ssmu.data=ssmu.basic.coords[1:1472,], r.data=Marg_r, COLONIES=FALSE, BATHTUBS=TRUE, FULL=FALSE, SAVE.IMAGES=FALSE){
  # uses a dataset for ssmu polygons that has expanded runs along lines of latitude for proper projections
  # N.Colors specifies the color ramp that will be used. It should be on the interval 0 to 100, 
  # and N.Colors can be interpreted as the maximum habitat utilization, where a value of 75 means 75% utilization
  # Specifing the SPP in the function call and the Seasons (directly below) is used for header information in the plot.
  # COLONIES=TRUE plots colony locations other than those used to track animals
  # SCALED=TRUE scales the colors used to represent overlap to maximum values (better saturation) of the predator and fishery activity
  # BATHTUBS=TRUE plots the boundary areas along with all SSMUs
  # FULL=TRUE plots full extent of the bathtubs, as specified. Since these catch-all basins are enormous, setting to FALSE simply chops them to be more appropriate for the plot
  # SAVE.IMAGES=TRUE saves the plots to the folder specified in the code, at the bottom. 
  #
  require("sp")
  require("fields")
  
  # to plot without bathtubs, subset the data here
  if(!BATHTUBS){
    print("Excluding bathtubs from analysis.")
    ssmu.data<-ssmu.data[ssmu.data$SubArea!="Bathtub",]
    bt.rows<-grepl("BT", r.data$SSMU)
    r.data<-r.data[!bt.rows,]
    lats=c(-74,-50)
    longs=c(-70,-30)
    UP=315
    bb<-bbox(cbind(longs, lats))
    XLIM=c(-0.24,0.24)
    YLIM=c(0.26,0.65)
  } else {
    if(FULL){
      lats=c(-78,-45)
      longs=c(-160,-20)
      UP=270
      bb<-bbox(cbind(longs, lats))
      XLIM=c(-0.61,0.61)
      YLIM=c(0.1,0.71)
    } else {
      lats=c(-74,-45)
      longs=c(-80,-30) # don't plot the whole of BT1
      UP=305
      bb<-bbox(cbind(longs, lats))
      XLIM=c(-0.3,0.3)
      YLIM=c(0.25,0.71)
      # remove most-western parts of BT1
      tt.ssmu<-ssmu.data[ssmu.data$SSMU!="BT1",]
      bt1<-ssmu.data[ssmu.data$SSMU=="BT1",]
      bt1<-bt1[bt1$Longitude>=longs[1],]
      ssmu.data<-rbind(tt.ssmu, bt1)
    }
  }
  SEASONS=names(r.data)[-1]
  polys<-unique(ssmu.data$SSMU)
  n.poly<-length(polys)
  #
  # YOU CAN RUN THE FOLLOWING FUNCTION IF YOU NEED TO. A SHORELINE OBJECT WAS ALREADY CREATED HERE
  #shoreline<-close_coastlines(BB=bb)
  #save(shoreline, file="c:/users/jth/desktop/foosa_ggp/r/shoreline")
  #stop()
  #
  #project the coast
  coast.proj<-mapproject(shoreline$Long, shoreline$Lat, projection="lambert", parameters=bb[2,], orientation=c(90,0,UP))
  #
  ssmu.midlat<-tapply(ssmu.data$Latitude, ssmu.data$SSMU, mean)
  ssmu.midlong<-tapply(ssmu.data$Longitude, ssmu.data$SSMU, mean)
  ssmu.id<-tapply(ssmu.data$SSMU_ID, ssmu.data$SSMU, min)
  text.coords<-data.frame(SSMU=names(ssmu.midlat), ID=ssmu.id, Longitude=ssmu.midlong, Latitude=ssmu.midlat)
  # correct a few text point
  tt.ssmus<-c("APPA","SGPA", "SOPA", "APBSE","APW", "SOSE","APDPW", "APDPE", "APEI", "SONE")
  new.longs<-c(-67,-45,-38,-57,-64.25,-45.03249, -61.75, -58.71833, -54.41,-43.9)
  new.lats<-c(-62,-53, -60,-62.40553, -64,-61.05,-62.25,-61.4,-61.5,-60.3)
  for(i in 1:length(tt.ssmus)){
    text.coords$Longitude[text.coords$SSMU==tt.ssmus[i]]<-new.longs[i]
    text.coords$Latitude[text.coords$SSMU==tt.ssmus[i]]<-new.lats[i]
  }
  text.proj<-mapproject(text.coords$Longitude, text.coords$Latitude, projection="lambert", parameters=bb[2,], orientation=c(90,0,UP))
  #
  #set up some color indices for plotting in the polygons
  #color.ex<-rev(terrain.colors(N.Colors+1, 1))
  require("RColorBrewer")
  # RYG<-brewer.pal(11, "RdYlGn")  #call stoplight palette, red-yellow-green
  #stoplight colors with threshold
  RYG<-my_colors <- c("#000000","#000000","#72011b","#A50026","#A50026","#A50026","#D7191C","#D7191C","#D7191C","#FDAE61","#FFFFBF","#EBFFBF","#B4FF99") #alternative, demonstrating all red below a threshold
  ##alternative, demonstrating all red below a threshold: 
  # RYG<-my_colors <- c("#000000","#000000","#72011b","#A50026","#A50026","#A50026","#A50026","#D7191C","#D7191C","#D7191C","#D7191C","#FDAE61","#FFFFBF","#FFFFF4") 
  
  RYGpal<-colorRampPalette(RYG)  #extend palette for 101 colors (basically a range)
  color.ex<-RYGpal(101)
  #color.ex<-rev(tim.colors(n=N.Colors+1))  
### For Marginal r ###
  # set up interval to match marginal_r to a color
  # the interval is based on 100 evently spaced intervals over the full range of Marginal r. 
  # currently, spacing of 0.0025 acheives a decent interval based on a data range -0.18 to 0.03
  # yielding a scale range -0.2, 0.0475
  # if more precision is needed (since the median r values are not as variable, this can be changed)
  # color.index<-seq(from= -0.2, by=0.0025, length.out=100)  # Marginal r, option 1
  # color.index<-seq(from= -0.1, by=0.002, length.out=100)  # Marginal r, option 2
  #
# # Create ranges for marg r data color index using data 
  #   abs.x<-max(abs(c(range(r.data[,2],na.rm=TRUE), range(r.data[,3],na.rm=TRUE))), na.rm=TRUE)
  #   # some math to determine appropriate limits
  #   dec.x<-100000*(round(abs.x,5)-round(abs.x,4))
  #   if(dec.x>0){
  #     abs.x <- round(abs.x,4)+5/100000 # goes to nearest 5 thousandth
  #   } else {
  #     abs.x <- round(abs.x,4) # goes to nearest hundreth
  #   }
  #   # now take the negative and mirror this range with 100 intervals (n=101)
  #   from.x <- -abs.x
  #   int.x<- abs((from.x-abs.x)/100)
  #   color.index<-seq(from=from.x, by=int.x, length.out=100)
  #
### For Abundance - ESK ###
  # Final abundance is a proportion 0 to 1, with threshold at 0.75
  # Therefore, want stoplight colors to end with red at 0.75, for extended palette of 100 colors:
  # Will need to change this if threshold updated, i.e.other than 0.75
  from.x <- 0          # Limit stoplight colors to around the 0.75 threshold, so need to set that range
  int.x <- 0.01  
  color.index<-seq(from=from.x, by=int.x, length.out=100)
#
  n.season<-length(names(r.data))-1
  # produce the map for each season
  for(j in 1:n.season){
    windows()
    layout(matrix(c(1,1,1,1,1,1,1,1,2,2),byrow=TRUE,5,2))
    Season<-SEASONS[j]
    MAIN=paste(SPP, " ", Season, sep="")
    #MAIN=Season
    plot(coast.proj$x, coast.proj$y, type="l", lty=1, lwd=0.25, main=MAIN, col="black", xlim=XLIM, ylim=YLIM,axes=FALSE, xlab="",ylab="")
    #col.vec<-numeric()
    fill.values<-r.data[,j+1]
    proj.list<-list()
    for(i in 1:n.poly){
      poly.name<-polys[i]
      ssmu<-ssmu.data[ssmu.data$SSMU==poly.name,]
      ssmu.proj<-mapproject(ssmu$Longitude, ssmu$Latitude, projection="lambert", parameters=bb[2,], orientation=c(90,0,UP))
      proj.list[[i]]<-ssmu.proj
      which.ssmu<-match(poly.name, r.data[,1])
      #tt.match<-match(round(fill.values[which.ssmu],2), color.index, incomparables=NULL)      
      tt.match<-findInterval(fill.values[which.ssmu], color.index, rightmost.closed=TRUE, all.inside=TRUE)
      #col.vec[i]<-tt.match      
      poly.fill<-color.ex[tt.match]
      # if whales are plotted, remove data from SSMUs 10-12 (I know this is inefficient, I'm being dumb)
      if(SPP=="whales"){
        if(poly.name=="SOW"){
          poly.fill<-"white"
        }
        if(poly.name=="SONE"){
          poly.fill<-"white"
        }
        if(poly.name=="SOSE"){
          poly.fill<-"white"
        }
      }

      # use a slightly different border for bathtubs to highlight areas not in Area 48
      if(grepl("BT", poly.name)){
        polygon(ssmu.proj$x, ssmu.proj$y, col=poly.fill, border="black", lty=2, lwd=3) #if you want BTs different
      } else {
        polygon(ssmu.proj$x, ssmu.proj$y, col=poly.fill, border="black", lwd=1.75)
      }
    }
    #lines(coast.proj$x, coast.proj$y, col="black", lwd=0.25)
    polygon(coast.proj$x, coast.proj$y, col="darkgrey", border=NA)
    map.grid(lim=c(bb[1,], bb[2,]), ny=5, nx=6, pretty=TRUE, col="darkgrey", lwd=0.5, lty=1)  
    Season<-SEASONS[j]
    MAIN=paste(SPP, " ", Season, sep="")
    #main=MAIN
    text(text.proj$x, text.proj$y, labels=text.coords$ID, cex=0.66, adj=c(0.5,0.5))
    box()
    # now add the color bar reference in lower portion of plot window
    x<-seq(from=from.x, by=int.x, length.out=101)   
    y<-rep(0, N.Colors+1)
    # XLAB<-"Marginal growth rate over simulation period"  # marginal r
    XLAB<-"Proportion of abundance without climate change"  # abundance
    plot(x,y, axes=FALSE, pch="", xlab=XLAB, ylab="")
    for(i in 1:(N.Colors+1)){
      points(x[i],y[i], pch=22,cex=3, col=NULL, bg=color.ex[i])
    }
    # add a second axis with points describing percent change
    #props<-seq(from=0, by=0.1, to=2)
    #rprops<-log(props)/Nyears
    # props<-exp(x*Nyears)
    # n.props<-length(props)
    # #points(x=rprops, y=rep(0, n.props), pch="|", cex=1, col="black")
    #axis(1, at=seq(from=0, to=N.Colors, by=10), labels=seq(from= -0.1, by=0.02, length.out=11), tick=FALSE, xlab=XLAB, ylab="")
    axis(1, tick=TRUE, at=seq(from=from.x, by=int.x*10, length.out=11), labels=seq(from= from.x, by=int.x*10, length.out=11))
    # axis(3, tick=TRUE, at=seq(from=from.x, by=int.x*10, length.out=11), labels=round(props,3)[c(1,11,21,31,41,51,61,71,81,91,101)])
    # mtext(side=3, line=3, text="Proportional change in population over simulation period", cex=0.66)
    # 
    #optionally save the graphs as .eps and .jpg files
    if(SAVE.IMAGES){
      filename1<-paste("c:/users/jth/desktop/argos/saved plots/", SPP,"_",SEASONS[j],"_SSMU.pdf", sep="")
      #filename2<-paste("c:/users/jth/desktop/argos/saved plots/", SPP,"_",SEASONS[j],"_SSMU.jpg", sep="")
      dev.copy(device=pdf, filename1)
      dev.off()
      #dev.copy(device=jpeg, filename=filename2, quality=100)
      #dev.off()
      graphics.off()
    }
  }
  # end of file
}