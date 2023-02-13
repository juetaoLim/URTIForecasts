rm(list=ls())
cd <- ""  #change to own directory here
files <- list.files(paste0(cd,"/out/forecast_ouput/"))

#function plots forecasts

forecastPlotter <- function(y,yf,xLab,yLab,stepAhead,legendModLab){
  plot(y=c(min(c(y,yf),na.rm=T),max(c(y,yf),na.rm=T)),
       x=c(0,length(y)),
       col="white",
       xaxt='n',
       xlab=xLab,
       ylab=yLab)
  train_ind <- max(which(is.na(yf)))
  polygon(x=c(-100,train_ind,train_ind,-100),y=c(-9999,-9999,9999,9999),col="azure2",border="white")
  points(y=yf,x=seq(1,length(yf)),col="darkred",pch=20)
  lines(y=y,x=seq(1,length(y)))
  legend(x="topleft",legend=c("Observed",paste0(legendModLab," ",stepAhead," week ahead forecast")),lty=c(1,NA),pch=c(NA,20),col=c("black","darkred"),bty='n',cex=0.8)
  mtext(text="train data",side=1,adj=0.1,padj=-2)
  mtext(text="test data",side=1,adj=0.9,padj=-2)
  axis(side=1,at=seq(0,length(y),by=52),labels=F)
  axis(side=1,at=seq(0,length(y),by=52)+26,labels=seq(12,22,by=1),tick=F)
  box()
  
}

#function plots forecast against actual 


for (j in files){
 load(paste0(cd,"/out/forecast_ouput/",j))
}



for (i in 1:length(`AcuteUpperRespiratoryTractinfections Forecast`)){
  pdf(paste0(cd,"/out/forecast_plots/breakdown/forecastsCombiStepAhead",i,".pdf"),width=8,height=5)
  par(las=1,cex.axis=0.8)
  temp <- `AcuteUpperRespiratoryTractinfections Forecast`[[i]]
  forecastPlotter(y=temp[,6],
                  yf=temp[,5],
                  xLab="Year",
                  yLab="URTI Case Counts",
                  legendModLab="Forecast combination",
                  stepAhead=i)
  dev.off()
  
  pdf(paste0(cd,"/out/forecast_plots/breakdown/forecastsARStepAhead",i,".pdf"),width=8,height=5)
  par(las=1,cex.axis=0.8)
  temp <- `AcuteUpperRespiratoryTractinfections Forecast`[[i]]
  forecastPlotter(y=temp[,6],
                  yf=temp[,1],
                  xLab="Year",
                  yLab="URTI Case Counts",
                  legendModLab="AR (Baseline)",
                  stepAhead=i)
  dev.off()
  
  pdf(paste0(cd,"/out/forecast_plots/breakdown/forecastsLASSOStepAhead",i,".pdf"),width=8,height=5)
  par(las=1,cex.axis=0.8)
  temp <- `AcuteUpperRespiratoryTractinfections Forecast`[[i]]
  forecastPlotter(y=temp[,6],
                  yf=temp[,2],
                  xLab="Year",
                  yLab="URTI Case Counts",
                  legendModLab="LASSO",
                  stepAhead=i)
  dev.off()
  
  pdf(paste0(cd,"/out/forecast_plots/breakdown/forecastsGBMStepAhead",i,".pdf"),width=8,height=5)
  par(las=1,cex.axis=0.8)
  temp <- `AcuteUpperRespiratoryTractinfections Forecast`[[i]]
  forecastPlotter(y=temp[,6],
                  yf=temp[,4],
                  xLab="Year",
                  yLab="URTI Case Counts",
                  legendModLab="GBM",
                  stepAhead=i)
  dev.off()
  
  pdf(paste0(cd,"/out/forecast_plots/breakdown/forecastsNAIVEStepAhead",i,".pdf"),width=8,height=5)
  par(las=1,cex.axis=0.8)
  temp <- `AcuteUpperRespiratoryTractinfections Forecast`[[i]]
  forecastPlotter(y=temp[,6],
                  yf=temp[,143],
                  xLab="Year",
                  yLab="URTI Case Counts",
                  legendModLab="Naive",
                  stepAhead=i)
  dev.off()
  
}




forecastPlotter2 <- function(y,yf,xLab,yLab,stepAhead,panelLab){
  y  <- y/1000
  yf <- yf/1000
  plot(y=c(min(c(y,yf),na.rm=T),max(c(y,yf),na.rm=T)),
       x=c(min(c(y,yf),na.rm=T),max(c(y,yf),na.rm=T)),
       col="white",
       xlab=xLab,
       ylab=yLab)
  abline(a=0,b=1, col = "gray")
  points(y=yf,x=y,col="darkred",pch=20)
  # legend(x="topleft",legend=c("fore",paste0(stepAhead," week ahead forecast")),lty=c(1,NA),pch=c(NA,20),col=c("black","darkred"),bty='n',cex=0.8)
  mtext(text="underpredict",side=1,adj=0.9,padj=-2,cex=0.8)
  mtext(text="overpredict",side=3,adj=0.1,padj=2,cex=0.8)
  mtext(panelLab,side=3,adj=0,cex=0.8)
  box()
  
}

pdf(paste0(cd,"/out/forecast_plots/forecasts2.pdf"),width=10,height=5)
# par(las=1,cex.axis=0.8)
panelName = c("A","B","C","D","E","F","G","H")
par(las=1,cex.axis=0.9,cex.lab=0.85,mfrow=c(2,4),pty='s',mar=c(0.2,0.1,0.1,0.1),mai=c(0.2,0.55,0.1,0.2), mgp=c(2,1,0))
for (i in 1:length(`AcuteUpperRespiratoryTractinfections Forecast`)){
  temp <- `AcuteUpperRespiratoryTractinfections Forecast`[[i]]
  forecastPlotter2(y=temp[,6],
                  yf=temp[,5],
                  xLab="Observation",
                  yLab=paste0("Forecast Horizon ",i),
                  stepAhead=i,
                  panelLab = panelName[i])
}

dev.off()

dataPlotter <- function(y,xLab,yLab,panelLab){
  plot(y=c(min(y,na.rm=T),max(y,na.rm=T)),
       x=c(0,length(y)),
       col="white",
       xaxt='n',
       xlab=xLab,
       ylab=yLab)
  lines(y=y,x=seq(1,length(y)))
  axis(side=1,at=seq(0,length(y),by=52),labels=F)
  axis(side=1,at=seq(0,length(y),by=52)+26,labels=seq(12,22,by=1),tick=F)
  axis(side=1,at=seq(0,length(y),by=4),labels=F,col.ticks = "darkred",lwd.ticks=0.2,tck=-0.03)
  mtext(panelLab,side=3,adj=0,cex=0.8)
  box()
  
}

#plots for URTI case counts and weather
load(paste0(cd,"out/merged_2021-2012.rds"))
pdf(paste0(cd,"/out/forecast_plots/data.pdf"),width=5,height=9)
par(las=1,cex.axis=0.9,cex.lab=0.85,mfrow=c(5,1),mar=c(1,3.5,1,0.5),mai=c(0.3,0.55,0.2,0.2))
dataPlotter(y= as.numeric(df_all$Acute.Upper.Respiratory.Tract.infections),
                xLab="",
                yLab="Acute URTI Case Counts",
            panelLab='A')

dataPlotter(y= as.numeric(df_all$mean_temp),
            xLab="",
            yLab="Mean Temperature",
            panelLab='B')

dataPlotter(y= as.numeric(df_all$mean_tp),
            xLab="",
            yLab="Total Precipitation",
            panelLab='C')

dataPlotter(y= as.numeric(df_all$mean_rh),
            xLab="",
            yLab="Relative Humidity",
            panelLab='D')

dataPlotter(y= as.numeric(df_all$mean_ah),
            xLab="",
            yLab="Absolute Humidity",
            panelLab='E')
dev.off()
