rm(list=ls())
cd <- ""  #change to own directory here
files <- list.files(paste0(cd,"/out/forecast_ouput/"))
for (j in files){
  load(paste0(cd,"/out/forecast_ouput/",j))
}

require(forecast)
computeAssessments <- function(y, 
                               yf,
                               naive,
                               horizon){
  
  #compute forecast error metrics
  ind <- which(!is.na(yf[,1]))
  y <- y[ind]
  naive <- naive[ind]
  yf <- yf[ind,]
  yfn <- cbind(yf,naive)
  
  mae <- abs(y - yfn)
  mae <- apply(mae,MARGIN=2,mean)
  
  rmse <- (y - yfn)^2
  rmse <- apply(rmse,MARGIN=2,mean)
  rmse <- sqrt(rmse)
  
  mape <- abs((y - yfn)/y)
  mape <- apply(mape,MARGIN=2,mean)
  
  mase <- mae[1:6]/mae[6]
  assessments <- rbind(mae,rmse,mape,mase)
  rownames(assessments) <- c('mae','rmse','mape','mase')
  #compute forecast errors pairwise, compute pairwise DM test pvalue for one sided test at 0.05 level
  errors = y - yfn
  if(colSums(errors)[2]==colSums(errors)[3]) { errors <- errors[,-3]}
  store <- list()
  for (i in 1:ncol(errors)){
    store[[i]] <- apply(errors[,-i],
                        MARGIN=2,
                        function(x,y=errors[,i],z=horizon)dm.test(x,y,alternative="two.sided",h=z,power=1))
    store[[i]] <- lapply(store[[i]],function(x)x$p.value)
    store[[i]] <- unlist( store[[i]] )
    store[[i]] <- append(store[[i]],values=1,after=i-1)
  }
  
  store <- do.call(rbind,store)
  rownames(store) <- colnames(errors)
  round(store,digits=3)
  
  
  out <- list(assessments,store)
  return(out)
}

storeAssessments <- list()
for (i in 1:length(`AcuteUpperRespiratoryTractinfections Forecast`)){
  
  Y <- `AcuteUpperRespiratoryTractinfections Forecast`[[i]][,6]
  Yf <- `AcuteUpperRespiratoryTractinfections Forecast`[[i]][,1:5]
  Naive <- which(colnames(`AcuteUpperRespiratoryTractinfections Forecast`[[i]])== "AcuteUpperRespiratoryTractinfections Lag 1")
  Naive <- `AcuteUpperRespiratoryTractinfections Forecast`[[i]][,Naive]
  
  storeAssessments[[i]] <- computeAssessments(y=Y, 
                     yf=Yf,
                     naive=Naive,
                     horizon=i)
}

#do as plots

mae <-  do.call(rbind,lapply(storeAssessments,function(x)x[[1]][1,]))
rmse <- do.call(rbind,lapply(storeAssessments,function(x)x[[1]][2,]))
mape <- do.call(rbind,lapply(storeAssessments,function(x)x[[1]][3,]))
mase <- do.call(rbind,lapply(storeAssessments,function(x)x[[1]][4,]))


assessmentPlotter1 <- function(metricMatrix,
                               xlab="Forecast Horizon (Weeks Ahead)",
                               ylab,col=c("red","blue","darkgreen","cadetblue4","orange","black"),
                               panelName,
                               leg=F,PCH=c(15,16,17,18,19,8)){
  names <- c("AR (Baseline)","LASSO","ENET","GBM","Comb","Naive")
  horizon = 1:8
  plot(y=c(min(c(metricMatrix),na.rm=T),max(c(metricMatrix),na.rm=T)),
       x=c(1,nrow(metricMatrix)),
       col="white",
       xaxt='n',
       xlab=xlab,
       ylab=ylab)
  
  for (i in 1:ncol(metricMatrix)){
    lines(metricMatrix[,i],col=col[i])
    points(y=metricMatrix[,i],x=seq(1,length(metricMatrix[,i])),col=col[i],pch=PCH[i],cex=1.2)
  }
  
  if (leg){
  legend(x="topleft",legend=names,col=col,pch=PCH,bty='n')
  }  
  axis(side=1,at=seq(0,nrow(metricMatrix),by=1))
  mtext(panelName,side=3,adj=0,cex=0.8)
  box()
  
}

pdf(paste0(cd,"/out/forecast_plots/forecasts3.pdf"),width=10,height=5)
# par(las=1,cex.axis=0.8)
par(las=1,cex.axis=0.9,cex.lab=0.85,mfrow=c(1,4),pty='s',mar=c(0.2,0.1,0.1,0.1),mai=c(0.2,0.55,0.1,0.2), mgp=c(3,1,0))
assessmentPlotter1(metricMatrix=mape,ylab="Mean Absolute Percentage Error",panelName="A",leg=T)
assessmentPlotter1(metricMatrix=rmse,ylab="Mean Squared Forecast Error",panelName="B")
assessmentPlotter1(metricMatrix=mae,ylab="Mean Absolute Forecast Error",panelName="C")
assessmentPlotter1(metricMatrix=mase,ylab="Mean Absolute Scaled Error",panelName="D")
dev.off()



##plot heatmaps for DM test statistics
# temp <- storeAssessments[[1]][[2]]
# temp[which(temp>=0.05)] <- 1
# temp[which(temp<0.05)] <- 0
# rownames(temp) = colnames(temp) <- c("AR","LASSO","GBM","Comb","Naive")
require(pheatmap)
require(gridExtra)
storePlots <- list()
labs <- c("A","B","C","D","E","F","G","H")
for (i in 1:length(storeAssessments)){
  temp <- storeAssessments[[i]][[2]]
  temp[which(temp>=0.1)] <- 1
  temp[which(temp<0.1)] <- 0
  rownames(temp) = colnames(temp) <- c("AR","LAS","GBM","CO","NAI")
  leg=T
  if (i<8){leg=F}
  temp[lower.tri(temp)] <- NA
  diag(temp) <- NA
  temp <- pheatmap::pheatmap(temp,
                             border_color = "lightgrey",
                             color=c("red","black"), 
                             treeheight_row = 0,
                             treeheight_col = 0,cluster_rows=F, cluster_cols=F,
                             legend=leg,
                             legend_breaks=c(0,1),
                             legend_labels=c("NE","E"),main=paste0(labs[i],": Horizon ",i))
  
  
  storePlots[[i]]  <- temp[[4]]
}


g <- grid.arrange(arrangeGrob(grobs= storePlots,ncol=4))
ggsave(paste0(cd,"/out/forecast_plots/forecasts4.pdf"),g)

