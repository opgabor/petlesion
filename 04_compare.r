library("PerformanceAnalytics")


df=read.csv("df.csv")

#eredmenyeket itt taroljuk: sorok = syudy_id-k, oszlopok = szegmentacios modszerek, kiveve manualis (ezzel korrelaltatunk mindent)
corrs=matrix(0,nrow=length(unique(df$study_id)),ncol=length(unique(df$segmentation_method))-1)
colnames(corrs)=c("FCM","GMM","KM","AC","RG","RW","MAJ_6_3","MAJ_6_4","MAJ_6_5","MAJ_6_6")
rownames(corrs)=sort(unique(df$study_id))

for( stuid in sort(unique(df$study_id)) )
{
    print(stuid)

    sub_df=df[which(df$study_id==stuid),]
    sub_df[,1:2]=NULL
    rownames(sub_df)=sub_df[,1]
    sub_df[,1]=NULL
    sub_df_t=t(sub_df)


    #normalize
    maxOld=max(sub_df_t)
    minOld=min(sub_df_t)
    maxNew=100
    minNew=0
    sub_df_t_n=matrix(0,nrow=dim(sub_df_t)[1],ncol=dim(sub_df_t)[2])
    rownames(sub_df_t_n)=rownames(sub_df_t)
    colnames(sub_df_t_n)=colnames(sub_df_t)
    for( i in 1:nrow(sub_df_t) )
    {
        for( j in 1:ncol(sub_df_t) )
        {
            v=sub_df_t[i,j]
            nv = minNew + (maxNew/(maxOld-minOld)) * (v-minOld)
    #	    print(paste(as.character(v)," -> ",as.character(nv)))
	    sub_df_t_n[i,j]=nv
        }
    }

    #delete outliers
    idx=unique(sort(which(sub_df_t_n>=10,arr.ind=TRUE)[,1]))
    sub_df_t_n=sub_df_t_n[-idx,]
    #sub_df_t_n=sub_df_t_n[-c(7,20,82),]
    
#    png(file=paste0("corrChart_wout_outliers_",stuid,".png"),width=1000,height=1000)
#    chart.Correlation(sub_df_t_n,method="pearson")
#    dev.off()
#    stop()

    #as.character(df[which(df$study_id==7),]$segmentation_method)
    #"manual"  "FCM"     "GMM"     "KM"      "AC"      "RG"      "RW" "MAJ_6_3" "MAJ_6_4" "MAJ_6_5" "MAJ_6_6"

    manMask=sub_df_t_n[,1]
    for( mask in  c("FCM","GMM","KM","AC","RG","RW","MAJ_6_3","MAJ_6_4","MAJ_6_5","MAJ_6_6") )
    { 
#       print(mask) 

	if(mask %in% colnames(sub_df_t_n))
	{
	    tmpMask=sub_df_t_n[,mask]
	    corrs[as.character(stuid),mask] = cor(manMask,tmpMask)
	}
    }

}

#corrs=read.csv("corrs_wout_outliers.csv")
write.csv(corrs, "corrs_wout_outliers.csv", row.names=FALSE, quote=FALSE)
#corrs=readRDS(file="corrs_wout_outliers.Rda")
saveRDS(corrs, file="corrs_wout_outliers.Rda")

#stop()



require(gplots)
require(RColorBrewer)
library("grDevices")

#myCol <- c("blue", "green", "yellow", "orange", "red")
n <- 10
myCol <- rainbow(n)
#myCol <- heat.colors(n)
#myCol <- terrain.colors(n)
#myCol <- topo.colors(n)
#myCol <- cm.colors(n)
#myBreaks <- c(0.8, .84, 0.88, 0.92, 0.96, 1.0)
from=0.9
to=1.0
myBreaks <- seq(from,to,(to-from)/n)

#betoldunk az elejere egy nagyobb intervallumot, fekete szinnel
myCol=append("#000000",myCol)
myBreaks=append(-0.1,myBreaks)

myCol[length(myCol)]="#ffffff"

#legendak gyartasa
lgnd=0
for(i in 1:length(myBreaks)-1){if(i==1){ lgnd=paste(myBreaks[i],"-",myBreaks[i+1])}else{ lgnd=append(lgnd,paste(myBreaks[i],"-",myBreaks[i+1])) } }


png(file="corrs_wout_outliers.png",width=1000,height=1000)
#png(file="corrs_w_outliers.png",width=1000,height=1000)
hm <- heatmap(corrs, scale="none", Rowv=NA, Colv=NA,
                col = myCol, ## using your colors
                breaks = myBreaks, ## using your breaks
                dendrogram = "none",  ## to suppress warnings
                margins=c(5,12), cexRow=0.5, cexCol=1.0,  key=TRUE, keysize=1.5,
                trace="none")
legend("right", fill = myCol,legend = lgnd)
dev.off()



