require(gplots)
require(RColorBrewer)
library("grDevices")
#install.packages("randomcoloR")
library("randomcoloR")
library(corrplot)

############################################################################################################
#Fisher modszer, fuggetlen p-value-ek osszekombinalasara
#2 db R egyszerre jelen a gepen: https://tuxette.nathalievialaneix.eu/2021/02/multiple-versions-of-R.html
#  sudo apt install libcurl4
#  sudo apt-get remove libcurl3
#  sudo apt --fix-broken install
#  sudo apt install libcurl4
#  sudo dpkg -i r-4.1.0_1_amd64.deb
#  sudo apt --fix-broken install
#  sudo dpkg -i r-4.1.0_1_amd64.deb

library("poolr")
#csak vector data adhato at neki
#https://cran.r-project.org/web/packages/poolr/poolr.pdf
#fisherM <- function(p,r){fisher(p)$p}
fisherM <- function(p,r){fisher(p, adjust = "generalized", R = mvnconv(r))$p}
#fisherM <- function(p,r){fisher(p, adjust = "nyholt", R = mvnconv(r, target = "p", cov2cor = TRUE))$p}

#kezzel szamolt Brown's modszer
# library(poolr)
# nnunet=read.csv("nnUNetPyradiomics.csv")
# man_nnunet=read.csv("manual_nnunet_pvalue.csv")
# rownames(nnunet)=nnunet$Case
# nnunet$Case=NULL
# r=cor(t(nnunet))
# fisher(man_nnunet[,2], adjust = "generalized", R = mvnconv(r))
#    0.1724165
#
#or the correlation matrix can be calculated like this (from manual segmentation):
#man=read.csv("ManualMaskPyradiomics.csv")
#rownames(man)=man$Case
#man$Case=NULL
#rr=cor(t(man))
#fisher(man_nnunet[,2], adjust = "generalized", R = mvnconv(rr))$p
#    0.1732013

fisherMM <- function(data){
    #data = -log10( apply(dfresAll,2,fisherM) )  
    #^2 = -2 sum_{i=1}^k ln(p_i)
    #fisher(data)$p
    S=0
    for(i in length(data)){
        S=S-2*log(data[i])
    }
    #print(S)
}

FDR <- function(data){p.adjust(data,method="fdr")}

#must have one more break than colour
#  "#000000"   "#FF0000FF"    "#FFAA00FF"   "#AAFF00FF"   "#00FF00FF"   "#00FFAAFF"   "#00AAFFFF"   "#0000FFFF"   "#AA00FFFF"   "#FF00AAFF"   "#FFFFFF"
#0          40             42            44             46           48             50            52            54            56            58            60

fn=""
#szetbontja a palettat from-tol to-ig, from alatt minden black, high felett minden white
hmap <- function(data,palette="rainbow",n=10,low=0.0,from=0.01,to=0.05,high=1.0, REV=FALSE, fn="") 
{
    #palettes: hcl.pals()
    myCol = switch(
	palette,
    "spectral"= hcl.colors(n, palette = "Spectral", alpha = NULL, rev = REV, fixup = TRUE),
	"rainbow" = rainbow(n,rev=REV),
	"heat"    = heat.colors(n,rev=REV),
	"terrain" = terrain.colors(n,rev=REV), 
	"topo"    = topo.colors(n,rev=REV), 
	"cm"      = cm.colors(n,rev=REV),
	"rnd"	  = randomColor(10)	#c("blue", "green", "yellow", "orange", "red")

    )
    myBreaks <- seq(from,to,(to-from)/n) 	#from es to benne van = 11 szam 10 tartomany

    #betoldunk az elejere egy nagyobb intervallumot, fekete szinnel, a vegere egy nagyobb intervallumot, feher szinnel
    if(REV==TRUE)
    {
        myCol=append("#FFFFFF",myCol)
        myCol=append(myCol,"#000000")
    
    } else {
        myCol=append("#000000",myCol)
        myCol=append(myCol,"#FFFFFF")
    }
    myBreaks=append(low,myBreaks)
    myBreaks=append(myBreaks,high)

#    myCol[length(myCol)]="#ffffff"

    #legendak gyartasa
    lgnd=0
    for(i in 1:length(myBreaks)-1)
    {
        if(i==1)
        { 
            lgnd=paste(myBreaks[i],"-",myBreaks[i+1])
        } else { 
            lgnd=append(lgnd,paste(myBreaks[i],"-",myBreaks[i+1])) 
        } 
    }

    if (is.na(fn) || fn != '') { png(file=paste0(fn,".png"),width=1000,height=1000) }
    hm <- heatmap(data, scale="none", Rowv=NA, Colv=NA, col = myCol,breaks = myBreaks,margins=c(8,15), cexRow=1.5, cexCol=1.5)
    #cexRow: changes the size of the row label font.
    # dendrogram = "none"
    # key=TRUE,keysize=1.5, trace="none"
    legend("right", fill = myCol,legend = lgnd)
    if (is.na(fn) || fn != '') { dev.off() }
}

################
### 1
################
fun_show_heatmap_of_pvalues <- function(prefix,bw,dfres)
{
        ##X11()
        ##hmap(pValues, "rnd", 10, 0.0, 0.0, 0.05, 1.0, "navegre")
        ##hmap(pValues, "rnd", 10, 0.0, 0.0, 0.05, 1.0)
        dn=paste0("./figurak/",prefix,"_bw_",bw,"_hmap_of_minusLogPValues_of_man_and_algi_vs_radPramsInGroup/")
        dir.create(dn)
        fn=paste0(radiomicsGroupName,"_")
        png(file=paste0(dn,fn,".png"),width=1000,height=1000)
        hmap( -log(as.matrix(dfres)),  "rainbow", 10,   0.0, 0.0,   10.0, 10.0 )
        dev.off()
    #readline(prompt="Press [enter] to continue")
}

#################
### 2
#################
fun_show_corrplot_of_pvalues <- function(prefix, bw, dfres)
{
       ##X11()
        ##hmap(pValues, "rnd", 10, 0.0, 0.0, 0.05, 1.0, "navegre")
        ##hmap(pValues, "rnd", 10, 0.0, 0.0, 0.05, 1.0)
        dn=paste0("./figurak/",prefix,"_bw_",bw,"_corrplot_of_minusLogPValues_of_man_and_algi_vs_radPramsInGroup/")
        dir.create(dn)
        fn=paste0(radiomicsGroupName,"_")
        png(file=paste0(dn,fn,".png"),width=1000,height=1000)
        #hmap( -log(as.matrix(dfres)),  "rainbow", 10,   0.0, 0.0,   10.0, 10.0 )
        corrplot( -log(as.matrix(dfres)), is.corr=F,tl.cex = 2,cl.cex=1.3)
        dev.off()
    
        cairo_ps(file=paste0(dn,fn,".eps")) #,width=1000,height=1000
        corrplot( -log(as.matrix(dfres)), is.corr=F,tl.cex = 1,cl.cex=0.7)
        dev.off()    
}

#################
### 3
#################
# bw = bin width
fun_show_ggboxplot_of_pvalues <- function(prefix,bw,DATA.FRAME,segmMethodNames)
{
        library("ggpubr")
        library("ggplot2")

        my_comparisons <- list(
         c("AC", "FCM"), c("AC","GMM"), c("AC","KM"), c("AC","MAJ_6_3"), c("AC","MAJ_6_4"), c("AC","MAJ_6_5"), c("AC","MAJ_6_6"),c("AC","RG"),c("AC","RW"),c("AC","UT"),c("AC","nnunet531"),
         c("FCM","GMM"), c("FCM","KM"), c("FCM","MAJ_6_3"), c("FCM","MAJ_6_4"), c("FCM","MAJ_6_5"), c("FCM","MAJ_6_6"),c("FCM","RG"),c("FCM","RW"),c("FCM","UT"),c("FCM","nnunet531"),
         c("GMM","KM"), c("GMM","MAJ_6_3"), c("GMM","MAJ_6_4"), c("GMM","MAJ_6_5"), c("GMM","MAJ_6_6"),c("GMM","RG"),c("GMM","RW"),c("GMM","UT"),c("GMM","nnunet531"),
         c("KM","MAJ_6_3"), c("KM","MAJ_6_4"), c("KM","MAJ_6_5"), c("KM","MAJ_6_6"),c("KM","RG"),c("KM","RW"),c("KM","UT"),c("KM","nnunet531"),
         c("MAJ_6_3","MAJ_6_4"), c("MAJ_6_3","MAJ_6_5"), c("MAJ_6_3","MAJ_6_6"),c("MAJ_6_3","RG"),c("MAJ_6_3","RW"),c("MAJ_6_3","UT"),c("MAJ_6_3","nnunet531"),
         c("MAJ_6_4","MAJ_6_5"), c("MAJ_6_4","MAJ_6_6"),c("MAJ_6_4","RG"),c("MAJ_6_4","RW"),c("MAJ_6_4","UT"),c("MAJ_6_4","nnunet531"),
         c("MAJ_6_5","MAJ_6_6"),c("MAJ_6_5","RG"),c("MAJ_6_5","RW"),c("MAJ_6_5","UT"),c("MAJ_6_5","nnunet531"),
         c("MAJ_6_6","RG"),c("MAJ_6_6","RW"),c("MAJ_6_6","UT"),c("MAJ_6_6","nnunet531"),
         c("RG","RW"),c("RG","UT"),c("RG","nnunet531"),
         c("RW","UT"),c("RW","nnunet531"),
         c("UT","nnunet531")
        )

        library(RColorBrewer)
        # Define the number of colors you want
        nb.cols <- 11
        mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)
        # Create a ggplot with 18 colors 
        # Use scale_fill_manual

        #ha a stat_compare_means-ben rosszul irok vmit, akkor megmondja mit lehet beirni method-ra
        #Allowed methods are one of: t.test, t.test, t.test, wilcox.test, wilcox.test, wilcox.test, anova, anova, kruskal.test, kruskal.test

        #Sys.sleep(3)
        #readLines(stdin(),1)

        ##    ggboxplot(DATA.FRAME,x="algorithm",y="-log(p-value w/ FDR)",color="algorithm",palette="npg", orientation = "horizontal")
        ##   p = ggboxplot(DATA.FRAME,x="algorithm",y="-log(p-value w/ FDR)",color="algorithm",palette="npg", orientation = "horizontal") + stat_compare_means(comparisons = my_comparisons, method = "wilcox.test")
        p = ggboxplot(DATA.FRAME,x="algorithm",y="neg_log_p_val_w_FDR",color="algorithm", orientation = "horizontal",ylab="-log(p-value with FDR)") +
             scale_fill_manual(values = mycolors) + theme_minimal() + stat_compare_means(comparisons = my_comparisons, method = "wilcox.test")

        #a print azert kell, mert a ggplot2 fgv-ek nem mukodnek jol source vagy fgv torzsbol valo hivas eseten: https://stackoverflow.com/questions/17126128/ggplot2-does-not-appear-to-work-when-inside-a-function-r
        dn=paste0("./figurak/",prefix,"_bw_",bw,"_ggboxplot_of_minusLogPValues_of_man_and_algi_vs_radParamGroups/")
        dir.create(dn)
        fn=paste0(radiomicsGroupName,"")
        png(file=paste0(dn,fn,".png"),width=1000,height=1000)
        print(p)
        dev.off()

        #ellenorzes, hogy jol szamol-e a diagram generalo R csomag:
        # kruskal.test(DATA.FRAME[which(DATA.FRAME$algorithm=="fcm"),1],DATA.FRAME[which(DATA.FRAME$algorithm=="ac"),1])$p.value
        # Kruskal-Wallis chi-squared = 17, df = 16, p-value = 0.3856
        # Kruskal-Wallis chi-squared = 13, df = 13, p-value = 0.4478
}

##############
### 4
##############
fun_show_corrplot_of_pairwise_wilcox_of_pvalues <- function(prefix,bw,segmMethodNames,DATA.FRAME)
{
        compAlgs = matrix( 0, nrow=(length(segmMethodNames)-0), ncol=(length(segmMethodNames)-0) )
        colnames(compAlgs) = segmMethodNames
        rownames(compAlgs) = segmMethodNames
        for( i in 2:length(segmMethodNames) )                           #2...11,
        {                                                               #     2        3         4         5         6              7         8         9        10   11
            alg1 = segmMethodNames[i]                                   # "gmm"     "km"      "ac"      "rg"      "rw"      "maj_6_3" "maj_6_4" "maj_6_5" "maj_6_6" "ut"
            for( j in 1:(i-1) )                                         #1...10,
            {
                #print(paste0(i,", ",j))                                #     1         2        3                                                                         10
                alg2 = segmMethodNames[j]                               # "fcm"     "gmm"     "km"      "ac"      "rg"      "rw"      "maj_6_3" "maj_6_4" "maj_6_5" "maj_6_6"
                #print(paste0(alg1,", ",alg2))                          # (gmm-fcm) (km-fcm,km-gmm) ... (ut-fcm,ut-gmm,...,ut-maj66)
                compAlgs[i,j] = wilcox.test(DATA.FRAME[which(DATA.FRAME$algorithm==alg1),1],DATA.FRAME[which(DATA.FRAME$algorithm==alg2),1])$p.value+.01
                #compAlgs[i,j] = kruskal.test(DATA.FRAME[which(DATA.FRAME$algorithm==alg1),1],DATA.FRAME[which(DATA.FRAME$algorithm==alg2),1])$p.value
                #compAlgs[i,j] = t.test(DATA.FRAME[which(DATA.FRAME$algorithm==alg1),1],DATA.FRAME[which(DATA.FRAME$algorithm==alg2),1])$p.value
            }
        }

        #hmap( compAlgs,  "rainbow", 10,   0.0, 0.0,   .4, .4 )
        dn=paste0("./figurak/",prefix,"_bw_",bw,"_corrplot_of_pairwise_wilcox_test_of_minusLogPValues_of_man_and_algi_vs_radParamGroups/")
        dir.create(dn)
        fn=paste0(radiomicsGroupName,"")
        png(file=paste0(dn,fn,".png"),width=1000,height=1000)
        corrplot(compAlgs,is.corr=F)
        dev.off()
}

####################
### 5
####################
fun_calc_fisherM_of_pvalues <- function(prefix,bw,radiomicsGroupNames,segmMethodNames,fm)
{
#    lfm=-log10(fm)
#    rownames(lfm) = radiomicsGroupNames
#    colnames(lfm) = segmMethodNames

    #dn=paste0("./figurak/",prefix,"_bw_",bw,"_hmap_and_barplot_of_minusLogFisherPValues_of_man_and_algi_vs_radParamGroups/")
    #dir.create(dn)
    #fn=paste0("fisher_combined_p_values_vs_algi_radParamGroups_","_")
    #png(file=paste0(dn,fn,".png"),width=1000,height=1000)
    ##hmap( lfm,  "rainbow", 10,   0.0, 0.0,   250, 300 )
    #X11()
    #hmap( lfm,  "heat", 10,   round(min(lfm),2)-0.01, round(min(lfm[,5]),2),  round(max(lfm[,5]),2), round(max(lfm),2)+0.01, REV=F )           #low, from, to, high
    #X11()
    rownames(fm) = radiomicsGroupNames
    colnames(fm) = segmMethodNames
    corrplot( -log10(as.matrix(fm)), is.corr=F)
   
    #dev.off()
}

#######################
### 6
#######################
fun_calc_sum_of_fisherM_of_pvalues <- function(prefix,bw,segmMethodNames,radiomicsGroupNames,fm)
{
    lfm=-log10(fm)
    sum=rep(0,length(segmMethodNames))                  #= 0 0 0 0 0 0 0 0 0 0 0
    for(i in 1:length(segmMethodNames) )                        #1...11
    {
        for(j in 1:length(radiomicsGroupNames))         #1...7
        {
            sum[i] = sum[i]+lfm[j,i]
        }
    }
    algs=data.frame(sum)
    rownames(algs)=segmMethodNames
    dn=paste0("./figurak/",prefix,"_bw_",bw,"_hmap_and_barplot_of_minusLogFisherPValues_of_man_and_algi_vs_radParamGroups/")
    dir.create(dn)
    fn=paste0("summarized_fisher_combined_pavlues_vs_algi","_")
    png(file=paste0(dn,fn,".png"),width=1000,height=1000)
    barplot(algs$sum,names.arg=rownames(algs),xlab="algorithms",ylab="summed up Fisher combined p-values")
    dev.off()
}

######################
### 7
######################
fun_calc_super_fisherM_of_pvalues <- function(prefix,bw,fm,segmMethodNames)
{
    salgs=data.frame(-log(apply(fm,2,fisherM)))
    rownames(salgs)=segmMethodNames
    colnames(salgs)="superfisher"
    dn=paste0("./figurak/",prefix,"_bw_",bw,"_hmap_and_barplot_of_minusLogFisherPValues_of_man_and_algi_vs_radParamGroups/")
    dir.create(dn)
    fn=paste0("super_summarized_fisher_combined_pavlues_vs_algi","_")
    png(file=paste0(dn,fn,".png"),width=1000,height=1000)
    barplot(salgs$superfisher,names.arg=rownames(salgs),xlab="algorithms",ylab="super combined Fisher combined p-values")
    dev.off()
}

#################################################
#BONYOLULT lasd alatta
#manualis es egyes alg-ok terfogatainak eloszlasa
#volidx=grep("firstorder_volume",colnames(df))                                          #22
#methods=as.character(unique(df$segmentation_method))                                   #"w_manual" "ut"       "maj_6_6"  "maj_6_5"  "maj_6_4"  "maj_6_3" "rw"       "rg"       "ac"
##volumes=data.frame( log10( df[which(df[,3]=="w_manual"),volidx] ) )
#volumes=matrix(0,nrow=length(df[which(df[,3]=="w_manual"),volidx]),ncol=length(methods)) #43x12
#for(segmMethod in methods)
#{
#    #volumes=cbind( volumes,log10(df[which(df[,3]==segmMethod),volidx]) )
#    #print( paste(segmMethod,length(df[which(df[,3]==segmMethod),volidx])) )
#    for(j in seq(1,length(df[which(df[,3]==segmMethod),volidx])) )
#    {
#       volumes[j,grep(segmMethod,methods)]=df[which(df[,3]==segmMethod),volidx][j]
#    }
#}
#colnames(volumes)=methods
#Volumes=data.frame(volumes)

###################
### 8
###################
#fun_calc2_fisherM_of_pvalues <- function(dat)
fun_calc2_fisherM_of_pvalues <- function(segmMethodNames,dat)   #,radiomicsGroupName,radiomicsGroupNames
{
    #korrelacios matrix gyartasa a fisher modszerhez, mert a kulonbozo parameterek nem fuggetlenek egymastol
    r<-cor(t(dat))

    data = -log10( apply(dat,2,fisherM,r) )                 #apply() masodik parametere = 2, mert a dfresAll oszlopaibol(=alg-ok, osszesen 11db) csinal vektort es arra alkalmazza

    #X11()
    dn=paste0("./figurak/",prefix,"_bw_",bw,"_histogram_of_minusLogFisherPValues_of_man_and_algi/")
    dir.create(dn)
    fn=paste0("fisher_combined_pavlues_vs_algi","_")
    png(file=paste0(dn,fn,".png"),width=1000,height=1000)
    par(mar=c(5,5,4,4))  #margins  bottom, left, top, right respectively.
    barplot(data,names.arg=names(data),xlab="algorithms",ylab="negative logarithm of Fisher combined p-values",cex.axis=1.5, cex.names=1.0,cex.lab=2)
    dev.off()

    cairo_ps(file=paste0(dn,fn,".eps")) #,width=1000,height=1000
    par(mar=c(5,5,4,4))  #margins  bottom, left, top, right respectively.
    barplot(data,names.arg=names(data),xlab="algorithms",ylab="Fisher combined p-values",cex.axis=1.0, cex.names=1.0,cex.lab=1,las=2)
    dev.off()    
    
    write.csv(t(round(data,2)),file=paste0(dn,fn,".csv") )
}

fun_calc3_fisherM_of_pvalues <- function(dfresAll)
{
    data = -log10( apply(dfresAll,2,fisherMM) )                 #apply() masodik parametere = 2, mert a dfresAll oszlopaibol(=alg-ok, osszesen 11db) csinal vektort es arra alkalmazza

    #X11()
    barplot(data,names.arg=names(data),xlab="algorithms",ylab="Fisher combined p-values")
}
