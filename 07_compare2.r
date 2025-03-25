#elotte a df.csv-ben a manual-t lecsereltem w_manual-ra, hogy az abc-ben a manual a vegere keruljon, Igy jon ki jol a pairwise wilcox tablazat szepen. Egyebkent a manualt
#betenne a tabla kozepere.

#prefix=""
prefix="cut_"
#prefix="bw_filtered_"
#prefix="bw_cut_"
#prefix="filtered_"
bw=''			#ha nincs bw, akkor ezt be kell allitani ures stringre, mert az R megjegyzi a korabbi bw erteket
#bw=0.2                 #ha a prefix bw-vel kezdodik, akkor a rekordok szama 2x annyi, mert van bin_width=0.2 es =0.31-gyel szamolt sor is
#bw=0.31

show_heatmap_of_pvalues				= FALSE
show_corrplot_of_pvalues			= FALSE
show_ggboxplot_of_pvalues			= FALSE
show_corrplot_of_pairwise_wilcox_of_pvalues	= FALSE
calc_fisherM_of_pvalues				= TRUE
calc2_fisherM_of_pvalues			= TRUE
calc_super_fisherM_of_pvalues			= FALSE
calc_sum_of_fisherM_of_pvalues			= FALSE
calc_distrib_of_volumes				= FALSE

df=read.csv(paste0("./queried_CSVs/",prefix,"_bw_",bw,"_df.csv"))		#cut__bw__df.csv
#df=read.csv("df_nem_lyukas.2022.12.17.csv")

source("015_funcs.r")

#getOption("width")
#options(width=400)

#az elso 3 oszlopot kihagyjuk = study_instance_uid, study_id, segmentation_method
#from=4
#pValues = matrix(0,nrow=length(colnames(df))-from+1,ncol=length(unique(df$segmentation_method))-1) #oszlopok szamaban azert van -1, mert a manualist manualishoz nem hasonlitom

#options(width=300)
#head(df[1:10,1:10])
#study_instance_uid study_id segmentation_method firstorder_10percentile firstorder_90percentile firstorder_energy firstorder_entropy firstorder_interquartilerange firstorder_kurtosis firstorder_maximum ... firstorder_volume
#42174574200751            7            w_manual                  1.6831                  6.4405          32983.67             3.0571                        2.4556              3.7615            10.3408     x
#42174574200751            7                 FCM                  3.4361                  7.8697          27040.65             3.0671                        2.3989              2.7356            10.3408     y
#42174574200751            7                 GMM                  5.1152                  8.5798          19878.61             3.0282                        2.0694              2.4336            10.3408     z
#42174574200751            7                  KM                  3.2653                  7.7676          27681.76             3.0329                        2.4551              2.7885            10.3408     i

segmMethodNames = as.character(unique(df$segmentation_method))
segmMethodNames = segmMethodNames[segmMethodNames!="w_manual"]	#kiszedem a manualis modszert
radiomicsGroupNames = c("firstorder","glcm","shape","glszm","glrlm","ngtdm","gldm")

#Fisher modszerhez eredmenyeinek tarolasahoz matrix helyfoglalas
fm = matrix(0,nrow=length(radiomicsGroupNames),ncol=length(segmMethodNames)-0) # 7: ennyi radmics csoport van, az oszlopok szamaban azert van -0, mert korabban a manualist manualishoz nem hasonlitottam

#######################################################################
#itt gyujtom majd ossze az osszes radiomics parameterhez tartozo p ertekeket
PVals = data.frame(matrix(0,nrow=length(colnames(df))-3,ncol=length(segmMethodNames))) #-3 azert mert az 4. oszloptol jonnek a rad. params.
colnames(PVals) = segmMethodNames

#########################################################################################
#az itt kovetkezo for ciklusban csak egy rad param group-ba tartozo ertkekkel foglalkozok
#vegig megyek az oszlopokon = radiomics parametreken. Ezek lesznek az eredmeny matrix (pValues) sorai
#for(i in from:length(colnames(df)) )
#vegig megyek az oszlop csoportokon = radiomics parameter csoportokon. Ezek lesznek az eredmeny matrix (pValues) sorai
cntr=0	#to fill the PVals matrix properly
for( radiomicsGroupName in radiomicsGroupNames )
{
    print(radiomicsGroupName)
    radParamsInGroup = grep(paste0(radiomicsGroupName,"_.*"), colnames(df))	#indexeket ad vissza, pl.:  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
    from=radParamsInGroup[1]
    #placeholder
    pValues = matrix(0,nrow=length( radParamsInGroup ),ncol=length(unique(df$segmentation_method))-1) #oszlopok szamaban azert van -1, mert a manualist manualishoz nem hasonlitom
    #adott csoporton belul kiszedek minden parametert
    for(i in radParamsInGroup )
    {
	#minden betegtol osszegyujtom a manualis modszerre es az i-vel jelolt aktualis rdiomics parameterre vonatkozo radiomics parametereket
        manualRadParams=df[ which(df[,3]=="w_manual"), i ]		#43 elemu vektor = ahany paciens van, pl.: i = "firstorder_10percentile"
        for(j in 1:length(segmMethodNames)) 				#nincs benne a manualis 
        {
    	    #vegig megyek a sorokon, amik egy faktor valtozo (1,2,3,4,5,6,7,8,9,10,{FCM,GMM,KM,stb}), ennek csak a kulonbozo elemeit veszem ki
    	    segmMethodName = segmMethodNames[j]
	    #minden betegtol osszegyujtom az aktualis segm. modszerre es az i-vel jelolt aktualis rdiomics parameterre vonatkozo radiomics parametereket
	    segmMethodRadParams = df[ which(df[,3]==segmMethodName), i ]	#szinten 43 elemu vektor, jobb hijan neha csak 38 pl. a "MAJ_6_6"-nal, ott amiatt, hogy az rg 2 esetben nem mukodott maj_6_6 sem keletkezett
    	    #print(paste0(as.character(i),", manual-", segmMethodName ));
    	    #es osszehasonlitom a manualis roi-n belul szamolt radiomics parametereket a kulonbozo szegmentacios modszerekkel keszult roikon beluli radiomics parameterekkel
    	    #test      = pairwise.wilcox.test(df[,i],df$segmentation_method)$p.value[10,]

	    # Wilcokoxon test akkor hasznalhato, ha a mintak nem tesznek eleget a normalitas vizsgalatnak
	    # Ekkor a student T proba helyett ez jo. Valami olyasmit csinal, hogy atrendezi az adatokat,
	    # hozzatesz valamilyen szempont szerint...
	    # ket eloszlas tavolsagat hatarozza meg
    	    test      = wilcox.test(manualRadParams,segmMethodRadParams)$p.value	

    	    pvalue   = as.numeric( test );
    	    #pValues[i-from+1,j-1] = pvalue
    	    pValues[i-from+1,j] = pvalue	#pValues: soronkent toltom fel, egy sor = egy param. Egy sorban algoritmusonkent toltok fel. Dimenzio = parameterekszama x algoritmusok szama
	}
	cntr = cntr + 1
	PVals[cntr,] = pValues[i-from+1,]		#egy sor egy param, egy oszlop egy alg
	rownames(PVals)[cntr]=colnames(df)[i]		#rad paramsbol szedegetem ki
    }

    colnames(pValues) = segmMethodNames
    rownames(pValues) = colnames(df[,radParamsInGroup])

    #head(dfres[1:10,1:10])
    #                                       FCM          GMM           KM           AC          RG           RW      MAJ_6_3      MAJ_6_4      MAJ_6_5      MAJ_6_6
    #firstorder_10percentile       1.054852e-11 2.987034e-20 2.770514e-11 1.054852e-11 0.001278102 8.781316e-16 8.948429e-12 1.340985e-13 7.198637e-17 2.137595e-20
    #firstorder_90percentile       2.115973e-01 1.088371e-02 2.380338e-01 1.902470e-01 0.789953864 5.829643e-02 2.084465e-01 1.379511e-01 5.060634e-02 8.909926e-03
    #firstorder_energy             3.181054e-01 3.788517e-03 3.394843e-01 2.936290e-01 0.836625242 1.523351e-01 3.663133e-01 2.593187e-01 1.123179e-01 2.473036e-03

    dfres=data.frame(pValues)				#sorok: rad.params in rad.param.group, oszlopok: alg-ok

    #false discovery rate: FDR = FP / (FP + TP)
    #oszloponkent (algoritmusonkent) tortenik a false discovery rate kompenzalas, mert kulonbozo alg.-kat akarunk osszehasonlitani
    for(k in 1:(length(segmMethodNames)) ){ p.adjust(dfres[,k],method="fdr") }

    #install.packages("fdrtool")
    #library("fdrtool")
    #for(k in 1:(length(segmMethodNames)-1) ){ fdrtool(dfres[,k], statistic="pvalue") }

    #filter out p-value >= 5%; a kulonbseg nem szignifikans
    #pValues[which(pValues>=0.05)]=0
    #dfres[(which(dfres>=0.05,arr.ind=T))]=0

    ############################################################################################################
    #hmap_of_minusLogPValues_of_man_and_algi_vs_radPramsInGroup
    if(show_heatmap_of_pvalues==T)
    {
	fun_show_heatmap_of_pvalues(prefix,bw,dfres)
    }

    ############################################################################################################
    #corrplot_of_minusLogPValues_of_man_and_algi_vs_radPramsInGroup
    #hmap_of_minusLogPValues_of_man_and_algi_vs_radPramsInGroup
    if(show_corrplot_of_pvalues==T)
    {
	fun_show_corrplot_of_pvalues(prefix,bw,dfres)
    }

    ############################################################################################################
    #paronkenti wilcoxon test-et futtat az alg-ok osszehason-ra (a stat sokasagok nem norm. eloszlasuak /ezert wilcox es nem student-t/), hogy vajon
    #a sokasasgok mean-jei mennyire ternek el egymastol. Ha a p-value < 5%, akkor kulonboznek. Ha tobb sokasagot hasonlitunk ossze egyszerre, arra kruskal-wallis ad p-value-t
    #rm -rf ~/R/x86_64-pc-linux-gnu-library/3.4/00LOCK-rlang/
    # install.packages("https://cran.r-project.org/src/contrib/rlang_1.0.6.tar.gz",repos=NULL,type="source")
    if(show_ggboxplot_of_pvalues==T)
    {
	fun_show_ggboxplot_of_pvalues(prefix,bw,dfres,segmMethodNames)
    }

    ############################################################################################################
    #heatmap-es abrazolas, megnezem, hogy az egyes alg-okhoz tartozo radiomics params, ha osszefuggo adatokat alkotnanak (=egy populaciohoz tartoznanak), (abban az ertelemben egyformak, hogy mindegyik p-value), milyen tavol vannak egymastol
    if(show_corrplot_of_pairwise_wilcox_of_pvalues==T)
    {
	fun_show_corrplot_of_pairwise_wilcox_of_pvalues(prefix,bw,segmMethodNames,DATA.FRAME)
    }
    
    ###########################################################
    #Fisher Modszer, fuggetlen p-value-ek osszekombinalasara
    if(calc_fisherM_of_pvalues==T)
    {
        library("poolr")
        rowIndex = grep(radiomicsGroupName,radiomicsGroupNames)		# 1...7 (7 rad.para.group letezik)
        fm[rowIndex,] = apply(dfres,2,fisherM)				#apply() masodik parametere = 2, mert a dfres oszlopaibol(=alg-ok, osszesen 11db) csinal vektort es arra alkalmazza a fisherM-et. dfres sorai = rad.params in rad.param.group
    }
    
}#end of radiomicsGroupNames loop

# names(options())
# options(width=400)

stop()

#############################################################################
#vesszuk a negativ logaritmusat a Fisher p ertkekenek rad. par. csoportonkent
if(calc_fisherM_of_pvalues==T)
{
    fun_calc_fisherM_of_pvalues(prefix,bw,radiomicsGroupNames,segmMethodNames,dfres,fm)
}

###########################################################################################################
#alg-onkent osszeadom a rad.param.group-okhoz tartozo p-value-kat, igy jobban osszehasonlithatoak az alg-ok
if(calc_sum_of_fisherM_of_pvalues==T)
{
    fun_calc_sum_of_fisherM_of_pvalues(prefix,bw,segmMethodNames,radiomicsGroupNames,algs)
}

###########################################################################################################################
#szuper fisher modszer = a fisher modszerrel osszekombinalt p-value-kat ujra fisher modszerrel osszekombinalom, majd -log()
if(calc_super_fisherM_of_pvalues==T)
{
    fun_calc_super_fisherM_of_pvalues(prefix,bw,fm,segmMethodNames)
}

#################################################
#ez egyszerubb az elozore
#manualis es egyes alg-ok terfogatainak eloszlasa
if(calc_distrib_of_volumes==T)
{
    tmp=data.frame(log10(df[,22]), df[,3])
    colnames(tmp)=c("firstorder_volume","segmentation_method")
    X11()
    ggplot(data = tmp) + ggtitle("log10 of manual and segm algs volumes") + geom_histogram(aes(x = firstorder_volume), colour = "black", fill = "white") + facet_wrap(~ segmentation_method)
}

#3D
#x=seq(1,11)
#y=seq(1,7)
#z=-log10(t(fm))
#persp(x,y,z,main="title", zlab = "-log Fisher parameter",theta = 30, phi = 30,col = rainbow(max(z)+1,alpha=1)[z[-1,-1]+1], shade = 0.4,border = NA, box = TRUE)


#################################################################################################################
#vesszuk a negativ logaritmusat a Fisher p ertkekenek, de nem csoportonkent, mint a korabbi fisher-es szamolasban
if(calc2_fisherM_of_pvalues==T)
{
    fun_calc2_fisherM_of_pvalues(segmMethodNames,PVals,radiomicsGroupName,radiomicsGroupNames,data)
}

