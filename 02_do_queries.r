#dbSendQuery(con, statement, …) 	Queries one statement to the database. 
#dbGetQuery(con,statement, …) 		In one operation, it submits, executes, and extracts output.

#df1 annyival tobb mint a tobbi df, hogy az elso oszlop a syudyInstanceUID, utana a study_id es segmentacios modszer. Ezeket a tobbibol trolom.
#minden df ugyanannyi rekordot tartalmaz, mert azok vannak lekerve, ahol a processed=1, es van manualis modszer ezek a kezzel maszkos feldolgozasok

#a studies tabla szerkezete
# id  |  x  |  y  |  z  |    taj    | bno | study_date | result | study_description 

#preprocesszalasok: 1 alapeset-semmi nem tortent, 1 vagas, 1 morfologiai elofeld es a binwidth-el szamolt esetek a cut-ra es filterre
#					bw
#            firstorder_params		''
#        cut_firstorder_params		''		alul, felul elhagyunk 1-1 szeletet
#   filtered_firstorder_params		''		elotte szurjuk open/close-zal a maszkokat
#     bw_cut_firstorder_params	x2	0.2/0.31	a radiomics parameter szamolo pyradiomics a hisztogramokat adott binwidth-del szamolja, es elhagyunk alul felul 1-1 szeletet
#bw_filtered_firstorder_params	x2	0.2/0.31	a radiomics parameter szamolo pyradiomics a hisztogramokat adott binwidth-del szamolja, elotte szurjuk open/close-zal a maszkokat


#igy lehet lekerdeznim, hogy hany darab table neveben van benne a firstorder:
#psql -h x.x.x. -U y -W -d z
#pw
#petlesion=> select table_name from information_schema.tables where table_name similar to '%firstorder_%';
#          table_name           
#-------------------------------
# firstorder_params
# cut_firstorder_params
# filtered_firstorder_params
# bw_cut_firstorder_params	x2
# bw_filtered_firstorder_params	x2
#(5 rows)

#a firstorder_params-ban azert van egy id-hoz 39 eset rendelve, mert ebben a tabalaban ki van szamolva a 0.2 es 0.31 es 0-as bin_width-el is minden
# select id,study_id,segmentation_method,bin_cnt,bin_width  from firstorder_params where study_id=7;
#  id   | study_id | segmentation_method | bin_cnt | bin_width 
#-------+----------+---------------------+---------+-----------
# 12050 |        7 | nnunet531           |       0 |      0.31
# 12562 |        7 | manual              |       0 |      0.31
#...
# 12609 |        7 | nnunet531           |       0 |       0.2
# 13121 |        7 | manual              |       0 |       0.2
# 13122 |        7 | AC                  |       0 |       0.2
#...
# 13637 |        7 | manual              |      13 |         0
#(39 rows)

#a cut_firstorder_params egy id-hoz 13 eset van rendelve mert itt a bin_count a sturges formula szerint van szamolva a binÜwidth nem erdekes
#petlesion=> select id,study_id,segmentation_method,bin_cnt,bin_width  from cut_firstorder_params where study_id=7;
#  id  | study_id | segmentation_method | bin_cnt | bin_width 
#------+----------+---------------------+---------+-----------
# 3256 |        7 | nnunet531           |      12 |         0
# 3155 |        7 | manual              |      12 |         0
#...
#(13 rows)

#uigy a filtered_firstorder_params-ban is 13 eset van
#petlesion=> select id,study_id,segmentation_method,bin_cnt,bin_width  from filtered_firstorder_params where study_id=7;
#  id  | study_id | segmentation_method | bin_cnt | bin_width 
#------+----------+---------------------+---------+-----------
# 2454 |        7 | nnunet531           |      11 |         0
#...
#(13 rows)

#bw_cut_firstorder_params-ban 26 eset van
#petlesion=> select id,study_id,segmentation_method,bin_cnt,bin_width  from bw_cut_firstorder_params where study_id=7;
#  id  | study_id | segmentation_method | bin_cnt | bin_width 
#------+----------+---------------------+---------+-----------
# 7581 |        7 | nnunet531           |       0 |      0.31
# 7646 |        7 | nnunet531           |       0 |       0.2
#...
#(26 rows)

#uigy a bw_filtered_firstorder_params-ban is 26 eset van
#petlesion=> select id,study_id,segmentation_method,bin_cnt,bin_width  from bw_filtered_firstorder_params where study_id=7;
#  id  | study_id | segmentation_method | bin_cnt | bin_width 
#------+----------+---------------------+---------+-----------
# 3414 |        7 | nnunet531           |       0 |       0.2
# 3365 |        7 | nnunet531           |       0 |      0.31
#...
#(26 rows)

#Osszesen 7 tabla jon le

####################
### _bw__df.csv* ###
####################
#prefix=""

########################
### cut__bw__df.csv* ###
########################
#prefix="cut_"

#############################
### filtered__bw__df.csv* ###
#############################
#prefix="filtered_"

###############################
### bw_cut__bw_0.2_df.csv*  ###
### bw_cut__bw_0.31_df.csv* ###
###############################
#prefix="bw_cut_"

###################################
### bw_filtered__bw_0.2_df.csv* ###
#bw_filtered__bw_0.31_df.csv*   ###
###################################
prefix="bw_filtered_"
    
#bw=''			#ha nincs bw, akkor ezt be kell allitani ures stringre, mert az R megjegyzi a korabbi bw erteket
#bw=0.2			#ha a prefix bw-vel kezdodik, akkor a rekordok szama 2x annyi, mert van bin_width=0.2 es =0.31-gyel szamolt sor is
bw=0.31


#select * from firstorder_params where study_id in (select distinct study_id from cut_firstorder_params)
#select * from firstorder_params where study_id in (select distinct study_id from cut_firstorder_params) and 

#azok az esetek kellenek, ahol a studies tablalban a processed flag 1-re van billentve + ahol van manualis modszer, ezek alapjan leszukitem a study_id-k halmazat
if(bw=='')
{
    #a prefix vagy cut vagy filtered vagy ures
    SQL=paste0("select studies.study_instance_uid,",prefix,"firstorder_params.* from ",prefix,"firstorder_params inner join studies on 
        ",prefix,"firstorder_params.study_id=studies.id 
        where 
    	    study_id in (select id from studies where processed=1 order by id ASC) 
        and 
	    study_id in (select study_id from firstorder_params where firstorder_params.segmentation_method='manual') 
	and 
	    bin_width=0 order by study_id,segmentation_method;")	#bin_width=0 azert kell mert a fotablaban (firstorder_params,glcm_,glrlm_,stb.params) Dani kiszamolta a binwidth=0.2 es 0.31 eseteket is ami felesleges munka volt. 
									#Ha bin_width=0, akkor a bin_cnt nem nulla. A bin_count-ot a sturges formulaval szamoltuk ki.
}else
{
    SQL=paste0("select studies.study_instance_uid,",prefix,"firstorder_params.* from ",prefix,"firstorder_params inner join studies on 
        ",prefix,"firstorder_params.study_id=studies.id 
        where 
    	    study_id in (select id from studies where processed=1 order by id ASC) 
        and 
	    study_id in (select study_id from firstorder_params where firstorder_params.segmentation_method='manual') 
	and 
	    bin_width=",bw," order by study_id,segmentation_method;")
}
df1 <- dbGetQuery(connec, SQL )
df1$id=NULL
df1$bin_cnt=NULL
df1$bin_width=NULL
df1$note=NULL


if(bw=='')
{
SQL=paste0("select * from ",prefix,"glcm_params 
	where 
	    study_id in (select id from studies where processed=1 order by id ASC ) 
	and 
	    study_id in (select study_id from firstorder_params where firstorder_params.segmentation_method='manual' ) 
	and 
	    bin_width=0 order by study_id,segmentation_method;")	#bin_width=0 azert kell mert a fotablaban (firstorder_params,glcm_,glrlm_,stb.params) Dani kiszamolta a binwidth=0.2 es 0.31 eseteket is ami felesleges munka volt. 
									#Ha bin_width=0, akkor a bin_cnt nem nulla. A bin_count-ot a sturges formulaval szamoltuk ki.
}else
{
SQL=paste0("select * from ",prefix,"glcm_params       
	where 
	    study_id in (select id from studies where processed=1 order by id ASC ) 
	and 
	    study_id in (select study_id from firstorder_params where firstorder_params.segmentation_method='manual' ) 
	and 
	    bin_width=",bw," order by study_id,segmentation_method;")
}
df2 <- dbGetQuery(connec, SQL )
df2$id=NULL
df2$bin_cnt=NULL
df2$bin_width=NULL
df2$study_id=NULL
df2$segmentation_method=NULL
df2$note=NULL

if(bw=='')
{
SQL=paste0("select * from ",prefix,"gldm_params 
	where  
	    study_id in (select id from studies where processed=1 order by id ASC) 
	and 
	    study_id in (select study_id from firstorder_params where firstorder_params.segmentation_method='manual' ) 
	and 
	    bin_width=0 order by study_id,segmentation_method;")	#bin_width=0 azert kell mert a fotablaban (firstorder_params,glcm_,glrlm_,stb.params) Dani kiszamolta a binwidth=0.2 es 0.31 eseteket is ami felesleges munka volt. 
									#Ha bin_width=0, akkor a bin_cnt nem nulla. A bin_count-ot a sturges formulaval szamoltuk ki.
}else
{
SQL=paste0("select * from ",prefix,"gldm_params 
	where 
	    study_id in (select id from studies where processed=1 order by id ASC) 
	and 
	    study_id in (select study_id from firstorder_params where firstorder_params.segmentation_method='manual' ) 
	and 
	    bin_width=",bw," order by study_id,segmentation_method;")
}
df3 <- dbGetQuery(connec, SQL )
df3$id=NULL
df3$bin_cnt=NULL
df3$bin_width=NULL
df3$study_id=NULL
df3$segmentation_method=NULL
df3$note=NULL

if(bw=='')
{
SQL=paste0("select * from ",prefix,"glrlm_params 
	where 
	    study_id in (select id from studies where processed=1 order by id ASC) 
	and 
	    study_id in (select study_id from firstorder_params where firstorder_params.segmentation_method='manual' ) 
	and 
	    bin_width=0 order by study_id,segmentation_method;")	#bin_width=0 azert kell mert a fotablaban (firstorder_params,glcm_,glrlm_,stb.params) Dani kiszamolta a binwidth=0.2 es 0.31 eseteket is ami felesleges munka volt. 
									#Ha bin_width=0, akkor a bin_cnt nem nulla. A bin_count-ot a sturges formulaval szamoltuk ki.
}else
{
SQL=paste0("select * from ",prefix,"glrlm_params 
	where 
	    study_id in (select id from studies where processed=1 order by id ASC) 
	and 
	    study_id in (select study_id from firstorder_params where firstorder_params.segmentation_method='manual' ) 
	and 
	    bin_width=",bw," order by study_id,segmentation_method;")
}
df4 <- dbGetQuery(connec, SQL )
df4$id=NULL
df4$bin_cnt=NULL
df4$bin_width=NULL
df4$study_id=NULL
df4$segmentation_method=NULL
df4$note=NULL

if(bw=='')
{
SQL=paste0("select * from ",prefix,"glszm_params 
	where 
	    study_id in (select id from studies where processed=1 order by id ASC) 
	and 
	    study_id in (select study_id from firstorder_params where firstorder_params.segmentation_method='manual' ) 
	and 
	    bin_width=0 order by study_id,segmentation_method;")	#bin_width=0 azert kell mert a fotablaban (firstorder_params,glcm_,glrlm_,stb.params) Dani kiszamolta a binwidth=0.2 es 0.31 eseteket is ami felesleges munka volt. 
									#Ha bin_width=0, akkor a bin_cnt nem nulla. A bin_count-ot a sturges formulaval szamoltuk ki.
}else
{
SQL=paste0("select * from ",prefix,"glszm_params 
	where 
	    study_id in (select id from studies where processed=1 order by id ASC) 
	and 
	    study_id in (select study_id from firstorder_params where firstorder_params.segmentation_method='manual' ) 
	and 
	    bin_width=",bw," order by study_id,segmentation_method;")
}
df5 <- dbGetQuery(connec, SQL )
df5$id=NULL
df5$bin_cnt=NULL
df5$bin_width=NULL
df5$study_id=NULL
df5$segmentation_method=NULL
df5$note=NULL

if(bw=='')
{
SQL=paste0("select * from ",prefix,"ngtdm_params 
	where 
	    study_id in (select id from studies where processed=1 order by id ASC) 
	and 
	    study_id in (select study_id from firstorder_params where firstorder_params.segmentation_method='manual' ) 
	and 
	    bin_width=0 order by study_id,segmentation_method;")	#bin_width=0 azert kell mert a fotablaban (firstorder_params,glcm_,glrlm_,stb.params) Dani kiszamolta a binwidth=0.2 es 0.31 eseteket is ami felesleges munka volt. 
									#Ha bin_width=0, akkor a bin_cnt nem nulla. A bin_count-ot a sturges formulaval szamoltuk ki.
}else
{
SQL=paste0("select * from ",prefix,"ngtdm_params 
	where 
	    study_id in (select id from studies where processed=1 order by id ASC) 
	and 
	    study_id in (select study_id from firstorder_params where firstorder_params.segmentation_method='manual' ) 
	and 
	    bin_width=",bw," order by study_id,segmentation_method;")
}
df6 <- dbGetQuery(connec, SQL )
df6$id=NULL
df6$bin_cnt=NULL
df6$bin_width=NULL
df6$study_id=NULL
df6$segmentation_method=NULL
df6$note=NULL

if(bw=='')
{
SQL=paste0("select * from ",prefix,"shape_params 
	where 
	    study_id in (select id from studies where processed=1 order by id ASC) 
	and 
	    study_id in (select study_id from firstorder_params where firstorder_params.segmentation_method='manual' ) 
	and 
	    bin_width=0 order by study_id,segmentation_method;")	#bin_width=0 azert kell mert a fotablaban (firstorder_params,glcm_,glrlm_,stb.params) Dani kiszamolta a binwidth=0.2 es 0.31 eseteket is ami felesleges munka volt. 
									#Ha bin_width=0, akkor a bin_cnt nem nulla. A bin_count-ot a sturges formulaval szamoltuk ki.
}else
{
SQL=paste0("select * from ",prefix,"shape_params 
	where 
	    study_id in (select id from studies where processed=1 order by id ASC) 
	and 
	    study_id in (select study_id from firstorder_params where firstorder_params.segmentation_method='manual' ) 
	and 
	    bin_width=",bw," order by study_id,segmentation_method;")
}
df7 <- dbGetQuery(connec, SQL )
df7$id=NULL
df7$bin_cnt=NULL
df7$bin_width=NULL
df7$study_id=NULL
df7$segmentation_method=NULL
df7$note=NULL

df=cbind(df1,df2,df3,df4,df5,df6,df7)

rm(df1,df2,df3,df4,df5,df6,df7)

#df=read.csv("df.csv")
write.csv(df, paste0("queried_CSVs/",prefix,"_bw_",bw,"_df.csv"), row.names=FALSE, quote=FALSE)
#df=readRDS(file="df.Rda")
#saveRDS(df, file="df.Rda")
