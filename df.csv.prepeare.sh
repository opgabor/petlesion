#prefix=""
#prefix="cut_"
#prefix="filtered_"
#prefix="bw_cut_"
prefix="bw_filtered_"
#bw=''
#bw=0.2                 #ha a prefix bw-vel kezdodik, akkor a rekordok szama 2x annyi, mert van bin_width=0.2 es =0.31-gyel szamolt sor is
bw=0.31

echo "file: " $prefix"_bw_"$bw"_"df.csv

cat $prefix"_bw_"$bw"_"df.csv | sed -r 's/(.*)manual(.*)/\1w_manual\2/g' > df_preproc.csv
#grep -v 'mask' df_preproc.csv >> df_preproc1.csv
#mv df_preproc1.csv $prefix"_bw_"$bw"_"df.csv

mv df_preproc.csv $prefix"_bw_"$bw"_"df.csv
rm -rf df_preproc.csv