#mnt/raid6_data/user/ogabor/norm-student-wilckoxtest.txt
# normaitas vizsgalat
#shapiro.test
# normalitas tesztek kimenetelei:
# W = 0.9712, p-value = 0.8984,  -> normalis ha p-value>5% (90%)

# Wilcokoxon test akkor hasznalhato, ha a mintak nem tesznek eleget a normalitas vizsgalatnak
# Ekkor a student T proba helyett ez jo. Valami olyasmit csinal, hogy atrendezi az adatokat,
# hozzatesz valamilyen szempont szerint...

#df=read.csv("df.csv")
df=read.csv("queried_CSVs/_bw__df.csv")
for( i in 4:111){ test=(shapiro.test(df[,i]))$p.value; if(test>=0.05){ print(paste(as.character(i),": ", as.character(test), "normal" ))} else{ print(paste(as.character(i),": ", as.character(test), "not normal" )) } }
