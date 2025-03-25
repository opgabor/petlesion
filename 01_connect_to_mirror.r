#https://hevodata.com/learn/rpostgresql/

#kezzel cli-bol:
##root@dot:~# psql -h x.x.x.x -p y -U z -W p -d q -l
#ogabor@dot:$ psql -h x.x.x.x -p y -U z           -l
#2025 utan:
#kezzel:      psql -h x.x.x.x           -U y -W         -d q

# source( "aholvan/connect_to_mirror.r" )

#install.packages('RPostgreSQL')
library('RPostgreSQL')

#dsn_hostname = "x.x.x.x"
dsn_hostname = "y.y.y.y"
dsn_port = "5432"
dsn_uid = "xx"
dsn_pwd = "yy"
dsn_database = "zzz"

tryCatch (
	{
		drv <- dbDriver("PostgreSQL")
		print("Connecting to Database…")
		connec <- dbConnect (drv, 
			dbname = dsn_database,
			host = dsn_hostname, 
			port = dsn_port,
			user = dsn_uid, 
			password = dsn_pwd
		)
		print("Database Connected!")
	},
	error=function(cond) 
	{
		print("Unable to connect to Database.")
	}
)



