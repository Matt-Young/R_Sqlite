# R functions for using sqlite
# marshall some values into a macro replace @ @1 @2 @3 with user supplied strings.
vars &lt;- function(s,query) {
y &lt;- strsplit(query,"")[[1]]
x &lt;- tokenize(s)
result &lt;- ""
nvar &lt;- 1
nprm &lt;- 1
#print(length(y)); print(length(x))
while(nprm &lt;= length(y) ) {
if(y[nprm] != "@") 
result &lt;-paste(result,y[nprm],sep="")
else {
ych &lt;-y[nprm+1];
if( (nprm &lt; length(y)) &amp;&amp; 
((ych == "1") || (ych == "2") || (ych == "3")) )
{	ivar &lt;- as.integer(ych)
result &lt;- paste(result,x[ivar],sep="")
nprm &lt;- nprm+1
} else {
if(nvar &lt;= length(x) ) { 
result &lt;- paste(result,x[nvar],sep="")
nvar &lt;- nvar + 1
} else 
result &lt;- paste(result,"NULL",sep="")}
}
nprm &lt;- nprm+1
}	
result	}

# Execute Sqlite with an input file
SQL &lt;- function(s,d,mode) {
if(mode == "h")	
x &lt;- paste(".moite3de tabs\n.output \"r.dat\"\n.headers on\n",s,sep="")
else  	x &lt;- paste(".mode tabs\n.output \"r.dat\"\n",s,sep="")
writeLines(x,"query.sql")
y &lt;- paste("sql ",d," &lt; query.sql", sep="")
shell(y,intern=TRUE)

# Grab the result file from an sqlite execution
if(mode == "h") read.csv("r.dat",header= TRUE,sep="\t")
else if(mode != "w") read.csv("r.dat",header=FALSE,sep="\t")
}

# write a tab delimited file from data.frame, no quotes hopefully
# read from Sqlite with: read.csv("r.dat",header=FALSE,sep="\t")
writeSql &lt;- function(k) {
f &lt;-  file("sql.dat", "wb")
if(class(k) == "data.frame") {
write.table(k,file="sql.dat",sep='\t',col.names=FALSE,row.names=FALSE)
}
}

helpSql &lt;&lt;- list(

"qraw('sql text',database,mode ='w')",
"qm(d) = get table names from db",
"qrl(t,db) read 20 from table t in db",
"qrb(t,db) read 200 from table t in db",
"qw(x,t,db) - write x to table t to database",
"qcreate(t,db,'createstring') inculude sql parent, no spaces",
"qinsert(t,db.'value string') ditto",
"qp(q,db) - Plot result from query q against db as a time series",
"qframe(tb,db,x,mode) ")

qp &lt;- function(s,db) { r &lt;- SQL(s,db,mode="r");plot(r$V1,r$V2,type = "l",main = s) }
qraw &lt;- function(s,db,mode ="r") {SQL(s,db,mode = mode)}
qw &lt;- function(tb,db,x) { writeSql(x);
SQL(vars(tb,".import  sql.dat @\n"),db,mode = "w")}
qm &lt;- function(d) {SQL(sql$master,d,mode ="h") }
qh &lt;- function() {for(i in 1:length(helpSql)) writeLines(helpSql[[i]])}
qrb &lt;- function(t,db,mode="r")
{ SQL(vars(paste(t, "200"),sql$read),db,mode=mode) }
qrl &lt;- function(t,db,mode="r") 
{ SQL(vars(paste(t, "20"),sql$read),db,mode=mode) }

qinsert &lt;- function(tb,db,v) {
x &lt;- vars(paste(tb,v),sql$insert);SQL(x,db,mode = "w")  }
qcreate &lt;- function(tb,db,s) {
x &lt;- vars(paste(tb,s),sql$create);SQL(x,db,mode="w")}
qframe &lt;- function(tb,db,x ="",mode="") {

if(  length(grep("c",mode))) {
n &lt;- names(x)
s &lt;- "("
for(i in 1:(length(n)-1)) s &lt;- paste(s,n[i],",",sep="")
s &lt;- paste(s,n[length(n)],")")
qcreate(tb,db,s) 
}
if(length(grep("w",mode))) qw(tb,db,x)
if(length(grep("r",mode))) qrb(tb,db,mode="h")
}
# a group in sql syntax
pastev &lt;- function(v) {
result &lt;- paste("(",v[1]," text",sep="")
j &lt;- 2
while(j &lt; length(v))
result &lt;- paste(result,",",v[j]," text",sep="")
paste(result,")",sep="")
}
# tokenize a string
tokenize &lt;- function(s) {
j &lt;- 0; i &lt;- 1; y &lt;- ""
x &lt;- strsplit(s,"")[[1]]
while(i &lt; length(x)  &amp;&amp; x[i] == ' ') i &lt;- i+1
while(i &lt;= length(x) ) {
j &lt;- j+1; y[j] =""
xc &lt;- x[i]
y[j] &lt;- paste(y[j],xc,sep="");
i &lt;- i+1
if(xc == "'" || xc == "(") {
if(xc == '(') xc = ')'  
while((i &lt;= length(x)) &amp;&amp; (x[i] != xc)) 
{y[j] &lt;- paste(y[j],x[i],sep=""); 
i &lt;- i+1; }
y[j] &lt;- paste(y[j],xc,sep="")
i &lt;- i+1
}
else while(i &lt;= length(x)  &amp;&amp; x[i] != ' ') {
y[j] &lt;-paste(y[j],x[i],sep="")
i &lt;- i+1
}
while(i &lt;= length(x)  &amp;&amp; x[i] == ' ') i &lt;- i+1
}
y }

###  The rest are a few Sql query strings that are useful.
sql &lt;&lt;-list(
read = "select * from @ limit @;",
master = "select * from sqlite_master;",
offset = "select * from @ limit @ offset @;",
insert = "insert into @ values @;",
create = "drop table if exists @1; create table @1 @2 ;" 
)