#File: Rams2arlSequence.r
#Written By: Daniel M. Matross
#Date: 3/18/2004
#-----------------------------
#A script file to sequence the RAMS Metdata files we have from
#Saulo for conversion to arl. Quick and dirty with hardcoded names.
#This is just designed to call the fucntions.
#

source("/home/dmatross/RamsToARLProject/Rams2ARL.r")

#dayseq<-c(paste("200305",29:31,sep=""),paste("200306",1:30,sep=""))
#dayseq<-c("20030515")#,"20030618")
dayseq<-c(paste("2003060",1:9,sep=""),paste("200306",10:30,sep=""))

for (k in 1:length(dayseq))
{
	print(paste("Beginning execution of RamsToARL",dayseq[k]))	
	bramsFile<-paste("/deas/group/cobra/Metdata/brams_",substring(dayseq[k],5,6),"_",
			substring(dayseq[k],7,8),"_",substring(dayseq[k],1,4),"_1.bin",sep="")
	RamsToARL(dayseq[k],"b")
	system(paste("chmod g+rx",bramsFile),intern=T)
	#system(paste("gzip",bramsFile),intern=T)

}

