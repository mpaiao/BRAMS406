#File: Rams2ARL.r
#Written By: Daniel M. Matross
#Date: 3/12/2004
#------------------------------
#Functions designed to execute the conversion of met data from RAMS format
#to ARL format for use in the STILT model. Required inputs:
#
#inpath = directory of tarred file
#outpath = directory to which ARL file is written.
#date = date to converted in yyyymmdd format
#rams.origin = "h" for Harvard (David Medvigy) or "b" for Brazil (Saulo Freitas)
#jobpath = directory where the job will be executed and logged
#
#Note also that the executable file rams2arl.exe must be present in the folder
#specified by the 'jobpath' variable
#


#Function: RamsToARL
#usage: RamsToARL("20030525","b",inpath="/deas/group/stilt/Metdata/Rams",outpath="/deas/group/stilt/")
#-----------------------------------------------------------------------------------------
#Top level function to untar, read headers, write and execute .job file for Rams2ARL
#

RamsToARL<-function(dd,rams.origin,inpath="/deas/group/cobra/Metdata/RAMS/",outpath="/deas/group/cobra/Metdata/",
			jobpath="/deas/group/cobra/Metdata/RAMS/Rams2arlJobs/")
{
	if(nchar(dd)!=8)
		stop("Proper date format is a character string: YYYYMMDD")
	if(!(rams.origin=="h" | rams.origin =="b"))
		stop("Rams origin must be 'h' (Harvard) or 'b' (Brazil)")
	if(substring(inpath,nchar(inpath),nchar(inpath))!="/"    |
	   substring(outpath,nchar(outpath),nchar(outpath))!="/" |
	   substring(jobpath,nchar(jobpath),nchar(jobpath))!="/" )
		stop("All path names must end with a forward slash")
		
	fileList.nosort<-UntarRams(dd,inpath)
	
	
	ramsFileList<-fileList.nosort[which(substring(fileList.nosort,nchar(fileList.nosort)-3,nchar(fileList.nosort))==".vfm")]
	ramsHeadFile<-fileList.nosort[which(substring(fileList.nosort,nchar(fileList.nosort)-14,nchar(fileList.nosort)-4)=="000000-head")]
	
	n.zlayers<-GetNumRamsZLayers(ramsHeadFile)
	tint<-GetRamsTint(ramsFileList)
	
	jobfile<-WriteJobFile(dd,rams.origin,jobpath,outpath,n.zlayers,tint,ramsFileList,ramsHeadFile)
	

	logfile<-paste(jobpath,"rams2arl",dd,".log",sep="")
	system(paste("chmod a+rx ",jobfile,sep=""),intern=T)
	system(paste(jobfile," > ",logfile,sep=""),intern=T)
	RmRamsFiles(fileList.nosort,inpath)
}

#Function: WriteJobFile
#usage: filename<-WriteJobFile()
#---------------------------------------------
#File to write the job file for a given day of rams run. The job file is an executable that
#has all necessary information for the rams2arl function--which is called.
#

WriteJobFile<-function(dd,rams.origin,jobpath,outpath,n.zlayers,tint,infileList,ramsheadfile)
{
	jobfile<-paste(jobpath,"rams2arl",dd,".job",sep="")
	fileList<-paste("                   |",infileList,sep="")
	fileList[1]<-paste("RAMS files         |",infileList[1],sep="")
	outfile<-paste(outpath,rams.origin,"rams_",substring(dd,5,6),"_",substring(dd,7,8),"_",substring(dd,1,4),sep="")
	startday<-GetddFromFilen(infileList[1])
	endday<-GetddFromFilen(infileList[length(infileList)])

#Added 4/6/2004 attempt to deal with incorrect file ending by changing 23 to 24
#	if(substring(endday,nchar(endday)-1,nchar(endday))=="23")
#		endday<-paste(substring(endday,1,nchar(endday)-2),"24",sep="")
#end new addition

	nramsfiles<-length(infileList)	

	header1<-"#-------------------------------------------------------#"
	header2<-paste("#Rams2ARL",dd,".job: Written ",date()," #",sep="")
	header3<-"#Produced by RamsToARL.r, written by Daniel M. Matross  #"
	header4<-"#Adapted from University of Houston script              #"	
	header5<-"#-------------------------------------------------------#"
	comment1<-"# # of BKSP set to 1 (to not use dummy scalars at index 1, and dummy fluxes at index nnxp etc)"
	line1<-paste(jobpath,"rams2arl.exe << ieof",sep="")
	line2<-"RAMS Grid ID       |1"
	line3<-paste("# of layers in Z   |",n.zlayers,sep="")
	line4<-"# of BKSP          |1"
	line5<-paste("Start/end date     |",startday," ",endday,sep="")
	line6<-paste("OUTPUT file        |",outfile,sep="")
	line7<-paste("RAMS Header file   |",ramsheadfile,sep="")
	line8<-paste("Time interval(sec) |",tint,sep="")
	line9<-paste("#RAMS files to proc|",nramsfiles,sep="")
	endline<-"ieof"

	linesToWrite<-c(header1,
			header2,
			header3,
			header4,
			header5,
			comment1,
			line1,
			line2,
			line3,
			line4,
			line5,
			line6,
			line7,
			line8,
			line9,
			fileList,
			endline)

	write(linesToWrite,file=jobfile,append=F)	
	return(jobfile)
}

#Function: UntarRAMS
#usage: fileList<-UntarRAMS("20030530","/deas/group/stilt/RAMS")
#------------------------------------------------
#A function to untar and unzip a RAMS file that has been tarred. Utilizes LINUX
#system commands. Requires a date in YYYYMMDD and path for the tarile and 
#returns a list of the files it has created with full path information.
#

UntarRams<-function(dd,inpath)
{
	origPath<-system("pwd",intern=T)
	system(paste("cd",inpath),intern=T)
	
	tarfile<-paste(inpath,GetMonthFromdd(dd),".tar",sep="")
	print(tarfile)
	rawFiles<-system(paste("tar -vxf",tarfile),intern=T)
	rawFiles.fullpath<-paste(inpath,rawFiles,sep="")
	
	fileList.nosort<-rawFiles.fullpath
	for(i in 1:length(rawFiles.fullpath)) {
		system(paste("gunzip ", rawFiles.fullpath[i],sep=""),intern=T)
		fileList.nosort[i]<-substring(rawFiles.fullpath[i],1,nchar(rawFiles.fullpath[i])-3)	
	}

	system(paste("cd",origPath),intern=T)
	return(fileList.nosort)
}

#Function: GetNumRamsZLayers
#usage: n.zlayers<-GetNumRamsZLayers(headFile)
#---------------------------------------------
#Number of layers in the z coordinate used in the RAMS model run. This is information
#contained in the header files. It is extracted here from a single header file 
#and the other header files are assumed to contain the same information on the number
#of vertical layers. Requires a file name and returns the value of the nnzp variable
#in the header, or errors and stops the program.

GetNumRamsZLayers<-function(infile)
{
	formchecker<-scan(infile, what=character(),skip=103,n=1)
	if(formchecker!="__nnzp")
		stop("Header file format not recognized")
	dimchecker<-scan(infile,what=numeric(),skip=104,n=1)
	if(dimchecker!=1)
		stop("Improper dimensions for n.z")
	nnzp<-scan(infile,what=numeric(),skip=105,n=1)
	return(nnzp)
}


#Function: GetRamsTint
#usage: tint<-GetRamsTint(fileList)
#----------------------------------
#Function to return the time interval (in seconds) used in each rams file. The start time
#information is contained in the file names, thus differences are used to determine
#what the interval is. Requires a list of filenames and returns an integer count of 
#seconds in the interval.

GetRamsTint<-function(fileList)
{
	hh<-as.numeric(substring(fileList,nchar(fileList)-12,nchar(fileList)-11))
	mm<-as.numeric(substring(fileList,nchar(fileList)-10,nchar(fileList)-9))
	ss<-as.numeric(substring(fileList,nchar(fileList)-8,nchar(fileList)-7))

	julsecs<-3600*hh+60*mm+ss

	tdiff<-diff(julsecs)
	if(length(unique(tdiff))!=1)
		stop("Error in File List: Time interval not consistent")
	return(unique(tdiff))
}

#Sub-Function: GetddFromFilen
#----------------------------
#Function to retrieve date in YYYYMMDDHH format from Rams file name format.
#

GetddFromFilen<-function(infilen)
{
	yy<-substring(infilen,nchar(infilen)-23,nchar(infilen)-20)
	mm<-substring(infilen,nchar(infilen)-18,nchar(infilen)-17)
	dd<-substring(infilen,nchar(infilen)-15,nchar(infilen)-14)
	hh<-substring(infilen,nchar(infilen)-12,nchar(infilen)-11)
	nn<-substring(infilen,nchar(infilen)-10,nchar(infilen)-9)
	outdate<-paste(yy,mm,dd,hh,nn,sep="")
	return(outdate)
}

#Sub-Function: GetMonthFromdd
#----------------------------
#Function to retrieve date in MONDD Format from YYYYMMDD format
#

GetMonthFromdd<-function(dd)
{
	mm<-substring(dd,5,6)
	dy<-substring(dd,7,8)
	mon<-switch(as.integer(mm),"JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")

	if(is.null(mon))
		return(NULL)
	outday<-paste(mon,dy,sep="")		
	return(outday)
}

#Sub-Function: RmRamsFiles
#-------------------------
#Function to remove Rams Files opened by UntarRams. Also traces the directory structure
#set up and elimnates newly created directories.
#

RmRamsFiles<-function(fileList, inpath)
{
	for(p in 1:length(fileList))
		system(paste("rm", fileList[p]), intern=T)
	
	longdir.raw<-substring(fileList[1],nchar(inpath)+1,nchar(fileList[1]))
	longdir<-strsplit(longdir.raw,"/")[[1]] 
	if(length(longdir)<2) #don't remove single directory
		return()
	longdir<-longdir[1:(length(longdir)-1)] #remove filename; want directory only

	dirList<-NULL
	for (k in 0:(length(longdir)-1)) {
		addDir<-inpath	
		for(m in 1:(length(longdir)-k)) {
			addDir<-paste(addDir,longdir[m],"/",sep="")
		}
		dirList<-append(dirList,addDir)	
	}
		
	for(j in 1:length(dirList))
		system(paste("rmdir",dirList[j]),intern=T)
	
	return()
}
