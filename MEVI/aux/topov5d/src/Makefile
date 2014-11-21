#------------------------------------------------------------------------------------------#
# Makefile for the conversor of ECMWF to dprep (version 1.0, works only with plain binary) #
#------------------------------------------------------------------------------------------#


#----- Root directory ---------------------------------------------------------------------#
ROOTDIR=.

#------------------------------------------------------------------------------------------#
# FORTRAN 90/95 compiler specifications:                                                   #
# FCOMP is the fortran 90/95 compiler (e.g. gfortran, ifort, pgf95, etc.);                 #
# FOPTIONS is the list of options you want to add (plan accordingly to the compiler        #
#         (If you don't know what to add, you're more than welcome to leave it blank...)   #
#         You have to add the option for recursive calls:                                  #
#         In pgf90, add -Mrecursive; in ifort, add -recursive; in gfortran, it's default...#
#------------------------------------------------------------------------------------------#
FCOMP=ifort
FOPTIONS=-recursive -assume byterecl

#----- Name of the output executable file that you want -----------------------------------#
EXE=maketopo.x

#----- List of files to be compiled -------------------------------------------------------#
FFILES= $(ROOTDIR)/actualbin.f90     \
        $(ROOTDIR)/convtopo.f90      \
        $(ROOTDIR)/gettime.f90       \
        $(ROOTDIR)/maketopo.f90      \
        $(ROOTDIR)/match.f90         \
        $(ROOTDIR)/readctl.f90       \
        $(ROOTDIR)/swap32.f90        \
        $(ROOTDIR)/tolower.f90       \
        $(ROOTDIR)/updatetime.f90    \
        $(ROOTDIR)/usgstopo.f90      \
        $(ROOTDIR)/writetopo.f90


#------------------------------------------------------------------------------------------#
# Perhaps you don't need to change anything beyond this point...                           #
#------------------------------------------------------------------------------------------#

FOBJS=$(FFILES:.f90=.o)


all: $(EXE) 

$(EXE): $(FFILES)
	$(FCOMP) $(FOPTIONS) -c $?
	$(FCOMP) $(FOPTIONS) $(FOBJS) -o $(EXE)

clean:
	/bin/rm -f *.o $(EXE)

FORCE:
