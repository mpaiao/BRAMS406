EXE = brams406_2_arl.exe

LIB1 = ./lib/librams.a
#LIB2 = ../../source/lib_STD.a

#JCL: 
LIBDIR = ./lib

#JCL: FCOMP = f90
FCOMP = mpif90
#FCOMP = gfortran
#   flags for 'f90'
#FFLAGS = -c -O -free -I.
#   flags for 'pgf90'
#FFLAGS = -c -O -I.

#FFLAGS = -O -Mlfs -Mbounds -I.
FFLAGS = -O3 -Vaxlib -traceback -CB  -lowercase -assume byterecl
#FFLAGS = -O -ffree-line-length-none
#JCL: explicitly list out the source files
FFILES = adim2.f90 \
         cfgset.f90 \
         comp_arl.f90 \
         juldate.f90 \
         module_anheader.f90 \
         pakini.f90 \
         pakinp.f90 \
         pakndx.f90 \
         pakout16.f90 \
         pakrec.f90 \
         pakset.f90 \
         brams406_arl.f90 \
         readrams.f90 

#.f.o:
#*.o: $(FFILES)
#	rm -f $@
#JCL:	$(FCOMP) $(FFLAGS) $*.f
#	$(FCOMP) $(FFLAGS) *.f90
#	$(FCOMP) $(FFLAGS) $(FFILES)

MODULES = module_anheader.o

FOBJS = adim2.o    \
	cfgset.o     \
	comp_arl.o     \
	juldate.o     \
	pakini.o     \
	pakinp.o     \
	pakndx.o     \
	pakout.o     \
	pakrec.o     \
	pakset.o     \
	brams406_arl.o \
	readrams.o 

$(EXE) : $(FFILES)
	$(FCOMP) -o $(EXE) $(FFILES) $(FFLAGS) $(LIB1)
#$(EXE) : $(FFILES)
#	$(FCOMP) $(FFLAGS) $(FFILES) -Mbounds -o $(EXE) $(LIB1)
#test.exe : $(MODULES) $(FOBJS)
#	$(FCOMP) -o $@ $(FOBJS) $(MODULES) $(LIB1)  

clean:
	rm -f *.o *.mod brams406_2_arl.exe

