#------------------------------------------------------------------------------------------#
# MEVI Makefile                                                                            #
# Compilation instructions to MEVI.                                                        #
#                                                                                          #
# WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING WARNING! WARNING!!#
# WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING WARNING! WARNING!!#
# WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING WARNING! WARNING!!#
#                                                                                          #
#    Unless you are developing the code, you should not change anything in here. You can   #
# set up the compilation to your needs by changing include.mk only!                        #
#                                                                                          #
# WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING WARNING! WARNING!!#
# WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING WARNING! WARNING!!#
# WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING WARNING! WARNING!!#
#------------------------------------------------------------------------------------------#

include include.mk

#----- Configuring paths based on the provided root folder --------------------------------#

MEVI_VERSION = 1.0

MEVI_BASE    = $(MEVI_ROOT)/build/

MEVI_DRIVER  = $(MEVI_ROOT)/src/driver
MEVI_ECMWF   = $(MEVI_ROOT)/src/ecmwf
MEVI_ERANAT  = $(MEVI_ROOT)/src/eranat
MEVI_GRADS   = $(MEVI_ROOT)/src/grads
MEVI_GRIB    = $(MEVI_ROOT)/src/grib
MEVI_HDF5    = $(MEVI_ROOT)/src/hdf5
MEVI_IO      = $(MEVI_ROOT)/src/io
MEVI_MODULES = $(MEVI_ROOT)/src/modules
MEVI_NCEP    = $(MEVI_ROOT)/src/ncep
MEVI_RAMS    = $(MEVI_ROOT)/src/rams
MEVI_UTILS   = $(MEVI_ROOT)/src/utils
MEVI_VIS5D   = $(MEVI_ROOT)/src/vis5d
MEVI_WRF     = $(MEVI_ROOT)/src/wrf

#----- Define the executable name ---------------------------------------------------------#
EXE      = $(MEVI_BASE)/mevi_$(MEVI_VERSION)
LIBMODEL = $(MEVI_BASE)/mevi_$(MEVI_VERSION).a


#----- Compiler commands. -----------------------------------------------------------------#
INCLUDES   = -I$(MEVI_INCS) -I$(HDF5_INCS) -I$(NCDF_INCS) -I$(V5DP_INCS)
ALL_LIBS   = $(LIBS) $(HDF5_LIBS) $(NCDF_LIBS) $(V5DP_LIBS)
F_COMMAND  = $(F_COMP) $(F_OPTS) $(INCLUDES) -c
C_COMMAND  = $(C_COMP) $(C_OPTS) -D$(CMACH) $(INCLUDES) -c


#----- Including the object list ----------------------------------------------------------#
include objects.mk

#----- Defining targets -------------------------------------------------------------------#
all: $(EXE)

$(EXE): $(LIBMODEL) $(MAINOBJ)
	@echo ""
	$(F_COMP) -o $(EXE) $(MAINOBJ) $(F_OPTS) $(LIBMODEL) $(ALL_LIBS)
	@echo ""
	@echo Finished building === $(EXE)
	@echo ""

$(MAINOBJ): $(MAIN)
	@echo ""
	cp -f $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90) 

$(LIBMODEL): $(OBJECTS)
	$(ARCHIVE) $(LIBMODEL) $(OBJECTS)

FORCE: 

clean: 
	@echo ""
	rm -fv $(LIBMODEL) $(EXE) *.o *.mod *.F90 *.f90 *.stb *.d
	@echo ""

#------ Defining the dependencies ---------------------------------------------------------#
include depend.mk


