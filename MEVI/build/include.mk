#------------------------------------------------------------------------------------------#
#    This is the file you need to adjust depending on your system and needs.               #
#------------------------------------------------------------------------------------------#



#------ Define make (gnu makes works best) ------------------------------------------------#
MAKE = /usr/bin/make

# MEVI root directory
MEVI_ROOT=../../MEVI

#------------------------------------------------------------------------------------------#
#    HDF5 libraries. You don't have to include them if you don't want to use anything in   #
# HDF5, in which case you can set up USE_HDF5 to 0, and leave HDF5_INCS and HDF5_LIBS in   #
# blank.                                                                                   #
#------------------------------------------------------------------------------------------#
USE_HDF5=1
HDF5_INCS=/n/sw/hdf5/hdf5-1.8.1_intel-10.0.015/include
HDF5_LIBS=-L/n/sw/hdf5/hdf5-1.8.1_intel-10.0.015/lib -lhdf5 -lm
#HDF5_INCS=/group/wofsy/software/hdf5/hdf5/lib/ -I/group/wofsy/software/hdf5/hdf5/include
#HDF5_LIBS=-L/group/wofsy/software/hdf5/hdf5/lib/ -lhdf5 -lm \
           -L/group/wofsy/software/hdf5/zlib/lib -lz

#------------------------------------------------------------------------------------------#
#    NetCDF libraries. You don't have to include them if you don't want to use anything in #
# netCDF, in which case you can set up USE_NCDF to 0, and leave NCDF_INCS and NCDF_LIBS in #
# blank.                                                                                   #
#------------------------------------------------------------------------------------------#
USE_NCDF=1
NCDF_INCS=/n/sw/odyssey-apps/netcdf/3.6.3/netcdf-3.6.3/include
NCDF_LIBS=-L/n/sw/odyssey-apps/netcdf/3.6.3/netcdf-3.6.3/lib -lnetcdf
#NCDF_INCS=/group/wofsy/software/netcdf-3.6.2/include
#NCDF_LIBS=-L/group/wofsy/software/netcdf-3.6.2/lib -lnetcdf

#------------------------------------------------------------------------------------------#
#    Vis5D libraries. You don't have to include them if you don't want to use anything in  #
# Vis5D, in which case you can set up USE_V5DP to 0, and leave V5DP_INCS and V5DP_LIBS in #
# blank.                                                                                   #
#------------------------------------------------------------------------------------------#
USE_V5DP=1
V5DP_INCS=/n/sw/odyssey-apps/vis5d+-1.3.0-beta/include/vis5d+
V5DP_LIBS=-L/n/sw/odyssey-apps/vis5d+-1.3.0-beta/lib -lv5d

#------ Defining the compiler and library paths in case they are not in LD_LIBRARY_PATH ---#
CMACH=PC_LINUX1
F_COMP=ifort
C_COMP=icc
LIBS=

#------ Defining compiler options ---------------------------------------------------------#
#F_OPTS= -FR -O0 -recursive -Vaxlib -static -check all -g -fpe0 -ftz -gen-interfaces     \
	-warn interfaces -debug extended -debug inline_debug_info -debug-parameters all \
	-traceback -ftrapuv

#C_OPTS= -O0 -DLITTLE -static -g -traceback -debug extended
F_OPTS = -FR -O3 -recursive -unroll -traceback -static
C_OPTS = -DLITTLE -static -traceback

#------ Archive command -------------------------------------------------------------------#
ARCHIVE=ar rs
