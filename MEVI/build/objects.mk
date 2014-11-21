
# Define file/object containing the main program
MAIN    = $(MEVI_DRIVER)/mevi_main.f90
MAINOBJ = mevi_main.o

# Define objects:
OBJECTS =                       \
	an_header.o             \
	charutils.o             \
	dateutils.o             \
	ecmwf_coordinates.o     \
	ecmwf_fill_infotable.o  \
	ecmwfcio.o              \
	eranat_coordinates.o    \
	eranat_fill_infotable.o \
	eranatcio.o             \
	fatal_error.o           \
	filelist.o              \
	interputils.o           \
	load_namelist.o         \
	load_variable.o         \
	map_input_vars.o        \
	mevi_coord.o            \
	mevi_driver.o           \
	mod_grid.o              \
	mod_ioopts.o            \
	mod_maxdims.o           \
	mod_model.o             \
	mod_namelist.o          \
	mod_netcdf.o            \
	mod_scratch.o           \
	mod_time.o              \
	mod_v5d.o               \
	mod_ncdf_globio.o       \
	ncep_fill_infotable.o   \
	ncepcio.o               \
	ralph2_output.o         \
	rams_fill_infotable.o   \
	rcio_light.o            \
	rconstants.o            \
	rsys.o                  \
	therm_lib.o             \
	utils_c.o               \
	utils_f.o               \
	v5d_output.o            \
	wcio.o                  \
	wrf_coordinates.o       \
	wrf_fill_infotable.o    \
	zwrfcio.o               \
	zwrf_fill_infotable.o
