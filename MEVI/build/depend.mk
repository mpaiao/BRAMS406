an_header.o : $(MEVI_MODULES)/an_header.f90 mod_maxdims.o mod_time.o
	cp -f $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

charutils.o : $(MEVI_UTILS)/charutils.f90
	cp -f $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

dateutils.o : $(MEVI_UTILS)/dateutils.f90 rconstants.o mod_time.o
	cp -f $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

ecmwf_coordinates.o: $(MEVI_ECMWF)/ecmwf_coordinates.f90 an_header.o mod_grid.o mod_model.o
	cp -f $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

ecmwf_fill_infotable.o: $(MEVI_ECMWF)/ecmwf_fill_infotable.F90 mod_maxdims.o mod_model.o   \
	an_header.o mod_netcdf.o mod_ioopts.o mod_ncdf_globio.o
	cp -f $< $(<F:.F90=.F90)
	$(F_COMMAND) -DUSE_NCDF=$(USE_NCDF) $(<F:.F90=.F90)
	rm -f $(<F:.F90=.F90)

ecmwfcio.o: $(MEVI_ECMWF)/ecmwfcio.F90 mod_maxdims.o mod_model.o an_header.o mod_netcdf.o  \
	mod_ioopts.o mod_ncdf_globio.o 
	cp -f $< $(<F:.F90=.F90)
	$(F_COMMAND) -DUSE_NCDF=$(USE_NCDF) $(<F:.F90=.F90)
	rm -f $(<F:.F90=.F90)

eranat_coordinates.o: $(MEVI_ERANAT)/eranat_coordinates.f90 an_header.o mod_grid.o         \
	mod_model.o
	cp -f $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

eranat_fill_infotable.o: $(MEVI_ERANAT)/eranat_fill_infotable.F90 mod_maxdims.o            \
	mod_model.o an_header.o mod_netcdf.o mod_ioopts.o mod_ncdf_globio.o
	cp -f $< $(<F:.F90=.F90)
	$(F_COMMAND) -DUSE_NCDF=$(USE_NCDF) $(<F:.F90=.F90)
	rm -f $(<F:.F90=.F90)

eranatcio.o: $(MEVI_ERANAT)/eranatcio.F90 mod_maxdims.o mod_model.o                        \
	an_header.o mod_netcdf.o mod_ioopts.o mod_ncdf_globio.o 
	cp -f $< $(<F:.F90=.F90)
	$(F_COMMAND) -DUSE_NCDF=$(USE_NCDF) $(<F:.F90=.F90)
	rm -f $(<F:.F90=.F90)

fatal_error.o : $(MEVI_UTILS)/fatal_error.f90
	cp -f $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

filelist.o : $(MEVI_UTILS)/filelist.F90 mod_maxdims.o 
	cp -f $< $(<F:.F90=.F90)
	$(F_COMMAND) -D$(CMACH) $(<F:.F90=.F90)
	rm -f $(<F:.F90=.F90)

interputils.o : $(MEVI_UTILS)/interputils.f90 mod_ioopts.o
	cp -f $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

load_namelist.o : $(MEVI_IO)/load_namelist.f90 mod_maxdims.o mod_namelist.o mod_ioopts.o   \
	mod_model.o mod_model.o
	cp -f $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

load_variable.o: $(MEVI_IO)/load_variable.F90 an_header.o mod_ioopts.o mod_maxdims.o       \
	mod_netcdf.o mod_scratch.o mod_ncdf_globio.o mod_maxdims.o
	cp -f $< $(<F:.F90=.F90)
	$(F_COMMAND) -DUSE_NCDF=$(USE_NCDF) $(<F:.F90=.F90)
	rm -f $(<F:.F90=.F90)

map_input_vars.o : $(MEVI_IO)/map_input_vars.f90 mod_ioopts.o an_header.o mod_maxdims.o    \
	mod_model.o
	cp -f $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

mevi_coord.o : $(MEVI_DRIVER)/mevi_coord.f90 mod_model.o mod_grid.o an_header.o            \
	mod_ioopts.o
	cp -f $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

mevi_driver.o : $(MEVI_DRIVER)/mevi_driver.f90 an_header.o mod_ioopts.o
	cp -f $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

mod_grid.o : $(MEVI_MODULES)/mod_grid.f90
	cp -f $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

mod_ioopts.o : $(MEVI_MODULES)/mod_ioopts.f90 mod_maxdims.o mod_time.o
	cp -f $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

mod_maxdims.o : $(MEVI_MODULES)/mod_maxdims.f90
	cp -f $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

mod_namelist.o : $(MEVI_MODULES)/mod_namelist.f90 mod_maxdims.o mod_ioopts.o
	cp -f $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

mod_ncdf_globio.o: $(MEVI_MODULES)/mod_ncdf_globio.F90 mod_ioopts.o mod_netcdf.o           \
	mod_scratch.o rconstants.o
	cp -f $< $(<F:.F90=.F90)
	$(F_COMMAND) -DUSE_NCDF=$(USE_NCDF) $(<F:.F90=.F90)
	rm -f $(<F:.F90=.F90)

mod_netcdf.o : $(MEVI_MODULES)/mod_netcdf.F90 mod_time.o
	cp -f $< $(<F:.F90=.F90)
	$(F_COMMAND) -DUSE_NCDF=$(USE_NCDF) $(<F:.F90=.F90)
	rm -f $(<F:.F90=.F90)

mod_model.o : $(MEVI_MODULES)/mod_model.f90 mod_maxdims.o mod_time.o
	cp -f $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

mod_scratch.o : $(MEVI_MODULES)/mod_scratch.f90 mod_model.o mod_ioopts.o
	cp -f $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

mod_time.o : $(MEVI_MODULES)/mod_time.f90
	cp -f $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

mod_v5d.o : $(MEVI_MODULES)/mod_v5d.F90
	cp -f $< $(<F:.F90=.F90)
	$(F_COMMAND) -DUSE_V5DP=$(USE_V5DP) $(<F:.F90=.F90)
	rm -f $(<F:.F90=.F90)

ncep_fill_infotable.o: $(MEVI_NCEP)/ncep_fill_infotable.F90 mod_maxdims.o mod_model.o      \
	an_header.o mod_netcdf.o mod_ioopts.o mod_ncdf_globio.o
	cp -f $< $(<F:.F90=.F90)
	$(F_COMMAND) -DUSE_NCDF=$(USE_NCDF) $(<F:.F90=.F90)
	rm -f $(<F:.F90=.F90)

ncepcio.o: $(MEVI_NCEP)/ncepcio.F90 mod_maxdims.o mod_model.o      \
	an_header.o mod_netcdf.o mod_ioopts.o mod_ncdf_globio.o 
	cp -f $< $(<F:.F90=.F90)
	$(F_COMMAND) -DUSE_NCDF=$(USE_NCDF) $(<F:.F90=.F90)
	rm -f $(<F:.F90=.F90)

ralph2_output.o : $(MEVI_RAMS)/ralph2_output.f90 mod_grid.o mod_ioopts.o mod_maxdims.o     \
	mod_model.o an_header.o rconstants.o mod_scratch.o
	cp -f $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

rams_fill_infotable.o : $(MEVI_RAMS)/rams_fill_infotable.f90 an_header.o mod_maxdims.o     \
	mod_model.o mod_ioopts.o
	cp -f $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

rcio_light.o : $(MEVI_RAMS)/rcio_light.f90 mod_maxdims.o mod_model.o
	cp -f $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

rconstants.o : $(MEVI_UTILS)/rconstants.f90
	cp -f $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

rsys.o: $(MEVI_UTILS)/rsys.F90
	cp -f $< $(<F:.F90=.F90)
	$(F_COMMAND) -D$(CMACH) $(<F:.F90=.F90)
	rm -f $(<F:.F90=.F90)

therm_lib.o: $(MEVI_UTILS)/therm_lib.f90 rconstants.o
	cp -f $< $(<F:.f90=.f90)
	$(F_COMMAND) -D$(CMACH) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

utils_c.o: $(MEVI_UTILS)/utils_c.c
	$(C_COMMAND) $<

utils_f.o : $(MEVI_UTILS)/utils_f.f90
	cp -f $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

v5d_output.o : $(MEVI_VIS5D)/v5d_output.F90 mod_v5d.o mod_ioopts.o mod_maxdims.o           \
	mod_model.o an_header.o mod_grid.o
	cp -f $< $(<F:.F90=.F90)
	$(F_COMMAND) -DUSE_V5DP=$(USE_V5DP) $(<F:.F90=.F90)
	rm -f $(<F:.F90=.F90)

wcio.o: $(MEVI_WRF)/wcio.F90 mod_model.o mod_ncdf_globio.o mod_ioopts.o mod_maxdims.o      \
	mod_netcdf.o mod_scratch.o
	cp -f $< $(<F:.F90=.F90)
	$(F_COMMAND) -DUSE_NCDF=$(USE_NCDF) $(<F:.F90=.F90)
	rm -f $(<F:.F90=.F90)

wrf_coordinates.o: $(MEVI_WRF)/wrf_coordinates.F90 an_header.o mod_grid.o mod_model.o      \
	mod_netcdf.o mod_scratch.o mod_ncdf_globio.o mod_ioopts.o
	cp -f $< $(<F:.F90=.F90)
	$(F_COMMAND) -DUSE_NCDF=$(USE_NCDF) $(<F:.F90=.F90)
	rm -f $(<F:.F90=.F90)

wrf_fill_infotable.o: $(MEVI_WRF)/wrf_fill_infotable.F90 mod_maxdims.o mod_model.o         \
	an_header.o mod_netcdf.o mod_ioopts.o mod_ncdf_globio.o
	cp -f $< $(<F:.F90=.F90)
	$(F_COMMAND) -DUSE_NCDF=$(USE_NCDF) $(<F:.F90=.F90)
	rm -f $(<F:.F90=.F90)

zwrfcio.o: $(MEVI_WRF)/zwrfcio.F90 mod_model.o mod_ncdf_globio.o mod_ioopts.o              \
	mod_maxdims.o mod_netcdf.o mod_scratch.o rconstants.o
	cp -f $< $(<F:.F90=.F90)
	$(F_COMMAND) -DUSE_NCDF=$(USE_NCDF) $(<F:.F90=.F90)
	rm -f $(<F:.F90=.F90)

zwrf_fill_infotable.o: $(MEVI_WRF)/zwrf_fill_infotable.F90 mod_maxdims.o mod_model.o       \
	an_header.o mod_netcdf.o mod_ioopts.o mod_ncdf_globio.o
	cp -f $< $(<F:.F90=.F90)
	$(F_COMMAND) -DUSE_NCDF=$(USE_NCDF) $(<F:.F90=.F90)
	rm -f $(<F:.F90=.F90)
