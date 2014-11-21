#!/bin/bash
#----------------------------------------------------------------
# University of Houston
# Air Quality Modeling and Monitoring Center
# 218 Old Science Building
# Houston, TX 77204-5048
# USA
#
# email: sbkim@math.uh.edu (Dr. Seung-Bum Kim)
#        dwbyun@math.uh.edu (Dr. Daewon W. Byun)
#----------------------------------------------------------------
if [ 'x'${1} = 'xclean' ]
then
   /bin/rm -v brams406_2_arl.exe 2> /dev/null
   /bin/rm -v ./lib/*.o ./lib/*.a 2> /dev/null
else 
   /bin/rm -v brams406_2_arl.exe 2> /dev/null
   /bin/rm -v ./lib/*.o ./lib/*.a 2> /dev/null
   (cd ./lib; make -f Make.lib)
   make
fi
