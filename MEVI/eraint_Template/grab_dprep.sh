#! /bin/bash
#==========================================================================================#
#==========================================================================================#
# bash-shell script to download selected files from <server_name> using Wget               #
# NOTE: if you want to run under a different shell, make sure you change the 'set'         #
#       commands according to your shell's syntax.                                         #
# after you save the file, don't forget to make it executable                              #
#   i.e. - "chmod u+x <name_of_script>"                                                    #
#------------------------------------------------------------------------------------------#




#==========================================================================================#
#==========================================================================================#
#     Change settings here.  Don't forget to set up the domain in MEVI_IN BEFORE you run   #
# this script.                                                                             #
#------------------------------------------------------------------------------------------#

#------------------------------------------------------------------------------------------#
# Replace email and password with your password                                            #
# IMPORTANT NOTE:  If your password uses a special character that has special meaning      #
#                  to bash, you should escape it with a backslash                          #
#                  Example:  set passwd = "my\!password"                                   #
#------------------------------------------------------------------------------------------#
email="my@email.edu"
passwd="my\!password"
#------------------------------------------------------------------------------------------#



#------------------------------------------------------------------------------------------#
#       Time and file size information.                                                    #
#                                                                                          #
# MONTHA -- First month to download                                                        #
# YEARA  -- First year to download                                                         #
# MONTHZ -- Last month to download                                                         #
# YEARZ  -- Last year to download                                                          #
# SIZEOK -- Expected size (bytes) of the converted RALPH-2 file.  This is useful if you    #
#           need to resume this script.  Make sure to change it BEFORE you re-submit...    #
#------------------------------------------------------------------------------------------#
montha=1        # First month to download
yeara=2011      # First year to download
monthz=12       # Last  month to download
yearz=2012      # Last  year to download
sizeok=19828552 # Size of RALPH-2 file that is ok (useful if you need to resume this
                # script
#------------------------------------------------------------------------------------------#


#------------------------------------------------------------------------------------------#
# Experienced Wget Users: add additional command-line flags here                           #
#   - Use the -r (--recursive) option with care                                            #
#   - Do NOT use the -b (--background) option - simultaneous file downloads                #
#     can cause your data access to be blocked.                                            #
#------------------------------------------------------------------------------------------#
opts="-N"
#------------------------------------------------------------------------------------------#



#------------------------------------------------------------------------------------------#
#      If you get a certificate verification error (version 1.10 or higher), uncomment the #
# following line.                                                                          #
#------------------------------------------------------------------------------------------#
#cert_opt="--no-check-certificate"
cert_opt=""
#------------------------------------------------------------------------------------------#



#==========================================================================================#
#==========================================================================================#
#==========================================================================================#
#==========================================================================================#
#==========================================================================================#
#==========================================================================================#
#==========================================================================================#
#==========================================================================================#
#                                                                                          #
#   NO NEED TO CHANGE ANYTHING BEYOND THIS POINT UNLESS YOU ARE DEVELOPING THE SCRIPT!!!   #
#                                                                                          #
#==========================================================================================#
#==========================================================================================#
#==========================================================================================#
#==========================================================================================#
#==========================================================================================#
#==========================================================================================#
#==========================================================================================#
#==========================================================================================#




#----- Authentication file. ---------------------------------------------------------------#
authentic=auth.rda_ucar.edu
#------------------------------------------------------------------------------------------#


#----- Remove authentication file in case we resume the script. ---------------------------#
/bin/rm -fv ${authentic}
#------------------------------------------------------------------------------------------#



#------------------------------------------------------------------------------------------#
#    authenticate - NOTE: You should only execute this command ONE TIME.                   #
#    Executing this command for every data file you download may cause your download       #
# privileges to be suspended.                                                              #
#------------------------------------------------------------------------------------------#
wget ${cert_opt} -O /dev/null --save-cookies ${authentic} \
   --post-data="email=${email}&passwd=${passwd}&action=login" \
   https://rda.ucar.edu/cgi-bin/login
#------------------------------------------------------------------------------------------#

#----- Main directory. --------------------------------------------------------------------#
archroot='http://rda.ucar.edu/data/ds627.0/ei.oper.an.pl'
#------------------------------------------------------------------------------------------#


let year=${yeara}-1
while [ ${year} -lt ${yearz} ]
do
   let year=${year}+1

   #----- Create the directory if there isn't one. ----------------------------------------#
   if [ ! -s ${year} ]
   then
     mkdir ${year}
   fi

   #----- Check whether this year is leap or not, and copy the right array to daymax. -----#
   let year400=${year}%400
   let year100=${year}%100
   let year4=${year}%4
   if [ ${year400} -eq 0 ] || [ ${year4} -eq 0 -a ${year100} -ne 0 ]
   then
      daymax=( 0 31 29 31 30 31 30 31 31 30 31 30 31 )
   else
      daymax=( 0 31 28 31 30 31 30 31 31 30 31 30 31 )
   fi
   #---------------------------------------------------------------------------------------#


   #---------------------------------------------------------------------------------------#
   #     Check whether this is the first or the last year.                                 #
   #---------------------------------------------------------------------------------------#
   if [ ${year} -eq ${yeara} ]
   then
      month1st=${montha}
   else
      month1st=1
   fi
   if [ ${year} -eq ${yearz} ]
   then
      monthlast=${monthz}
   else
      monthlast=12
   fi
   #---------------------------------------------------------------------------------------#



   #---------------------------------------------------------------------------------------#
   #     Loop over months, days, and times, and download the files.                        #
   #---------------------------------------------------------------------------------------#
   let month=${month1st}-1
   while [ ${month} -lt ${monthlast} ]
   do
      #----- Update month. ----------------------------------------------------------------#
      let month=${month}+1

      #----- Make string version of month. ------------------------------------------------#
      let cmonth=${month}+100
      cmonth=`echo ${cmonth} | awk '{print substr($1,2,2)}'`

      #----- Create the month directory in case there isn't one. --------------------------#
      if [ ! -s ${year}/${cmonth} ]
      then
         mkdir ${year}/${cmonth}
      fi

      day=0
      while [ ${day} -lt ${daymax[${month}]} ]
      do
         let day=${day}+1
         #----- Make string version of day. -----------------------------------------------#
         let cday=${day}+100
         cday=`echo ${cday} | awk '{print substr($1,2,2)}'`

         hour=-6
         while [ ${hour} -lt 18 ]
         do
            let hour=${hour}+6
            #----- Make string version of day. --------------------------------------------#
            let chour=${hour}+100
            chour=`echo ${chour} | awk '{print substr($1,2,2)}'`

            yrmon=${year}${cmonth}
            ymdh=${year}${cmonth}${cday}${chour}

            #----- Build the scalar and wind file names. ----------------------------------#
            scal=${archroot}/${yrmon}/ei.oper.an.pl.regn128sc.${ymdh}
            wind=${archroot}/${yrmon}/ei.oper.an.pl.regn128uv.${ymdh}
            bind=ecmwf_interim_${ymdh}.grb
            ncdf=ecmwf_interim_${ymdh}.nc
            dprep=eraint-g01-${year}-${cmonth}-${cday}-${chour}00


            #----- Check whether we need to download the data. ----------------------------#
            if [ ! -s ${year}/${cmonth}/${dprep} ]
            then
               grabgrib=1
            else
               size=`du -sb ${year}/${cmonth}/${dprep} | awk '{print $1}'`
               if [ ${size} -eq ${sizeok} ]
               then
                  grabgrib=0
               else
                  /bin/rm -f ${year}/${cmonth}/${dprep}
                  grabgrib=1
               fi
            fi

            if [ ${grabgrib} -eq 1 ]
            then
               echo ${scal}
               #----- Download the files. -------------------------------------------------#
               wget ${cert_opt} ${opts} --load-cookies ${authentic} ${scal}
               wget ${cert_opt} ${opts} --load-cookies ${authentic} ${wind}

               scalhere=`basename ${scal}`
               windhere=`basename ${wind}`

               #----- Concatenate the grib files. -----------------------------------------#
               cat < ${scalhere} > ${bind}
               cat < ${windhere} >> ${bind}
               ncl_convert2nc ${bind}

               ./mevi -f MEVI_IN 

               /bin/mv ${dprep} ${year}/${cmonth}

               /bin/rm -f ${scalhere} ${windhere} ${bind} ${ncdf}
            fi

         done
      done
   done
done



#------------------------------------------------------------------------------------------#
#     Clean up after downloading everything.                                               #
#------------------------------------------------------------------------------------------#
/bin/rm -fv ${authentic}
#------------------------------------------------------------------------------------------#

