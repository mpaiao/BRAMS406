!==========================================================================================!
!==========================================================================================!
!  PROGRAM MEVI - Meteorological Evaluation and Visualization Interface                    !
!  Developed by Marcos Longo - EPS/Harvard University - July 2008.                         !
!                                                                                          !
!     This program converts meteorological files from the most common formats to Vis-5D    !
! and GrADS.                                                                               !
!                                                                                          !
!     Supported formats include:                                                           !
!                                                                                          !
!     1. NetCDF; 2. HDF5; 3. GRIB; 4. GrADS; 5. (B)RAMS vfm; 6. VIS-5D;                    !
!==========================================================================================!
!==========================================================================================!
program mevi_main
   use mod_maxdims, only : &
           maxstr          ! ! Maximum string length
   implicit none
   
   !---------------------------------------------------------------------------------------!
   !   Variable declaration section:                                                       !
   !---------------------------------------------------------------------------------------!
   integer                              :: numarg               ! # of arguments
   integer                              :: arg                  ! Argument counter
   character(len=maxstr), dimension(2)  :: arguments            ! List of arguments
   character(len=maxstr)                :: mevi_in              ! Namelist name
   character(len=12)                    :: c0,c1                ! Just to print the banner
   !
   real                                 :: t1,w1,w2,wtime_start ! For time calculation
   real, external                       :: walltime             ! To compute runtime
   !---------------------------------------------------------------------------------------!
   

   !---------------------------------------------------------------------------------------!
   ! 1.  Reading arguments:                                                                !
   !---------------------------------------------------------------------------------------!
   numarg = iargc()

   !---------------------------------------------------------------------------------------!
   ! 2.  Checking arguments                                                                !
   !---------------------------------------------------------------------------------------!
   select case (numarg) 
   case (0) ! No arguments provided, use default namelist name
      mevi_in='MEVI_IN'
      write (unit=*,fmt='(102a)') ('-',arg=1,102)
      write (unit=*,fmt='(a,1x,a,1x,a)')                                                     &
          ' No arguments, using the default namelist :',trim(mevi_in),'...'
      write (unit=*,fmt='(102a)') ('-',arg=1,102)
      write (unit=*,fmt=*)
   case (2) ! Correct # of arguments, checking whether they make sense or not.
      do arg=1,numarg
         call ugetarg(arg,arguments(arg))
      end do
      if (trim(arguments(1)) == '-f') then
         mevi_in=trim(arguments(2))
         write (unit=*,fmt='(102a)') ('-',arg=1,102)
         write (unit=*,fmt='(a,1x,a,1x,a)')                                                     &
             ' Using the user-defined namelist :',trim(mevi_in),'...'
         write (unit=*,fmt='(102a)') ('-',arg=1,102)
         write (unit=*,fmt=*)
      else
         call fatal_error('Invalid syntax! Usage: ./(mevi_executable) [-f (mevi_nlist)]' &
                         ,'mevi_main','mevi_main.f90')
      end if
   case default
      call fatal_error('Invalid syntax! Usage: ./(mevi_executable) [-f (mevi_nlist)]' &
                      ,'mevi_main','mevi_main.f90')
   
   end select

   !----- Initialise time -----------------------------------------------------------------! 
   wtime_start=walltime(0.)
   w1=walltime(wtime_start)

   !---------------------------------------------------------------------------------------!
   ! 3.  Loading the namelist and storing the information at the appropriate modules.      !
   !---------------------------------------------------------------------------------------!
   call load_namelist(mevi_in)
   
   
   !---------------------------------------------------------------------------------------!
   ! 4.  Retrieving the files available, in case we are using RAMS/BRAMS.                  !
   !---------------------------------------------------------------------------------------!
   call mevi_driver()

   !----- Getting the final time and printing the final time banner -----------------------!
   w2=walltime(wtime_start)
   write(c0,"(f12.2)") w2
   write(unit=*,fmt='(/,a,/)') ' === File conversion ends; Total elapsed time='//&
                               trim(adjustl(c0))//' ==='
  

   write (unit=*,fmt='')

   stop '****** MEVI execution ends ******'
end program mevi_main
!==========================================================================================!
!==========================================================================================!
