!------------------------------------------------------------------------------------------!
!  Module ioopts: contains the list of input/output basic settings.                        !
!------------------------------------------------------------------------------------------!
module mod_ioopts
   use mod_maxdims, only: maxstr,maxvarin,maxlevs,maxgrds,maxfiles,maxtimes,maxdepend
   use mod_time   , only : time_stt
   implicit none
   
   !----- Namelist variables --------------------------------------------------------------!
   character(len=maxstr) :: intype  ! Type of input file
   character(len=maxstr) :: outtype ! Type of output file
   character(len=maxstr) :: inpref  ! Type of input prefix you want
   character(len=maxstr) :: outpref ! Prefix of output file

   integer                                     :: nvars      ! # of variables
   character(len=maxstr) , dimension(maxvarin) :: vars       ! Variable list



   !---------------------------------------------------------------------------------------!
   !   Coordinate variables.                                                               !
   !---------------------------------------------------------------------------------------!
   real                  , dimension(maxgrds)  :: lonw       ! Westernmost longitude  
   real                  , dimension(maxgrds)  :: lone       ! Easternmost longitude 
   real                  , dimension(maxgrds)  :: lats       ! Southernmost latitude
   real                  , dimension(maxgrds)  :: latn       ! Northernmost latitude 
   real                  , dimension(maxgrds)  :: levb       ! Bottommost level
   real                  , dimension(maxgrds)  :: levt       ! Maximum possible lat. idx 

   integer               , dimension(maxgrds)  :: i1st       ! 1st longitude index  
   integer               , dimension(maxgrds)  :: ilast      ! Maximum possible long. idx 
   integer               , dimension(maxgrds)  :: j1st       ! 1st latitude index  
   integer               , dimension(maxgrds)  :: jlast      ! Maximum possible lat. idx 
   integer               , dimension(maxgrds)  :: k1st       ! 1st level index  
   integer               , dimension(maxgrds)  :: klast      ! Maximum possible level idx 
   integer               , dimension(maxgrds)  :: ttta       ! First        time
   integer               , dimension(maxgrds)  :: tttz       ! Last         time
   logical                                     :: x_360      ! X goes from 0 to 360 degrees
   logical               , dimension(maxgrds)  :: x_cyc      ! We must use cyclic conditions
   logical                                     :: y_n2s      ! Y is from North to South
   logical                                     :: z_t2b      ! Z is from top to bottom
   !----- Size reduction stuff ------------------------------------------------------------!
   integer              , dimension(maxgrds)  :: strinx        ! Points to skip in X
   integer              , dimension(maxgrds)  :: striny        ! Points to skip in Y
   integer              , dimension(maxgrds)  :: strinz        ! Points to skip in Z

   !----- Variable map --------------------------------------------------------------------!
   integer               , dimension(maxfiles,maxvarin,maxgrds) :: var_address

   !----- Library map, with dependency list -----------------------------------------------!
   integer               , dimension(maxfiles,maxvarin,maxgrds,maxdepend) :: lib_address
   integer               , dimension(maxvarin)                            :: lib_ndepend

   !----- Time map ------------------------------------------------------------------------!
   type(time_stt)        , dimension(maxtimes)                  :: outtimes
   integer                                                      :: nouttimes
   integer               , dimension(maxfiles,maxtimes,maxgrds) :: time_address

   !----- Parameters ----------------------------------------------------------------------!
   logical               , parameter           :: missflg_logical = .false.
   integer               , parameter           :: missflg_int     = 999999
   real                  , parameter           :: missflg_real    = 1.E+34 
   character(len=maxstr) , parameter           :: missflg_char    = '___'
   real                  , parameter           :: missflg_dble    = 1.D+34 
   integer               , parameter           :: mevi_absent     = -999
   integer               , parameter           :: mevi_badtype    = -888
   integer               , parameter           :: mevi_partly     = -777
   integer               , parameter           :: mevi_libvar     = -666
   integer               , parameter           :: mevi_notfound   = -555

   !----- Types that will be considered for Vis5D conversion ------------------------------!
   integer, dimension(04), parameter           :: v5d_ok_types    = (/ 22, 33, 62, 63/) 
   integer, dimension(02), parameter           :: ralph2_ok_types = (/ 33, 63 /) 
 

end module mod_ioopts
