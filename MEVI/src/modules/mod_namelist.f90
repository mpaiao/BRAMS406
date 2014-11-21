!==========================================================================================!
!==========================================================================================!
!  Module namelist: contains the namelist structure, as well as initial values.            !
!------------------------------------------------------------------------------------------!
module mod_namelist
   use mod_maxdims, only : maxstr          & ! intent(in)
                         , maxvarin        & ! intent(in)
                         , maxgrds         & ! intent(in)
                         , maxlevs         ! ! intent(in)
   use mod_ioopts , only : missflg_int     & ! intent(in)
                         , missflg_real    & ! intent(in)
                         , missflg_char    & ! intent(in)
                         , missflg_logical ! ! intent(in)
   implicit none
   !---------------------------------------------------------------------------------------!

   type namelist_struct

      !----- Paths, prefixes, and types of inputs and outputs -----------------------------!
      character(len=maxstr) :: intype  ! Type of input file you want
      character(len=maxstr) :: outtype ! Type of output file you want
      character(len=maxstr) :: inpref  ! Type of input prefix you want
      character(len=maxstr) :: outpref ! Prefix of output file
      
      !----- Structure containing (B)RAMS or WRF related stuff ----------------------------!
      integer                                    :: nvars      ! # of variables           
      character(len=maxstr), dimension(maxvarin) :: vars       ! Variable list            

      real                  , dimension(maxgrds) :: lonw       ! Westernmost longitude  
      real                  , dimension(maxgrds) :: lone       ! Easternmost longitude 
      real                  , dimension(maxgrds) :: lats       ! Southernmost latitude
      real                  , dimension(maxgrds) :: latn       ! Northernmost latitude 
      real                  , dimension(maxgrds) :: levb       ! Bottommost level
      real                  , dimension(maxgrds) :: levt       ! Maximum possible lat. idx 

      integer              , dimension(maxgrds)  :: i1st       ! 1st longitude index  
      integer              , dimension(maxgrds)  :: ilast      ! Maximum possible long. idx
      integer              , dimension(maxgrds)  :: j1st       ! 1st latitude index  
      integer              , dimension(maxgrds)  :: jlast      ! Maximum possible lat. idx 
      integer              , dimension(maxgrds)  :: k1st       ! 1st level index  
      integer              , dimension(maxgrds)  :: klast      ! Maximum possible level idx
      integer              , dimension(maxgrds)  :: ttta       ! First        time  
      integer              , dimension(maxgrds)  :: tttz       ! Last         time  
      !----- Size reduction stuff ---------------------------------------------------------!
      integer              , dimension(maxgrds)  :: strinx       ! Points to skip in X
      integer              , dimension(maxgrds)  :: striny       ! Points to skip in Y
      integer              , dimension(maxgrds)  :: strinz       ! Points to skip in Z
      !----- (B)RAMS-related stuff --------------------------------------------------------!
      logical                                    :: vfmfrmt      ! RAMS Input format
      logical                                    :: polar2lonlat ! Interpolate to lon/lat?
   end type namelist_struct
   !---------------------------------------------------------------------------------------!

   type(namelist_struct) :: nl ! Structure containing all data read by the namelist


   !=======================================================================================!
   !=======================================================================================!


   contains



   !=======================================================================================!
   !=======================================================================================!
   !   Subroutine that initialises the namelist structures with some default variables, in !
   ! case the user doesn't provide some structures. This is going to initialise all struc- !
   ! tures with non-sense numbers so it will fail in case the user doesn't fill the name-  !
   ! list properly and attempts to use it.                                                 !
   !---------------------------------------------------------------------------------------!
   subroutine initialise_namelist()
      implicit none
      !----- Local variables. -------------------------------------------------------------!
      integer  :: n
      !------------------------------------------------------------------------------------!


      !----- Initialise IO options. -------------------------------------------------------!
      nl%intype  = missflg_char
      nl%outtype = missflg_char
      nl%inpref  = missflg_char
      nl%outpref = missflg_char
      
      !----- Initialise RAMS options. -----------------------------------------------------!
      nl%nvars        = missflg_int
      nl%vars         = (/ (missflg_char, n=1,maxvarin) /)
      nl%vfmfrmt      = missflg_logical
      nl%polar2lonlat = missflg_logical

      nl%i1st         = (/ (missflg_int , n=1,maxgrds ) /)
      nl%ilast        = (/ (missflg_int , n=1,maxgrds ) /)
      nl%j1st         = (/ (missflg_int , n=1,maxgrds ) /)
      nl%jlast        = (/ (missflg_int , n=1,maxgrds ) /)
      nl%k1st         = (/ (missflg_int , n=1,maxgrds ) /)
      nl%klast        = (/ (missflg_int , n=1,maxgrds ) /)

      nl%ttta         = (/ (missflg_int , n=1,maxgrds ) /)
      nl%tttz         = (/ (missflg_int , n=1,maxgrds ) /)
      nl%strinx       = (/ (missflg_int , n=1,maxgrds ) /)
      nl%striny       = (/ (missflg_int , n=1,maxgrds ) /)
      nl%strinz       = (/ (missflg_int , n=1,maxgrds ) /)
      return
   end subroutine initialise_namelist
   !=======================================================================================!
   !=======================================================================================!
end module mod_namelist
!==========================================================================================!
!==========================================================================================!
