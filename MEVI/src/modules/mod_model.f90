!==========================================================================================!
!==========================================================================================!
!  MEVI. module model: contains variables from the input model.                            !
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
module mod_model

   use mod_maxdims , only : maxstr,maxvarin,maxlevs,maxgrds,nzpmax,nxpmax,nypmax,nzgmax    &
                           ,maxtimes
   use mod_time    , only : time_stt
   implicit none

   !----- Variables from MEVI_IN ----------------------------------------------------------!
   logical  :: vfmfrmt      ! RAMS Input format
   logical  :: polar2lonlat ! Interpolate to lon/lat?

   
   !----- Variables from the header -------------------------------------------------------!
   integer                            :: if_adap ! Vertical coordinate
   integer                            :: ngrids  ! Number of grids 
   integer                            :: nzg     ! Number of ground levels
   integer                            :: nzs     ! Number of water levels
   integer                            :: nclouds ! Number of cloud spectral sizes
   integer                            :: nwave   ! Number of wave lengths
   integer                            :: naddsc  ! Number of additional scalars
   integer                            :: npatch  ! Number of patches
   integer                            :: ihtran  ! Flag for horizontal coordinate:        
                                                 !   0 - Lon/Lat;
                                                 !   1 - Polar-stereographic;
                                                 !   2 - Lambert;
                                                 !   3 - Gaussian.
   !
   real                               :: polelon ! It depends on ihtran:
                                                 !   0 - Lon. @ NW corner;
                                                 !   1 - Pole longitude
                                                 !   2 - Longitude parallel to columns
   real                               :: polelat ! It depends on ihtran:
                                                 !   0 - Lat. @ NW corner;
                                                 !   1 - Pole latitude
                                                 !   2 - Lambert's first std. latitude
   real                               :: l2ndlat ! Used only if ihtran=2, it's Lambert's
                                                 !   second standard latitude

   real   , dimension(maxgrds)        :: centlon ! Longitude of the grid centre
   real   , dimension(maxgrds)        :: centlat ! Latitude of the grid centre
   !
   real   , dimension(maxgrds)        :: deltaxn ! Grid-dependent delta x
   real   , dimension(maxgrds)        :: deltayn ! Grid-dependent delta y
   !
   integer, dimension(maxgrds)        :: nnxp    ! Number of points in the X direction;
   integer, dimension(maxgrds)        :: nnyp    ! Number of points in the Y direction;
   integer, dimension(maxgrds)        :: nnzp    ! Number of points in the Z direction;
  
   !----- Vertical levels associated to Polar-Stereographic and vertical coordinates-------!
   real   , dimension(nxpmax)         :: atn     ! auxiliary coordinate variable
   real   , dimension(nxpmax,maxgrds) :: xtn     ! x coordinate of cell centre on PS
   real   , dimension(nxpmax,maxgrds) :: xmn     ! x coordinate of higher cell boundary 
   real   , dimension(nypmax,maxgrds) :: ytn     ! y coordinate of cell centre on PS
   real   , dimension(nypmax,maxgrds) :: ymn     ! y coordinate of higher cell boundary 
   real   , dimension(nzpmax,maxgrds) :: ztn     ! z coordinate of interval center
                                                 !   For sigma-p, these are the sigma levels
   real   , dimension(nzpmax,maxgrds) :: zmn     ! z coordinate of grid point
                                                 !   For sigma-p, these are the sigma levels
   real   , dimension(nzpmax,maxgrds) :: dztn    ! dz of interval center
   real   , dimension(nzpmax,maxgrds) :: dzmn    ! dz of grid point

   !----- Soil vertical levels ------------------------------------------------------------!
   real   , dimension(nzgmax)         :: slz     ! Soil levels.

   !----- Scratch Time strucures ----------------------------------------------------------!
   type(time_stt), dimension(maxtimes) :: this_time   ! Scratch to hold file time
   type(time_stt)                      :: zero_time   ! Scratch to hold init. time

end module mod_model
!==========================================================================================!
!==========================================================================================!
