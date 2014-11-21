!==========================================================================================!
!==========================================================================================!
! MEVI. Module mod_v5d. This is essentially the data available at v5df.h. There is nothing !
!       wrong with the include file, except that it is fortran 77, so it is incompatible.  !
!       These constants should be the same as the one available there, though, because     !
!       these are the maximum dimensions vis5d can handle.                                 !
!==========================================================================================!
!==========================================================================================!
module mod_v5d

   implicit none
   !---------------------------------------------------------------------------------------!
   !    Function prototype. See the Vis5D README (available at doc/vis5d_README.1st file   !
   ! for details. These are the functions you'll want to use for writing v5d file          !
   ! converters. These functions are defined at the libv5d.a library                       !
   !---------------------------------------------------------------------------------------!
   integer, external :: v5dcreate
   integer, external :: v5dcreatesimple
   integer, external :: v5dwrite
   integer, external :: v5dmcfile
   integer, external :: v5dclose

   !---------------------------------------------------------------------------------------!
   ! 5-D grid limits:                                                                      !
   !                                                                                       !
   !    WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING!   !
   !    WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING!   !
   !    WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING!   !
   !    WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING!   !
   !    WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING!   !
   !    WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING!   !
   !                                                                                       !
   ! These parameter MUST match those in v5d.h !!!                                         !
   !---------------------------------------------------------------------------------------!
   integer, parameter :: MAXVARS    = 200
   integer, parameter :: MAXTIMES   = 400
   integer, parameter :: MAXROWS    = 400
   integer, parameter :: MAXCOLUMNS = 400
   integer, parameter :: MAXLEVELS  = 400


   !---------------------------------------------------------------------------------------!
   ! Missing values                                                                        !
   !---------------------------------------------------------------------------------------!
   real   , parameter :: MISSING  =  1.0E35
   integer, parameter :: IMISSING = -987654

   !---------------------------------------------------------------------------------------!
   !    The following variables are not defined in v5df.h or v5d.h, but they are Vis-5D    !
   ! related parameters, adding here. Even though they are not defined, these are values   !
   ! present at the sample file, so think twice before changing it...                      !
   !---------------------------------------------------------------------------------------!
   integer              , parameter                :: maxproj     = 100
   integer              , parameter                :: v5d_varlen  = 10
   integer              , parameter                :: v5d_unitlen = 10


end module mod_v5d
