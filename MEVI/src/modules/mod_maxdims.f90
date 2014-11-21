!------------------------------------------------------------------------------------------!
!  Module maxdims: contains parameters with the maximum dimensions allowed.                !
!------------------------------------------------------------------------------------------!
module mod_maxdims
   implicit none

   !----- Maximum string length -----------------------------------------------------------!
   integer, parameter :: maxstr   = 256
   
   !----- Maximum number of input variables -----------------------------------------------!
   integer, parameter :: maxvarin = 200
   
   !----- Maximum number of grids that I can handle ---------------------------------------!
   integer, parameter :: maxgrds  = 8
   
   !----- Maximum number of vertical levels -----------------------------------------------!
   integer, parameter :: maxlevs  = 200
   
   !----- Maximum rank that the data is allowed to have -----------------------------------!
   integer, parameter :: maxrank  = 6
   
   !----- Maximum number of files that MEVI can attempt to work ---------------------------!
   integer, parameter :: maxfiles  = 400

   !----- Maximum number of points each rank ----------------------------------------------!
   integer, parameter :: nxpmax    = 400 ! Maximum number of points in the horizontal
   integer, parameter :: nypmax    = 400 ! Maximum number of points in the horizontal
   integer, parameter :: nzpmax    = 362 ! Maximum number of points in the air column
   integer, parameter :: nzgmax    =  20 ! Maximum number of soil/snow/water layers
   integer, parameter :: maxpatch  =  12 ! Maximum number of patches
   integer, parameter :: maxclouds =   6 ! Maximum number of clouds

   !----- Maximum number of times per file ------------------------------------------------!
   integer, parameter :: maxtimes = 400

   !----- Maximum number of dependencies for a library variable. --------------------------!
   integer, parameter :: maxdepend = 10
end module mod_maxdims
