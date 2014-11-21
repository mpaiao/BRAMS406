!==========================================================================================!
!==========================================================================================!
! Module grid: This module contains the input grid structure.                              !
!------------------------------------------------------------------------------------------!
module mod_grid
   implicit none

   type grid_struct
      real, pointer, dimension(:,:  ) :: lon
      real, pointer, dimension(:,:  ) :: lat
      real, pointer, dimension(:,:  ) :: surf
      real, pointer, dimension(:,:,:) :: pres
      real, pointer, dimension(:)     :: lev
      real                            :: top
   end type grid_struct

   !---------------------------------------------------------------------------------------!
   !    Full domain grid. Currently not really in use except for the vertical, it will be  !
   ! in the future because we will then interpolate the fields to the output grid rather   !
   ! than just taking the points...                                                        !
   !---------------------------------------------------------------------------------------!
   type(grid_struct), allocatable, dimension(:) :: grid_g
   type(grid_struct), allocatable, dimension(:) :: grid_o
   
   contains
   !=======================================================================================!
   !=======================================================================================!
   subroutine alloc_grid(grid,nx,ny,nz)
      implicit none 
      type(grid_struct), intent(inout) :: grid
      integer          , intent(in)    :: nx,ny,nz

      allocate(grid%lon (nx,ny   ))
      allocate(grid%lat (nx,ny   ))
      allocate(grid%surf(nx,ny   ))
      allocate(grid%pres(nx,ny,nz))
      allocate(grid%lev (      nz))

      return
   end subroutine alloc_grid
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   subroutine nullify_grid(grid)
      implicit none
      type(grid_struct), intent(inout) :: grid

      if (associated(grid%lon )) nullify(grid%lon )
      if (associated(grid%lat )) nullify(grid%lat )
      if (associated(grid%surf)) nullify(grid%surf)
      if (associated(grid%pres)) nullify(grid%pres)
      if (associated(grid%lev )) nullify(grid%lev )

      return
   end subroutine nullify_grid
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   subroutine zero_grid(grid)
      implicit none
      type(grid_struct), intent(inout) :: grid

      if (associated(grid%lon )) grid%lon  = 0.0
      if (associated(grid%lat )) grid%lat  = 0.0
      if (associated(grid%surf)) grid%surf = 0.0
      if (associated(grid%pres)) grid%pres = 0.0
      if (associated(grid%lev )) grid%lev  = 0.0

      return
   end subroutine zero_grid
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   subroutine dealloc_grid(grid)
      implicit none
      type(grid_struct), intent(inout) :: grid

      if (associated(grid%lon )) deallocate(grid%lon )
      if (associated(grid%lat )) deallocate(grid%lat )
      if (associated(grid%surf)) deallocate(grid%surf)
      if (associated(grid%pres)) deallocate(grid%pres)
      if (associated(grid%lev )) deallocate(grid%lev )

      return
   end subroutine dealloc_grid
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   !     This subroutine copies a full grid to a subset, with offset and stride given at   !
   ! the input list.                                                                       !
   !---------------------------------------------------------------------------------------!
   subroutine copy_grid(grid_i,nx_i,ny_i,nz_i,grid_o,nx_o,ny_o,nz_o                        &
                       ,offx,offy,offz,stridex,stridey,stridez)
      implicit none
      !----- Input arguments, grid definition ---------------------------------------------!
      integer, intent(in)            :: nx_i,ny_i,nz_i
      integer, intent(in)            :: nx_o,ny_o,nz_o
      integer, intent(in)            :: offx,offy,offz,stridex,stridey,stridez
      !----- Input grid -------------------------------------------------------------------!
      type(grid_struct), intent(in)  :: grid_i
      !----- Output grid ------------------------------------------------------------------!
      type(grid_struct), intent(out) :: grid_o
      !----- Local variables --------------------------------------------------------------!
      integer                        :: xi,yi,zi,xo,yo,zo
      !------------------------------------------------------------------------------------!

      zo = 0
      do zi=1+offz,nz_i,stridez
         zo = zo + 1
         grid_o%lev(zo) = grid_i%lev(zi)
      end do
      
      xo = 0
      do xi=1+offx,nx_i,stridex
           xo=xo+1
           yo=0
         do yi=1+offy,ny_i,stridey
            yo=yo+1
            grid_o%lon (xo,yo) = grid_i%lon (xi,yi)
            grid_o%lat (xo,yo) = grid_i%lat (xi,yi)
            grid_o%surf(xo,yo) = grid_i%surf(xi,yi)
            zo=0
            do zi = 1+offz,nz_i,stridez
               zo=zo+1
               grid_o%pres(xo,yo,zo) = grid_i%pres(xi,yi,zi)
            end do
         end do
      end do

      return
   end subroutine copy_grid
   !=======================================================================================!
   !=======================================================================================!
end module mod_grid
!==========================================================================================!
!==========================================================================================!

