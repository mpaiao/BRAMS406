!==========================================================================================!
!==========================================================================================!
!  Module scratch: contains parameters with the maximum dimensions allowed. The variable   !
! has two associated numbers, the first one being the dimension and the second one just an !
! index. Their "dimension" is given by its size rather than their rank, and this is to     !
! allow more flexibility.                                                                  !
!------------------------------------------------------------------------------------------!
module mod_scratch
   implicit none

   type scratch_struct
      real   , pointer, dimension(:):: tmparr11,tmparr12,tmparr13,tmparr14,tmparr15,tmparr16
      real   , pointer, dimension(:):: tmparr21,tmparr22,tmparr23,tmparr24,tmparr25,tmparr26
      real   , pointer, dimension(:):: tmparr31,tmparr32,tmparr33,tmparr34,tmparr35,tmparr36
      real   , pointer, dimension(:):: tmparr41,tmparr42,tmparr43,tmparr44,tmparr45,tmparr46
      integer, pointer, dimension(:):: intarr61,intarr62,intarr63,intarr64,intarr65,intarr66
      !----- These are scratch arrays that should be used exclusively by the output -------!
      real   , pointer, dimension(:):: outarr1d,outarr2d,outarr3d,outarr4d

   end type scratch_struct
   
   type(scratch_struct) :: scratch

   contains
   
   subroutine alloc_scratch(ngrida,ngridz)
      use mod_model, only : nnzp,nnxp,nnyp,nzg,nzs,nclouds,npatch
      use mod_ioopts, only: intype
      implicit none 
      integer ,intent(in) :: ngrida,ngridz
      integer             :: ng,maxzp,maxyp,maxxp
      integer, save       :: size1d=0,size2d=0,size3d=0,size4d=0
      integer             :: size1dnew,size2dnew,size3dnew,size4dnew

      !------------------------------------------------------------------------------------!
      !    The memory allocation is done by using the information specific for this run    !
      ! to allocate the maximum size needed and just this.                                 !
      !------------------------------------------------------------------------------------!
      maxxp=maxval(nnxp(ngrida:ngridz),dim=1)
      maxyp=maxval(nnyp(ngrida:ngridz),dim=1)
      maxzp=maxval(nnzp(ngrida:ngridz),dim=1)
      size1dnew=max(maxxp,maxyp,maxzp,nzg,nzs,npatch,nclouds)
      size2dnew=maxxp*maxyp
      size3dnew=max(maxxp*maxyp*npatch,maxxp*maxyp*nclouds,maxxp*maxyp*nzg,maxxp*maxyp*nzs &
                ,maxxp*maxyp*maxzp)
      select case (trim(intype))
      !------------------------------------------------------------------------------------!
      !  4D variables with enough memory to store cloud information will be allocated only !
      ! for RAMS. WRF has cloud information but it doesn't have any 4D variable. Since all !
      ! ensemble members are save, that would translate into a horribly large array that   !
      ! could cause memory issues.                                                         !
      !------------------------------------------------------------------------------------!
      case ('rams','brams')
         size4dnew=max(maxxp*maxyp*maxzp*nclouds,maxxp*maxyp*nzg*npatch                    &
                      ,maxxp*maxyp*nzs*npatch)
      case default
         size4dnew=max(maxxp*maxyp*nzg*npatch,maxxp*maxyp*nzs*npatch)
      end select
      !------------------------------------------------------------------------------------!

      if (size1dnew > size1d) then
         size1d=size1dnew
         call dealloc_scratch(1)
         call nullify_scratch(1)
         allocate(scratch%tmparr11(size1d))
         allocate(scratch%tmparr12(size1d))
         allocate(scratch%tmparr13(size1d))
         allocate(scratch%tmparr14(size1d))
         allocate(scratch%tmparr15(size1d))
         allocate(scratch%tmparr16(size1d))

         allocate(scratch%intarr61(size1d))
         allocate(scratch%intarr62(size1d))
         allocate(scratch%intarr63(size1d))
         allocate(scratch%intarr64(size1d))
         allocate(scratch%intarr65(size1d))
         allocate(scratch%intarr66(size1d))

         allocate(scratch%outarr1d(size1d))
      end if



      if (size2dnew > size2d) then
         size2d=size2dnew
         call dealloc_scratch(2)
         call nullify_scratch(2)
         allocate(scratch%tmparr21(size2d))
         allocate(scratch%tmparr22(size2d))
         allocate(scratch%tmparr23(size2d))
         allocate(scratch%tmparr24(size2d))
         allocate(scratch%tmparr25(size2d))
         allocate(scratch%tmparr26(size2d))

         allocate(scratch%outarr2d(size2d))
      end if



      if (size3dnew > size3d) then
         size3d=size3dnew
         call dealloc_scratch(3)
         call nullify_scratch(3)
         allocate(scratch%tmparr31(size3d))
         allocate(scratch%tmparr32(size3d))
         allocate(scratch%tmparr33(size3d))
         allocate(scratch%tmparr34(size3d))
         allocate(scratch%tmparr35(size3d))
         allocate(scratch%tmparr36(size3d))

         allocate(scratch%outarr3d(size3d))
      end if



      if (size4dnew > size4d) then
         size4d=size4dnew
         call dealloc_scratch(4)
         call nullify_scratch(4)
         allocate(scratch%tmparr41(size4d))
         allocate(scratch%tmparr42(size4d))
         allocate(scratch%tmparr43(size4d))
         allocate(scratch%tmparr44(size4d))
         allocate(scratch%tmparr45(size4d))
         allocate(scratch%tmparr46(size4d))

         allocate(scratch%outarr4d(size4d))
      end if

      return
   end subroutine alloc_scratch
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   subroutine nullify_scratch(dim)
      implicit none
      integer, intent(in) :: dim

      select case (dim)
      case (1)
         if (associated(scratch%tmparr11)) nullify(scratch%tmparr11)
         if (associated(scratch%tmparr12)) nullify(scratch%tmparr12)
         if (associated(scratch%tmparr13)) nullify(scratch%tmparr13)
         if (associated(scratch%tmparr14)) nullify(scratch%tmparr14)
         if (associated(scratch%tmparr15)) nullify(scratch%tmparr15)
         if (associated(scratch%tmparr16)) nullify(scratch%tmparr16)

         if (associated(scratch%intarr61)) nullify(scratch%intarr61)
         if (associated(scratch%intarr62)) nullify(scratch%intarr62)
         if (associated(scratch%intarr63)) nullify(scratch%intarr63)
         if (associated(scratch%intarr64)) nullify(scratch%intarr64)
         if (associated(scratch%intarr65)) nullify(scratch%intarr65)
         if (associated(scratch%intarr66)) nullify(scratch%intarr66)

         if (associated(scratch%outarr1d)) nullify(scratch%outarr1d)

      case (2)
         if (associated(scratch%tmparr21)) nullify(scratch%tmparr21)
         if (associated(scratch%tmparr22)) nullify(scratch%tmparr22)
         if (associated(scratch%tmparr23)) nullify(scratch%tmparr23)
         if (associated(scratch%tmparr24)) nullify(scratch%tmparr24)
         if (associated(scratch%tmparr25)) nullify(scratch%tmparr25)
         if (associated(scratch%tmparr26)) nullify(scratch%tmparr26)

         if (associated(scratch%outarr2d)) nullify(scratch%outarr2d)

      case (3)
         if (associated(scratch%tmparr31)) nullify(scratch%tmparr31)
         if (associated(scratch%tmparr32)) nullify(scratch%tmparr32)
         if (associated(scratch%tmparr33)) nullify(scratch%tmparr33)
         if (associated(scratch%tmparr34)) nullify(scratch%tmparr34)
         if (associated(scratch%tmparr35)) nullify(scratch%tmparr35)
         if (associated(scratch%tmparr36)) nullify(scratch%tmparr36)

         if (associated(scratch%outarr3d)) nullify(scratch%outarr3d)

      case (4)
         if (associated(scratch%tmparr41)) nullify(scratch%tmparr41)
         if (associated(scratch%tmparr42)) nullify(scratch%tmparr42)
         if (associated(scratch%tmparr43)) nullify(scratch%tmparr43)
         if (associated(scratch%tmparr44)) nullify(scratch%tmparr44)
         if (associated(scratch%tmparr45)) nullify(scratch%tmparr45)
         if (associated(scratch%tmparr46)) nullify(scratch%tmparr46)

         if (associated(scratch%outarr4d)) nullify(scratch%outarr4d)

      end select

      return
   end subroutine nullify_scratch
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   subroutine dealloc_scratch(dim)
      implicit none
      integer, intent(in) :: dim

      select case (dim)
      case (1)
         if (associated(scratch%tmparr11)) deallocate(scratch%tmparr11)
         if (associated(scratch%tmparr12)) deallocate(scratch%tmparr12)
         if (associated(scratch%tmparr13)) deallocate(scratch%tmparr13)
         if (associated(scratch%tmparr14)) deallocate(scratch%tmparr14)
         if (associated(scratch%tmparr15)) deallocate(scratch%tmparr15)
         if (associated(scratch%tmparr16)) deallocate(scratch%tmparr16)

         if (associated(scratch%intarr61)) deallocate(scratch%intarr61)
         if (associated(scratch%intarr62)) deallocate(scratch%intarr62)
         if (associated(scratch%intarr63)) deallocate(scratch%intarr63)
         if (associated(scratch%intarr64)) deallocate(scratch%intarr64)
         if (associated(scratch%intarr65)) deallocate(scratch%intarr65)
         if (associated(scratch%intarr66)) deallocate(scratch%intarr66)

         if (associated(scratch%outarr1d)) deallocate(scratch%outarr1d)

      case (2)
         if (associated(scratch%tmparr21)) deallocate(scratch%tmparr21)
         if (associated(scratch%tmparr22)) deallocate(scratch%tmparr22)
         if (associated(scratch%tmparr23)) deallocate(scratch%tmparr23)
         if (associated(scratch%tmparr24)) deallocate(scratch%tmparr24)
         if (associated(scratch%tmparr25)) deallocate(scratch%tmparr25)
         if (associated(scratch%tmparr26)) deallocate(scratch%tmparr26)

         if (associated(scratch%outarr2d)) deallocate(scratch%outarr2d)

      case (3)
         if (associated(scratch%tmparr31)) deallocate(scratch%tmparr31)
         if (associated(scratch%tmparr32)) deallocate(scratch%tmparr32)
         if (associated(scratch%tmparr33)) deallocate(scratch%tmparr33)
         if (associated(scratch%tmparr34)) deallocate(scratch%tmparr34)
         if (associated(scratch%tmparr35)) deallocate(scratch%tmparr35)
         if (associated(scratch%tmparr36)) deallocate(scratch%tmparr36)

         if (associated(scratch%outarr3d)) deallocate(scratch%outarr3d)

      case (4)
         if (associated(scratch%tmparr41)) deallocate(scratch%tmparr41)
         if (associated(scratch%tmparr42)) deallocate(scratch%tmparr42)
         if (associated(scratch%tmparr43)) deallocate(scratch%tmparr43)
         if (associated(scratch%tmparr44)) deallocate(scratch%tmparr44)
         if (associated(scratch%tmparr45)) deallocate(scratch%tmparr45)
         if (associated(scratch%tmparr46)) deallocate(scratch%tmparr46)

         if (associated(scratch%outarr4d)) deallocate(scratch%outarr4d)

      end select

      return

      return
   end subroutine dealloc_scratch
!==========================================================================================!
!==========================================================================================!
end module mod_scratch
!==========================================================================================!
!==========================================================================================!
