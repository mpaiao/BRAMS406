!============================================================================
! Subroutine that swaps the data info if necessary.
subroutine swap32real(a,n)
!============================================================================

  implicit none

  integer,      intent(in)                  :: n
  real(kind=4), intent(inout), dimension(n) :: a

  !
  !      REVERSE ORDER OF BYTES IN INTEGER*4 WORD, or REAL*4
  !
  integer (kind=4)  ::   ijklmn
  !
  character (len=1) :: jtemp(4)
  character (len=1) :: ktemp
  real(kind=4)    :: r4mold=6. !MLO - Real mold for bit transfer, the number itself has no meaning.
  integer(kind=4) :: i4mold=6  !MLO - Integer mold for bit transfer, the number itself has no meaning.
  !
  ! Local variables
  integer :: i, itemp

  equivalence (jtemp(1),itemp)
  !
  save
  !
  ![MLO - Alternative way to save bit representation and preserve interface between subroutine and call
  do i = 1,n
     ijklmn   = transfer(a(i),i4mold)
     itemp    = ijklmn
     ktemp    = jtemp(4)
     jtemp(4) = jtemp(1)
     jtemp(1) = ktemp
     ktemp    = jtemp(3)
     jtemp(3) = jtemp(2)
     jtemp(2) = ktemp
     ijklmn   = itemp
     a(i)     = transfer(ijklmn,r4mold)
  enddo

  return
end subroutine swap32real


!============================================================================
! Subroutine that swaps the data info if necessary.
subroutine swap32inte(a,n)
!============================================================================

  implicit none

  integer,         intent(in)                  :: n
  integer(kind=4), intent(inout), dimension(n) :: a

  !
  !      REVERSE ORDER OF BYTES IN INTEGER*4 WORD, or REAL*4
  !
  integer (kind=4)  ::   ijklmn
  !
  character (len=1) :: jtemp(4)
  character (len=1) :: ktemp
  integer(kind=4) :: i4mold=6  !MLO - Integer mold for bit transfer, the number itself has no meaning.
  !
  ! Local variables
  integer :: i, itemp

  equivalence (jtemp(1),itemp)
  !
  save
  !
  ![MLO - Alternative way to save bit representation and preserve interface between subroutine and call
  do i = 1,n
     ijklmn   = a(i)
     itemp    = ijklmn
     ktemp    = jtemp(4)
     jtemp(4) = jtemp(1)
     jtemp(1) = ktemp
     ktemp    = jtemp(3)
     jtemp(3) = jtemp(2)
     jtemp(2) = ktemp
     ijklmn   = itemp
     a(i)     = ijklmn
  enddo

  return
end subroutine swap32inte


!============================================================================
! Subroutine that swaps the data info if necessary.
subroutine swap16inte(a,n)
!============================================================================
   implicit none
   ! does a byteswap on integer2 number
   integer        , intent(in)                   :: n
   integer(kind=2), intent(inout), dimension(n)  :: a

   integer :: k

   integer(kind=1), dimension(2) :: ii, jj
   integer(kind=2)               ::  i, j

   equivalence (i,ii)
   equivalence (j,jj)

   do k=1,n
      i = a(k)

      jj(1) = ii(2)
      jj(2) = ii(1)

      a(k) = j
   end do
   return
end subroutine swap16inte
