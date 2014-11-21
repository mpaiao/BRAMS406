!==========================================================================================!
!==========================================================================================!
!    Subroutine load_namelist. This subroutine will fill the appropriate modules with the  !
! information gathered by the namelist.                                                    !
!==========================================================================================!
!==========================================================================================!
subroutine load_namelist(mevi_in)
   use mod_maxdims,  only : maxstr               ! ! intent(in)
   use mod_namelist, only : nl                   & ! intent(out)
                          , initialise_namelist  ! ! sub-routine
   use mod_ioopts  , only : intype               & ! intent(out)
                          , outtype              & ! intent(out)
                          , inpref               & ! intent(out)
                          , outpref              & ! intent(out)
                          , nvars                & ! intent(out)
                          , vars                 & ! intent(out)
                          , lone                 & ! intent(out)
                          , lonw                 & ! intent(out)
                          , lats                 & ! intent(out)
                          , latn                 & ! intent(out)
                          , levb                 & ! intent(out)
                          , levt                 & ! intent(out)
                          , i1st                 & ! intent(out)
                          , ilast                & ! intent(out)
                          , j1st                 & ! intent(out)
                          , jlast                & ! intent(out)
                          , k1st                 & ! intent(out)
                          , klast                & ! intent(out)
                          , ttta                 & ! intent(out)
                          , tttz                 & ! intent(out)
                          , strinx               & ! intent(out)
                          , striny               & ! intent(out)
                          , strinz               ! ! intent(out)
   use mod_model   , only : vfmfrmt              & ! intent(out)
                          , polar2lonlat         ! ! intent(out)
   implicit none
   
   !---------------------------------------------------------------------------------------!
   !    Variable declaration
   !---------------------------------------------------------------------------------------!
   !----- Arguments -----------------------------------------------------------------------!
   character(len=maxstr), intent(in) :: mevi_in  ! Namelist file name
   !----- Internal variables --------------------------------------------------------------!
   integer                           :: ierr     ! Error flag
   integer                           :: v        ! Counter
   !---------------------------------------------------------------------------------------!

   namelist /mevi_options/ nl

   !---------------------------------------------------------------------------------------!
   ! 1. First step is to initialise the namelist with some default (non-sense) parameters  !
   !---------------------------------------------------------------------------------------!
   call initialise_namelist()
   
   !---------------------------------------------------------------------------------------!
   ! 2. Read the namelist.                                                                 !
   !---------------------------------------------------------------------------------------!
   open (unit=10,file=trim(mevi_in),status='old',action='read',iostat=ierr)
   if (ierr /= 0) then
      call fatal_error('Problems opening namelist '//trim(mevi_in)//'!!!'                  &
                      ,'load_namelist','load_namelist.f90')
   else
!      read (unit=10,nml=mevi_options,iostat=ierr)
      read (unit=10,nml=mevi_options)
      if (ierr /= 0) then
        call fatal_error('Problems reading namelist '//trim(mevi_in)//'!!!'                &
                        ,'load_namelist','load_namelist.f90')
      end if
   end if

   !---------------------------------------------------------------------------------------!
   ! 3. Copy the namelist to the proper modules,                                           !
   !---------------------------------------------------------------------------------------!

   intype       = nl%intype
   outtype      = nl%outtype
   inpref       = nl%inpref
   outpref      = nl%outpref
   
   nvars        = nl%nvars
   vars         = nl%vars
   
   lone         = nl%lone
   lonw         = nl%lonw
   lats         = nl%lats
   latn         = nl%latn
   levb         = nl%levb
   levt         = nl%levt

   i1st         = nl%i1st
   ilast        = nl%ilast
   j1st         = nl%j1st
   jlast        = nl%jlast
   k1st         = nl%k1st
   klast        = nl%klast
   ttta         = nl%ttta
   tttz         = nl%tttz

   strinx       = nl%strinx   
   striny       = nl%striny  
   strinz       = nl%strinz   
   
   vfmfrmt      = nl%vfmfrmt
   polar2lonlat = nl%polar2lonlat

   !---------------------------------------------------------------------------------------!
   ! 3. Converting setting variables into lower-case, so the code is case insenstive for   !
   !    options.                                                                           !
   !---------------------------------------------------------------------------------------!
   do v=1,nvars
      call tolower (vars(v))
   end do

   !---------------------------------------------------------------------------------------!
   ! 4. Copying the namelist to the proper modules                                         !
   !---------------------------------------------------------------------------------------!
   call dump_namelist()


   return
end subroutine load_namelist
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
!   This subroutine simply dumps the data read at the namelist on screen, so the users     !
! won't get bored and will able to check that MEVI understood what they asked.             !
!------------------------------------------------------------------------------------------!
subroutine dump_namelist()
   use mod_maxdims , only : maxstr       & ! intent(in)
                          , maxgrds      ! ! intent(in)
   use mod_ioopts  , only : intype       & ! intent(out)
                          , outtype      & ! intent(out)
                          , inpref       & ! intent(out)
                          , outpref      & ! intent(out)
                          , nvars        & ! intent(out)
                          , vars         & ! intent(out)
                          , lone         & ! intent(out)
                          , lonw         & ! intent(out)
                          , lats         & ! intent(out)
                          , latn         & ! intent(out)
                          , levb         & ! intent(out)
                          , levt         & ! intent(out)
                          , i1st         & ! intent(out)
                          , ilast        & ! intent(out)
                          , j1st         & ! intent(out)
                          , jlast        & ! intent(out)
                          , k1st         & ! intent(out)
                          , klast        & ! intent(out)
                          , ttta         & ! intent(out)
                          , tttz         & ! intent(out)
                          , strinx       & ! intent(out)
                          , striny       & ! intent(out)
                          , strinz       ! ! intent(out)
   use mod_model   , only : vfmfrmt      & ! intent(in)
                          , polar2lonlat ! ! intent(in)
   implicit none
   !----- Local variables. ----------------------------------------------------------------!
   character(len=maxstr) :: myform
   integer               :: v
   integer               :: va
   integer               :: vz
   !---------------------------------------------------------------------------------------!

   write (unit=*,fmt='(102a)') ('-',v=1,102)
   write (unit=*,fmt='(a)') ' MEVI - Namelist information:                                 '
   write (unit=*,fmt='(102a)') ('-',v=1,102)
   write (unit=*,fmt='(a,1x,a)'  ) ' -> Intype:        ',trim(intype)
   write (unit=*,fmt='(a,1x,a)'  ) ' -> Outtype:       ',trim(outtype)
   write (unit=*,fmt='(a,1x,a)'  ) ' -> Inpref:        ',trim(inpref)
   write (unit=*,fmt='(a,1x,a)'  ) ' -> Outpref:       ',trim(outpref)
   write (unit=*,fmt='(a)'       ) ' '
   
   write (unit=*,fmt='(a,1x,i5)' ) ' -> Nvars:         ',nvars
   
   va=1; vz=min(6,nvars)
   write (unit=*,fmt='(a,6(1x,a))') ' -> Vars:          ',(trim(vars(v)),v=va,vz)
   do va=7,nvars,6
       vz=min(va+5,nvars)
       write (unit=*,fmt='(19x,6(1x,a))') (trim(vars(v)),v=va,vz)
   end do

   write(myform,fmt='(a,i5.5,a)') '(a,1x,',maxgrds,'(1x,f8.3))'
   write(unit=*,fmt=trim(myform)) ' -> LonW:          ',(lonw  (v),v=1,maxgrds)
   write(unit=*,fmt=trim(myform)) ' -> LonE:          ',(lone  (v),v=1,maxgrds)
   write(unit=*,fmt=trim(myform)) ' -> LatS:          ',(lats  (v),v=1,maxgrds)
   write(unit=*,fmt=trim(myform)) ' -> LatN:          ',(latn  (v),v=1,maxgrds)
   write(unit=*,fmt=trim(myform)) ' -> LevB:          ',(levb  (v),v=1,maxgrds)
   write(unit=*,fmt=trim(myform)) ' -> LevT:          ',(levt  (v),v=1,maxgrds)

   write(myform,fmt='(a,i5.5,a)') '(a,1x,',maxgrds,'(1x,i8))'
   write(unit=*,fmt=trim(myform)) ' -> I1st:          ',(i1st  (v),v=1,maxgrds)
   write(unit=*,fmt=trim(myform)) ' -> Ilast:         ',(ilast (v),v=1,maxgrds)
   write(unit=*,fmt=trim(myform)) ' -> J1st:          ',(j1st  (v),v=1,maxgrds)
   write(unit=*,fmt=trim(myform)) ' -> Jlast:         ',(jlast (v),v=1,maxgrds)
   write(unit=*,fmt=trim(myform)) ' -> K1st:          ',(k1st  (v),v=1,maxgrds)
   write(unit=*,fmt=trim(myform)) ' -> Klast:         ',(klast (v),v=1,maxgrds)
   write(unit=*,fmt=trim(myform)) ' -> Ttta:          ',(ttta  (v),v=1,maxgrds)
   write(unit=*,fmt=trim(myform)) ' -> Tttz:          ',(tttz  (v),v=1,maxgrds)
   write(unit=*,fmt=trim(myform)) ' -> Strinx:        ',(strinx(v),v=1,maxgrds)
   write(unit=*,fmt=trim(myform)) ' -> Striny:        ',(striny(v),v=1,maxgrds)
   write(unit=*,fmt=trim(myform)) ' -> Strinz:        ',(strinz(v),v=1,maxgrds)

   write(unit=*,fmt='(a,1x,l1)')  ' -> Vfmfrmt:       ',vfmfrmt
   write(unit=*,fmt='(a,1x,l1)')  ' -> Polar2lonlat:  ',polar2lonlat
   write (unit=*,fmt='(102a)') ('-',v=1,102)
   

   return
end subroutine dump_namelist
!==========================================================================================!
!==========================================================================================!
