!############################# Change Log ##################################
! 2.3.0.2
!
! 000921 MJB all ##
!            Cleaned up screen output. ##
! 000830 CJT gradr ##
!            Included interface.h. ##
!
!###########################################################################
!  Copyright (C)  1990, 1995, 1999, 2000 - All Rights Reserved
!  Regional Atmospheric Modeling System - RAMS
!  Mission Research Corporation / *ASTeR Division
!###########################################################################

subroutine gradr (n1,n2,n3,ia,iz,ja,jz,vc3da,vc3db,dir,gpnt,topt  &
                 ,xm,xt,ym,yt,zm,zt,deltay,dzm,dzt,vctr1,vctr2,ztop  &
                 ,jdim,ihtran,platn,plonn)

dimension vc3da(n1,n2,n3),vc3db(n1,n2,n3),e(1),topt(n1,n2)  &
         ,xm(*),xt(*),ym(*),yt(*),zm(*),zt(*),dzm(*),dzt(*)  &
         ,vctr1(*),vctr2(*)
character*(*) dir,gpnt
character*6 optyp

include 'interface.h'

optyp = 'gradnt'
goto 10

entry divcartr (n1,n2,n3,ia,iz,ja,jz,vc3da,vc3db,dir,gpnt,topt  &
               ,xm,xt,ym,yt,zm,zt,deltay,dzm,dzt,vctr1,vctr2,ztop,jdim)
optyp = 'divcrt'
goto 10

entry divstarr (n1,n2,n3,ia,iz,ja,jz,vc3da,vc3db,dir,gpnt,topt  &
               ,xm,xt,ym,yt,zm,zt,deltay,dzm,dzt,vctr1,vctr2,ztop,jdim)
optyp = 'divstr'

10 continue

jaa = ja
jzz = jz
if(jdim.eq.0) then
   jaa = 1
   jzz = 1
endif

!print*,'in grad-',dir,gpnt,optyp,'   !!!!!!!!!!!!!!!!'

isizexy = n1 * n2
isizeall = isizexy * 33
iaddr=iralloc(isizeall,e,ioff)

iglat   = ioff    + 1
iglon   = iglat   + isizexy
ifmapu  = iglon   + isizexy
ifmapv  = ifmapu  + isizexy
ifmapt  = ifmapv  + isizexy
ifmapm  = ifmapt  + isizexy
ifmapui = ifmapm  + isizexy
ifmapvi = ifmapui + isizexy
ifmapti = ifmapvi + isizexy
ifmapmi = ifmapti + isizexy
idxu    = ifmapmi + isizexy
idxv    = idxu    + isizexy
idxt    = idxv    + isizexy
idxm    = idxt    + isizexy
idyu    = idxm    + isizexy
idyv    = idyu    + isizexy
idyt    = idyv    + isizexy
idym    = idyt    + isizexy
itopu   = idym    + isizexy
itopv   = itopu   + isizexy
itopm   = itopv   + isizexy
irtgt   = itopm   + isizexy
irtgu   = irtgt   + isizexy
irtgv   = irtgu   + isizexy
irtgm   = irtgv   + isizexy
if13u   = irtgm   + isizexy
if13v   = if13u   + isizexy
if13t   = if13v   + isizexy
if13m   = if13t   + isizexy
if23u   = if13m   + isizexy
if23v   = if23u   + isizexy
if23t   = if23v   + isizexy
if23m   = if23t   + isizexy

if(ihtran.eq.1)then
   CALL revu_POLARST (N1,N2,E(IGLAT),E(IGLON),E(IFMAPU),E(IFMAPV)  &
                     ,E(IFMAPT),E(IFMAPM),E(IFMAPUI),E(IFMAPVI)  &
                     ,E(IFMAPTI),E(IFMAPMI),xm,xt,ym,yt,platn,plonn)
else
   CALL AE0(N1*N2,E(IFMAPU),1.)
   CALL AE0(N1*N2,E(IFMAPV),1.)
   CALL AE0(N1*N2,E(IFMAPT),1.)
   CALL AE0(N1*N2,E(IFMAPM),1.)
   CALL AE0(N1*N2,E(IFMAPUI),1.)
   CALL AE0(N1*N2,E(IFMAPVI),1.)
   CALL AE0(N1*N2,E(IFMAPTI),1.)
   CALL AE0(N1*N2,E(IFMAPMI),1.)
ENDIF

CALL revu_GRDSPC (N1,N2,E(IDXU),E(IDXV),E(IDXT),E(IDXM)  &
                 ,E(IDYU),E(IDYV),E(IDYT),E(IDYM)  &
                 ,E(IFMAPU),E(IFMAPV),E(IFMAPT),E(IFMAPM)  &
                 ,xm,xt,ym,yt,jdim,deltay)

CALL revu_TRANSFM (n1,n2,n3,TOPT,E(ITOPU),E(ITOPV),E(ITOPM)  &
                  ,E(IRTGT),E(IRTGU),E(IRTGV),E(IRTGM),E(IF13U),E(IF13V)  &
                  ,E(IF13T),E(IF13M),E(IF23U),E(IF23V),E(IF23T)  &
                  ,E(IF23M),E(IFMAPU),E(IFMAPV),E(IDXU),E(IDXV)  &
                  ,E(IDXT),E(IDXM),E(IDYU),E(IDYV),E(IDYT),E(IDYM)  &
                  ,xm,xt,ym,yt,zm,zt,jdim,ztop,vctr1,vctr2)

if(dir.eq.'xdir')then

   if(gpnt.eq.'upnt')then
      call gradxur (n1,n2,n3,ia,iz,jaa,jzz,optyp,vc3da,vc3db,vctr1  &
                   ,e(irtgu),e(irtgt),e(idxt),dzt,e(ifmapui),e(ifmapt)  &
                   ,e(if13t),hw,vctr2,'t',jdim)
   elseif(gpnt.eq.'vpnt')then
      call gradxtr (n1,n2,n3,ia,iz,jaa,jzz,optyp,vc3da,vc3db,vctr1  &
                   ,e(irtgv),e(irtgm),e(idxm),dzt,e(ifmapvi),e(ifmapm)  &
                   ,e(if13m),hw,vctr2,'t',jdim)
   elseif(gpnt.eq.'wpnt')then
      call gradxtr (n1,n2,n3,ia,iz,jaa,jzz,optyp,vc3da,vc3db,vctr1  &
                   ,e(irtgt),e(irtgu),e(idxu),dzm,e(ifmapti),e(ifmapu)  &
                  ,e(if13u),ht,vctr2,'w',jdim)
   elseif(gpnt.eq.'tpnt')then
      call gradxtr (n1,n2,n3,ia,iz,jaa,jzz,optyp,vc3da,vc3db,vctr1  &
                   ,e(irtgt),e(irtgu),e(idxu),dzt,e(ifmapti),e(ifmapu)  &
                   ,e(if13u),hw,vctr2,'t',jdim)
   elseif(gpnt.eq.'npnt')then
      call gradxtr (n1,n2,n3,ia,iz,jaa,jzz,optyp,vc3da,vc3db,vctr1  &
                   ,e(irtgv),e(irtgm),e(idxm),dzm,e(ifmapvi),e(ifmapm)  &
                   ,e(if13m),ht,vctr2,'w',jdim)
   elseif(gpnt.eq.'opnt')then
      call gradxur (n1,n2,n3,ia,iz,jaa,jzz,optyp,vc3da,vc3db,vctr1  &
                   ,e(irtgu),e(irtgt),e(idxt),dzm,e(ifmapui),e(ifmapt)  &
                   ,e(if13t),ht,vctr2,'w',jdim)
   elseif(gpnt.eq.'ppnt')then
      call gradxur (n1,n2,n3,ia,iz,jaa,jzz,optyp,vc3da,vc3db,vctr1  &
                   ,e(irtgm),e(irtgv),e(idxv),dzt,e(ifmapmi),e(ifmapv)  &
                   ,e(if13v),hw,vctr2,'t',jdim)
   elseif(gpnt.eq.'mpnt')then
      call gradxur (n1,n2,n3,ia,iz,jaa,jzz,optyp,vc3da,vc3db,vctr1  &
                   ,e(irtgm),e(irtgv),e(idxv),dzm,e(ifmapmi),e(ifmapv)  &
                   ,e(if13v),ht,vctr2,'w',jdim)
   endif
   
elseif(dir.eq.'ydir')then

   if(gpnt.eq.'upnt')then
      call gradytr (n1,n2,n3,ia,iz,jaa,jzz,optyp,vc3da,vc3db,vctr1  &
                   ,e(irtgu),e(irtgm),e(idym),dzt,e(ifmapui),e(ifmapm)  &
                   ,e(if23m),hw,vctr2,'t',jdim)
   elseif(gpnt.eq.'vpnt')then
      call gradyvr (n1,n2,n3,ia,iz,jaa,jzz,optyp,vc3da,vc3db,vctr1  &
                   ,e(irtgv),e(irtgt),e(idyt),dzt,e(ifmapvi),e(ifmapt)  &
                   ,e(if23t),hw,vctr2,'t',jdim)
   elseif(gpnt.eq.'wpnt')then
      call gradytr (n1,n2,n3,ia,iz,jaa,jzz,optyp,vc3da,vc3db,vctr1  &
                   ,e(irtgt),e(irtgv),e(idyv),dzm,e(ifmapti),e(ifmapv)  &
                   ,e(if23v),ht,vctr2,'w',jdim)
   elseif(gpnt.eq.'tpnt')then
      call gradytr (n1,n2,n3,ia,iz,jaa,jzz,optyp,vc3da,vc3db,vctr1  &
                   ,e(irtgt),e(irtgv),e(idyv),dzt,e(ifmapti),e(ifmapv)  &
                   ,e(if23v),hw,vctr2,'t',jdim)
   elseif(gpnt.eq.'npnt')then
      call gradyvr (n1,n2,n3,ia,iz,jaa,jzz,optyp,vc3da,vc3db,vctr1  &
                   ,e(irtgv),e(irtgt),e(idyt),dzm,e(ifmapvi),e(ifmapt)  &
                   ,e(if23t),ht,vctr2,'w',jdim)
   elseif(gpnt.eq.'opnt')then
      call gradytr (n1,n2,n3,ia,iz,jaa,jzz,optyp,vc3da,vc3db,vctr1  &
                   ,e(irtgu),e(irtgm),e(idym),dzm,e(ifmapui),e(ifmapm)  &
                   ,e(if23m),ht,vctr2,'w',jdim)
   elseif(gpnt.eq.'ppnt')then
      call gradyvr (n1,n2,n3,ia,iz,jaa,jzz,optyp,vc3da,vc3db,vctr1  &
                   ,e(irtgm),e(irtgu),e(idyu),dzt,e(ifmapmi),e(ifmapu)  &
                   ,e(if23u),hw,vctr2,'t',jdim)
   elseif(gpnt.eq.'mpnt')then
      call gradyvr (n1,n2,n3,ia,iz,jaa,jzz,optyp,vc3da,vc3db,vctr1  &
                   ,e(irtgm),e(irtgu),e(idyu),dzm,e(ifmapmi),e(ifmapu)  &
                   ,e(if23u),ht,vctr2,'w',jdim)
   endif
   
elseif(dir.eq.'zdir')then

   if(gpnt.eq.'upnt')then
     call gradztr (n1,n2,n3,ia,iz,jaa,jzz,vc3da,vc3db,e(irtgu),dzm)
   elseif(gpnt.eq.'vpnt')then
     call gradztr (n1,n2,n3,ia,iz,jaa,jzz,vc3da,vc3db,e(irtgv),dzm)
   elseif(gpnt.eq.'wpnt')then
     call gradzwr (n1,n2,n3,ia,iz,jaa,jzz,vc3da,vc3db,e(irtgt),dzt)
   elseif(gpnt.eq.'tpnt')then
     call gradztr (n1,n2,n3,ia,iz,jaa,jzz,vc3da,vc3db,e(irtgt),dzm)
   elseif(gpnt.eq.'npnt')then
     call gradzwr (n1,n2,n3,ia,iz,jaa,jzz,vc3da,vc3db,e(irtgv),dzt)
   elseif(gpnt.eq.'opnt')then
     call gradzwr (n1,n2,n3,ia,iz,jaa,jzz,vc3da,vc3db,e(irtgu),dzt)
   elseif(gpnt.eq.'ppnt')then
     call gradztr (n1,n2,n3,ia,iz,jaa,jzz,vc3da,vc3db,e(irtgm),dzm)
   elseif(gpnt.eq.'mpnt')then
     call gradzwr (n1,n2,n3,ia,iz,jaa,jzz,vc3da,vc3db,e(irtgm),dzt)
   endif
   
endif

ierr=irfree(iaddr)

return
end

!***************************************************************************

subroutine revu_grad1 (n1,n2,n3)

dimension vc3da(n1,n2,n3),vc3db(n1,n2,n3),vc1da(*)  &
         ,rtge(n1,n2),rtgc(n1,n2),dx(n1,n2),dy(n1,n2)  &
         ,fmap(n1,n2),fmapi(n1,n2),dz(*),fq(n1,n2),hq(*),hq4(*)
character*(*) optyp,lev

! this is a general subroutine which computes any component of the
! gradient or divergence of vc3da and stores it in vc3db.

entry gradxur(n1,n2,n3,ia,iz,ja,jz,optyp,vc3da,vc3db,vc1da  &
   ,rtge,rtgc,dx,dz,fmapi,fmap,fq,hq,hq4,lev,jd)

if(optyp.eq.'gradnt')then
   do j = ja,jz
      do i = ia,iz
         do k = 1,n3
            vc3db(i,j,k) = (vc3da(i,j,k) * rtge(i,j)  &
               - vc3da(i-1,j,k) * rtge(i-1,j))  &
               * dx(i,j) / rtgc(i,j)
         enddo
      enddo
   enddo
else
   do j = ja,jz
      do i = ia,iz
         do k = 1,n3
            vc3db(i,j,k) = (vc3da(i,j,k) * rtge(i,j) * fmapi(i,j)  &
               - vc3da(i-1,j,k) * rtge(i-1,j) * fmapi(i-1,j))  &
               * dx(i,j) / rtgc(i,j) * fmap(i,j)
         enddo
      enddo
   enddo
endif

if(optyp.ne.'divstr')then
   if(lev.eq.'w')then
      do k = 1,n3
         hq4(k) = 0.25 * hq(k)
      enddo
   else
      do k = 2,n3
         hq4(k) = 0.25 * hq(k-1)
      enddo
   endif

   do j = ja,jz
      do i = ia,iz
         do k = 2,n3
            vc1da(k) = hq4(k) * (vc3da(i,j,k) + vc3da(i,j,k-1)  &
               + vc3da(i-1,j,k) + vc3da(i-1,j,k-1))
         enddo
         do k = 2,n3-1
            vc3db(i,j,k) = vc3db(i,j,k)  &
               + fq(i,j) * dz(k) * (vc1da(k+1) - vc1da(k))
         enddo
         vc3db(i,j,1) = vc3db(i,j,2)
         if(lev.eq.'w') vc3db(i,j,n3-1) = vc3db(i,j,n3-2)
         if(lev.eq.'t') vc3db(i,j,n3) = vc3db(i,j,n3-1)
      enddo
   enddo
endif

return

entry gradxtr (n1,n2,n3,ia,iz,ja,jz,optyp,vc3da,vc3db,vc1da  &
              ,rtge,rtgc,dx,dz,fmapi,fmap,fq,hq,hq4,lev,jd)

if(optyp.eq.'gradnt')then
   do j = ja,jz
      do i = ia,iz
         do k = 1,n3
            vc3db(i,j,k) = (vc3da(i+1,j,k) * rtge(i+1,j)  &
               - vc3da(i,j,k) * rtge(i,j))  &
               * dx(i,j) / rtgc(i,j)
         enddo
      enddo
   enddo
else
   do j = ja,jz
      do i = ia,iz
         do k = 1,n3
            vc3db(i,j,k) = (vc3da(i+1,j,k) * rtge(i+1,j)  &
               * fmapi(i+1,j) - vc3da(i,j,k) * rtge(i,j)  &
               * fmapi(i,j)) * dx(i,j) / rtgc(i,j) * fmap(i,j)
         enddo
      enddo
   enddo
endif

if(optyp.ne.'divstr')then
   if(lev.eq.'w')then
      do k = 1,n3
         hq4(k) = 0.25 * hq(k)
      enddo
   else
      do k = 2,n3
         hq4(k) = 0.25 * hq(k-1)
      enddo
   endif

   do j = ja,jz
      do i = ia,iz
         do k = 2,n3
            vc1da(k) = hq4(k) * (vc3da(i,j,k) + vc3da(i,j,k-1)  &
               + vc3da(i+1,j,k) + vc3da(i+1,j,k-1))
         enddo
         do k = 2,n3-1
            vc3db(i,j,k) = vc3db(i,j,k)  &
               + fq(i,j) * dz(k) * (vc1da(k+1) - vc1da(k))
         enddo
         vc3db(i,j,1) = vc3db(i,j,2)
         if(lev.eq.'w') vc3db(i,j,n3-1) = vc3db(i,j,n3-2)
         if(lev.eq.'t') vc3db(i,j,n3) = vc3db(i,j,n3-1)
      enddo
   enddo
endif

return

entry gradyvr (n1,n2,n3,ia,iz,ja,jz,optyp,vc3da,vc3db,vc1da  &
              ,rtge,rtgc,dy,dz,fmapi,fmap,fq,hq,hq4,lev,jd)

if(optyp.eq.'gradnt')then
   do j = ja,jz
      do i = ia,iz
         do k = 1,n3
            vc3db(i,j,k) = (vc3da(i,j,k) * rtge(i,j)  &
               - vc3da(i,j-jd,k) * rtge(i,j-jd))  &
               * dy(i,j) / rtgc(i,j)
         enddo
      enddo
   enddo
else
   do j = ja,jz
      do i = ia,iz
         do k = 1,n3
            vc3db(i,j,k) = (vc3da(i,j,k) * rtge(i,j) * fmapi(i,j)  &
               - vc3da(i,j-jd,k) * rtge(i,j-jd) * fmapi(i,j-jd))  &
               * dy(i,j) / rtgc(i,j) * fmap(i,j)
         enddo
      enddo
   enddo
endif

if(optyp.ne.'divstr')then
   if(lev.eq.'w')then
      do k = 1,n3
         hq4(k) = 0.25 * hq(k)
      enddo
   else
      do k = 2,n3
         hq4(k) = 0.25 * hq(k-1)
      enddo
   endif

   do j = ja,jz
      do i = ia,iz
         do k = 2,n3
            vc1da(k) = hq4(k) * (vc3da(i,j,k) + vc3da(i,j,k-1)  &
               + vc3da(i,j-jd,k) + vc3da(i,j-jd,k-1))
         enddo
         do k = 2,n3-1
            vc3db(i,j,k) = vc3db(i,j,k)  &
               + fq(i,j) * dz(k) * (vc1da(k+1) - vc1da(k))
         enddo
         vc3db(i,j,1) = vc3db(i,j,2)
         if(lev.eq.'w') vc3db(i,j,n3-1) = vc3db(i,j,n3-2)
         if(lev.eq.'t') vc3db(i,j,n3) = vc3db(i,j,n3-1)
      enddo
   enddo
endif

return

entry gradytr (n1,n2,n3,ia,iz,ja,jz,optyp,vc3da,vc3db,vc1da  &
              ,rtge,rtgc,dy,dz,fmapi,fmap,fq,hq,hq4,lev,jd)

if(optyp.eq.'gradnt')then
   do j = ja,jz
      do i = ia,iz
         do k = 1,n3
            vc3db(i,j,k) = (vc3da(i,j+jd,k) * rtge(i,j+jd)  &
               - vc3da(i,j,k) * rtge(i,j))  &
               * dy(i,j) / rtgc(i,j)
         enddo
      enddo
   enddo
else
   do j = ja,jz
      do i = ia,iz
         do k = 1,n3
            vc3db(i,j,k) = (vc3da(i,j+jd,k) * rtge(i,j+jd)  &
               * fmapi(i,j+jd)  &
               - vc3da(i,j,k) * rtge(i,j) * fmapi(i,j))  &
               * dy(i,j) / rtgc(i,j) * fmap(i,j)
         enddo
      enddo
   enddo
endif

if(optyp.ne.'divstr')then
   if(lev.eq.'w')then
      do k = 1,n3
         hq4(k) = 0.25 * hq(k)
      enddo
   else
      do k = 2,n3
         hq4(k) = 0.25 * hq(k-1)
      enddo
   endif

   do j = ja,jz
      do i = ia,iz
         do k = 2,n3
            vc1da(k) = hq4(k) * (vc3da(i,j,k) + vc3da(i,j,k-1)  &
               + vc3da(i,j+jd,k) + vc3da(i,j+jd,k-1))
         enddo
         do k = 2,n3-1
            vc3db(i,j,k) = vc3db(i,j,k)  &
               + fq(i,j) * dz(k) * (vc1da(k+1) - vc1da(k))
         enddo
         vc3db(i,j,1) = vc3db(i,j,2)
         if(lev.eq.'w') vc3db(i,j,n3-1) = vc3db(i,j,n3-2)
         if(lev.eq.'t') vc3db(i,j,n3) = vc3db(i,j,n3-1)
      enddo
   enddo
endif

return

entry gradzwr (n1,n2,n3,ia,iz,ja,jz,vc3da,vc3db,rtgc,dz)

do j = ja,jz
   do i = ia,iz
      do k = 2,n3
         vc3db(i,j,k) = (vc3da(i,j,k) - vc3da(i,j,k-1)) * dz(k) / rtgc(i,j)
      enddo
   enddo
enddo
return

entry gradztr (n1,n2,n3,ia,iz,ja,jz,vc3da,vc3db,rtgc,dz)

do j = ja,jz
   do i = ia,iz
      do k = 1,n3-1
         vc3db(i,j,k) = (vc3da(i,j,k+1) - vc3da(i,j,k)) * dz(k) / rtgc(i,j)
      enddo
   enddo
enddo
return
end

!***************************************************************************

SUBROUTINE revu_POLARST (N1,N2,GLAT,GLON,FMAPU,FMAPV,FMAPT,FMAPM  &
                        ,FMAPUI,FMAPVI,FMAPTI,FMAPMI,xm,xt,ym,yt  &
                        ,platn,plonn)

DIMENSION GLAT(N1,N2),GLON(N1,N2),FMAPU(N1,N2),FMAPUI(N1,N2)  &
         ,FMAPV(N1,N2),FMAPT(N1,N2),FMAPM(N1,N2),FMAPVI(N1,N2)  &
         ,FMAPTI(N1,N2),FMAPMI(N1,N2),xm(*),xt(*),ym(*),yt(*)

! Calculates map factors and inverse map factors at u,v,t,m-points and
! geographical lat/lon at t-points for a given polar stereographic grid

!print*, 'in polarst:n1,n2=',n1,n2

ERAD=6367000.
c1 = (2. * erad) ** 2
DO J = 1,N2
   DO I = 1,N1
      xm2 = xm(i) * xm(i)
      xt2 = xt(i) * xt(i)
      ym2 = ym(j) * ym(j)
      yt2 = yt(j) * yt(j)

      FMAPT(I,J) = 1. + (xt2 + yt2) / c1
      FMAPU(I,J) = 1. + (xm2 + yt2) / c1
      FMAPV(I,J) = 1. + (xt2 + ym2) / c1
      FMAPM(I,J) = 1. + (xm2 + ym2) / c1

      FMAPUI(I,J) = 1.0 / FMAPU(I,J)
      FMAPVI(I,J) = 1.0 / FMAPV(I,J)
      FMAPTI(I,J) = 1.0 / FMAPT(I,J)
      FMAPMI(I,J) = 1.0 / FMAPM(I,J)

      call xy_ll(GLAT(I,J),GLON(I,J),platn,plonn,XT(I),YT(J))

      !write(6,344)i,j,fmapt(i,j),fmapm(i,j),glat(i,j),glon(i,j),XT(I),YT(J)
      !344 format('polst:i,j,fmt,fmm,glt,gln,x,y',2i4,6e15.6)

   ENDDO
ENDDO

!IF (IHTRAN .EQ. 0) THEN
!   CALL AE0(N1*N2,FMAPU,1.)
!   CALL AE0(N1*N2,FMAPV,1.)
!   CALL AE0(N1*N2,FMAPT,1.)
!   CALL AE0(N1*N2,FMAPM,1.)
!   CALL AE0(N1*N2,FMAPUI,1.)
!   CALL AE0(N1*N2,FMAPVI,1.)
!   CALL AE0(N1*N2,FMAPTI,1.)
!   CALL AE0(N1*N2,FMAPMI,1.)
!ENDIF

RETURN
END

!***************************************************************************

SUBROUTINE revu_GRDSPC (N1,N2,DXU,DXV,DXT,DXM,DYU,DYV,DYT,DYM  &
                       ,FMAPU,FMAPV,FMAPT,FMAPM,xm,xt,ym,yt,jdim,deltay)
   
DIMENSION DXU(N1,N2),DXV(N1,N2),DXT(N1,N2),DXM(N1,N2)  &
         ,DYU(N1,N2),DYV(N1,N2),DYT(N1,N2),DYM(N1,N2)  &
         ,FMAPU(N1,N2),FMAPV(N1,N2),FMAPT(N1,N2),FMAPM(N1,N2)  &
         ,xm(*),xt(*),ym(*),yt(*)

DO J=1,N2
   DO I=1,N1-1
      DXU(I,J)=FMAPU(I,J)/(XT(I+1)-XT(I))
      DXM(I,J)=FMAPM(I,J)/(XT(I+1)-XT(I))
   ENDDO
   DXU(N2,J)=DXU(N2-1,J)*FMAPU(N2,J)/FMAPU(N2-1,J)
   DXM(N2,J)=DXM(N2-1,J)*FMAPM(N2,J)/FMAPM(N2-1,J)
   DO I=2,N1
      DXV(I,J)=FMAPV(I,J)/(XM(I)-XM(I-1))
      DXT(I,J)=FMAPT(I,J)/(XM(I)-XM(I-1))
   ENDDO
   DXV(1,J)=DXV(2,J)*FMAPV(1,J)/FMAPV(2,J)
   DXT(1,J)=DXT(2,J)*FMAPT(1,J)/FMAPT(2,J)
ENDDO

IF(JDIM.EQ.1)THEN
   DO I=1,N1
      DO J=1,N2-1
         DYV(I,J)=FMAPV(I,J)/(YT(J+1)-YT(J))
         DYM(I,J)=FMAPM(I,J)/(YT(J+1)-YT(J))
      ENDDO
      DYV(I,N2)=DYV(I,N2-1)*FMAPV(I,N2)/FMAPV(I,N2-1)
      DYM(I,N2)=DYM(I,N2-1)*FMAPM(I,N2)/FMAPM(I,N2-1)
      DO J=2,N2
         DYU(I,J)=FMAPU(I,J)/(YM(J)-YM(J-1))
         DYT(I,J)=FMAPT(I,J)/(YM(J)-YM(J-1))
      ENDDO
      DYU(I,1)=DYU(I,2)*FMAPU(I,1)/FMAPU(I,2)
      DYT(I,1)=DYT(I,2)*FMAPT(I,1)/FMAPT(I,2)
   ENDDO
ELSE
   DO I=1,N1
      DO J=1,N2
         DYU(I,J)=1./DELTAY
         DYV(I,J)=1./DELTAY
         DYT(I,J)=1./DELTAY
         DYM(I,J)=1./DELTAY
      ENDDO
   ENDDO
ENDIF

RETURN
END

!***************************************************************************

SUBROUTINE revu_TRANSFM (N1,N2,n3,TOPT,TOPU,TOPV,TOPM,RTGT,RTGU  &
                        ,RTGV,RTGM,F13U,F13V,F13T,F13M,F23U,F23V,F23T,F23M  &
                        ,FMAPU,FMAPV,DXU,DXV,DXT,DXM,DYU,DYV,DYT,DYM  &
                        ,xm,xt,ym,yt,zm,zt,jdim,ztop,ht,hw)
                        
DIMENSION TOPT(N1,N2),TOPU(N1,N2),TOPV(N1,N2),TOPM(N1,N2)  &
         ,RTGT(N1,N2),RTGU(N1,N2),RTGV(N1,N2),RTGM(N1,N2)  &
         ,F13U(N1,N2),F13V(N1,N2),F13T(N1,N2),F13M(N1,N2)  &
         ,F23U(N1,N2),F23V(N1,N2),F23T(N1,N2),F23M(N1,N2)  &
         ,FMAPU(N1,N2),FMAPV(N1,N2)  &
         ,DXU(N1,N2),DXV(N1,N2),DXT(N1,N2),DXM(N1,N2)  &
         ,DYU(N1,N2),DYV(N1,N2),DYT(N1,N2),DYM(N1,N2)  &
         ,xm(*),xt(*),ym(*),yt(*),zm(*),zt(*),ht(*),hw(*)
DATA TERDEV/0./
SAVE

! This routine computes the coordinate transformation constants
! based on the topographical values of TOPT.

DO J=1,N2
   DO I=1,N1-1
      TOPU(I,J)=TOPT(I,J)+(TOPT(I+1,J)-TOPT(I,J))  &
               *(XM(I)-XT(I))/(XT(I+1)-XT(I))
      TERDEV=MAX(TERDEV,ABS(TOPT(I,J)))
   ENDDO
   TOPU(N1,J)=TOPT(N1,J)+(TOPT(N1,J)-TOPT(N1-1,J))  &
             *(XM(N1)-XT(N1))/(XT(N1)-XT(N1-1))
ENDDO
IF(TERDEV.LT.1.E-6)THEN
   ITOPO=0
ELSE
   ITOPO=1
ENDIF

IF(JDIM.EQ.1)THEN
   DO I=1,N1
      DO J=1,N2-1
         TOPV(I,J)=TOPT(I,J)+(TOPT(I,J+1)-TOPT(I,J))  &
                  *(YM(J)-YT(J))/(YT(J+1)-YT(J))
         TOPM(I,J)=TOPU(I,J)+(TOPU(I,J+1)-TOPU(I,J))  &
                  *(YM(J)-YT(J))/(YT(J+1)-YT(J))
      ENDDO
      TOPV(I,N2)=TOPT(I,N2)+(TOPT(I,N2)-TOPT(I,N2-1))  &
                *(YM(N2)-YT(N2))/(YT(N2)-YT(N2-1))
      TOPM(I,N2)=TOPU(I,N2)+(TOPU(I,N2)-TOPU(I,N2-1))  &
                *(YM(N2)-YT(N2))/(YT(N2)-YT(N2-1))
   ENDDO
ELSE
   DO I=1,N1
      DO J=1,N2
         TOPV(I,J)=TOPT(I,J)
         TOPM(I,J)=TOPU(I,J)
      ENDDO
   ENDDO
ENDIF

DO K=1,N3
  HT(K)=ZT(K)/ZTOP-1.0
  HW(K)=ZM(K)/ZTOP-1.0
ENDDO

DO J=1,N2
  DO I=1,N1
    RTGT(I,J)=1.-TOPT(I,J)/ZTOP
    RTGU(I,J)=1.-TOPU(I,J)/ZTOP
    RTGV(I,J)=1.-TOPV(I,J)/ZTOP
    RTGM(I,J)=1.-TOPM(I,J)/ZTOP
    F13T(I,J)=(TOPU(I,J)-TOPU(I-1,J))*DXT(I,J)/RTGT(I,J)
    F13U(I,J)=(TOPT(I+1,J)-TOPT(I,J))*DXU(I,J)/RTGU(I,J)
    F13V(I,J)=(TOPM(I,J)-TOPM(I-1,J))*DXV(I,J)/RTGV(I,J)
    F13M(I,J)=(TOPV(I+1,J)-TOPV(I,J))*DXM(I,J)/RTGM(I,J)
    F23T(I,J)=(TOPV(I,J)-TOPV(I,J-JDIM))*DYT(I,J)/RTGT(I,J)
    F23U(I,J)=(TOPM(I,J)-TOPM(I,J-JDIM))*DYU(I,J)/RTGU(I,J)
    F23V(I,J)=(TOPT(I,J+JDIM)-TOPT(I,J))*DYV(I,J)/RTGV(I,J)
    F23M(I,J)=(TOPU(I,J+JDIM)-TOPU(I,J))*DYM(I,J)/RTGM(I,J)
  ENDDO
ENDDO
DO J=1,N2
  F13T(1,J)=F13U(1,J)
  F13V(1,J)=F13M(1,J)
  F13U(N1,J)=F13T(N1,J)
  F13M(N1,J)=F13V(N1,J)
ENDDO
IF(JDIM.EQ.1)THEN
  DO I=1,N1
    F23T(I,1)=F23V(I,1)
    F23U(I,1)=F23M(I,1)
    F23V(I,N2)=F23T(I,N2)
    F23M(I,N2)=F23U(I,N2)
  ENDDO
ENDIF

RETURN
END

