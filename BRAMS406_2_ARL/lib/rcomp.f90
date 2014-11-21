!############################# Change Log ##################################
! 2.3.0.1
!
! 000830 CJT rams_comp ##
!            Corrected reference to "cpr" and "r" to "cpor" and "rgas". ##
!
!###########################################################################
!  Copyright (C)  1990, 1995, 1999, 2000 - All Rights Reserved
!  Regional Atmospheric Modeling System - RAMS
!  Mission Research Corporation / *ASTeR Division
!###########################################################################

subroutine RAMS_comp(n1,n2,n3)
use somevars
dimension a(n1,n2,n3),b(n1,n2,n3),c(n1,n2,n3),e(n1,n2,n3),topt(n1,n2)
dimension theta(n1,n2,n3),pp(n1,n2,n3),slp(n1,n2),z(n1,n2,n3)
include 'rcommons.h'
include 'rconstants.h'
dimension slmsts0(12)
dimension h(n1,n2,n3,nclouds)
data slmsts0/0.395, 0.410, 0.435, 0.485, 0.451, 0.420  &
            ,0.477, 0.476, 0.426, 0.492, 0.482, 0.863/

entry RAMS_comp_onecloud(n1,n2,n3,l,a,h)
   if (l > nclouds) then
      do k=1,n3
         do j=1,n2
            do i=1,n1
               a(i,j,k)=0.
            end do
         end do
      end do
   else
      do k=1,n3
         do j=1,n2
            do i=1,n1
               a(i,j,k)=h(i,j,k,l)
            end do
         end do
      end do
   end if
return

entry RAMS_comp_vals(n1,n2,n3,a)
   print*,'==================== values =========================='
   do k=1,n3
      do j=1,n2
         do i=1,n1
            if(a(i,j,k).ne.0.) write(*,'(3i3,e14.6)') i,j,k  &
                                                     ,a(i,j,k)
         enddo
      enddo
   enddo
   print*,'======================================================'
return

entry RAMS_comp_tot(n1,n2,n3,a)
   tot=0.
   do k=1,n3
      do j=1,n2
         do i=1,n1
            tot=tot+a(i,j,k)
         enddo
      enddo
   enddo
   write(*,'(a,e12.6)') '-> total- ',tot
return

entry RAMS_comp_maxval(n1,n2,n3,a)
   zmax=-1.0e30
   do k=1,n3
      do j=1,n2
         do i=1,n1
            if(a(i,j,k).gt.zmax) then
               zmax=a(i,j,k)
               maxx=i
               maxy=j
               maxz=k
            endif
         enddo
      enddo
   enddo
   write(*,'(a,e12.6,a,3i3)') '-> max- ',zmax,' at i,j,k-',maxx  &
                              ,maxy,maxz
return

entry RAMS_comp_minval(n1,n2,n3,a)
   zmin=1.0e30
   do k=1,n3
      do j=1,n2
         do i=1,n1
            if(a(i,j,k).lt.zmin) then
               zmin=a(i,j,k)
               minx=i
               miny=j
               minz=k
            endif
         enddo
      enddo
   enddo
   write(*,'(a,e12.6,a,3i3)') '-> min- ',zmin,' at i,j,k-',minx  &
                              ,miny,minz
return

entry RAMS_comp_zero(n1,n2,n3,a)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=0.
         enddo
      enddo
   enddo
return

entry RAMS_comp_1minus(n1,n2,n3,a)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=1.-a(i,j,k)
         enddo
      enddo
   enddo
return

entry RAMS_comp_mults(n1,n2,n3,a,s)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=a(i,j,k) * s
         enddo
      enddo
   enddo
return

entry RAMS_comp_accum(n1,n2,n3,a,b)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=a(i,j,k)+b(i,j,k)
         enddo
      enddo
   enddo
return

entry RAMS_comp_noneg(n1,n2,n3,a)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=max(a(i,j,k),0.)
         enddo
      enddo
   enddo
return

entry RAMS_comp_subt(n1,n2,n3,a,b)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=a(i,j,k)-b(i,j,k)
         enddo
      enddo
   enddo
return

entry RAMS_comp_mult(n1,n2,n3,a,b)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=a(i,j,k)*b(i,j,k)
         enddo
      enddo
   enddo
return

entry RAMS_comp_z(n1,n2,n3,a,c,ngrd)
   do k=1,n3
      do j=1,n2
         do i=1,n1
!            a(i,j,k)=c(i,j,1)  &
!                 +ztn(k,ngrd)*(1.-c(i,j,1)/zmn(nnzp(1)-1,1))
             a(i,j,k)=zmn(k,ngrd)*(1.-c(i,j,1)/zmn(nnzp(1)-1,1))
         enddo
      enddo
   enddo
return

entry RAMS_comp_rotate(n1,n2,n3,a,b,ngrd)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            call xy_ll(qlat,qlon,platn(ngrd),plonn(ngrd)  &
               ,xtn(i,ngrd),ytn(j,ngrd))
            u=a(i,j,k)
            v=b(i,j,k)
            call uvtoueve(u,v,a(i,j,k),b(i,j,k)  &
                         ,qlat,qlon,platn(ngrd),plonn(ngrd))
         enddo
      enddo
   enddo
return

entry RAMS_comp_tempK(n1,n2,n3,a,b)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=a(i,j,k)*b(i,j,k)/cp
         enddo
      enddo
   enddo
return

entry RAMS_comp_press(n1,n2,n3,a)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=(a(i,j,k)/cp)**cpor*p00*.01
         enddo
      enddo
   enddo
return

entry RAMS_comp_tempC(n1,n2,n3,a)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=a(i,j,k)-273.16
         enddo
      enddo
   enddo
return

entry RAMS_comp_tempF(n1,n2,n3,a)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=(a(i,j,k)-273.16)*1.8+32.
         enddo
      enddo
   enddo
return

entry RAMS_comp_wcms(n1,n2,n3,a)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=a(i,j,k)*100.
         enddo
      enddo
   enddo
return

entry RAMS_comp_avgw(n1,n2,n3,a)
   do k=n3,2,-1
      do j=1,n2
         do i=1,n1
            a(i,j,k)=0.5*(a(i,j,k)+a(i,j,k-1))
         enddo
      enddo
   enddo
return

entry RAMS_comp_avgu(n1,n2,n3,a)
   do k=1,n3
      do j=1,n2
         do i=n1,2,-1
            a(i,j,k)=0.5*(a(i,j,k)+a(i-1,j,k))
         enddo
      enddo
   enddo
return

entry RAMS_comp_avgv(n1,n2,n3,a)
   do k=1,n3
      do j=n2,2,-1
         do i=1,n1
            a(i,j,k)=0.5*(a(i,j,k)+a(i,j-1,k))
         enddo
      enddo
   enddo
return

entry RAMS_comp_sfcdiv(n1,n2,n3,a,ngrd)
   do j=1,n2
      do i=1,n1
         a(i,j,1)=-(a(i,j,2)-a(i,j,1))*dztn(2,ngrd)
      enddo
   enddo
return

entry RAMS_comp_rt(n1,n2,n3,a)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=max(0.,a(i,j,k))*1000.
         enddo
      enddo
   enddo
return

entry RAMS_comp_hr_pcprate(n1,n2,n3,a,b,c)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=(b(i,j,k)-c(i,j,k))
         enddo
      enddo
   enddo
return

entry RAMS_comp_speed(n1,n2,n3,a,b)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=sqrt(a(i,j,k)**2+b(i,j,k)**2)
         enddo
      enddo
   enddo
return

entry RAMS_comp_dir(n1,n2,n3,a,b,ngrd)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            call xy_ll(qlat,qlon,platn(ngrd),plonn(ngrd)  &
               ,xtn(i,ngrd),ytn(j,ngrd))
            u=a(i,j,k)
            v=b(i,j,k)
            call uvtoueve(u,v,a(i,j,k),b(i,j,k)  &
                         ,qlat,qlon,platn(ngrd),plonn(ngrd))
            call winddf(a(i,j,k),ff,a(i,j,k),b(i,j,k))
         enddo
      enddo
   enddo
return

entry RAMS_comp_dewK(n1,n2,n3,a,b,c)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            xpress=(b(i,j,k)/cp)**cpor*p00
            xtemp=c(i,j,k)*b(i,j,k)/cp
            xwatsat=rs(xpress,xtemp)
            a(i,j,k)=td(xpress,min(a(i,j,k),xwatsat) )
         enddo
      enddo
   enddo
return

entry RAMS_comp_thete(n1,n2,n3,a,b,c)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            xpress=(b(i,j,k)/cp)**cpor*p00
            xtemp=c(i,j,k)*b(i,j,k)/cp
            xwatsat=rs(xpress,xtemp)
            a(i,j,k)=c(i,j,k)*exp( alvl*xwatsat  &
                 /(cp*td(xpress,min(a(i,j,k),xwatsat) )) )
         enddo
      enddo
   enddo
return

entry RAMS_comp_thetv(n1,n2,n3,a,b)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=a(i,j,k)*(1. + .61 * b(i,j,k))
         enddo
      enddo
   enddo
return

entry RAMS_comp_bowen(n1,n2,n3,a,b)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=a(i,j,k)/max(1.e-12,b(i,j,k))*1004./2.5e6
         enddo
      enddo
   enddo
return

entry RAMS_comp_rh(n1,n2,n3,a,b,c)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            xtemp=c(i,j,k)*b(i,j,k)/cp
            xpress=(b(i,j,k)/cp)**cpor*p00
            a(i,j,k)=100.*min(1.  &
                 ,max(0.,a(i,j,k)/rs(xpress,xtemp)))
         enddo
      enddo
   enddo
return

entry RAMS_comp_watsat(n1,n2,n3,a,b,c)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            c(i,j,k)=c(i,j,k)*a(i,j,k)/cp
            b(i,j,k)=(b(i,j,k)/cp)**cpor*p00
            a(i,j,k)=rs(b(i,j,k),c(i,j,k))
         enddo
      enddo
   enddo
return

entry RAMS_comp_relvortx(n1,n2,n3,a,b,c,topt,ngrd)

   factor = ztn(1,ngrd) / ztn(2,ngrd)
   do j=1,n2
      do i=1,n1
         a(i,j,1) = a(i,j,2) * factor
      enddo
   enddo

   call gradr(n1,n2,n3,2,n1-1,1,n2-1,b,c,'ydir','wpnt',topt  &
      ,xmn(1,ngrd),xtn(1,ngrd),ymn(1,ngrd),ytn(1,ngrd)  &
      ,zmn(1,ngrd),ztn(1,ngrd),deltayn(ngrd)  &
      ,dzmn(1,ngrd),dztn(1,ngrd),vctr1,vctr2,zmn(nnzp(1)-1,1)  &
      ,jdim,ihtran,platn(ngrd),plonn(ngrd))
   call gradr(n1,n2,n3,2,n1-1,1,n2-1,a,b,'zdir','vpnt',topt  &
      ,xmn(1,ngrd),xtn(1,ngrd),ymn(1,ngrd),ytn(1,ngrd)  &
      ,zmn(1,ngrd),ztn(1,ngrd),deltayn(ngrd)  &
      ,dzmn(1,ngrd),dztn(1,ngrd),vctr1,vctr2,zmn(nnzp(1)-1,1)  &
      ,jdim,ihtran,platn(ngrd),plonn(ngrd))

   do k=1,n3
      do j=1,n2
         do i=1,n1
            b(i,j,k) = c(i,j,k) - b(i,j,k)
         enddo
      enddo
   enddo

   do j = 1,n2
      j1 = max(j-1,1)
      j2 = min(j,n2-1)
      do i = 1,n1
         do k = 1,n3
            k1 = max(k-1,1)
            k2 = min(k,n3-1)
            a(i,j,k) =0.25 * (b(i,j1,k1) + b(i,j1,k2)  &
                            + b(i,j2,k1) + b(i,j2,k2))
         enddo
      enddo
   enddo
  return

entry RAMS_comp_relvorty(n1,n2,n3,a,b,c,topt,ngrd)

   factor = ztn(1,ngrd) / ztn(2,ngrd)
   do j=1,n2
      do i=1,n1
         a(i,j,1) = a(i,j,2) * factor
      enddo
   enddo

   call gradr(n1,n2,n3,1,n1-1,2,n2-1,b,c,'xdir','wpnt',topt  &
      ,xmn(1,ngrd),xtn(1,ngrd),ymn(1,ngrd),ytn(1,ngrd)  &
      ,zmn(1,ngrd),ztn(1,ngrd),deltayn(ngrd)  &
      ,dzmn(1,ngrd),dztn(1,ngrd),vctr1,vctr2,zmn(nnzp(1)-1,1)  &
      ,jdim,ihtran,platn(ngrd),plonn(ngrd))
   call gradr(n1,n2,n3,1,n1-1,2,n2-1,a,b,'zdir','upnt',topt  &
      ,xmn(1,ngrd),xtn(1,ngrd),ymn(1,ngrd),ytn(1,ngrd)  &
      ,zmn(1,ngrd),ztn(1,ngrd),deltayn(ngrd)  &
      ,dzmn(1,ngrd),dztn(1,ngrd),vctr1,vctr2,zmn(nnzp(1)-1,1)  &
      ,jdim,ihtran,platn(ngrd),plonn(ngrd))

   do k=1,n3
      do j=1,n2
         do i=1,n1
            b(i,j,k) = b(i,j,k) - c(i,j,k)
         enddo
      enddo
   enddo

   do j = 1,n2
      do i = 1,n1
         i1 = max(i-1,1)
         i2 = min(i,n1-1)
         do k = 1,n3
            k1 = max(k-1,1)
            k2 = min(k,n3-1)
            a(i,j,k) = 0.25 * (b(i1,j,k1) + b(i1,j,k2)  &
                             + b(i2,j,k1) + b(i2,j,k2))
         enddo
      enddo
   enddo


   return

entry RAMS_comp_relvortz(n1,n2,n3,a,b,c,topt,ngrd)

   factor = ztn(1,ngrd) / ztn(2,ngrd)
   do j=1,n2
      do i=1,n1
         a(i,j,1) = a(i,j,2) * factor
         b(i,j,1) = b(i,j,2) * factor
      enddo
   enddo

   call gradr(n1,n2,n3,1,n1-1,1,n2-1,b,c,'xdir','vpnt',topt  &
      ,xmn(1,ngrd),xtn(1,ngrd),ymn(1,ngrd),ytn(1,ngrd)  &
      ,zmn(1,ngrd),ztn(1,ngrd),deltayn(ngrd)  &
      ,dzmn(1,ngrd),dztn(1,ngrd),vctr1,vctr2,zmn(nnzp(1)-1,1)  &
      ,jdim,ihtran,platn(ngrd),plonn(ngrd))

   call gradr(n1,n2,n3,1,n1-1,1,n2-1,a,b,'ydir','upnt',topt  &
      ,xmn(1,ngrd),xtn(1,ngrd),ymn(1,ngrd),ytn(1,ngrd)  &
      ,zmn(1,ngrd),ztn(1,ngrd),deltayn(ngrd)  &
      ,dzmn(1,ngrd),dztn(1,ngrd),vctr1,vctr2,zmn(nnzp(1)-1,1)  &
      ,jdim,ihtran,platn(ngrd),plonn(ngrd))

   do k=1,n3
      do j=1,n2
         do i=1,n1
            b(i,j,k) = c(i,j,k) - b(i,j,k)
         enddo
      enddo
   enddo

   do j = 1,n2
      j1 = max(j-1,1)
      j2 = min(j,n2-1)
      do i = 1,n1
         i1 = max(i-1,1)
         i2 = min(i,n1-1)
         do k = 1,n3
            a(i,j,k) = 0.25 * (b(i1,j1,k) + b(i1,j2,k)  &
                             + b(i2,j1,k) + b(i2,j2,k))
         enddo
      enddo
   enddo
   return

entry RAMS_comp_totvortz(n1,n2,n3,a,b,c,topt,ngrd)

   factor = ztn(1,ngrd) / ztn(2,ngrd)
   do j=1,n2
      do i=1,n1
         a(i,j,1) = a(i,j,2) * factor
         b(i,j,1) = b(i,j,2) * factor
      enddo
   enddo

   call gradr(n1,n2,n3,1,n1-1,1,n2-1,b,c,'xdir','vpnt',topt  &
      ,xmn(1,ngrd),xtn(1,ngrd),ymn(1,ngrd),ytn(1,ngrd)  &
      ,zmn(1,ngrd),ztn(1,ngrd),deltayn(ngrd)  &
      ,dzmn(1,ngrd),dztn(1,ngrd),vctr1,vctr2,zmn(nnzp(1)-1,1)  &
      ,jdim,ihtran,platn(ngrd),plonn(ngrd))
   call gradr(n1,n2,n3,1,n1-1,1,n2-1,a,b,'ydir','upnt',topt  &
      ,xmn(1,ngrd),xtn(1,ngrd),ymn(1,ngrd),ytn(1,ngrd)  &
      ,zmn(1,ngrd),ztn(1,ngrd),deltayn(ngrd)  &
      ,dzmn(1,ngrd),dztn(1,ngrd),vctr1,vctr2,zmn(nnzp(1)-1,1)  &
      ,jdim,ihtran,platn(ngrd),plonn(ngrd))

   do k=1,n3
      do j=1,n2
         do i=1,n1
            b(i,j,k) = c(i,j,k) - b(i,j,k)
         enddo
      enddo
   enddo

   do j = 1,n2
      j1 = max(j-1,1)
      j2 = min(j,n2-1)
      do i = 1,n1
         i1 = max(i-1,1)
         i2 = min(i,n1-1)
         do k = 1,n3
            a(i,j,k) = 0.25 * (b(i1,j1,k) + b(i1,j2,k)  &
                             + b(i2,j1,k) + b(i2,j2,k))
         enddo
      enddo
   enddo

   omega2 = 2. * 7.292e-5
   do j = 1,n2
      do i = 1,n1
         call xy_ll(xlat,xlon,platn(ngrd),plonn(ngrd)  &
              ,xtn(i,ngrd),ytn(j,ngrd))
         fcor = omega2 * sin(xlat * pi180)
         do k = 1,n3
            a(i,j,k) = a(i,j,k) + fcor
         enddo
      enddo
   enddo
   return

entry RAMS_comp_potvortz(n1,n2,n3,a,b,c,e,topt,ngrd)

   call gradr(n1,n2,n3,1,n1-1,1,n2-1,b,e,'zdir','tpnt',topt  &
      ,xmn(1,ngrd),xtn(1,ngrd),ymn(1,ngrd),ytn(1,ngrd)  &
      ,zmn(1,ngrd),ztn(1,ngrd),deltayn(ngrd)  &
      ,dzmn(1,ngrd),dztn(1,ngrd),vctr1,vctr2,zmn(nnzp(1)-1,1)  &
      ,jdim,ihtran,platn(ngrd),plonn(ngrd))

   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k) = a(i,j,k) * e(i,j,k) / (9.8 * c(i,j,k))
         enddo
      enddo
   enddo
   return

entry RAMS_comp_vegclass(n1,n2,n3,a)
   do i=1,n1
      a(i,1,1) = a(i,1,1) + .1
   enddo
   return

entry RAMS_comp_horizdiv(n1,n2,n3,a)
   do k=2,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k)=-(a(i,j,k)-a(i,j,k-1))*dztn(k,ngrd)
         enddo
      enddo
   enddo
return

entry RAMS_comp_vertint(n1,n2,n3,a,topt,ngrd)
   ztop = zmn(nnzp(1)-1,1)
   do j = 1,n2
      do i = 1,n1
         rtgt = 1. - topt(i,j) / ztop
         a(i,j,1) = 0.
         do k = 2,n3-1
            a(i,j,1) = a(i,j,1) + a(i,j,k) * (zmn(k,ngrd)-zmn(k-1,ngrd)) * rtgt
         enddo
      enddo
   enddo
return

entry RAMS_comp_dn0(n1,n2,n3,a,b,c,topt,ngrd)
   ztop = zmn(nnzp(1)-1,1)
   do j=1,n2
      do i=1,n1
         do k=1,n3
             vctr2(k)=ztn(k,ngrd)*(1.-topt(i,j)/ztop)+topt(i,j)
         enddo
         call htint(n3,pi01dn(1,ngrd),ztn(1,ngrd),n3,vctr11,vctr2)
         call htint(n3,th01dn(1,ngrd),ztn(1,ngrd),n3,vctr12,vctr2)

         do k=1,n3
            b(i,j,k)=vctr12(k)
         enddo
         a(i,j,n3) = vctr11(n3)

         c1=g*2.*(1.-topt(i,j)/ztop)
         c2=(1-cpor)
         c3=cp**c2
         do k=n3-1,1,-1
            a(i,j,k)=a(i,j,k+1)  &
                +c1/((b(i,j,k)+b(i,j,k+1))*mydzmn(k,ngrd))
         enddo

         do k=1,n3
            c(i,j,k)=(c3*p00)/(rgas*b(i,j,k)*a(i,j,k)**c2)
         enddo

      enddo
   enddo
return

entry RAMS_comp_ppress(n1,n2,n3,a,c)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k) = 1000. * (a(i,j,k)/cp) ** cpor  &
                     - 1000. * (c(i,j,k)/cp) ** cpor
         enddo
      enddo
   enddo
return

entry RAMS_comp_raintemp(n1,n2,n3,a)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k) = (a(i,j,k) - 334000.) / 4186.
         enddo
      enddo
   enddo
return

entry RAMS_comp_qwtc(n1,n2,n3,a,b,c)

! Unlike subroutine qwtk in rsurf.f which is receives qw in J/m^3, this
! subroutine inputs 'a' in units of J/cm^3.  Thus, we first convert back
! to J/m^3.  Also, 'b' is input as wgp, and is thus multiplied first by
! 1.e3 to convert to kg/m^3.

   do k=1,n3
      do j=1,n2
         do i=1,n1

            qwliq0 = b(i,j,k) * 3.34e8
            nsoil = nint(c(i,j,k))
            dryhcap = slcpd(nsoil)

            a(i,j,k) = a(i,j,k) * 1.e6
            b(i,j,k) = b(i,j,k) * 1.e3

            if (a(i,j,k) .le. 0.) then
               a(i,j,k) = a(i,j,k)  &
                  / (2093. * b(i,j,k) + dryhcap)
               b(i,j,k) = 0.
            elseif (a(i,j,k) .ge. qwliq0) then
               a(i,j,k) = (a(i,j,k) - qwliq0)  &
                  / (4186. * b(i,j,k) + dryhcap)
               b(i,j,k) = 1.
            else
               b(i,j,k) = a(i,j,k) / qwliq0
               a(i,j,k) = 0.
            endif

         enddo
      enddo
   enddo
return

entry RAMS_comp_copysst(n1,n2,n3,a)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            a(i,j,k) = a(i,j,n3) - 273.15
         enddo
      enddo
   enddo
return

entry RAMS_comp_qtcpcp(n1,n2,n3,a)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            if (a(i,j,k) .le. 0.) then
               a(i,j,k) = a(i,j,k) / 2093.
            elseif (a(i,j,k) .le. 80.) then
               a(i,j,k) = 0.
            else
               a(i,j,k) = (a(i,j,k) - 334000.) / 4186.
            endif
         enddo
      enddo
   enddo
return

entry RAMS_comp_fracliq(n1,n2,n3,a)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            if (a(i,j,k) .le. 0.) then
               a(i,j,k) = 0.
            elseif (a(i,j,k) .ge. 334000.) then
               a(i,j,k) = 1.
            else
               a(i,j,k) = a(i,j,k) / 334000.
            endif
         enddo
      enddo
   enddo
return

entry RAMS_comp_fracice(n1,n2,n3,a)
   do k=1,n3
      do j=1,n2
         do i=1,n1
            if (a(i,j,k) .le. 0.) then
               a(i,j,k) = 1.
            elseif (a(i,j,k) .ge. 334000.) then
               a(i,j,k) = 0.
            else
               a(i,j,k) = 1. - a(i,j,k) / 334000.
            endif
         enddo
      enddo
   enddo
return

entry RAMS_comp_hydrodiam(n1,n2,n3,a,c,ccfmas,ppwmas)
   rpwmas = 1. / ppwmas
   do k=1,n3
      do j=1,n2
         do i=1,n1
            if(a(i,j,k) .gt. 1.e-10 .and. c(i,j,k).gt.1.e-10)then
               a(i,j,k) = (a(i,j,k) / (c(i,j,k) * ccfmas))**rpwmas
            else
               a(i,j,k) = 0.
            endif
         enddo
      enddo
   enddo
return

entry RAMS_comp_slmstf(n1,n2,n3,a,c)
   do i=1,n1
      a(i,1,1) = a(i,1,1) / max(1.e-6,slmsts0(nint(c(i,1,1))))
   enddo
return

entry rams_sum_snowlayers(n1,n2,n3,a)
   do ip=1,n3
      do k=2,n2
         do ij=1,n1
            a(ij,1,ip) = a(ij,1,ip) + a(ij,k,ip)
         enddo
      enddo
   enddo
return

entry rams_fill_sst(n1,n2,n3,kp,a,c)
   do j=1,n2
      do i = 1,n1
         a(i,j,1) = c(i,j,kp)
      enddo
   enddo
return

entry rams_comp_pcpgnorm(n1,n2,n3,a,c)
return

entry rams_comp_vapnorm(n1,n2,n3,a,c)
return

entry rams_comp_snownorm(n1,n2,n3,a,c)
return

entry rams_comp_vegnorm(n1,n2,n3,a,c)
return

entry rams_comp_cannorm(n1,n2,n3,a,c)
return

entry RAMS_comp_pbl(n1,n2,n3,a,c,ngrd)
   tkethrsh=0.001   !tke threshold for PBL height in m2/s2
   do j=1,n2
      do i=1,n1
         pblht=0.
         do k=2,n3
            pblht=ztn(k,ngrd)*(1.-c(i,j,1)/zmn(nnzp(1)-1,1))
!                  if(i.ge.10.and.i.le.25.and.j.ge.13.and.j.le.25)
!     &               print*,'i,j,k,z,pbl=',i,j,k,ztn(k,ngrd),pblht
            if(a(i,j,k).le.tkethrsh)goto 10
         enddo
 10      continue
         do k=1,n3
           a(i,j,k)=pblht
         enddo
      enddo
   enddo
!         call cpezct(a(i,j,2),n1,n2)
return

entry RAMS_comp_etrans(n1,n2,n3,a,b,a2d)
   do j=1,n2
      do i=1,n1
         temp1=a(i,j,1)*b(i,j,1)/cp
         press1=(b(i,j,1)/cp)**cpor*p00
         dens=press1/(rgas*temp1)
         if(i.eq.5.and.j.eq.5) then
            print*,'============++++++'
            print*,temp1,press1,dens,a2d(i,j)
         endif
         a(i,j,1)=a2d(i,j)*dens*1.e-3*39.37*3600.
         do k=2,n3
            a(i,j,k)=a(i,j,1)
         enddo
      enddo
   enddo
return

entry RAMS_comp_slpress(n1,n2,n3,theta,pp,z,slp)
!
!     This subroutine calculates the pressure at level zlev. it
!       is hardwired here to calculate mean sea level pressure,
!       but can be easily changed to calculate pressure at any level
!       by just changing zlev.
!     a standard atmosphere lapse rate of 6.5 c/km is used to
!       interpolate temperature down from level 2 in the model.
!

   sl_p00=1000.
   sl_g=9.8
   sl_cp=1004.
   sl_r=287.
   sl_cpor=sl_cp/sl_r
!c      rlap=-.0065  ! standard temp lapse rate
   rlap=.0025     ! approx standard theta lapse rate
   zlev=0.

do j=1,n2
   do i=1,n1
      do k=2,n3
         if(z(i,j,k).ge.zlev) then
            ktop=k
            kbot=k-1
            go to 31
         endif
      enddo
31    continue

      if(i.eq.1.and.j.eq.1)  &
         print*,'kbot:',kbot,ktop,z(i,j,kbot),z(i,j,ktop)  &
            ,pp(i,j,kbot),theta(i,j,kbot)
         ddz=zlev-z(i,j,kbot)
         if(zlev.lt.z(i,j,kbot))then
            thbar=(theta(i,j,kbot)-.5*ddz*rlap)
         else
            thbar=.5*(theta(i,j,kbot)+theta(i,j,ktop))
         endif
         slp(i,j)=pp(i,j,kbot)-ddz*sl_g/thbar
         slp(i,j)=(slp(i,j)/sl_cp)**sl_cpor*sl_p00
      enddo
   enddo
return

entry RAMS_comp_ctprof(n1,n2,n3,a,b,ngrd)
   do i=1,n1
      do j=1,n2
         kmax=0
         do k=1,n3
            if(a(i,j,k).ge.0.0001.and.b(i,j,k).ge.0.99)kmax=k
         enddo
         if(kmax.gt.2)then
            a(i,j,1)=ztn(kmax,ngrd)
         else
            a(i,j,1)=0.0
         endif
      enddo
   enddo
return

end


subroutine RAMS_comp_multap(n1,n2,n3,n4,a,b)
dimension a(n1,n2,n4),b(n1,n2,n3)
   do k=1,n4
      do j=1,n2
         do i=1,n1
            a(i,j,k)=a(i,j,k)*b(i,j,1)
         enddo
      enddo
   enddo
return
end



subroutine RAMS_comp_patchsum(n1,n2,n3,n4,a,f,psum)
real a(n1,n2,n3,n4),f(n1,n2,n4),psum(n1,n2,n3)

! This routine is for quantities such as net roughness that are defined
! for all patches

do k = 1,n3
   do j = 1,n2
      do i = 1,n1
         psum(i,j,k) = 0.
         do ip = 1,n4
            psum(i,j,k) = psum(i,j,k) + f(i,j,ip) * a(i,j,k,ip)
         enddo
      enddo
   enddo
enddo

! Copy psum into f, which was passed in as a(1).  n3 may exceed n4 but this
! should be ok.

do k = 1,n3
   do j = 1,n2
      do i = 1,n1
         f(i,j,k) = psum(i,j,k)
      enddo
   enddo
enddo

return
end



subroutine RAMS_comp_patchsum_l(n1,n2,n3,n4,a,f,psum)
real a(n1,n2,n3,n4),f(n1,n2,n4),psum(n1,n2,n3)

! This routine is for quantities such as veg roughness that are not
! defined for water patches

do k = 1,n3
   do j = 1,n2
      do i = 1,n1
         if (f(i,j,1) .lt. .991) then
            psum(i,j,k) = 0.
            do ip = 2,n4
               psum(i,j,k) = psum(i,j,k) + f(i,j,ip) * a(i,j,k,ip)  &
                           / (1. - f(i,j,1))
            enddo
         else
            psum(i,j,k) = a(i,j,k,2)
         endif
      enddo
   enddo
enddo

! Copy psum into f, which was passed in as a(1).  n3 may exceed n4 but this
! should be ok.

do k = 1,n3
   do j = 1,n2
      do i = 1,n1
         f(i,j,k) = psum(i,j,k)
      enddo
   enddo
enddo

return
end

subroutine RAMS_comp_bigpatch(n1,n2,n3,n4,a,f,b)
real a(n1,n2,n3,n4),f(n1,n2,n4),b(n1,n2,n3)

! Extract LSP value from largest patch

do k = 1,n3
   do j = 1,n2
      do i = 1,n1
         if (f(i,j,2) .ge. f(i,j,1)) then
            b(i,j,k) = a(i,j,k,2)
         else
            b(i,j,k) = a(i,j,k,1)
         endif
      enddo
   enddo
enddo

! Copy b into f, which was passed in as a(1).  n3 may exceed n4 but this
! should be ok.

do k = 1,n3
   do j = 1,n2
      do i = 1,n1
         f(i,j,k) = b(i,j,k)
      enddo
   enddo
enddo

return
end


subroutine RAMS_comp_5050(n1,n2,n3,a,d)
real a(n1,n2),d(n1,n2,n3)

do j = 1,n2
   do i = 1,n1
      a(i,j) = .5 * (a(i,j) + d(i,j,2))
   enddo
enddo

return
end

!-----------------------------------------------------------------------

subroutine RAMS_reduced_temp(n1,n2,n3,n4,tempnew,speed,ustar  &
        ,tstar,znew,zold,zrough,patfrac,cantemp,theta,topo,ztop)
implicit none
integer :: n1,n2,n3,n4,i,j,np
real :: tempnew(n1,n2),speed(n1,n2,n3),ustar(n1,n2,n4),znew,zold  &
         ,zrough(n1,n2,n4),patfrac(n1,n2,n4),cantemp(n1,n2,n4)  &
         ,theta(n1,n2,n3),topo(n1,n2),ztop,tstar(n1,n2,n4)
include 'rconstants.h'

real:: richno,rtgt,zagl,rtemp,rtempw,z0,a2,spd



do j=1,n2
   do i=1,n1
      
      rtgt=1.-topo(i,j)/ztop
      zagl=zold*rtgt
      
      rtempw=0.
      
      do np=1,n4
      
         z0=zrough(i,j,np)
         if(np==1) z0=.001
         spd=max(speed(i,j,2),.25)

         richno=g*zagl*(theta(i,j,2)-cantemp(i,j,np))  &
                     /(theta(i,j,2)*spd**2)
         a2 = (vonk / log(znew / z0)) ** 2

         if(richno.gt.0.) then
            rtemp=cantemp(i,j,np)  &
             +(ustar(i,j,np)*tstar(i,j,np))/(a2*spd)  &
                    *(1.+15.*richno/sqrt(1+5*richno))  
            rtemp=min(max(rtemp, cantemp(i,j,np)),theta(i,j,2))
         else
            rtemp=cantemp(i,j,np)  &
             +((ustar(i,j,np)*tstar(i,j,np))/(a2*spd))  &
               / (1.- 15.*richno/(1.+75.*a2   &
                             * sqrt(-znew*richno/z0)))
            rtemp=max(min(rtemp, cantemp(i,j,np)),theta(i,j,2))
         endif
         
         if((i==50.and.j==25)) then
            print*,'====tempf2m:',i,j
            print*,np,patfrac(i,j,np),cantemp(i,j,np)
            print*,np,ustar(i,j,np),zrough(i,j,np),tstar(i,j,np)
            print*,np,theta(i,j,2),speed(i,j,2),rtemp
         endif
         
         rtempw=rtempw+rtemp*patfrac(i,j,np)
      
      enddo
      
      tempnew(i,j)=rtempw
      

   enddo
enddo

return
end

!-----------------------------------------------------------------------

subroutine RAMS_reduced_wind(n1,n2,n3,n4,velnew,speed,ustar &
         ,znew,zold,zrough,patfrac,cantemp,theta,pi,topo,ztop)
implicit none
integer :: n1,n2,n3,n4,i,j,np
real :: velnew(n1,n2),speed(n1,n2,n3),ustar(n1,n2,n4),znew,zold  &
          ,zrough(n1,n2,n4),patfrac(n1,n2,n4),cantemp(n1,n2,n4)  &
          ,theta(n1,n2,n3),pi(n1,n2,n3),topo(n1,n2),ztop
include 'rconstants.h'

real:: richno,rtgt,zagl,rwind,rwindw,z0,a2,spd,cantheta,sfcpi



do j=1,n2
   do i=1,n1
      
      rtgt=1.-topo(i,j)/ztop
      zagl=zold*rtgt
      sfcpi=.5*(pi(i,j,1)+pi(i,j,2))
      
      rwindw=0.
      
      do np=1,n4
      
         z0=zrough(i,j,np)
         if(np==1) z0=.001
         spd=max(speed(i,j,2),.25)
         cantheta=cantemp(i,j,np)*cp/sfcpi

         richno=g*zagl*(theta(i,j,2)-cantheta)  &
                      /(theta(i,j,2)*spd**2)
         a2 = (vonk / log(znew / z0)) ** 2

         if(richno.gt.0.) then
            rwind=sqrt(ustar(i,j,np)**2/a2   &
                     *(1.+10.*richno/sqrt(1+5*richno)) )
         else
            rwind=sqrt( ustar(i,j,np)**2/a2  &
                / (1.- 10.*richno/(1.+75.*a2  &
                              * sqrt(-znew*richno/z0))))
         endif
         
         rwind=max(min(rwind,speed(i,j,2)),0.)
         
         if(i==50.and.j==25) then
            print*,'====speed10m'
            print*,np,patfrac(i,j,np),cantemp(i,j,np)
            print*,np,ustar(i,j,np),zrough(i,j,np)
            print*,np,theta(i,j,2),speed(i,j,2),rwind
         endif
         
         rwindw=rwindw+rwind*patfrac(i,j,np)
      
      enddo
      
      velnew(i,j)=rwindw
      

   enddo
enddo

return
end




      
!------------------subroutine to calculate cloud fraction
subroutine cldfraction(n1,n2,n3,frac,pi,rh)
implicit none
include 'rconstants.h'

integer :: i,j,k,kmax,n1,n2,n3
real :: frac(n1,n2),pi(n1,n2,n3),rh(n1,n2,n3)
real, allocatable::rhc(:), cs(:)
real :: kappai,c_1,c_2,c_junk,pop2,csmax

c_1     = 2.
c_junk  = 3.
c_2     = c_junk**0.5
kappai = (1./.286)


allocate (rhc(n3),cs(n3) )
      print*,'+++++++:',n1,n2,n3

do j=1,n2
   do i=1,n1
      frac(i,j) = 0.
      csmax = 0.
      kmax  = 0
      do k = 1, n3
         rhc(k)= 0.
         cs(k)= 0.
      enddo

      do k = 1, n3
         pop2 = (pi(i,j,k)/pi(i,j,2))**kappai

         rhc(k) = 100. - (100.*c_1*pop2)*  &
               (1.-pop2)*(1.+c_2*(pop2-0.5))

         if(rh(i,j,k) .ge. rhc(k))then
            if(rhc(k).eq.100.)rhc(k)=rhc(k)+0.0000001
            cs(k) = ( (rh(i,j,k)-rhc(k))/(100.-rhc(k)) ) **2. 
         else
            cs(k) = 0.
         endif
         if(cs(k).gt.csmax)then
            csmax=cs(k)
            kmax = k
         endif
         frac(i,j) = frac(i,j) + cs(k)*(1./float(k))
      if(i==20.and.j==20) print*,'+++++++:',k,pi(i,j,k),rh(i,j,k),frac(i,j)
      enddo
      
      csmax=max(csmax,0.)

!      frac(i,j) = 1.-min(1.,max(0.,csmax))
      frac(i,j) = 1.-min(1.,max(0.,frac(i,j)))  ! actually returns 
                                                ! clear sky fraction

   enddo
enddo

deallocate (rhc,cs)

return
end



