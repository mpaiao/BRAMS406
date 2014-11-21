!     ******************************************************************

function td(p,rs)

implicit none
real rr,rs,es,esln,p,td

rr=rs+1e-8
es=p*rr/(.622+rr)
esln=log(es)
td=(35.86*esln-4947.2325)/(esln-23.6837)

return
end

!     ******************************************************************

FUNCTION RS(P,T)

ES=610.78*EXP(17.269*(T-273.16)/(T-35.86))
RS=.622*ES/(P-ES)

RETURN
END

!     ******************************************************************
!***************************************************************************

subroutine date_add_to (inyear,inmonth,indate,inhour  &
                        ,tinc,tunits,outyear,outmonth,outdate,outhour)

! add a time increment to a date and output new date
! -> uses hhmmss for hours, 4 digit year

integer inyear,inmonth,indate,inhour  &
       ,outyear,outmonth,outdate,outhour
real tinc
character*1 tunits
dimension mondays(12)
data mondays/31,28,31,30,31,30,31,31,30,31,30,31/

! convert input time to seconds

ttinc=tinc
if(tunits.eq.'m') ttinc=tinc*60.
if(tunits.eq.'h') ttinc=tinc*3600.
if(tunits.eq.'d') ttinc=tinc*86400.
!print*,'inc:',tinc,tunits,ttinc

xhourin=inhour/10000
xminin=mod(inhour,10000)/100
xsecin=mod(inhour,100)
strtim=xhourin+xminin/60.+xsecin/3600.
!print*,'strtim=',strtim

izhours=int(mod(strtim+ttinc/3600.,24.)+.001)
izmin  =int(mod(strtim+ttinc/3600.,1.)*60+.001)
izsec  =int(mod(strtim*3600.+ttinc,60.)+.001)
!print*,'izs=',izhours,izmin,izsec

outhour= izhours*10000+izmin*100+izsec

iround=.001
if(ttinc<0.) iround=-.001
iadddays=int((strtim+ttinc/3600.)/24.+iround)

outyear=inyear
outdate=indate+iadddays
outmonth=inmonth

20 continue
   idays=mondays(outmonth)
   if(outmonth==2.and.mod(outyear,4)==0)  idays=29

   if(outdate.gt.idays) then
      outdate=outdate-idays
      outmonth=outmonth+1
      if(outmonth.gt.12) then
         outyear=outyear+1
         outmonth=1
      endif
   elseif(outdate.lt.1) then
      if(outmonth.eq.1)outmonth=13
      idays=mondays(outmonth-1)
      if(outmonth-1.eq.2.and.mod(outyear,4).eq.0)  idays=29
      outdate=idays+outdate
      outmonth=outmonth-1
      if(outmonth.eq.12)outyear=outyear-1
   else
      goto 21
   endif

   goto 20

21 continue

!print*,'out stuff:',outyear,outmonth,outdate,outhour

return
end



integer function lastchar(str)
character*(*) str

! returns last non-blank character position from a string

ln=len(str)
do n=ln,1,-1
   if(str(n:n).ne.' ') then
      lastchar=n
      return
   endif
enddo
lastchar=0

return
end

!***************************************************************************

integer function lastslash(str)
character*(*) str

! returns last slash character position from a string

ln=len(str)
do n=ln,1,-1
   if(str(n:n).eq.'/') then
      lastslash=n
      return
   endif
enddo
lastslash=0

return
end
!***************************************************************************

subroutine parse(str,tokens,ntok)
character*(*) str,tokens(*)
character sep*1
data ntokmax/100/

! this routine "parses" character string str into different pieces
! or tokens by looking for  possible token separators (toks
! str contains nch characters.  the number of tokens identified is nto
! the character string tokens are stored in tokens.

sep=' '
ntok=0
npt=1
nch=lastchar(str)
nc=1
do ntok=1,ntokmax
   do n=nc,nch
      if(str(n:n).ne.sep) then
         ntbeg=n
         goto 21
      endif
   enddo
   21 continue
   do n=ntbeg,nch
      if(str(n:n).eq.sep) then
         ntend=n-1
         goto 22
      endif
      if(n.eq.nch) then
         ntend=n
         goto 22
      endif
   enddo
   22 continue
   tokens(ntok)=str(ntbeg:ntend)
   nc=ntend+1
   if(nc.ge.nch) goto 25
enddo

25 continue

!do nc=1,nch
!   if(str(nc:nc).eq.sep.or.nc.eq.nch)then
!      if(nc-npt.ge.1)then
!         ntok=ntok+1
!         tokens(ntok)=str(npt:nc-1)
!         if(nc.eq.nch.and.str(nc:nc).ne.sep)then
!            tokens(ntok)=str(npt:nc)
!            go to 10
!         endif
!      endif
!      ntok=ntok+1
!      tokens(ntok)=str(nc:nc)
!      npt=nc+1
!      go to 10
!   endif
!   10 continue
!enddo

return
end

!----------------------------------------------------------------------------

      SUBROUTINE TIMING(ICALL,T1)

!     Routine returns CPU time.  Called with ICALL=1 at beginning
!     of timestep, ICALL=2 at end of timestep.

      dimension et(2)
      
#if defined(STARDENT)
      EXTERNAL CPUTIM
      REAL*4 CPUTIM
#endif

      IF(ICALL.EQ.1) THEN
      
#if defined(VAX)
        IAD0=0
        CALL LIB$INIT_TIMER(IAD0)
#endif
#if defined(CRAY)
        T1=SECOND(AAA)
#endif
#if defined(STARDENT)
        T1=CPUTIM(0.)
#endif
#if defined(IBM)
        T1=MCLOCK(AAA)/100.
#endif
#if defined(SUN) || defined(SGI) || defined(PC_NT1)
        T1=ETIME(et)
#endif
#if defined(HP) || defined(PC_LINUX1) || defined(ALPHA) 
        T1=ETIME(et)
#endif

      ELSEIF(ICALL.EQ.2) THEN

#if defined(VAX)
        CALL LIB$SHOW_TIMER(IAD0,2)
#endif
#if defined(CRAY)
        T1=SECOND(AAA)
#endif
#if defined(STARDENT)
        T1=CPUTIM(0.)
#endif
#if defined(IBM)
        T1=MCLOCK(AAA)/100.
#endif
#if defined(SUN) || defined(SGI) || defined(PC_NT1)
        T1=ETIME(et)
#endif
#if defined(HP) || defined(PC_LINUX1)
        T1=ETIME(et)
#endif
#if defined(ALPHA) 
        T1=ETIME(et)
#endif
      ENDIF
      
      RETURN
      END

subroutine error_mess(msg)
character*(*) msg

print*,msg

return
end

!
!     ******************************************************************
!
SUBROUTINE AE0(NPTS,A,B)
DIMENSION A(NPTS)
DO I=1,NPTS
  A(I)=B
ENDDO
RETURN
END

!
!     ******************************************************************
!
SUBROUTINE WINDUV(DD,FF,UU,VV)
DATA PI/3.14159/,PI180/.01745329/,V180PI/57.2957795/
UU=-FF*SIN(DD*PI180)
VV=-FF*COS(DD*PI180)
RETURN
ENTRY WINDDF(DD,FF,UU,VV)
U=UU
V=VV
FF=SQRT(U*U+V*V)
IF(ABS(U).LT.1.E-20)U=1.E-20
IF(ABS(V).LT.1.E-20)V=1.E-20
DD=ATAN2(-U,-V)*V180PI
IF(DD.LT.0.)DD=DD+360.
RETURN
END


!     ******************************************************************

SUBROUTINE HTINT(NZZ1,VCTRA,ELEVA,NZZ2,VCTRB,ELEVB)
DIMENSION VCTRA(*),VCTRB(*),ELEVA(*),ELEVB(*)

L=1
DO 20 K=1,NZZ2
30 CONTINUE
IF(ELEVB(K).LT.ELEVA(1))GO TO 35
IF(ELEVB(K).GE.ELEVA(L).AND.ELEVB(K).LE.ELEVA(L+1))GO TO 35
IF(ELEVB(K).GT.ELEVA(NZZ1))GO TO 36
L=L+1
IF(L.EQ.NZZ1) then
  print *,'htint:nzz1',nzz1
  do kk=1,L
    print*,'kk,eleva(kk),elevb(kk)',eleva(kk),elevb(kk)
  enddo
  stop 'htint'
endif
GO TO 30
35 CONTINUE
WT=(ELEVB(K)-ELEVA(L))/(ELEVA(L+1)-ELEVA(L))
VCTRB(K)=VCTRA(L)+(VCTRA(L+1)-VCTRA(L))*WT
GO TO 20
36 CONTINUE
WT=(ELEVB(K)-ELEVA(NZZ1))/(ELEVA(NZZ1-1)-ELEVA(NZZ1))
VCTRB(K)=VCTRA(NZZ1)+(VCTRA(NZZ1-1)-VCTRA(NZZ1))*WT
20 CONTINUE

RETURN
END
