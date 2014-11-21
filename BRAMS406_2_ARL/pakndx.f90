!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  PAKNDX           PAcK iNDeX writes index record
!   PRGMMR:    ROLAND DRAXLER   ORG: R/ARL           DATE:96-06-01
!
! ABSTRACT:  THIS CODE WRITTEN AT THE AIR RESOURCES LABORATORY ...
!   PACK INDEX - AFTER ALL THE RECORDS FOR A PARTICULAR TIME
!   PERIOD HAVE BEEN WRITTEN TO A FILE, THIS ROUTINE WRITES THE
!   INDEX RECORD FOR THAT TIME GROUP.  THE INDEX RECORD IS ALWAYS
!   THE FIRST RECORD OF THE TIME GROUP.  IT INCLUDES GRID DEFINITION
!   VARIABLES, AND CHECKSUM INFORMATION.
!
! PROGRAM HISTORY LOG:
!   Last Revised: 14 Feb 1997 (RRD) 
!                 02 Feb 2001 (RRD) - fortran90 upgrade
!
! USAGE:  CALL PAKNDX(LUNIT)
!
!   INPUT ARGUMENT LIST:   see below
!   OUTPUT ARGUMENT LIST:  see below
!   INPUT FILES:           none
!   OUTPUT FILES:          none
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM RS6000
!
!$$$

SUBROUTINE PAKNDX(LUNIT)

  IMPLICIT NONE

!-------------------------------------------------------------------------------
 
  INCLUDE 'defpack.inc'

  INTEGER, INTENT(IN) :: lunit
  INTEGER             :: nvar,nlvl,nrec,jrec,nhl1,nhl2 
  INTEGER             :: i,j,k,l,kk,ng,kg,kol 
  REAL                :: zl 
  CHARACTER(50)       :: label   ! standard record label
  CHARACTER(MLEN)     :: header  ! extender header

! pass structure between routines
  COMMON / PAKCOM / GV, NG

!-------------------------------------------------------------------------------

!==>determine which grid

  KG=0
  DO KK=1,NG
     IF(LUNIT.EQ.GV(KK)%KUNIT)KG=KK
  END DO
  IF(KG.EQ.0)THEN
     WRITE(*,*)'ERROR pakndx: Requesting uninitialized unit (call pakset)'
     STOP
  END IF

!==>conventional 50 byte label

  WRITE(LABEL,'(5I2,2I2,A4,I4,2E14.7)')                                        &
     GV(KG)%IY0,GV(KG)%IM0,GV(KG)%ID0,GV(KG)%IH0,GV(KG)%IC0,                   &
     0,GV(KG)%IG,'INDX',0,0.0,0.0

!==>first part of header: 1 -> 110 

  WRITE(HEADER(1:110),'(A4,I3,I2,12F7.2,3I3,I2,I6)')                           &
     GV(KG)%MODEL,GV(KG)%ICX,GV(KG)%MN0,GV(KG)%GRIDS,                          &
     GV(KG)%NXG,GV(KG)%NYG,GV(KG)%NLVL,GV(KG)%KSYS,GV(KG)%LENH
  
  !==>loop through remainder of the extended header

  KOL=111
  NLVL=GV(KG)%NLVL

  DO L=1,NLVL
     ZL=GV(KG)%HEIGHT(L)

!    precision depends upon the height coordinate
     IF(ZL.GE.10000.0)THEN
        WRITE(HEADER(KOL:KOL+7),'(F6.0,I2)')ZL,GV(KG)%NVAR(L)
     ELSEIF(ZL.GE.1000.0)THEN
        WRITE(HEADER(KOL:KOL+7),'(F6.1,I2)')ZL,GV(KG)%NVAR(L)
     ELSEIF(ZL.GE.100.0.AND.ZL.LT.1000.0)THEN
        WRITE(HEADER(KOL:KOL+7),'(F6.2,I2)')ZL,GV(KG)%NVAR(L)
     ELSEIF(ZL.GE.10.0.AND.ZL.LT.100.0)THEN
        WRITE(HEADER(KOL:KOL+7),'(F6.3,I2)')ZL,GV(KG)%NVAR(L)
     ELSEIF(ZL.GE.1.0.AND.ZL.LT.10.0)THEN
        WRITE(HEADER(KOL:KOL+7),'(F6.4,I2)')ZL,GV(KG)%NVAR(L)
     ELSE
        WRITE(HEADER(KOL:KOL+7),'(F6.5,I2)')ZL,GV(KG)%NVAR(L)
     END IF

!    add variable id's and checksums
     KOL=KOL+8
     NVAR=GV(KG)%NVAR(L)
     DO K=1,NVAR
!CHG (10/01/03) change from 8 to 16 bit
!        WRITE(HEADER(KOL:KOL+7),'(A4,I3)') GV(KG)%VARB(K,L), GV(KG)%CHKS(K,L)

!        WRITE(*,*)'pakndx:',L,NLVL

        WRITE(HEADER(KOL:KOL+9),'(A4,I5)') GV(KG)%VARB(K,L),                    &
              GV(KG)%CHKS(K,L)
!        WRITE(*,*)'pakndx:',HEADER(KOL:KOL+9),KOL
!        KOL=KOL+8
        KOL=KOL+10
     END DO
  END DO

!==>write extended header to disk

  NHL1=1
! number of index records
  NREC=GV(KG)%NHREC
! point to first index record
  JREC=GV(KG)%MREC

! test for previous setup
  IF(JREC.LT.1)THEN
     WRITE(*,*)'ERROR pakndx: no prior calls to pakrec'
     STOP
  END IF

  DO K=1,NREC
!    byte count for each index
     NHL2=NHL1+GV(KG)%LREC-1
     IF(K.EQ.NREC)NHL2=NHL1+GV(KG)%NHBYT-1

     WRITE(GV(KG)%KUNIT,REC=JREC)LABEL,HEADER(NHL1:NHL2)
     JREC=JREC+1
     NHL1=NHL2+1
  END DO

!==>clear flags

! checksum table
  DO J=1,MLVL
  DO I=1,MVAR
     GV(KG)%CHKS(I,J)=0
  END DO
  END DO

! new time flag
  GV(KG)%NEWT=.TRUE.

END SUBROUTINE pakndx
