!----------------------------------------------------------------
! University of Houston
! Air Quality Modeling and Monitoring Center
! 218 Old Science Building
! Houston, TX 77204-5048
! USA
!
! email: sbkim@math.uh.edu (Dr. Seung-Bum Kim)
!        dwbyun@math.uh.edu (Dr. Daewon W. Byun)
!----------------------------------------------------------------
!
!-----RAMS fields
real, dimension(mnx) :: xrm, xrt
real, dimension(mny) :: yrm, yrt  
real, dimension(mnx,mny) :: xrutm, yrutm

!-----ARL and intermediate ARRAY
! 3D
real, dimension(mnz,mnx,mny) ::        &
     u,v,w,t,p,rv,dn0,tke,pi0,         &
     pp,theta,                         &              
!JCL(3/14/03) new vars
!DMM(1/21/05) added sigwb and tlgrb
     cfu1,cfu2,cfd1,dfu1,dfu2,efu1,efu2,dfd1,efd1,tlgr,tlgrb,sigw,sigwb  


! 2D
real, dimension(mnx,mny) ::            &
     umof,vmof,                        &
     dlwf,dswf,ustr,tstr,qstr,         &
     shgt,glat,glon,tpp1,              &
     rh2m,prss,t02m,                   &
     uwnd,vwnd,sphu,temp,pres,tken,wwnd, & 
     solw, &                                ! JCL(3/14/03)  new variables
!CHG(9/17/03) new vars
     cfu1n,cfu2n,cfd1n,dfu1n,dfu2n,efu1n,efu2n,dfd1n,efd1n,tlgrn,sigwn, &
!MLO(3/1/09) new var
    pblhgt, &
!MLO(3/1/09) ED-2 variables
     gpp,plresp,hetresp

! COMMON BLOCKS
common /ramsgrid/ xrm,yrm,xrt,yrt,xrutm,yrutm
common /arlfld/ u,v,w,t,p,rv,dn0,tke,pi0,pp,       &
                theta,umof,vmof,                   &
		dlwf,dswf,ustr,tstr,qstr,shgt,     &
		glat,glon,tpp1,rh2m,prss,t02m,     &
                uwnd,vwnd,sphu,temp,pres,tken,wwnd, &
! JCL(3/14/03)  new variables 
! DMM(1/21/05)  added sigwb and tlgrb
                cfu1,cfu2,cfd1,dfu1,dfu2,efu1,efu2,dfd1,efd1, &   
                tlgr,tlgrb,sigw,sigwb,  &
!CHG(9/17/03) new vars
                cfu1n,cfu2n,cfd1n,dfu1n,dfu2n,efu1n,efu2n,  &
                dfd1n,efd1n,tlgrn,sigwn, &
!MLO(3/1/09) ED-2 variables
                gpp,plresp,hetresp
