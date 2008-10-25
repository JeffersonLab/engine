*------------------------------------------------------------------------
*
*       HMS_TRACK  HMS Tracking Routines 
*      -=========-
* 
*	Forward and Backward Tracking of electrons in the Jlab HMS hall 
*       C spectrometer
*
*       Note: - the HMS routines use a lab (HMS) coord. system
*               and the corresponding COSY coord. system, both 
*               right handed with
*                 x : pointing downwards
*                 y : perpendicular to x,z, 
*                     pointing to the left (if seen in z-direction)
*                 z : HMS axis, pointing from the target to the focal plane
*
*             - all lengths (x,y,z,l,...) are measured in [m]
*             - all angles are measured as dx/dz,dy/dz (lab coords.)
*               or as A,B (COSY coords.)       
*             - the momentum is measured in delta (relative momentum
*               deviation  = 1-p0/pHMS)
*
*       PART 1: Forward tracking using COSY transport matrices  
*              
*
*       PART 2: Reconstruction (backward tracking) using reconstruction 
*               and COSY transport matrices (including the 
*               effects of a vertical beam offset (out-of plane))  
*                    
*
*       written by Markus Muehlbauer for the GEN Experiment
*
* frw 9/2000
*       changes made in the course of the migration to g77 compiler:
*        - fixed some typos (old ones, too)
*        - all COSY conversion code was already commented out (why?
*          by who?) so I removed it and made the code more readable
*        - various variables were not initialized prior to reading
*          from file.  This may or may not be an issue, but fixed it
*          anyway
*
*------------------------------------------------------------------------

*------------------------------------------------------------------------
*
*       PART 1:  HMS Forward Tracking (Target to Focal Plane)
*      -=======-       
*
*	Forward tracking in the Jlab HMS hall C spectrometer
*       using COSY transport matrices
*   
*       developed by Cris Cothran
*       modified  by Markus Muehlbauer
*         - CCs orignal program converted into subroutines
*         - mad additions for pure tracking, without checking the acceptance
*         - and changed the innermost loops applying the matrix
*           (which speeds up the whole thing by a factor of about 30)
* 
*       Supplies:
*         hmsInitForward (map) 
*           load the forward transport maps
*         hmsForward (uT,zT,u,z)
*            make a single step transport calculation 
*            (without treating the acceptance)
*         hmsAccept (uT,zT,u,z)
*            make a multi step transport calculation 
*            (also treating the acceptance)  
*       
*       Note: - Before calling hmsForward or hmsAccept the forward 
*               transport maps have to be loaded by a call to hmsInitForward
*------------------------------------------------------------------------

*------------------------------------------------------------------------


*------------------------------------------------------------------------
*------------------------------------------------------------------------
*
*       PART 2:  HMS Reconstruction (Backward Tracking; Focal Plane to Target)
*      -=======-       
*
*	Reconstruction (backward tracking) in the Jlab HMS hall C 
*       spectrometer using reconstruction and forward COSY matrices 
*       (including the effects of beam offsets (out-of plane)) 
*   
*       Both the normal in-plane scattering and the more special 
*       out-of-plane scattering are handeled. The later makes use 
*       of the forward COSY matrices. The algorithm was tested for
*       beam offsets in the range of cm (up or below the 
*       nominal scattering plane) 
* 
*       Supplies:
*         hmsInitRecon (map,p0) 
*           load the reconstruction maps
*         hmsInPlane (u,uT,ok)
*           reconstruction of the target coordinates 
*          (delta, dx/dz, y, dy/dz) at z=0
*         hmsOutOfPlane (u,x,uT,ok)
*           reconstruction of the target coordinates 
*           (delta, dx/dz, y, dy/dz) at z=0 including the 
*           vertical beam offset
*
*       Note: - Before calling hmsReconInPlane or hmsReconOutOfPlaneAccept 
*               the reconstruction map has to be loaded by a call to 
*               hmsInitRecon
*             - Before calling hmsReconOutOfPlane the forward transport 
*               maps have to be loaded by a call to hmsInitForward
*------------------------------------------------------------------------

************************************************************     
************************************************************
      
      SUBROUTINE genRecon (u,x,y,uT,ok,dx,bdl,th,p,mass,spect)
      IMPLICIT NONE
      REAL     u(5),x,y,uT(6)
      LOGICAL  ok
      real p                ! momentum (MeV). (mom<0 for e-, mom>0 for p,d)
      real mass               ! mass of particle (MeV)
      integer spect            

      INCLUDE 'gen_constants.par'


*     --  performs the reconstruction of the target coordinates 
*     (delta, dx/dz, y, dy/dz) including the effects of the
*     target magnetic field and the vertical beam offset
*     
*     Parameter:
*     u      I : focal plane coordinates  
*     u(1,2)  : x [m], dx/dz = out of plane coords. (downwards) 
*     u(3,4)  : y [m], dy/dz = inplane coords. (perp. on x,z)
*     u(5)    :  vert. beam offset [m] (out of plane coord.; downwards)
*     x      I : vert. beam offset [m] (out of plane coord.; downwards)
*     y      I : hori. beam offsey [m] (inplane coord.; perp on x-beam, z-beam)
*     uT     O : target coordinates
*     uT(1,2) : x [m], dx/dz = out of plane coord. (downwards) 
*                    uT(3,4) : y [m], dy/dz = inplane coord. (perp. on x,z)
*     uT(5)   : z [m] = in axis coordinate (towards HMS)  
*     uT(6)   : delta = relative deviation of the particle 
*     momentum from p0
*     ok   IO  : status variable 
*     - if false no action is taken 
*     - set to false when no reconstruction is found 
      
      REAL*8 th,ctheta,stheta ! cosine and sine of central spectrometer angle
*      COMMON /genParameter/theta,ctheta,stheta,p 
c     
      INCLUDE 'gen_event_info.cmn'
      logical outside_fieldmap
      common /mkjtemp/ outside_fieldmap
      
!     REAL    xx,dx,vT(6),vTx(6),utsave(6),vtsave(6),usave(4)
      REAL*8    xx,dx,vT(9),vTx(9),utsave(6),vtsave(6),usave(6)
      real*8 save_dx,save_diff_dx
      INTEGER i,n,ii
      real*8 REF_VAL	
      parameter (REF_VAL=100.)  ! what does this acyually correspond to?
!     parameter (REF_VAL=80.)         ! what does this acyually correspond to?
      real*8 OTHER_REF
      parameter (OTHER_REF=30.) ! what does this acyually correspond to?
      
      REAL       eps            ! accurracy for x in mm
      PARAMETER (eps = 0.2)     ! (one more iteration is performed 
!  after the given accuraccy is reached) 
      real bdl
      real eng              ! energy of the particle     
      integer flag_az           !	OR - 7/04
      common /azimuth/ flag_az	!	OR - 7/04	
      
      flag_az = 1		!	OR - 7/04
      
      bdl = 0.0
      xx = u(5)
! find a first approximation for uT
c     CALL hmsReconOutOfPlane (u,xx,uT,ok)
!     write(*,*)'u(1:4) - 0', u(1),u(2),u(3),u(4)
!     write(*,*)'uT(1:4) - before 1st. ReconXTar', uT(1),uT(2),uT(3),uT(4)
      
      CALL hmsReconXtar (u,uT,ok)
      
      
      
!     write(*,*)'u(1:4) - 1', u(1),u(2),u(3),u(4)
!     write(*,*)'uT(1:4) - after 1st. ReconXTar', uT(1),uT(2),uT(3),uT(4)
      
c     if (.not. ok) write(*,*) ' hmsreconxtar is not ok'
c     
c     
      
!     write(*,*)'x,y, u(5), uT(1)',x,y,u(5),uT(1)
      
      IF (.NOT. ok) RETURN
c     write(*,*) ' after hmsReconXtar uT = ',uT
!     drift to a field free region and calculate the velocities
      vT(1) = REF_VAL*(uT(1)+1.*uT(2))
!     vT(1) = REF_VAL*(x+1.*uT(2))
      vT(2) = REF_VAL*(uT(3)+1.*uT(4))
      vT(3) = REF_VAL*1.
      vT(6) = OTHER_REF/SQRT(1+uT(2)**2+uT(4)**2)
      vT(4) = uT(2)*vT(6)
      vT(5) = uT(4)*vT(6) 
      do ii=1,6
         utsave(ii)=ut(ii)
         vtsave(ii)=vt(ii)
         usave(ii)=u(ii)
      enddo
!     and track into the magnetic field to the beam plane (perp. to y)
c     write(*,*) ' before trgtracktoplane vt = ',vt
      
!     write(*,*) 'vT(1:6) before 1st. trk2plane',vT(1),vT(2),vT(3),vT(4),vT(5),vT(6)
      
*     Here need to to implement detection of protons/electrons in the HMS arm
*     p should be initialized as hpcentral

      ctheta = COS(th*degree)
      stheta = SIN(th*degree)

      if (spect.eq.-1) then
         eng = -1.*sqrt(p**2+mass**2)
      else if (spect.eq.1.) then
         eng = 1.*sqrt(p**2+mass**2)
      else
      endif

      
*   CALL trgTrackToPlane (vT,-p*(1+uT(6))/MeV,1.d00,
*   >     0.0d00,-ctheta,stheta,y*REF_VAL,ok)


      CALL trgTrackToPlane (vT,eng,1.0d00,0.0d00,-ctheta,stheta,y*REF_VAL,ok)
     
!     write(*,*) 'vT(1:6) after 1st. trk2plane',vT(1),vT(2),vT(3),vT(4),vT(5),vT(6)
!     write(*,*) ' Bdl - 1',vT(7),vT(8),vT(9),ok
      
      if ( .not. ok) then
c     write(*,*) '**** failed first call to trgTrackToPlane in gen_recon *** outside fieldmap = ',outside_fieldmap
      endif
      n  = 0
      dx = 1.
      save_diff_dx = 1.  
      save_dx=dx
c     if (outside_fieldmap) then
c     write(*,*) gen_event_ID_number,'trgtracktoplane',n,dx,ok,outside_fieldmap,x,vt(1)
c     write(*,'(i2,2f10.5,/,4f15.5,/6f15.8,/6f15.8)') ok,x,y,usave,utsave,vt
c     endif
      DO WHILE ((dx .GT. .1) .AND. (n .LT. 10) .and. (save_diff_dx .gt. 0) .AND. ok)
!     DO WHILE ((dx .GT. .01) .AND. (n .LT. 10) .and. (save_diff_dx .gt. 0) .AND. ok)
         dx = abs(x*REF_VAL-vT(1))
!     track to the z=0 plane to find a correction for the x-offset   
         vTx(1) = REF_VAL*x 
         DO i=2,6
            vTx(i) = vT(i)
         ENDDO
         
*         CALL trgTrackToPlane (vT, -p*(1+uT(6))/MeV,1.,0., 0.,1.,0., ok)
         CALL trgTrackToPlane (vT,eng,1.0d00,0.0d00,0.0d00,1.0d00,0.0d00,ok)
         
!     write(*,*) ' Bdl - 2',vT(7),vT(8),vT(9),ok
         
c     if (outside_fieldmap) then
c     write(*,*) gen_event_ID_number,'trgtracktoplane',n,dx,ok,outside_fieldmap,x,vt(1)
c     write(*,'(i2,2f10.5,/,4f15.5,/6f15.8,/6f15.8)') ok,x,y,usave,utsave,vt
c     endif
*         CALL trgTrackToPlane (vTx,-p*(1+uT(6))/MeV,1.,0., 0.,1.,0., ok) 
         CALL trgTrackToPlane (vTx,eng,1.0d00,0.0d00,0.0d00,1.0d00,0.0d00,ok) 
         
!     write(*,*) ' Bdl - 3',vTx(7),vTx(8),vTx(9),ok
         
c     if (outside_fieldmap) then
c     write(*,*) gen_event_ID_number,'trgtracktoplane',n,dx,ok,outside_fieldmap,x,vt(1)
c     write(*,'(i2,2f10.5,/,4f15.5,/6f15.8,/6f15.8)') ok,x,y,usave,utsave,vt
c     endif
         xx = xx+(vTx(1)-vT(1))*0.01 ! what unit conversion is this???
         
!     now find a better approximation for uT
c     CALL hmsReconOutOfPlane (u,xx,uT,ok)
         
!     write(*,*)'u(1:4) - 2', u(1),u(2),u(3),u(4)
!     write(*,*)'uT(1:4) - before 2nd. ReconXTar', uT(1),uT(2),uT(3),uT(4)
         
         u(5) = xx
         CALL hmsReconXtar (u,uT,ok)
         
!     write(*,*)'u(1:4) - 3', u(1),u(2),u(3),u(4)
!     write(*,*)'uT(1:4) - after 2nd. ReconXTar', uT(1),uT(2),uT(3),uT(4)
         
!     drift to a field free region and calculate the velocities
         vT(1) = REF_VAL*(uT(1)+1.*uT(2))
         vT(2) = REF_VAL*(uT(3)+1.*uT(4))
         vT(3) = REF_VAL*1.
         vT(6) = OTHER_REF/SQRT(1+uT(2)**2+uT(4)**2)
         vT(4) = uT(2)*vT(6)
         vT(5) = uT(4)*vT(6) 
         
!     and track into the magnetic field to the beam plane (perp. to y)
         
!     write(*,*) 'vT(1:6) before last trk2plane',vT(1),vT(2),vT(3),vT(4),vT(5),vT(6)
         
*         CALL trgTrackToPlane (vT,-p*(1+uT(6))/MeV,1.,
*     >        0.,-ctheta,stheta,y*REF_VAL,ok) 
         CALL trgTrackToPlane (vT,eng,1.d00,0.d00,-ctheta,stheta,y*REF_VAL,ok) 
!     write(*,*) 'vT(1:6) after last trk2plane',vT(1),vT(2),vT(3),vT(4),vT(5),vT(6)
         
         
         
         bdl = sqrt(vT(7)**2+vT(8)**2+vT(9)**2)
!     write(*,*) ' Bdl - 4',vT(7),vT(8),vT(9),ok
         
c     if (outside_fieldmap) then
c     write(*,*) gen_event_ID_number,'trgtracktoplane',n,dx,ok,outside_fieldmap,x,vt(1)
c     write(*,'(i2,2f10.5,/,4f15.5,/6f15.8,/6f15.8)') ok,x,y,usave,utsave,vt
c     endif
         dx = abs(x*REF_VAL-vT(1))
         save_diff_dx = save_dx - dx
         if (save_diff_dx .lt. 0 .and.   n .ne. 0) then
c     write(*,*) gen_event_ID_number,'trgtracktoplane1',n,dx,save_dx,save_diff_dx,ok,outside_fieldmap,x,vt(1)
            do ii=1,6
               vt(ii)=vtsave(ii)
            enddo
            ok = .false.
            if ( save_dx .lt. 1.0) ok = .true.
         else
c     write(*,*) gen_event_ID_number,'trgtracktoplane2',n,dx,save_dx,save_diff_dx,ok,outside_fieldmap,x,vt(1)
            n = n+1
            do ii=1,6
               vtsave(ii)=vt(ii)
            enddo
            save_dx = dx
         endif
      ENDDO
      IF (n .ge. 10 ) ok = .FALSE.
      if (.not. ok) then
c     write(*,*) '*** failed converging **** '
c     write(*,'(i2,i5,i3,2f10.5,/,4f15.5,/6f15.8,/2f15.8)') ok,gen_event_ID_number,n,x,y,usave,utsave,dx,save_dx
      endif      
!     calculate the result in HMS coordinates
      
!     write(*,*) 'vT(1:6) before return',vT(1),vT(2),vT(3),vT(4),vT(5),vT(6)
!     write(*,*) ' '
      
      
      uT(1) =  0.01*vT(1)  
      uT(2) = vT(4)/vT(6)
      uT(3) =  0.01*vT(2)
      uT(4) = vT(5)/vT(6)
      uT(5) =  0.01*vT(3)
c     write(*,*) ' after tracktoplane uT = ',uT   
      RETURN
      END
      
**************************************************************
**************************************************************

      SUBROUTINE hmsReconXtar (u,uT,ok)
      IMPLICIT NONE
      REAL     u(5),uT(6)
      LOGICAL  ok


      include 'gen_filenames.cmn'
      include 'gen_data_structures.cmn'  
      include 'hms_filenames.cmn'
      include 'hms_data_structures.cmn'  
      include 'hms_recon_elements.cmn'   
      include 'hms_bypass_switches.cmn'

      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      INCLUDE 'hms_tracking.cmn'
      INCLUDE 'hms_track_histid.cmn'
      INCLUDE 'hms_physics_sing.cmn'
      INCLUDE 'sane_data_structures.cmn'

       
* --  performs the reconstruction of the target coordinates 
*     (delta, dx/dz, y, dy/dz) at z=0
*     
*     Parameter: 
*       u      I : focal plane coordinates (lab)  
*                    u(1,2)  : x [m], dx/dz = out of plane coord. (downwards) 
*                    u(3,4)  : y [m], dy/dz = inplane coord. (perp. on x,z)
*                    u(5)    : xtarget [m] from raster info 
*       uT     O : target coordinates (lab)
*                    uT(1,2) : x [m], dx/dz = out of plane coord. (downwards) 
*                    uT(3,4) : y [m], dy/dz = inplane coord. (perp. on x,z)
*                    uT(5)   : z [m] = in axis coordinate (towards HMS)  
*                    uT(6)   : delta (relative deviation of the particle 
*                                     momentum from p0)
*       ok   IO  : status variable 
*                   - if false no action is taken 
*                   - set to false when no reconstruction is found 

      ! matrix elemnts needed for calculating the focal plane offset 
 
      INTEGER i,j 
      REAL*8 tm
      real*8   sum(4),hut(5),hut_rot(5)
 
      COMMON /hmsfocalplane/sum,hut,hut_rot     
      DO i=1,6
         uT(i)  = 0.
      ENDDO
      
*     Reset COSY sums.
      do i = 1,4
         sum(i) = 0.
      enddo
      
      
      do i = 1,h_num_recon_terms
         tm = 1
         do j = 1,4
            if (h_recon_expon(j,i).ne.0.) then
               tm = tm*hut_rot(j)**h_recon_expon(j,i)
            endif
         enddo
         sum(1) = sum(1) + tm*h_recon_coeff(1,i) ! xp uT(2) trg(2)
         sum(2) = sum(2) + tm*h_recon_coeff(2,i) ! y  uT(3) trg(3)
         sum(3) = sum(3) + tm*h_recon_coeff(3,i) ! yp uT(4) trg(4)
         sum(4) = sum(4) + tm*h_recon_coeff(4,i) ! delta uT(6) trg(6)
      enddo
                                !  uT(5),trg(5) is z-position along the HMS spectrometer axis
                                !        used in tracking back to the target
!     uT(1),trg(1) is xtarget position, measured by slow raster.
      
      uT(1) = hut(5)
      uT(2) = sum(1)            ! unit meters
      uT(3) = sum(2)            ! unit meters
      uT(4) = sum(3)
      uT(5) = 0                 ! not important at this point
      uT(6) = sum(4)            !
      
      
      ok = ((ABS(uT(2)) .LT. 1.) .AND. (ABS(uT(3)) .LT. 1.) .AND.
     >     (ABS(uT(4)) .LT. 1.) .AND. (ABS(uT(6)) .LT. 1.)) 
      RETURN
      END   
      
************************************************************
************************************************************
