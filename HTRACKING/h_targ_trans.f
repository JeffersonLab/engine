      SUBROUTINE H_TARG_TRANS(ABORT,err,istat)
*--------------------------------------------------------
*-
*-   Purpose and Methods :  Transforms tracks from HMS focal plane to 
*-                          target.
*-
*-      Required Input BANKS     HMS_FOCAL_PLANE
*-
*-      Output BANKS             HMS_TARGET
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*-
*-  istat   (integer) Status flag. Value returned indicates the following:
*-           = 1      Normal return.
*-           = 2      Matrix elements not initted correctly.
*-    
* $Log$
* Revision 1.16.24.2.2.2  2008/10/29 06:20:21  cdaq
* Set hut(5)=rast_y
* Set loop from 4 to 5
*
* Revision 1.16.24.2.2.1  2008/10/21 20:33:17  cdaq
* target recon with B field added
*
* Revision 1.16.24.2  2007/10/27 21:15:32  cdaq
* fix erroneous submissions
*
* Revision 1.16  1999/02/23 18:50:38  csa
* (JRA) Correct (another) hut(5) error
*
* Revision 1.15  1999/02/10 17:47:27  csa
* Sign change in hut(5)
*
* Revision 1.14  1996/09/04 13:32:36  saw
* (JRA) Apply offsets to reconstruction
*
* Revision 1.13  1996/01/17 18:15:53  cdaq
* (JRA)
*
* Revision 1.12  1995/10/10 17:49:31  cdaq
* (JRA) Cleanup
*
* Revision 1.11  1995/08/08 16:01:17  cdaq
* (DD) Add detector and angular offsets
*
* Revision 1.10  1995/05/22  19:39:28  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.9  1995/04/06  19:31:54  cdaq
* (SAW) Put in ddutta's pre cosy x-x', y-y' transformation
*
* Revision 1.8  1995/03/22  16:22:40  cdaq
* (SAW) Previous change wrong.  COSY wants slopes.
* Target track data is now slopes.
*
* Revision 1.7  1995/02/10  18:46:01  cdaq
* (SAW) Convert focal plane slopes to angles before COSY transport.
* Target track data is now angles.
*
* Revision 1.6  1994/10/11  19:11:33  cdaq
* (SAW) Are the target traceback calculations right now???
*
* Revision 1.5  1994/08/18  04:29:59  cdaq
* (SAW) Arrington's changes??
*
* Revision 1.4  1994/06/14  04:51:21  cdaq
* (DFG) Add fill HLINK_TAR_FP 1 to 1
*
* Revision 1.3  1994/06/06  17:03:17  cdaq
* (DFG) Protect against asin argument > 1.0
*
* Revision 1.2  1994/05/13  02:28:59  cdaq
* (DFG)   Add call to h_fill_dc_target_hist
*         Add calculation of HP_TAR
* (SAW)   Cosmetic changes to source
*
* Revision 1.1  1994/02/19  06:19:24  cdaq
* Initial revision
*- Modified 21-JAN-94   D. F. Geesaman
*-           Add ABORT,err to returns.
*- Version:  0.1 (In development)  18-Nov-1993 (DHP)
*-
*
* Abstract: Reconstruct target scattering variables from track variables in
*           the detectors, using a polynomial (Taylor series) map. The track,
*           target, and map data are all maintained in common blocks.
*
* NOTE:     This version assumes that the beam is not rastered.
*           Also, there is no treatment of error matrices, yet.
*
* Output arguments:
*
*
* Right-handed coordinates are assumed: X=down, Z=downstream, Y = (Z cross X)
*
* Author:   David H. Potterveld, Argonne National Lab, Nov. 1993
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*12 here
      parameter (here= 'h_targ_trans')
*
      logical ABORT,ok
      character*(*) err
      integer*4   istat
      real*4 x_coord, y_coord
*
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      INCLUDE 'hms_tracking.cmn'
      INCLUDE 'hms_recon_elements.cmn'
      INCLUDE 'hms_track_histid.cmn'
      INCLUDE 'hms_physics_sing.cmn'
      INCLUDE 'sane_data_structures.cmn'


*
*--------------------------------------------------------
*
* Misc. variables.

      integer*4        i,j,itrk

      real*8   sum(4),hut(5),term,hut_rot(5)
      real*8   trg(6),bdl,dx

      COMMON /hmsfocalplane/sum,hut,hut_rot 
*=============================Executable Code =============================
      ABORT= .FALSE.
      err= ' '
* Check for correct initialization.

      if (h_recon_initted.ne.1) then
         istat = 2
         return
      endif
      istat = 1

      x_coord = gsr_beamx/100.  ! SLOW RASTER BEAM X coordinate obtained from the ADCs, in meters 
      y_coord = gsr_beamy/100.  ! SLOW RASTER BEAM Y coordinate obtained from the ADCs, in meters 
      

* Loop over tracks.

      hntracks_tar = hntracks_fp
      
      do itrk = 1,hntracks_fp
         
*     set link between target and focal plane track. Currently 1 to 1
         
         hlink_tar_fp(itrk) = itrk
         
         
*     Reset COSY sums.
         do i = 1,4
            sum(i) = 0.
         enddo


* Load track data into local array, Converting to COSY units.
* It is assumed that the track coordinates are reported at
* the same focal plane as the COSY matrix elements were calculated.
* Also note that the COSY track slopes HUT(2) and HUT(4) are actually
* the SINE of the track angle in the XZ and YZ planes.

         hut(1) = hx_fp(itrk)/100. + h_z_true_focus*hxp_fp(itrk)
     $        + h_det_offset_x          ! include detector offset  (m)
! includes transformation to actual focus if not at Z=0.
 
         hut(2) = hxp_fp(itrk) + h_ang_offset_x           !radians

         hut(3) = hy_fp(itrk)/100. + h_z_true_focus*hyp_fp(itrk)
     $        + h_det_offset_y     !m
! again icludes transformation to true focus.

         hut(4) = hyp_fp(itrk) + h_ang_offset_y           !radians

         hut(5)= -gbeam_y/100. ! spectrometer target X in meter!
                                ! note that pos. spect. X = neg. beam Y, here should be the coordinate given by the slow raster
c         hut(5)= x_coord ! spectrometer target X in meter - given by the Slow Raster!
                                ! note that pos. spect. X = neg. beam Y, here should be the coordinate given by the slow raster


! now transform 
*         hx_fp_rot(itrk)=  hut(1) + h_det_offset_x    ! include detector offset
*         hy_fp_rot(itrk)=  hut(3) + h_det_offset_y 
*         hxp_fp_rot(itrk)= hut(2) + hut(1)*h_ang_slope_x
*         hyp_fp_rot(itrk)= hut(4) + hut(3)*h_ang_slope_y
*         hut_rot(1)= hx_fp_rot(itrk)
*         hut_rot(2)= hxp_fp_rot(itrk)
*         hut_rot(3)= hy_fp_rot(itrk)
*         hut_rot(4)= hyp_fp_rot(itrk)
*        h*_fp_rot never used except here, so remove the intermediate step.

         hut_rot(1) = hut(1)
         hut_rot(2) = hut(2) + hut(1)*h_ang_slope_x
         hut_rot(3) = hut(3)
         hut_rot(4) = hut(4) + hut(3)*h_ang_slope_y
         hut_rot(5) = hut(5)

* Introducing the target magnetic field option 

         if (SANE_TGTFIELD_B.eq.0.0) then
            
*     Compute COSY sums.
            
            do i = 1,h_num_recon_terms
               term = 1.
               do j = 1,5
                  if (h_recon_expon(j,i).ne.0.)
     ,                 term = term*hut_rot(j)**h_recon_expon(j,i)
               enddo
               sum(1) = sum(1) + term*h_recon_coeff(1,i) ! xp uT(2) trg(2)
               sum(2) = sum(2) + term*h_recon_coeff(2,i) ! y  uT(3) trg(3)
               sum(3) = sum(3) + term*h_recon_coeff(3,i) ! yp uT(4) trg(4)
               sum(4) = sum(4) + term*h_recon_coeff(4,i) ! delta uT(6) trg(6)
            enddo
!     uT(5),trg(5) is z-position along the HMS spectrometer axis
!     used in tracking back to the target
!     uT(1),trg(1) is xtarget position, measured by slow raster.
            
*     Load output values.
            
            hx_tar(itrk) = x_coord ! beam slow raster coord.
            hy_tar(itrk) = sum(2)*100. !cm.
            hxp_tar(itrk) = sum(1) !Slope xp
            hyp_tar(itrk) = sum(3) !Slope yp            
            hz_tar(itrk) = 0.0  !Track is at origin
            hdelta_tar(itrk) = sum(4)*100. !percent.
            
         else
            
*     Parameter:
*     subroutine genRecon(u,x,y,uT,ok,dx,bdl)
*     u      I : focal plane coordinates  
*     u(1,2)  : x [m], dx/dz = out of plane coords. (downwards) 
*     u(3,4)  : y [m], dy/dz = inplane coords. (perp. on x,z)
*     u(5)    :  vert. beam offset [m] (out of plane coord.; downwards)
*     x_coord      I : vert. beam offset [m] (out of plane coord.; downwards)
*     y_coord      I : hori. beam offsey [m] (inplane coord.; perp on x-beam, z-beam)
*     uT     O : target coordinates
*     uT(1,2) : x [m], dx/dz = out of plane coord. (downwards) 
*                    uT(3,4) : y [m], dy/dz = inplane coord. (perp. on x,z)
*     uT(5)   : z [m] = in axis coordinate (towards HMS)  
*     uT(6)   : delta = relative deviation of the particle 
*     momentum from p0
*     ok   IO  : status variable 
*     - if false no action is taken 
*     - set to false when no reconstruction is found 
            
            
            ok = .TRUE.
            
            CALL trgInitFieldANGLES(SANE_HMS_OMEGA,SANE_HMS_PHI)
            
            CALL genRecon (hut_rot, x_coord, y_coord, trg, ok, dx, bdl,
     >           htheta_lab, hpcentral, hpartmass, 1.) ! set for protons
            
*     CALL genRecon (hut_rot, x_coord, y_coord, trg, ok, dx, bdl,
*     >           hpcentral, mass_electron, -1.)  ! set for electrons
            
            hx_tar(itrk)     = trg(1)*meter ! target x 
            hy_tar(itrk)     = trg(3)*meter ! target y   
            hz_tar(itrk)     = trg(5)*meter ! target z 
            hxp_tar(itrk)    = trg(2) ! slope  xp
            hyp_tar(itrk)    = trg(4) ! slope  yp
            hdelta_tar(itrk) = trg(6)*100. !percent.
            h_bdl(itrk)  = bdl
            
         endif                  ! loop over the magnetic field (on or off)
      enddo                     !End of loop over tracks.
      
*     Apply offesets to the reconstructed variables
      
      hdelta_tar(itrk) = hdelta_tar(itrk) + hdelta_offset
      hyp_tar(itrk) = hyp_tar(itrk) + htheta_offset
      hxp_tar(itrk) = hxp_tar(itrk) + hphi_offset
      
      hp_tar(itrk)  = hpcentral*(1.0 + hdelta_tar(itrk)/100.) !Momentum in GeV
      
*     All done...
*     print target bank if debug flag set
      if(hdebugtartrackprint.gt.0) then
         call h_print_tar_tracks
      endif
*     Fill hardwired histograms if hturnon_target_hist is non zero
*     
      if(hturnon_target_hist.gt.0) then
         call h_fill_dc_target_hist(ABORT,err)
         if(ABORT) then
            call g_add_path(here,err)
         endif
      endif
      return
      end
      
