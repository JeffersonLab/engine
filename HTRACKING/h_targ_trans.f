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
      logical ABORT
      character*(*) err
      integer*4   istat
*
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'hms_tracking.cmn'
      include 'hms_recon_elements.cmn'
      include 'hms_track_histid.cmn'
*
*--------------------------------------------------------
*
* Misc. variables.

      integer*4        i,j,itrk

      real*8           sum(4),hut(4),term,hut_rot(4)

*=============================Executable Code =============================
      ABORT= .FALSE.
      err= ' '
* Check for correct initialization.

      if (h_recon_initted.ne.1) then
         istat = 2
         return
      endif
      istat = 1

* Loop over tracks.

      hntracks_tar = hntracks_fp
      do itrk = 1,hntracks_fp
*     set link between target and focal plane track. Currently 1 to 1
         hlink_tar_fp(itrk) = itrk

* Reset COSY sums.

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

* Compute COSY sums.
         do i = 1,h_num_recon_terms
            term = 1.
            do j = 1,4
               if (h_recon_expon(j,i).ne.0.)
     $             term = term*hut_rot(j)**h_recon_expon(j,i)
            enddo
            sum(1) = sum(1) + term*h_recon_coeff(1,i)
            sum(2) = sum(2) + term*h_recon_coeff(2,i)
            sum(3) = sum(3) + term*h_recon_coeff(3,i)
            sum(4) = sum(4) + term*h_recon_coeff(4,i)
         enddo
* Protext against asin argument > 1.
c         if(sum(1).gt. 1.0) sum(1)=  0.99
c         if(sum(1).lt. -1.0) sum(1)= -.99
c         if(sum(3).gt. 1.0) sum(3)=  0.99
c         if(sum(3).lt. -1.0) sum(3)= -.99

     
* Load output values.

         hx_tar(itrk) = 0.              ! ** No beam raster yet **
         hy_tar(itrk) = sum(2)*100.     !cm.
         hxp_tar(itrk) = sum(1)         !Slope xp
         hyp_tar(itrk) = sum(3)         !Slope yp

         hz_tar(itrk) = 0.0             !Track is at origin
         hdelta_tar(itrk) = sum(4)*100. !percent.
         HP_TAR(itrk)  = HPCENTRAL*(1.0 + sum(4)) !Momentum in GeV

* The above coordinates are in the spectrometer reference frame in which the
* Z axis is along the central ray. Do we need to rotate to the lab frame?
* For now, I assume not.

      enddo                             !End of loop over tracks.

* All done...
*       print target bank if debug flag set
      if(hdebugtartrackprint.gt.0) then
         call h_print_tar_tracks
      endif
* Fill hardwired histograms if hturnon_target_hist is non zero
*
      if(hturnon_target_hist.gt.0) then
         call h_fill_dc_target_hist(ABORT,err)
         if(ABORT) then
            call g_add_path(here,err)
         endif
      endif
      return
      end
