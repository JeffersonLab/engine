      SUBROUTINE S_TARG_TRANS(ABORT,err,istat)
*--------------------------------------------------------
*-
*-   Purpose and Methods :  Transforms tracks from SOS focal plane to 
*-                          target.
*-
*-      Required Input BANKS     SOS_FOCAL_PLANE
*-
*-      Output BANKS             SOS_TARGET
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*-   istat   (integer) Status flag. Value returned indicates the following:
*-           = 1      Normal return.
*-           = 2      Matrix elements not initted correctly.
*- 
* Version:  0.1 (In development)  18-Nov-1993 (DHP)
*-   
*-Modified 21-JAN-94  D.F.Geesaman
*-            Add ABORT and err
* $Log: s_targ_trans.f,v $
* Revision 1.15  1999/02/23 19:01:10  csa
* (JRA) Correct (another) hut(5) error
*
* Revision 1.14  1999/02/10 17:47:17  csa
* Sign change in hut(5)
*
* Revision 1.13  1996/09/05 20:15:53  saw
* (JRA) Apply offsets to reconstruction
*
* Revision 1.12  1996/01/17 18:10:27  cdaq
* (JRA)
*
* Revision 1.11  1995/10/10 17:52:40  cdaq
* (JRA) Cleanup
*
* Revision 1.10  1995/08/08 16:01:57  cdaq
* (DD) Add detector and angular offsets
*
* Revision 1.9  1995/05/22  19:45:57  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.8  1995/03/23  16:51:57  cdaq
* (SAW) Previous change wrong.  COSY wants slopes.
* Target track data is now slopes.
*
* Revision 1.7  1995/02/23  16:03:05  cdaq
* (SAW) Convert focal plane slopes to angles before COSY transport.
* Target track data is now angles.
*
* Revision 1.6  1994/11/23  14:03:27  cdaq
* (SPB) Recopied from hms file and modified names for SOS
*
* Revision 1.5  1994/08/18  04:35:28  cdaq
* (SAW) ???
*
* Revision 1.4  1994/06/14  04:33:22  cdaq
* (DFG) Add fill SLINK_TAR_FP 1 to 1
*
* Revision 1.3  1994/06/07  01:58:56  cdaq
* (DFG) Protect against asin argument > 1.0
*
* Revision 1.2  1994/05/13  03:45:52  cdaq
* (DFG) Add call to s_fill_dc_target_hist
*       Add calculation of SP_TAR
* (SAW) Cosmetic changes to source
*
* Revision 1.1  1994/02/21  16:41:11  cdaq
* Initial revision
*
*
* Abstract: Reconstruct target scattering variables from track variables in
*           the detectors, using a polynomial (Taylor series) map. The track,
*           target, and map data are all maintained in common blocks.
*
* NOTE:     This version assumes that the beam is not rastered.
*           Also, there is no treatment of error matrices, yet.
*-
* Right-handed coordinates are assumed: X=down, Z=downstream, Y = (Z cross X)
*
* Author:   David H. Potterveld, Argonne National Lab, Nov. 1993
*______________________________________________________________________________
      IMPLICIT NONE
      SAVE
*
      character*12 here
      parameter (here= 's_targ_trans')
*
      logical ABORT
      character*(*) err
      integer*4   istat
*
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'sos_tracking.cmn'
      include 'sos_recon_elements.cmn'
      include 'sos_track_histid.cmn'
      include 'sos_physics_sing.cmn'
*
* Misc. variables.

      integer*4        i,j,itrk
      
      real*8           sum(4),hut(5),term,hut_rot(5)

*============================= Executable Code ================================
      ABORT= .FALSE.
      err= ' '

* Check for correct initialization.

      if (s_recon_initted.ne.1) then
         istat = 2
         return
      endif
      istat = 1
      
* Loop over tracks.

      sntracks_tar = sntracks_fp
      do itrk = 1,sntracks_fp
*
*     set link between target and focal plane track. Currenty 1 to 1
         slink_tar_fp(itrk) = itrk
*         
* Reset COSY sums.
         do i = 1,4
            sum(i) = 0.
         enddo

* Load track data into local array, Converting to COSY units.
* Note:  At this point, the focal plane variables sxp_fp and syp_fp are
* still slopes.  We convert them to angles before running them through the
* COSY transport matrices.
* It is assumed that the track coordinates are reported at
* the same focal plane as the COSY matrix elements were calculated.
         hut(1) = sx_fp(itrk)/100.+ s_z_true_focus*sxp_fp(itrk)
     $        + s_det_offset_x !m + detector offset
! includes transformation to actual focus if not at Z=0.

         hut(2) = sxp_fp(itrk) + s_ang_offset_x         !COSY wants slopes

         hut(3) = sy_fp(itrk)/100. + s_z_true_focus*syp_fp(itrk)
     $        + s_det_offset_y     !m + detector offset
! again icludes transformation to true focus.

         hut(4) = syp_fp(itrk) + s_ang_offset_y         !COSY wants slopes

         hut(5) = -gbeam_y/100.         ! spectrometer target X in meter!    

! now transform
         hut_rot(1) = hut(1)
         hut_rot(3) = hut(3)
         hut_rot(2) = hut(2) + hut(1)*s_ang_slope_x
         hut_rot(4) = hut(4) + hut(3)*s_ang_slope_y
         hut_rot(5) = hut(5)
* Compute COSY sums.

         do i = 1,s_num_recon_terms
            term = 1.
            do j = 1,5
               if (s_recon_expon(j,i).ne.0.)
     $              term = term*hut(j)**s_recon_expon(j,i)
            enddo
            sum(1) = sum(1) + term*s_recon_coeff(1,i)
            sum(2) = sum(2) + term*s_recon_coeff(2,i)
            sum(3) = sum(3) + term*s_recon_coeff(3,i)
         enddo

! For the SOS only the delta needs the transformation

         do i = 1,s_num_recon_terms
            term = 1.
            do j = 1,5
               if (s_recon_expon(j,i).ne.0.)
     $              term = term*hut_rot(j)**s_recon_expon(j,i)
            enddo
            sum(4) = sum(4) + term*s_recon_coeff(4,i)
         enddo

* Protect against asin argument > 1.
c         if(sum(1).gt. 1.0)  sum(1)= 0.99
c         if(sum(1).lt. -1.0) sum(1)= -.99
c         if(sum(3).gt. 1.0) sum(3)=  0.99
c         if(sum(3).lt. -1.0) sum(3)= -.99
   
* Load output values.
         sx_tar(itrk) = 0               ! ** No beam raster **
         sy_tar(itrk) = sum(2)*100.     !cm.
         sxp_tar(itrk) = sum(1)         !Slope xp
         syp_tar(itrk) = sum(3)         !Slope yp

         sz_tar(itrk) = 0.0             !Track is at origin
         sdelta_tar(itrk) = sum(4)*100. !percent.

* Apply offsets to reconstruction.
         sdelta_tar(itrk) = sdelta_tar(itrk) + sdelta_offset
         syp_tar(itrk) = syp_tar(itrk) + stheta_offset
         sxp_tar(itrk) = sxp_tar(itrk) + sphi_offset

         sp_tar(itrk)  = spcentral*(1.0 + sdelta_tar(itrk)/100.) !Momentum in GeV

* The above coordinates are in the spectrometer reference frame in which the
* Z axis is along the central ray. Do we need to rotate to the lab frame?
* For now, I assume not.

      enddo                             !End of loop over tracks.

* All done...
*       check print flag to print results
      if(sdebugtartrackprint.gt.0) then
         call s_print_tar_tracks
      endif
* Fill hardwired histograms if sturnon_target_hist is non zero
*
      if(sturnon_target_hist.gt.0) then
         call s_fill_dc_target_hist(ABORT,err)
         if(ABORT) then
            call g_add_path(here,err)
         endif
      endif
      return
      end
