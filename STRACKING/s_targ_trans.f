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
* $Log$
* Revision 1.2  1994/05/13 03:45:52  cdaq
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
      character*50 here
      parameter (here= 'S_TARG_TRANS')
*
      logical ABORT
      character*(*) err
      integer*4   istat
*
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'sos_tracking.cmn'
      include 'sos_recon_elements.cmn'
      include 'sos_tracking_histid.cmn'
*
* Misc. variables.

      integer*4        i,j,k,l,m,n,itrk
      
      real*8           sum(4),hut(4),term,temp

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
         
* Reset COSY sums.

         do i = 1,4
            sum(i) = 0.
         enddo
         
* Load track data into local array, Converting to COSY units.
* It is assumed that the track coordinates are reported at
* the same focal plane as the COSY matrix elements were calculated.
* Also note that the COSY track slopes HUT(2) and HUT(4) are actually
* the SINE of the track angle in the XZ and YZ planes.

         hut(1) = sx_fp(itrk)/100.      !Meters.
         hut(2) = sin(atan(sxp_fp(itrk))) !SINE.
         hut(3) = sy_fp(itrk)/100.      !Meters.
         hut(4) = sin(atan(syp_fp(itrk))) !SINE.
         

* Compute COSY sums.

         do i = 1,s_num_recon_terms
            term = 1.
            do j = 1,4
               temp = 1.0
               if (s_recon_expon(j,i).ne.0.) temp = hut(j)**s_recon_expon(j
     $              ,i)
               term = term*temp
            enddo
            sum(1) = sum(1) + term*s_recon_coeff(1,i)
            sum(2) = sum(2) + term*s_recon_coeff(2,i)
            sum(3) = sum(3) + term*s_recon_coeff(3,i)
            sum(4) = sum(4) + term*s_recon_coeff(4,i)
         enddo
     
* Load output values.

         sx_tar(itrk) = sum(2)*100.     !cm.
         sxp_tar(itrk) = tan(asin(sum(1))) !Slope (dX/dZ)
         sy_tar(itrk) = 0.              ! ** No beam raster **
         syp_tar(itrk) = tan(asin(sum(3))) !Slope (dY/dZ)
         sz_tar(itrk) = 0.              !Track is at origin.
         sdelta_tar(itrk) = sum(4)*100. !percent.
         SP_TAR(itrk)  = SPCENTRAL*(1.0 + sum(4)) !Momentum in GeV
*
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
