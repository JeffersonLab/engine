       SUBROUTINE h_register_param(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Initializes HMS quantities 
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created  8-Nov-1993   Kevin B. Beard
*-   Modified 20-Nov-1993  KBB for new errors
*-            14 Feb-1994  DFG Put in real variables
*-
*- All standards are from "Proposal for Hall C Analysis Software
*- Vade Mecum, Draft 1.0" by D.F.Geesamn and S.Wood, 7 May 1993
* $Log$
* Revision 1.11.24.1  2007/08/22 19:09:30  frw
* added FPP
*
* Revision 1.12  2006/06/22 frw
* added FPP structures
*
* Revision 1.11  2002/12/20 21:53:34  jones
* Modified by Hamlet for new HMS aerogel
*
* Revision 1.10  1995/08/31 14:45:31  cdaq
* (JRA) Register Cerenkov variables
*
* Revision 1.9  1995/05/17  13:57:20  cdaq
* (JRA) Register pedestal variables
*
* Revision 1.8  1994/08/18  03:52:45  cdaq
* (SAW) Call makereg generated routines to register variables
*
* Revision 1.7  1994/06/17  17:46:36  cdaq
* (KBB) Upgrade error reporting
*
* Revision 1.6  1994/06/06  17:13:37  cdaq
* (DFG) add call to register bypass switches and statistics
*
* Revision 1.5  1994/03/24  19:41:33  cdaq
* (DFG) Move actual registereing of variables to subroutines
*
* Revision 1.4  1994/02/23  15:39:02  cdaq
* (SAW) ABORT now when ierr.NE.0
*
* Revision 1.3  1994/02/22  20:39:21  cdaq
* (SAW) Fix booboo
*
* Revision 1.2  1994/02/22  18:52:06  cdaq
* (SAW) Move regpar declarations to gen_routines.dec.  Make title arg null.
*
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*16 here
      parameter (here= 'H_register_param')
*
      logical ABORT
      character*(*) err
*
      logical FAIL
      character*1000 why
*
*--------------------------------------------------------
      err= ' '
      ABORT = .false.
*
*     register tracking variables
*

      call r_hms_tracking
      call r_hms_geometry
      call r_hms_track_histid
      call r_hms_recon_elements
      call r_hms_physics_sing
*
*     register cal, tof and cer variables
*

      call r_hms_scin_parms
      call r_hms_scin_tof
      call r_hms_cer_parms
      call r_hms_calorimeter
      call r_hms_id_histid
      call r_hms_aero_parms
*
*     register FPP variables
*
      call r_hms_fpp_params
*
*     register bypass switches
*

      call r_hms_bypass_switches

*
*     register hms statistics
*

      call r_hms_statistics
      call r_hms_pedestals
*
      IF(ABORT .or. err.NE.' ') call G_add_path(here,err)
*
      RETURN
      END
