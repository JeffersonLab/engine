      SUBROUTINE s_register_param(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Initializes SOS quantities 
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
* $Log: s_register_param.f,v $
* Revision 1.11  1996/11/07 19:53:37  saw
* (WH) Add lucite parameters
*
* Revision 1.10  1996/04/30 17:15:12  saw
* (JRA) Register Aerogel variables
*
* Revision 1.9  1995/08/31 20:43:41  cdaq
* (JRA) Register Cerenkov variables
*
* Revision 1.8  1995/05/17  16:43:28  cdaq
* (JRA) Register pedestal variables
*
* Revision 1.7  1994/08/18  03:59:50  cdaq
* (SAW) Call makereg generated routines to register variables
*
* Revision 1.6  1994/06/07  03:01:22  cdaq
* (DFG) add call to register bypass switches and statistics
*
* Revision 1.5  1994/03/24  19:54:54  cdaq
* (DFG) Put actual registering of variables in subroutines
*
* Revision 1.4  1994/02/23  15:39:50  cdaq
* (SAW) ABORT now when ierr.NE.0
*
* Revision 1.3  1994/02/22  20:39:28  cdaq
* (SAW) Fix booboo
*
* Revision 1.2  1994/02/22  18:52:19  cdaq
* (SAW) Move regpar declarations to gen_routines.dec.  Make title arg null.
*
* Revision 1.1  1994/02/22  18:42:21  cdaq
* Initial revision
*
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*16 here
      parameter (here= 's_register_param')
*
      logical ABORT
      character*(*) err
*
*--------------------------------------------------------
      err= ' '
      ABORT = .false.
*
*     register tracking variables
*

      call r_sos_tracking
      call r_sos_geometry
      call r_sos_track_histid
      call r_sos_recon_elements
      call r_sos_physics_sing
*
*     register cal, tof and cer variables
*

      call r_sos_scin_parms
      call r_sos_scin_tof
      call r_sos_cer_parms
      call r_sos_aero_parms
      call r_sos_lucite_parms
      call r_sos_calorimeter
      call r_sos_id_histid
*
*     register bypass switches
*

      call r_sos_bypass_switches

*
*     register sos statistics
*

      call r_sos_statistics
      call r_sos_pedestals
*
      return
      end
