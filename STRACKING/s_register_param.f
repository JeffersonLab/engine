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
* $Log$
* Revision 1.4  1994/02/23 15:39:50  cdaq
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
      parameter (here= 'S_register_param')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_routines.dec'
      INCLUDE 'sos_tracking.cmn'
      INCLUDE 'sos_geometry.cmn'
*
      INTEGER ierr
*
*--------------------------------------------------------
      err= ' '
      abort = .false.
*
*
*      ierr= regparmreal('raddeg',raddeg,0)
*      IF(ierr.NE.0) err= 'unable to register "raddeg"'
*      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmint('sdc_num_planes',sdc_num_planes,0)
      IF(ierr.NE.0) err= 'unable to register "sdc_num_planes"'
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmint('sdc_num_chambers',sdc_num_chambers,0)
      IF(ierr.NE.0) err= 'unable to register "sdc_num_chambers"'
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmrealarray('sdc_zpos',sdc_zpos,SMAX_NUM_DC_PLANES,0)
      IF(ierr.NE.0) err= 'unable to register "sdc_zpos"'
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmrealarray('sdc_alpha_angle',sdc_alpha_angle,
     &     SMAX_NUM_DC_PLANES,0)
      IF(ierr.NE.0) err= 'unable to register "sdc_alpha_angle"'
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmrealarray('sdc_beta_angle',sdc_beta_angle,
     &     SMAX_NUM_DC_PLANES,0)
      IF(ierr.NE.0) err= 'unable to register "sdc_beta_angle"'
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmrealarray('sdc_gamma_angle',sdc_gamma_angle,
     &     SMAX_NUM_DC_PLANES,0)
      IF(ierr.NE.0) err= 'unable to register "sdc_gamma_angle"'
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmrealarray('sdc_sigma',sdc_sigma,
     &     SMAX_NUM_DC_PLANES,0)
      IF(ierr.NE.0) err= 'unable to register "sdc_sigma"'
      ierr= regparmrealarray('sdc_pitch',sdc_pitch,
     &     SMAX_NUM_DC_PLANES,0)
      IF(ierr.NE.0) err= 'unable to register "sdc_pitch"'
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmrealarray('sdc_nrwire',sdc_nrwire,
     &     SMAX_NUM_DC_PLANES,0)
      IF(ierr.NE.0) err= 'unable to register "sdc_nrwire"'
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmrealarray('sdc_central_wire',sdc_central_wire,
     &     SMAX_NUM_DC_PLANES,0)
      IF(ierr.NE.0) err= 'unable to register "sdc_central_wire"'
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmrealarray('sdc_xcenter',sdc_xcenter,
     &     SMAX_NUM_DC_PLANES,0)
      IF(ierr.NE.0) err= 'unable to register "sdc_xcenter"'
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmrealarray('sdc_ycenter',sdc_ycenter,
     &     SMAX_NUM_DC_PLANES,0)
      IF(ierr.NE.0) err= 'unable to register "sdc_ycenter"'
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmintarray('sdc_chamber_planes',sdc_chamber_planes,
     &     SMAX_NUM_DC_PLANES,0)
      IF(ierr.NE.0) err= 'unable to register "sdc_chamber_planes"'
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmreal('sxt_track_criterion',sxt_track_criterion,
     &     0)
      IF(ierr.NE.0) err= 'unable to register "sxt_track_criterion"'
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmreal('syt_track_criterion',syt_track_criterion,
     &     0)
      IF(ierr.NE.0) err= 'unable to register "syt_track_criterion"'
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmreal('sxpt_track_criterion',sxpt_track_criterion,
     &     0)
      IF(ierr.NE.0) err= 'unable to register "sxpt_track_criterion"'
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmreal('sypt_track_criterion',sypt_track_criterion,
     &     0)
      IF(ierr.NE.0) err= 'unable to register "sypt_track_criterion"'
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmintarray('smin_hit',smin_hit,smax_num_chambers
     &     ,0)
      IF(ierr.NE.0) err= 'unable to register "smin_hit"'
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmintarray('smin_combos',smin_combos,
     &     smax_num_chambers,0)
      IF(ierr.NE.0) err= 'unable to register "smin_combos"'
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmrealarray('sspace_point_criterion',
     &     sspace_point_criterion,
     &     smax_num_chambers,0)
      IF(ierr.NE.0) err= 'unable to register "sspace_point_criterion"'
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmreal('sdrift_velocity',sdrift_velocity,0)
      IF(ierr.NE.0) err= 'unable to register "sdrift_velocity"'
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmint('sluno',sluno,0)
      IF(ierr.NE.0) err= 'unable to register "sluno"'
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmint('sdebugflagpsi',sdebugflagpsi,0)
      IF(ierr.NE.0) err= 'unable to register "sdebugflagpsi"'
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmint('sdebugflaggeometry',sdebugflaggeometry,0)
      IF(ierr.NE.0) err= 'unable to register "sdebugflaggeometry"'
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmint('sdebugflagpr',sdebugflagpr,0)
      IF(ierr.NE.0) err= 'unable to register "sdebugflagppr"'
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmint('sdebugflagstubs',sdebugflagstubs,0)
      IF(ierr.NE.0) err= 'unable to register "sdebugflagstubs"'
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmint('sdebuglinkstubs',sdebuglinkstubs,0)
      IF(ierr.NE.0) err= 'unable to register "sdebuglinkstubs"'
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmint('sdebugtrackprint',sdebugtrackprint,0)
      IF(ierr.NE.0) err= 'unable to register "sdebugtrackprint"'
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmint('sdebugstubchisq',sdebugstubchisq,0)
      IF(ierr.NE.0) err= 'unable to register "sdebugstubchisq"'
      ABORT= ierr.ne.0 .or. ABORT
      ierr= regparmint('sdebugtartrackprint',sdebugtartrackprint,0)
      IF(ierr.NE.0) err= 'unable to register "sdebugtartrackprint"'
      ABORT= ierr.ne.0 .or. ABORT
*
      IF(ABORT) THEN
         call G_add_path(here,err)
      ENDIF
*
      RETURN
      END
