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
* Revision 1.2  1994/02/22 18:52:06  cdaq
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
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_routines.dec'
      INCLUDE 'hms_tracking.cmn'
      INCLUDE 'hms_geometry.cmn'
*
      INTEGER ierr
*
*--------------------------------------------------------
      err= ' '
      abort = .false.
*
*
*       ierr= regparmreal('raddeg',raddeg,0)
*       IF(ierr.NE.0) err= 'unable to register "raddeg"'
*       ABORT= ierr.EQ.0 .or. ABORT
      ierr= regparmint('hdc_num_planes',hdc_num_planes,0)
      IF(ierr.NE.0) err= 'unable to register "hdc_num_planes"'
      ABORT= ierr.EQ.0 .or. ABORT
      ierr= regparmint('hdc_num_chambers',hdc_num_chambers,0)
      IF(ierr.NE.0) err= 'unable to register "hdc_num_chambers"'
      ABORT= ierr.EQ.0 .or. ABORT
      ierr= regparmrealarray('hdc_zpos',hdc_zpos,HMAX_NUM_DC_PLANES,0)
      IF(ierr.NE.0) err= 'unable to register "hdc_zpos"'
      ABORT= ierr.EQ.0 .or. ABORT
      ierr= regparmrealarray('hdc_alpha_angle',hdc_alpha_angle,
     &     HMAX_NUM_DC_PLANES,0)
      IF(ierr.NE.0) err= 'unable to register "hdc_alpha_angle"'
      ABORT= ierr.EQ.0 .or. ABORT
      ierr= regparmrealarray('hdc_beta_angle',hdc_beta_angle,
     &     HMAX_NUM_DC_PLANES,0)
      IF(ierr.NE.0) err= 'unable to register "hdc_beta_angle"'
      ABORT= ierr.EQ.0 .or. ABORT
      ierr= regparmrealarray('hdc_gamma_angle',hdc_gamma_angle,
     &     HMAX_NUM_DC_PLANES,0)
      IF(ierr.NE.0) err= 'unable to register "hdc_gamma_angle"'
      ABORT= ierr.EQ.0 .or. ABORT
      ierr= regparmrealarray('hdc_sigma',hdc_sigma,
     &     HMAX_NUM_DC_PLANES,0)
      IF(ierr.NE.0) err= 'unable to register "hdc_sigma"'
      ierr= regparmrealarray('hdc_pitch',hdc_pitch,
     &     HMAX_NUM_DC_PLANES,0)
      IF(ierr.NE.0) err= 'unable to register "hdc_pitch"'
      ABORT= ierr.EQ.0 .or. ABORT
      ierr= regparmrealarray('hdc_nrwire',hdc_nrwire,
     &     HMAX_NUM_DC_PLANES,0)
      IF(ierr.NE.0) err= 'unable to register "hdc_nrwire"'
      ABORT= ierr.EQ.0 .or. ABORT
      ierr= regparmrealarray('hdc_central_wire',hdc_central_wire,
     &     HMAX_NUM_DC_PLANES,0)
      IF(ierr.NE.0) err= 'unable to register "hdc_central_wire"'
      ABORT= ierr.EQ.0 .or. ABORT
      ierr= regparmrealarray('hdc_xcenter',hdc_xcenter,
     &     HMAX_NUM_DC_PLANES,0)
      IF(ierr.NE.0) err= 'unable to register "hdc_xcenter"'
      ABORT= ierr.EQ.0 .or. ABORT
      ierr= regparmrealarray('hdc_ycenter',hdc_ycenter,
     &     HMAX_NUM_DC_PLANES,0)
      IF(ierr.NE.0) err= 'unable to register "hdc_ycenter"'
      ABORT= ierr.EQ.0 .or. ABORT
      ierr= regparmintarray('hdc_chamber_planes',hdc_chamber_planes,
     &     HMAX_NUM_DC_PLANES,0)
      IF(ierr.NE.0) err= 'unable to register "hdc_chamber_planes"'
      ABORT= ierr.EQ.0 .or. ABORT
      ierr= regparmintarray('hdc_wire_counting',hdc_wire_counting,
     &     HMAX_NUM_DC_PLANES,0)
      IF(ierr.NE.0) err= 'unable to register "hdc_wire_counting"'
      ABORT= ierr.EQ.0 .or. ABORT
      ierr= regparmreal('hdrift_velocity',hdrift_velocity,
     &     0)
      IF(ierr.NE.0) err= 'unable to register "hdrift_velocity"'
      ABORT= ierr.EQ.0 .or. ABORT
      ierr= regparmreal('hxt_track_criterion',hxt_track_criterion,
     &     0)
      IF(ierr.NE.0) err= 'unable to register "hxt_track_criterion"'
      ABORT= ierr.EQ.0 .or. ABORT
      ierr= regparmreal('hyt_track_criterion',hyt_track_criterion,
     &     0)
      IF(ierr.NE.0) err= 'unable to register "hyt_track_criterion"'
      ABORT= ierr.EQ.0 .or. ABORT
      ierr= regparmreal('hxpt_track_criterion',hxpt_track_criterion,
     &     0)
      IF(ierr.NE.0) err= 'unable to register "hxpt_track_criterion"'
      ABORT= ierr.EQ.0 .or. ABORT
      ierr= regparmreal('hypt_track_criterion',hypt_track_criterion,
     &     0)
      IF(ierr.NE.0) err= 'unable to register "hypt_track_criterion"'
      ABORT= ierr.EQ.0 .or. ABORT
      ierr= regparmintarray('hmin_hit',hmin_hit,hmax_num_chambers
     &     ,0)
      IF(ierr.NE.0) err= 'unable to register "hmin_hit"'
      ABORT= ierr.EQ.0 .or. ABORT
      ierr= regparmintarray('hmin_combos',hmin_combos,
     &     hmax_num_chambers,0)
      IF(ierr.NE.0) err= 'unable to register "hmin_combos"'
      ABORT= ierr.EQ.0 .or. ABORT
      ierr= regparmrealarray('hspace_point_criterion',
     &     hspace_point_criterion,
     &     hmax_num_chambers,0)
      IF(ierr.NE.0) err= 'unable to register "hspace_point_criterion"'
      ABORT= ierr.EQ.0 .or. ABORT
      ierr= regparmint('hluno',hluno,0)
      IF(ierr.NE.0) err= 'unable to register "hluno"'
      ABORT= ierr.EQ.0 .or. ABORT
      ierr= regparmint('hdebug0psi',hdebug0psi,0)
      IF(ierr.NE.0) err= 'unable to register "hdebug0psi"'
      ABORT= ierr.EQ.0 .or. ABORT
      ierr= regparmint('hdebug0geometry',hdebug0geometry,0)
      IF(ierr.NE.0) err= 'unable to register "hdebug0geometry"'
      ABORT= ierr.EQ.0 .or. ABORT
      ierr= regparmint('hdebug0pr',hdebug0pr,0)
      IF(ierr.NE.0) err= 'unable to register "hdebug0ppr"'
      ABORT= ierr.EQ.0 .or. ABORT
      ierr= regparmint('hdebug0stubs',hdebug0stubs,0)
      IF(ierr.NE.0) err= 'unable to register "hdebug0stubs"'
      ABORT= ierr.EQ.0 .or. ABORT
      ierr= regparmint('hdebuglinkstubs',hdebuglinkstubs,0)
      IF(ierr.NE.0) err= 'unable to register "hdebuglinkstubs"'
      ABORT= ierr.EQ.0 .or. ABORT
      ierr= regparmint('hdebugtrackprint',hdebugtrackprint,0)
      IF(ierr.NE.0) err= 'unable to register "hdebugtrackprint"'
      ABORT= ierr.EQ.0 .or. ABORT
      ierr= regparmint('hdebugstubchisq',hdebugstubchisq,0)
      IF(ierr.NE.0) err= 'unable to register "hdebugstubchisq"'
      ABORT= ierr.EQ.0 .or. ABORT
      ierr= regparmint('hdebugtartrackprint',hdebugtartrackprint,0)
      IF(ierr.NE.0) err= 'unable to register "hdebugtartrackprint"'
      ABORT= ierr.EQ.0 .or. ABORT
*     
      IF(ABORT) THEN
         call G_add_path(here,err)
      ENDIF
*
      RETURN
      END
