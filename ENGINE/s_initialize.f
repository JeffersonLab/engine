       SUBROUTINE S_initialize(ABORT,err)
*--------------------------------------------------------
*-       Prototype C analysis routine
*-
*-
*-   Purpose and Methods : Initializes HMS quantities 
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created  8-Nov-1993   Kevin B. Beard
*-   Modified 20-Nov-1993  KBB for new errors
*-    $Log$
*-    Revision 1.2  1994/02/03 14:28:27  cdaq
*-    Make clear that last arg of reg calls is a title.  Use null for now.
*-
c Revision 1.1  1994/02/02  21:37:55  cdaq
c Initial revision
c
*-
*- All standards are from "Proposal for Hall C Analysis Software
*- Vade Mecum, Draft 1.0" by D.F.Geesamn and S.Wood, 7 May 1993
*-
*--------------------------------------------------------
       IMPLICIT NONE
       SAVE
*
       character*12 here
       parameter (here= 'S_initialize')
*
       logical ABORT
       character*(*) err
*
       INCLUDE 'gen_data_structures.cmn'
       INCLUDE 'gen_routines.dec'
*
       INTEGER ierr,nulltitle
*
*--------------------------------------------------------
       err= ' '
*
       nulltitle = 0
       ierr= regparmint('SMAX_DC_HITS',SMAX_DC_HITS,nulltitle)
       IF(ierr.NE.0) err= 'unable to register "SMAX_DC_HITS"'
       ABORT= ierr.NE.0
*
       ierr= regparmint('SNUM_DC_PLANES',SNUM_DC_PLANES,nulltitle)
       IF(ierr.NE.0) err= 'unable to register "SNUM_DC_PLANES"'
       ABORT= ierr.NE.0 .or. ABORT
*
       ierr= regparmint('SMAX_SCIN_HITS',SMAX_SCIN_HITS,nulltitle)
       IF(ierr.NE.0) err= 'unable to register "SMAX_SCIN_HITS"'
       ABORT= ierr.NE.0 .or. ABORT
*
       ierr= regparmint('SNUM_SCIN_PLANES',SNUM_SCIN_PLANES,nulltitle)
       IF(ierr.NE.0) err= 'unable to register "SNUM_SCIN_PLANES"'
       ABORT= ierr.NE.0 .or. ABORT
*
       ierr= regparmint('SMAX_CAL_BLOCKS',SMAX_CAL_BLOCKS,nulltitle)
       IF(ierr.NE.0) err= 'unable to register "SMAX_CAL_BLOCKS"'
       ABORT= ierr.NE.0 .or. ABORT
*
       ierr= regparmint('SMAX_CER_HITS',SMAX_CER_HITS,nulltitle)
       IF(ierr.NE.0) err= 'unable to register "SMAX_CER_HITS"'
       ABORT= ierr.NE.0 .or. ABORT
*
       ierr= regparmint('SNTRACKS_MAX',SNTRACKS_MAX,nulltitle)
       IF(ierr.NE.0) err= 'unable to register "SNTRACKS_MAX"'
       ABORT= ierr.NE.0 .or. ABORT
*
       ierr= regparmint('SNTRACKHITS_MAX',SNTRACKHITS_MAX,nulltitle)
       IF(ierr.NE.0) err= 'unable to register "SNTRACKHITS_MAX"'
       ABORT= ierr.NE.0 .or. ABORT
*
       IF(ABORT) THEN
         call G_add_path(here,err)
       ENDIF
*
       RETURN
       END
