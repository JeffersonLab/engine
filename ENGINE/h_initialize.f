       SUBROUTINE H_initialize(ABORT,err)
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
*-   Modified 20-Nov-1993   KBB for new errors
*-    $Log$
*-    Revision 1.3  1994/02/04 17:35:56  cdaq
*-    KBB replaced flag with title
*-
*-
*- All standards are from "Proposal for Hall C Analysis Software
*- Vade Mecum, Draft 1.0" by D.F.Geesamn and S.Wood, 7 May 1993
*-
*--------------------------------------------------------
       IMPLICIT NONE
       SAVE
*
       character*12 here
       parameter (here= 'H_initialize')
*
       logical ABORT
       character*(*) err
*
       INCLUDE 'gen_data_structures.cmn'
       INCLUDE 'gen_routines.dec'
*
       INTEGER ierr,success
       CHARACTER*80 title
*
*--------------------------------------------------------
       err= ' '
*
       title= 'HMS drift chamber max. # hits'
       ierr= regparmint('HMAX_DC_HITS',HMAX_DC_HITS,title)
       IF(ierr.NE.0) err= 'unable to register "HMAX_DC_HITS"'
       ABORT= ierr.NE.0
*
       title= 'HMS drift chamber number of planes'
       ierr= regparmint('HNUM_DC_PLANES',HNUM_DC_PLANES,title)
       IF(ierr.NE.0) err= 'unable to register "HNUM_DC_PLANES"'
       ABORT= ierr.NE.0 .or. ABORT
*
       title= 'HMS scintillator max. # hits'
       ierr= regparmint('HMAX_SCIN_HITS',HMAX_SCIN_HITS,title)
       IF(ierr.NE.0) err= 'unable to register "HMAX_SCIN_HITS"'
       ABORT= ierr.NE.0 .or. ABORT
*
       title= 'HMS scintillator chamber number of planes'
       ierr= regparmint('HNUM_SCIN_PLANES',HNUM_SCIN_PLANES,title)
       IF(ierr.NE.0) err= 'unable to register "HNUM_SCIN_PLANES"'
       ABORT= ierr.NE.0 .or. ABORT
*
       title= 'HMS calorimeter number of blocks'
       ierr= regparmint('HMAX_CAL_BLOCKS',HMAX_CAL_BLOCKS,title)
       IF(ierr.NE.0) err= 'unable to register "HMAX_CAL_BLOCKS"'
       ABORT= ierr.NE.0 .or. ABORT
*
       title= 'HMS cerenkov chamber max. # of hits'
       ierr= regparmint('HMAX_CER_HITS',HMAX_CER_HITS,title)
       IF(ierr.NE.0) err= 'unable to register "HMAX_CER_HITS"'
       ABORT= ierr.NE.0 .or. ABORT
*
       title= 'HMS max. # of tracks'
       ierr= regparmint('HNTRACKS_MAX',HNTRACKS_MAX,title)
       IF(ierr.NE.0) err= 'unable to register "HNTRACKS_MAX"'
       ABORT= ierr.NE.0 .or. ABORT
*
       title= 'HMS max. # of track hits'
       ierr= regparmint('HNTRACKHITS_MAX',HNTRACKHITS_MAX,title)
       IF(ierr.NE.0) err= 'unable to register "HNTRACKHITS_MAX"'
       ABORT= ierr.NE.0 .or. ABORT
*
       IF(ABORT) THEN
         call G_add_path(here,err)
       ENDIF
*
       RETURN
       END
