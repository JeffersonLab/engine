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
*-    Revision 1.1  1994/02/04 17:12:16  cdaq
*-    Initial revision
*-
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
       INTEGER ierr,flag,success
*
*--------------------------------------------------------
       INTEGER ierr,flag,success
       ierr= regparmint('HMAX_DC_HITS',HMAX_DC_HITS,flag)
       IF(ierr.NE.0) err= 'unable to register "HMAX_DC_HITS"'
       ABORT= ierr.NE.0

       ierr= regparmint('HMAX_DC_HITS',HMAX_DC_HITS,flag)
       ABORT= ierr.NE.0 .or. ABORT
*

       ierr= regparmint('HNUM_DC_PLANES',HNUM_DC_PLANES,flag)
*
       ierr= regparmint('HNUM_SCIN_PLANES',HNUM_SCIN_PLANES,flag)
       IF(ierr.NE.0) err= 'unable to register "HNUM_SCIN_PLANES"'
       ierr= regparmint('HMAX_SCIN_HITS',HMAX_SCIN_HITS,flag)
       ierr= regparmint('HMAX_CAL_BLOCKS',HMAX_CAL_BLOCKS,flag)
       IF(ierr.NE.0) err= 'unable to register "HMAX_CAL_BLOCKS"'
       ABORT= ierr.NE.0 .or. ABORT
       ierr= regparmint('HNUM_SCIN_PLANES',HNUM_SCIN_PLANES,flag)
       IF(ierr.NE.0) err= 'unable to register "HMAX_CER_HITS"'
       ABORT= ierr.NE.0 .or. ABORT
*
       ierr= regparmint('HMAX_CAL_BLOCKS',HMAX_CAL_BLOCKS,flag)
       ABORT= ierr.NE.0 .or. ABORT
*
       ierr= regparmint('HNTRACKHITS_MAX',HNTRACKHITS_MAX,flag)
       ierr= regparmint('HMAX_CER_HITS',HMAX_CER_HITS,flag)
*
       IF(ABORT) THEN
         call G_add_path(here,err)
       ierr= regparmint('HNTRACKS_MAX',HNTRACKS_MAX,flag)
       RETURN
       END

