       SUBROUTINE H_reconstruction(ABORT,err)
*--------------------------------------------------------
*-       Prototype C analysis routine
*-
*-
*-   Purpose and Methods : reconstruction of HMS quantities 
*-
*-   Output: ABORT              - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created  8-Nov-1993   Kevin B. Beard, HU
*-   Modified 20-Nov-1993   KBB for new errors
*-    $Log$
*-    Revision 1.2  1994/02/04 20:49:31  cdaq
*-    Print out some raw hit data
*-
c Revision 1.1  1994/02/04  20:47:59  cdaq
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
       character*16 here
       parameter (here= 'H_reconstruction')
*
       logical ABORT
       character*(*) err
*
       INCLUDE 'gen_data_structures.cmn'
       INCLUDE 'gen_constants.par'
       INCLUDE 'gen_units.par'
*
       integer i
*
*--------------------------------------------------------
*
       ABORT= .FALSE.
       err= here//':not yet written!'     !warning
*
       TYPE *,'  HDC_RAW_TOT_HITS=',HDC_RAW_TOT_HITS
       Do i=1,HDC_RAW_TOT_HITS
          type *,HDC_RAW_PLANE_NUM(i),HDC_RAW_WIRE_NUM(i),
     &         HDC_RAW_TDC(i)
       EndDo       

       IF(ABORT) call G_add_path(here,err)
*
       RETURN
       END
