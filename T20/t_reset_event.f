      SUBROUTINE T_reset_event(ABORT,err)
*--------------------------------------------------------
*-       Prototype C analysis routine
*-
*-
*-   Purpose and Methods : Resets all T20 quantities at the beginning of the run
*-
*- 
*-   Output: ABORT		- success or failure
*-         : err	- reason for failure, if any
*- 
*-   Created  22-Jan-1997   Stephen A. Wood

*- All standards are from "Proposal for Hall C Analysis Software
*- Vade Mecum, Draft 1.0" by D.F.Geesamn and S.Wood, 7 May 1993
* $Log$
* Revision 1.1  1998/12/01 20:54:42  saw
* Initial revision
*
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*13 here
      parameter (here= 'T_reset_event')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 't20_data_structures.cmn'
      include 't20_misc.cmn'
      include 't20_hms.cmn'
       INCLUDE 'gen_data_structures.cmn'
       INCLUDE 'hms_data_structures.cmn'
c      include 'gen_misc.cmn'
      include 'hms_tracking.cmn'
c      include 'hms_pedestals.cmn'
c      include 'hms_scin_parms.cmn'
*
      INTEGER hit
*
*--------------------------------------------------------
*
      DO hit= 1,TMAX_MWPC_HITS
        TMWPC_RAW_PLANE_NUM(hit) = 0
        TMWPC_RAW_WIRE_NUM(hit) = 0
        TMWPC_RAW_TDC(hit) = 0
      ENDDO
      TMWPC_RAW_TOT_HITS = 0
*
      DO hit= 1,TMAX_HODO_HITS
        THODO_PLANE_NUM(hit) = 0
        THODO_BAR_NUM(hit) = 0
        THODO_TDC_VAL(hit) = 0
      ENDDO
      THODO_TOT_HITS = 0
*     
      DO hit= 1,TMAX_MISC_HITS
        TMISC_RAW_ADDR1(hit) = 0
        TMISC_RAW_ADDR2(hit) = 0
        TMISC_RAW_DATA(hit) = 0
      ENDDO
      TMISC_TOT_HITS = 0

      DO hit= 1,TTSTMAX_STRAW_HITS
        TTST_RAW_PLANE_NUM(hit) = 0
        TTST_RAW_GROUP_NUM(hit) = 0
        TTST_RAW_TDC(hit) = 0
      ENDDO
      TTST_RAW_TOT_HITS = 0

c     reset variables calculated in t_hms.f
      tsinhtheta = -2.
      te_v = 0.
      thms_td1 = 0.
      thms_td2 = 0.
      tq2 = 0.
c
c Note:  These don't really belong here (saw)
c
      hstheta = 0.
      hsenergy = 0.
      hfoundtrack = .false.
      hcleantrack = .false.
      hntracks_fp = 0

      ABORT= .FALSE.
      err= ' '
      RETURN
      END
