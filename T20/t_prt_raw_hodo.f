      SUBROUTINE  t_prt_raw_hodo(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Dump POLDER_RAW_HODO BANKS
*-
*-      Required Input BANKS     POLDER_RAW_HODO
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 25-JAN-1997   S. A. Wood
* $Log$
* Revision 1.1  1998/12/01 20:57:05  saw
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*14 here
      parameter (here= 't_prt_raw_hodo')
*
      logical ABORT
      character*(*) err
*
      integer*4 j
      include 't20_data_structures.cmn'
      include 'gen_constants.par'
      include 'gen_units.par'
      include 't20_tracking.cmn'
      include 't20_hodo_parms.cmn'
*
*--------------------------------------------------------
      ABORT = .FALSE.
      err = ' '
      write(tluno,'(''        POLDER_RAW_HODO BANKS'')')
      write(tluno,'(''     THODO_TOT_HITS='',I4)') THODO_TOT_HITS
      if(THODO_TOT_HITS.GT.0) then
        write(tluno,'('' Num  Plane    Bar          TDC'')')
        write(tluno,'(1x,i2,2x,i3,7x,i4,8x,i10)')
     &       (j,THODO_PLANE_NUM(j),THODO_BAR_NUM(j),
     $       THODO_TDC_VAL(j),j=1,THODO_TOT_HITS )
      endif
      RETURN
      END
