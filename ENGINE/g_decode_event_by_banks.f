      subroutine g_decode_event_by_banks(ABORT, err)
*-----------------------------------------------------------------------
*-     Purpose and Methods: Pull out individual Fastbus banks from event
*-                          for subsequent decoding from commoned CRAW
*-
*-     Find the beginning of each ROC bank and send it off to 
*-    "g_decode_fb_bank".
*-
*-     Inputs:
*-         bank       Pointer to the first word (length) of a data bank.
*-
*-     Outputs:
*-        ABORT       success or failure
*-        err         explanation for failure
*-
*-     Created   3-Dec-1993   Kevin Beard, Hampton U.
*-    $Log$
*-    Revision 1.1  1994/02/01 20:38:58  cdaq
*-    Initial revision
*-
c Revision 1.1  1994/02/01  18:36:51  cdaq
c New version from Kevin Beard
c
*-----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*30 here
      parameter (here= 'g_decode_event_by_banks')
*
      logical ABORT
      character*(*) err
*
      include 'gen_data_structures.cmn'
      include 'fbgen.cmn'
*
      integer mth,begin_ROCbank,N_ROCbanks_fnd
      integer tab(FBGEN_MAX_TABLE)        !plenty large enough for MAX# ROCs
      logical WARN
*
*-----------------------------------------------------------------------
*
      call FBgen_build_table(CRAW,tab,ABORT,err)
      IF(ABORT) THEN
        call G_add_path(here,err)
        RETURN
      ENDIF
*
      N_ROCbanks_fnd= tab(FBGEN_NROCs)
*
      DO mth= 1,N_ROCbanks_fnd
	begin_ROCbank= tab(FBGEN_ROCs_start+mth-1)
        If(begin_ROCbank.LE.2 .or. begin_ROCbank.GE.CRAW(1)) Then
          err= here//':bad start of ROC bank- corrupted table'
          RETURN
        EndIf
*
        call g_decode_fb_bank(ABORT, err, CRAW(begin_ROCbank) )
*
        If(ABORT) Then
          call G_add_path(here,err)
          RETURN
        EndIf
      ENDDO
*
      WARN= N_ROCbanks_fnd.LT.1
      IF(WARN) THEN
        err= ':event contained no ROC banks'
        call G_add_path(here,err)
      ENDIF
*
      RETURN
      END


