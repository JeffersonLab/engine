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
*-    Revision 1.1  1994/02/01 20:39:47  cdaq
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


                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               _ADC,
     $              HCER_TUBE_NUM, 1, HCER_ADC, 0, 0, 0)

*
*        1         2         3         4         5         6         7
*23456789012345678901234567890123456789012345678901234567890123456789012
*
            else if(did.eq.SDC_ID) then
               pointer = pointer +
     $              g_decode_fb_detector(roc, bank(pointer), 
     &              maxwords, did,
     $              SMAX_DC_HITS, SDC_RAW_TOT_HITS, SDC_RAW_PLANE_NUM,
     $              SDC_RAW_WIRE_NUM,1 ,SDC_RAW_TDC,0, 0, 0)

            else if (did.eq.SSCIN_ID) then
               pointer = pointer +
     $              g_decode_fb_detector(roc, bank(pointer), 
     &              maxwords, did,
     $              SMAX_SCIN_HITS, SSCIN_TOT_HITS, SSCIN_PLANE_NUM,
     $              SSCIN_COUNTER_NUM, 4, SSCIN_ADC_POS, SSCIN_ADC_NEG
     $              ,SSCIN_TDC_POS, SSCIN_TDC_NEG)

            else if (did.eq.SCAL_ID) then
               pointer = pointer +
     $              g_decode_fb_detector(roc, bank(pointer), 
     &              maxwords, did, 
     $              SMAX_CAL_BLOCKS, SCAL_TOT_HITS, SCAL_COLUMN, 
     $              SCAL_ROW, 1,SCAL_ADC, 0, 0, 0)

            else if (did.eq.SCER_ID) then
*
*     Cerenkov has no plane array.  Pass it SCER_COR_ADC.  Unpacker will
*     fill it with zeros or ones.  (Or whatever we tell the unpacker the
*     plane number is.)
*     
               pointer = pointer +
     $              g_decode_fb_detector(roc, bank(pointer), 
     &              maxwords, did,
     $              SMAX_CER_HITS, SCER_TOT_HITS, SCER_COR_ADC,
     $              SCER_TUBE_NUM, 1, SCER_ADC, 0, 0, 0)

*
          else
              pointer = pointer + 1               ! Skip Module header words
          endif
*
        else
              pointer = pointer + 1               ! Skip bad slots
        endif
*
      enddo
      ABORT= .FALSE.
      error= ' '
      return
      end
