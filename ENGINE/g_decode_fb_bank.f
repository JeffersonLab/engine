      subroutine g_decode_fb_bank(ABORT, error, bank)
*
*     Purpose and Methods: Decode a Fastbus bank.
*
*     Looks at detector ID for a word in a data bank and passes the
*     appopriate data structure pointers to the g_decode_fb_detector routine.
*     That routine will return when it gets to another detector in which
*     case the the present routine will dispatch g_decode_fb_detector with a
*     new set of pointers.
*
*     This routine must be modified when new detectors are added.  It may
*     also may need to modified if fastbus modules other than from LeCroy
*     are used.
*
*     It is the responsibility of the calling routine to call
*     g_decode_fb_bank only for banks of fastbus data.
*
*     Inputs:
*
*     bank       Pointer to the first word (length) of a data bank.
*
*     Outputs:
*
*     ABORT
*     error
*
*     Created  16-NOV-1993   Stephen Wood, CEBAF
*     Modified  3-Dec-1993   Kevin Beard, Hampton U.
*
      implicit none
      SAVE
      logical ABORT
      character*(*) error
      integer*4 bank(*)

*     This routine unpacks a ROC bank.  It looks a fastbus word to
*     determine which detector it belongs to.  It then passes the
*     appropriate arrays for that detector to detector independent unpacker
*     G_DECODE_FB_DETECTOR which will unpack words from the bank into the
*     hit arrays until the detector changes or it runs out of data.
*     G_DECODE_FB_DETECTOR returns a pointer to the next data word to be
*     processed.
*
      include 'gen_detectorids.cmn'
      include 'gen_data_structures.cmn'

      integer*4 pointer                         ! Pointer FB data word
      integer*4 banklength,maxwords
      integer*4 roc,subadd,slot
      integer*4 did                             ! Detector ID
      integer*4 g_decode_getdid                 ! Get detector ID routine
      integer*4 g_decode_fb_detector            ! Detector unpacking routine

      banklength = bank(1) + 1                  ! Bank length including count

      roc = iand(ishft(bank(2),-16),'1F'X)      ! Get ROC from header

      pointer = 3                               ! First word of bank

      do while (pointer .le. banklength)

         subadd = iand(ishft(bank(pointer),-17),'7F'X)

         if (subadd .lt. '7F'X) then            ! Only valid slots

            slot = iand(ishft(bank(pointer),-27),'1F'X)
            did = g_decode_getdid(roc,slot,subadd)       ! Map into detector ID
            maxwords = banklength - pointer + 1
*
*        1         2         3         4         5         6         7
*23456789012345678901234567890123456789012345678901234567890123456789012
*
            if(did.eq.HDC_ID) then
               pointer = pointer +
     $              g_decode_fb_detector(roc, bank(pointer), 
     &              maxwords, did, 
     $              HMAX_DC_HITS, HDC_RAW_TOT_HITS, HDC_RAW_PLANE_NUM,
     $              HDC_RAW_WIRE_NUM,1 ,HDC_RAW_TDC,0, 0, 0)

            else if (did.eq.HSCIN_ID) then
               pointer = pointer +
     $              g_decode_fb_detector(roc, bank(pointer), 
     &              maxwords, did, 
     $              HMAX_SCIN_HITS, HSCIN_TOT_HITS, HSCIN_PLANE_NUM,
     $              HSCIN_COUNTER_NUM, 4, HSCIN_ADC_POS, HSCIN_ADC_NEG,
     $              HSCIN_TDC_POS, HSCIN_TDC_NEG)

            else if (did.eq.HCAL_ID) then
               pointer = pointer +
     $              g_decode_fb_detector(roc, bank(pointer), 
     &              maxwords, did,  HMAX_CAL_BLOCKS,
     $              HCAL_TOT_HITS, HCAL_COLUMN, HCAL_ROW,
     $              1,HCAL_ADC, 0, 0, 0)

            else if (did.eq.HCER_ID) then
*
*     Cerenkov has no plane array.  Pass it HCER_COR_ADC.  Unpacker will
*     fill it with zeros or ones.  (Or whatever we tell the unpacker the
*     plane number is.)
*     
               pointer = pointer +
     $              g_decode_fb_detector(roc, bank(pointer), 
     &              maxwords, did, 
     $              HMAX_CER_HITS, HCER_TOT_HITS, HCER_PLANE,
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
     $              SMAX_CER_HITS, SCER_TOT_HITS, SCER_PLANE,
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
