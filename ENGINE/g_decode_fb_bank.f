      subroutine g_decode_fb_bank(bank, ABORT, error)
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
* $Log$
* Revision 1.16  1995/05/22 20:50:45  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.15  1995/05/22  13:35:40  cdaq
* (SAW) Fix up some problems with decoding of parallel link wrappers around
* fastbus events.  Still doesn't hadle two fb rocs wrapped into one bank.
*
* Revision 1.14  1995/05/11  17:17:00  cdaq
* (SAW) Extend || link hack for SOS.  Add Aerogel detector.
*
* Revision 1.13  1995/04/01  19:44:50  cdaq
* (SAW) Add BPM hitlist
*
* Revision 1.12  1995/01/27  20:12:48  cdaq
* (SAW) Add hacks to deal with parallel link data.  Pass lastslot variable to
*       g_decode_fb_detector so it can find 1881M/1877 headers.
*
* Revision 1.11  1994/11/22  20:13:02  cdaq
* (SPB) Update array names for raw SOS Scintillator bank
*
* Revision 1.10  1994/06/28  20:01:23  cdaq
* (SAW) Change arrays that HMS scintillators go into
*
* Revision 1.9  1994/06/18  02:45:49  cdaq
* (SAW) Add code for miscleaneous data and uninstrumented channels
*
* Revision 1.8  1994/06/09  04:48:28  cdaq
* (SAW) Fix length argument on gmc_mc_decode call again
*
* Revision 1.7  1994/04/13  18:49:10  cdaq
* (KBB Fix length argument on gmc_mc_deocde call
*
*
      implicit none
      SAVE
*
      character*16 here
      parameter (here='g_decode_fb_bank')
*
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
      include 'gen_detectorids.par'
      include 'gen_data_structures.cmn'
      include 'hms_data_structures.cmn'
      include 'sos_data_structures.cmn'
      include 'gen_decode_common.cmn'
      include 'mc_structures.cmn'

      integer*4 pointer                         ! Pointer FB data word
      integer*4 banklength,maxwords
      integer*4 roc,subadd,slot,lastslot
      integer*4 stat_roc
      integer*4 did                             ! Detector ID
      integer*4 g_decode_getdid                 ! Get detector ID routine
      integer*4 g_decode_fb_detector            ! Detector unpacking routine
      integer*4 i

      banklength = bank(1) + 1                  ! Bank length including count

      stat_roc = ishft(bank(2),-16)
      roc = iand(stat_roc,'1F'X)                ! Get ROC from header
*
*     First look for special Monte Carlo Banks
*
      if(stat_roc.eq.mc_status_and_ROC) then
        call gmc_mc_decode(banklength-2,bank(3),ABORT,error)
        if(ABORT) then
          call g_add_path(here,error)
        endif
        return
      endif
*
      if(roc.ge.G_DECODE_MAXROCS.and.roc.ne.9) then
        ABORT = .false.                 ! Just warn
        error = ':ROC out of range'
        call g_add_path(here,error)
        return
      endif
*
      pointer = 3                               ! First word of bank
*      if(roc.ge.7.and.roc.le.9) then
*        write(6,'(7z9)') (bank(i),i=1,7)
*        write(6,'(7z9),/') (bank(i),i=8,14)
*      endif
      if (roc.eq.7 .or. roc.eq.8 .or. roc.eq.9) then
*
*     These 3 rocs are VME front ends for fastbus crates.  At present
*     we assume that each VME front end is only taking data from one
*     FB roc and that this FB roc # is in 4 word of the bank.  This
*     hack will not work when we have roc 8 taking data from both
*     fbch1 and fbch2.  But it should work for runs up through
*     at least 5/31/95.
*
        pointer=pointer+2               !using parallel link, so next
        stat_roc = ishft(bank(pointer-1),-16)!2 words are fb roc header.
        roc = iand(stat_roc,'1F'X)
*        print *,iand(ishft(bank(2),-16),'1F'X),'->',roc
      endif
c      if (roc.eq.7) then                ! Change || link ROC #'s into ROC #'s
c        roc = 2                         ! used by the FB crates
c      else if(roc.eq.8) then            ! so that only one map file is
c        roc = 1                         ! needed.  (SAW 12/11/94)
c      else if(roc.eq.9) then            ! FBSOS
c        roc = 4
c      endif
      lastslot = -1
      do while (pointer .le. banklength)

        slot = iand(ishft(bank(pointer),-27),'1F'X)
        if(slot.gt.0.and.slot.le.G_DECODE_MAXSLOTS) then
          subadd = iand(ishft(bank(pointer),
     $         -g_decode_subaddbit(roc+1,slot)),'7F'X)
***         subadd = iand(ishft(bank(pointer),-17),'7F'X)
***         subadd = iand(ishft(bank(pointer),-16),'7F'X)

          if (subadd .lt. '7F'X) then   ! Only valid subaddress
                                        ! This skips module headers

            did = g_decode_getdid(roc,slot,subadd) ! Map into detector ID
            maxwords = banklength - pointer + 1
*
*        1         2         3         4         5         6         7
*23456789012345678901234567890123456789012345678901234567890123456789012
*
            if(did.eq.HDC_ID) then
              pointer = pointer +
     $             g_decode_fb_detector(lastslot, roc, bank(pointer), 
     &             maxwords, did, 
     $             HMAX_DC_HITS, HDC_RAW_TOT_HITS, HDC_RAW_PLANE_NUM,
     $             HDC_RAW_WIRE_NUM,1 ,HDC_RAW_TDC,0, 0, 0)

            else if (did.eq.HSCIN_ID) then
              pointer = pointer +
     $             g_decode_fb_detector(lastslot, roc, bank(pointer), 
     &             maxwords, did, 
     $             HMAX_ALL_SCIN_HITS, HSCIN_ALL_TOT_HITS, 
     $             HSCIN_ALL_PLANE_NUM, HSCIN_ALL_COUNTER_NUM, 4,
     $             HSCIN_ALL_ADC_POS, HSCIN_ALL_ADC_NEG,
     $             HSCIN_ALL_TDC_POS, HSCIN_ALL_TDC_NEG)

            else if (did.eq.HCAL_ID) then
              pointer = pointer +
     $             g_decode_fb_detector(lastslot, roc, bank(pointer), 
     &             maxwords, did,
     $             HMAX_CAL_BLOCKS, HCAL_TOT_HITS, HCAL_COLUMN,
     $             HCAL_ROW, 1, HCAL_ADC, 0, 0, 0)

            else if (did.eq.HCER_ID) then
*
*     Cerenkov has no plane array.  Pass it HCER_COR_ADC.  Unpacker will
*     fill it with zeros or ones.  (Or whatever we tell the unpacker the
*     plane number is.)
*     
              pointer = pointer +
     $             g_decode_fb_detector(lastslot, roc, bank(pointer), 
     &             maxwords, did, 
     $             HMAX_CER_HITS, HCER_TOT_HITS, HCER_PLANE,
     $             HCER_TUBE_NUM, 1, HCER_ADC, 0, 0, 0)

            else if (did.eq.HMISC_ID) then
*
*     This array is for data words that don't belong to a specific
*     detector counter.  Things like energy sums, and TDC's from various
*     points in the logic will go here.  Most likely we will set ADDR1
*     always to 1, and ADDR2 will start at 1.
*     
              pointer = pointer +
     $             g_decode_fb_detector(lastslot, roc, bank(pointer), 
     &             maxwords, did, 
     $             HMAX_MISC_HITS, HMISC_TOT_HITS, HMISC_RAW_ADDR1,
     $             HMISC_RAW_ADDR2, 1, HMISC_RAW_DATA, 0, 0, 0)

*
*        1         2         3         4         5         6         7
*23456789012345678901234567890123456789012345678901234567890123456789012
*
            else if(did.eq.SDC_ID) then
              pointer = pointer +
     $             g_decode_fb_detector(lastslot, roc, bank(pointer), 
     &             maxwords, did,
     $             SMAX_DC_HITS, SDC_RAW_TOT_HITS, SDC_RAW_PLANE_NUM,
     $             SDC_RAW_WIRE_NUM,1 ,SDC_RAW_TDC,0, 0, 0)

            else if (did.eq.SSCIN_ID) then
              pointer = pointer +
     $             g_decode_fb_detector(lastslot, roc, bank(pointer), 
     &             maxwords, did,
     $             SMAX_ALL_SCIN_HITS, SSCIN_ALL_TOT_HITS,
     $             SSCIN_ALL_PLANE_NUM, SSCIN_ALL_COUNTER_NUM, 4,
     $             SSCIN_ALL_ADC_POS, SSCIN_ALL_ADC_NEG,
     $             SSCIN_ALL_TDC_POS, SSCIN_ALL_TDC_NEG)

            else if (did.eq.SCAL_ID) then
              pointer = pointer +
     $             g_decode_fb_detector(lastslot, roc, bank(pointer), 
     &             maxwords, did, 
     $             SMAX_CAL_BLOCKS, SCAL_TOT_HITS, SCAL_COLUMN, 
     $             SCAL_ROW, 1, SCAL_ADC, 0, 0, 0)

            else if (did.eq.SCER_ID) then
*
              pointer = pointer +
     $             g_decode_fb_detector(lastslot, roc, bank(pointer), 
     &             maxwords, did,
     $             SMAX_CER_HITS, SCER_TOT_HITS, SCER_PLANE,
     $             SCER_TUBE_NUM, 1, SCER_ADC, 0, 0, 0)

            else if (did.eq.SAER_ID) then
*
*     Aerogel has two tubes for each "counter".  Since there are no
*     TDC's, we will tell the decoder that we have 4 signals, but pass
*     a dummy array for the 3rd and 4th signal.
*
*     SAER_PLANE is a dummy array.
*     
              pointer = pointer +
     $             g_decode_fb_detector(lastslot, roc, bank(pointer), 
     &             maxwords, did,
     $             SMAX_AER_HITS, SAER_TOT_HITS, SAER_PLANE,
     $             SAER_PAIR_NUM, 4, SAER_ADC_LEFT, SAER_ADC_RIGHT,
     $             SAER_DUMMY, SAER_DUMMY)

            else if (did.eq.SMISC_ID) then
*
*     This array is for data words that don't belong to a specific
*     detector counter.  Things like energy sums, and TDC's from various
*     points in the logic will go here.  Most likely we will set ADDR1
*     always to 1, and ADDR2 will start at 1.
*     
              pointer = pointer +
     $             g_decode_fb_detector(lastslot, roc, bank(pointer), 
     &             maxwords, did, 
     $             SMAX_MISC_HITS, SMISC_TOT_HITS, SMISC_RAW_ADDR1,
     $             SMISC_RAW_ADDR2, 1, SMISC_RAW_DATA, 0, 0, 0)

*
*     BPM/Raster ADC values. 
*
            else if (did.eq.CBPM_ID) then
              pointer = pointer +
     $             g_decode_fb_detector(lastslot, roc, bank(pointer), 
     &             maxwords, did,
     $             CMAX_BPM_HITS, CBPM_TOT_HITS, CBPM_DEVICE,
     $             CBPM_ADCNUM, 1, CBPM_ADCVAL, 0, 0, 0)
              
*
*     Data from Uninstrumented channels and slots go into a special array
*
            else if (did.eq.UNINST_ID) then
              pointer = pointer +
     $             g_decode_fb_detector(lastslot, roc, bank(pointer), 
     &             maxwords, did, 
     $             GMAX_UNINST_HITS, GUNINST_TOT_HITS, GUNINST_RAW_ROCSLOT,
     $             GUNINST_RAW_SUBADD, 1, GUNINST_RAW_DATAWORD, 0, 0, 0)

            else
*     Should never get here.  Unknown detector ID's or did=-1 for bad ROC#
*     or SLOT# will come here.
*
              print *,"BAD DID, unknown ROC,SLOT",roc,slot
              pointer = pointer + 1     ! Skip unknown detector id's
            endif
          else
            lastslot = slot
            pointer = pointer + 1       ! Skip Bad subaddresses (module header)
          endif
*
        else
          pointer = pointer + 1         ! Skip bad slots
        endif
*
      enddo
      ABORT= .FALSE.
      error= ' '
      return
      end
**************
*     Local Variables:
*     mode: fortran
*     fortran-if-indent: 2
*     fortran-do-indent: 2
*     End:
