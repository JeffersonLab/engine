      subroutine h_trans_misc(abort,errmsg)
*-------------------------------------------------------------------
* author: John Arrington
* created: 4/8/95
*
* h_trans_misc fills the hms_decoded_misc common block
*
* $Log$
* Revision 1.7  1999/01/27 16:02:39  saw
* Check if some hists are defined before filling
*
* Revision 1.6  1996/09/04 14:24:13  saw
* (JRA) Add misc. tdc's
*
* Revision 1.5  1996/01/24 16:00:04  saw
* (JRA) Replace 48 with hmax_misc_hits
*
* Revision 1.4  1996/01/16 21:36:43  cdaq
* (JRA) Misc. fixes.
*
* Revision 1.3  1995/07/20 14:26:00  cdaq
* (JRA) Add second index (TDC/ADC) to hmisc_dec_data
*
* Revision 1.2  1995/05/22  19:39:32  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1995/04/12  03:59:32  cdaq
* Initial revision
*
*
*--------------------------------------------------------

      implicit none

      include 'hms_data_structures.cmn'
      include 'hms_scin_parms.cmn'
      include 'hms_id_histid.cmn'

      logical abort
      character*1024 errmsg
      character*20 here
      parameter (here = 'h_trans_misc')

      integer*4 ihit,ich,isig

      save

      do ihit = 1 , hmax_misc_hits
        hmisc_dec_data(ihit,1) = 0     ! Clear TDC's
        hmisc_dec_data(ihit,2) = -1     ! Clear ADC's
      enddo
      
      do ihit = 1 , hmisc_tot_hits
        ich=hmisc_raw_addr2(ihit)
        isig=hmisc_raw_addr1(ihit)
        hmisc_dec_data(ich,isig) = hmisc_raw_data(ihit)
        hmisc_scaler(ich,isig) = hmisc_scaler(ich,isig) + 1
        if (isig.eq.1.and.hidmisctdcs.gt.0) then        !TDC
          call hf1(hidmisctdcs,float(hmisc_dec_data(ich,isig)),1.)
        endif
      enddo

      return
      end
