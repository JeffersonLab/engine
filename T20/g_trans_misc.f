      subroutine g_trans_misc(abort,errmsg)
*-------------------------------------------------------------------
* author: John Arrington
* created: 1/16/96
*
* Modified for t20 on 1997/0321 by Glenn Collins
*
* g_trans_misc fills the gen_decoded_misc common block
*
* $Log$
* Revision 1.1  1998/12/01 21:00:45  saw
* Initial revision
*
* Revision 1.1  1996/01/22 15:14:10  saw
* Initial revision
*
*--------------------------------------------------------

      implicit none

      include 'gen_data_structures.cmn'
      include 'gen_misc.cmn'
      logical abort
      character*1024 errmsg
      character*20 here
      parameter (here = 'g_trans_misc')

      integer*4 ihit

      save

      abort = .false.
      errmsg = ' '

      do ihit = 1 , gmax_misc_hits
        gmisc_dec_data(ihit,1) = 0      ! Clear HRTDCs
        gmisc_dec_data(ihit,2) = -1     ! Clear ADCs
**later        gmisc_dec_data(ihit,3) = 0      ! Clear MHTDCs
      enddo
C
C*** Note for clarity, the first number in the gmisc_dec_data array is the
C    "signal" number (addr2), while the second is the LeCroy unit type (addr1)
C        For GMISC events, the module types have the following addr1
C           1872A HRTDC: 1
C           1881M ADC:   2
C           1877  MHTDC: 3
C
      do ihit = 1 , gmisc_tot_hits
         gmisc_dec_data(gmisc_raw_addr2(ihit),gmisc_raw_addr1(ihit)) =
     $       gmisc_raw_data(ihit)
C*** Note:  Possibly want to fill user filled histograms here in the loop. for mh-tdc
      enddo
c
c         c_hrtdc_s22 =  gmisc_dec_data(10,1)
c         c_hrtdc_tts1 = gmisc_dec_data(64,1)
c         c_hrtdc_hms = gmisc_dec_data(1,1)
**later         c_mhtdc_s22 = gmisc_dec_data(1,3)
**later         c_mhtdc_tts1 = gmisc_dec_data(2,3)
**later         c_mhtdc_hms = gmisc_dec_data(16,3)

      return
      end
