      subroutine s_trans_misc(abort,errmsg)
*-------------------------------------------------------------------
* author: John Arrington
* created: 4/8/95
*
* s_trans_misc fills the sos_decoded_misc common block
*
* $Log$
* Revision 1.4  1996/01/24 16:08:38  saw
* (JRA) Replace 48 with smax_misc_hits
*
* Revision 1.3  1996/01/17 18:12:35  cdaq
* (JRA) Misc. fixes.
*
* Revision 1.2  1995/05/22 19:46:03  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1995/04/12  03:59:23  cdaq
* Initial revision
*
*
*--------------------------------------------------------

      implicit none

      include 'sos_data_structures.cmn'
      include 'sos_scin_parms.cmn'

      logical abort
      character*1024 errmsg
      character*20 here
      parameter (here = 's_trans_misc')

      integer*4 ihit

      save
      
      do ihit = 1 , smax_misc_hits
        smisc_dec_data(ihit,1) = 0     ! Clear TDC's
        smisc_dec_data(ihit,2) = -1     ! Clear ADC's
      enddo

      do ihit = 1 , smisc_tot_hits
        smisc_dec_data(smisc_raw_addr2(ihit),smisc_raw_addr1(ihit)) =
     $       smisc_raw_data(ihit)
      enddo

      return
      end
