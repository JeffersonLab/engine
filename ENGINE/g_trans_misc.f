      subroutine g_trans_misc(abort,errmsg)
*-------------------------------------------------------------------
* author: John Arrington
* created: 1/16/96
*
* g_trans_misc fills the gen_decoded_misc common block
*
* $Log$
* Revision 1.1  1996/01/22 15:14:10  saw
* Initial revision
*
*--------------------------------------------------------

      implicit none

      include 'gen_data_structures.cmn'

      logical abort
      character*1024 errmsg
      character*20 here
      parameter (here = 'g_trans_misc')

      integer*4 ihit

      save

      abort = .false.
      errmsg = ' '

      do ihit = 1 , gmax_misc_hits
        gmisc_dec_data(ihit,1) = 0     ! Clear TDC's
        gmisc_dec_data(ihit,2) = -1     ! Clear ADC's
      enddo
      
      do ihit = 1 , gmisc_tot_hits
        gmisc_dec_data(gmisc_raw_addr2(ihit),gmisc_raw_addr1(ihit)) =
     $       gmisc_raw_data(ihit)
      enddo

      return
      end
