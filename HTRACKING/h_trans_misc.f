      subroutine h_trans_misc(abort,errmsg)
*-------------------------------------------------------------------
* author: John Arrington
* created: 4/8/95
*
* h_trans_misc fills the hms_decoded_misc common block
*
* $Log$
* Revision 1.2  1995/05/22 19:39:32  cdaq
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

      logical abort
      character*1024 errmsg
      character*20 here
      parameter (here = 'h_trans_misc')

      integer*4 ihit

      save

      do ihit = 1 , 48
         hmisc_dec_data(ihit) = 0
      enddo
      
      do ihit = 1 , hmisc_tot_hits
c        write(50,*) ihit, hmisc_raw_addr1(ihit), hmisc_raw_addr2(ihit), hmisc_raw_data(ihit)
c        write(51,*) ihit, hmisc_raw_addr2(ihit), hmisc_raw_data(ihit)
        hmisc_dec_data(hmisc_raw_addr2(ihit)) = hmisc_raw_data(ihit)
      enddo

      do ihit = 1 , 48
c        write(52,*) ihit, hmisc_dec_data(ihit)
      enddo

      return
      end
