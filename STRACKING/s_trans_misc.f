      subroutine s_trans_misc(abort,errmsg)
*-------------------------------------------------------------------
* author: John Arrington
* created: 4/8/95
*
* s_trans_misc fills the sos_decoded_misc common block
*
* $Log$
* Revision 1.1  1995/04/12 03:59:23  cdaq
* Initial revision
*
*
*--------------------------------------------------------

      implicit none

      include 'gen_data_structures.cmn'
      include 'sos_scin_parms.cmn'

      logical abort
      character*1024 errmsg
      character*20 here
      parameter (here = 's_trans_misc')

      integer*4 ihit

      save
      
      do ihit = 1 , 48
        smisc_dec_data(ihit)=0
      enddo

      do ihit = 1 , smisc_tot_hits
c        write(60,*) ihit, smisc_raw_addr1(ihit), smisc_raw_addr2(ihit), smisc_raw_data(ihit)
c        write(61,*) ihit, smisc_raw_addr2(ihit), smisc_raw_data(ihit)
        smisc_dec_data(smisc_raw_addr2(ihit)) = smisc_raw_data(ihit)
      enddo

      do ihit = 1 , 48
c        write(62,*) ihit, smisc_dec_data(ihit)
      enddo

      return
      end
