
      subroutine s_init_cer(ABORT,err)

*-------------------------------------------------------------------
*
* author: Chris Cothran
* created: 5/25/95
*
* s_init_cer initializes parameters relevant to the SOS Cerenkov.
* $Log: s_init_cer.f,v $
* Revision 1.1  1995/08/31 15:05:05  cdaq
* Initial revision
*
*-------------------------------------------------------------------

      implicit none

      include 'sos_cer_parms.cmn'

      logical abort
      character*(*) err
      character*20 here
      parameter (here='s_init_cer')

      integer*4 ii

      save

      do ii = 1, scer_num_regions
        scer_track_counter(ii) = 0
        scer_fired_counter(ii) = 0
      enddo

      return
      end
