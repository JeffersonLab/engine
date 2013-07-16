
      subroutine h_init_cer(ABORT,err)

*-------------------------------------------------------------------
*
* author: Chris Cothran
* created: 5/25/95
*
* h_init_cer initializes parameters relevant to the HMS Cerenkov.
* $Log: h_init_cer.f,v $
* Revision 1.1  1995/08/31 14:53:56  cdaq
* Initial revision
*
*-------------------------------------------------------------------

      implicit none

      include 'hms_cer_parms.cmn'

      logical abort
      character*(*) err
      character*20 here
      parameter (here='h_init_cer')

      integer*4 ii

      save

      do ii = 1, hcer_num_regions
        hcer_track_counter(ii) = 0
        hcer_fired_counter(ii) = 0
      enddo

      return
      end
