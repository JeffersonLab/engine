      subroutine b_report_bad_data(lunout,ABORT,err)

      implicit none
      save

      character*17 here
      parameter(here='b_report_bad_data')

      logical ABORT
      character*(*) err

      integer lunout

      include 'bigcal_data_structures.cmn'

c     for now, don't do anything. for the HMS, the equivalent subroutine does 
c     nothing because "we always use the pedestal events"
c     I will assume that is the case here.

      abort=.false.
      err=' '

      return
      end
