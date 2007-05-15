      subroutine b_init_shower(ABORT,err)

      implicit none
      save

      character*13 here
      parameter(here='b_init_shower')

      logical ABORT
      character*(*) err

      include 'bigcal_data_structures.cmn'
      include 'bigcal_shower_parms.cmn'

      ABORT=.false.
      err=' '

c     don't do anything for now...
c     boy do I hope I can get this CTP parm stuff working!!!!
      
      return 
      end
