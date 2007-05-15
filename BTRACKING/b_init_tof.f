      subroutine b_init_tof(ABORT,err)

      implicit none
      save
      
      character*10 here
      parameter(here='b_init_tof')

      logical ABORT
      character*(*) err
      
      include 'bigcal_data_structures.cmn'
      include 'bigcal_tof_parms.cmn'
      
      
c     do nothing for now. Hopefully all variables are correctly read in from CTP parm
c     files

      ABORT=.false.
      err = ' '

      return 
      end
