      subroutine b_init_physics(ABORT,err)

      implicit none
      save
      
      character*14 here
      parameter(here='b_init_physics')

      logical ABORT
      character*(*) err

      include 'gen_data_structures.cmn'
      include 'bigcal_data_structures.cmn'
      include 'gen_constants.par'
      include 'gen_units.par'

      abort=.false.
      err=' '

c     BIGCAL_HEIGHT = 0.
      BIGCAL_THETA_RAD = BIGCAL_THETA_DEG * tt / 180.
      
      BIGCAL_SINTHETA = sin(BIGCAL_THETA_RAD)
      BIGCAL_COSTHETA = cos(BIGCAL_THETA_RAD)

c     that's all for now

      return 
      end
      
      
