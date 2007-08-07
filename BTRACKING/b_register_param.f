      subroutine b_register_param(ABORT,err)
      
      IMPLICIT NONE
      SAVE
*
      character*16 here
      parameter (here= 'B_register_param')
*
      logical ABORT
      character*(*) err
*
      logical FAIL
      character*1000 why

      err= ' '
      ABORT = .false.

      call r_bigcal_geometry
      call r_bigcal_gain_parms
      call r_bigcal_tof_parms
      call r_bigcal_shower_parms

      call r_bigcal_bypass_switches
      call r_bigcal_hist_id

c$$$      call r_bigcal_statistics
c$$$      call r_bigcal_pedestals

      IF(ABORT .or. err.NE.' ') call G_add_path(here,err)

      return 
      end
