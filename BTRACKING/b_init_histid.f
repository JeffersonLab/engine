      subroutine b_init_histid(ABORT,err)

      implicit none
      save
      
      character*13 here
      parameter(here='b_init_histid')

      logical ABORT
      character*(*) err

      external thgetid
      integer*4 thgetid
      
      include 'bigcal_data_structures.cmn'
      include 'bigcal_gain_parms.cmn'
      include 'bigcal_shower_parms.cmn'
      include 'bigcal_tof_parms.cmn'
      include 'bigcal_geometry.cmn'

c     For now, don't do anything here: I don't necessarily want to do 
c     hard-coded histograms. I would like to use CTP. Will come back later
c     if this proves more convenient






      return
      end
