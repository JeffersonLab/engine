      subroutine gep_init_histid(abort,err)

      implicit none
      save

      character*15 here
      parameter(here='gep_init_histid')

      logical abort
      character*(*) err
      external thgetid
      integer*4 thgetid
     
      include 'gep_data_structures.cmn'
      include 'gep_hist_id.cmn'

c     do nothing until we set up some histograms...
c     miscellaneous hard-coded histograms for checkout

      abort=.false.
      err=' '

      gepid_gep_trigtype = thgetid('gep_trigtype')
      gepid_gep_evtype = thgetid('gep_evtype')
      gepid_gep_trigtype_v_evtype = thgetid('gep_trigtype_v_evtype')
      gepid_gep_ntrigs = thgetid('gep_ntrigs')

      return 
      end
