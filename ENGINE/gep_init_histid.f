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

      abort=.false.
      err=' '

      

      return 
      end
