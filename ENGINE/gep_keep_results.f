      subroutine gep_keep_results(ABORT,err)
      
      implicit none
      save

      include 'gep_data_structures.cmn'
      include 'hms_data_structures.cmn'
      include 'bigcal_data_structures.cmn'

      character*14 here
      parameter(here='b_keep_results')

      logical abort
      character*(*) err

      abort=.false.
      err=' '

      if(HSNUM_FPTRACK.gt.0 .and. BIGCAL_PHYS_NTRACK .gt. 0) then
         call gep_ntuple_keep(abort,err)
      endif
      if(abort) then
         call G_add_path(here,err)
         return
      else
         err=' '
      endif
      
      return
      end
