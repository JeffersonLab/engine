      subroutine gep_ntuple_shutdown(ABORT,err)
c     final shutdown of GEp ntuple

      implicit none
      save

      character*19 here
      parameter(here='gep_ntuple_shutdown')

      logical abort
      character*(*) err

      include 'gep_ntuple.cmn'
      include 'gen_routines.dec'

      logical fail
      character*80 why,directory,name
      character*1000 msg
      integer io,id,cycle,m

      err=' '
      abort=.false.

      if(.not.gep_ntuple_exists) return

      call gep_ntuple_close(abort,err)

      if(gep_ntuple_exists) then
         abort=.true.
      endif

      gep_ntuple_ID=0
      gep_ntuple_name=' '
      gep_ntuple_file=' '
      gep_ntuple_title=' '
      gep_ntuple_size=0
      do m=1,gep_ntuple_size
         gep_ntuple_tag(m)=' '
         gep_ntuple_contents(m)=0.
      enddo

      if(abort) call G_add_path(here,err)

      return 
      end
