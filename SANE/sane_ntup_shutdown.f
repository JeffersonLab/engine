      subroutine sane_ntup_shutdown(ABORT,err)
c     final shutdown of BigCal ntuple
      implicit none
      save
      
      character*17 here
      parameter(here='sane_ntuple_shutdown')

      logical abort
      character*(*) err

      include 'b_ntuple.cmn'
      include 'sane_ntuple.cmn'
      include 'gen_routines.dec'

      logical fail
      character*80 why,directory,name
      character*1000 msg
      integer io,id,cycle,m

      err=' '
      abort=.false.

      if(.not.sane_ntuple_exists) return ! nothing to do

      call sane_ntup_close(ABORT,err)

      if(sane_ntuple_exists) then
         abort=.true.
      endif
      
      sane_ntuple_ID=0
      sane_ntuple_name=' '
      sane_ntuple_file=' '
      sane_ntuple_title=' '
      sane_ntuple_size=0
      do m=1,sanemax_ntuple_size
         sane_ntuple_tag(m)=  ' '
         sane_ntuple_contents(m) = 0.
      enddo

      if(abort) call G_add_path(here,err)
      
      return 
      end
