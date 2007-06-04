      subroutine b_ntuple_shutdown(ABORT,err)
c     final shutdown of BigCal ntuple
      implicit none
      save
      
      character*17 here
      parameter(here='b_ntuple_shutdown')

      logical abort
      character*(*) err

      include 'b_ntuple.cmn'
      include 'gen_routines.dec'

      logical fail
      character*80 why,directory,name
      character*1000 msg
      integer io,id,cycle,m

      err=' '
      abort=.false.

      if(.not.b_ntuple_exists) return ! nothing to do

      call b_ntuple_close(ABORT,err)

      if(b_ntuple_exists) then
         abort=.true.
      endif
      
      b_ntuple_ID=0
      b_ntuple_name=' '
      b_ntuple_file=' '
      b_ntuple_title=' '
      b_ntuple_size=0
      do m=1,bmax_ntuple_size
         b_ntuple_tag(m)=  ' '
         b_ntuple_contents(m) = 0.
      enddo

      if(abort) call G_add_path(here,err)
      
      return 
      end
