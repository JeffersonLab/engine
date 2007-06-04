      subroutine b_ntuple_change(ABORT,err)
      
      implicit none
      save
      
      character*15 here
      parameter(here='b_ntuple_change')

      logical ABORT
      character*(*) err

      include 'b_ntuple.cmn'
      include 'gen_run_info.cmn'

      character*1 ifile
      character*80 file
      character*1000 pat

      integer*4 ilo,fn_len

      integer g_important_length

      call b_ntuple_close(ABORT,err)

      if(b_ntuple_exists) then
         ABORT=.true.
      endif
      
      call NO_nulls(b_ntuple_file) 
      
      file = b_ntuple_file

      call NO_nulls(file)
      call g_sub_run_number(file,gen_run_number)

      b_ntuple_filesegments = b_ntuple_filesegments + 1
      !write(*,*) 'computing ifile'
      if(b_ntuple_filesegments.lt.10) then
         ifile = char(ichar('0')+b_ntuple_filesegments)
      else 
         ifile = char(ichar('a')+b_ntuple_filesegments-10)
      endif
      !write(*,*), 'ifile = ',ifile

      fn_len = g_important_length(file)
      ilo = index(file,'.hbook')
      if((ilo.le.1).or.(ilo.gt.fn_len-5)) then
         ilo=index(file,'.rzdat')
      endif

      if((ilo.gt.1).and.(ilo.lt.fn_len)) then
         file = file(1:ilo-1)//'.'//ifile//file(ilo:fn_len)
      else 
         abort=.true.
      endif
      
      !write(*,*) 'new file name = ',file

      if(.not.abort) call b_ntuple_open(file,ABORT,err)

      if(abort) then
         err=':unable to change BigCal ntuple file segment'
         call G_add_path(here,err)
      else
         pat=':changed BigCal ntuple file segment'
         call G_add_path(here,pat)
         call G_log_message('INFO: '//pat)
      endif
      
      return 
      end
      
