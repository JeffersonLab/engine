      subroutine sane_ntup_change(ABORT,err)
      
      implicit none
      save
      
      character*15 here
      parameter(here='sane_ntuple_change')

      logical ABORT
      character*(*) err

      include 'sane_ntuple.cmn'
      include 'gen_run_info.cmn'

      character*1 ifile
      character*1 iifile
      character*80 file
      character*1000 pat

      integer*4 ilo,fn_len

      integer g_important_length

      call sane_ntup_close(ABORT,err)

      if(sane_ntuple_exists) then
         ABORT=.true.
      endif
      
      call NO_nulls(sane_ntuple_file) 
      
      file = sane_ntuple_file

      call NO_nulls(file)
      call g_sub_run_number(file,gen_run_number)

      sane_ntuple_filesegments = sane_ntuple_filesegments + 1
      if(sane_ntuple_filesegments.eq.10)then
         sane_ntuple_filesegments = 1
         sane_ntuple_auxsegments  = sane_ntuple_auxsegments + 1
      endif
      !write(*,*) 'computing ifile'
c      if(sane_ntuple_filesegments.lt.10) then
         ifile = char(ichar('0')+sane_ntuple_filesegments)
         iifile = char(ichar('0')+sane_ntuple_auxsegments)
c      else 
c         ifile = char(ichar('a')+sane_ntuple_filesegments-10)
c      endif
c      write(*,*), 'ifile = ',ifile, file

      fn_len = g_important_length(file)
      ilo = index(file,'.hbook')
      if((ilo.le.1).or.(ilo.gt.fn_len-5)) then
         ilo=index(file,'.rzdat')
      endif

      if((ilo.gt.1).and.(ilo.lt.fn_len)) then
         file = file(1:ilo-1)//'.'//iifile//'.'//ifile//file(ilo:fn_len)
         sane_Ntuple_name = sane_Ntuple_name(1:ilo-7)//'_'//iifile//'_'//ifile
c         write(*,*)'File name is', file
c      else 
c         abort=.true.
      endif
      
      !write(*,*) 'new file name = ',file

      if(.not.abort) call sane_ntup_open(file,ABORT,err)

      if(abort) then
         err=':unable to change BigCal ntuple file segment'
         call G_add_path(here,err)
      else
         pat=':changed SANE ntuple file segment'
         call G_add_path(here,pat)
         call G_log_message('INFO: '//pat)
      endif
      
      return 
      end
      
