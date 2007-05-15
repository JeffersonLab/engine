      subroutine gep_ntuple_change(ABORT,err)

      implicit none
      save

      character*17 here
      parameter(here='gep_ntuple_change')

      logical abort
      character*(*) err

      include 'gep_ntuple.cmn'
      include 'gen_run_info.cmn'

      character*1 ifile
      character*80 file
      character*1000 pat

      integer*4 ilo,fn_len

*     functions
      integer g_important_length

      call gep_ntuple_close(abort,err)

      if(gep_ntuple_exists) then
         abort=.true.
      endif

      call NO_nulls(gep_ntuple_file) 
      file = gep_ntuple_file

      call NO_nulls(file)
      call g_sub_run_number(file,gen_run_number)

      gep_ntuple_filesegments = gep_ntuple_filesegments + 1
      if(gep_ntuple_filesegments.le.9) then
         ifile = char(ichar('0')+gep_ntuple_filesegments)
      else 
         ifile = char(ichar('a') + gep_ntuple_filesegments-10)
      endif

      fn_len = g_important_length(file)

      ilo=index(file,'.hbook')
      if((ilo.le.1).or.(ilo.gt.fn_len-5)) then
         ilo=index(file,'.rzdat')
      endif

      if((ilo.gt.1).and.(ilo.lt.fn_len)) then
         file = file(1:ilo-1)//'.'//ifile//file(ilo:fn_len)
      else 
         abort=.true.
      endif

      if(.not.abort) call gep_ntuple_open(file,ABORT,err)

      if(abort) then
         err='unable to change GEp ntuple file segment'
         call G_add_path(here,err)
      else 
         pat=':changed GEp Ntuple file segment'
         call G_add_path(here,pat)
         call G_log_message('INFO: '//pat)
      endif
      
      return 
      end
