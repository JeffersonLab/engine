      Subroutine sane_ntup_init(ABORT,err)
      implicit none
      save

      character*13 here
      parameter(here='sane_ntuple_init')
      include 'b_ntuple.cmn'
      include 'sane_ntuple.cmn'
      include 'sane_ntuple.dte'
      include 'gen_routines.dec'
      include 'gen_run_info.cmn'

      character*80 default_name 
      parameter(default_name='sane_ntuple')

      logical ABORT
      character*(*) err

      character*80 file
      character*80 name
      character*1000 pat,msg
      integer ilo,fn_len,m,i,j,k
      character*1 ifile
      character*1 iifile
      err=' '
      abort=.false.


c      write(*,*)'SANE',sane_ntuple_file,sane_ntuple_exists,sane_ntuple_id
      if(sane_ntuple_exists) then 
         !write(*,*) 'calling sane_ntuple_shutdown'
         call sane_ntup_shutdown(ABORT,err)
         if(abort) then 
            call G_add_path(here,err)
            return
         endif
      endif

      call no_nulls(sane_ntuple_file) ! replace null characters with blanks

      if(sane_ntuple_file.eq.' ') return
      sane_ntuple_id = default_sane_ntuple_ID
      sane_ntuple_name = default_name
      if(sane_ntuple_title.eq.' ') then
         msg = sane_ntuple_name//' '//sane_ntuple_file
         !write(*,*) 'bigcal ntuple title = ',msg
         call only_one_blank(msg)
         sane_ntuple_title= msg
c         sane_ntuple_title= sane_ntuple_file
         
      endif
      
      file = sane_ntuple_file
     
      call g_sub_run_number(file,gen_run_number)
      if(sane_ntuple_max_segmentevents.gt.0) then
         sane_ntuple_filesegments = 1
         sane_ntuple_auxsegments  = sane_ntuple_auxsegments + 1
         ifile = char(ichar('0')+sane_ntuple_filesegments)
         iifile = char(ichar('0')+sane_ntuple_auxsegments)
         fn_len = g_important_length(file)
         ilo=index(file,'.hbook')
         if((ilo.le.1).or.(ilo.gt.fn_len-5))then
            ilo=index(file,'.rzdat')
         endif
         
         if((ilo.gt.1).and.(ilo.lt.fn_len)) then
            file = file(1:ilo-1)//'.'//iifile//'.'//ifile//file(ilo:fn_len)
            sane_Ntuple_name = default_name(1:ilo-7)//'_'//iifile//'_'//ifile
         else
            abort=.true.
            return
         endif
         
         write(*,*) ' using segmented sane rzdat files
     $        first filename: ',file
      else
         write(*,*) ' Not using segmented sane rzdat files 
     $        first filename: ',file
      endif

      if(sane_ntuple_type.eq.2) then ! raw ntuple
         sane_ntuple_id = 9502
         write(*,*)'SANE NT ID is',sane_ntuple_id
      endif
      if(sane_ntuple_type.eq.1) then ! physics  ntuple added Jul 3,2008
         sane_ntuple_id = 9501
         write(*,*)'SANE NT ID is',sane_ntuple_id
      endif

      write(*,*)'SANE INIT',file,sane_ntuple_type

      call sane_ntup_open(file,ABORT,err)


      if(abort) then
         err= ':unable to create SANE ntuple'
         call G_add_path(here,err)
      else
         pat= ':created SANE ntuple'
         call G_add_path(here,pat)
         call G_log_message('INFO: '//pat)
      endif  

      return 

      end
