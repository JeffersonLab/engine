      subroutine b_ntuple_init(ABORT,err)

      implicit none
      save

      character*13 here
      parameter(here='b_ntuple_init')
      
c      integer itype

      include 'b_ntuple.cmn'
      include 'gen_routines.dec'
      include 'bigcal_data_structures.cmn'
      include 'gen_run_info.cmn'
      include 'b_ntuple.dte'

      character*80 default_name 
      parameter(default_name='BIGCAL_ntuple')

      logical ABORT
      character*(*) err

      character*80 file
      character*80 name
      character*1000 pat,msg
      integer ilo,fn_len,m,i,j,k
      character*1 ifile

      character*5 Ecelltag
      character*2 ixtag,iytag

c$$$      data b_ntuple_exists/.false./
c$$$      data b_ntuple_ID/0/
c$$$      data b_ntuple_file/' '/
c$$$      data b_ntuple_name/' '/
c$$$      data b_ntuple_title/' '/
c$$$      data b_ntuple_directory/' '/
c$$$      data b_ntuple_IOchannel/0/
c$$$      data b_ntuple_size/0/
c$$$      data b_ntuple_tag/bmax_ntuple_size*' '/
c$$$      data b_ntuple_contents/bmax_ntuple_size*0/

      err=' '
      abort=.false.

      if(b_ntuple_exists) then 
         !write(*,*) 'calling b_ntuple_shutdown'
         call b_ntuple_shutdown(ABORT,err)
         if(abort) then 
            call G_add_path(here,err)
            return
         endif
      endif

      call no_nulls(b_ntuple_file) ! replace null characters with blanks

      if(b_ntuple_file.eq.' ') return
      b_ntuple_id = default_b_ntuple_ID
      b_ntuple_name = default_name
      if(b_ntuple_title.eq.' ') then
         msg = b_ntuple_name//' '//b_ntuple_file
         !write(*,*) 'bigcal ntuple title = ',msg
         call only_one_blank(msg)
         b_ntuple_title= msg
         
      endif
      
      file = b_ntuple_file
      call g_sub_run_number(file,gen_run_number)

c      write(*,*) 'b_ntuple_max_segmentevents=',b_ntuple_max_segmentevents

      if(b_ntuple_max_segmentevents.gt.0) then
         b_ntuple_filesegments = 1
         ifile = char(ichar('0')+b_ntuple_filesegments)
         fn_len = g_important_length(file)
         ilo=index(file,'.hbook')
         if((ilo.le.1).or.(ilo.gt.fn_len-5))then
            ilo=index(file,'.rzdat')
         endif
         
         if((ilo.gt.1).and.(ilo.lt.fn_len)) then
            file = file(1:ilo-1)//'.'//ifile//file(ilo:fn_len)
         else
            abort=.true.
            return
         endif
         
         write(*,*) ' using segmented bigcal rzdat files
     $        first filename: ',file
      else
         write(*,*) ' Not using segmented bigcal rzdat files 
     $        first filename: ',file
      endif

c     choose ntuple type based on input argument

      if(bigcal_ntuple_type.eq.2) then ! cosmics ntuple
         b_ntuple_id = 9501
      endif
      

      !write(*,*) 'calling b_ntuple_open'
c      b_ntuple_exists=.TRUE.
         call b_ntuple_open(file,ABORT,err)
      !write(*,*) 'b_ntuple_open successful, itype=',bigcal_ntuple_type

      if(abort) then
         err= ':unable to create BigCal ntuple'
         call G_add_path(here,err)
      else
         pat= ':created BigCal ntuple'
         call G_add_path(here,pat)
         call G_log_message('INFO: '//pat)
      endif  

      return 
      end
