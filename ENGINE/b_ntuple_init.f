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
      character*2 ifile

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
         ifile = '0'//char(ichar('0')+b_ntuple_filesegments)
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
      
      if(bigcal_ntuple_type.eq.3) then ! lightweight, row-wise "best cluster" ntuple:
         m = 0
         m = m + 1
         b_ntuple_tag(m) = 'evid'
         m = m + 1
         b_ntuple_tag(m) = 'evtype'
         m = m + 1
         b_ntuple_tag(m) = 'btrigtime'
         m = m + 1
         b_ntuple_tag(m) = 'na'
         m = m + 1 
         b_ntuple_tag(m) = 'nt'
         m = m + 1
         b_ntuple_tag(m) = 'nta'
         m = m + 1
         b_ntuple_tag(m) = 'ntt'
         m = m + 1
         b_ntuple_tag(m) = 'rowmax'
         m = m + 1
         b_ntuple_tag(m) = 'colmax'
         m = m + 1
         b_ntuple_tag(m) = 'adcmax'
         m = m + 1
         b_ntuple_tag(m) = 'nclust'
         m = m + 1
         b_ntuple_tag(m) = 'nmax'
         m = m + 1
         b_ntuple_tag(m) = 'ncell'
         m = m + 1
         b_ntuple_tag(m) = 'nx'
         m = m + 1
         b_ntuple_tag(m) = 'ny'
         m = m + 1
         b_ntuple_tag(m) = 'n8'
         m = m + 1
         b_ntuple_tag(m) = 'n64'
         m = m + 1
         b_ntuple_tag(m) = 'rowclust'
         m = m + 1
         b_ntuple_tag(m) = 'colclust'
         m = m + 1         
         b_ntuple_tag(m) = 'xcell'
         m = m + 1
         b_ntuple_tag(m) = 'ycell'
         m = m + 1
         b_ntuple_tag(m) = 'xmom'
         m = m + 1
         b_ntuple_tag(m) = 'ymom'
         m = m + 1
         b_ntuple_tag(m) = 'xclust'
         m = m + 1
         b_ntuple_tag(m) = 'yclust'
         m = m + 1
         b_ntuple_tag(m) = 'aclust'
         m = m + 1
         b_ntuple_tag(m) = 'eclust'
         m = m + 1
         b_ntuple_tag(m) = 'tmean8'
         m = m + 1
         b_ntuple_tag(m) = 'trms8'
         m = m + 1
         b_ntuple_tag(m) = 'tmean64'
         m = m + 1
         b_ntuple_tag(m) = 'trms64'
         m = m + 1
         b_ntuple_tag(m) = 'thetarad'
         m = m + 1
         b_ntuple_tag(m) = 'phirad'
         m = m + 1
         b_ntuple_tag(m) = 'energy'
         m = m + 1
         b_ntuple_tag(m) = 'ctimeb'
         m = m + 1
         b_ntuple_tag(m) = 'tofcor'
         m = m + 1
         b_ntuple_tag(m) = 'hsntrk'
         m = m + 1
         b_ntuple_tag(m) = 'chi2'
         m = m + 1
         b_ntuple_tag(m) = 'chi2_x'
         m = m + 1
         b_ntuple_tag(m) = 'chi2_y'
         m = m + 1
         b_ntuple_tag(m) = 'chi2_th'
         m = m + 1
         b_ntuple_tag(m) = 'chi2_ph'
         m = m + 1
         b_ntuple_tag(m) = 'chi2_E'
         m = m + 1
         b_ntuple_tag(m) = 'chi2_t'
         m = m + 1
         b_ntuple_tag(m) = 'ehms'
         m = m + 1
         b_ntuple_tag(m) = 'thhms'
         m = m + 1
         b_ntuple_tag(m) = 'phhms'
         m = m + 1
         b_ntuple_tag(m) = 'xhms'
         m = m + 1
         b_ntuple_tag(m) = 'yhms'
         m = m + 1
         b_ntuple_tag(m) = 'thms'
         m = m + 1
         b_ntuple_tag(m) = 'dpelh'
         m = m + 1
         b_ntuple_tag(m) = 'dpelb'
         m = m + 1
         b_ntuple_tag(m) = 'vxh'
         m = m + 1
         b_ntuple_tag(m) = 'vyh'
         m = m + 1
         b_ntuple_tag(m) = 'vzh'
         m = m + 1 
         b_ntuple_tag(m) = 'xptar'
         m = m + 1
         b_ntuple_tag(m) = 'yptar'

         b_ntuple_size = m
      endif

      !write(*,*) 'calling b_ntuple_open'

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
