      subroutine b_ntuple_init(itype,ABORT,err)

      implicit none
      save

      character*13 here
      parameter(here='b_ntuple_init')
      
      integer itype

      include 'b_ntuple.cmn'
      include 'gen_routines.dec'
      include 'bigcal_data_structures.cmn'
      include 'gen_run_info.cmn'
      include 'b_ntuple.dte'

      character*80 default_name 
      parameter(default_name='BIGCALntuple')

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
         write(*,*) 'calling b_ntuple_shutdown'
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
         msg = name//' '//b_ntuple_file
         call only_one_blank(msg)
         b_ntuple_title= msg
      endif
      
      file = b_ntuple_file
      call g_sub_run_number(file,gen_run_number)

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

      m=0

      if(itype.le.2) then ! "normal" ntuple, also used for ep calibration
         m=m+1
         b_ntuple_tag(m) = 'time' ! coin time in ns of best cluster
         m=m+1
         b_ntuple_tag(m) = 'etheta' ! polar angle in degrees
         m=m+1
         b_ntuple_tag(m) = 'ephi' ! azimuthal angle in degrees
         m=m+1
         b_ntuple_tag(m) = 'Etot' ! energy in GeV
         m=m+1 
         b_ntuple_tag(m) = 'xcal' ! x of calo impact point
         m=m+1
         b_ntuple_tag(m) = 'ycal' ! y of calo impact point
         m=m+1
         b_ntuple_tag(m) = 'zcal' ! z of calo impact point
         m=m+1
         b_ntuple_tag(m) = 'px' ! px of electron
         m=m+1
         b_ntuple_tag(m) = 'py' ! py of electron
         m=m+1
         b_ntuple_tag(m) = 'pz' ! pz of electron
         m=m+1
         b_ntuple_tag(m) = 'beta' ! v/c of electron
         m=m+1
         b_ntuple_tag(m) = 'tof' ! time of flight of electron
         m=m+1
         b_ntuple_tag(m) = 'iymax' ! iy of central cell
         m=m+1
         b_ntuple_tag(m) = 'ixmax' ! ix of central cell
         m=m+1
         b_ntuple_tag(m) = 'xclust' ! recon. x of cluster
         m=m+1
         b_ntuple_tag(m) = 'yclust' ! recon. y of cluster
         m=m+1
         b_ntuple_tag(m) = 'Eclust' ! Esum of cluster
         m=m+1
         b_ntuple_tag(m) = 'xmom' ! x moment of cluster
         m=m+1
         b_ntuple_tag(m) = 'ymom' ! y moment of cluster
         m=m+1
         b_ntuple_tag(m) = 't8avg' ! average time of sum of 8
         m=m+1
         b_ntuple_tag(m) = 't64avg' ! average time of sum of 64
         m=m+1
         b_ntuple_tag(m) = 'L8sum' ! level sum for sum8 channels
         m=m+1
         b_ntuple_tag(m) = 'L64sum' ! level sum for sum64 channels
         do i=-2,2        ! cell amplitudes (5x5)
            do j=-2,2
               m=m+1
               if(i.lt.0)then
                  iytag='-'//char(-i+ichar('0')) !!should I use char(i) or char(i+48)? 
               else if(i.gt.0)then    !!(ASCII for numeric digits 0-9)
                  iytag='+'//char(i+ichar('0'))
               else 
                  iytag='_'//char(i+ichar('0'))
               endif

               if(j.lt.0)then
                  ixtag='-'//char(-j+ichar('0'))
               else if(j.gt.0)then
                  ixtag='+'//char(j+ichar('0'))
               else 
                  ixtag='_'//char(j+ichar('0'))
               endif

               Ecelltag='E'//iytag//ixtag
               b_ntuple_tag(m) = Ecelltag
            enddo
         enddo 
c     so far we have added 48 quantities to the ntuple: 23 'per event' 
c     quantities + 25 cell amplitudes for the best cluster. We only allow 
c     one cluster to get into the ntuple per event for now.
      else if(itype.eq.3) then ! cosmic tracking
         ! don't add this right now
      else if(itype.eq.4) then ! laser/light box/gain monitoring 
         ! don't add this right now
      endif
      
      b_ntuple_size = m

      call b_ntuple_open(file,ABORT,err)

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
