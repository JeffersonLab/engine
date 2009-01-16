      subroutine b_ntuple_open(file,ABORT,err)

      implicit none
      save

      character*13 here
      parameter(here='b_ntuple_open')

      logical ABORT
      character*(*) err
      integer iquest
      Common /QUEST/ Iquest(100)

      include 'b_ntuple.cmn'
      include 'bigcal_bypass_switches.cmn'
      include 'gen_run_info.cmn'

c      integer itype

      integer default_bank,default_recl
      parameter(default_bank=8000)     !4 bytes/word
      parameter(default_recl=1024)     !record length
      character*80 title,file
      character*80 directory,name
      character*1000 pat,msg,chform
      integer status,size,io,id,bank,recL,iv(10),m
      real rv(10)

      logical HEXIST        !CERNLIB function

      err=' '
      ABORT=.false.
      if(b_ntuple_exists) then
         call b_ntuple_shutdown(ABORT,err)
         if(abort) then
            call G_add_path(here,err)
            return
         endif
      endif

c     get any free IO channel

      call g_IO_control(io,'ANY',ABORT,err)
      b_ntuple_exists = .not.ABORT
      if(ABORT) then
         call G_add_path(here,err)
         return
      endif
      
      b_ntuple_iochannel = io

      id = b_ntuple_id
      name = b_ntuple_name
      title = b_ntuple_title
      ABORT = HEXIST(id)
      if(ABORT) then
         call g_IO_control(b_ntuple_iochannel,'FREE',ABORT,err)
         call g_build_note(':HBOOK id#$ already in use',
     $        '$',id,' ',rv,' ',err)
         call G_add_path(here,err)
         return
      endif

      call HCDIR(directory,'R') !CERNLIB read current directory
  
c$$$      if(bigcal_ntuple_type.eq.1) then
c$$$        recL = default_recl
c$$$        call HROPEN(io,name,file,'N',recL,status)
c$$$      else
c$$$        recL = 8191
c$$$        iquest(10) = 65000
c$$$        call HROPEN(io,name,file,'NQ',recL,status)
c$$$      endif

      recL = 8191
      iquest(10) = 65000
      call HROPEN(io,name,file,'NQ',recL,status)
      

      ABORT= status.ne.0
      if(ABORT) then
         call g_IO_control(b_ntuple_iochannel,'FREE',ABORT,err)
         iv(1) = status
         iv(2) = io
         pat= ':HROPEN error#$ opening IO#$ "'//file//'"'
         call G_build_note(pat,'$',iv,' ',rv,' ',err)
         call G_add_path(here,err)
         return
      endif

      size = b_ntuple_size
      bank = default_bank
      title = b_ntuple_title
      
      if(bigcal_ntuple_type.eq.1) then ! col-wise ntuple for cluster analysis
         call hbset('BSIZE',8176,status)
         call HBNT(id,title,' ')
      if(b_ntuple_switch.ne.0)then

        call HBNAME(id,'bevinfo',bgid,'bgid:I*4,bgtype:I*4,'//
     $        'btrigtype:I*4,btrigtime')
        call HBNAME(id,'bhits',ngooda,'ngooda:I*4,ngoodt:I*4,'//
     $       'ngoodta:I*4,ngoodtt:I*4,irowmax:I*4,icolmax:I*4,'//
     $       'max_adc:R*4')
        
        if(bbypass_find_clusters.eq.0) then
           call HBNAME(id,'clustblock',nclust,
     $          'nclust[0,50]:I*4,ncellclust(nclust)[0,50]:I*4,'//
     $          'ncellbad(nclust)[0,50]:I*4,'//
     $          'ncellx(nclust),ncelly(nclust),iycell(50,nclust),'//
     $          'ixcell(50,nclust),cellbad(50,nclust):L,'//
     $          'xcell(50,nclust),ycell(50,nclust),'//
     $          'eblock(50,nclust),ablock(50,nclust),'//
     $          'xmoment(nclust),ymoment(nclust),'//
     $          'eclust(nclust),aclust(nclust),'//
     $          'xclust(nclust),yclust(nclust)')
           if(bbypass_calc_cluster_time.eq.0) then
              if(bbypass_sum8.eq.0) then
                 call HBNAME(id,'clusttdc',nclust8,
     $                'nclust8[0,50]:I*4,'//
     $                'ncell8clust(nclust8)[0,10]:I*4,'//
     $                'irow8hit(10,nclust8)[0,56]:I*4,'//
     $                'icol8hit(10,nclust8)[0,4]:I*4,'//
     $                'nhit8clust(10,nclust8)[0,8]:I*4,'//
     $                's8(10,nclust8),'//
     $                'tcell8(10,8,nclust8),tclust8(nclust8),'//
     $                'tcut8(nclust8),tcut8cor(nclust8),'//
     $                'trms8(nclust8)')
              endif
              
              if(bbypass_sum64.eq.0) then
                 call HBNAME(id,'clusttrig',nclust64,
     $                'nclust64[0,50]:I*4,'//
     $                'ncell64clust(nclust64)[0,6]:I*4,'//
     $                'irow64hit(6,nclust64)[0,19]:I*4,'//
     $                'icol64hit(6,nclust64)[0,2]:I*4,'//
     $                'nhit64clust(6,nclust64)[0,8]:I*4,'//
     $                'tcell64(6,8,nclust64),a64(6,nclust64),'//
     $                's64(6,nclust64),tclust64(nclust64),'//
     $                'tcut64(nclust64),tcut64cor(nclust64),'//
     $                'trms64(nclust64)')
              endif
           endif
           
           if(bbypass_calc_physics.eq.0) then
              call HBNAME(id,'clustphys',ntrack,'ntrack[0,50]:I*4,'//
     $             'ibest[0,50]:I*4,thetarad(ntrack),'//
     $             'phirad(ntrack),energy(ntrack),'//
     $             'xface(ntrack),yface(ntrack),'//
     $             'zface(ntrack),px(ntrack),py(ntrack),pz(ntrack),'//
     $             'ctime_clust(ntrack),chi2clust(ntrack),'//
     $             'chi2contr(6,ntrack)')
           endif
           
           call HBNAME(id,'bad_clust',nmax,'nmax[0,50]:I*4,'//
     $          'edge_max(nmax):L,not_enough(nmax):L,'//
     $          'too_long_x(nmax):L,too_long_y(nmax):L,'//
     $          'below_thresh(nmax):L,above_max(nmax):L,'//
     $          'second_max(nmax):L')
        endif

        if(gen_bigcal_mc.ne.0) then
           call HBNAME(id,'MC_Clust',evid_g,'evid_g:I*4,'//
     $          'ntrk_g[0,50]:I*4,'//
     $          'pid_g(ntrk_g)[0,50]:I*4,xvertex_g,yvertex_g,'//
     $          'zvertex_g,pxgeant(ntrk_g),pygeant(ntrk_g),'//
     $          'pzgeant(ntrk_g),xgeant(ntrk_g),ygeant(ntrk_g),'//
     $          'egeant(ntrk_g),pgeant(ntrk_g),gthetarad(ntrk_g),'//
     $          'gphirad(ntrk_g)')
        endif
c        write(*,*) 'before adding hmsblk, gen_run_enable=',gen_run_enable
c        write(*,*) 'before adding hmsblk, gen_bigcal_mc=',gen_bigcal_mc
        if(gen_run_enable(5).ne.0.and.(gen_bigcal_mc.eq.3.or.
     $       gen_bigcal_mc.eq.0)) then
c           write(*,*) 'adding block hmsblk to bigcal ntuple'
           call HBNAME(id,'hmsblk',TH_HMS,'TH_HMS,PH_HMS,E_HMS,'//
     $          'X_HMS,Y_HMS,dPel_HMS')
        endif
        endif
      else if(bigcal_ntuple_type.eq.2) then ! col-wise ntuple for cosmics analysis

        !write(*,*) 'booking cosmic hits ntuple:'

        !write(*,*) 'calling hbnt title=',title,' ID=',id
        call hbset('BSIZE',8176,status)
        call hbnt(id,title,' ')
        
        !write(*,*) 'hbnt successful, calling hbname(clear)'

        call hbname(id,' ',0,'$clear')

        !write(*,*) 'clear successful, creating chform'
        chform='nahit[0,1856]:I*4,xa(nahit)[1,32]:I*4,'//
     *       'ya(nahit)[1,56]:I*4,aa(nahit)[-100,8192]:I*4,'//
     *       'nthit[0,3072]:I*4,xt(nthit)[1,4]:I*4,'//
     *       'yt(nthit)[1,56]:I*4,hn(nthit)[1,8]:I*4,'//
     *       'tt(nthit)[-100,8192]:I*4,'//
     *       'ntahit[0,38]:I*4,xta(ntahit)[1,2]:I*4,'//
     *       'yta(ntahit)[1,19]:I*4,taa(ntahit)[-100,8192]:I*4,'//
     *       'ntthit[0,336]:I*4,xtt(ntthit)[1,2]:I*4,'//
     *       'ytt(ntthit)[1,21]:I*4,hnt(ntthit)[1,8]:I*4,'//
     *       'ttt(ntthit)[-100,8192]:I*4'
        !write(*,*) 'chform successful, calling hbname(chform)'
        call hbname(id,'hits',nahit,chform)

        !write(*,*) 'booking of cosmic hits ntuple successful'

      endif
        
      call HCDIR(b_ntuple_directory,'R')     ! record ntuple directory

      call HCDIR(directory,' ')          !reset CERNLIB directory

      b_ntuple_exists = HEXIST(b_ntuple_id)

      abort = .not.b_ntuple_exists

      iv(1) = id
      iv(2) = io
      pat = 'Ntuple id#$ [' // b_ntuple_directory // '/]' //
     $     name // ' IO#$ "' // file // '"'
      call G_build_note(pat,'$',iv,' ',rv,' ',msg)

      call sub_string(msg,'/]','/]')

      if(abort) then
         err = 'unable to create '//msg
         call G_add_path(here,err)
      else
         pat=':created '//msg
         call G_add_path(here,pat)
         call G_log_message('INFO: '//pat)
      endif

      return 
      end
