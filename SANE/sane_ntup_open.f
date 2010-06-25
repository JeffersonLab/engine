      Subroutine sane_ntup_open(file,ABORT,err)
      implicit none
      save

      character*13 here
      parameter(here='sane_ntuple_open')

      logical ABORT
      character*(*) err
      integer iquest
      Common /QUEST/ Iquest(100)

      include 'b_ntuple.cmn'
      include 'sane_ntuple.cmn'
      include 'bigcal_bypass_switches.cmn'
      include 'gen_run_info.cmn'
      include 'gen_data_structures.cmn'

      integer default_bank,default_recl, histnum,ii
      parameter(default_bank=8000)     !4 bytes/word
      parameter(default_recl=8191)     !record length
      character*80 title,file
      character*80 directory,name
      character*1000 pat,msg,chform
      integer status,size,io,id,bank,recL,iv(10),m,itcol,itrow,icc
      real rv(10)
      

      logical HEXIST        !CERNLIB function
ccccccccccccccc
      err=' '
      ABORT=.false.
      if(sane_ntuple_exists) then
         call sane_ntup_shutdown(ABORT,err)
         if(abort) then
            call G_add_path(here,err)
            return
         endif
      endif

c     get any free IO channel

      call g_IO_control(io,'ANY',ABORT,err)
c      sane_ntuple_exists = .not.ABORT
      if(ABORT) then
         call G_add_path(here,err)
         return
      endif

      sane_ntuple_iochannel = io

      id = sane_ntuple_id
      name = sane_ntuple_name
      title = sane_ntuple_title
      ABORT = HEXIST(id)
      if(ABORT) then
         call g_IO_control(sane_ntuple_iochannel,'FREE',ABORT,err)
         call g_build_note(':HBOOK id#$ already in use',
     $        '$',id,' ',rv,' ',err)
         call G_add_path(here,err)
         return
      endif

      call HCDIR(directory,'R') !CERNLIB read current directory
      write(*,*)'DIR ',directory,name
c      call HLIMIT(NWPAWC)
      recL = 8191
      iquest(10) = 512000
      call HROPEN(io,name,file,'NQ',recL,status)
      
      write(*,*)'ifile=',file
      ABORT= status.ne.0
      if(ABORT) then
         call g_IO_control(sane_ntuple_iochannel,'FREE',ABORT,err)
         iv(1) = status
         iv(2) = io
         pat= ':HROPEN error#$ opening IO#$ "'//file//'"'
         call G_build_note(pat,'$',iv,' ',rv,' ',err)
         call G_add_path(here,err)
         return
      endif

      size = sane_ntuple_size
      bank = default_bank
      title = sane_ntuple_title

      call hbset('BSIZE',8176,status)
      call HBNT(id,title,' ')
      if(sane_ntuple_type.lt.3)then
      call HBNAME(id,'SANEEV',tcharge,'tcharge:R*8,charge2s:R*8,'//
     $     'tcharge_help:R*8,charge2s_help:R*8,'//
     $     'tcharge_helm:R*8,charge2s_helm:R*8,'//
     $     'polarea:R*8,polarization:R*8,'//
     $     'hel_p_scaler:I*4, hel_n_scaler:I*4,'//
     $     'hel_p_trig:I*4, hel_n_trig:I*4,'//
     $     'dtime_p:R*8,dtime_n:R*8,half_plate:R*4')
      endif
               call HBNAME(id,'hmsblk',TH_HMS,'TH_HMS,PH_HMS,E_HMS,'//
     $              'X_HMS,Y_HMS,dPel_HMS')

      if(sane_ntuple_type.ge.2) then ! col-wise ntuple for cluster analysis
                                ! for calibration purposses
         
c     
c     
c     
         if(sane_ntuple_type.eq.2)then
        
         call HBNAME(id,'bevinfo',bgid,'bgid:I*4,bgtype:I*4,'//
     $        'btrigtype:I*4')
         call HBNAME(id,'bhits',ngooda,'ngooda:I*4,ngoodt:I*4,'//
     $        'ngoodta:I*4,ngoodtt:I*4,irowmax:I*4,icolmax:I*4,'//
     $        'max_adc:R*4')
         call HBNAME(id,'SANEY1',y1t_hit,
     $        'y1t_hit[0,900]:I*4,y1t_row(y1t_hit):I*4,'//
     $        'y1t_tdc(y1t_hit):I*4,'//
     $        'y1t_y(y1t_hit):R*4')
         call HBNAME(id,'SANEY2',y2t_hit,
     $        'y2t_hit[0,900]:I*4,y2t_row(y2t_hit):I*4,'//
     $        'y2t_tdc(y2t_hit):I*4,'//
     $        'y2t_y(y2t_hit):R*4')
         call HBNAME(id,'SANEX1',x1t_hit,
     $        'x1t_hit[0,900]:I*4,x1t_row(x1t_hit):I*4,'//
     $        'x1t_tdc(x1t_hit):I*4,'//
     $        'x1t_x(x1t_hit):R*4')
         call HBNAME(id,'SANECER',cer_hit,
     $        'cer_hit[0,50]:I*4,cer_num(cer_hit):I*4,'//
     $        'cer_tdc(cer_hit):I*4,cer_adcc(cer_hit):I*4')
         call HBNAME(id,'SANEADC',ceradc_hit,
     $        'ceradc_hit[0,15]:I*4,ceradc_num(ceradc_hit):I*4,'//
     $        'cer_adc(ceradc_hit):I*4')
         call HBNAME(id,'SANELUC',luc_hit,
     $        'luc_hit[0,90]:I*4,luc_row(luc_hit):I*4,'//
     $        'ladc_pos(luc_hit):I*4,ladc_neg(luc_hit):I*4,'//
     $        'ltdc_pos(luc_hit):I*4,ltdc_neg(luc_hit):I*4,'//
     $        'luc_y(luc_hit):R*4')
         
         
         endif

         if(bbypass_find_clusters.eq.0) then
            call HBNAME(id,'clustblock',nclust,
     $           'nclust[0,50]:I*4,ncellclust(nclust)[0,50]:I*4,'//
     $           'ncellbad(nclust)[0,50]:I*4,'//
     $           'ncellx(nclust),ncelly(nclust),iycell(50,nclust),'//
     $           'ixcell(50,nclust),cellbad(50,nclust):L,'//
     $           'xcell(50,nclust),ycell(50,nclust),'//
     $           'eblock(50,nclust),ablock(50,nclust),'//
     $           'xmoment(nclust),ymoment(nclust),'//
     $           'eclust(nclust),aclust(nclust),'//
     $           'xclust(nclust),yclust(nclust)')
            if(bbypass_calc_cluster_time.eq.0) then
               if(bbypass_sum8.eq.0) then
                  call HBNAME(id,'clusttdc',nclust8,
     $                 'nclust8[0,50]:I*4,'//
     $                 'ncell8clust(nclust8)[0,10]:I*4,'//
     $                 'irow8hit(10,nclust8)[0,56]:I*4,'//
     $                 'icol8hit(10,nclust8)[0,4]:I*4,'//
     $                 'nhit8clust(10,nclust8)[0,8]:I*4,'//
     $                 's8(10,nclust8),'//
     $                 'tcell8(10,8,nclust8),tclust8(nclust8),'//
     $                 'tcut8(nclust8),tcut8cor(nclust8),'//
     $                 'trms8(nclust8)')
 
               endif
               
               if(bbypass_sum64.eq.0) then
                  call HBNAME(id,'clusttrig',nclust64,
     $                 'nclust64[0,50]:I*4,'//
     $                 'ncell64clust(nclust64)[0,6]:I*4,'//
     $                 'irow64hit(6,nclust64)[0,19]:I*4,'//
     $                 'icol64hit(6,nclust64)[0,2]:I*4,'//
     $                 'nhit64clust(6,nclust64)[0,8]:I*4,'//
     $                 'tcell64(6,8,nclust64),a64(6,nclust64),'//
     $                 's64(6,nclust64),tclust64(nclust64),'//
     $                 'tcut64(nclust64),tcut64cor(nclust64),'//
     $                 'trms64(nclust64)')
                endif
            endif
            
            
            
            if(bbypass_calc_physics.eq.0)then
               call HBNAME(id,'clustphys',ntrack,'ntrack[0,50]:I*4,'//
     $              'ibest[0,50]:I*4,thetarad(ntrack),'//
     $              'phirad(ntrack),energy(ntrack),'//
     $              'xface(ntrack),yface(ntrack),'//
     $              'zface(ntrack),px(ntrack),py(ntrack),pz(ntrack),'//
     $              'ctime_clust(ntrack)')
            endif
            if(sane_ntuple_type.eq.2)then
               call HBNAME(id,'bad_clust',nmax,'nmax[0,50]:I*4,'//
     $              'edge_max(nmax):L,not_enough(nmax):L,'//
     $              'too_long_x(nmax):L,too_long_y(nmax):L,'//
     $              'below_thresh(nmax):L,above_max(nmax):L,'//
     $              'second_max(nmax):L')
            endif
         endif
         
c     
c     
c     
      endif
c         isane_plots = 100
      if(isane_plots.ne.100)then
         isane_plots = 100
cccc     ! For physics analysis  added on Jul 3 2008
c         call HBOOK2(10100,'TRACK X1',64, 1.,  65., 200,   -7500., -4500.,0.)
c         call HBOOK2(10101,'TRACK Y1',128,1., 129., 200,   -7500., -4500.,0.)
c         call HBOOK2(10102,'TRACK Y2',128,1., 129., 200,   -7500., -4500.,0.)
c
         call HBOOK2(10103,'TRACK X1 cer',64, 1.,  65., 200,   -7500., -4500.,0.)
         call HBOOK2(10104,'TRACK Y1 cer',128,1., 129., 200,   -7500., -4500.,0.)
         call HBOOK2(10105,'TRACK Y2 cer',128,1., 129., 200,   -7500., -4500.,0.)
         call HBOOK2(10106,'TRACK X1 vs BIG',64,-22., 22., 64,   -22., 22.,0.)
         call HBOOK2(10107,'TRACK Y1 vs BIG',128,-22., 22., 128,   -22., 22.,0.)
         call HBOOK2(10108,'TRACK Y2 vs BIG',128,-22., 22., 128,   -22., 22.,0.)
c         call HBOOK2(10109,'TRACK Y2 vs BIG',128,1., 129., 128,   -22., 22.,0.)


c         call HBOOK2(10111,'CER TDC',8, 1.,  9., 200,    -4000, 500., 0.)
c         call HBOOK2(10112,'CER ADC',8, 1.,  9., 100,    0.1, 5000., 0.)
c         call HBOOK1(10113,'BIGCAL TDC',400, 300.,400,0.)
c         call HBOOK2(10114,'BIGCAL TDC vs CER TDC',100, 310,  350., 100, -3000, -1000., 0.)
         do histnum=1,18
            call HBOOK2(17100+histnum,'Aclust vs cer TDC ',100, 0.,  2000., 200,    -3000, -1000., 0.)
            call HBOOK2(17200+histnum,'Aclust vs cer TDC ',100, 0.,  2000., 200,    -3000, -1000., 0.)
            call HBOOK2(18100+histnum,'Aclust vs cer TDC ',100, 0.,  2000., 100,    300, 400., 0.)
            call HBOOK2(18200+histnum,'Aclust vs cer TDC ',100, 0.,  2000., 100,    300, 400., 0.)
         enddo
            
         do histnum=1,8
            call HBOOK2(10500+histnum,'CER ADC vs TDC ',100, 10.,  2000., 200,    -3000, -1000., 0.)
            call HBOOK2(10520+histnum,'Aclust vs cer TDC ',100, 0.,  2000., 200,    -3000, -1000., 0.)
            call HBOOK2(10530+histnum,'Aclust vs cer TDC cor ',100, 0.,  2000., 200,    -3000, -1000., 0.)
            call HBOOK2(10510+histnum,'BIGCAL' ,33,0., 33.,  56,    0.,   56., 0.)
            call HBOOK2(10540+histnum,'CER ADC vs TDC ',100, 10.,  2000., 200,    -3000, -1000., 0.)
            call HBOOK2(10560+histnum,'TRIGBIG vs Cer TDC ',30, 25.,  55., 200,    -3000, -1000., 0.)
            call HBOOK2(10570+histnum,'TRIGBETA vs Cer TDC ',30, 45.,  75., 200,    -3000, -1000., 0.)
            call HBOOK2(10580+histnum,'TRIGBIG C vs Cer TDC ',30, 25.,  55., 200,    -3000, -1000., 0.)

c            if(grun.le.72487)then
               call HBOOK2(10710+histnum,'BIGCAL TDC vs CER TDC',20, 1,  21., 200, -500, 1000., 0.)
               call HBOOK2(10720+histnum,'BIGCAL TDC vs CER TDC',20, 1,  21., 200, -500, 1000., 0.)
c            else
c               call HBOOK2(10710+histnum,'BIGCAL TDC vs CER TDC',20, 1,  21., 60, -30, 30., 0.)
c               call HBOOK2(10720+histnum,'BIGCAL TDC vs CER TDC',20, 1,  21., 60, -30, 30., 0.)
                  
c            endif
            call HBOOK2(10730+histnum,'BIGCAL TDC vs CER TDC',20, 1,  21., 200, -500, 1000., 0.)
            call HBOOK2(10740+histnum,'BIGCAL TDC vs CER TDC',20, 1,  21., 200, -500, 1000., 0.)

         enddo
         call HBOOK2(10121,'LUC TDCPOS',28,1., 29., 200, -3500., 0., 0.)
         call HBOOK2(10122,'LUC TDCNEG',28,1., 29., 200, -3500., 0., 0.)
c         call HBOOK2(10125,'LUC ADCPOS',28,1., 29., 200,    0.1, 4000., 0.)
c         call HBOOK2(10126,'LUC ADCNEG',28,1., 29., 200,    0.1, 4000., 0.)
c         call HBOOK2(10131,'LUC TDCPOS cut',28,1., 29., 1500, -2250., -850., 0.)
c         call HBOOK2(10132,'LUC TDCNEG cut',28,1., 29., 1500, -2250., -850., 0.)
c         call HBOOK2(10135,'LUC ADCPOS cut',28,1., 29., 200,    0.1, 4000., 0.)
c         call HBOOK2(10136,'LUC ADCNEG cut',28,1., 29., 200,    0.1, 4000., 0.)
         if(sane_ntuple_type.eq.1) then 
            call HBOOK2(10128,'LUC_Y vs BIG_Y',40,-120., 120., 60,  -120., 120., 0.)
            do ii=1,28
cc     call HBPROF(10150+ii,'LUC_X vs BIG_X',100,-60., 60., 100,  -60., 60., 0.)
c               call HBPROF(10150+ii,'LUC_X vs BIG_X',100,-65., 65., -60,  60., 'S')
c               call HBOOK2(20250+ii,'Big_Y vs BIG_X',100,-100., 100, 120., -120,  120., 0.)
               call HBOOK2(20150+ii,'LUC_X vs BIG_X',100,-100., 300, 80., -60,  100., 0.)
            enddo
         

c            call HBOOK2(10220,'LUC_Y vs BIG_Y', 60,  -120., 120., 120,-20., 20., 0.)
c            call HBOOK2(10221,'LUC_Y vs BIG_Y', 60,  -120., 120., 120,-20., 20., 0.)
c            call HBOOK2(10221,'LUC_Y vs BIG_Yo',120,-20., 20., 60,  -120., 120., 0.)
c            call HBOOK2(10222,'dLUC_Y vs BIG_X', 80,-80., 80., 60,  -20., 20., 0.)
c            call HBOOK2(10223,'LUC_X vs BIG_Xo',60,-20., 20., 60,  -120., 120., 0.)
cc           call HBOOK2(10225,'dLUC_Y vs e',120,-30., 30., 40,  0., 2.4, 0.)
c           call HBPROF(10225,'dLUC_Y vs e',40, 0., 2.6, -20,  20., 'S')
c           call HBOOK2(10226,'dLUC_X vs e',120,-30., 30., 16,  0.4, 2., 0.)
c           call HBOOK2(10227,'Big_Y vs BIG_Yo', 120,  -120., 120.,120,-10., 10., 0.)

         endif
         call HBOOK2(10200,'BIGCAL' ,33,0., 33.,  56,    0.,   56., 0.)
         
         call HBOOK2(10210,'SLOW RASTER ADC' ,90,5000., 8000.,  90,    5000.,   8000., 0.)
         call HBOOK2(10211,'FAST RASTER ADC' ,90,2000., 5000.,  90,    2000.,   5000., 0.)
         call HBOOK2(10212,'SLOW RASTER ADC Corrected' ,90,-2.5, 2.5,  90,    -2.5,   2.5, 0.)
         call HBOOK2(10213,'FAST RASTER ADC Corrected' ,90,-2.5, 2.5,  90,    -2.5,   2.5, 0.)
         call HBOOK2(10214,'SEM X Y' ,90,-3., 3.,  90,    -3.,   3., 0.)
         call HBOOK2(10215,'SLOW RASTER ADC' ,90,5000., 8000.,  90,    5000.,   8000., 0.)
         call HBOOK2(10216,'SLOW RASTER ADC Corrected' ,90,-3., 3.,  90,    -3.,   3., 0.)
         
c         call HBOOK2(10300,'X_HMS vs xclust' ,60,-60., 60.,  60,    -60.,   60., 0.)
c         call HBOOK2(10301,'Y_HMS vs yclust' ,120,-120., 120.,  120,    -120.,   120., 0.)
c         call HBOOK2(10302,'X_HMS vs Y_HMS' ,60,-60., 60.,  120,    -120.,   120., 0.)
c         call HBOOK2(10303,'Xclust vs Yclust' ,60,-60., 60.,  120,    -120.,   120., 0.)
c         call HBOOK2(10304,'DX vs DY' ,40,-40., 40.,  60,    -60.,   60., 0.)
c         call HBOOK2(10310,'hsdelta vs hsyptar' ,40,-20., 20.,  100, -0.1,   0.1, 0.)
c         call HBOOK2(10311,'hsdelta vs hsxptar' ,40,-20., 20.,  200, -0.1,   0.3, 0.)
c         call HBOOK2(10312,'dpel_hms vs hsyptar' ,40,-0.2, 0.2,  100, -0.1,   0.1, 0.)
c         call HBOOK2(10313,'dpel_hms vs hsxptar' ,40,-0.2, 0.2,  200, -0.1,   0.3, 0.)
c         call HBOOK2(10314,'dpel_hms vs hsxtar' ,40,-0.2, 0.2,  100, -3,   3, 0.)
c         call HBOOK2(10315,'dpel_hms vs hsytar' ,40,-0.2, 0.2,  100, -3,   3, 0.)
c         call HBOOK2(10316,'raster_x vs xtar' ,100,-3., 3.,  100, -3.,   3., 0.)
c         call HBOOK2(10317,'raster_y vs ytar' ,100,-3., 3.,  100, -3.,   3., 0.)
         
         
         
c         call HBOOK1(10321, 'hsdelta' , 100,-.1,.1,0.)
c         call HBOOK1(10322, 'W2', 100, -0.3, 0.6,0.)
c         call HBOOK2(10323,' W2 vs hsxtar' ,100,-0.3, 0.3,  100, -0.1,   0.1, 0.)
c         call HBOOK2(10324,' W2 vs hsxtar' ,100,-0.3, 0.3,  100, -3,   3, 0.)
         
         
         call HBOOK2(10550,'Ytracker-Yrec vs P' ,100, 0.7, 1.5,  100, -1.5,   1.5, 0.)
         call HBOOK2(10551,'Xtracker-Xrec vs P' ,100, 0.7, 1.5,  100, -1.5,   1.5, 0.)
         
c         call HBOOK1(10601,'Xbj 2.5<Q2<3.5 helicity normalized' ,30, 0.2, 2.0 ,0.)
c         call HBOOK1(10611,'Xbj 2.5<Q2<3.5 no helicity normalized' ,30, 0.2, 2.0 ,0.)
c         call HBOOK1(10602,'Xbj 3.5<Q2<4.5 helicity normalized' ,30, 0.2, 2.0 ,0.)
c         call HBOOK1(10612,'Xbj 3.5<Q2<4.5 no helicity normalized' ,30, 0.2, 2.0 ,0.)
c         call HBOOK1(10603,'Xbj 4.5<Q2<5.5 helicity normalized' ,30, 0.2, 2.0 ,0.)
c         call HBOOK1(10613,'Xbj 4.5<Q2<5.5 no helicity normalized' ,30, 0.2, 2.0 ,0.)
c         call HBOOK1(10604,'Xbj 5.5<Q2<6.5 helicity normalized' ,30, 0.2, 2.0 ,0.)
c         call HBOOK1(10614,'Xbj 5.5<Q2<6.5 no helicity normalized' ,30, 0.2, 2.0 ,0.)
         
c         call HBOOK2(10620,'Xbj vs Q2' ,140, 0.1, 1.5,  100, 0.,  8., 0.)
c         call HBOOK2(10621,'W2 vs Q2' ,100, 0.5, 2.5,  100, 0.,  8., 0.)
         call HBOOK1(10622,'Mpi0' ,200, 0.05, 0.25, 0.)
         call HBOOK1(10623,'Mpi0_angles' ,200, 0.05, 0.25, 0.)
c
c
c     Physics Histograms
c     
      endif

      if(sane_ntuple_type.lt.4.and.sane_ntuple_type.ge.4)then

          call HBNAME(id,'TRIGGERTIME',T_trgHMS,
     $        'T_trgHMS:R*4, T_trgBIG:R*4, T_trgPI0:R*4,'//
     $        'T_trgBETA:R*4, T_trgCOIN1:R*4,T_trgCOIN2:R*4')
          call HBNAME(id,'SANECER',cer_hit,
     $        'cer_hit[0,50]:I*4,cer_num(cer_hit):I*4,'//
     $        'cer_tdc(cer_hit):I*4,cer_adcc(cer_hit):I*4')

      endif
      if(sane_ntuple_type.lt.3) then ! col-wise ntuple
         call HBNAME(id,'HMSINFO',hms_p,
     $        'hms_p:R*4,hms_e:R*4,hms_theta:R*4,hms_phi:R*4,'//
     $        'hsxfp_s:R*4,hsyfp_s:R*4,hsxpfp_s:R*4,hsypfp_s:R*4,'//
     $        'hms_xtar:R*4,hms_ytar:R*4,hms_yptar:R*4,'//
     $        'hms_xptar:R*4,hms_delta:R*4,hms_start:R*4,'//
     $        'hsshtrk_s:R*4, hsshsum_s:R*4, hsbeta_s:R*4,'//
     $        'rast_x:R*4,rast_y:R*4,'//
     $        'slow_rast_x:R*4,slow_rast_y:R*4,'//
     $        'sem_x:R*4,sem_y:R*4,'//
     $        'i_helicity:I*4,'//
     $        'hms_cer_npe1:R*4,hms_cer_npe2:R*4,'//
     $        ' hms_cer_adc1:R*4,hms_cer_adc2:R*4')
      endif
      
      if(sane_ntuple_type.eq.1) then ! col-wise ntuple
                                 ! for Physics purposses

         call HBNAME(id,'bevinfo',bgid,'bgid:I*4,bgtype:I*4,'//
     $        'btrigtype:I*4')
         call HBNAME(id,'bhits',ngooda,'ngooda:I*4,ngoodt:I*4,'//
     $        'ngoodta:I*4,ngoodtt:I*4,irowmax:I*4,icolmax:I*4,'//
     $        'max_adc:R*4')
         
         
         
         call HBNAME(id,'SANEPHYS',n_clust,
     $        'n_clust[0,15]:I*4,'//
     $        'E_clust(n_clust):R*4,'//
     $        'X_clust(n_clust):R*4, Y_clust(n_clust):R*4,'//
     $        'Z_clust(n_clust):R*4,'//
     $        'X_clust_r(n_clust):R*4, Y_clust_r(n_clust):R*4,'//
     $        'Z_clust_r(n_clust):R*4,'//
     $        'luc_h(n_clust)[0,20]:I*4,'//
     $        'X_luc(20,n_clust), Y_luc(20,n_clust), Z_luc(20,n_clust),'//
     $        'X_luc_r(20,n_clust),Y_luc_r(20,n_clust),'//
     $        'Z_luc_r(20,n_clust),'//
     $        'trc_hx(n_clust)[0,20]:I*4,'//
     $        'X_trc(20,n_clust),'//
     $        'Z_trc(20,n_clust),'//
     $        'trc_hy1(n_clust)[0,20]:I*4,'//
     $        'Y1_trc(20,n_clust),'//
     $        'Z1_trc(20,n_clust),'//
     $        'trc_hy2(n_clust)[0,20]:I*4,'//
     $        'Y2_trc(20,n_clust),'//
     $        'Z2_trc(20,n_clust),'//
     $        'Tr_Vertex(3,n_clust), Tr_Vertex_r(3,n_clust),'//
     $        'cer_h(n_clust)[0,20]:I*4,'//
     $        'cer_geom(n_clust)[0,20]:I*4,'//
     $        'cerb_time(n_clust):I*4,'//
     $        'cerb_adc(n_clust):I*4,'//
     $        'bigc_time(n_clust):I*4,'//
     $        'bigc_adc(n_clust):I*4,'//
     $        'cerbc_num(n_clust):I*4,'//
     $        'Theta_e(n_clust):R*4,Phi_e(n_clust):R*4,'//
     $        'Delta_Y(n_clust):R*4,Delta_X(n_clust):R*4,'//
     $        'X_Bjorken(n_clust):R*4, Q2(n_clust):R*4,'//
     $        ' W2(n_clust):R*4, ENue(n_clust):R*4')
c         call HBNAME(id,'SANEADC',ceradc_hit,
c     $        'ceradc_hit[0,15]:I*4,ceradc_num(ceradc_hit):I*4,'//
c     $        'cer_adc(ceradc_hit):I*4')

      endif
     
      call HCDIR(sane_ntuple_directory,'R') ! record ntuple directory
      write(*,*)'SANE DIR ',sane_ntuple_directory
      call HCDIR(directory,' ') !reset CERNLIB directory
      
      sane_ntuple_exists = HEXIST(sane_ntuple_id)
      
      abort = .not.sane_ntuple_exists
      
      iv(1) = id
      iv(2) = io
      pat = 'Ntuple id#$ [' // sane_ntuple_directory // '/]' //
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
