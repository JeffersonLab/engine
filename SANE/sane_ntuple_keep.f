      subroutine sane_ntuple_keep(ABORT,err)

      implicit none
      save

      character*13 here
      parameter(here='sane_ntuple_keep')

      logical abort
      character*(*) err
      integer i,j

      include 'b_ntuple.cmn'
      include 'bigcal_data_structures.cmn'
      include 'bigcal_tof_parms.cmn'
      include 'hms_data_structures.cmn'
      include 'bigcal_gain_parms.cmn'
      include 'gen_event_info.cmn'
      include 'gen_data_structures.cmn'
      include 'gep_data_structures.cmn'
      include 'sane_ntuple.cmn'
      include 'sane_data_structures.cmn'
      include 'sem_data_structures.cmn'
      INCLUDE 'h_ntuple.cmn'
      include 'f1trigger_data_structures.cmn'
      include 'hms_calorimeter.cmn'
      include 'gen_detectorids.par'
      logical HEXIST ! CERNLIB function
      integer t_sane,l_sane,cer_sane
      integer icycle,inum,ihit
c      logical middlebest

      real Mp
      parameter(Mp=.938272)


      err=' '
      ABORT=.false.

      if(sane_ntuple_max_segmentevents.gt.0) then
         if(sane_ntuple_segmentevents.gt.sane_ntuple_max_segmentevents) then
            call sane_ntup_change(ABORT,err)
            sane_ntuple_segmentevents=0
         else 
            sane_ntuple_segmentevents = sane_ntuple_segmentevents + 1
         endif
      endif

      if(.not.sane_ntuple_exists) return




      luc_hit = 0
      do i=1,LUCITE_SANE_RAW_TOT_HITS
         if(LUCITE_SANE_RAW_TDC_POS(i).gt.0.or.LUCITE_SANE_RAW_TDC_NEG(i).gt.0)then
            luc_hit         =  luc_hit+1
            luc_row(luc_hit)   =  LUCITE_SANE_RAW_COUNTER_NUM(i)
            ladc_pos(luc_hit)  =  LUCITE_SANE_RAW_ADC_POS(i) - luc_ped_mean_pos(luc_row(luc_hit))
            ladc_neg(luc_hit)  =  LUCITE_SANE_RAW_ADC_NEG(i) - luc_ped_mean_neg(luc_row(luc_hit))
            call CORRECT_RAW_TIME_SANE(LUCITE_SANE_RAW_TDC_POS(i),ltdc_pos(luc_hit))
            call CORRECT_RAW_TIME_SANE(LUCITE_SANE_RAW_TDC_NEG(i),ltdc_NEG(luc_hit))
c            write(*,*)LUCITE_SANE_RAW_TDC_POS(i),ltdc_pos(luc_hit),LUCITE_SANE_RAW_TDC_NEG(i),ltdc_neg(luc_hit),luc_row(luc_hit)

            luc_y(luc_hit)     =  -82.35 + (luc_row(luc_hit)-1)*6.1
            call NANcheck(luc_hit,LUCITE_SANE_ID)
            call NANcheck(luc_row(luc_hit),LUCITE_SANE_ID)
            call NANcheck(ladc_neg(luc_hit),LUCITE_SANE_ID)
            call NANcheck(ladc_pos(luc_hit),LUCITE_SANE_ID)
            call NANcheck(ltdc_neg(luc_hit),LUCITE_SANE_ID)
            call NANcheck(ltdc_pos(luc_hit),LUCITE_SANE_ID)
            
            call HFILL(10121, float(luc_row(luc_hit)), float(ltdc_pos(luc_hit)), 1.)
            call HFILL(10122, float(luc_row(luc_hit)), float(ltdc_neg(luc_hit)), 1.)
            call HFILL(10125, float(luc_row(luc_hit)), float(ladc_pos(luc_hit)), 1.)
            call HFILL(10126, float(luc_row(luc_hit)), float(ladc_neg(luc_hit)), 1.)
         endif
      enddo
      cer_hit = 0
      do i=1,CERENKOV_SANE_RAW_TOT_HITS
         if(CERENKOV_SANE_RAW_TDC(i).gt.0)then
            cer_hit         =  cer_hit+1
            cer_num(cer_hit)   =  CERENKOV_SANE_RAW_COUNTER_NUM(i)
            call CORRECT_RAW_TIME_SANE(CERENKOV_SANE_RAW_TDC(i),cer_tdc(cer_hit))
            cer_adc(cer_hit)   =  CERENKOV_SANE_RAW_ADC(i)-cer_sane_ped_mean(cer_num(cer_hit))
            call NANcheck(cer_hit,CERENKOV_SANE_ID)
            call NANcheck(cer_num(cer_hit),CERENKOV_SANE_ID)
            call NANcheck(cer_tdc(cer_hit),CERENKOV_SANE_ID)
            call NANcheck(cer_adc(cer_hit),CERENKOV_SANE_ID)

            call HFILL(10111,float(cer_num(cer_hit)),float(cer_tdc(cer_hit)), 1.)
            call HFILL(10112,float(cer_num(cer_hit)),float(cer_adc(cer_hit)), 1.)
            call HFILL(10500+cer_num(cer_hit),float(cer_adc(cer_hit)),
     ,           float(cer_tdc(cer_hit)),1.)
         endif

      enddo
 
      x1t_hit         =  0
      if(x1t_hit.gt.300) go to 10
      do i=1,TRACKER_SANE_RAW_TOT_HITS_X
         if(TRACKER_SANE_RAW_TDC_X(i).gt.0)then
            x1t_hit         =  x1t_hit+1
            x1t_row(x1t_hit)   =  TRACKER_SANE_RAW_COUNTER_X(i)
            call CORRECT_RAW_TIME_SANE(TRACKER_SANE_RAW_TDC_X(i),x1t_tdc(x1t_hit))
            x1t_x(x1t_hit)     =  -12.32+0.37422*(x1t_row(x1t_hit)-1)
            call NANcheck(x1t_hit,TRACKER_SANE_X_ID)
            call NANcheck(x1t_row(x1t_hit),TRACKER_SANE_X_ID)
            call NANcheck(x1t_tdc(x1t_hit),TRACKER_SANE_X_ID)
         endif
      enddo
      y1t_hit=0
      y2t_hit=0
      do i=1,TRACKER_SANE_RAW_TOT_HITS_Y
         if(TRACKER_SANE_RAW_TDC_Y(i).lt.67000.and.
     ,        TRACKER_SANE_RAW_TDC_Y(i).gt.0)then
            if(TRACKER_SANE_RAW_COUNTER_Y(i).lt.129)then
               y1t_hit            =  y1t_hit + 1 
c               write(*,*)'Tracker TDC', y1t_hit,TRACKER_SANE_RAW_TDC_Y(i)
               if(y1t_hit.gt.300) go to 20
               y1t_row(y1t_hit)   =  TRACKER_SANE_RAW_COUNTER_Y(i)
               call CORRECT_RAW_TIME_SANE(TRACKER_SANE_RAW_TDC_Y(i),y1t_tdc(y1t_hit))
               y1t_y(y1t_hit)     =  -22.225+(y1t_row(y1t_hit)-1)*0.35
               call NANcheck(y1t_hit,TRACKER_SANE_Y_ID)
               call NANcheck(y1t_row(y1t_hit),TRACKER_SANE_Y_ID)
               call NANcheck(y1t_tdc(y1t_hit),TRACKER_SANE_Y_ID)

             else if(TRACKER_SANE_RAW_COUNTER_Y(i).lt.257)then
               y2t_hit            =  y2t_hit + 1 
               if(y2t_hit.gt.300) go to 20
               y2t_row(y2t_hit)   =  TRACKER_SANE_RAW_COUNTER_Y(i)-128
               call CORRECT_RAW_TIME_SANE(TRACKER_SANE_RAW_TDC_Y(i),y2t_tdc(y2t_hit))
               y2t_y(y2t_hit)     =  -22.4+(y2t_row(y2t_hit)-1)*0.35
               call NANcheck(y2t_hit,TRACKER_SANE_Y_ID)
               call NANcheck(y2t_row(y1t_hit),TRACKER_SANE_Y_ID)
               call NANcheck(y2t_tdc(y1t_hit),TRACKER_SANE_Y_ID)
            endif
         endif
 20      CONTINUE
      enddo
c          do inum=1,nclust
             do ihit=1,x1t_hit
c                IF(abs(x1t_x(ihit)-TrackerX_SHIFT(1)-
c     ,               TrackerX_SHIFT(3)/Bigcal_SHIFT(3)*
c     ,               (xclust(inum))-Bigcal_SHIFT(1)).lt.2.6)then
                   call HFILL(10100,float(x1t_row(ihit)),float(x1t_tdc(ihit)),1.)
c                ENDIF
             enddo
             do ihit=1,y1t_hit
c                IF(abs(y1t_y(ihit)-TrackerY1_SHIFT(2)-
c     ,               TrackerY1_SHIFT(3)/Bigcal_SHIFT(3)*
c     ,               (yclust(inum))-Bigcal_SHIFT(2)).lt.2.6)then
                   call HFILL(10101,float(y1t_row(ihit)),float(y1t_tdc(ihit)),1.)
c                ENDIF
             enddo
             do ihit=1,y2t_hit
c                IF(abs(y2t_y(ihit)-TrackerY2_SHIFT(2)-
c     ,               TrackerY2_SHIFT(3)/Bigcal_SHIFT(3)*
c     ,               (yclust(inum))-Bigcal_SHIFT(2)).lt.2.6)then
                   call HFILL(10102,float(y2t_row(ihit)),float(y2t_tdc(ihit)),1.) ! 0.06 is ns convertion
c                ENDIF
             enddo
c         enddo

      if(HSNUM_FPTRACK.gt.0)then
         hms_p        = h_Ntuple_contents(2)	
         hms_e        = h_Ntuple_contents(3)
         hms_theta    = h_Ntuple_contents(6)
         hms_phi      = h_Ntuple_contents(7)
         hsxfp_s      = hsx_fp
         hsyfp_s      = hsy_fp
         hsxpfp_s     = hsxp_fp
         hsypfp_s     = hsyp_fp
         hms_ytar     = h_Ntuple_contents(18)
         hms_yptar    = h_Ntuple_contents(20)
         hms_xptar    = h_Ntuple_contents(19)
         hms_delta    = h_Ntuple_contents(21)
         hms_start    = hstart_time
         hsshtrk_s    = HSTRACK_ET
         hsshsum_s    = hsshsum
         hsbeta_s     = hsbeta 
         hms_cer_npe1 = hcer_npe(1)
         hms_cer_npe2 = hcer_npe(2)
         hms_cer_adc1 = hcer_adc(1) 
         hms_cer_adc2 = hcer_adc(2)
         call HFILL(10302,X_HMS,Y_HMS,1.) 
c         if(nclust.eq.1)then
         do i=1,nclust
            call HFILL(10300,X_HMS,xclust(i)+Bigcal_SHIFT(1),1.) 
            call HFILL(10304,X_HMS-Xclust(i)-Bigcal_SHIFT(1),Y_HMS-Yclust(i)-Bigcal_SHIFT(2),1.)
            if(abs(X_HMS-xclust(i)).lt.20)then
               call HFILL(10301,Y_HMS,Yclust(i)+Bigcal_SHIFT(2),1.) 
               call HFILL(10303,Xclust(i)+Bigcal_SHIFT(1),Yclust(i)+Bigcal_SHIFT(2),1.)
            endif
         enddo
c         endif

      endif
      rast_x       = gfry_raw_adc
      rast_y       = gfrx_raw_adc
      i_helicity   = gbeam_helicity_ADC
      slow_rast_x  = -n_sr_slopex*(gsrx_raw_adc-n_sr_adcx_zero)
      slow_rast_y  = -n_sr_slopey*(gsry_raw_adc-n_sr_adcy_zero)
      sem_x        = ntbpmx
      sem_y        = ntbpmy
      T_trgHMS     = gmisc_dec_data(11,1)
      T_trgBIG     = gmisc_dec_data(12,1) 
      T_trgPI0     = gmisc_dec_data(13,1)
      T_trgBETA    = gmisc_dec_data(14,1)
      T_trgCOIN1   = gmisc_dec_data(15,1)
      T_trgCOIN2   = gmisc_dec_data(16,1)

      call HFILL(10210,gsry_raw_adc,gsrx_raw_adc, 1.)
      call HFILL(10211,gfry_raw_adc,gfrx_raw_adc, 1.)

      call HFILL(10212,-n_sr_slopey*(gsry_raw_adc-n_sr_adcy_zero),-n_sr_slopex*(gsrx_raw_adc-n_sr_adcx_zero), 1.)
      call HFILL(10213,gfry_raw_adc,gfrx_raw_adc, 1.)
      do i =1,  nclust
         call Bigcal_Betta(i) 
         do j=1, ncellclust(i)
            call HFILL(10200,float(ixcell(j,i)),float(iycell(j,i)), 1.)
         enddo
         if(sane_ntuple_type.eq.1)then
            n_clust = nclust
            call icer(i)
            call Lucite(i)
            call tracker(i)
            call TrackerCoordnate(i)
            call GeometryMatch(i)
         endif
      enddo
      abort=.not.HEXIST(sane_ntuple_ID)
      if(abort) then
         call G_build_note(':Ntuple ID#$ does not exist',
     $        '$',sane_ntuple_ID,' ',0.,' ',err)
         call G_add_path(here,err)
      else 
         icycle=999999
         call HFNT(sane_ntuple_ID)
         
      endif
 10   CONTINUE
      return 
      end

      SUBROUTINE CORRECT_RAW_TIME_SANE(RAW_TDC,CORRECTED_TDC)
      IMPLICIT NONE
      include 'sane_data_structures.cmn'
      include 'f1trigger_data_structures.cmn'
c
c     Function arguments are RAW_TDC -raw TDC value
c     and CORRECTED_TDC -Corrected by Trigger time and rolover time 
c     MAKE SURE TO Include correct parameter files
c
c
      integer*4 RAW_TDC, CORRECTED_TDC, f1trigmax
      save

c find largest value of trigger time, to check rollover
      if(TRIGGER_F1_START_TDC_COUNTER(
     >        SANE_TRIGGER_COUNTER) .gt.f1trigmax) then
        write(6,'('' SANE trigger time max='',i8)')
     >  TRIGGER_F1_START_TDC_COUNTER(
     >        SANE_TRIGGER_COUNTER)
        f1trigmax = 
     >  TRIGGER_F1_START_TDC_COUNTER(
     >        SANE_TRIGGER_COUNTER)
      endif
      if(RAW_TDC.gt.0)then
         CORRECTED_TDC =  RAW_TDC - 
     ,        TRIGGER_F1_START_TDC_COUNTER(SANE_TRIGGER_COUNTER)
c
c     Taking care of ROLOVER For positive TDC
c     
         if(CORRECTED_TDC.lt.-30000)
     ,        CORRECTED_TDC = CORRECTED_TDC+TRIGGER_F1_ROLOVER(SANE_TRIGGER_COUNTER)
         if(CORRECTED_TDC.gt.30000)
     ,        CORRECTED_TDC = CORRECTED_TDC-TRIGGER_F1_ROLOVER(SANE_TRIGGER_COUNTER)
      else
         CORRECTED_TDC =0
      endif 

      end

      subroutine NANcheck(l,did)
      IMPLICIT NONE
      integer*4 l
      integer did
      if(l.ne.l)then
         l=0
         write(*,*)did
      endif
      end
