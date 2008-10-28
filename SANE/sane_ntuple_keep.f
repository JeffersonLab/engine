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




      luc_hit         =  LUCITE_SANE_RAW_TOT_HITS

      do i=1,LUCITE_SANE_RAW_TOT_HITS
            luc_row(i)   =  LUCITE_SANE_RAW_COUNTER_NUM(i)
            ladc_pos(i)  =  LUCITE_SANE_RAW_ADC_POS(i) - luc_ped_mean_pos(luc_row(i))
            ladc_neg(i)  =  LUCITE_SANE_RAW_ADC_NEG(i) - luc_ped_mean_neg(luc_row(i))
            call CORRECT_RAW_TIME_SANE(LUCITE_SANE_RAW_TDC_POS(i),ltdc_pos(i))
            call CORRECT_RAW_TIME_SANE(LUCITE_SANE_RAW_TDC_NEG(i),ltdc_NEG(i))

            luc_y(i)     =  -82.35 + (luc_row(i)-1)*6.1
            call HFILL(10121, float(luc_row(i)), float(ltdc_pos(i))*LUCITE_SANE_TDC_TIMING(luc_row(i)), 1.)
            call HFILL(10122, float(luc_row(i)), float(ltdc_neg(i))*LUCITE_SANE_TDC_TIMING(luc_row(i)), 1.)
            call HFILL(10125, float(luc_row(i)), float(ladc_pos(i)), 1.)
            call HFILL(10126, float(luc_row(i)), float(ladc_neg(i)), 1.)

      enddo

      cer_hit         =  CERENKOV_SANE_RAW_TOT_HITS
      do i=1,CERENKOV_SANE_RAW_TOT_HITS
            cer_num(i)   =  CERENKOV_SANE_RAW_COUNTER_NUM(i)
            call CORRECT_RAW_TIME_SANE(CERENKOV_SANE_RAW_TDC(i),cer_tdc(i))
            cer_adc(i)   =  CERENKOV_SANE_RAW_ADC(i)-cer_sane_ped_mean(cer_num(i))
            call HFILL(10111,float(cer_num(i)),float(cer_tdc(i))*CER_SANE_TDC_TIMING(cer_num(i)), 1.)
            call HFILL(10112,float(cer_num(i)),float(cer_adc(i)), 1.)
            call HFILL(10500+cer_num(i),float(cer_adc(i)),
     ,           float(cer_tdc(i))*CER_SANE_TDC_TIMING(cer_num(i)),1.)

      enddo
 
      x1t_hit         =  TRACKER_SANE_RAW_TOT_HITS_X
      if(x1t_hit.gt.300) go to 10
      do i=1,x1t_hit
         x1t_row(i)   =  TRACKER_SANE_RAW_COUNTER_X(i)
         call CORRECT_RAW_TIME_SANE(TRACKER_SANE_RAW_TDC_X(i),x1t_tdc(i))
         x1t_x(i)     =  -12.32+0.37422*(x1t_row(i)-1)
      enddo

      do i=1,TRACKER_SANE_RAW_TOT_HITS_Y
         if(TRACKER_SANE_RAW_TDC_Y(i).lt.10000.and.
     ,        TRACKER_SANE_RAW_TDC_Y(i).gt.0)then
            if(TRACKER_SANE_RAW_COUNTER_Y(i).lt.129)then
               y1t_hit            =  y1t_hit + 1 
               if(y1t_hit.gt.300) go to 10
               y1t_row(y1t_hit)   =  TRACKER_SANE_RAW_COUNTER_Y(i)
               call CORRECT_RAW_TIME_SANE(TRACKER_SANE_RAW_TDC_Y(i),y1t_tdc(y1t_tdc(y1t_hit)))
               y1t_y(y1t_hit)     =  -22.225+(y1t_row(y1t_hit)-1)*0.35
             else if(TRACKER_SANE_RAW_COUNTER_Y(i).lt.257)then
               y2t_hit            =  y2t_hit + 1 
               if(y2t_hit.gt.300) go to 10
               y2t_row(y2t_hit)   =  TRACKER_SANE_RAW_COUNTER_Y(i)-128
               call CORRECT_RAW_TIME_SANE(TRACKER_SANE_RAW_TDC_Y(i),y2t_tdc(y2t_tdc(y2t_hit)))
               y2t_y(y2t_hit)     =  -22.4+(y2t_row(y2t_hit)-1)*0.35
            endif
         endif
      enddo
          do inum=1,nclust
             do ihit=1,x1t_hit
                IF(abs(x1t_x(ihit)-TrackerX_SHIFT(1)-
     ,               TrackerX_SHIFT(3)/Bigcal_SHIFT(3)*
     ,               (xclust(inum))-Bigcal_SHIFT(1)).lt.0.6)then
                   call HFILL(10100,float(x1t_row(ihit)),float(x1t_tdc(ihit))*0.06,1.)
                ENDIF
             enddo
             do ihit=1,y1t_hit
                IF(abs(y1t_y(ihit)-TrackerY1_SHIFT(2)-
     ,               TrackerY1_SHIFT(3)/Bigcal_SHIFT(3)*
     ,               (yclust(inum))-Bigcal_SHIFT(2)).lt.0.6)then
                   call HFILL(10101,float(y1t_row(ihit)),float(y1t_tdc(i))*0.06,1.)
                ENDIF
             enddo
             do ihit=1,y2t_hit
                IF(abs(y2t_y(ihit)-TrackerY2_SHIFT(2)-
     ,               TrackerY2_SHIFT(3)/Bigcal_SHIFT(3)*
     ,               (yclust(inum))-Bigcal_SHIFT(2)).lt.0.6)then
                   call HFILL(10102,float(y2t_row(ihit)),float(y2t_tdc(i))*0.06,1.) ! 0.06 is ns convertion
                ENDIF
             enddo
         enddo

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

      endif
      rast_x       = gfry_raw_adc
      rast_y       = gfrx_raw_adc
      i_helicity   = gbeam_helicity_ADC
      slow_rast_x  = gsr_beamx
      slow_rast_y  = gsr_beamy
      sem_x        = ntbpmx
      sem_y        = ntbpmy
      call HFILL(10210,gsry_raw_adc,gsrx_raw_adc, 1.)
      call HFILL(10211,gfry_raw_adc,gfrx_raw_adc, 1.)

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

             CORRECTED_TDC =  RAW_TDC - 
     ,           TRIGGER_F1_START_TDC_COUNTER(SANE_TRIGGER_COUNTER)
c
c     Taking care of ROLOVER For positive TDC
c     
            if(CORRECTED_TDC.lt.-30000)
     ,           CORRECTED_TDC = CORRECTED_TDC+TRIGGER_F1_ROLOVER(SANE_TRIGGER_COUNTER)
            if(CORRECTED_TDC.gt.30000)
     ,           CORRECTED_TDC = CORRECTED_TDC-TRIGGER_F1_ROLOVER(SANE_TRIGGER_COUNTER)

      end
