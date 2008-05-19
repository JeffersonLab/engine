      subroutine sane_ntuple_keep(ABORT,err)

      implicit none
      save

      character*13 here
      parameter(here='sane_ntuple_keep')

      logical abort
      character*(*) err
      integer i

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
      INCLUDE 'h_ntuple.cmn'

      logical HEXIST ! CERNLIB function

c      logical middlebest

      real Mp
      parameter(Mp=.938272)


      err=' '
      ABORT=.false.
c      write(*,*)"SANE SEG", sane_ntuple_max_segmentevents, sane_ntuple_segmentevents
      if(sane_ntuple_max_segmentevents.gt.0) then
         if(sane_ntuple_segmentevents.gt.sane_ntuple_max_segmentevents) then
            call sane_ntup_change(ABORT,err)
            sane_ntuple_segmentevents=0
         else 
            sane_ntuple_segmentevents = sane_ntuple_segmentevents + 1
         endif
      endif

      if(.not.sane_ntuple_exists) return

c      write(*,*)"CLUSTER ENERGY",nclust,eclust


      luc_hit         =  LUCITE_SANE_RAW_TOT_HITS

      do i=1,LUCITE_SANE_RAW_TOT_HITS
            luc_row(i)   =  LUCITE_SANE_RAW_COUNTER_NUM(i)
            ladc_pos(i)  =  LUCITE_SANE_RAW_ADC_POS(i) - luc_ped_mean_pos(luc_row(i))
            ladc_neg(i)  =  LUCITE_SANE_RAW_ADC_NEG(i) - luc_ped_mean_neg(luc_row(i))
            ltdc_pos(i)  =  LUCITE_SANE_RAW_TDC_POS(i)
            ltdc_neg(i)  =  LUCITE_SANE_RAW_TDC_NEG(i)
            luc_y(i)     =  -82.35 + (luc_row(i)-1)*6.1
      enddo

      cer_hit         =  CERENKOV_SANE_RAW_TOT_HITS
      do i=1,CERENKOV_SANE_RAW_TOT_HITS
            cer_num(i)   =  CERENKOV_SANE_RAW_COUNTER_NUM(i)
            cer_tdc(i)   =  CERENKOV_SANE_RAW_TDC(i)
            cer_adc(i)   =  CERENKOV_SANE_RAW_ADC(i)-cer_sane_ped_mean(cer_num(i))

      enddo
 
      x1t_hit         =  TRACKER_SANE_RAW_TOT_HITS_X
      if(x1t_hit.gt.300) go to 10
      do i=1,x1t_hit
         x1t_row(i)   =  TRACKER_SANE_RAW_COUNTER_X(i)
         x1t_tdc(i)   =  TRACKER_SANE_RAW_TDC_X(i)
         x1t_x(i)     =  -12.32+0.37422*(x1t_row(i)-1)
      enddo
      do i=1,TRACKER_SANE_RAW_TOT_HITS_Y
         if(TRACKER_SANE_RAW_TDC_Y(i).lt.10000.and.
     ,        TRACKER_SANE_RAW_TDC_Y(i).gt.0)then
            if(TRACKER_SANE_RAW_COUNTER_Y(i).lt.129)then
               y1t_hit            =  y1t_hit + 1 
               if(y1t_hit.gt.300) go to 10
               y1t_row(y1t_hit)   =  TRACKER_SANE_RAW_COUNTER_Y(i)
               y1t_tdc(y1t_hit)   =  TRACKER_SANE_RAW_TDC_Y(i)
               y1t_y(y1t_hit)     =  -22.225+(y1t_row(y1t_hit)-1)*0.35
               
            else if(TRACKER_SANE_RAW_COUNTER_Y(i).lt.257)then
               y2t_hit            =  y2t_hit + 1 
               if(y2t_hit.gt.300) go to 10
               y2t_row(y2t_hit)   =  TRACKER_SANE_RAW_COUNTER_Y(i)-128
               y2t_tdc(y2t_hit)   =  TRACKER_SANE_RAW_TDC_Y(i)
               y2t_y(y2t_hit)     =  -22.4+(y2t_row(y2t_hit)-1)*0.35
               
            else if(TRACKER_SANE_RAW_COUNTER_Y(i).lt.385)then
               y3t_hit            =  y3t_hit + 1 
               if(y3t_hit.gt.300) go to 10
               y3t_row(y3t_hit)   =  TRACKER_SANE_RAW_COUNTER_Y(i)-256
               y3t_tdc(y3t_hit)   =  TRACKER_SANE_RAW_TDC_Y(i)
               y3t_y(y3t_hit)     =  -22.225+(y3t_row(y3t_hit)-1)*0.35
            endif
         endif
      enddo
      if(HSNUM_FPTRACK.gt.0)then
         hms_p        = h_Ntuple_contents(2)	
         hms_e        = h_Ntuple_contents(3)
         hms_theta    = h_Ntuple_contents(6)
         hms_phi      = h_Ntuple_contents(7)
         hms_ytar     = h_Ntuple_contents(18)
         hms_yptar    = h_Ntuple_contents(20)
         hms_xptar    = h_Ntuple_contents(19)
         hms_delta    = h_Ntuple_contents(21)
      endif
      rast_x       = gfry_raw_adc
      rast_y       = gfrx_raw_adc
c      if(nclust.gt.0)write(*,*)xclust
      abort=.not.HEXIST(sane_ntuple_ID)
      if(abort) then
         call G_build_note(':Ntuple ID#$ does not exist',
     $        '$',sane_ntuple_ID,' ',0.,' ',err)
         call G_add_path(here,err)
      else 
         
         call HFNT(sane_ntuple_ID)
         
      endif
 10   CONTINUE
      return 
      end
