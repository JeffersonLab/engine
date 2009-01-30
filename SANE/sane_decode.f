      subroutine sane_decode(pointer,lastslot, roc, bank, 
     &     maxwords, did)
      
      
*****************************************
*****************************************
      
      implicit none
      integer*4 pointer,lastslot, roc, bank(*) 
      integer*4 maxwords, did 
      integer*4 g_decode_fb_detector ! Detector unpacking routine
      include 'gen_detectorids.par'
      include 'sane_data_structures.cmn'
      
      
*****************************************
*****************************************
      
      
      if(did.eq.LUCITE_SANE_ID)then
         pointer = pointer +
     $        g_decode_fb_detector(lastslot, roc, bank(pointer), 
     &        maxwords, did, 
     $        LUCITE_SANE_MAX_HITS, 
     $        LUCITE_SANE_RAW_TOT_HITS, 
     $        LUCITE_SANE_RAW_PLANE,
     $        LUCITE_SANE_RAW_COUNTER_NUM, 2, 
     $        LUCITE_SANE_RAW_ADC_POS, 
     $        LUCITE_SANE_RAW_ADC_NEG, 
     $        0, 
     $        0)
      else if(did.eq.LUCITE_SANE_ID2)then
         pointer = pointer +
     $        g_decode_fb_detector(lastslot, roc, bank(pointer), 
     &        maxwords, did, 
     $        LUCITE_SANE_MAX_HITS, 
     $        LUCITE_SANE_RAW_TOT_HITS2, 
     $        LUCITE_SANE_RAW_PLANE2,
     $        LUCITE_SANE_RAW_COUNTER_NUM2, 1, 
     $        LUCITE_SANE_RAW_TDC_POS, 
     $        0, 
     $        0, 
     $        0)
c         write(*,*)'LUC TDC POS ',LUCITE_SANE_RAW_TDC_POS
      else if(did.eq.LUCITE_SANE_ID3)then
         pointer = pointer +
     $        g_decode_fb_detector(lastslot, roc, bank(pointer), 
     &        maxwords, did, 
     $        LUCITE_SANE_MAX_HITS, 
     $        LUCITE_SANE_RAW_TOT_HITS3, 
     $        LUCITE_SANE_RAW_PLANE3,
     $        LUCITE_SANE_RAW_COUNTER_NUM3, 1, 
     $        LUCITE_SANE_RAW_TDC_NEG, 
     $        0, 
     $        0, 
     $        0)
c         write(*,*)'LUC TDC NEG ',LUCITE_SANE_RAW_TDC_NEG
      else if(did.eq.CERENKOV_SANE_ID)then
c        WRITE(*,*)'HITS = ',CERENKOV_SANE_RAW_TOT_HITS
         pointer = pointer + 
     $        g_decode_fb_detector(lastslot, roc, bank(pointer), 
     $        maxwords, did, 
     $        CERENKOV_SANE_MAX_HITS, 
     $        CERENKOV_SANE_RAW_TOT_HITS, 
     $        CERENKOV_SANE_RAW_PLANE,
     $        CERENKOV_SANE_RAW_COUNTER_NUM, 1, 
     $        CERENKOV_SANE_RAW_ADC, 
     $        0, 0, 0)
c         WRITE(*,*)'cer ADC ',CERENKOV_SANE_RAW_ADC
      else if(did.eq.CERENKOV_SANE_ID2)then

         pointer = pointer + 
     $        g_decode_fb_detector(lastslot, roc, bank(pointer), 
     $        maxwords, did, 
     $        CERENKOV_SANE_MAX_HITS, 
     $        CERENKOV_SANE_RAW_TOT_HITS2, 
     $        CERENKOV_SANE_RAW_PLANE2,
     $        CERENKOV_SANE_RAW_COUNTER_NUM2, 1, 
     $        CERENKOV_SANE_RAW_TDC, 
     $        0, 0, 0)
c         WRITE(*,*)'HITS = ',CERENKOV_SANE_RAW_TOT_HITS2
c         WRITE(*,*)'cer tdc ',CERENKOV_SANE_RAW_TDC
C         WRITE(*,*)'cer ADC ',CERENKOV_SANE_RAW_ADC
         

      else if(did.eq.TRACKER_SANE_X_ID)then
         pointer = pointer + 
     $        g_decode_fb_detector(lastslot, roc, bank(pointer), 
     $        maxwords, did, 
     $        TRACKER_SANE_MAX_HITS, 
     $        TRACKER_SANE_RAW_TOT_HITS_X, 
     $        TRACKER_SANE_RAW_PLANE_X,
     $        TRACKER_SANE_RAW_COUNTER_X, 1, 
     $        TRACKER_SANE_RAW_TDC_X, 
     $        0, 0, 0)
      else if(did.eq.TRACKER_SANE_Y_ID)then

          pointer = pointer + 
     $        g_decode_fb_detector(lastslot, roc, bank(pointer), 
     $        maxwords, did, 
     $        TRACKER_SANE_MAX_HITS, 
     $        TRACKER_SANE_RAW_TOT_HITS_Y, 
     $        TRACKER_SANE_RAW_PLANE_Y,
     $        TRACKER_SANE_RAW_COUNTER_Y, 1, 
     $        TRACKER_SANE_RAW_TDC_Y, 
     $        0, 0, 0)

      endif

      end
