        subroutine f1trigger_decode(pointer,lastslot, roc, bank, 
     &     maxwords, did)
      
      
*****************************************
*****************************************
      
      implicit none
      integer*4 pointer,lastslot, roc, bank(*) 
      integer*4 maxwords, did 
      integer*4 g_decode_fb_detector ! Detector unpacking routine
      include 'gen_detectorids.par'
      include 'f1trigger_data_structures.cmn'
      
      
*****************************************
      if(did.eq.F1TRIGGER_ID)then
         pointer = pointer + 
     $        g_decode_fb_detector(lastslot, roc, bank(pointer), 
     $        maxwords, did, 
     $        TRIGGER_F1_MAX_HITS, 
     $        TRIGGER_F1_RAW_TOT_HITS, 
     $        TRIGGER_F1_RAW_PLANE,
     $        TRIGGER_F1_RAW_COUNTER, 1, 
     $        TRIGGER_F1_START_TDC, 
     $        0, 0, 0)
c         write(*,*)'F1 Trigger time = ',TRIGGER_F1_START_TDC

      endif
*****************************************
 
      end
  
