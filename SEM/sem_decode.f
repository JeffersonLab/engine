      subroutine sem_decode(pointer,lastslot, roc, bank, 
     &     maxwords, did)
      
      
*****************************************
*****************************************
      
      implicit none
      integer*4 pointer,lastslot, roc, bank(*) 
      integer*4 maxwords, did 
      integer*4 g_decode_fb_detector ! Detector unpacking routine
      include 'gen_detectorids.par'
      include 'sem_data_structures.cmn'
      
      
*****************************************
*****************************************
      
      
      if(did.eq.SEM_ID)then
         pointer = pointer +
     $        g_decode_fb_detector(lastslot, roc, bank(pointer), 
     &        maxwords, did, 
     $            N_TBPM_ALL_CHAN , N_TBPM_TOT_HITS, N_TBPM_ADDR1,
     $            N_TBPM_ADDR2, 1, N_TBPM_RAW_DATA, 0, 0, 0)
c         write(*,*)'SEM ',N_TBPM_TOT_HITS,did
c         write(*,*)'SEM ',N_TBPM_ADDR2
c         write(*,*)'SEM ',N_TBPM_RAW_DATA
      endif

      end
