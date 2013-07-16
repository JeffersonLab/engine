*=======================================================================
      subroutine h_cal(abort,errmsg)
*=======================================================================
*-
*-      Purpose: Computes the calorimeter particle ID quantities.
*-               Corrects the energy depositions for impact point 
*-               coordinate dependence.
*-
*-      Input Bank: HMS_TRACKS_CAL
*-
*-      Output Bank: HMS_TRACK_TESTS
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-      Created: 15 Mar 1994      Tsolak A. Amatuni
*
* $Log: h_cal.f,v $
* Revision 1.11  2003/04/03 00:43:13  jones
* Update to calibration (V. Tadevosyan0
*
* Revision 1.10  2002/09/26 14:31:56  jones
*     the energy determination for planes A and B can use
*     both pos and neg PMT depending on setting of hcal_num_neg_columns.
*
* Revision 1.9  1999/06/10 16:46:58  csa
* (JRA) Cosmetic changes
*
* Revision 1.8  1999/02/25 20:10:48  saw
* Vardan Tadevosyan shower code updates
*
* Revision 1.7  1999/02/03 21:13:22  saw
* Code for new Shower counter tubes
*
* Revision 1.6  1999/01/21 21:40:13  saw
* Extra shower counter tube modifications
*
* Revision 1.5  1998/12/17 22:02:38  saw
* Support extra set of tubes on HMS shower counter
*
* Revision 1.4  1995/05/22 19:39:04  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.3  1994/09/13  19:39:14  cdaq
* (JRA) Add preshower energy
*
* Revision 1.2  1994/04/12  21:24:55  cdaq
* (DFG) Put in real code and change name of print routine.
*
* Revision 1.1  1994/02/19  06:12:35  cdaq
* Initial revision
*
*--------------------------------------------------------
      implicit none
      save

      logical abort
      character*(*) errmsg

      character*5 here
      parameter (here='H_CAL')

      integer*4 nt           !Detector track number
      integer*4 nc           !Calorimeter cluster number
      real*4 cor         !Correction factor for X,Y dependenc.   ! Single   PMT
      real*4 cor_pos     !Correction factor for X,Y dependenc.   ! Single  "POS_PMT"
      real*4 cor_neg     !Correction factor for X,Y dependenc.   ! Single  "NEG_PMT" 
      real*4 h_correct_cal              !External function to compute "cor". 
      real*4 h_correct_cal_pos          !External function to compute "cor_pos". 
      real*4 h_correct_cal_neg          !External function to compute "cor_neg"   

      include 'hms_data_structures.cmn'
      include 'hms_calorimeter.cmn'
*
*--------------------------------------------------------
*
      do nt=1,hntracks_fp
         htrack_e1_pos(nt)=0.   !  Only pos_pmt for layer "A"
         htrack_e1_neg(nt)=0.   !  Only_neg_pmt for layer "A"
         htrack_e2_pos(nt)=0.   !  Only_pos_pmt for layer "B"  
         htrack_e2_neg(nt)=0.   !  Only_neg_pmt for layer "B" 
         htrack_e1(nt)=0.
         htrack_e2(nt)=0.
         htrack_e3(nt)=0.
         htrack_e4(nt)=0.
         htrack_et(nt)=0.
         htrack_preshower_e(nt)=0.
      enddo

      call h_clusters_cal(abort,errmsg)
      if(abort) then
         call g_add_path(here,errmsg)
         return
      endif

      call h_tracks_cal(abort,errmsg)
      if(abort) then
         call g_add_path(here,errmsg)
         return
      endif
*
*      Return if there are no tracks found or none of the found
*      tracks matches a cluster in the calorimeter.
*
      if(hntracks_fp .le.0) go to 100 !Return
      if(hntracks_cal.le.0) go to 100 !Return

      do nt =1,hntracks_fp

         nc=hcluster_track(nt)

         if(nc.gt.0) then
            cor    =h_correct_cal(htrack_xc(nt),htrack_yc(nt)) ! For single "pmt"
            cor_pos=h_correct_cal_pos(htrack_xc(nt),htrack_yc(nt)) ! For single "pos_pmt"
            cor_neg=h_correct_cal_neg(htrack_xc(nt),htrack_yc(nt)) ! For single "neg_pmt"

            hnblocks_cal(nt)=hcluster_size(nc)
*
            if(hcal_num_neg_columns.ge.1) then
               htrack_e1_pos(nt)=cor_pos*hcluster_e1_pos(nc) !  For "A" layer "POS_PMT"    
               htrack_e1_neg(nt)=cor_neg*hcluster_e1_neg(nc) !  For "A" layer "NEG_PMT"
               htrack_e1(nt)=htrack_e1_pos(nt)+htrack_e1_neg(nt) !  For "A" layer "POS"+"NEG_PMT"
            else
               htrack_e1(nt)=cor_pos*hcluster_e1(nc) !   IF ONLY "POS_PMT" in layer "A"                
            endif

            if(hcal_num_neg_columns.ge.2) then
               htrack_e2_pos(nt)=cor_pos*hcluster_e2_pos(nc) !  For "B" layer "POS_PMT"    
               htrack_e2_neg(nt)=cor_neg*hcluster_e2_neg(nc) !  For "B" layer "NEG_PMT"
               htrack_e2(nt)=htrack_e2_pos(nt)+htrack_e2_neg(nt) !  For "B" layer "POS"+"NEG_PMT"
            else
               htrack_e2(nt)=cor_pos*hcluster_e2(nc) !   IF ONLY "POS_PMT" in layer "B"
            endif

            if(hcal_num_neg_columns.ge.3) then
               print *,"Extra tubes on more than two layers not supported"
            endif

            htrack_e3(nt)=cor*hcluster_e3(nc)  
            htrack_e4(nt)=cor*hcluster_e4(nc)

            htrack_et(nt)=htrack_e1(nt)+htrack_e2(nt)+ htrack_e3(nt)
     &           +htrack_e4(nt) 

            htrack_preshower_e(nt)=htrack_e1(nt)

         endif                  !End ... if nc > 0

      enddo                     !End loop over detector tracks

 100  continue
      if(hdbg_tests_cal.gt.0) call h_prt_cal_tests

      return
      end
