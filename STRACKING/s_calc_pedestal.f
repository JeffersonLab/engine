      subroutine s_calc_pedestal(ABORT,err)
*
* $Log$
* Revision 1.1  1995/04/01 19:36:03  cdaq
* Initial revision
*
*
      implicit none
      save
*
      character*18 here
      parameter (here='s_calc_pedestal')
*
      logical ABORT
      character*(*) err
*
      integer*4 pln,cnt
      integer*4 blk
      real*4 sig2
*
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'sos_pedestals.cmn'
      INCLUDE 'sos_scin_parms.cmn'
      INCLUDE 'sos_calorimeter.cmn'
*
*
* HODOSCOPE PEDESTALS
*
      do pln = 1 , snum_scin_planes
        do cnt = 1 , snum_scin_elements
          if (shodo_pos_ped_num(pln,cnt) .ge. shodo_min_peds .and.
     &        shodo_min_peds .ne. 0) then
            sscin_all_ped_pos(pln,cnt) = shodo_pos_ped_sum(pln,cnt)/
     &              shodo_pos_ped_num(pln,cnt)
            sscin_all_ped_neg(pln,cnt) = shodo_neg_ped_sum(pln,cnt)/
     &              shodo_neg_ped_num(pln,cnt)
c            sig2 = shodo_pos_ped_sum2(pln,cnt)/shodo_pos_ped_num(pln,cnt) -
c     &              sscin_all_ped_pos(pln,cnt)**2
c            if (sig2.le.0) write(6,*) 'pos ped(',pln,',',cnt,') =',sig2
c            sscin_all_sig_pos(pln,cnt) = sqrt(max(0.,sig2))
c            sig2 = shodo_neg_ped_sum2(pln,cnt)/shodo_neg_ped_num(pln,cnt) -
c     &              sscin_all_ped_neg(pln,cnt)**2
c            if (sig2.le.0) write(6,*) 'neg ped(',pln,',',cnt,') =',sig2
c            sscin_all_sig_neg(pln,cnt) = sqrt(max(0.,sig2))
          endif
        enddo
      enddo
*
*
* CALORIMETER PEDESTALS
*

      do blk = 1 , scal_tot_hits
        if (scal_ped_num(blk) .ge. scal_min_peds .and.
     &      scal_min_peds .ne. 0) then
          scal_ped_mean(blk) = scal_ped_sum(blk) / scal_ped_num(blk)
          sig2 = scal_ped_sum2(blk)/scal_ped_num(blk)/scal_ped_num(blk) -
     &              scal_ped_mean(blk)**2
          if (sig2.le.0) write(6,*) 'cal ped(',blk,') =',sig2
          scal_ped_rms(blk) = sqrt(max(0.,sig2))
          scal_threshold(blk) = max(4.,3.*scal_ped_rms(blk))
        endif
      enddo

      return
      end
