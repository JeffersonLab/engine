      subroutine h_dump_peds(ABORT,err)
*
* $Log$
* Revision 1.1  1995/08/31 14:57:50  cdaq
* Initial revision
*
*
      implicit none
      save
*
      character*18 here
      parameter (here='h_calc_pedestal')
*
      logical ABORT
      character*(*) err
*
      integer*4 pln,cnt
      integer*4 blk
      integer*4 pmt
*
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'hms_pedestals.cmn'
      INCLUDE 'hms_scin_parms.cmn'
      INCLUDE 'hms_calorimeter.cmn'
*
*
* HODOSCOPE PEDESTALS
*

       open(unit=98,file='peds.hms',status='unknown')

c      write(98,*) 'HODOSCOPE +'
c      do pln = 1 , hnum_scin_planes
c        do cnt = 1 , hnum_scin_elements
c          write(98,1001) pln,cnt,hscin_all_ped_pos(pln,cnt),hhodo_all_sig_pos(pln,cnt)
c        enddo
c      enddo
c      write(98,*) 'HODOSCOPE -'
c      do pln = 1 , hnum_scin_planes
c        do cnt = 1 , hnum_scin_elements
c          write(98,1001) pln,cnt,hscin_all_ped_neg(pln,cnt),hhodo_all_sig_neg(pln,cnt)
c        enddo
c      enddo
      write(98,*) 'hscin_all_ped_pos ='
      do cnt = 1 , hnum_scin_elements
          write(98,111) (hscin_all_ped_pos(pln,cnt),pln=1,4)
      enddo
      write(98,*) 'hscin_all_ped_neg ='
      do cnt = 1 , hnum_scin_elements
          write(98,111) (hscin_all_ped_neg(pln,cnt),pln=1,4)
      enddo

111   format (10x,3(f6.1,','),f6.1)
c1001  format(2i4,2f10.2)
c1000  format(i4,2f10.2)
*
*
* CALORIMETER PEDESTALS
*
      write(98,*) '; calorimeter pedestal centroids'
      write(98,*) ' hcal_ped_mean = '
      write(98,112) (hcal_ped_mean(blk),blk=1,hmax_cal_rows)
      write(98,112) (hcal_ped_mean(blk),blk=hmax_cal_rows+1,2*hmax_cal_rows)
      write(98,112) (hcal_ped_mean(blk),blk=2*hmax_cal_rows+1,3*hmax_cal_rows)
      write(98,112) (hcal_ped_mean(blk),blk=3*hmax_cal_rows+1,4*hmax_cal_rows)
      write(98,*) '; calorimeter ped. sigma (sqrt(variance))'
      write(98,*) ' hcal_ped_rms = '
      write(98,112) (hcal_ped_rms(blk),blk=1,hmax_cal_rows)
      write(98,112) (hcal_ped_rms(blk),blk=hmax_cal_rows+1,2*hmax_cal_rows)
      write(98,112) (hcal_ped_rms(blk),blk=2*hmax_cal_rows+1,3*hmax_cal_rows)
      write(98,112) (hcal_ped_rms(blk),blk=3*hmax_cal_rows+1,4*hmax_cal_rows)
      write(98,*) '; calorimeter threshold above ped. =MIN(50,MAX(10,3*sigma))'
      write(98,*) 'hcal_threshold = '
      write(98,112) (hcal_threshold(blk),blk=1,hmax_cal_rows)
      write(98,112) (hcal_threshold(blk),blk=hmax_cal_rows+1,2*hmax_cal_rows)
      write(98,112) (hcal_threshold(blk),blk=2*hmax_cal_rows+1,3*hmax_cal_rows)
      write(98,112) (hcal_threshold(blk),blk=3*hmax_cal_rows+1,4*hmax_cal_rows)
112   format (12(f5.1,','),f5.1)

c      do blk = 1 , hmax_cal_blocks
c         write(98,1000) blk,hcal_ped_mean(blk),hcal_ped_rms(blk)
c      enddo

*
*
* GAS CERENKOV PEDESTALS
*
c      write(98,*) 'CERENKOV'
c      do pmt = 1 , hmax_cer_hits
c          write(98,1000) pmt,hcer_ped_mean(pmt),hcer_ped_rms(pmt)
c      enddo

      close(98)

      return
      end
