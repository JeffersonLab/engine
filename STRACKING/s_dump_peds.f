      subroutine s_dump_peds(ABORT,err)
*
* $Log$
* Revision 1.1  1995/08/31 18:06:45  cdaq
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
      integer*4 pmt
*
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'sos_pedestals.cmn'
      INCLUDE 'sos_scin_parms.cmn'
      INCLUDE 'sos_calorimeter.cmn'
*
*
* HODOSCOPE PEDESTALS
*

      open(unit=99,file='peds.sos',status='unknown')

c      write(99,*) 'HODOSCOPE +'
c      do pln = 1 , snum_scin_planes
c        do cnt = 1 , snum_scin_elements
c          write(99,1001) pln,cnt,sscin_all_ped_pos(pln,cnt),shodo_all_sig_pos(pln,cnt)
c        enddo
c      enddo
c      write(99,*) 'HODOSCOPE -'
c      do pln = 1 , snum_scin_planes
c        do cnt = 1 , snum_scin_elements
c          write(99,1001) pln,cnt,sscin_all_ped_neg(pln,cnt),shodo_all_sig_neg(pln,cnt)
c        enddo
c      enddo
      write(99,*) 'sscin_all_ped_pos ='
      do cnt = 1 , snum_scin_elements
          write(99,111) (sscin_all_ped_pos(pln,cnt),pln=1,4)
      enddo
      write(99,*) 'sscin_all_ped_neg ='
      do cnt = 1 , snum_scin_elements
          write(99,111) (sscin_all_ped_neg(pln,cnt),pln=1,4)
      enddo

111   format(10x,3(f6.1,','),f6.1)
c1001  format(2i4,2f10.2)
c1000  format(i4,2f10.2)
*
*
* CALORIMETER PEDESTALS
*
      write(99,*) '; calorimeter pedestal centroids'
      write(99,*) ' scal_ped_mean = '
      write(99,112) (scal_ped_mean(blk),blk=1,smax_cal_rows)
      write(99,112) (scal_ped_mean(blk),blk=smax_cal_rows+1,2*smax_cal_rows)
      write(99,112) (scal_ped_mean(blk),blk=2*smax_cal_rows+1,3*smax_cal_rows)
      write(99,112) (scal_ped_mean(blk),blk=3*smax_cal_rows+1,4*smax_cal_rows)
      write(99,*) '; calorimeter ped. sigma (sqrt(variance))'
      write(99,*) ' scal_ped_rms = '
      write(99,112) (scal_ped_rms(blk),blk=1,smax_cal_rows)
      write(99,112) (scal_ped_rms(blk),blk=smax_cal_rows+1,2*smax_cal_rows)
      write(99,112) (scal_ped_rms(blk),blk=2*smax_cal_rows+1,3*smax_cal_rows)
      write(99,112) (scal_ped_rms(blk),blk=3*smax_cal_rows+1,4*smax_cal_rows)
      write(99,*) '; calorimeter threshold above ped. =MIN(50,MAX(10,3*sigma))'
      write(99,*) 'scal_threshold = '
      write(99,112) (scal_threshold(blk),blk=1,smax_cal_rows)
      write(99,112) (scal_threshold(blk),blk=smax_cal_rows+1,2*smax_cal_rows)
      write(99,112) (scal_threshold(blk),blk=2*smax_cal_rows+1,3*smax_cal_rows)
      write(99,112) (scal_threshold(blk),blk=3*smax_cal_rows+1,4*smax_cal_rows)
112   format (10(f5.1,','),f5.1)

*
*
* GAS CERENKOV PEDESTALS
*
c      write(99,*) 'CERENKOV'
c      do pmt = 1 , smax_cer_hits
c          write(99,1000) pmt,scer_ped_mean(pmt),scer_ped_rms(pmt)
c      enddo

      close(99)

      return
      end
