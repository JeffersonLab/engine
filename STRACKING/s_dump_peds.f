      subroutine s_dump_peds(ABORT,err)
*
* $Log$
* Revision 1.2  1995/10/09 20:18:31  cdaq
* (JRA) Cleanup, add cerenkov pedestals
*
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


      open(unit=99,file='peds.sos',status='unknown')

      write(99,*) 'These are the values that were used for the analysis'
      write(99,*) '      (from the param file or pedestal events)'
      write(99,*)
*
*
* HODOSCOPE PEDESTALS
*
      write(99,*) 'sscin_all_ped_pos ='
      do cnt = 1 , snum_scin_elements
          write(99,111) (sscin_all_ped_pos(pln,cnt),pln=1,4)
      enddo
      write(99,*) 'sscin_all_ped_neg ='
      do cnt = 1 , snum_scin_elements
          write(99,111) (sscin_all_ped_neg(pln,cnt),pln=1,4)
      enddo

111   format(10x,3(f6.1,','),f6.1)
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
      write(99,*) 'scer_ped_mean = '
      write(99,113) (scer_ped_mean(pmt),pmt=1,smax_cer_hits)
113   format (3(f5.1,','),f5.1)

      close(99)

      return
      end
