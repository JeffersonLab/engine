      subroutine h_dump_peds(ABORT,err)
*
* $Log$
* Revision 1.2  1995/10/09 20:18:06  cdaq
* (JRA) Cleanup, add cerenkov pedestals
*
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


      open(unit=98,file='peds.hms',status='unknown')

      write(98,*) 'These are the values that were used for the analysis'
      write(98,*) '      (from the param file or pedestal events)'
      write(98,*)
*
*
* HODOSCOPE PEDESTALS
*
      write(98,*) 'hscin_all_ped_pos ='
      do cnt = 1 , hnum_scin_elements
        write(98,111) (hscin_all_ped_pos(pln,cnt),pln=1,4)
      enddo
      write(98,*) 'hscin_all_ped_neg ='
      do cnt = 1 , hnum_scin_elements
          write(98,111) (hscin_all_ped_neg(pln,cnt),pln=1,4)
      enddo

111   format (10x,3(f6.1,','),f6.1)
*
*
* CALORIMETER PEDESTALS
*
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

*
*
* GAS CERENKOV PEDESTALS
*
      write(98,*) 'hcer_ped_mean = '
      write(98,113) (hcer_ped_mean(pmt),pmt=1,hmax_cer_hits)
113   format (f5.1,',',f5.1)

      close(98)

      return
      end
