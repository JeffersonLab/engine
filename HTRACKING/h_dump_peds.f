      subroutine h_dump_peds(ABORT,err)
*
* $Log$
* Revision 1.4  1996/01/24 15:57:06  saw
* (JRA) Remove MISC pedestals
*
* Revision 1.3  1996/01/16 21:47:10  cdaq
* (JRA)
*
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
      character*132 file

      integer SPAREID
      parameter (SPAREID=67)
*
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'hms_pedestals.cmn'
      INCLUDE 'hms_scin_parms.cmn'
      INCLUDE 'hms_calorimeter.cmn'
      INCLUDE 'hms_cer_parms.cmn'
      INCLUDE 'hms_filenames.cmn'
      INCLUDE 'gen_run_info.cmn'


      if (h_pedestal_output_filename.ne.' ') then
        file=h_pedestal_output_filename
        call g_sub_run_number(file, gen_run_number)
        open(unit=SPAREID,file=file,status='unknown')
      else
        return
      endif

      write(SPAREID,*) 'These are the values that were used for the analysis'
      write(SPAREID,*) '      (from the param file or pedestal events)'
      write(SPAREID,*)
*
*
* HODOSCOPE PEDESTALS
*
      write(SPAREID,*) 'hscin_all_ped_pos ='
      do cnt = 1 , hnum_scin_elements
        write(SPAREID,111) (hscin_all_ped_pos(pln,cnt),pln=1,4)
      enddo
      write(SPAREID,*) 'hhodo_new_ped_pos ='
      do cnt = 1 , hnum_scin_elements
        write(SPAREID,111) (hhodo_new_ped_pos(pln,cnt),pln=1,4)
      enddo
      write(SPAREID,*) 'hhodo_new_sig_pos ='
      do cnt = 1 , hnum_scin_elements
        write(SPAREID,111) (hhodo_new_sig_pos(pln,cnt),pln=1,4)
      enddo
      write(SPAREID,*) 'hhodo_new_threshold_pos ='
      do cnt = 1 , hnum_scin_elements
        write(SPAREID,111) (hhodo_new_threshold_pos(pln,cnt),pln=1,4)
      enddo
      write(SPAREID,*) 'hscin_all_ped_neg ='
      do cnt = 1 , hnum_scin_elements
          write(SPAREID,111) (hscin_all_ped_neg(pln,cnt),pln=1,4)
      enddo
      write(SPAREID,*) 'hhodo_new_ped_neg ='
      do cnt = 1 , hnum_scin_elements
        write(SPAREID,111) (hhodo_new_ped_neg(pln,cnt),pln=1,4)
      enddo
      write(SPAREID,*) 'hhodo_new_sig_neg ='
      do cnt = 1 , hnum_scin_elements
        write(SPAREID,111) (hhodo_new_sig_neg(pln,cnt),pln=1,4)
      enddo
      write(SPAREID,*) 'hhodo_new_threshold_neg ='
      do cnt = 1 , hnum_scin_elements
          write(SPAREID,111) (hhodo_new_threshold_neg(pln,cnt),pln=1,4)
      enddo

111   format (10x,3(f6.1,','),f6.1)
*
*
* CALORIMETER PEDESTALS
*
      write(SPAREID,*) ' hcal_ped_mean = '
      write(SPAREID,112) (hcal_ped_mean(blk),blk=1,hmax_cal_rows)
      write(SPAREID,112) (hcal_ped_mean(blk),blk=hmax_cal_rows+1,2*hmax_cal_rows)
      write(SPAREID,112) (hcal_ped_mean(blk),blk=2*hmax_cal_rows+1,3*hmax_cal_rows)
      write(SPAREID,112) (hcal_ped_mean(blk),blk=3*hmax_cal_rows+1,4*hmax_cal_rows)
      write(SPAREID,*) '; calorimeter ped. sigma (sqrt(variance))'
      write(SPAREID,*) ' hcal_ped_rms = '
      write(SPAREID,112) (hcal_ped_rms(blk),blk=1,hmax_cal_rows)
      write(SPAREID,112) (hcal_ped_rms(blk),blk=hmax_cal_rows+1,2*hmax_cal_rows)
      write(SPAREID,112) (hcal_ped_rms(blk),blk=2*hmax_cal_rows+1,3*hmax_cal_rows)
      write(SPAREID,112) (hcal_ped_rms(blk),blk=3*hmax_cal_rows+1,4*hmax_cal_rows)
      write(SPAREID,*) '; calorimeter threshold above ped. =MIN(50,MAX(10,3*sigma))'
      write(SPAREID,*) 'hcal_threshold = '
      write(SPAREID,112) (hcal_threshold(blk),blk=1,hmax_cal_rows)
      write(SPAREID,112) (hcal_threshold(blk),blk=hmax_cal_rows+1,2*hmax_cal_rows)
      write(SPAREID,112) (hcal_threshold(blk),blk=2*hmax_cal_rows+1,3*hmax_cal_rows)
      write(SPAREID,112) (hcal_threshold(blk),blk=3*hmax_cal_rows+1,4*hmax_cal_rows)
112   format (12(f5.1,','),f5.1)

*
*
* GAS CERENKOV PEDESTALS
*
      write(SPAREID,*) 'hcer_ped = '
      write(SPAREID,113) (hcer_ped(pmt),pmt=1,hmax_cer_hits)
113   format (f5.1,',',f5.1)

      close(SPAREID)

      return
      end
