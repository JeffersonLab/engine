      subroutine s_dump_peds(ABORT,err)
*
* $Log$
* Revision 1.6.16.2  2004/07/06 21:14:23  cdaq
* undid last change (cerenkov pedestals)
*
* Revision 1.6.16.1  2004/07/06 16:55:54  cdaq
* commented out s_cer pedestal write statements at the very end
*
* Revision 1.6  1999/01/29 17:34:58  saw
* Add variables for second tubes on shower counter
*
* Revision 1.5  1996/11/07 19:50:44  saw
* (JRA) ??
*
* Revision 1.4  1996/04/30 17:11:20  saw
* (JRA) Cleanup
*
* Revision 1.3  1996/01/17 19:04:27  cdaq
* (JRA)
*
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
      character*11 here
      parameter (here='s_dump_peds')
*
      logical ABORT
      character*(*) err
*
      integer*4 pln,cnt
      integer*4 blk
      integer*4 pmt
      character*132 file

      integer*4 SPAREID
      parameter (SPAREID=67)
*
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'sos_pedestals.cmn'
      INCLUDE 'sos_scin_parms.cmn'
      INCLUDE 'sos_calorimeter.cmn'
      INCLUDE 'sos_cer_parms.cmn'
      INCLUDE 'sos_filenames.cmn'
      INCLUDE 'gen_run_info.cmn'

      if (s_pedestal_output_filename.ne.' ') then
        file=s_pedestal_output_filename
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
      write(SPAREID,*) 'sscin_all_ped_pos ='
      do cnt = 1 , snum_scin_elements
          write(SPAREID,111) (sscin_all_ped_pos(pln,cnt),pln=1,4)
      enddo
      write(SPAREID,*) 'shodo_new_ped_pos ='
      do cnt = 1 , snum_scin_elements
        write(SPAREID,111) (shodo_new_ped_pos(pln,cnt),pln=1,4)
      enddo
      write(SPAREID,*) 'shodo_new_sig_pos ='
      do cnt = 1 , snum_scin_elements
        write(SPAREID,111) (shodo_new_sig_pos(pln,cnt),pln=1,4)
      enddo
      write(SPAREID,*) 'shodo_new_threshold_pos ='
      do cnt = 1 , snum_scin_elements
          write(SPAREID,111) (shodo_new_threshold_pos(pln,cnt),pln=1,4)
      enddo
      write(SPAREID,*) 'sscin_all_ped_neg ='
      do cnt = 1 , snum_scin_elements
          write(SPAREID,111) (sscin_all_ped_neg(pln,cnt),pln=1,4)
      enddo
      write(SPAREID,*) 'shodo_new_ped_neg ='
      do cnt = 1 , snum_scin_elements
        write(SPAREID,111) (shodo_new_ped_neg(pln,cnt),pln=1,4)
      enddo
      write(SPAREID,*) 'shodo_new_sig_neg ='
      do cnt = 1 , snum_scin_elements
        write(SPAREID,111) (shodo_new_sig_neg(pln,cnt),pln=1,4)
      enddo
      write(SPAREID,*) 'shodo_new_threshold_neg ='
      do cnt = 1 , snum_scin_elements
          write(SPAREID,111) (shodo_new_threshold_neg(pln,cnt),pln=1,4)
      enddo

111   format(10x,3(f6.1,','),f6.1)
*
*
* CALORIMETER PEDESTALS ( Hamlet test version) 
*
      write(SPAREID,*) ' scal_pos_ped_mean = '
      write(SPAREID,112) (scal_pos_ped_mean(blk),blk=1,smax_cal_rows)
      write(SPAREID,112) (scal_pos_ped_mean(blk),blk=smax_cal_rows+1,2*smax_cal_rows)
      write(SPAREID,112) (scal_pos_ped_mean(blk),blk=2*smax_cal_rows+1,3*smax_cal_rows)
      write(SPAREID,112) (scal_pos_ped_mean(blk),blk=3*smax_cal_rows+1,4*smax_cal_rows)
      write(SPAREID,*) '; calorimeter ped. sigma (sqrt(variance))'
      write(SPAREID,*) ' scal_pos_ped_rms = '
      write(SPAREID,112) (scal_pos_ped_rms(blk),blk=1,smax_cal_rows)
      write(SPAREID,112) (scal_pos_ped_rms(blk),blk=smax_cal_rows+1,2*smax_cal_rows)
      write(SPAREID,112) (scal_pos_ped_rms(blk),blk=2*smax_cal_rows+1,3*smax_cal_rows)
      write(SPAREID,112) (scal_pos_ped_rms(blk),blk=3*smax_cal_rows+1,4*smax_cal_rows)
      write(SPAREID,*) '; calorimeter threshold above ped. =MIN(50,MAX(10,3*sigma))'
      write(SPAREID,*) 'scal_new_threshold_pos = '
      write(SPAREID,112) (scal_new_adc_threshold_pos(blk),blk=1,smax_cal_rows)
      write(SPAREID,112) (scal_new_adc_threshold_pos(blk),blk=smax_cal_rows+1,2*smax_cal_rows)
      write(SPAREID,112) (scal_new_adc_threshold_pos(blk),blk=2*smax_cal_rows+1,3*smax_cal_rows)
      write(SPAREID,112) (scal_new_adc_threshold_pos(blk),blk=3*smax_cal_rows+1,4*smax_cal_rows)
*      
      write(SPAREID,*) ' scal_neg_ped_mean = '
      write(SPAREID,112) (scal_neg_ped_mean(blk),blk=1,smax_cal_rows)
      write(SPAREID,112) (scal_neg_ped_mean(blk),blk=smax_cal_rows+1,2*smax_cal_rows)
      write(SPAREID,112) (scal_neg_ped_mean(blk),blk=2*smax_cal_rows+1,3*smax_cal_rows)
      write(SPAREID,112) (scal_neg_ped_mean(blk),blk=3*smax_cal_rows+1,4*smax_cal_rows)
      write(SPAREID,*) '; calorimeter ped. sigma (sqrt(variance))'
      write(SPAREID,*) ' scal_ped_neg_rms = '
      write(SPAREID,112) (scal_neg_ped_rms(blk),blk=1,smax_cal_rows)
      write(SPAREID,112) (scal_neg_ped_rms(blk),blk=smax_cal_rows+1,2*smax_cal_rows)
      write(SPAREID,112) (scal_neg_ped_rms(blk),blk=2*smax_cal_rows+1,3*smax_cal_rows)
      write(SPAREID,112) (scal_neg_ped_rms(blk),blk=3*smax_cal_rows+1,4*smax_cal_rows)
      write(SPAREID,*) '; calorimeter threshold above ped. =MIN(50,MAX(10,3*sigma))'
      write(SPAREID,*) 'scal_new_threshold_neg = '
      write(SPAREID,112) (scal_new_adc_threshold_neg(blk),blk=1,smax_cal_rows)
      write(SPAREID,112) (scal_new_adc_threshold_neg(blk),blk=smax_cal_rows+1,2*smax_cal_rows)
      write(SPAREID,112) (scal_new_adc_threshold_neg(blk),blk=2*smax_cal_rows+1,3*smax_cal_rows)
      write(SPAREID,112) (scal_new_adc_threshold_neg(blk),blk=3*smax_cal_rows+1,4*smax_cal_rows)
***
112   format (12(f5.1,','),f5.1)

*
* GAS CERENKOV PEDESTALS
*
      write(SPAREID,*) 'scer_ped = '
      write(SPAREID,113) (scer_ped(pmt),pmt=1,smax_cer_hits)
113   format (3(f5.1,','),f5.1)

      close(SPAREID)

      return
      end
