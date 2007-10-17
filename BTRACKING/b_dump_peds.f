      subroutine b_dump_peds(ABORT,err)

      implicit none
      save

      character*11 here
      parameter(here='b_dump_peds')

      logical abort
      character*(*) err

      integer spareid
      parameter(spareid=67)

      character*132 file
      integer*4 irow,icol,igroup,ihalf

      include 'bigcal_data_structures.cmn'
      include 'bigcal_gain_parms.cmn'
      include 'bigcal_filenames.cmn'
      include 'gen_run_info.cmn'
      
      if(b_pedestal_output_filename.ne.' ') then
         file = b_pedestal_output_filename
         call g_sub_run_number(file,gen_run_number)
         open(unit=spareid,file=file,status='unknown')
      else 
         return
      endif

      write(spareid,*) ';these are the values that were used '
      write(spareid,*) ';for the analysis (from the param file'// 
     $     ' or ped. events)'
      write(spareid,666) 'bigcal_prot_min_peds = ',bigcal_prot_min_peds
      write(spareid,666) 'bigcal_rcs_min_peds = ',bigcal_rcs_min_peds
      write(spareid,666) 'bigcal_trig_min_peds = ',bigcal_trig_min_peds
      write(spareid,667) 'bigcal_prot_min_thresh = ',bigcal_prot_min_thresh
      write(spareid,667) 'bigcal_prot_max_thresh = ',bigcal_prot_max_thresh
      write(spareid,667) 'bigcal_rcs_min_thresh = ',bigcal_rcs_min_thresh
      write(spareid,667) 'bigcal_rcs_max_thresh = ',bigcal_rcs_max_thresh
      write(spareid,667) 'bigcal_trig_min_thresh = ',bigcal_trig_min_thresh
      write(spareid,667) 'bigcal_trig_max_thresh = ',bigcal_trig_max_thresh
      write(spareid,666) 'bigcal_prot_nsparse = ',bigcal_prot_nsparse
      write(spareid,666) 'bigcal_rcs_nsparse = ',bigcal_rcs_nsparse
      write(spareid,666) 'bigcal_trig_nsparse = ',bigcal_trig_nsparse
 666  format(A30,I6)
 667  format(A30,F8.5)
      write(spareid,*) 'bigcal_prot_ped_mean = '
      do irow=1,bigcal_prot_ny
         write(spareid,100) (bigcal_prot_ped_mean(icol+bigcal_prot_nx*
     $        (irow-1)),icol=1,16)
         if(irow.lt.bigcal_prot_ny) then
            write(spareid,100) (bigcal_prot_ped_mean(icol+bigcal_prot_nx*
     $           (irow-1)),icol=17,32)
         else 
            write(spareid,130) (bigcal_prot_ped_mean(icol+bigcal_prot_nx*
     $           (irow-1)),icol=17,32)
         endif
      enddo
      write(spareid,*) 'bigcal_prot_ped_rms = '
      do irow=1,bigcal_prot_ny
         write(spareid,100) (bigcal_prot_ped_rms(icol+bigcal_prot_nx*
     $        (irow-1)),icol=1,16)
         if(irow.lt.bigcal_prot_ny) then
            write(spareid,100) (bigcal_prot_ped_rms(icol+bigcal_prot_nx*
     $           (irow-1)),icol=17,32)
         else
            write(spareid,130) (bigcal_prot_ped_rms(icol+bigcal_prot_nx*
     $           (irow-1)),icol=17,32)
         endif
      enddo
      write(spareid,*) 'bigcal_prot_adc_threshold = '
      do irow=1,bigcal_prot_ny
         write(spareid,100) (bigcal_prot_adc_threshold(icol + 
     $        bigcal_prot_nx*(irow-1)),icol=1,16)
         if(irow.lt.bigcal_prot_ny) then
            write(spareid,100) (bigcal_prot_adc_threshold(icol + 
     $           bigcal_prot_nx*(irow-1)),icol=17,32)
         else 
            write(spareid,130) (bigcal_prot_adc_threshold(icol + 
     $           bigcal_prot_nx*(irow-1)),icol=17,32)
         endif
      enddo
 100  format(16(f8.2,','))
 130  format(15(f8.2,','),f8.2)
      write(spareid,*) 'bigcal_rcs_ped_mean = '
      do irow=1,bigcal_rcs_ny
         write(spareid,101) (bigcal_rcs_ped_mean(icol+bigcal_rcs_nx*
     $        (irow-1)),icol=1,15)
         if(irow.lt.bigcal_rcs_ny) then
            write(spareid,101) (bigcal_rcs_ped_mean(icol+bigcal_rcs_nx*
     $           (irow-1)),icol=16,30)
         else 
            write(spareid,131) (bigcal_rcs_ped_mean(icol+bigcal_rcs_nx*
     $           (irow-1)),icol=16,30)
         endif
      enddo
      write(spareid,*) 'bigcal_rcs_ped_rms = '
      do irow=1,bigcal_rcs_ny
         write(spareid,101) (bigcal_rcs_ped_rms(icol+bigcal_rcs_nx*
     $        (irow-1)),icol=1,15)
         if(irow.lt.bigcal_rcs_ny) then
            write(spareid,101) (bigcal_rcs_ped_rms(icol+bigcal_rcs_nx*
     $           (irow-1)),icol=16,30)
         else
            write(spareid,131) (bigcal_rcs_ped_rms(icol+bigcal_rcs_nx*
     $           (irow-1)),icol=16,30)
         endif
      enddo
      write(spareid,*) 'bigcal_rcs_adc_threshold = '
      do irow=1,bigcal_rcs_ny
         write(spareid,101) (bigcal_rcs_adc_threshold(icol + 
     $        bigcal_rcs_nx*(irow-1)),icol=1,15)
         if(irow.lt.bigcal_rcs_ny) then
            write(spareid,101) (bigcal_rcs_adc_threshold(icol + 
     $           bigcal_rcs_nx*(irow-1)),icol=16,30)
         else 
            write(spareid,131) (bigcal_rcs_adc_threshold(icol + 
     $           bigcal_rcs_nx*(irow-1)),icol=16,30)
         endif
      enddo

 101  format(15(f8.2,','))
 131  format(14(f8.2,','),f8.2)
      write(spareid,*) 'bigcal_trig_ped_mean = '
      do igroup=1,19
         if(igroup.lt.19) then
            write(spareid,102) (bigcal_trig_ped_mean(ihalf+(igroup-1)*2),
     $           ihalf=1,2)
         else
            write(spareid,132) (bigcal_trig_ped_mean(ihalf+(igroup-1)*2),
     $           ihalf=1,2)
         endif
      enddo
      write(spareid,*) 'bigcal_trig_ped_rms = '
      do igroup=1,19
         if(igroup.lt.19) then
            write(spareid,102) (bigcal_trig_ped_rms(ihalf+(igroup-1)*2),
     $           ihalf=1,2)
         else
            write(spareid,132) (bigcal_trig_ped_rms(ihalf+(igroup-1)*2),
     $           ihalf=1,2)
         endif
      enddo
      write(spareid,*) 'bigcal_trig_adc_threshold = '
      do igroup=1,19
         if(igroup.lt.19) then
            write(spareid,102) (bigcal_trig_adc_threshold(ihalf+(igroup-1)
     $           * 2),ihalf=1,2)
         else
            write(spareid,132) (bigcal_trig_adc_threshold(ihalf+(igroup-1)
     $           * 2),ihalf=1,2)
         endif
      enddo
 102  format(2(f8.2,','))
 132  format(f8.2,',',f8.2)
      close(spareid)

      return
      end
