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

      write(spareid,*) 'these are the values that were used '
      write(spareid,*)  'for the analysis (from the param file 
     $     or ped. events)'
      write(spareid,*) 'bigcal_prot_ped_mean = '
      do irow=1,bigcal_prot_ny
         write(spareid,100) (bigcal_prot_ped_mean(icol+bigcal_prot_nx*
     $        (irow-1)),icol=1,16)
         write(spareid,100) (bigcal_prot_ped_mean(icol+bigcal_prot_nx*
     $        (irow-1)),icol=17,32)
      enddo
      write(spareid,*) 'bigcal_prot_ped_rms = '
      do irow=1,bigcal_prot_ny
         write(spareid,100) (bigcal_prot_ped_rms(icol+bigcal_prot_nx*
     $        (irow-1)),icol=1,16)
         write(spareid,100) (bigcal_prot_ped_rms(icol+bigcal_prot_nx*
     $        (irow-1)),icol=17,32)
      enddo
      write(spareid,*) 'bigcal_prot_adc_threshold = '
      do irow=1,bigcal_prot_ny
         write(spareid,100) (bigcal_prot_adc_threshold(icol + 
     $        bigcal_prot_nx*(irow-1)),icol=1,16)
         write(spareid,100) (bigcal_prot_adc_threshold(icol + 
     $        bigcal_prot_nx*(irow-1)),icol=17,32)
      enddo
 100  format(16(f6.1,','))
      
      write(spareid,*) 'bigcal_rcs_ped_mean = '
      do irow=1,bigcal_rcs_ny
         write(spareid,101) (bigcal_rcs_ped_mean(icol+bigcal_rcs_nx*
     $        (irow-1)),icol=1,15)
         write(spareid,101) (bigcal_rcs_ped_mean(icol+bigcal_rcs_nx*
     $        (irow-1)),icol=16,30)
      enddo
      write(spareid,*) 'bigcal_rcs_ped_rms = '
      do irow=1,bigcal_rcs_ny
         write(spareid,101) (bigcal_rcs_ped_rms(icol+bigcal_rcs_nx*
     $        (irow-1)),icol=1,15)
         write(spareid,101) (bigcal_rcs_ped_rms(icol+bigcal_rcs_nx*
     $        (irow-1)),icol=16,30)
      enddo
      write(spareid,*) 'bigcal_rcs_adc_threshold = '
      do irow=1,bigcal_rcs_ny
         write(spareid,101) (bigcal_rcs_adc_threshold(icol + 
     $        bigcal_rcs_nx*(irow-1)),icol=1,15)
         write(spareid,101) (bigcal_rcs_adc_threshold(icol + 
     $        bigcal_rcs_nx*(irow-1)),icol=16,30)
      enddo

 101  format(15(f6.1,','))
      
      write(spareid,*) 'bigcal_trig_ped_mean = '
      do igroup=1,19
         write(spareid,102) (bigcal_trig_ped_mean(ihalf+(igroup-1)*2),
     $        ihalf=1,2)
      enddo
      write(spareid,*) 'bigcal_trig_ped_rms = '
      do igroup=1,19
         write(spareid,102) (bigcal_trig_ped_rms(ihalf+(igroup-1)*2),
     $        ihalf=1,2)
      enddo
      write(spareid,*) 'bigcal_trig_adc_threshold = '
      do igroup=1,19
         write(spareid,102) (bigcal_trig_adc_threshold(ihalf+(igroup-1)
     $        * 2),ihalf=1,2)
      enddo
 102  format(2(f6.1,','))

      close(spareid)

      return
      end
