      subroutine g_calc_beam_pedestal(ABORT,err)
*
* $Log$
* Revision 1.4  2003/09/05 15:33:55  jones
* Merge in online03 changes (mkj)
*
* Revision 1.3.2.1  2003/04/09 16:53:40  cdaq
* Modified so that it does not write out slot = 15 to threshold file (MKJ)
*
* Revision 1.3  1996/12/12 22:10:20  saw
* (SAW) Remove disabling of inputs  3 and 4 in slot 15 (Adds the slow
* raster)
*
c Revision 1.2  96/09/04  14:32:27  14:32:27  saw (Stephen A. Wood)
c (JRA) ??
c 
* Revision 1.1  1996/01/22 15:10:14  saw
* Initial revision
*
      implicit none
      save
*
      character*22 here
      parameter (here='g_calc_beam_pedestal')
*
      logical ABORT
      character*(*) err
*
      integer*4 imisc
      integer*4 ind,ihit
      integer*4 roc,slot
      integer*4 signalcount,istart
      real*4 sig2
      real*4 num
      character*132 file
*
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_decode_common.cmn'
      INCLUDE 'gen_run_info.cmn'
      INCLUDE 'hms_filenames.cmn'
*
      integer SPAREID
      parameter (SPAREID=67)
*
*
* MISC. PEDESTALS
*
      ind = 0
      do ihit = 1 , gmax_misc_hits
        if (gmisc_raw_addr1(ihit).eq.2) then     !  ADC data.
          imisc = gmisc_raw_addr2(ihit)
          num=max(1.,float(gmisc_ped_num(imisc,2)))
          gmisc_new_ped(imisc,2) = float(gmisc_ped_sum(imisc,2)) / num
          sig2 = float(gmisc_ped_sum2(imisc,2))/ num - gmisc_new_ped(imisc,2)**2
          gmisc_new_rms(imisc,2) = sqrt(max(0.,sig2))
          gmisc_new_adc_threshold(imisc,2)=gmisc_new_ped(imisc,2)+10.
          gmisc_dum_adc_threshold(imisc,2)=0   !don't sparsify USED channels.
          if (abs(gmisc_ped(imisc,2)-gmisc_new_ped(imisc,2))
     &                 .ge.(2.*gmisc_new_rms(imisc,2))) then
            ind = ind + 1
            gmisc_changed_tube(ind)=imisc
            gmisc_ped_change(ind)=gmisc_new_ped(imisc,2)-gmisc_ped(imisc,2)
          endif
          if (num.gt.gmisc_min_peds .and. gmisc_min_peds.ne.0) then
            gmisc_ped(imisc,2)=gmisc_new_ped(imisc,2)
            gmisc_ped_rms(imisc,2)=gmisc_new_rms(imisc,2)
          endif
        endif    !chose ADC hits.
      enddo
      gmisc_num_ped_changes = ind
*
      call g_calc_bpm_pedestal
      call g_calc_raster_pedestal
*
* WRITE THRESHOLDS TO FILE FOR HARDWARE SPARCIFICATION
*
      if (h_threshold_output_filename.ne.' ') then !the ADC is in the HMS ROC.
        file=h_threshold_output_filename
        call g_sub_run_number(file, gen_run_number)
      
        open(unit=SPAREID,file=file,status='unknown')
      
        write(SPAREID,*) '# This is the ADC threshold file generated automatically'
        write(SPAREID,*) '# from the pedestal data from run number ',gen_run_number

        roc=1

        slot=15
        signalcount=1
      istart=g_decode_slotpointer(roc,slot)
      if (istart.ne.-1) then   !uninstrumented slot.
        write(SPAREID,*) 'slot=',slot
      endif


ccc        gmisc_dum_adc_threshold(3,2)=4000   !empty slots after blm.
ccc        gmisc_dum_adc_threshold(4,2)=4000

        call g_output_thresholds(SPAREID,roc,slot,signalcount,gmax_misc_hits,
     &      gmisc_dum_adc_threshold,0,gmisc_new_rms,0)

*        close(unit=SPAREID)     !don't close. needed by h_calc_pedestal.f
      endif

      return
      end
