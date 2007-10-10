      subroutine b_print_raw_bad(ABORT,err)
      
      implicit none
      save

      character*16 here
      parameter(here='b_print_raw_bad')

      logical ABORT
      character*(*) err

      include 'bigcal_data_structures.cmn'
      include 'bigcal_bypass_switches.cmn'
      include 'gen_event_info.cmn'

      integer j

      abort=.false.
      err=' '

      if(bigcal_prot_nbad.gt.0) then
         write(bluno,99) 'gen_event_id_number=',gen_event_id_number
         write(bluno,99) 'bigcal_prot_nbad=',bigcal_prot_nbad
         write(bluno,100) 'row,col,adc='
         write(bluno,101) (bigcal_prot_iybad(j),bigcal_prot_ixbad(j),
     $        bigcal_prot_adc_bad(j),j=1,bigcal_prot_badplusgood)
      endif
      if(bigcal_rcs_nbad.gt.0) then
         write(bluno,99) 'gen_event_id_number=',gen_event_id_number
         write(bluno,99) 'bigcal_rcs_nbad=',bigcal_rcs_nbad
         write(bluno,100) 'row,col,adc='
         write(bluno,101) (bigcal_rcs_iybad(j)+32,bigcal_rcs_ixbad(j),
     $        bigcal_rcs_adc_bad(j),j=1,bigcal_rcs_badplusgood)
      endif
      if(bigcal_atrig_nbad.gt.0) then
         write(bluno,99) 'gen_event_id_number=',gen_event_id_number
         write(bluno,99) 'bigcal_atrig_nbad=',bigcal_atrig_nbad
         write(bluno,100) 'group,half,adc='
         write(bluno,101) (bigcal_atrig_igroup_bad(j),bigcal_atrig_ihalf_bad(j),
     $        bigcal_atrig_adc_bad(j),j=1,bigcal_atrig_badplusgood)
      endif

 99   format(A22,I7)
 100  format(A15)
 101  format(2I5,I7)
      
      return 
      end
