      subroutine b_print_raw_adc(ABORT,err)

      implicit none
      save
      
      character*16 here
      parameter(here='b_print_raw_adc')

      logical ABORT
      character*(*) err
      
      include 'bigcal_data_structures.cmn'
      include 'bigcal_gain_parms.cmn'
      include 'bigcal_bypass_switches.cmn'
      include 'gen_event_info.cmn'

      character*19 c1
      parameter(c1='BIGCAL_PROT_NHIT = ')
      character*7 c2
      parameter(c2='IY = , ')
      character*7 c3
      parameter(c3='IX = , ')
      character*10 c4
      parameter(c4='raw adc = ')

      character*18 c5
      parameter(c5='BIGCAL_RCS_NHIT = ')

      integer j

      ABORT=.false.
      err = ' '
      if(BIGCAL_PROT_NHIT.gt.0) then
         write(bluno,99) 'gen_event_id_number=',gen_event_id_number
         write(bluno,100) c1,BIGCAL_PROT_NHIT
         write(bluno,101) c2,c3,c4
         write(bluno,102) 
     $        (BIGCAL_PROT_IY(j),BIGCAL_PROT_IX(j),
     $        BIGCAL_PROT_ADC_RAW(j),j=1,BIGCAL_PROT_NHIT)
      endif
      if(BIGCAL_RCS_NHIT.gt.0) then
         write(bluno,99) 'gen_event_id_number=',gen_Event_id_number
         write(bluno,103) c5,BIGCAL_RCS_NHIT
         write(bluno,101) c2,c3,c4
         write(bluno,102) 
     $        (BIGCAL_RCS_IY(j),BIGCAL_RCS_IX(j),
     $        BIGCAL_RCS_ADC_RAW(j),j=1,BIGCAL_RCS_NHIT)
      endif
 99   format(A20,I7)
 100  FORMAT(A19,I5)
 101  FORMAT(2A7,A10)
 102  FORMAT(2I7,I10)
 103  FORMAT(A18,I5)

      return 
      end
