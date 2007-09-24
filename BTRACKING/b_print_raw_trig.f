      subroutine b_print_raw_trig(ABORT,err)

      implicit none
      save

      character*17 here
      parameter(here='b_print_raw_trig')
      
      logical ABORT
      character*(*) err

      include 'bigcal_data_structures.cmn'
      include 'bigcal_gain_parms.cmn'
      include 'bigcal_tof_parms.cmn'
      include 'bigcal_bypass_switches.cmn'
      include 'gen_event_info.cmn'

      character*20 c1
      parameter(c1='BIGCAL_TTRIG_NHIT = ')
      character*10 c2
      parameter(c2='IHALF = , ')
      character*11 c3
      parameter(c3='IGROUP = , ')
      character*12 c4
      parameter(c4='raw adc = , ')
      character*10 c5
      parameter(c5='raw tdc = , ')
      character*20 c6
      parameter(c6='BIGCAL_ATRIG_NHIT = ')

      integer j
      
      ABORT=.false.
      err = ' '
      
      if(BIGCAL_TTRIG_NHIT.gt.0) then
         write(bluno,99) 'gen_event_id_number=',gen_event_id_number
         write(bluno,100) c1,BIGCAL_TTRIG_NHIT
         write(bluno,101) c2,c3,c5
         write(bluno,102) 
     $        (BIGCAL_TTRIG_IHALF(j),BIGCAL_TTRIG_IGROUP(j),
     $        BIGCAL_TTRIG_TDC_RAW(j),
     $        j=1,BIGCAL_TTRIG_NHIT)
      endif

      if(BIGCAL_ATRIG_NHIT.gt.0) then
         write(bluno,99) 'gen_event_id_number=',gen_event_id_number
         write(bluno,100) c6,BIGCAL_ATRIG_NHIT
         write(bluno,101) c2,c3,c4
         write(bluno,102) 
     $        (BIGCAL_ATRIG_IHALF(j),BIGCAL_ATRIG_IGROUP(j),
     $        BIGCAL_ATRIG_ADC_RAW(j),j=1,BIGCAL_ATRIG_NHIT)
      endif

 99   format(A20,I7)
 100  format(A20,I5)
 101  format(A10,A11,A12)
 102  format(I10,I11,I12)


      return 
      end
