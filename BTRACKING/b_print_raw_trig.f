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

      character*19 c1
      parameter(c1='BIGCAL_TRIG_NHIT = ')
      character*10 c2
      parameter(c2='IHALF = , ')
      character*11 c3
      parameter(c3='IGROUP = , ')
      character*12 c4
      parameter(c4='raw adc = , ')
      character*10 c5
      parameter(c5='raw tdc = ')

      integer j
      
      ABORT=.false.
      err = ' '
      
      if(BIGCAL_TRIG_NHIT.gt.0) then
         write(bluno,100) c1,BIGCAL_TRIG_NHIT
         write(bluno,101) c2,c3,c4,c5
         write(bluno,102) 
     $        (BIGCAL_TRIG_IHALF(j),BIGCAL_TRIG_IGROUP(j),
     $        BIGCAL_TRIG_ADC_RAW(j),BIGCAL_TRIG_TDC_RAW(j),
     $        j=1,BIGCAL_TRIG_NHIT)
      endif

 100  format(A19,I5)
 101  format(A10,A11,A12,A10)
 102  format(I10,I11,I12,I10)

      return 
      end
