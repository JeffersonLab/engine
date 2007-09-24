      subroutine b_print_raw_tdc(ABORT,err)

      implicit none
      save

      character*16 here
      parameter(here='b_print_raw_tdc')

      logical ABORT
      character*(*) err

      include 'bigcal_data_structures.cmn'
      include 'bigcal_tof_parms.cmn'
      include 'bigcal_bypass_switches.cmn'
      include 'gen_event_info.cmn'

      character*18 c1
      parameter(c1='BIGCAL_TDC_NHIT = ')
      character*9 c2
      parameter(c2='IROW = , ')
      character*11 c3
      parameter(c3='IGROUP = , ')
      character*10 c4
      parameter(c4='raw tdc = ')

      integer j

      ABORT=.false.
      err=' '
      
      if(BIGCAL_TDC_NHIT.gt.0) then
         write(bluno,99) 'gen_event_id_number=',gen_event_id_number
         write(bluno,100) c1,BIGCAL_TDC_NHIT
         write(bluno,101) c2,c3,c4
         write(bluno,102)
     $        (BIGCAL_TDC_RAW_IROW(j),BIGCAL_TDC_RAW_IGROUP(j),
     $        BIGCAL_TDC_RAW(j),j=1,BIGCAL_TDC_NHIT)
      endif
 99   format(A20,I7)
 100  FORMAT(A18,I5)
 101  FORMAT(A9,A11,A10)
 102  FORMAT(I9,I11,I10)

      return 
      end
