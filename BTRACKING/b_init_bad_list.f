      subroutine b_init_bad_list(abort,err)
      
      implicit none
      save

      character*15 here
      parameter(here='b_init_bad_list')

      logical abort
      character*(*) err

      character*80 filename

      integer*4 iochan
      integer*4 row,col,nbad,cell,i

      include 'bigcal_data_structures.cmn'
      include 'bigcal_gain_parms.cmn'
      include 'bigcal_filenames.cmn'
      include 'bigcal_bypass_switches.cmn'

      abort=.false.
      err=' '

      nbad = 0

      filename = b_bad_chan_list_filename
      
c     get any free io channel
      call g_IO_control(iochan,'ANY',abort,err)

      open(unit=iochan,file=filename,form='formatted',status='old',err=101)

 8    read(iochan,fmt=*,end=102,err=102) row,col
      
      if((row.ge.1.and.row.le.32.and.col.ge.1.and.col.le.32).or.
     $     (row.ge.33.and.row.le.56.and.col.ge.1.and.col.le.30)) then
         nbad = nbad + 1
         if(row.le.32) then
            cell = col + 32*(row-1)
            bigcal_prot_cfac(cell) = 0.
c     set a high threshold so that hits from the bad channel list will not make it into 
c     the good hit array by accident
            bigcal_prot_adc_threshold(cell) = 10000.
         else 
            cell = col + 30*(row-33) + 1024
            bigcal_rcs_cfac(cell-1024) = 0.
c     set a high threshold so that hits from the bad channel list will not make it into 
c     the good hit array by accident
            bigcal_rcs_adc_threshold(cell) = 10000.
   
         endif
         
         bigcal_bad_chan_list(cell) = .true.
   
         goto 8
      else 
         write(*,*) 'invalid row and/or column. Will not read any '//
     $        'more channels'
         goto 102
      endif

 101  write(*,*) 'could not open bad channel list file, list not '//
     $     'initialized!'
      b_use_bad_chan_list = 0

      call g_IO_control(iochan,'FREE',abort,err)
      close(iochan)

      return

 102  write(*,*) 'finished reading BigCal bad channel list'
      write(*,*) 'nbad=',nbad

      if(nbad.eq.0) b_use_bad_chan_list = 0

      call g_IO_control(iochan,'FREE',abort,err)
      close(iochan)

      return
      end
