      subroutine g_dump_peds(ABORT,err)
*
* $Log: g_dump_peds.f,v $
* Revision 1.1  1996/04/29 19:46:35  saw
* Initial revision
*
      implicit none
      save
*
      character*12 here
      parameter (here='g_dump_peds')
*
      logical ABORT
      character*(*) err
*
      integer*4 ind
      character*132 file

      integer SPAREID
      parameter (SPAREID=67)
*
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_filenames.cmn'
      INCLUDE 'gen_run_info.cmn'


      if (g_pedestal_output_filename.ne.' ') then
        file=g_pedestal_output_filename
        call g_sub_run_number(file, gen_run_number)
        open(unit=SPAREID,file=file,status='unknown')
      else
        return
      endif

      write(SPAREID,*) 'These are the values that were used for the analysis'
      write(SPAREID,*) '      (from the param file or pedestal events)'
      write(SPAREID,*)
*
* MISC PEDESTALS
*
      write(SPAREID,*) 'gmisc_ped = '
      write(SPAREID,113) (gmisc_ped(ind,2),ind=1,16)
113   format (7(f6.1,','),f6.1)

      close(SPAREID)

      return
      end
