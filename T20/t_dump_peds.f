      subroutine t_dump_peds(ABORT,err)
*
*     Look in h_dump_peds and s_dump_peds for examples
*
*
* $Log$
* Revision 1.1  1998/12/01 20:57:41  saw
* Initial revision
*
      implicit none
      save
*
      character*11 here
      parameter (here='t_dump_peds')
*
      logical ABORT
      character*(*) err
*
      character*132 file

      integer*4 SPAREID
      parameter (SPAREID=67)
*
      INCLUDE 't20_data_structures.cmn'
      INCLUDE 't20_pedestals.cmn'
      INCLUDE 't20_filenames.cmn'
      INCLUDE 'gen_run_info.cmn'

      if (t_pedestal_output_filename.ne.' ') then
        file=t_pedestal_output_filename
        call g_sub_run_number(file, gen_run_number)
        open(unit=SPAREID,file=file,status='unknown')
      else
        return
      endif

      write(SPAREID,*) 'These are the values that were used for the analysis'
      write(SPAREID,*) '      (from the param file or pedestal events)'
      write(SPAREID,*)
*
      close(SPAREID)

      return
      end
