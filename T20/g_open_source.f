      SUBROUTINE G_open_source(ABORT,err)
*----------------------------------------------------------------------
*-       Prototype hall C open (FASTBUS) CODA file routine
*- 
*-   Purpose and Methods : Initialization is performed and status returned
*- 
*-   Output: ABORT      - success or failure
*-         : err        - reason for failure, if any
*- 
*-   Created  30-Nov-1993   Kevin B. Beard
*
* $Log$
* Revision 1.1  1997/05/23 19:38:57  saw
* Initial revision
*
*-
*- All standards are from "Proposal for Hall C Analysis Software
*- Vade Mecum, Draft 1.0" by D.F.Geesamn and S.Wood, 7 May 1993
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*13 here
      parameter (here= 'G_open_source')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'gen_filenames.cmn'
      INCLUDE 'gen_run_info.cmn'
*
      integer*4 status
      integer*4 evopen                          ! CODA routine
      integer*4 tc_open                 ! polder calibration data file opener
      character*132 file
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
      g_data_source_in_hndl= 0
*
      file = g_data_source_filename
      call g_sub_run_number(file,gen_run_number)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC POLDER
* change here for t20 D.P. 21/02/97    
      if(gen_run_number.GT.100000) then
         write(6,*) "|",file,"|"
         status = tc_open(file,'r',g_data_source_in_hndl)
      else
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
         status = evopen(file,'r',g_data_source_in_hndl)
      endif   

      if(status.ne.0) then
*         call cemsg(status,0,err)
         g_data_source_opened = .false.
      else
         g_data_source_opened = .true.
      endif
*
      IF(.not.g_data_source_opened) THEN
        err= ':could not open "'//file//'"'
        call G_add_path(here,err)
        ABORT = .TRUE.
      ENDIF
*
      RETURN
      END

