      SUBROUTINE G_get_next_event(ABORT,err)
*--------------------------------------------------------
*-       Prototype C analysis routine
*-
*-
*-   Purpose and Methods : gets the CRAW (C raw data) buffer
*-                         from a FASTBUS CODA file
*-
*-   Output: ABORT      - success or failure
*-         : err        - reason for failure, if any
*- 
*-   Created  29-Oct-1993   Kevin B. Beard
*-   Modified 1-Dec-1993    KBB: borrowed L.Dennis's hall B routines
* $Log$
* Revision 1.5  2002/09/25 13:50:49  jones
*    a.  include file gen_run_info.cmn
*    b.  add code for segmented runs
*
* Revision 1.4  1996/01/16 18:32:21  cdaq
* no change
*
* Revision 1.3  1994/04/12 18:45:53  cdaq
* (SAW) Add include for the CRAW event buffer common
*
* Revision 1.2  1994/02/11  15:43:08  cdaq
* Replace fbgen library call with plain evread call
*
* Revision 1.1  1994/02/01  20:40:55  cdaq
* Initial revision
*
*-
*- All standards are from "Proposal for Hall C Analysis Software
*- Vade Mecum, Draft 1.0" by D.F.Geesamn and S.Wood, 7 May 1993
*-
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*16 here
      parameter (here= 'G_get_next_event')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'gen_data_structures.cmn'
      include 'gen_craw.cmn'
      INCLUDE 'gen_filenames.cmn'
      INCLUDE 'gen_run_info.cmn'
*
      integer g_important_length
*
      integer maxsize
      integer*4 status
      integer*4 evread, evclose, evopen    ! Coda event read routine
      character*132 file
      integer*4 fname_len
*
*--------------------------------------------------------
*
      err= ' '
*
      ABORT= .NOT.g_data_source_opened
*
      IF(ABORT) THEN
*
        err= ':no data source open'
*
      ELSE                     !try to get next event
*
        maxsize= LENGTH_CRAW

        status = evread(g_data_source_in_hndl,CRAW,maxsize)

        if(status.ne.0) then
           if(g_segment.ge.0) then ! This is a segmented run, look for more

              print *,"Closing segment ",g_segment
              status = evclose(g_data_source_in_hndl) ! Should check result
              g_data_source_opened = .false.
              g_segment = g_segment+1

              file = g_data_source_filename
              call g_sub_run_number(file, gen_run_number)
              fname_len = g_important_length(file)
              if(g_segment.lt.10) then
                 fname_len = fname_len + 1
                 file(fname_len:fname_len) = '.'
                 fname_len = fname_len + 1
                 file(fname_len:fname_len) = char(ichar('0')+g_segment)
              else if(g_segment.lt.100) then
                 fname_len = fname_len + 1
                 file(fname_len:fname_len) = '.'
                 fname_len = fname_len + 1
                 file(fname_len:fname_len) = char(ichar('0')+g_segment/10)
                 fname_len = fname_len + 1
                 file(fname_len:fname_len) = char(ichar('0')
     $                +g_segment-10*(g_segment/10))

              else ! Only support up to 100 segments
                 ABORT = .true.
              endif
              if(.NOT.ABORT) then
                 print *,"Opening segment ",g_segment
                 status = evopen(file,'r',g_data_source_in_hndl)
                 if(status.eq.0) then
                    g_data_source_opened = .true.
                    status = evread(g_data_source_in_hndl,CRAW,maxsize)
                    if(status.ne.0) then
 1                     ABORT = .true.
                       call G_append(err,' & cannot read from file')
                    endif
                 else
                    call G_append(err,' & cannot open file')
                    ABORT = .true.
                 endif
              endif
           else
              call cemsg(status,0,err) ! Get error string from CODA
              ABORT = .true.
           endif
        endif
      ENDIF
*
      IF(ABORT) call G_add_path(here,err)    
*
      RETURN
      END

