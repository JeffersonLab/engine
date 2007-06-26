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
* Revision 1.5.24.2  2007/06/26 16:36:45  puckett
* latest changes for monte carlo analysis, latest fixes for cluster finding routine
*
* Revision 1.5.24.1  2007/06/20 18:26:32  puckett
* Added BigCal Monte Carlo analysis capability
*
* Revision 1.5  2002/09/25 13:51:30  jones
*     add code for analyzing segmented data files.
*
* Revision 1.4  1996/01/16 18:16:01  cdaq
* no change
*
* Revision 1.3  1995/05/11 16:18:51  cdaq
* (SAW) Allow %d run number substitution in data source filename
*
* Revision 1.2  1995/01/27  20:11:48  cdaq
* (SAW) Add setting of ABORT
*
* Revision 1.1  1994/02/04  22:11:29  cdaq
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
c      include 'bigcal_bypass_switches.cmn'
      include 'bigcal_data_structures.cmn'
*
      integer g_important_length
*
      integer*4 status
      integer*4 evopen                          ! CODA routine
      character*132 file
      integer fname_len
      integer*4 io_dat
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
      g_data_source_in_hndl= 0
*
      file = g_data_source_filename
      if(gen_bigcal_mc.ne.0) then ! Monte Carlo!!!
         if(gen_bigcal_mc.eq.1) then
            call g_IO_control(io_dat,'ANY',ABORT,err)
            if(abort) then
               call g_add_path(here,err)
               return 
            endif
            call g_sub_run_number(file,gen_run_number)
            g_data_source_in_hndl = io_dat
            open(unit=g_data_source_in_hndl,file=file,status='old',
     $           form='unformatted',err=34)
            
            goto 35
            
 34         g_data_source_opened = .false.
            abort = .true. 
            err='error opening bigcal mc .dat file '//file
            call g_add_path(here,err)
            return
            
 35         write(*,*) 'Opened bigcal mc .dat file '//file//
     $           ' successfully'
            g_data_source_opened = .true.
            EOF_MC_DAT = .false.
         else if(gen_bigcal_mc.eq.2) then
            ! open wei's ntuple file
            g_data_source_opened = .false. !for now
         endif
      else 
         call g_sub_run_number(file,gen_run_number)
         status = evopen(file,'r',g_data_source_in_hndl)
         if(status.ne.0) then
*     call cemsg(status,0,err)
*     If filename doesn't end in a digit, try adding ".0" to the end and
*     opening that.
            fname_len = g_important_length(file)
            if(ichar(file(fname_len:fname_len)).le.ichar('0')
     $           .or.ichar(file(fname_len:fname_len)).ge.ichar('9'))then
               g_segment = 0    ! First segment
               file(fname_len+1:fname_len+2) = '.0'
               status = evopen(file,'r',g_data_source_in_hndl)
               if(status.ne.0) then
                  g_data_source_opened = .false.
               else
                  g_data_source_opened = .true.
               endif
            else
               g_data_source_opened = .false.   
            endif
         else
            g_data_source_opened = .true.
            g_segment = -1      ! Not segmented
         endif
*     
         IF(.not.g_data_source_opened) THEN
            err= ':could not open "'//file//'"'
            call G_add_path(here,err)
            ABORT = .TRUE.
         ENDIF
*     
      endif
      RETURN
      END

