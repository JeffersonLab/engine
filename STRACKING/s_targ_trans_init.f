      subroutine s_targ_trans_init(ABORT,err,istat)
*______________________________________________________________________________
*
* Facility: CEBAF Hall-C software.
*
* Module:   s_targ_trans_init
*
* Version:  0.1 (In development)
*
* $Log$
* Revision 1.6  2008/09/25 00:08:35  jones
* Updated to run with gfortran compiler
*
* Revision 1.5.16.1  2007/09/10 20:28:01  pcarter
* Implemented changes to allow compilation on RHEL 3,4,5 and MacOSX
*
* Revision 1.5  2004/02/19 16:42:13  jones
* Can set filename for the SOS matrix elements using the parameter
* s_recon_coeff_filename . If parameter is not set then uses
* sos_recon_coeff.dat
*
* Revision 1.4  1996/09/04 20:57:23  saw
* (JRA) Add target x to track definition
*
* Revision 1.3  1996/01/17 18:56:58  cdaq
* (JRA)
*
* Revision 1.2  1995/08/08 16:08:36  cdaq
* (DD) Add detector and angular offsets
*
* Revision 1.1  1994/05/13  03:50:18  cdaq
* Initial revision
*
* Abstract: Temporary routine to initialize SOS reconstruction coefficients
*           from a datafile.
*
* Output arguments:
*
*   istat   (integer) Status flag. Value returned indicates the following:
*            = 1      Normal return.
*            = 2      Datafile could not be opened.
*            = 4      Error reading datafile.
*            = 6      Datafile overflowed the internal arrays.
*
* Author:   David H. Potterveld, Argonne National Lab, Nov. 1993
* Modified: D. F. Geesaman   Add Abort, err arguments
*                            Use G_IO_CONTROL to get LUN
*______________________________________________________________________________

      implicit none

! Argument definitions.
      logical ABORT
      character*(*) err

      integer         istat

! Include files.

      include 'sos_recon_elements.cmn'  !Recon coefficients.
      include 'gen_filenames.cmn'
      include 'sos_filenames.cmn'
 
! Misc. variables.

      integer*4       i,j,chan

      character*132   line

! ============================= Executable Code ================================

! Reset flag, and zero arrays.
      err= ' '
      ABORT = .FALSE.
      s_recon_initted = 0
      do j = 1,smax_recon_elements
        do i = 1,4
          s_recon_coeff(i,j) = 0.
          s_recon_expon(i,j) = 0.
        enddo
        s_recon_expon(5,j) = 0.
      enddo

      s_ang_slope_x=0.0
      s_ang_slope_y=0.0
      s_ang_offset_x=0.0
      s_ang_offset_y=0.0
      s_det_offset_x=0.0
      s_det_offset_y=0.0
      s_z_true_focus=0.0
 
      istat = 1                         !Assume success.
! Get an I/O unit to open datafiles.
c      call G_IO_control(chan,'ANY',ABORT,err) !"ASK"="ANY"
      chan = G_LUN_TEMP

! Open and read in coefficients.

      if ( s_recon_coeff_filename .eq. ' ' ) then
         s_recon_coeff_filename = 'sos_recon_coeff.dat'
       else
      endif
      write(*,*) '*******'
       write(*,*) ' Opening SOS matrix element file ',s_recon_coeff_filename
      write(*,*) '*******'
      open (unit=chan,status='old',file=s_recon_coeff_filename,err=92)

! Read header comments.

      line = '!'
      do while (line(1:1).eq.'!')
        read (chan,1001,err=94) line
      enddo

* Read in focal plane rotation coefficients.
      do while (line(1:4).ne.' ---')
        if(line(1:13).eq.'s_ang_slope_x')read(line,1201,err=94)s_ang_slope_x
        if(line(1:13).eq.'s_ang_slope_y')read(line,1201,err=94)s_ang_slope_y
        if(line(1:14).eq.'s_ang_offset_x')read(line,1201,err=94)s_ang_offset_x
        if(line(1:14).eq.'s_ang_offset_y')read(line,1201,err=94)s_ang_offset_y
        if(line(1:14).eq.'s_det_offset_x')read(line,1201,err=94)s_det_offset_x
        if(line(1:14).eq.'s_det_offset_y')read(line,1201,err=94)s_det_offset_y
        if(line(1:14).eq.'s_z_true_focus')read(line,1201,err=94)s_z_true_focus
        read (chan,1001,err=94) line
      enddo

! Read in coefficients and exponents.
      line =' '
      read (chan,1001,err=94) line
      s_num_recon_terms = 0
      do while (line(1:4).ne.' ---')
        s_num_recon_terms = s_num_recon_terms + 1
        if (s_num_recon_terms.gt.smax_recon_elements) goto 96
        read (line,1200,err=94) (s_recon_coeff(i,s_num_recon_terms),i=1,4),
     >       (s_recon_expon(j,s_num_recon_terms),j=1,5)
        read (chan,1001,err=94) line
      enddo

! Data read in OK.

      s_recon_initted = 1
      goto 100

! File reading or data processing errors.

 92   istat = 2                         !Error opening file.
* If file does not exist, report err and then continue for development
      err = 'error opening file '//s_recon_coeff_filename
      call g_rep_err(ABORT,err)
      stop
      goto 100

 94   istat = 4                         !Error reading or processing data.
      ABORT=.true.
      err = 'error processing file'//s_recon_coeff_filename
      stop
      goto 100

 96   istat = 6                         !Too much data in file for arrays.
      ABORT=.true.
      err = 'too much data in file '//s_recon_coeff_filename
      stop
      goto 100

! Done with open file.

 100  close (unit=chan)
*     free lun
c      call G_IO_control(chan,'FREE',ABORT,err) !"FINISH"="FREE"
      return

! ============================ Format Statements ===============================

 1001 format(a)
 1200 format(1x,4g16.9,1x,5i1)
 1201 format(17x,g16.9)

      end
