      subroutine h_targ_trans_init(ABORT,err,istat)
*______________________________________________________________________________
*
* Facility: CEBAF Hall-C software.
*
* Module:   h_targ_trans_init
*
* Version:  0.1 (In development)
* $Log$
* Revision 1.5  1996/09/04 13:34:30  saw
* (JRA) Add target x to track definition
*
* Revision 1.4  1995/08/08 16:01:37  cdaq
* (DD) Add detector and angular offsets
*
* Revision 1.3  1995/05/11  19:13:27  cdaq
* (JRA) Fix errors in reading of focal plane rot coeffs
*
* Revision 1.2  1995/04/06  19:32:19  cdaq
* (SAW) Put in ddutta's pre cosy x-x', y-y' transformation
*
* Revision 1.1  1994/05/13  03:51:55  cdaq
* Initial revision
*
*
* Abstract: Temporary routine to initialize HMS reconstruction coefficients
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

* Argument definitions.
      logical ABORT
      character*(*) err

      integer         istat

* Include files.

      include 'hms_recon_elements.cmn'  !Recon coefficients.
      include 'gen_filenames.cmn'
 
* Misc. variables.

      integer*4       i,j,k,l,m,n,chan
      logical*4       opened

      character*132   line

*============================= Executable Code ================================

* Reset flag, and zero arrays.
      err= ' '
      ABORT = .FALSE.
      h_recon_initted = 0
      do j = 1,hmax_recon_elements
         do i = 1,4
            h_recon_coeff(i,j) = 0.
            h_recon_expon(i,j) = 0.
         enddo
         h_recon_expon(5,j) = 0.
      enddo
      h_ang_slope_x=0.0
      h_ang_slope_y=0.0
      h_ang_offset_x=0.0
      h_ang_offset_y=0.0
      h_det_offset_x=0.0
      h_det_offset_y=0.0
      h_z_true_focus=0.0

      istat = 1                         !Assume success.
* Get an I/O unit to open datafiles.
c      call G_IO_control(chan,'ANY',ABORT,err) !"ASK"="ANY"
      chan = G_LUN_TEMP

* Open and read in coefficients.

      open (unit=chan,status='old',name='hms_recon_coeff.dat',err=92)

* Read header comments.

      line = '!'
      do while (line(1:1).eq.'!')
        read (chan,1001,err=94) line
      enddo

* Read in focal plane rotation coefficients.
      do while (line(1:4).ne.' ---')
        if(line(1:13).eq.'h_ang_slope_x')read(line,1201,err=94)h_ang_slope_x
        if(line(1:13).eq.'h_ang_slope_y')read(line,1201,err=94)h_ang_slope_y
        if(line(1:14).eq.'h_ang_offset_x')read(line,1201,err=94)h_ang_offset_x
        if(line(1:14).eq.'h_ang_offset_y')read(line,1201,err=94)h_ang_offset_y
        if(line(1:14).eq.'h_det_offset_x')read(line,1201,err=94)h_det_offset_x
        if(line(1:14).eq.'h_det_offset_y')read(line,1201,err=94)h_det_offset_y
        if(line(1:14).eq.'h_z_true_focus')read(line,1201,err=94)h_z_true_focus
        read (chan,1001,err=94) line
      enddo
* Read in reconstruction coefficients and exponents.
      line=' '
      read (chan,1001,err=94) line
      h_num_recon_terms = 0
      do while (line(1:4).ne.' ---')
         h_num_recon_terms = h_num_recon_terms + 1
         if (h_num_recon_terms.gt.hmax_recon_elements) goto 96
         read (line,1200,err=94) (h_recon_coeff(i,h_num_recon_terms),i=1,4)
     $        ,(h_recon_expon(j,h_num_recon_terms),j=1,5)
         read (chan,1001,err=94) line
      enddo

* Data read in OK.

      h_recon_initted = 1
      goto 100

* File reading or data processing errors.

 92   istat = 2                         !Error opening file.
* If file does not exist, report err and then continue for development
      err = 'error opening file hms_recon_coeff.dat'
      call g_rep_err(ABORT,err)
      goto 100

 94   istat = 4                         !Error reading or processing data.
      ABORT=.true.
      err = 'error processing file hms_recon_coeff.dat'
      goto 100

 96   istat = 6                         !Too much data in file for arrays.
      ABORT=.true.
      err = 'too much data in file hms_recon_coeff.dat'
      goto 100

* Done with open file.

 100  close (unit=chan)
*     free lun
c      call G_IO_control(chan,'FREE',ABORT,err) !"FINISH"="FREE"
      return

*============================ Format Statements ===============================

 1001 format(a)
 1200 format(1x,4g16.9,1x,5i1)
 1201 format(17x,g16.9)
      
      end
