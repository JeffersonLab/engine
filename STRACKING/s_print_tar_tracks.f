      subroutine s_print_tar_tracks
*______________________________________________________________________________
*
* Facility: CEBAF Hall-C software.
*
* Module:   s_print_tar_tracks
*
* Version:  0.1 (In development)  18-Nov-1993 (DHP)
*
* Abstract: Dump selected track data in SOS_TARGET common block.
*
* Author:   David H. Potterveld, Argonne National Lab, Nov. 1993
* modified  D. F. Geesaman     21 Jan 94
*              changed name for s_target_dump to s_print_tar_tracks
*              made output lun sluno
* $Log: s_print_tar_tracks.f,v $
* Revision 1.3  1995/05/22 19:45:46  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1994/05/13  03:20:37  cdaq
* (DFG) Check for more than zero tracks
*
* Revision 1.1  1994/02/21  16:38:38  cdaq
* Initial revision
*
*______________________________________________________________________________

      implicit none

! Include files.

      include 'sos_data_structures.cmn'
      include 'sos_tracking.cmn'

! Misc. variables.

      integer*4 itrk

* ============================= Executable Code ===============================
      if(sntracks_tar .gt. 0 ) then
! Write out header.
        write (sluno,1001) 'SOS TARGET TRACKS'
	write (sluno,1002)

! Loop over tracks.

        do itrk = 1,sntracks_tar

! Write out data lines.

          write (sluno,1003) itrk,
     >         sx_tar(itrk),sxp_tar(itrk),
     >         sy_tar(itrk),syp_tar(itrk),
     >         sz_tar(itrk),
     >         sdelta_tar(itrk),
     >         sp_tar(itrk)

	enddo
      endif
      return

* ============================ Format Statements ==============================

 1001 format(a)
 1002 format(/,1x,'TRK',t10,'SX_TAR',t20,'SXP_TAR',t30,'SY_TAR',t40
     $     ,'SYP_TAR',t50,'SZ_TAR',t60,'SDELTA_TAR',t72,'SP_TAR')
 1003 format(1x,i2,t8,3(f10.6,f10.5),f10.5)

	end


