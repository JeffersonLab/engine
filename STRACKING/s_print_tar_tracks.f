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
* $Log$
* Revision 1.1  1994/02/21 16:38:38  cdaq
* Initial revision
*
*______________________________________________________________________________

        implicit none

! Include files.

        include 'gen_data_structures.cmn'
        include 'sos_tracking.cmn'

! Misc. variables.

        integer*4        i,j,k,l,m,n,
     >                   itrk

! ============================= Executable Code ================================

! Write out header.
        write (sluno,1001) 'SOS TARGET TRACKS'
	write (sluno,1002)

! Loop over tracks.

        do itrk = 1,sntracks_tar

! Write out data lines.

	   write (sluno,1003) itrk,
     >        sx_tar(itrk),sxp_tar(itrk),
     >        sy_tar(itrk),syp_tar(itrk),
     >        sz_tar(itrk),
     >        sdelta_tar(itrk)

	enddo

	return

! ============================ Format Statements ===============================

1001    format(a)
1002	format(/,1x,'TRK',t8,'SX_TAR',t20,'SXP_TAR',t32,'SY_TAR',t44,'SYP_TAR',
     >         t56,'SZ_TAR',t68,'SDELTA_TAR')
1003	format(1x,i4,t8,6(f12.6))

	end
