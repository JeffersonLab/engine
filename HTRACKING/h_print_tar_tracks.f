        subroutine h_print_tar_tracks
*+______________________________________________________________________________
*
* Facility: CEBAF Hall-C software.
*
* Module:   h_print_tar_tracks
*
* Version:  0.1 (In development)  18-Nov-1993 (DHP)
*
* Abstract: Print selected track data in HMS_TARGET common block.
*
* Author:   David H. Potterveld, Argonne National Lab, Nov. 1993
* modified: D. F. Geesaman       21 Jan 1994
*            changed name from h_target_dump to h_print_tar_tracks
*            made outpu lun hluno
* $Log$
* Revision 1.1  1994/02/19 06:17:16  cdaq
* Initial revision
*
*-______________________________________________________________________________

        implicit none

! Include files.

        include 'gen_data_structures.cmn'
        include 'hms_tracking.cmn'
! Misc. variables.

        integer*4        i,j,k,l,m,n,
     >                   itrk

! ============================= Executable Code ================================

! Write out header.
        write (hluno,1001)   'HMS TARGET TRACKS'
	write (hluno,1002)

! Loop over tracks.

        do itrk = 1,hntracks_tar

! Write out data lines.

	   write (hluno,1003) itrk,
     >        hx_tar(itrk),hxp_tar(itrk),
     >        hy_tar(itrk),hyp_tar(itrk),
     >        hz_tar(itrk),
     >        hdelta_tar(itrk)

	enddo

	return

! ============================ Format Statements ===============================

1001    format(a)
1002	format(/,1x,'TRK',t8,'HX_TAR',t20,'HXP_TAR',t32,'HY_TAR',t44,'HYP_TAR',
     >         t56,'HZ_TAR',t68,'HDELTA_TAR')
1003	format(1x,i4,t8,6(f12.6))

	end
