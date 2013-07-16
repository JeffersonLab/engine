	subroutine g_uglast
*
* This routine cleans up after GEANT
*
* August, 1994, Pat Welch, Oregon State University, tpw@physics.orst.edu
*
* $Log: g_uglast.f,v $
* Revision 1.1  1995/03/14 21:27:32  cdaq
* Initial revision
*

	implicit none

	call glast		! print statistics and histograms

	call igend		! close HIGZ files

	end
