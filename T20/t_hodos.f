      subroutine t_hodos(ABORT,err)
*
* $Log$
* Revision 1.1  1998/12/01 20:56:31  saw
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*12 here
      parameter (here= '')
*
      logical ABORT
      character*(*) err
*
      include 't20_data_structures.cmn'
      include 't20_tracking.cmn'
      include 't20_geometry.cmn'
      include 't20_track_histid.cmn'
      include 't20_bypass_switches.cmn'	
      include 't20_hodo.cmn'
*--------------------------------------------------------
c
      integer*4 ibar, ihit, iplane
      real*4 ribar, rch, rtdc
*      integer th_hodmaxbar(4) /30,30,24,24/
c      integer th_hodmaxbar(4) /60,60,60,60/	!temporary
      integer th_hodmaxbar(4)
      data th_hodmaxbar /60,60,60,60/                     !temporary

      ABORT= .FALSE.
      err= ' '

c----------------------------------------
c ** fill th*pl*_tdc_i(ibar): most recent TDC value for each bar, each plane
c ** fill th*pl*_tdc_all: for each plane, fill all tdc values into one hosto.

      do ibar=1,60
	th1p1_tdc_i(ibar)=0
	th1p2_tdc_i(ibar)=0
	th2p1_tdc_i(ibar)=0
	th2p2_tdc_i(ibar)=0
      enddo

      do ihit=1,thodo_tot_hits
	ibar=thodo_bar_num(ihit)
	ribar=float(ibar)
	iplane = thodo_plane_num(ihit)
	if (ibar.le.0.or.ibar.gt.th_hodmaxbar(iplane)) then
	  write(6,*) 'shpl1 ibar=',ibar
	else
	  rtdc= float(thodo_tdc_val(ihit))
	  rch = float((iplane-1)*30+ibar)	   !all 4 planes, offset by 30
	  call hf2(tidhod_allbars_vs_tdc,rch,rtdc,1.)!tdc vs bar for all 4 planes
	  if(iplane.eq.1)then
	    th1p1_tdc_i(ibar)=thodo_tdc_val(ihit)
	    call hf1(tidh1p1_tdc_all,ribar,1.)
	  endif
	  if(iplane.eq.2)then
	    th1p2_tdc_i(ibar)=thodo_tdc_val(ihit)
	    call hf1(tidh1p2_tdc_all,ribar,1.)
	  endif
	  if(iplane.eq.3)then
	    th2p1_tdc_i(ibar)=thodo_tdc_val(ihit)
	    call hf1(tidh2p1_tdc_all,ribar,1.)
	  endif
	  if(iplane.eq.4)then
	    th2p2_tdc_i(ibar)=thodo_tdc_val(ihit)
	    call hf1(tidh2p2_tdc_all,ribar,1.)
	  endif
 	endif
      enddo 
c----------------------------------------

      RETURN
      END
*********
*     Local Variables:
*     mode: fortran
*     fortran-if-indent: 2
*     fortran-do-indent: 2
*     End:

