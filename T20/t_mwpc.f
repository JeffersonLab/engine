      subroutine t_mwpc(ABORT,err)
*
* $Log$
* Revision 1.1  1998/12/01 20:55:18  saw
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*12 here
      parameter (here= 't_trans_mwpc')
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

      integer*4 hitperw(158,9)
      integer*4 iwire, iplane, ihit
      integer*4 itdc
      real*4 rwire, rhit
      real*4 rtdc

      ABORT= .FALSE.
      err= ' '

c----------------
c ** fill user histograms: tPl1_anytdc, tPl1_tdc_vs_wire
c    and CTP variables tMwpc_pl* with last TDC value for each wire
      do iwire=1,158
	do iplane = 1,9
	  hitperw(iwire,iplane) = 0
	enddo
	tMwpc_pl1(iwire)=0
        tMwpc_pl2(iwire)=0
        tMwpc_pl3(iwire)=0
        tMwpc_pl4(iwire)=0
        tMwpc_pl5(iwire)=0
        tMwpc_pl6(iwire)=0
      enddo
      tmwpl1_wire_mult=0
      tmwpl2_wire_mult=0
      tmwpl3_wire_mult=0
      tmwpl4_wire_mult=0
      tmwpl5_wire_mult=0
      tmwpl6_wire_mult=0

      if (tmwpc_raw_tot_hits.gt.300) then
        write (6,*) 't_mwpc: total hits:', tmwpc_raw_tot_hits
      endif
      do ihit=1,tmwpc_raw_tot_hits
	iplane = tmwpc_raw_plane_num(ihit)
	iwire=tmwpc_raw_wire_num(ihit)
	rwire=float(iwire)
	itdc=tmwpc_raw_tdc(ihit)
	rtdc=float(itdc)
C
CC   Some tests
        if(iplane.lt.1.or.iplane.gt.9)then 
           write(6,*)'t_mwpc:  You have a bad plane number: ',iplane
        endif
        if(iwire.lt.1.or.iwire.gt.158)then
           write(6,*)'t_mwpc: You have a bad wire number: ',iwire,'in plane: ',iplane
	else
cc
	  if (itdc.ne.0) hitperw(iwire,iplane)=hitperw(iwire,iplane)+1
	  if(iplane.eq.1)then
	    if (tMwpc_pl1(iwire).eq.0) then		!only fill once a wire
	      tmwpl1_wire_mult=tmwpl1_wire_mult+1
	    endif
	    tMwpc_pl1(iwire)=itdc
	    call hf1(tidmwpl1_anytdc,rtdc,1.)
	    call hf2(tidmwpl1_tdc_vs_wire,rwire,rtdc,1.)
	    call hf1(tidmwpl1,rwire,1.)
	  endif
	  if(iplane.eq.2)then
	    if (tMwpc_pl2(iwire).eq.0) then		!only fill once a wire
	      tmwpl2_wire_mult=tmwpl2_wire_mult+1
	    endif
	    tMwpc_pl2(iwire)=itdc
	    call hf1(tidmwpl2_anytdc,rtdc,1.)
	    call hf2(tidmwpl2_tdc_vs_wire,rwire,rtdc,1.)
	    call hf1(tidmwpl2,rwire,1.)
	  endif
	  if(iplane.eq.3)then
	    if (tMwpc_pl3(iwire).eq.0) then		!only fill once a wire
	      tmwpl3_wire_mult=tmwpl3_wire_mult+1
	    endif
	    tMwpc_pl3(iwire)=itdc
	    call hf1(tidmwpl3_anytdc,rtdc,1.)
	    call hf2(tidmwpl3_tdc_vs_wire,rwire,rtdc,1.)
	    call hf1(tidmwpl3,rwire,1.)
	  endif
	  if(iplane.eq.4)then
	    if (tMwpc_pl4(iwire).eq.0) then		!only fill once a wire
	      tmwpl4_wire_mult=tmwpl4_wire_mult+1
	    endif
	    tMwpc_pl4(iwire)=itdc
	    call hf1(tidmwpl4_anytdc,rtdc,1.)
	    call hf2(tidmwpl4_tdc_vs_wire,rwire,rtdc,1.)
	    call hf1(tidmwpl4,rwire,1.)
	  endif
	  if(iplane.eq.5)then
	    if (tMwpc_pl5(iwire).eq.0) then		!only fill once a wire
	      tmwpl5_wire_mult=tmwpl5_wire_mult+1
	    endif
	    tMwpc_pl5(iwire)=itdc
	    call hf1(tidmwpl5_anytdc,rtdc,1.)
	    call hf2(tidmwpl5_tdc_vs_wire,rwire,rtdc,1.)
	    call hf1(tidmwpl5,rwire,1.)
	  endif
	  if(iplane.eq.6)then
	    if (tMwpc_pl6(iwire).eq.0) then		!only fill once a wire
	      tmwpl6_wire_mult=tmwpl6_wire_mult+1
	    endif
	    tMwpc_pl6(iwire)=itdc
	    call hf1(tidmwpl6_anytdc,rtdc,1.)
	    call hf2(tidmwpl6_tdc_vs_wire,rwire,rtdc,1.)
	    call hf1(tidmwpl6,rwire,1.)
          endif

C******** mwpc3

          if(iplane.ge.1.and.iplane.le.6)goto 100
          if(iwire.ne.1.and.iwire.ne.2)then
             write(6,*)'t_mwpc: You have a bad wire number in chamber3: ',iwire,'in plane: ',iplane 
          endif
          if(iplane.eq.7)then
            if(iwire.eq.1)then
               t_r_wire_pl7=rtdc
               call hf1(t_rwirepl7,rtdc,1.)
            else
               t_l_wire_pl7=rtdc
               call hf1(t_lwirepl7,rtdc,1.)
            endif   
	  endif
          if(iplane.eq.8)then
            if(iwire.eq.1)then
               t_r_wire_pl8=rtdc
               call hf1(t_rwirepl8,rtdc,1.)
            else
               t_l_wire_pl8=rtdc
               call hf1(t_lwirepl8,rtdc,1.)
            endif   
	  endif
          if(iplane.eq.9)then
            if(iwire.eq.1)then
               t_r_wire_pl9=rtdc
               call hf1(t_rwirepl9,rtdc,1.)
            else
               t_l_wire_pl9=rtdc
               call hf1(t_lwirepl9,rtdc,1.)
            endif   
	  endif
C  Some tmwpc3 sums and differences
          tmwpc3sumpl7=t_r_wire_pl7+t_l_wire_pl7
          tmwpc3diffpl7=t_r_wire_pl7-t_l_wire_pl7
          tmwpc3sumpl8=t_r_wire_pl8+t_l_wire_pl8
          tmwpc3diffpl8=t_r_wire_pl8-t_l_wire_pl8
          tmwpc3sumpl9=t_r_wire_pl9+t_l_wire_pl9
          tmwpc3diffpl9=t_r_wire_pl9-t_l_wire_pl9


100     endif
      enddo
c **  copy hits per wire into user array
      do iwire = 1,158
        rhit = float(hitperw(iwire,1))
	call hf1(tidmwpl1_MultPerWire,rhit,1.)
        rhit = float(hitperw(iwire,2))
	call hf1(tidmwpl2_MultPerWire,rhit,1.)
        rhit = float(hitperw(iwire,3))
	call hf1(tidmwpl3_MultPerWire,rhit,1.)
        rhit = float(hitperw(iwire,4))
	call hf1(tidmwpl4_MultPerWire,rhit,1.)
        rhit = float(hitperw(iwire,5))
	call hf1(tidmwpl5_MultPerWire,rhit,1.)
        rhit = float(hitperw(iwire,6))
	call hf1(tidmwpl6_MultPerWire,rhit,1.)
      enddo
*
      RETURN
      END
