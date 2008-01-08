      SUBROUTINE h_fpp_drift(hit,RoughTrack,prop_delay,
     >                       drift_time,drift_distance,ABORT,err)
*--------------------------------------------------------
*    Hall C  HMS Focal Plane Polarimeter Code
*
*  Purpose: determine fully corrected drift distance for raw hit
* 
*  Created by Frank R. Wesselmann,  February 2004
*
*--------------------------------------------------------

      IMPLICIT NONE

      include 'gen_detectorids.par'
      include 'gen_decode_common.cmn'
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'hms_fpp_params.cmn'
      INCLUDE 'hms_geometry.cmn'
      INCLUDE 'hms_fpp_event.cmn'
      INCLUDE 'hms_bypass_switches.cmn'

      character*11 here
      parameter (here= 'h_fpp_drift')

      integer*4 hit             ! number of hit in raw hits array
      real*4 RoughTrack(6)      ! rough track parameter for corrections
      real*4 prop_delay         ! wire propagation delay
      real*4 drift_time         ! fully corrected drift time
      real*4 drift_time_orig    ! un-corrected drift time
      real*4 drift_distance     ! drift distance determ. from drift time

      logical ABORT
      character*(*) err

      integer*4 Plane, Wire
      integer*4 Set,Chamber,Layer
      integer*4 ii,p,i,j
      integer*4 binno

      real*4 correction, fraction, a
      real*8 mx8,my8,mu8,Px8,Py8,alpha8

      real*4 ejbtime			! really simple time to distance calc
      real*4 ejbdrift			! really simple time to distance calc
      common /HMS_FPP_ejbdrift/ ejbtime(120,4), ejbdrift(120,4)


      ABORT= .FALSE.
      err= ' '

      drift_distance = H_FPP_BAD_DRIFT

*     * get hit data from raw array
      Plane = HFPP_raw_plane(hit)
      Wire  = HFPP_raw_wire(hit)
      drift_time = HFPP_HitTime(hit)
      drift_time_orig = HFPP_HitTime(hit)

      Set     = HFPP_plane2set(Plane)
      Chamber = HFPP_plane2chamber(Plane)
      Layer   = HFPP_plane2layer(Plane)

      if(hbypass_trans_fpp.eq.2) then
              drift_distance = abs(HFPP_drift_dist(Set,Chamber,Layer,Wire))
              return
      endif      
      
********************  corrections to drift time *******************************

* drift time is expected to measure time from particle interacting in drift
* cell until the signal is seen on the sense wire, meaning we expect the
* time values to be more positive for longer drift distances!
* in reality, we only know the trigger signal time and the time when the
* sense wire signal hits the TDC
* since the length of the signal cables and the processing delays are
* independent of the geometric event, we can consider them fixed and they are
* absorbed into the drift map or the time offset
* this leaves the following corrections:
* - trigger time: corrections to the tirgger time to obtain the actual
*   interaction time of the particle with the scintillator
* - time of flight: interaction in lower-z layers are earlier (relative to
*   trigger!) than higher-z ones
* - wire walk correction: signal needs to propagate along sense wire to
*   amplifier (readout) card and the path length depends on track geometry


*     * correct trigger time
      drift_time = drift_time - hstart_time


      if (.FALSE.) then
*       * apply time of flight correction to offset trigger time?
*       * we use the simple and consistent approach to correct based
*       * on an externally FIXED velocity based on our absolute 
*       * z position, so trigger time is interpreted to be valid at z=0
*       * whatever offset is needed needs to be absorbed into HFPP_tDriftOffset
*       * it might be nice if the particle speed was NOT fixed...
	correction = (HFPP_layerZ(Set,Chamber,Layer)+HFPP_Zoff(Set)) / HFPP_particlespeed
	drift_time = drift_time - correction 

cfrw  we could also base the TOF speed on the HMS track speed, as follows:
cfrw  p = hp_tar(HSNUM_FPTRACK)
cfrw  speed = speed_of_light * p/sqrt(p*p+hpartmass*hpartmass)

cfrw  actually, using the measured particle velocity, we can find ToF just like
cfrw  HMS DCs do -- or could because they use fixed, pre-determined ToF to each layer --
cfrw  but the event HMS reference time (hstart_time) is based on a corrected value,
cfrw  determined in  h_trans_scin.f
cfrw  using this approach, we get the velocity as  29.979*hbeta_pcent  where
cfrw    hbeta_pcent = hpcentral/sqrt(hpcentral*hpcentral+hpartmass*hpartmass)
cfrw  and the path is the wire z-coord in the same system as used by the hscin -- find
cfrw  the parameter assingment of  hscin_1x_zpos  to get details;
cfrw  the HMS reference time is calculated at z=0 is this system

      endif




      if (.TRUE.) then
*       * apply wire propagation delay correction, supplied externally
        drift_time = drift_time - prop_delay
      endif

*      write(*,*)'Drift time: ',drift_time_orig,prop_delay,correction,hstart_time,drift_time

* implement drift time "kluge" (ejb)
      if(hbypass_trans_fpp.eq.4) then
              if(drift_time.gt.4000.0) then
                   drift_distance=H_FPP_BAD_DRIFT
                   return
              endif
              if(Set.eq.1) then
                drift_distance=(drift_time+30.0)/210.0*1.38
              else
                drift_distance=(drift_time+10.0)/210.0*1.38
              endif
              if(drift_distance.lt.0)drift_distance=0.0001
              if(drift_distance.gt.1.28)drift_distance=1.28
c              write(*,*)'Kluge: ',drift_time,drift_distance
              return
      endif


********************  convert drift time to drift distance ********************
c      write(*,*)'Drift type = ',hfpp_drift_type

      if (hfpp_drift_type.eq.0) then		! no drift, use 0.5cm ***************
          drift_distance = 0.5

      elseif (hfpp_drift_type.eq.1) then		! look-up table ***************

          if (drift_time.lt.hfpp_drift_Tmin .or.
     >        drift_time.gt.hfpp_drift_Tmax	 ) then   ! skip rare random/early hit
            drift_distance = H_FPP_BAD_DRIFT
            RETURN
          endif

*         * find closest time bin for drift map
          binno = 1 + int((drift_time-hfpp_drift_Tmin)/hfpp_drift_dT)
          if (binno.lt.1 .or. binno.ge.hfpp_drift_Nbins) then	! should never happen
            drift_distance = H_FPP_BAD_DRIFT
            RETURN
          endif

*         * interpolate between two relevant time bins
          fraction = (drift_time-hfpp_drift_Tmin) / hfpp_drift_dT
          binno = 1 + int(fraction)
          fraction = fraction - float(binno) - 1.5  ! range -0.5 to 0.5

          if (fraction.lt.0.0) then !below midpoint
            fraction = -1.0*fraction
            if (binno.eq.1) then  !already at bottom bin
              drift_distance = 2.0 * (1.0-fraction) * hfpp_driftmap(Layer,binno)  !assume bottom edge of bin is 0 drift
            else
              drift_distance =      fraction  * hfpp_driftmap(Layer,binno-1)
     >    		     + (1.0-fraction) * hfpp_driftmap(Layer,binno)
            endif

          else  		    !above midpoint
            if (binno.eq.hfpp_drift_Nbins) then  !already at top bin
              drift_distance = H_FPP_BAD_DRIFT
              RETURN
            else
              drift_distance =      fraction  * hfpp_driftmap(Layer,binno+1)
     >    		     + (1.0-fraction) * hfpp_driftmap(Layer,binno)
            endif
          endif

          if (drift_distance.lt.0.0) then
            drift_distance = H_FPP_BAD_DRIFT
            RETURN
          endif


      elseif (hfpp_drift_type.eq.2) then	! polynomial ******************

          if (drift_time.gt.hfpp_drift_Tmax) then
            drift_distance = H_FPP_BAD_DRIFT
            RETURN
          elseif (drift_time.lt.hfpp_drift_Tmin) then
            drift_distance = H_FPP_BAD_DRIFT
            RETURN
          endif

          drift_distance = 0.0
          do ii=1,hfpp_drift_Nterms
            p = ii-1
            a = hfpp_drift_coeffs(Layer,ii)
            drift_distance = drift_distance + a * drift_time**p
          enddo !ii

          if (drift_distance.gt.hfpp_drift_Xmax) then
            drift_distance = H_FPP_BAD_DRIFT
            RETURN
          endif

      elseif (hfpp_drift_type.eq.3) then !simple ejb time to dist calculation
          j=(Set-1)*2+Chamber
          do i=2,100
               if (ejbtime(i,j).gt.drift_time_orig) then
                    drift_distance = ejbdrift(i,j)-
     >			(ejbdrift(i,j)-ejbdrift(i-1,j))*
     >			    ((ejbtime(i,j)-drift_time_orig)/
     >                         (ejbtime(i,j)-ejbtime(i-1,j)))
                    goto 9191
               endif
          enddo
9191      continue

          if(Set.eq.1) then
		if(drift_time_orig.lt.60.0) drift_distance=0.0001
		if(drift_time_orig.gt.495.0) drift_distance=1.25
	  else
		if(drift_time_orig.lt.75.0) drift_distance=0.0001
		if(drift_time_orig.gt.495.0) drift_distance=1.25
	  endif
          if(drift_time_orig.gt.4000.0) drift_distance = H_FPP_BAD_DRIFT
          
          if (drift_distance.gt.hfpp_drift_Xmax) then
            drift_distance = hfpp_drift_Xmax
            RETURN
          endif

      elseif (hfpp_drift_type.eq.4) then	! constant speed **************

          if (drift_time.gt.hfpp_drift_Tmax) then
            drift_distance = H_FPP_BAD_DRIFT
            RETURN
          elseif (drift_time.lt.hfpp_drift_Tmin) then
            drift_distance = H_FPP_BAD_DRIFT
            RETURN
          endif

          drift_distance = hfpp_drift_dT * drift_time
                    
          if (drift_distance.gt.hfpp_drift_Xmax) then
            drift_distance = H_FPP_BAD_DRIFT
            RETURN
          endif

          if (drift_distance.lt.0.0) then
            drift_distance = H_FPP_BAD_DRIFT
            RETURN
          endif


      elseif (hfpp_drift_type.eq.5) then	! experimental hardcoded

          if (drift_time.gt.hfpp_drift_Tmax) then
            drift_distance = H_FPP_BAD_DRIFT
            RETURN
          elseif (drift_time.lt.hfpp_drift_Tmin) then
            drift_distance = H_FPP_BAD_DRIFT
            RETURN
          endif

          drift_distance = sqrt( (drift_time-hfpp_drift_Tmin) / 
     >                           (hfpp_drift_Tmax-hfpp_drift_Tmin) )
                    
          if (drift_distance.gt.hfpp_drift_Xmax) then
            drift_distance = H_FPP_BAD_DRIFT
            RETURN
          endif

          if (drift_distance.lt.0.0) then
            drift_distance = H_FPP_BAD_DRIFT
            RETURN
          endif

      else					! bad selector ****************
          drift_distance = H_FPP_BAD_DRIFT
          write(err,*) 'unknown drift map type: ',hfpp_drift_type
          ABORT = .true.
          call g_rep_err(ABORT,err)
          RETURN
      endif




********************  corrections to drift distance ***************************


      if (.FALSE.) then
*       * apply out-of-plane correction IF NEEDED
*       * this corrects for the fact that the drift distance is the
*       * closest approach distance, which is not generally in the
*       * wire plane, but the tracking uses the in-plane distance!
*       * note that this is NOT a correction to the time but to the distance!
*       * This correction may be obviated by the drift map if it gives the
*       * in-layer coordinate already (as GARFIELD simulations might)...
        Px8 = dble(HFPP_direction(Set,Chamber,Layer,1))   !projection of u onto x
        Py8 = dble(HFPP_direction(Set,Chamber,Layer,2))   !projection of u onto y
        mx8 = dble(RoughTrack(1))
        my8 = dble(RoughTrack(3))
        mu8 = Px8*mx8 + Py8*my8
        alpha8 = datan(mu8)
	if (alpha8.ne.0.d0) then
          drift_distance = drift_distance * sngl(1.d0 / dabs(dcos(alpha8)))
	endif
      endif
 

*     * make sure the result is meaningful!
      if (drift_distance.gt.HFPP_maxdrift(Plane)) then
        drift_distance = H_FPP_BAD_DRIFT
      endif


      RETURN
      END


c==============================================================================
c==============================================================================
c==============================================================================
c==============================================================================


      SUBROUTINE h_fpp_drift_init(ABORT,err)
*--------------------------------------------------------
*    Hall C  HMS Focal Plane Polarimeter Code
*
*  Purpose: tracking in one set of FPP drift chambers
*           find best track fitted to wire centers
*           test all possible permutations until good track found
* 
*  Created by Frank R. Wesselmann,  February 2004
*
*--------------------------------------------------------

      IMPLICIT NONE

      include 'gen_detectorids.par'
      include 'gen_decode_common.cmn'
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'hms_fpp_params.cmn'

      character*16 here
      parameter (here= 'h_fpp_drift_init')

      logical ABORT
      character*(*) err

      integer LUN
      integer*4 i,Plane
      real*4 rflag
      real*4 timebins(H_FPP_DRIFT_MAX_BINS)

      real*4 ejbtime			! really simple time to distance calc
      real*4 ejbdrift			! really simple time to distance calc
      common /HMS_FPP_ejbdrift/ ejbtime(120,4), ejbdrift(120,4)


      write(6,'(''\n[47;34;1m FPP  Drift  Map: [49;0m'')')

      hfpp_drift_type = 0

      hfpp_drift_Nbins = 0
      hfpp_drift_dT   = 0.0
      hfpp_drift_Tmin = 0.0
      hfpp_drift_Tmax = 0.0
      hfpp_drift_Xmax = 0.0
      hfpp_drift_Nterms = 0

      if (hfpp_driftmap_filename.eq.' ') then
        print *,' No drift map specified for the HMS FPP chambers.'
        print *,' Using fixed drift distance of +/-0.5 cm, best fit.\n'
        RETURN
      endif


      call g_IO_control(LUN,'ANY',ABORT,err)  !get IO channel
c      write(*,*)'FPP Drift Map File:',hfpp_driftmap_filename 
      open(LUN,file=hfpp_driftmap_filename,err=900)

      read(LUN,*,err=901,end=900) rflag, hfpp_drift_Xmax
      hfpp_drift_type = int(rflag)


      if (hfpp_drift_type.eq.1) then		! look-up table ***************

          read(LUN,*,err=902,end=900) hfpp_drift_Nbins

	  if (hfpp_drift_Nbins.gt.H_FPP_DRIFT_MAX_BINS) then
	    hfpp_drift_Nbins = H_FPP_DRIFT_MAX_BINS
            write(err,*) 'Too many bins in FPP drift map ',hfpp_driftmap_filename
            call g_rep_err(ABORT,err)
	  endif

          do i=1,hfpp_drift_Nbins
	    read(LUN,*,err=902,end=900) 
     >             timebins(i), (hfpp_driftmap(Plane,i),Plane=1,H_FPP_N_PLANES)
	  enddo !i

          if (hfpp_drift_Nbins.gt.2) then
            hfpp_drift_dT = timebins(2) - timebins(1)      !midpoints of bin!!!
            hfpp_drift_Tmin = timebins(1)
     >      		    - 0.5*hfpp_drift_dT
            hfpp_drift_Tmax = timebins(hfpp_drift_Nbins)
     >                      + 0.5*hfpp_drift_dT
          else
            write(err,*) 'Only ',hfpp_drift_Nbins,' entries for FPP drift map ',hfpp_driftmap_filename
            call g_rep_err(ABORT,err)
          endif

          if (hfpp_drift_Nbins.le.0) goto 902
          if (hfpp_drift_dT.le.0.0) goto 902

          print *,' The selected drift map file uses a look-up table to determine'
          print *,' the drift in the focal plane polarimeter chambers.'
          print *,' The selected map has ',hfpp_drift_Nbins,' time bins and a maximum.'
          print *,' drift distance of ',hfpp_drift_Xmax,' cm.\n'
	  

      elseif (hfpp_drift_type.eq.2) then	! polynomial ******************

          read(LUN,*,err=903,end=900) hfpp_drift_Tmin, hfpp_drift_Tmax

          read(LUN,*,err=903,end=900) hfpp_drift_Nterms
          if (hfpp_drift_Nterms.gt.H_FPP_DRIFT_MAX_TERMS) then
            hfpp_drift_Nterms = H_FPP_DRIFT_MAX_TERMS
          endif

          do i=1,hfpp_drift_Nterms
            read(LUN,*,err=903,end=900) 
     >    	    (hfpp_drift_coeffs(Plane,i),Plane=1,H_FPP_N_PLANES)
          enddo

          print *,' The selected drift map file uses a polynomial to calculate'
          print *,' the drift in the focal plane polarimeter chambers.'
          print *,' The order of this polynomial is :',hfpp_drift_Nterms
          print *,' The applicability range of this drift map is:'
          print *,' ',hfpp_drift_Tmin,' < t_drift < ',hfpp_drift_Tmax,'\n'

      elseif (hfpp_drift_type.eq.3) then	! EJB map *********************

          print *,' The selected drift map file uses a REALLY simple look-up table to determine'
          print *,' the drift in the focal plane polarimeter chambers. (ejb)\n'
	  do i=1,100
	          read(LUN,*,err=901,end=900)ejbtime(i,1),ejbdrift(i,1)
	  enddo
	  do i=1,100
	          read(LUN,*,err=901,end=900)ejbtime(i,2),ejbdrift(i,2)
	  enddo
	  do i=1,100
	          read(LUN,*,err=901,end=900)ejbtime(i,3),ejbdrift(i,3)
	  enddo
	  do i=1,100
	          read(LUN,*,err=901,end=900)ejbtime(i,4),ejbdrift(i,4)
	  enddo

      elseif (hfpp_drift_type.eq.4) then	! constant speed **************

          read(LUN,*,err=905,end=900) hfpp_drift_dT
          read(LUN,*,err=905,end=900) hfpp_drift_Tmin, hfpp_drift_Tmax

          print *,' The selected drift map file uses constant drift'
          print *,' velocity in the focal plane polarimeter chambers.'
          print *,' The speed is :',hfpp_drift_dT,' cm/ns'
          print *,' The applicability range of this drift map is:'
          print *,'  ',hfpp_drift_Tmin,' ns < t_drift < ',hfpp_drift_Tmax,' ns'
          print *,' with a maximum drift distance of ',hfpp_drift_Xmax,' cm.\n'

      elseif (hfpp_drift_type.eq.5) then	! experimental

          read(LUN,*,err=905,end=900) hfpp_drift_Tmin, hfpp_drift_Tmax

          print *,' The selected drift map file uses a special function'
          print *,' for the focal plane polarimeter chambers.'
          print *,' The applicability range of this drift map is:'
          print *,'  ',hfpp_drift_Tmin,' ns < t_drift < ',hfpp_drift_Tmax,' ns'
          print *,' with a maximum drift distance of ',hfpp_drift_Xmax,' cm.\n'

      else					! bad selector ****************
          goto 904
      endif

      goto 990


 900  continue
      err = 'error opening drift map file: '//hfpp_driftmap_filename
      ABORT = .true.
      call g_rep_err(ABORT,err)
      goto 990

 901  continue
      err = 'error reading drift map file header: '//hfpp_driftmap_filename
      ABORT = .true.
      call g_rep_err(ABORT,err)
      goto 990

 902  continue
      err = 'error reading drift map - bad lookup table: '//hfpp_driftmap_filename
      ABORT = .true.
      call g_rep_err(ABORT,err)
      goto 990

 903  continue
      err = 'error reading drift map - bad polynomial: '//hfpp_driftmap_filename
      ABORT = .true.
      call g_rep_err(ABORT,err)
      goto 990

 904  continue
      err = 'error reading drift map - unknown drift map type: '//hfpp_driftmap_filename
      ABORT = .true.
      call g_rep_err(ABORT,err)
      goto 990

 905  continue
      err = 'error reading drift map - bad constant speed: '//hfpp_driftmap_filename
      ABORT = .true.
      call g_rep_err(ABORT,err)
      goto 990


 990  continue
      close(LUN)
      call G_IO_control(LUN,'FREE',ABORT,err) !free up IO channel
      IF(ABORT) THEN
        call G_add_path(here,err)
      ENDIF


      RETURN
      END
