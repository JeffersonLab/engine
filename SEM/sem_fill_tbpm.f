      subroutine sem_fill_tbpm()

***********************************************************
*     Author:   B. Zihlmann
*     Date:     16 june 1998
*
************************************************************
* THE ADC IS LOCATED IN THE HMS FASTBUS CRATE SLOT 14
* IN THE INPUT CHANNELS 0 - 7
*
* N_TBPM_RAW_DATA(1): I_CENTER_X 
* N_TBPM_RAW_DATA(2): INV I_CENTER_X
* N_TBPM_RAW_DATA(3): I_EDGE_X
* N_TBPM_RAW_DATA(4): INV I_EDGE_X
*
* N_TBPM_RAW_DATA(5): I_CENTER_Y 
* N_TBPM_RAW_DATA(6): INV I_CENTER_Y 
* N_TBPM_RAW_DATA(7): I_EDGE_Y
* N_TBPM_RAW_DATA(8): INV I_EDGE_Y
*
* VARIABLE N_TBPM_CUTOFF: USED TO ELIMINATE ZERO DATA DUE TO BEAM OFF
* VARIABLE N_TBPM_ADCCUT: CHECKS FOR ADC OVERFLOW
*
* ERROR MESSAGE IS CREATED WHEN ADC OVERFLOW OCCURE TOO FREQUENETLY
*******************************************************************
* changes
*
* mz 04/17/00 Changed n_fill_tbpm.f from foil numbers to mm 
*             (according to propsition from M. Steinacher)
*
* frw 11-2000  changed code to allow for out-of-plane reconstruction
*             even without raster.  
*             new hms_recon_xychoice value of 0 forces bad SEM
*             event to be interpreted as being at center of target
* frw 8-2001  new hms_recon_xychoice value of -1 forces SEM event
*             mode position to whatever user has specified
*             changed mode=0 to also use these values (0 is an option)
*******************************************************************

      IMPLICIT NONE

      character*50 here
      parameter (here= 'sem_fill_tbpm')
      
      logical echo
      parameter (echo = .false.)

      include 'sem_data_structures.cmn'
      include 'gen_data_structures.cmn'
c      include 'hms_bypass_switches.cmn'
      include 'gen_constants.par'

      
      real*4  centx, centy,sx,sy
      real*4  maxx,maxy
      integer*4 i,counter,plane

      logical*4 limitx,limity,cutoffx,cutoffy

      real*8 SEM_freq, helclock
      real*4 LIMITCOUNTER(2),EVCOUNTER

      integer PLANE_TBPM
      parameter (PLANE_TBPM=2)
      
      real*4 SOME_SCALE
      parameter (SOME_SCALE=7.5)

      SAVE

*--------------------------------------------------------


      if (echo) print *,'filling TBPM'

      EVCOUNTER=EVCOUNTER+1
 
*     * decode TBPM hit array
      do i = 1 , N_TBPM_TOT_HITS

        plane   = N_TBPM_ADDR1(i)
        counter = N_TBPM_ADDR2(i)

        if (plane .ne. PLANE_TBPM) then
          write(6,*) ' !!!!! bad SEM plane ID=',plane,
     >       ' !!!!   (should be=',PLANE_TBPM,')   counter=',counter
        elseif (counter .gt. num_tbpm) then
          write(6,*) ' !!!!! bad SEM counter ID=,counter,
     >         ' !!!!   (should be<=',num_tbpm,')'
        else
          N_TBPM_DATA(counter) = float(N_TBPM_RAW_DATA(i))
        endif

      enddo


      if (echo) print *,'TBPM decoded'


*     * subtract pedestals
      do i = 1,NUM_TBPM
        N_TBPM_DATA(i) = MAX((N_TBPM_DATA(i)
     &       - ndet_ped_tbpm(i)),0.)
      enddo


      if (echo) print *,'TBPM peds subtracted'

      if (n_tbpm_methode.eq.1) then ! THIS IS THE OLD METHODE

        if (echo) print *,'TBPM old method'

        if (N_TBPM_DATA(3).gt.N_TBPM_DATA(4)) then
          maxx = -N_TBPM_DATA(3)
        else
          maxx = N_TBPM_DATA(4)
        endif
        if (N_TBPM_DATA(7).gt.N_TBPM_DATA(8)) then
          maxy = -N_TBPM_DATA(7)
        else
          maxy = N_TBPM_DATA(8)
        endif
        
        if (N_TBPM_DATA(1).gt.N_TBPM_DATA(2)) then
          centx = -N_TBPM_DATA(1)
        else
          centx = N_TBPM_DATA(2)
        endif
        if (N_TBPM_DATA(5).gt.N_TBPM_DATA(6)) then
          centy = -N_TBPM_DATA(5)
        else
          centy = N_TBPM_DATA(6)
        endif

        if (echo) print *,'TBPM old 2'
        
        sx = (maxx+centx)/max(abs(maxx+centx),0.00001)
        sy = (maxy+centy)/max(abs(maxy+centy),0.00001)
        
        if (abs(maxx+centx).gt.0.00001) then
          ntbpmx = sx*SOME_SCALE*(1.+(maxx-centx)/(maxx+centx))
        else
          ntbpmx = sx*SOME_SCALE*(1.+(maxx-centx)/0.00001)
        endif
        
        if (abs(maxy+centy).gt.0.00001) then
          ntbpmy = sy*SOME_SCALE*(1.+(maxy-centy)/(maxy+centy))
        else
          ntbpmy = sy*SOME_SCALE*(1.+(maxy-centy)/0.00001)
        endif
        
      else   ! THIS IS THE NEW BETHER METHODE
        
        if (echo) print *,'TBPM new method'

        Sx = abs(N_TBPM_DATA(1)) + abs(N_TBPM_DATA(2)) + 
     $       abs(N_TBPM_DATA(3)) + abs(N_TBPM_DATA(4))
        Sy = abs(N_TBPM_DATA(5)) + abs(N_TBPM_DATA(6)) + 
     $       abs(N_TBPM_DATA(7)) + abs(N_TBPM_DATA(8))
        
        cutoffx = .false.
        if (Sx.gt.n_tbpm_cutoff) then
          limitx = .false.
          do i = 1,4
            if (N_TBPM_DATA(i).gt.n_tbpm_adccut) limitx=.true.
          enddo
        else
          cutoffx = .true.
        endif
        
        cutoffy = .false.
        if (Sy.gt.n_tbpm_cutoff) then
          limity = .false.
          do i = 5,8
            if (N_TBPM_DATA(i).gt.n_tbpm_adccut) limity=.true.
          enddo
        else
          cutoffy = .true.
        endif
        
        if (echo) print *,'TBPM new 2'
        
        if ((.not.cutoffy).and.(.not.cutoffx)) then
                  
          if (echo) print *,'TBPM new 2a'

          if ((.not.limitx).and.(.not.limity)) then
            
            if ((N_TBPM_DATA(1)+ N_TBPM_DATA(3)).gt.
     $           (N_TBPM_DATA(2) + N_TBPM_DATA(4))) then
              
              ntbpmx = -SOME_SCALE*(1+(N_TBPM_DATA(3)-N_TBPM_DATA(1))/
     $             max((N_TBPM_DATA(3)+N_TBPM_DATA(1)),0.00001)) + 0.5
              
            else       
              ntbpmx = SOME_SCALE*(1+(N_TBPM_DATA(4)-N_TBPM_DATA(2))/
     $             max((N_TBPM_DATA(4)+N_TBPM_DATA(2)),0.00001)) - 0.5
              
            endif
            
            if ((N_TBPM_DATA(5)+ N_TBPM_DATA(7)).gt.
     $           (N_TBPM_DATA(6) + N_TBPM_DATA(8))) then
              
              ntbpmy = -SOME_SCALE*(1+(N_TBPM_DATA(7)-N_TBPM_DATA(5))/
     $             max((N_TBPM_DATA(7)+N_TBPM_DATA(5)),0.00001)) + 0.5
              
            else       
              ntbpmy = SOME_SCALE*(1+(N_TBPM_DATA(8)-N_TBPM_DATA(6))/
     $             max((N_TBPM_DATA(8)+N_TBPM_DATA(6)),0.00001)) - 0.5
              
            endif
            
          else if((limitx).and.(.not.limity)) then
            ntbpmx = 25.
            ntbpmy = 0.
            LIMITCOUNTER(1) = LIMITCOUNTER(1) + 1
            
          else if((limity).and.(.not.limitx)) then
            ntbpmx = 0.
            ntbpmy = 25.
            LIMITCOUNTER(2) = LIMITCOUNTER(2) + 1
            
          else if (limity.and.limitx) then
            ntbpmx = 25.
            ntbpmy = 25.
            LIMITCOUNTER(1) = LIMITCOUNTER(1) + 1
            LIMITCOUNTER(2) = LIMITCOUNTER(2) + 1
            
          endif
          
        else         ! updated by frw 11-2000
                          
          if (echo) print *,'TBPM new 2b'

	  if (slow_raster_correction .eq. 1) then 
	  
	    ntbpmx = -25.   ! using SEM -- flag the
	    ntbpmy = -25.   ! occasional bad beam position info
	    
	  elseif (slow_raster_correction .eq. 0) then
	  
	    ntbpmx = n_force_SEMx     ! set beam position to
	    ntbpmy = n_force_SEMy     !  user choice
	    
	  endif
	  
	endif  !cutoff
        
      endif  !n_tbpm_methode
                          
      if (echo) print *,'TBPM beyond methode'

c      if (slow_raster_correction .eq. -1) then
c	ntbpmx = n_force_SEMx	  ! ALWAYS set beam position
c	ntbpmy = n_force_SEMy	  !  to user choice
c      endif

c      if (slow_raster_correction .eq. 4) then
c                          
c        if (echo) print *,'TBPM recon 4'
c
c*       * determine mean beam position and size based on SEM in static mode
c*       * these calculations are based on reference frequencies determined at
c*       * centroid positions of +/- 15mm and diameter values of 0mm and 30mm
c
c        helclock = g_hclock_rate
c
c        SEM_freq = g_helicityscaler(gsemx_index) / helclock
c        gsem_meanxpos = -15. + (SEM_freq - gsem_xcal_lo) * 30.
c     >                                     / (gsem_xcal_hi - gsem_xcal_lo)
c        SEM_freq = g_helicityscaler(gsemy_index) / helclock
c        gsem_meanypos = -15. + (SEM_freq - gsem_ycal_lo) * 30.
c     >                                     / (gsem_ycal_hi - gsem_ycal_lo)
c
c        SEM_freq = g_helicityscaler(gsemdx_index) / helclock
c        gsem_meanxsize = (SEM_freq - gsem_dxcal_lo) * 30.
c     >                                     / (gsem_dxcal_hi - gsem_dxcal_lo)
c
c        SEM_freq = g_helicityscaler(gsemdy_index) / helclock
c        gsem_meanysize = (SEM_freq - gsem_dycal_lo) * 30.
c     >                                     / (gsem_dycal_hi - gsem_dycal_lo)
c     
c*       * limit range -- use different error than event mode
c        if (abs(gsem_meanxpos).gt.20.) gsem_meanxpos = -22.5
c        if (abs(gsem_meanypos).gt.20.) gsem_meanypos = -22.5
c        if (abs(gsem_meanxsize).gt.40.) gsem_meanxsize = -1.
c        if (abs(gsem_meanysize).gt.40.) gsem_meanysize = -1.
c
c      else
c        if (echo) print *,'TBPM recon not 4'
c      endif
c      
c      if (echo) print *,'TBPM histogramming'
c      
c      
c      if (EVCOUNTER.GT.100.) THEN
c         IF (LIMITCOUNTER(1)/EVCOUNTER .GT. 0.1) THEN
c            PRINT *,
c     $   'ERROR: TBPM X-ADC OVERFLOW GT 10% GATE WIDTH REDUCTION REQUIRED'
c         ENDIF
c         IF (LIMITCOUNTER(2)/EVCOUNTER .GT. 0.1) THEN
c            PRINT *,
c     $   'ERROR: TBPM Y-ADC OVERFLOW GT 10% GATE WIDTH REDUCTION REQUIRED'
c         ENDIF
c         
c      ENDIF

      end
