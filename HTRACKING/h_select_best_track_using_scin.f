      SUBROUTINE H_SELECT_BEST_TRACK_USING_SCIN(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Select the best track through the HMS
*-                              by see which track is closest to S2y
*-                         or if no S2y then use closest to S2x
*-                         if neither than smallest chi-squared.
*-
*-      Required Input BANKS
*-
*-      Output BANKS 
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*50 here
      parameter (here= 'H_SELECT_BEST_TRACK')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'gen_routines.dec'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      INCLUDE 'hms_physics_sing.cmn'
      INCLUDE 'hms_calorimeter.cmn'
      INCLUDE 'hms_scin_parms.cmn'
      INCLUDE 'hms_scin_tof.cmn'
      INCLUDE 'hms_tracking.cmn'
 
*
*     local variables 
      integer*4 goodtrack,track,i,j
      real*4 chi2perdeg,chi2min

      integer pln,cnt
      integer hit_cnt(hnum_scin_planes)
      integer nhit,zz,t
      real*4 y2dmin,x2dmin,zap
      real*4  hit_pos(hnum_scin_planes),hit_dist(hnum_scin_planes)          
      real*4 stub_x(HNTRACKS_MAX),stub_y(HNTRACKS_MAX)
      real*4 y2d(HNTRACKS_MAX),x2d(HNTRACKS_MAX)
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '
*     Need to test to chose the best track
      HSNUM_FPTRACK = 0
      HSNUM_TARTRACK = 0
      if( HNTRACKS_FP.GT. 0) then                  !!! (1) !!!
         chi2min= 1e10
	 goodtrack = 0
	 y2dmin=100.
	 x2dmin=100.	 
         zap=0.  
        do track = 1, HNTRACKS_FP
          if( HNFREE_FP(track).ge. hsel_ndegreesmin) then      !!! (2) !!!
           chi2perdeg = HCHI2_FP(track)/FLOAT(HNFREE_FP(track))
*     simple particle id tests
             if(( HDEDX(track,1).gt.hsel_dedx1min).and.        !!! (3) !!!
     &             ( HDEDX(track,1).lt.hsel_dedx1max).and.
     &             ( HBETA(track).gt.hsel_betamin).and.
     &             ( HBETA(track).lt.hsel_betamax).and.
     &             ( HTRACK_ET(track) .gt. hsel_etmin)   .and.
     &             ( HTRACK_ET(track) .lt. hsel_etmax)) then
*first, fill the arrays of which scins were hit
             do i=1,4
               do j=1,hscin_1x_nr
                hscinhit(i,j)=0
               enddo
             enddo
             do i=1,hscin_tot_hits
               hscinhit(hscin_plane_num(i),hscin_counter_num(i))=1
             enddo
c 
        hit_pos(4)=hy_fp(track) + hyp_fp(track)*(hscin_2y_zpos+0.5*hscin_2y_dzpos)
        hit_cnt(4)=nint((hhodo_center(4,1)-hit_pos(4))/hscin_2y_spacing)+1
        hit_cnt(4)=max(min(hit_cnt(4),nint(hnum_scin_counters(4))),1)                
	hit_dist(4)=hit_pos(4)-(hhodo_center(4,1)-hscin_2y_spacing*(hit_cnt(4)-1))
	 
** hhodo_center(4.1) = 31.35
** hscin_2y_spacing = 7.5
** hnum_scin_counters(4) = 10
** hscin_2y_zpos = 318.51
** hscin_2y_dzpos = 2.12

        if(hntracks_fp.gt.1) then           !!! (4) !!!
	zap=0.
	t=0.
        do j=1,10
	 if(hscinhit(4,j).eq.1) then
	  y2d(track)=abs(hit_cnt(4)-j)
	  t=t+1
	  if(t.eq.1) zap=y2d(track)
	   
	   if(t.eq.2.and.y2d(track).lt.zap) then
	    zap=y2d(track)
	   endif
	   if(t.eq.3.and.y2d(track).lt.zap) then
	    zap=y2d(track)
	   endif
	   if(t.eq.4.and.y2d(track).lt.zap) then
	    zap=y2d(track)
	   endif
	   if(t.eq.5.and.y2d(track).lt.zap) then
	    zap=y2d(track)
	   endif
	   if(t.eq.6.and.y2d(track).lt.zap) then
	    zap=y2d(track)
	   endif

         endif
	enddo 
	y2d(track)=zap
	endif                            !!! (4) !!!
	
	if(hntracks_fp.eq.1) y2d(track)=0.

        hit_pos(3)=hx_fp(track) + hxp_fp(track)*(hscin_2x_zpos+0.5*hscin_2x_dzpos)
        hit_cnt(3)=nint((hit_pos(3)-hhodo_center(3,1))/hscin_2x_spacing)+1
        hit_cnt(3)=max(min(hit_cnt(3),nint(hnum_scin_counters(3))),1)
        hit_dist(3)=hit_pos(3)-(hscin_2x_spacing*(hit_cnt(3)-1)+hhodo_center(3,1))         
      
        if(hntracks_fp.gt.1) then           !!! (4) !!!
	zap=0.
	t=0.
        do j=1,16
	 if(hscinhit(3,j).eq.1) then
	  x2d(track)=abs(hit_cnt(3)-j)
	  t=t+1
	  if(t.eq.1) zap=x2d(track)
	   if(t.eq.2.and.x2d(track).lt.zap) then
	    zap=x2d(track)
	   endif
	   if(t.eq.3.and.x2d(track).lt.zap) then
	    zap=x2d(track)
	   endif
	   if(t.eq.4.and.x2d(track).lt.zap) then
	    zap=x2d(track)
	   endif
	   if(t.eq.5.and.x2d(track).lt.zap) then
	    zap=x2d(track)
	   endif
	   if(t.eq.6.and.x2d(track).lt.zap) then
	    zap=x2d(track)
	   endif
         endif
	enddo 
	x2d(track)=zap
	endif                            !!! (4) !!!
	
	if(hntracks_fp.eq.1) x2d(track)=0.

	      if(y2d(track).le.y2dmin) then  
 	       if(y2d(track).lt.y2dmin) then
	         x2dmin=100.                
	         chi2min=1e10 
	       endif 
		 
	      if(x2d(track).le.x2dmin) then
	       if(x2d(track).lt.x2dmin) then
	         chi2min=1e10
	       endif	 
	      
 	      if(chi2perdeg.lt.chi2min) then 
	       	      		     
                  goodtrack = track
		  y2dmin=y2d(track)
		  x2dmin=x2d(track)
		  chi2min=chi2perdeg		   
              endif    		                      
 	      endif
	      endif
	      
 	       
 
	       	                                          
	    endif                   !!! (3) !!!
          endif                     !!! (2) !!!      
        enddo                        



          if (goodtrack.eq.0) then	  
           chi2min= 1e10
	   do track = 1, HNTRACKS_FP   
	   if( HNFREE_FP(track).ge. hsel_ndegreesmin) then  	   
            chi2perdeg = HCHI2_FP(track)/FLOAT(HNFREE_FP(track))
	        if(chi2perdeg.lt.chi2min) then
                 goodtrack = track
                 chi2min = chi2perdeg		     
                endif 
           endif  		    
	   enddo    
	  endif	 



        HSNUM_TARTRACK = goodtrack
        HSNUM_FPTRACK  = goodtrack
	 

        if(goodtrack.eq.0) return       ! return if no valid tracks
      endif    !!! (1) !!!
      

      return
        end
