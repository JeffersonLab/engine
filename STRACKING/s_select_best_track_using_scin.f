      SUBROUTINE S_SELECT_BEST_TRACK_USING_SCIN(ABORT,err)
*--------------------------------------------------------
*     -
*     -   Purpose and Methods : Select the best track through the SOS
*     -                              by see which track is closest to S2y
*     -                         or if no S2y then use closest to S2x
*     -                         if neither than smallest chi-squared.
*     -
*     -      Required Input BANKS
*     -
*     -      Output BANKS 
*     -
*     -   Output: ABORT           - success or failure
*     -         : err             - reason for failure, if any
*     - 
*     -
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      character*50 here
      parameter (here= 'S_SELECT_BEST_TRACK')
*     
      logical ABORT
      character*(*) err
*     
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'gen_routines.dec'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      INCLUDE 'sos_physics_sing.cmn'
      INCLUDE 'sos_calorimeter.cmn'
      INCLUDE 'sos_scin_parms.cmn'
      INCLUDE 'sos_scin_tof.cmn'
      INCLUDE 'sos_tracking.cmn'
      
*     
*     local variables 
      integer*4 goodtrack,track,i,j
      real*4 chi2perdeg,chi2min
      
      integer pln,cnt
      integer hit_cnt(snum_scin_planes)
      integer nhit,zz,t
      real*4 y2dmin,x2dmin,zap
      real*4  hit_pos(snum_scin_planes),hit_dist(snum_scin_planes)          
      real*4 stub_x(SNTRACKS_MAX),stub_y(SNTRACKS_MAX)
      real*4 y2d(SNTRACKS_MAX),x2d(SNTRACKS_MAX)
*--------------------------------------------------------
*     
      ABORT= .FALSE.
      err= ' '
*     Need to test to chose the best track
      SSNUM_FPTRACK = 0
      SSNUM_TARTRACK = 0
      if( SNTRACKS_FP.GT. 0) then !!! (1) !!!
         chi2min= 1e10
	 goodtrack = 0
	 y2dmin=100.
	 x2dmin=100.	 
         zap=0.  
         do track = 1, SNTRACKS_FP
            if( SNFREE_FP(track).ge. ssel_ndegreesmin) then !!! (2) !!!
               chi2perdeg = SCHI2_FP(track)/FLOAT(SNFREE_FP(track))
*     simple particle id tests
               if(( SDEDX(track,1).gt.ssel_dedx1min).and. !!! (3) !!!
     &              ( SDEDX(track,1).lt.ssel_dedx1max).and.
     &              ( SBETA(track).gt.ssel_betamin).and.
     &              ( SBETA(track).lt.ssel_betamax).and.
     &              ( STRACK_ET(track) .gt. ssel_etmin)   .and.
     &              ( STRACK_ET(track) .lt. ssel_etmax)) then
*     first, fill the arrays of which scins were hit
                  do i=1,4
                     do j=1,sscin_1x_nr
                        sscinhit(i,j)=0
                     enddo
                  enddo
                  do i=1,sscin_tot_hits
                     sscinhit(sscin_plane_num(i),sscin_counter_num(i))=1
                  enddo
c     
                  hit_pos(4)=sy_fp(track) + syp_fp(track)*(sscin_2y_zpos+0.5*sscin_2y_dzpos)
                  hit_cnt(4)=nint((shodo_center(4,1)-hit_pos(4))/sscin_2y_spacing)+1
                  hit_cnt(4)=max(min(hit_cnt(4),nint(snum_scin_counters(4))),1)                
                  hit_dist(4)=hit_pos(4)-(shodo_center(4,1)-sscin_2y_spacing*(hit_cnt(4)-1))
                  
*     * shodo_center(4.1) = 31.35
*     * sscin_2y_spacing = 7.5
*     * snum_scin_counters(4) = 10
*     * sscin_2y_zpos = 318.51
*     * sscin_2y_dzpos = 2.12
                  
                  if(sntracks_fp.gt.1) then !!! (4) !!!
                     zap=0.
                     t=0.
                     do j=1,10
                        if(sscinhit(4,j).eq.1) then
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
                  endif         !!! (4) !!!
                  
                  if(sntracks_fp.eq.1) y2d(track)=0.
                  
                  hit_pos(3)=sx_fp(track) + sxp_fp(track)*(sscin_2x_zpos
     &                 +0.5*sscin_2x_dzpos)
                  hit_cnt(3)=nint((hit_pos(3)-
     &                 shodo_center(3,1))/sscin_2x_spacing)+1
                  hit_cnt(3)=max(min(hit_cnt(3),
     &                 nint(snum_scin_counters(3))),1)
                  hit_dist(3)=hit_pos(3)-
     &                 (sscin_2x_spacing*(hit_cnt(3)-1)
     &                 +shodo_center(3,1))         
                  
                  if(sntracks_fp.gt.1) then !!! (4) !!!
                     zap=0.
                     t=0.
                     do j=1,16
                        if(sscinhit(3,j).eq.1) then
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
                  endif         !!! (4) !!!
                  
                  if(sntracks_fp.eq.1) x2d(track)=0.
                  
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
                  
                  
                  
                  
               endif            !!! (3) !!!
            endif               !!! (2) !!!      
         enddo                        
         
         
         
         if (goodtrack.eq.0) then	  
            chi2min= 1e10
            do track = 1, SNTRACKS_FP   
               if( SNFREE_FP(track).ge. ssel_ndegreesmin) then  	   
                  chi2perdeg = SCHI2_FP(track)/FLOAT(SNFREE_FP(track))
                  if(chi2perdeg.lt.chi2min) then
                     goodtrack = track
                     chi2min = chi2perdeg		     
                  endif 
               endif  		    
            enddo    
         endif	 
         

         
         SSNUM_TARTRACK = goodtrack
         SSNUM_FPTRACK  = goodtrack
	 
         
         if(goodtrack.eq.0) return ! return if no valid tracks
      endif                     !!! (1) !!!
      
      
      return
      end
      
