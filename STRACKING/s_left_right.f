      subroutine S_LEFT_RIGHT(ABORT,err)
* Warning: This routine contains lots of gobbledeguk that won't work if the
*     number of chambers is changed to 3.
*
*
*     This routine fits stubs to all possible left-right combinations of
*     drift distances and chooses the set with the minimum chi**2
*     It then fills the SDC_WIRE_COORD variable for each hit in a good
*     space point.
*     d. f. geesaman           31 August 1993
* $Log$
* Revision 1.10  1996/01/17 19:01:59  cdaq
* (JRA)
*
* Revision 1.9  1995/10/10 15:59:06  cdaq
* (JRA) Remove sdc_sing_wcoord stuff
*
* Revision 1.8  1995/08/31 18:44:23  cdaq
* (JRA) Fix some logic in small angle L/R determination loop
*
* Revision 1.7  1995/07/20  18:57:39  cdaq
* (SAW) Declare jibset for f2c compatibility
*
* Revision 1.6  1995/05/22  19:45:42  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.5  1995/05/11  21:05:32  cdaq
* (JRA) Fix errors in left right selection.  Add some commented out code
*
* Revision 1.4  1995/04/01  20:42:35  cdaq
* (SAW) Fix typos
*
* Revision 1.3  1994/12/01  21:55:08  cdaq
* (SAW) Generalize for variable # of chambers.
*       Add Small Ang approx for Brookhaven chambers
*
* Revision 1.2  1994/11/22  21:14:25  cdaq
* (SPB) Recopied from hms file and modified names for SOS
* (SAW) Don't count on Mack's monster if statement working for
*       sdc_num_chambers > 2
*
* Revision 1.1  1994/02/21  16:14:42  cdaq
* Initial revision
*
*
      implicit none
      save
      include 'sos_data_structures.cmn'
      include 'sos_tracking.cmn'
      include 'sos_geometry.cmn'
*
      external jbit                     ! cernlib bit routine
      integer*4 jbit
      integer*4 jibset                  ! Declare to help f2c
*
*     local variables
*
      character*12 here
      parameter (here= 's_left_right')
*
      logical ABORT
      character*(*) err
      integer*4 isp, ihit,iswhit, idummy, pmloop, ich
      integer*4 nplusminus
      integer*4 numhits,npaired,ihit2
      integer*4 hits(smax_hits_per_point), pl(smax_hits_per_point)
      integer*4 pindex
      real*4 wc(smax_hits_per_point)
      integer*4 plane, isa_y1, isa_y2
      integer*4 plusminusknown(smax_hits_per_point)
      real*4 plusminus(smax_hits_per_point)
      real*4 plusminusbest(smax_hits_per_point)
      real*4 chi2
      real*4 minchi2
      real*4 stub(4)
      logical smallAngOk
*
      ABORT= .FALSE.
      err=' '

      do isp=1,snspace_points_tot       ! loop over all space points
        do ich=1,sdc_num_chambers
          gplanesdc(isp,ich) = 0
        enddo
        minchi2=1e10
        smallAngOK = .FALSE.
        isa_y1 = 0
        isa_y2 = 0
        numhits=sspace_point_hits(isp,1)
        nplusminus=2**numhits
*
*     Identify which plane the space point is in.
*
        ich = (SDC_PLANE_NUM(sspace_point_hits(isp,2+1))-1)
     $       /(sdc_planes_per_chamber)+1
        do ihit=1,numhits
          hits(ihit)=sspace_point_hits(isp,2+ihit)
          pl(ihit)=SDC_PLANE_NUM(hits(ihit))
          
          gplanesdc(isp,ich)=jibset(gplanesdc(isp,ich),pl(ihit)-
     $         ((ich-1)*(sdc_planes_per_chamber)+1))

c          if(pl(ihit).ge.1 .and. pl(ihit).le.6)then
c            gplanesdc1(isp)=jibset(gplanesdc1(isp),pl(ihit)-1)
c          else
c            gplanesdc2(isp)=jibset(gplanesdc2(isp),pl(ihit)-7)
c          endif

          wc(ihit)=SDC_WIRE_CENTER(hits(ihit))
          plusminusknown(ihit) = 0
          if(s_hms_style_chambers.eq.1) then
            if(pl(ihit).eq.2 .OR. pl(ihit).eq.8)  isa_y1 = ihit
            if(pl(ihit).eq.5 .OR. pl(ihit).eq.11) isa_y2 = ihit
          endif
        enddo
          

* djm 10/2/94 check bad sdc pattern units to set the index for the inverse
* matrix SAAINV(i,j,pindex).
*
        if(gplanesdc(isp,ich).eq.'3F'x) then
          pindex=sdc_num_planes+ich
        else if (gplanesdc(isp,ich).eq.'3E'x) then
          pindex=(ich-1)*(sdc_planes_per_chamber) + 1
        else if (gplanesdc(isp,ich).eq.'3D'x) then
          pindex=(ich-1)*(sdc_planes_per_chamber) + 2
        else if (gplanesdc(isp,ich).eq.'3B'x) then
          pindex=(ich-1)*(sdc_planes_per_chamber) + 3
        else if (gplanesdc(isp,ich).eq.'37'x) then
          pindex=(ich-1)*(sdc_planes_per_chamber) + 4
        else if (gplanesdc(isp,ich).eq.'2F'x) then
          pindex=(ich-1)*(sdc_planes_per_chamber) + 5
        else if (gplanesdc(isp,ich).eq.'1F'x) then
          pindex=(ich-1)*(sdc_planes_per_chamber) + 6
        else
          pindex=-1
        endif

*     check if small angle L/R determination of Y and Y' planes is possible
        if(sSmallAngleApprox.ne.0) then
          if(s_hms_style_chambers.eq.1) then
            if(isa_y1.gt.0 .AND. isa_y2.gt.0) then
              if(wc(isa_y2).le.wc(isa_y1)) then
                plusminusknown(isa_y1) = -1
                plusminusknown(isa_y2) = 1
              else
                plusminusknown(isa_y1) = 1
                plusminusknown(isa_y2) = -1
              endif
              nplusminus = 2**(numhits-2)
            endif
          else                          ! SOS chambers
*
*     Brookhaven chamber L/R code
*     Can we assume that hits are sorted by plane?  As best I (SAW) can
*     tell, we can not.
*
            ihit = 1
            npaired = 0
            do ihit=1,numhits
              if(pl(ihit)-2*(pl(ihit)/2) .eq. 1) then ! Odd plane
                do ihit2=1,numhits      ! Look for the adjacent plane
                  if(pl(ihit2)-pl(ihit).eq.1) then ! Adjacent plane found
                    if(wc(ihit2).le.wc(ihit)) then
                      plusminusknown(ihit) = -1
                      plusminusknown(ihit2) = 1
                    else
                      plusminusknown(ihit) = 1
                      plusminusknown(ihit2) = -1
                    endif
                    npaired = npaired + 2
                  endif
                enddo
              endif
            enddo
          endif
          nplusminus = 2**(numhits-npaired)
*     Let's hope that following code will work with nplusminus = 1
        endif

*     use bit value of integer word to set + or -
        do pmloop=0,nplusminus-1
          iswhit = 1
          do ihit=1,numhits
            if(plusminusknown(ihit).ne.0) then
              plusminus(ihit) = float(plusminusknown(ihit))
            else
              if(jbit(pmloop,iswhit).eq.1) then
                plusminus(ihit)=1.0
              else
                plusminus(ihit)=-1.0
              endif
              iswhit = iswhit + 1
            endif
          enddo

          if (pindex.ge.0 .and. pindex.le.14) then
            call s_find_best_stub(numhits,hits,pl,pindex,plusminus,stub,chi2)
            if(sdebugstubchisq.ne.0) then
              write(sluno,'('' sos pmloop='',i4,''   chi2='',e14.6)')
     &             pmloop,chi2
            endif
            if (chi2.lt.minchi2)  then
              minchi2=chi2
              do idummy=1,numhits
                plusminusbest(idummy)=plusminus(idummy)
              enddo
              do idummy=1,4
                sbeststub(isp,idummy)=stub(idummy)
              enddo
            endif                       ! end if on lower chi2
          else                          ! if pindex<0 or >14
            write(6,*) 'pindex=',pindex,' in s_left_right'
          endif
        enddo                           ! end loop on possible left-right
*
*     calculate final coordinate based on plusminusbest
*           
        do ihit=1,numhits
          SDC_WIRE_COORD(sspace_point_hits(isp,ihit+2))=
     &         SDC_WIRE_CENTER(sspace_point_hits(isp,ihit+2)) +
     &         plusminusbest(ihit)*SDC_DRIFT_DIS(sspace_point_hits(isp,ihit
     $         +2))
        enddo
*
*     stubs are calculated in rotated coordinate system
*     use first hit to determine chamber
        plane=SDC_PLANE_NUM(hits(1))
        stub(3)=(sbeststub(isp,3) - stanbeta(plane))
     &       /(1.0 + sbeststub(isp,3)*stanbeta(plane))
        stub(4)=sbeststub(isp,4)
     &       /(sbeststub(isp,3)*ssinbeta(plane)+scosbeta(plane))
        
        stub(1)=sbeststub(isp,1)*scosbeta(plane) 
     &       - sbeststub(isp,1)*stub(3)*ssinbeta(plane)
        stub(2)=sbeststub(isp,2) 
     &       - sbeststub(isp,1)*stub(4)*ssinbeta(plane)
        sbeststub(isp,1)=stub(1)
        sbeststub(isp,2)=stub(2)
        sbeststub(isp,3)=stub(3)
        sbeststub(isp,4)=stub(4)

*
      enddo                             ! end loop over space points
*
*     write out results if sdebugflagstubs is set
      if(sdebugflagstubs.ne.0) then
        call s_print_stubs
      endif
      return
      end
