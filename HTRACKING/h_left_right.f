      subroutine H_LEFT_RIGHT(ABORT,err)
*     This routine fits stubs to all possible left-right combinations of
*     drift distances and chooses the set with the minimum chi**2
*     It then fills the HDC_WIRE_COORD variable for each hit in a good
*     space point.
*     d. f. geesaman           17 January 1994
* $Log$
* Revision 1.13  1999/02/23 18:39:52  csa
* (JRA) Correct two typos in 4/6 code
*
* Revision 1.12  1999/02/10 18:23:41  csa
* Added 4/6 tracking code (D. Meekins)
*
* Revision 1.11  1996/01/16 21:52:43  cdaq
* (JRA)
*
* Revision 1.10  1995/10/10 15:56:36  cdaq
* (JRA) Cleanup massive nested if's.  Remove hdc_sing_wcoord stuff.
*
* Revision 1.9  1995/07/19 20:12:46  cdaq
* (SAW) Declear jibset for f2c compatibility
*
* Revision 1.8  1995/05/22  19:39:14  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.7  1995/04/06  19:29:23  cdaq
* (JRA) HMAX_NUM_DC_PLANES -> HDC_NUM_PLANES
*
* Revision 1.6  1994/10/12  18:30:52  cdaq
* (DJM) Fill hit pattern arrays
*
* Revision 1.5  1994/10/11  18:59:15  cdaq
* (DJM) Fill hdc_sing_wcoord for histogramming
*
* Revision 1.4  1994/08/16  13:26:50  cdaq
* (DJA) Fix typo (Change wc from integer*4 to real*4)
*
* Revision 1.3  1994/08/14  02:11:18  cdaq
* (DA) Change Y' in chamber 1 from plane 4 (wrong) to plane 5 (correct)
*
* Revision 1.2  1994/08/04  15:03:46  cdaq
* (DA) Incorporate small angle approximation of L/R for YY' planes
*
* Revision 1.1  1994/02/19  06:15:15  cdaq
* Initial revision
*
*
      implicit none
      save
      include 'hms_data_structures.cmn'
      include 'hms_tracking.cmn'
      include 'hms_geometry.cmn'
*
      externaljbit                            ! cernlib bit routine
      integer*4 jbit
      integer*4 jibset                  ! Declare to help f2c
*
*     local variables
*
      character*12 here
      parameter (here= 'h_left_right')
*
      logical ABORT
      character*(*) err
      integer*4 isp, ihit, iswhit, idummy,pmloop
      integer*4 nplusminus
      integer*4 numhits
      integer*4 hits(hmax_hits_per_point),pl(hmax_hits_per_point)
      integer*4 pindex,icounter
      real*4 wc(hmax_hits_per_point)
      integer*4 plane, isa_y1, isa_y2
      integer*4 plusminusknown(hmax_hits_per_point)
      real*4 plusminus(hmax_hits_per_point)
      real*4 plusminusbest(hmax_hits_per_point),tmppmbest(hmax_hits_per_point)
      real*4 tmpbeststub(hnum_fpray_param)
      real*4 chi2
      real*4 minchi2,tmpminchi2
      real*4 xp_fit, xp_expect, minxp
      real*4 stub(4)
      logical smallAngOK
*
      ABORT= .FALSE.
      err=' '

*     djm 10/2/94 added initialization/setting of gplanehdc1(isp)/2 pattern
*     units. Presently we are accepting 5/6 or 6/6 planes per chamber. 

      do isp=1,hnspace_points_tot             ! loop over all space points
*        write(6,*) 'h_left_right: check 1: spacepoint ',isp
        gplanehdc1(isp) = 0
        gplanehdc2(isp) = 0
        minchi2=1e10
        minxp=0.25
        smallAngOK = .FALSE.
        isa_y1 = 0
        isa_y2 = 0
        numhits=hspace_point_hits(isp,1)
        if (numhits.lt.0) then
           write(6,*) 'h_left_right: numhits < 0'
        elseif (numhits.eq.0) then
           write(6,*) 'h_left_right: numhits = 0'
        endif
        nplusminus=2**numhits
        do ihit=1,numhits
          hits(ihit)=hspace_point_hits(isp,2+ihit)
          pl(ihit)=HDC_PLANE_NUM(hits(ihit))
          
          if(pl(ihit).ge.1 .and. pl(ihit).le.6)then
            gplanehdc1(isp)=jibset(gplanehdc1(isp),pl(ihit)-1)
          else
            gplanehdc2(isp)=jibset(gplanehdc2(isp),pl(ihit)-7)
          endif
          
          wc(ihit)=HDC_WIRE_CENTER(hits(ihit))
          plusminusknown(ihit) = 0
          if(pl(ihit).eq.2 .OR. pl(ihit).eq.8)  isa_y1 = ihit
          if(pl(ihit).eq.5 .OR. pl(ihit).eq.11) isa_y2 = ihit
        enddo

* djm 10/2/94 check bad hdc pattern units to set the index for the inverse
* matrix AAINV(i,j,pindex).  
        if(pl(1).ge.1 .and. pl(1).le.6)then !use first hit to test if hdc1
         
          if(gplanehdc1(isp).eq.63)then
            pindex=13                   !first 6 bits set, so 6 planes hit
          else if(gplanehdc1(isp).eq.62)then
            pindex=1                    !missing lowest order bit, missing x1
          else if(gplanehdc1(isp).eq.61)then
            pindex=2
          else if(gplanehdc1(isp).eq.59)then
            pindex=3
          else if(gplanehdc1(isp).eq.55)then
            pindex=4
          else if(gplanehdc1(isp).eq.47)then
            pindex=5
          else if(gplanehdc1(isp).eq.31)then
            pindex=6
          else if(gplanehdc1(isp).eq.15)then !4/6 planes fire
            pindex=15
          else if(gplanehdc1(isp).eq.23)then !4/6 planes fire
            pindex=16
          else if(gplanehdc1(isp).eq.27)then !4/6 planes fire
            pindex=17
          else if(gplanehdc1(isp).eq.29)then !4/6 planes fire
            pindex=18
          else if(gplanehdc1(isp).eq.30)then !4/6 planes fire
            pindex=19
          else if(gplanehdc1(isp).eq.39)then !4/6 planes fire
            pindex=20
          else if(gplanehdc1(isp).eq.43)then !4/6 planes fire
            pindex=21
          else if(gplanehdc1(isp).eq.45)then !4/6 planes fire
            pindex=22
          else if(gplanehdc1(isp).eq.46)then !4/6 planes fire
            pindex=23
          else if(gplanehdc1(isp).eq.51)then !4/6 planes fire
            pindex=24
          else if(gplanehdc1(isp).eq.53)then !4/6 planes fire
            pindex=25
          else if(gplanehdc1(isp).eq.54)then !4/6 planes fire
            pindex=26
          else if(gplanehdc1(isp).eq.57)then !4/6 planes fire
            pindex=27
          else if(gplanehdc1(isp).eq.58)then !4/6 planes fire
            pindex=28
          else if(gplanehdc1(isp).eq.60)then !4/6 planes fire
            pindex=29
          else
            pindex=-1                   !multiple missing planes or other problem
          end if
          
        else                            !must be hdc2

          if(gplanehdc2(isp).eq.63)then
            pindex=14                   !first 6 bits set, so 6 planes hit
          else if(gplanehdc2(isp).eq.62)then
            pindex=7                    !missing lowest order bit, missing x1
          else if(gplanehdc2(isp).eq.61)then
            pindex=8
          else if(gplanehdc2(isp).eq.59)then
            pindex=9
          else if(gplanehdc2(isp).eq.55)then
            pindex=10
          else if(gplanehdc2(isp).eq.47)then
            pindex=11
          else if(gplanehdc2(isp).eq.31)then
            pindex=12
          else if(gplanehdc2(isp).eq.15)then !4/6 planes fire
            pindex=30
          else if(gplanehdc2(isp).eq.23)then !4/6 planes fire
            pindex=31
          else if(gplanehdc2(isp).eq.27)then !4/6 planes fire
            pindex=32
          else if(gplanehdc2(isp).eq.29)then !4/6 planes fire
            pindex=33
          else if(gplanehdc2(isp).eq.30)then !4/6 planes fire
            pindex=34
          else if(gplanehdc2(isp).eq.39)then !4/6 planes fire
            pindex=35
          else if(gplanehdc2(isp).eq.43)then !4/6 planes fire
            pindex=36
          else if(gplanehdc2(isp).eq.45)then !4/6 planes fire
            pindex=37
          else if(gplanehdc2(isp).eq.46)then !4/6 planes fire
            pindex=38
          else if(gplanehdc2(isp).eq.51)then !4/6 planes fire
            pindex=39
          else if(gplanehdc2(isp).eq.53)then !4/6 planes fire
            pindex=40
          else if(gplanehdc2(isp).eq.54)then !4/6 planes fire
            pindex=41
          else if(gplanehdc2(isp).eq.57)then !4/6 planes fire
            pindex=42
          else if(gplanehdc2(isp).eq.58)then !4/6 planes fire
            pindex=43
          else if(gplanehdc2(isp).eq.60)then !4/6 planes fire
            pindex=44
          else
            pindex=-2                   !multiple missing planes or other problem
          end if
        endif                           !end test whether hdc1 or hdc2

*        write(6,*) 'h_left_right: check 2: pindex = ',pindex

*     check if small angle L/R determination of Y and Y' planes is possible
        if(isa_y1.gt.0 .AND. isa_y2.gt.0) smallAngOK = .TRUE.
        if((hSmallAngleApprox.ne.0) .AND. (smallAngOK)) then
          if(wc(isa_y2).le.wc(isa_y1)) then
            plusminusknown(isa_y1) = -1
            plusminusknown(isa_y2) = 1
          else
            plusminusknown(isa_y1) = 1
            plusminusknown(isa_y2) = -1
          endif
          if ((numhits-2).lt.0) then
             write(6,*) 'h_left_right: numhits-2 < 0'
          elseif ((numhits-2).eq.0) then
             write(6,*) 'h_left_right: numhits-2 = 0'
          endif
          nplusminus = 2**(numhits-2)
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
            call h_find_best_stub(numhits,hits,pl,pindex,plusminus,stub,chi2)
*jv            if(hdebugstubchisq.ne.0) write(hluno,'(''hms  pmloop='',i4,
*jv     $           ''   chi2='',e14.6)') pmloop,chi2
            if(hdebugstubchisq.ne.0) write(6,'(''hms  pmloop='',i4,
     $           ''   chi2='',e14.6)') pmloop,chi2

* Take best chi2 IF x' of the stub agrees with x' as expected from x.
* Sometimes an incorrect x' gives a good chi2 for the stub, even though it is
* not the correct left/right combination for the real track.
* Rotate x'(=stub(3)) to hut coordinates and compare to x' expected from x.
* THIS ASSUMES STANDARD HMS TUNE!!!!, for which x' is approx. x/875.

            if (chi2.lt.minchi2)  then
              if ((stub(3)*htanbeta(pl(1))) .eq. -1.) then
                 write(6,*) 'h_left_right: error 3'
              endif
              xp_fit=stub(3)-htanbeta(pl(1))/(1.0+stub(3)*htanbeta(pl(1)))
              xp_expect = hspace_points(isp,1)/875. ! **TUNE DEPENDANT**
              if (abs(xp_fit-xp_expect).le.hstub_max_xpdiff) then
                minchi2=chi2
                do idummy=1,numhits
                  plusminusbest(idummy)=plusminus(idummy)
                enddo
                do idummy=1,4
                  hbeststub(isp,idummy)=stub(idummy)
                enddo
              else                      !record best stub failing angle cut, in case none pass.
                tmpminchi2=chi2
                do idummy=1,numhits
                  tmppmbest(idummy)=plusminus(idummy)
                enddo
                do idummy=1,4
                  tmpbeststub(idummy)=stub(idummy)
                enddo
              endif
            endif                       ! end if on lower chi2
          else                          ! if pindex<0 or >14
             if (pindex.ge.15.and.pindex.le.44) then  ! 4/6 plane tracking
                call h_find_best_stub(numhits,hits,pl,pindex,plusminus,stub
     $               ,chi2)
              if ((stub(3)*htanbeta(pl(1))) .eq. -1.) then
                 write(6,*) 'h_left_right: error 3'
              endif
                xp_fit=stub(3)-htanbeta(pl(1))/(1.0+stub(3)*htanbeta(pl(1)))
                if(abs(xp_fit).le.abs(minxp)) then ! tune dependent
                    minxp=xp_fit
                    minchi2=chi2
                    do icounter=1,numhits
                       plusminusbest(icounter)=plusminus(icounter)
                       hbeststub(isp,icounter)=stub(icounter)
                    enddo
                 endif
             else
                write(6,*) 'pindex=',pindex,' in h_left_right','gplanehdc1/2=',gplanehdc1(isp),gplanehdc2(isp)
             endif ! 4/6 plane tracking
          endif
        enddo                           ! end loop on possible left-right
*
*        write(6,*) 'h_left_right: ! end loop on possible left-right'

        if (minchi2.ge.9.9e+9) then     !no track passed angle cut.
          minchi2=tmpminchi2
          do idummy=1,numhits
            plusminusbest(idummy)=tmppmbest(idummy)
          enddo
          do idummy=1,4
            hbeststub(isp,idummy)=tmpbeststub(idummy)
          enddo
        endif
*
*     calculate final coordinate based on plusminusbest
*           
        do ihit=1,numhits
          HDC_WIRE_COORD(hspace_point_hits(isp,ihit+2))=
     &         HDC_WIRE_CENTER(hspace_point_hits(isp,ihit+2)) +
     &         plusminusbest(ihit)*HDC_DRIFT_DIS(hspace_point_hits(isp,ihit
     $         +2))
        enddo
*
*     stubs are calculated in rotated coordinate system
*     use first hit to determine chamber
        plane=HDC_PLANE_NUM(hits(1))
        if (hbeststub(isp,3)-htanbeta(plane) .eq. -1.) then
           write(6,*) 'h_left_right: stub3 error'
        endif
        stub(3)=(hbeststub(isp,3) - htanbeta(plane))
     &       /(1.0 + hbeststub(isp,3)*htanbeta(plane))

        if (hbeststub(isp,3)*hsinbeta(plane) .eq. -hcosbeta(plane)) then
           write(6,*) 'h_left_right: stub4 error'
        endif
        stub(4)=hbeststub(isp,4)
     &       /(hbeststub(isp,3)*hsinbeta(plane)+hcosbeta(plane))

        stub(1)=hbeststub(isp,1)*hcosbeta(plane) 
     &       - hbeststub(isp,1)*stub(3)*hsinbeta(plane)
        stub(2)=hbeststub(isp,2) 
     &       - hbeststub(isp,1)*stub(4)*hsinbeta(plane)
        hbeststub(isp,1)=stub(1)
        hbeststub(isp,2)=stub(2)
        hbeststub(isp,3)=stub(3)
        hbeststub(isp,4)=stub(4)
*
      enddo                             ! end loop over space points
*
*     write out results if debugflagstubs is set
      if(hdebugflagstubs.ne.0) then
        call h_print_stubs
      endif
      return
      end
