      subroutine H_GENERATE_GEOMETRY
*
*     This subroutine reads in the wire plane parameters and fills all the
*     geometrical constants used in Track Fitting for the HMS spectrometer
*     The constants are stored in hms_geometry.cmn
*
*     d.f. geesaman           2 Sept 1993
*     modified                14 feb 1994 for CTP input.
*                             Change HPLANE_PARAM to individual arrays
* $Log: h_generate_geometry.f,v $
* Revision 1.9  1999/02/10 18:23:48  csa
* Added 4/6 tracking code (D. Meekins)
*
* Revision 1.7  1996/04/30 12:43:54  saw
* (JRA) Set up card drift time delay structures
*
* Revision 1.6  1995/10/10 13:49:33  cdaq
* (JRA) Cosmetics and comments
*
* Revision 1.5  1995/05/22 19:39:13  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.4  1995/04/06  19:28:27  cdaq
* (SAW) Remove hardwired plane and chamber counts
*
* Revision 1.3  1994/11/22  20:05:58  cdaq
* (SAW) Add h's in front of fract, aa3, det3, aainv3.
*
* Revision 1.2  1994/10/12  18:23:47  cdaq
* (DJM) Calculate 3x3 matrices and inverses
*
* Revision 1.1  1994/02/19  06:14:45  cdaq
* Initial revision
*
*
      implicit none
      include "hms_data_structures.cmn"
      include "hms_tracking.cmn"
      include "hms_geometry.cmn"
*
*     local variables
      logical missing_card_no
      integer*4 pln,i,j,k,pindex,ich,icounter
      real*4 cosalpha,sinalpha,cosbeta,sinbeta,cosgamma,singamma,z0
      real*4 stubxchi,stubxpsi,stubychi,stubypsi
      real*4 sumsqupsi,sumsquchi,sumcross,denom
*jv      real*4 hdum4of6coeff(hdc_num_planes)
      real*4 hdum4of6coeff(hmax_num_dc_planes)
*
*     read basic parameters from CTP input file
*     hdc_zpos(pln) = Z0
*     hdc_alpha_angle(pln) = ALPHA
*     hdc_beta_angle(pln)  = BETA
*     hdc_gamma_angle(pln) = GAMMA
*     hdc_pitch(pln)       = Wire spacing
*     hdc_nrwire(pln)      = Number of wires
*     hdc_central_wire(pln) = Location of center of wire 1
*     hdc_sigma(pln)       = sigma
*
      hdc_planes_per_chamber = hdc_num_planes / hdc_num_chambers

      missing_card_no = .false.
      do j=1,hmax_num_dc_planes
        do i=1,hdc_max_wires_per_plane
          if (hdc_card_no(i,j).eq.0) then
            missing_card_no = .true.
            hdc_card_no(i,j)=1        !avoid 0 in array index
            hdc_card_delay(1)=0.      !no delay for wires
          endif
        enddo
      enddo
      if (missing_card_no) write(6,*) 'missing hdc_card_no, blame JRA'
*
*     loop over all planes

      do pln=1,hdc_num_planes
        hdc_plane_num(pln)=pln
        z0=hdc_zpos(pln)
        cosalpha = cos(hdc_alpha_angle(pln))
        sinalpha = sin(hdc_alpha_angle(pln))
        cosbeta  = cos(hdc_beta_angle(pln))
        sinbeta  = sin(hdc_beta_angle(pln))
        cosgamma = cos(hdc_gamma_angle(pln))
        singamma = sin(hdc_gamma_angle(pln))
*
        hsinbeta(pln) = sinbeta
        hcosbeta(pln) = cosbeta
*     make sure cosbeta is not zero
        if(abs(cosbeta).lt.1e-10) then
          write(hluno,'('' unphysical beta rotation in hms plane'',i4,
     &      ''    beta='',f10.5)') pln,hdc_beta_angle(pln)
        endif
        htanbeta(pln) = sinbeta / cosbeta
*
* compute chi,psi to x,y,z transformation coefficient(comments are beta=gamma=0)
        hzchi(pln) = -cosalpha*sinbeta + sinalpha*cosbeta*singamma  !  =0.
        hzpsi(pln) =  sinalpha*sinbeta + cosalpha*cosbeta*singamma  !  =0.
        hxchi(pln) = -cosalpha*cosbeta - sinalpha*sinbeta*singamma  !-cos(a)
        hxpsi(pln) =  sinalpha*cosbeta - cosalpha*sinbeta*singamma  ! sin(a)
        hychi(pln) =  sinalpha*cosgamma ! sin(a)
        hypsi(pln) =  cosalpha*cosgamma ! cos(a)
*
*     stub transfromations are done in beta=gamma=0 system
        stubxchi = -cosalpha                                   !-cos(a)
        stubxpsi =  sinalpha                                   ! sin(a)
        stubychi =  sinalpha                                   ! sin(a)
        stubypsi =  cosalpha                                   ! cos(a)

* parameters for wire propogation correction. dt=distance from centerline of
* chamber = ( xcoeff*x + ycoeff*y )*corr / veloc.
        if (cosalpha .le. 0.707) then  !x-like wire, need dist. from x=0 line
          hdc_readout_x(pln) = .true.
          hdc_readout_corr(pln) = 1./sinalpha
        else                           !y-like wire, need dist. from y=0 line
          hdc_readout_x(pln) = .false.
          hdc_readout_corr(pln) = 1./cosalpha
        endif
*
*     fill hpsi0,hchi0,hz0  used in stub fit
*
        sumsqupsi = hzpsi(pln)**2 + hxpsi(pln)**2 + hypsi(pln)**2 ! =1.
        sumsquchi = hzchi(pln)**2 + hxchi(pln)**2 + hychi(pln)**2 ! =1.
        sumcross =   hzpsi(pln)*hzchi(pln) + hxpsi(pln)*hxchi(pln)
     &             + hypsi(pln)*hychi(pln)                       ! =0.
        denom = sumsqupsi*sumsquchi-sumcross**2                        ! =1.
        hpsi0(pln) = (-z0*hzpsi(pln)*sumsquchi                   !  =0.
     &                   +z0*hzchi(pln)*sumcross) / denom
        hchi0(pln) = (-z0*hzchi(pln)*sumsqupsi                   !  =0.
     &                   +z0*hzpsi(pln)*sumcross) / denom
*     calculate magnitude of hphi0                               !  =z0
        hphi0(pln) = sqrt(
     &         (z0+hzpsi(pln)*hpsi0(pln)+hzchi(pln)*hchi0(pln))**2
     &          + (hxpsi(pln)*hpsi0(pln)+hxchi(pln)*hchi0(pln))**2
     &          + (hypsi(pln)*hpsi0(pln)+hychi(pln)*hchi0(pln))**2 )
        if(z0.lt.0) hphi0(pln)=-hphi0(pln)        
*
* hstubcoef used in stub fits. check these. I don't think they are correct.
        denom = stubxpsi*stubychi - stubxchi*stubypsi     !  =1.
        hstubcoef(pln,1)= stubychi/(hdc_sigma(pln)*denom)  !sin(a)/sigma
        hstubcoef(pln,2)= -stubxchi/(hdc_sigma(pln)*denom) !cos(a)/sigma
        hstubcoef(pln,3)= hphi0(pln)*hstubcoef(pln,1)     !z0*sin(a)/sig
        hstubcoef(pln,4)= hphi0(pln)*hstubcoef(pln,2)     !z0*cos(a)/sig
*
*     xsp and ysp used in space point pattern recognition
*
        hxsp(pln) = hychi(pln) / denom                 !sin(a)
        hysp(pln) = -hxchi(pln) / denom                        !cos(a)
*
*     compute track fitting coefficients
*
        hplane_coeff(1,pln)= hzchi(pln)                                  ! =0.
        hplane_coeff(2,pln)=-hzchi(pln)                                  ! =0.
        hplane_coeff(3,pln)= hychi(pln)*(hdc_zpos(pln)-hlocrayzt) !sin(a)*(z-hlocrayzt)
        hplane_coeff(4,pln)= hxchi(pln)*(hlocrayzt-hdc_zpos(pln)) !cos(a)*(z-hlocrayzt)
        hplane_coeff(5,pln)= hychi(pln)                                  !sin(a)
        hplane_coeff(6,pln)=-hxchi(pln)                                  !cos(a)
        hplane_coeff(7,pln)= hzchi(pln)*hypsi(pln) - hychi(pln)*hzpsi(pln) !0.
        hplane_coeff(8,pln)=-hzchi(pln)*hxpsi(pln) + hxchi(pln)*hzpsi(pln) !0.
        hplane_coeff(9,pln)= hychi(pln)*hxpsi(pln) - hxchi(pln)*hypsi(pln) !1.
*
      enddo                  !  end hdc_num_planes

* djm 10/2/94 generate/store the inverse matrices HAAINV3(i,j,pindex) used in solve_3by3_hdc
* pindex = 1    plane 1 missing from hdc1
* pindex = 2    plane 2 missing from hdc1
*   etc.
* pindex = 7    plane 1 missing from hdc2
* pindex = 8    plane 2 missing from hdc2
*   etc.
* pindex = 13   hdc1 no missing planes
* pindex = 14   hdc2 no missing planes
*
*     pindex for 4/6 planes is not as meaningful.  you have to know what an
*     index represents in missing planes.  pindex now has a meaningful
*     range of 1 to 26.  from 15 to 26 there are 4/6 planes that fire and
*     both y and yprime must fire.  that leaves 6 possiblities for each
*     chamber.  the only way to setup haainv is to do it manually.  pindex
*     is used in this fashion so that the 3rd index in haainv has meaning
*     and is contiuous running from 1 to 26 instead of jumping around which
*     would be the case if it were to contain useful plane information.
*     see h_left_right.f for a list of plane configurations that are
*     attached to pindex above 14.

      do pindex=1, HDC_NUM_PLANES + HDC_NUM_CHAMBERS + 30

* generate the matrix HAA3 for an hdc missing a particular plane
        do i=1,3
          do j=1,3
            HAA3(i,j)=0.
            if(j.lt.i)then              ! HAA3 is symmetric so only calculate 6 terms
              HAA3(i,j)=HAA3(j,i)
            else
              if(pindex.le.HDC_NUM_PLANES) then
                ich = (pindex-1)/(HDC_PLANES_PER_CHAMBER)+1
                do k=(ich-1)*(HDC_PLANES_PER_CHAMBER)+1
     $               ,ich*(HDC_PLANES_PER_CHAMBER)
                  if(pindex.ne.k) then
                    HAA3(i,j)=HAA3(i,j) + hstubcoef(k,i)*hstubcoef(k,j)
                  endif
                enddo
              else 
                 if(pindex.le.(HDC_NUM_PLANES+HDC_NUM_CHAMBERS)) then
                   ich = pindex - HDC_NUM_PLANES
                   do k=(ich-1)*(HDC_PLANES_PER_CHAMBER)+1
     $                  ,ich*(HDC_PLANES_PER_CHAMBER)
                     HAA3(i,j)=HAA3(i,j) + hstubcoef(k,i)*hstubcoef(k,j)
                   enddo
                 else  ! 4/6 planes are hit
                    do icounter=1,hdc_num_planes
                       hdum4of6coeff(icounter)=1
                    enddo
                    if(pindex.eq.15.or.pindex.eq.30)then
                       hdum4of6coeff(6)=0 !plane 6 did not fire
                       hdum4of6coeff(5)=0 !plane 5 did not fire
                       hdum4of6coeff(12)=0 !plane 12 did not fire
                       hdum4of6coeff(11)=0 !plane 11 did not fire
                    endif
                    if(pindex.eq.16.or.pindex.eq.31)then
                       hdum4of6coeff(6)=0 !plane 6 did not fire
                       hdum4of6coeff(4)=0 !plane 4 did not fire
                       hdum4of6coeff(12)=0 !plane 12 did not fire
                       hdum4of6coeff(10)=0 !plane 10 did not fire
                     endif
                    if(pindex.eq.17.or.pindex.eq.32)then
                       hdum4of6coeff(6)=0 !plane 6 did not fire
                       hdum4of6coeff(3)=0 !plane 3 did not fire
                       hdum4of6coeff(12)=0 !plane 12 did not fire
                       hdum4of6coeff(9)=0 !plane 9 did not fire
                     endif
                    if(pindex.eq.18.or.pindex.eq.33)then
                       hdum4of6coeff(6)=0 !plane 6 did not fire
                       hdum4of6coeff(2)=0 !plane 2 did not fire
                       hdum4of6coeff(12)=0 !plane 12 did not fire
                       hdum4of6coeff(8)=0 !plane 9 did not fire
                    endif
                    if(pindex.eq.19.or.pindex.eq.34)then
                       hdum4of6coeff(6)=0 !plane 6 did not fire
                       hdum4of6coeff(1)=0 !plane 1 did not fire
                       hdum4of6coeff(12)=0 !plane 12 did not fire
                       hdum4of6coeff(7)=0 !plane 7 did not fire
                    endif
                    if(pindex.eq.20.or.pindex.eq.35)then
                       hdum4of6coeff(5)=0 !plane 5 did not fire
                       hdum4of6coeff(4)=0 !plane 4 did not fire
                       hdum4of6coeff(11)=0 !plane 11 did not fire
                       hdum4of6coeff(10)=0 !plane 10 did not fire
                    endif
                    if(pindex.eq.21.or.pindex.eq.36)then
                       hdum4of6coeff(5)=0 !plane 5 did not fire
                       hdum4of6coeff(3)=0 !plane 3 did not fire
                       hdum4of6coeff(11)=0 !plane 11 did not fire
                       hdum4of6coeff(9)=0 !plane 9 did not fire
                    endif
                    if(pindex.eq.22.or.pindex.eq.37)then
                       hdum4of6coeff(5)=0 !plane 5 did not fire
                       hdum4of6coeff(2)=0 !plane 2 did not fire
                       hdum4of6coeff(11)=0 !plane 11 did not fire
                       hdum4of6coeff(8)=0 !plane 8 did not fire
                    endif
                    if(pindex.eq.23.or.pindex.eq.38)then
                       hdum4of6coeff(5)=0 !plane 5 did not fire
                       hdum4of6coeff(1)=0 !plane 1 did not fire
                       hdum4of6coeff(11)=0 !plane 11 did not fire
                       hdum4of6coeff(7)=0 !plane 7 did not fire
                    endif
                    if(pindex.eq.24.or.pindex.eq.39)then
                       hdum4of6coeff(4)=0 !plane 4 did not fire
                       hdum4of6coeff(3)=0 !plane 3 did not fire
                       hdum4of6coeff(10)=0 !plane 10 did not fire
                       hdum4of6coeff(9)=0 !plane 9 did not fire
                    endif
                    if(pindex.eq.25.or.pindex.eq.40)then
                       hdum4of6coeff(4)=0 !plane 4 did not fire
                       hdum4of6coeff(2)=0 !plane 2 did not fire
                       hdum4of6coeff(10)=0 !plane 10 did not fire
                       hdum4of6coeff(8)=0 !plane 8 did not fire
                    endif
                    if(pindex.eq.26.or.pindex.eq.41)then
                       hdum4of6coeff(4)=0 !plane 4 did not fire
                       hdum4of6coeff(1)=0 !plane 1 did not fire
                       hdum4of6coeff(10)=0 !plane 10 did not fire
                       hdum4of6coeff(7)=0 !plane 7 did not fire
                    endif
                    if(pindex.eq.27.or.pindex.eq.42)then
                       hdum4of6coeff(3)=0 !plane 3 did not fire
                       hdum4of6coeff(2)=0 !plane 2 did not fire
                       hdum4of6coeff(9)=0 !plane 9 did not fire
                       hdum4of6coeff(8)=0 !plane 8 did not fire
                    endif
                    if(pindex.eq.28.or.pindex.eq.43)then
                       hdum4of6coeff(3)=0 !plane 3 did not fire
                       hdum4of6coeff(1)=0 !plane 1 did not fire
                       hdum4of6coeff(9)=0 !plane 9 did not fire
                       hdum4of6coeff(7)=0 !plane 7 did not fire
                    endif
                    if(pindex.eq.29.or.pindex.eq.44)then
                       hdum4of6coeff(2)=0 !plane 2 did not fire
                       hdum4of6coeff(1)=0 !plane 1 did not fire
                       hdum4of6coeff(8)=0 !plane 8 did not fire
                       hdum4of6coeff(7)=0 !plane 7 did not fire
                    endif
                    if(pindex.ge.15.and.pindex.le.29)then
                       ich=1
                    else 
                       ich=2
                    endif
                    do k=(ich-1)*(HDC_PLANES_PER_CHAMBER)+1
     $                  ,ich*(HDC_PLANES_PER_CHAMBER)
                       HAA3(i,j)=HAA3(i,j) + hstubcoef(k,i)*hstubcoef(k,j)
     $                      *hdum4of6coeff(k)
                    enddo
                 endif
              endif
            endif                       !end test j lt i
          enddo                         !end j loop
        enddo                           !end i loop

* form the inverse matrix HAAINV3 for each configuration
        HAAINV3(1,1,pindex)=(HAA3(2,2)*HAA3(3,3)-HAA3(2,3)**2)
        HAAINV3(1,2,pindex)=-(HAA3(1,2)*HAA3(3,3)-HAA3(1,3)*HAA3(2,3))
        HAAINV3(1,3,pindex)=(HAA3(1,2)*HAA3(2,3)-HAA3(1,3)*HAA3(2,2))
        HDET3(pindex)=HAA3(1,1)*HAAINV3(1,1,pindex)+HAA3(1,2)*HAAINV3(1,2
     $       ,pindex)+HAA3(1,3)*HAAINV3(1,3,pindex)
        if(abs(hdet3(pindex)).le.1e-20)then
          write(6,*
     $         )'******************************************************'
          write(6,*
     $         )'Warning! Determinate of matrix HAA3(i,j) is nearly zero.'
          write(6,*)'All tracks using pindex=',pindex,' will be zerfucked.'
          write(6,*)'Fix problem in h_generate_geometry.f or else!'
          write(6,*
     $         )'******************************************************'
          hdet3(pindex)=1.
        endif
        HAAINV3(1,1,pindex)=HAAINV3(1,1,pindex)/HDET3(pindex)
        HAAINV3(1,2,pindex)=HAAINV3(1,2,pindex)/HDET3(pindex)
        HAAINV3(1,3,pindex)=HAAINV3(1,3,pindex)/HDET3(pindex)
        HAAINV3(2,2,pindex)=(HAA3(1,1)*HAA3(3,3)-HAA3(1,3)**2)/HDET3(pindex
     $       )
        HAAINV3(2,3,pindex)= -(HAA3(1,1)*HAA3(2,3)-HAA3(1,2)*HAA3(3,1))
     $       /HDET3(pindex)
        HAAINV3(3,3,pindex)=(HAA3(1,1)*HAA3(2,2)-HAA3(1,2)**2)/HDET3(pindex
     $       )

      enddo                             !end pindex loop

*     for debug write out all parameters
      if(hdebugflaggeometry.ne.0) then
        write(hluno,'(''    HMS PLANE PARAMETERS: '')')
        write(hluno,'('' plane   z0      alpha      beta     gamma    wire  ''
     &                '' number  center  resolution'')')
        write(hluno,'('' number                                      spacing ''
     &                '' wires  position'')')
        write(hluno,1000) (hdc_plane_num(j),
     &                     hdc_zpos(j),
     &                     hdc_alpha_angle(j),
     &                     hdc_beta_angle(j),
     &                     hdc_gamma_angle(j),
     &                     hdc_pitch(j),
     &                     hdc_nrwire(j),
     &                     hdc_central_wire(j),
     &                     hdc_sigma(j),j=1,hdc_num_planes)
1000  format(1x,i4,f9.4,3f10.6,f8.4,i6,f10.4,f10.6)
        write(hluno,'(''  plane'',
     &        ''  hzchi     hzpsi     hxchi     hxpsi     hychi     hypsi'')')
        write(hluno,1001) (i, hzchi(i),hzpsi(i),hxchi(i),hxpsi(i),hychi(i),
     &               hypsi(i),i=1,hdc_num_planes )
1001  format(i5,6f10.6)
        write(hluno,'(''plane'',
     &        ''    hpsi0       hchi0       hphi0'')')
       write(hluno,1002) (i, hpsi0(i),hchi0(i),hphi0(i),i=1,hdc_num_planes)
1002  format(i5,3f12.6)
        write(hluno,'(''  plane'',
     &        ''     hstubcoef 1        2              3             4'')')
        write(hluno,1003) (i, hstubcoef(i,1),hstubcoef(i,2),hstubcoef(i,3),
     &         hstubcoef(i,4),i=1,hdc_num_planes)
1003  format(i5,4f15.6)
        write(hluno,'(''                 hplane_coeff'')')
        write(hluno,'('' plane     1        2       3        4        5'',
     &   ''      6       7       8        9'')')
        do j=1,hdc_num_planes
          write(hluno,1004) j,(hplane_coeff(i,j),i=1,9) 
        enddo                           ! end of print over planes loop
1004  format(1x,i3,f10.5,2f8.3,f9.3,4f8.3,f9.3)
*
      endif                               !   end if on debug print out
      return
      end
