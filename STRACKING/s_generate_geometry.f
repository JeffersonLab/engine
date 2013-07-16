      subroutine s_generate_geometry
*
*     This subroutine reads in the wire plane parameters and fills all the
*     geometrical constants used in Track Fitting for the SOS spectrometer
*     The constants are stored in sos_geometry.cmn
*
*     d.f. geesaman           2 Sept 1993
*     modified                14 feb 1994 for CTP input.
*                             Change SPLANE_PARAM to individual arrays
* $Log: s_generate_geometry.f,v $
* Revision 1.7  1996/09/05 13:30:18  saw
* (JRA) Format statement changes
*
* Revision 1.6  1996/04/30 17:13:09  saw
* (JRA) Set up card drift time delay structures
*
* Revision 1.5  1995/10/10 14:21:21  cdaq
* (JRA) Calculate wire velocity correction parameters.  Cosmetics and comments
*
* Revision 1.4  1995/05/22 19:45:40  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.3  1995/04/01  20:42:06  cdaq
* (SAW) Use sdc_planes_per_chamber instead of (sdc_num_planes/sdc_num_chambers)
*
* Revision 1.2  1994/11/22  20:19:22  cdaq
* (SPB) Recopied from hms file and modified names for SOS
* (SAW) Remove hardwired plane and chamber counts.
*
* Revision 1.1  1994/02/21  16:14:17  cdaq
* Initial revision
*
*
      implicit none
      include "sos_data_structures.cmn"
      include "sos_tracking.cmn"
      include "sos_geometry.cmn"
*
*     local variables
      logical missing_card_no
      integer*4 pln,i,j,k,pindex,ich
      real*4 cosalpha,sinalpha,cosbeta,sinbeta,cosgamma,singamma,z0
      real*4 stubxchi,stubxpsi,stubychi,stubypsi
      real*4 sumsqupsi,sumsquchi,sumcross,denom
*
*     read basic parameters from CTP input file
*     sdc_zpos(pln) = Z0
*     sdc_alpha_angle(pln) = ALPHA
*     sdc_beta_angle(pln)  = BETA
*     sdc_gamma_angle(pln) = GAMMA
*     sdc_pitch(pln)       = Wire spacing
*     sdc_nrwire(pln)      = Number of wires
*     sdc_central_wire(pln) = Location of center of wire 1
*     sdc_sigma(pln)       = sigma
*
      sdc_planes_per_chamber = sdc_num_planes / sdc_num_chambers

      missing_card_no = .false.
      do j=1,smax_num_dc_planes
        do i=1,sdc_max_wires_per_plane
          if (sdc_card_no(i,j).eq.0) then
       write(6,*) 'card number = 0 for wire,plane=',i,j
            missing_card_no = .true.
            sdc_card_no(i,j)=1        !avoid 0 in array index
            sdc_card_delay(1)=0      !no delay for wires
          endif
        enddo
      enddo
      if (missing_card_no) write(6,*) 'missing sdc_card_no(IGNORE THIS-JRA)'
*
*     loop over all planes
*
      do pln=1,sdc_num_planes
        sdc_plane_num(pln)=pln
        z0=sdc_zpos(pln)
        cosalpha = cos(sdc_alpha_angle(pln))
        sinalpha = sin(sdc_alpha_angle(pln))
        cosbeta  = cos(sdc_beta_angle(pln))
        sinbeta  = sin(sdc_beta_angle(pln))
        cosgamma = cos(sdc_gamma_angle(pln))
        singamma = sin(sdc_gamma_angle(pln))
*
        ssinbeta(pln) = sinbeta
        scosbeta(pln) = cosbeta
*     make sure cosbeta is not zero
        if(abs(cosbeta).lt.1e-10) then
          write(sluno,'('' unphysical beta rotation in sos plane'',i4,
     &      ''    beta='',f10.5)') pln,sdc_beta_angle(pln)
        endif
        stanbeta(pln) = sinbeta / cosbeta
*
* compute chi,psi to x,y,z transformation coefficient(comments are beta=gamma=0)
        szchi(pln) = -cosalpha*sinbeta + sinalpha*cosbeta*singamma   !  =0.
        szpsi(pln) =  sinalpha*sinbeta + cosalpha*cosbeta*singamma   !  =0.
        sxchi(pln) = -cosalpha*cosbeta - sinalpha*sinbeta*singamma   !-cos(a)
        sxpsi(pln) =  sinalpha*cosbeta - cosalpha*sinbeta*singamma   ! sin(a)
        sychi(pln) =  sinalpha*cosgamma                                     ! sin(a)
        sypsi(pln) =  cosalpha*cosgamma                                     ! cos(a)
*
*     stub transformations are done in beta=gamma=0 system
        stubxchi = -cosalpha                                   !-cos(a)
        stubxpsi =  sinalpha                                   ! sin(a)
        stubychi =  sinalpha                                   ! sin(a)
        stubypsi =  cosalpha                                   ! cos(a)

* parameters for wire propogation correction. dt=distance from centerline of
* chamber = ( xcoeff*x + ycoeff*y )*corr / veloc.
        if (cosalpha .le. 0.707) then  !x-like wire, need dist. from x=0 line
          sdc_readout_x(pln) = .true.
          sdc_readout_corr(pln) = 1./sinalpha
        else                           !y-like wire, need dist. from y=0 line
          sdc_readout_x(pln) = .false.
          sdc_readout_corr(pln) = 1./cosalpha
        endif
*
*     fill spsi0,schi0,sz0  used in stub fit
*
        sumsqupsi = szpsi(pln)**2 + sxpsi(pln)**2 + sypsi(pln)**2 ! =1.
        sumsquchi = szchi(pln)**2 + sxchi(pln)**2 + sychi(pln)**2 ! =1.
        sumcross =   szpsi(pln)*szchi(pln) + sxpsi(pln)*sxchi(pln)
     &             + sypsi(pln)*sychi(pln)                       ! =0.
        denom = sumsqupsi*sumsquchi-sumcross**2                           ! =1.
        spsi0(pln) = (-z0*szpsi(pln)*sumsquchi                   ! =0.
     &                   +z0*szchi(pln)*sumcross) / denom
        schi0(pln) = (-z0*szchi(pln)*sumsqupsi                   ! =0.
     &                   +z0*szpsi(pln)*sumcross) / denom
* calculate magnitude of sphi0                                   ! =z0
        sphi0(pln) = sqrt(
     &         (z0+szpsi(pln)*spsi0(pln)+szchi(pln)*schi0(pln))**2
     &          + (sxpsi(pln)*spsi0(pln)+sxchi(pln)*schi0(pln))**2
     &          + (sypsi(pln)*spsi0(pln)+sychi(pln)*schi0(pln))**2 )
        if(z0.lt.0) sphi0(pln)=-sphi0(pln)        
*
* sstubcoef used in stub fits. check these.  I don't think they are correct
        denom = stubxpsi*stubychi - stubxchi*stubypsi          !  =1.
        sstubcoef(pln,1)= stubychi/(sdc_sigma(pln)*denom)      !sin(a)/sigma
        sstubcoef(pln,2)= -stubxchi/(sdc_sigma(pln)*denom)     !cos(a)/sigma
        sstubcoef(pln,3)= sphi0(pln)*sstubcoef(pln,1)          !z0*sin(a)/sig
        sstubcoef(pln,4)= sphi0(pln)*sstubcoef(pln,2)          !z0*cos(a)/sig
*
*     xsp and ysp used in space point pattern recognition
*
        sxsp(pln) = sychi(pln) / denom  !sin(a)
        sysp(pln) = -sxchi(pln) / denom !cos(a)
*
*     compute track fitting coefficients
*
        splane_coeff(1,pln)= szchi(pln)                                  !  =0.
        splane_coeff(2,pln)=-szchi(pln)                                  !  =0.
        splane_coeff(3,pln)= sychi(pln)*(sdc_zpos(pln)-slocrayzt) !sin(a)*(z-slocrayzt)
        splane_coeff(4,pln)= sxchi(pln)*(slocrayzt-sdc_zpos(pln)) !cos(a)*(z-slocrayzt)
        splane_coeff(5,pln)= sychi(pln)                                  !sin(a)
        splane_coeff(6,pln)=-sxchi(pln)                                  !cos(a)
        splane_coeff(7,pln)= szchi(pln)*sypsi(pln) - sychi(pln)*szpsi(pln) !0.
        splane_coeff(8,pln)=-szchi(pln)*sxpsi(pln) + sxchi(pln)*szpsi(pln) !0.
        splane_coeff(9,pln)= sychi(pln)*sxpsi(pln) - sxchi(pln)*sypsi(pln) !1.
*
      enddo                  !  end loop over all planes
      
* djm 10/2/94 generate/store the inverse matrices SAAINV3(i,j,pindex) used in solve_3by3_hdc
* pindex = 1    plane 1 missing from sdc1
* pindex = 2    plane 2 missing from sdc1
*   etc.
* pindex = 7    plane 1 missing from sdc2
* pindex = 8    plane 2 missing from sdc2
*   etc.
* pindex = 13   sdc1 no missing planes
* pindex = 14   sdc2 no missing planes

*
*     The following is pretty gross, but might actually work for an
*     arbitrary number of chambers if each chamber has the same number of
*     planes and sdc_num_planes is SDC_NUM_CHAMBERS * # of planes/chamber
*
      do pindex=1,sdc_num_planes+SDC_NUM_CHAMBERS

* generate the matrix SAA3 for an sdc missing a particular plane
        do i=1,3
          do j=1,3
            SAA3(i,j)=0.
            if(j.lt.i)then              ! SAA3 is symmetric so only calculate 6 terms
              SAA3(i,j)=SAA3(j,i)
            else
              if(pindex.le.sdc_num_planes) then
                ich = (pindex-1)/(sdc_planes_per_chamber)+1
                do k=(ich-1)*(sdc_planes_per_chamber)+1
     $               ,ich*(sdc_planes_per_chamber)
                  if(pindex.ne.k) then
                    SAA3(i,j)=SAA3(i,j) + sstubcoef(k,i)*sstubcoef(k,j)
                  endif
                enddo
              else
                ich = pindex - sdc_num_planes
                do k=(ich-1)*(sdc_planes_per_chamber)+1
     $               ,ich*(sdc_planes_per_chamber)
                  SAA3(i,j)=SAA3(i,j) + sstubcoef(k,i)*sstubcoef(k,j)
                enddo
              endif
            endif                       !end test j lt i
          enddo                         !end j loop
        enddo                           !end i loop

* form the inverse matrix SAAINV3 for each configuration
        SAAINV3(1,1,pindex)=(SAA3(2,2)*SAA3(3,3)-SAA3(2,3)**2)
        SAAINV3(1,2,pindex)=-(SAA3(1,2)*SAA3(3,3)-SAA3(1,3)*SAA3(2,3))
        SAAINV3(1,3,pindex)=(SAA3(1,2)*SAA3(2,3)-SAA3(1,3)*SAA3(2,2))
        SDET3(pindex)=SAA3(1,1)*SAAINV3(1,1,pindex)+SAA3(1,2)*SAAINV3(1,2
     $       ,pindex)+SAA3(1,3)*SAAINV3(1,3,pindex)
        if(abs(sdet3(pindex)).le.1e-20)then
          write(6,*
     $         )'******************************************************'
          write(6,*
     $         )'Warning! Determinate of matrix SAA3(i,j) is nearly zero.'
          write(6,*)'All tracks using pindex=',pindex,' will be zerfucked.'
          write(6,*)'Fix problem in h_generate_geometry.f or else!'
          write(6,*
     $         )'******************************************************'
          sdet3(pindex)=1.
        endif
        SAAINV3(1,1,pindex)=SAAINV3(1,1,pindex)/SDET3(pindex)
        SAAINV3(1,2,pindex)=SAAINV3(1,2,pindex)/SDET3(pindex)
        SAAINV3(1,3,pindex)=SAAINV3(1,3,pindex)/SDET3(pindex)
        SAAINV3(2,2,pindex)=(SAA3(1,1)*SAA3(3,3)-SAA3(1,3)**2)/SDET3(pindex
     $       )
        SAAINV3(2,3,pindex)= -(SAA3(1,1)*SAA3(2,3)-SAA3(1,2)*SAA3(3,1))
     $       /SDET3(pindex)
        SAAINV3(3,3,pindex)=(SAA3(1,1)*SAA3(2,2)-SAA3(1,2)**2)/SDET3(pindex
     $       )

      enddo                             !end pindex loop
      
*     for debug write out all parameters
      if(sdebugflaggeometry.ne.0) then
        write(sluno,'(''    SOS PLANE PARAMETERS: '')')
        write(sluno,'('' plane   z0      alpha      beta     gamma    wire  ''
     &                '' number  center  resolution'')')
        write(sluno,'('' number                                      spacing ''
     &                '' wires  position'')')
        write(sluno,1000) (sdc_plane_num(j),
     &                     sdc_zpos(j),
     &                     sdc_alpha_angle(j),
     &                     sdc_beta_angle(j),
     &                     sdc_gamma_angle(j),
     &                     sdc_pitch(j),
     &                     sdc_nrwire(j),
     &                     sdc_central_wire(j),
     &                     sdc_sigma(j),j=1,sdc_num_planes)
1000  format(1x,i4,f9.4,3f10.6,f8.4,i6,f10.4,f10.6)
        write(sluno,'(''  plane'',
     &        ''  szchi     szpsi     sxchi     sxpsi     sychi     sypsi'')')
        write(sluno,1001) (i, szchi(i),szpsi(i),sxchi(i),sxpsi(i),sychi(i),
     &               sypsi(i),i=1,sdc_num_planes )
1001  format(i5,6f10.6)
        write(sluno,'(''plane'',
     &        ''    spsi0       schi0       sphi0'')')
       write(sluno,1002) (i, spsi0(i),schi0(i),sphi0(i),i=1,sdc_num_planes)
1002  format(i5,3f12.6)
        write(sluno,'(''  plane'',
     &        ''     sstubcoef 1        2              3             4'')')
        write(sluno,1003) (i, sstubcoef(i,1),sstubcoef(i,2),sstubcoef(i,3),
     &         sstubcoef(i,4),i=1,sdc_num_planes)
1003  format(i5,4f15.6)
        write(sluno,'(''                 splane_coeff'')')
        write(sluno,'('' plane     1        2       3        4        5'',
     &   ''      6       7       8        9'')')
        do j=1,sdc_num_planes
          write(sluno,1004) j,(splane_coeff(i,j),i=1,9) 
        enddo                           ! end of print over planes loop
1004  format(1x,i3,f10.5,2f8.3,f9.3,4f8.3,f9.3)
*
      endif                               !   end if on debug print out
      return
      end
