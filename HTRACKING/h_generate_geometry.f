      subroutine H_GENERATE_GEOMETRY
*
*     This subroutine reads in the wire plane parameters and fills all the
*     geometrical constants used in Track Fitting for the HMS spectrometer
*     The constants are stored in hms_geometry.cmn
*
*     d.f. geesaman           2 Sept 1993
*     modified                14 feb 1994 for CTP input.
*                             Change HPLANE_PARAM to individual arrays
* $Log$
* Revision 1.1  1994/02/19 06:14:45  cdaq
* Initial revision
*
*
      implicit none
      include "gen_data_structures.cmn"
      include "hms_tracking.cmn"
      include "hms_geometry.cmn"
*
*     local variables
      integer*4 iplane,i,j
      real*4 cosalpha,sinalpha,cosbeta,sinbeta,cosgamma,singamma,z0
      real*4 stubzchi,stubzpsi,stubxchi,stubxpsi,stubychi,stubypsi
      real*4 sumsqupsi,sumsquchi,sumcross,denom
*
*     read basic parameters from CTP input file
*     hdc_zpos(iplane) = Z0
*     hdc_alpha_angle(iplane) = ALPHA
*     hdc_beta_angle(iplane)  = BETA
*     hdc_gamma_angle(iplane) = GAMMA
*     hdc_pitch(iplane)       = Wire spacing
*     hdc_nrwire(iplane)      = Number of wires
*     hdc_central_wire(iplane) = Location of center of wire 1
*     hdc_sigma(iplane)       = sigma
*
*
*
*     loop over all planes

      do iplane=1,hdc_num_planes
        hdc_plane_num(iplane)=iplane
        z0=hdc_zpos(iplane)
        cosalpha = cos(hdc_alpha_angle(iplane))
        sinalpha = sin(hdc_alpha_angle(iplane))
        cosbeta  = cos(hdc_beta_angle(iplane))
        sinbeta  = sin(hdc_beta_angle(iplane))
        cosgamma = cos(hdc_gamma_angle(iplane))
        singamma = sin(hdc_gamma_angle(iplane))
*
        hsinbeta(iplane) = sinbeta
        hcosbeta(iplane) = cosbeta
*     make sure cosbeta is not zero
        if(abs(cosbeta).lt.1e-10) then
          write(hluno,'('' unphysical beta rotation in hms plane'',i4,
     &      ''    beta='',f10.5)') iplane,hdc_beta_angle(iplane)
        endif
        htanbeta(iplane) = sinbeta / cosbeta
*
*     compute chi,psi to x,y,z transformation coefficient
        hzchi(iplane) = -cosalpha*sinbeta + sinalpha*cosbeta*singamma
        hzpsi(iplane) =  sinalpha*sinbeta + cosalpha*cosbeta*singamma
        hxchi(iplane) = -cosalpha*cosbeta - sinalpha*sinbeta*singamma
        hxpsi(iplane) =  sinalpha*cosbeta - cosalpha*sinbeta*singamma
        hychi(iplane) =  sinalpha*cosgamma
        hypsi(iplane) =  cosalpha*cosgamma
*
*     stub transfromations are done in beta=gamma=0 system
        stubxchi = -cosalpha
        stubxpsi =  sinalpha
        stubychi =  sinalpha
        stubypsi =  cosalpha
*
*
*     fill hpsi0,hchi0,hz0  used in stub fit
*
        sumsqupsi = hzpsi(iplane)**2 + hxpsi(iplane)**2 + hypsi(iplane)**2
        sumsquchi = hzchi(iplane)**2 + hxchi(iplane)**2 + hychi(iplane)**2
        sumcross =   hzpsi(iplane)*hzchi(iplane) + hxpsi(iplane)*hxchi(iplane)
     &             + hypsi(iplane)*hychi(iplane)
        denom = sumsqupsi*sumsquchi-sumcross**2 
        hpsi0(iplane) = (-z0*hzpsi(iplane)*sumsquchi
     &                   +z0*hzchi(iplane)*sumcross) / denom
        hchi0(iplane) = (-z0*hzchi(iplane)*sumsqupsi
     &                   +z0*hzpsi(iplane)*sumcross) / denom
*     calculate magnitude of hphi0
        hphi0(iplane) = sqrt(
     &         (z0+hzpsi(iplane)*hpsi0(iplane)+hzchi(iplane)*hchi0(iplane))**2
     &          + (hxpsi(iplane)*hpsi0(iplane)+hxchi(iplane)*hchi0(iplane))**2
     &          + (hypsi(iplane)*hpsi0(iplane)+hychi(iplane)*hchi0(iplane))**2 )
        if(z0.lt.0) hphi0(iplane)=-hphi0(iplane)        
*
*     hstubcoef used in stub fits
*     check these   I don't think they are correct
        denom = stubxpsi*stubychi 
     &          - stubxchi*stubypsi
       hstubcoef(iplane,1)= stubychi/(hdc_sigma(iplane)*denom)
       hstubcoef(iplane,2)= -stubxchi/(hdc_sigma(iplane)*denom)
       hstubcoef(iplane,3)= hphi0(iplane)*hstubcoef(iplane,1)
       hstubcoef(iplane,4)= hphi0(iplane)*hstubcoef(iplane,2)
*
*     xsp and ysp used in space point pattern recognition
*
        hxsp(iplane) = hychi(iplane) / denom
        hysp(iplane) = -hxchi(iplane) / denom
*
*     compute track fitting coefficients
*
        hplane_coeff(1,iplane) =  hzchi(iplane)
        hplane_coeff(2,iplane) = -hzchi(iplane)
        hplane_coeff(3,iplane) =  hychi(iplane)*
     &                            (hdc_zpos(iplane)-hlocrayzt)
        hplane_coeff(4,iplane) =  hxchi(iplane)*
     &                            (hlocrayzt-hdc_zpos(iplane))
        hplane_coeff(5,iplane) =  hychi(iplane)
        hplane_coeff(6,iplane) = -hxchi(iplane)
        hplane_coeff(7,iplane) =  hzchi(iplane)*hypsi(iplane)
     &                          - hychi(iplane)*hzpsi(iplane)
        hplane_coeff(8,iplane) = -hzchi(iplane)*hxpsi(iplane)
     &                          + hxchi(iplane)*hzpsi(iplane)
        hplane_coeff(9,iplane) =  hychi(iplane)*hxpsi(iplane)
     &                          - hxchi(iplane)*hypsi(iplane)
*
      enddo                  !  end loop over all planes
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
1000  format(1x,i4,f9.4,3f10.6,f8.4,f6.0,f10.4,f10.6)
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


      
