      subroutine S_GENERATE_GEOMETRY
*
*     This subroutine reads in the wire plane parameters and fills all the
*     geometrical constants used in Track Fitting for the SOS spectrometer
*     The constants are stored in sos_geometry.cmn
*
*     d.f. geesaman           2 Sept 1993
*     modified                14 feb 1994 for CTP input.
*                             Change SPLANE_PARAM to individual arrays
* $Log$
* Revision 1.3  1995/04/01 20:42:06  cdaq
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
      include "gen_data_structures.cmn"
      include "sos_tracking.cmn"
      include "sos_geometry.cmn"
*
*     local variables
      integer*4 iplane,i,j,k,pindex,ich
      real*4 cosalpha,sinalpha,cosbeta,sinbeta,cosgamma,singamma,z0
      real*4 stubxchi,stubxpsi,stubychi,stubypsi
      real*4 sumsqupsi,sumsquchi,sumcross,denom
*
*     read basic parameters from CTP input file
*     sdc_zpos(iplane) = Z0
*     sdc_alpha_angle(iplane) = ALPHA
*     sdc_beta_angle(iplane)  = BETA
*     sdc_gamma_angle(iplane) = GAMMA
*     sdc_pitch(iplane)       = Wire spacing
*     sdc_nrwire(iplane)      = Number of wires
*     sdc_central_wire(iplane) = Location of center of wire 1
*     sdc_sigma(iplane)       = sigma
*
      sdc_planes_per_chamber = sdc_num_planes / sdc_num_chambers
*
*     loop over all planes
*
      do iplane=1,sdc_num_planes
        sdc_plane_num(iplane)=iplane
        z0=sdc_zpos(iplane)
        cosalpha = cos(sdc_alpha_angle(iplane))
        sinalpha = sin(sdc_alpha_angle(iplane))
        cosbeta  = cos(sdc_beta_angle(iplane))
        sinbeta  = sin(sdc_beta_angle(iplane))
        cosgamma = cos(sdc_gamma_angle(iplane))
        singamma = sin(sdc_gamma_angle(iplane))
*
        ssinbeta(iplane) = sinbeta
        scosbeta(iplane) = cosbeta
*     make sure cosbeta is not zero
        if(abs(cosbeta).lt.1e-10) then
          write(sluno,'('' unphysical beta rotation in sos plane'',i4,
     &      ''    beta='',f10.5)') iplane,sdc_beta_angle(iplane)
        endif
        stanbeta(iplane) = sinbeta / cosbeta
*
*     compute chi,psi to x,y,z transformation coefficient
        szchi(iplane) = -cosalpha*sinbeta + sinalpha*cosbeta*singamma
        szpsi(iplane) =  sinalpha*sinbeta + cosalpha*cosbeta*singamma
        sxchi(iplane) = -cosalpha*cosbeta - sinalpha*sinbeta*singamma
        sxpsi(iplane) =  sinalpha*cosbeta - cosalpha*sinbeta*singamma
        sychi(iplane) =  sinalpha*cosgamma
        sypsi(iplane) =  cosalpha*cosgamma
*
*     stub transfromations are done in beta=gamma=0 system
        stubxchi = -cosalpha
        stubxpsi =  sinalpha
        stubychi =  sinalpha
        stubypsi =  cosalpha
*
*
*     fill spsi0,schi0,sz0  used in stub fit
*
        sumsqupsi = szpsi(iplane)**2 + sxpsi(iplane)**2 + sypsi(iplane)**2
        sumsquchi = szchi(iplane)**2 + sxchi(iplane)**2 + sychi(iplane)**2
        sumcross =   szpsi(iplane)*szchi(iplane) + sxpsi(iplane)*sxchi(iplane)
     &             + sypsi(iplane)*sychi(iplane)
        denom = sumsqupsi*sumsquchi-sumcross**2 
        spsi0(iplane) = (-z0*szpsi(iplane)*sumsquchi
     &                   +z0*szchi(iplane)*sumcross) / denom
        schi0(iplane) = (-z0*szchi(iplane)*sumsqupsi
     &                   +z0*szpsi(iplane)*sumcross) / denom
*     calculate magnitude of sphi0
        sphi0(iplane) = sqrt(
     &         (z0+szpsi(iplane)*spsi0(iplane)+szchi(iplane)*schi0(iplane))**2
     &          + (sxpsi(iplane)*spsi0(iplane)+sxchi(iplane)*schi0(iplane))**2
     &          + (sypsi(iplane)*spsi0(iplane)+sychi(iplane)*schi0(iplane))**2 )
        if(z0.lt.0) sphi0(iplane)=-sphi0(iplane)        
*
*     sstubcoef used in stub fits
*     check these   I don't think they are correct
        denom = stubxpsi*stubychi 
     &          - stubxchi*stubypsi
        sstubcoef(iplane,1)= stubychi/(sdc_sigma(iplane)*denom)
        sstubcoef(iplane,2)= -stubxchi/(sdc_sigma(iplane)*denom)
        sstubcoef(iplane,3)= sphi0(iplane)*sstubcoef(iplane,1)
        sstubcoef(iplane,4)= sphi0(iplane)*sstubcoef(iplane,2)
*
*     xsp and ysp used in space point pattern recognition
*
        sxsp(iplane) = sychi(iplane) / denom
        sysp(iplane) = -sxchi(iplane) / denom
*
*     compute track fitting coefficients
*
        splane_coeff(1,iplane) =  szchi(iplane)
        splane_coeff(2,iplane) = -szchi(iplane)
        splane_coeff(3,iplane) =  sychi(iplane)*
     &                            (sdc_zpos(iplane)-slocrayzt)
        splane_coeff(4,iplane) =  sxchi(iplane)*
     &                            (slocrayzt-sdc_zpos(iplane))
        splane_coeff(5,iplane) =  sychi(iplane)
        splane_coeff(6,iplane) = -sxchi(iplane)
        splane_coeff(7,iplane) =  szchi(iplane)*sypsi(iplane)
     &                          - sychi(iplane)*szpsi(iplane)
        splane_coeff(8,iplane) = -szchi(iplane)*sxpsi(iplane)
     &                          + sxchi(iplane)*szpsi(iplane)
        splane_coeff(9,iplane) =  sychi(iplane)*sxpsi(iplane)
     &                          - sxchi(iplane)*sypsi(iplane)
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
*     planes and SDC_NUM_PLANES is SDC_NUM_CHAMBERS * # of planes/chamber
*
      do pindex=1,SDC_NUM_PLANES+SDC_NUM_CHAMBERS

* generate the matrix SAA3 for an sdc missing a particular plane
        do i=1,3
          do j=1,3
            SAA3(i,j)=0.
            if(j.lt.i)then              ! SAA3 is symmetric so only calculate 6 terms
              SAA3(i,j)=SAA3(j,i)
            else
              if(pindex.le.SDC_NUM_PLANES) then
                ich = (pindex-1)/(SDC_PLANES_PER_CHAMBER)+1
                do k=(ich-1)*(SDC_PLANES_PER_CHAMBER)+1
     $               ,ich*(SDC_PLANES_PER_CHAMBER)
                  if(pindex.ne.k) then
                    SAA3(i,j)=SAA3(i,j) + sstubcoef(k,i)*sstubcoef(k,j)
                  endif
                enddo
              else
                ich = pindex - SDC_NUM_PLANES
                do k=(ich-1)*(SDC_PLANES_PER_CHAMBER)+1
     $               ,ich*(SDC_PLANES_PER_CHAMBER)
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
1000  format(1x,i4,f9.4,3f10.6,f8.4,f6.0,f10.4,f10.6)
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
