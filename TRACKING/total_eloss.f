      subroutine total_eloss(arm,prt,z,a,tgthick,dens,angle,tgangle,beta,e_loss)
*-------------------------------------------------------------
*-         Prototype C routine
*- 
*-
*-    Purpose and Method :  Calulate energy loss for the beam before it 
*-                          reaches the target, energy loss in the hms and
*-                          energy loss in the sos
*-
*-    Output: loss            -   energy loss for the arm requested
*-    Created   1-Dec-1995  Rolf Ent
*
* $Log$
* Revision 1.3  1996/09/05 20:33:15  saw
* (JRA) Improvements?
*
* Revision 1.2  1996/01/24 16:31:35  saw
* (JRA) Cleanup
*
* Revision 1.1  1996/01/17 19:12:32  cdaq
* Initial revision
*
*------------------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      include 'gen_data_structures.cmn'
*

*
      INTEGER arm                       ! 0 : incident beam
                                        ! 1 : HMS
                                        ! 2 : SOS
      LOGICAL prt                       ! .true. : electron
                                        ! .false. : non-electron (beta .lt. 1)
      LOGICAL liquid

      REAL*4 degrad                     !convert degrees to radians
      REAL*4 z,a,tgthick,dens,angle,tgangle,beta
      REAL*4 thick,thick_side,thick_front,dens_comp,e_loss,total_loss

*
* incoming beam sees the following materials
*     a 3.0 mil Al-foil (upstream endcap of target) for a liquid target
*     half the target thickness
* HMS particle sees the following materials
*     For H2 or D2 target, a 2.5 inch diameter target is used:
*         1.25 inch of target material for H2 or D2, corrected for the
*             spectrometer angle,      OR      half the target length,
*             corrected for the spectrometer angle
*         a 5.0 mil Al-foil (target wall thickness), corrected for the
*                   spectrometer angle
*     For solid targets:
*         half the target thickness, corrected for the spectrometer angle
*     Additional materials:
*     a 16.0 mil Al-foil (scattering chamber exit foil)
*     a composite of 17 mil kevlar and 5 mil mylar (HMS entrance window)
* SOS particle sees the following materials
*     For H2 or D2 target, a 2.5 inch diameter target is used:
*         1.25 inch of target material for H2 or D2, corrected for the
*             spectrometer angle,      OR      half the target length,
*             corrected for the spectrometer angle
*         a 5.0 mil Al-foil (target wall thickness), corrected for the
*                   spectrometer angle,
*     For solid targets:
*         half the target thickness, corrected for the spectrometer angle
*     Additional materials:
*     a 8.0 mil Al-foil (scattering chamber exit foil)
*     a composite of 6 mil kevlar and 1.5 mil mylar (SOS entrance window)

      degrad = 3.14159/180.
       total_loss=0.0
       liquid =.FALSE.
      if(z.le.2.4) liquid =.TRUE.

      if(arm.eq.0) then
        if(liquid) then			!cryo target
          thick = 0.003*2.54*2.70
          call loss(.true.,13.,27.,thick/2.,2.70,beta,e_loss)
          total_loss = total_loss + e_loss
        endif
        thick = tgthick
        call loss(prt,z,a,thick,dens,beta,e_loss)
        total_loss = total_loss + e_loss 
      endif

      if (arm.eq.1) then                ! HMS
        if (liquid) then
          thick_front = 0.0
          if (angle.le.80.*degrad) thick_front =
     $         abs(tgthick/2./cos(angle))
          thick_side  = abs(1.25*2.54*dens/sin(angle))
          if (thick_side.ge.thick_front) then
            call loss(prt,z,a,thick_front,dens,beta,e_loss)
            total_loss = total_loss + e_loss
            thick = abs(0.005*2.54*2.70/cos(angle))
            call loss(prt,z,a,thick,2.70,beta,e_loss)
            total_loss = total_loss + e_loss
          else
            call loss(prt,z,a,thick_side,dens,beta,e_loss)
            total_loss = total_loss + e_loss
            thick = abs(0.005*2.54*2.70/sin(angle))
            call loss(prt,13.,27.,thick,2.70,beta,e_loss)
            total_loss = total_loss + e_loss
          endif
        endif
        if (.not.liquid) then
          thick = abs((tgthick/2.)/sin(3.14159-tgangle-angle))
          call loss(prt,z,a,thick,dens,beta,e_loss)
          total_loss = total_loss + e_loss
        endif
        thick = 0.016*2.54*2.70
        call loss(prt,13.,27.,thick,2.70,beta,e_loss)
        total_loss = total_loss + e_loss
* effective density for kevlar is 0.74
        thick = 0.005*2.54*1.35 + 0.017*2.54*0.74
        dens_comp = (5.0*1.35 + 17.0*0.74)/22.
* effective z for CH2 is 2.67, effective a is 4.67
        call loss(prt,2.67,4.67,thick,dens_comp,beta,e_loss)
        total_loss = total_loss + e_loss
      endif

      if (arm.eq.2) then                ! SOS
        if (liquid) then
          thick_front = 0.0
          if (angle.le.80.*degrad) thick_front=abs(tgthick/2./cos(angle))
          thick_side  = abs(1.25*2.54*dens/sin(angle))
          if (thick_side.ge.thick_front) then
            call loss(prt,z,a,thick_front,dens,beta,e_loss)
            total_loss = total_loss + e_loss
            thick = abs(0.005*2.54*2.70/cos(angle))
            call loss(prt,13.,27.,thick,2.70,beta,e_loss)
            total_loss = total_loss + e_loss
          else
            call loss(prt,z,a,thick_side,dens,beta,e_loss)
            total_loss = total_loss + e_loss
            thick = abs(0.005*2.54*2.70/sin(angle))
            call loss(prt,13.,27.,thick,2.70,beta,e_loss)
            total_loss = total_loss + e_loss
          endif
        endif
        if (.not.liquid) then
          thick = abs((tgthick/2.)/sin(tgangle-angle))
          call loss(prt,z,a,thick,dens,beta,e_loss) 
          total_loss = total_loss + e_loss
        endif
        thick = 0.008*2.54*2.70
        call loss(prt,13.,27.,thick,2.70,beta,e_loss)
        total_loss = total_loss + e_loss
* effective density for kevlar is 0.74
        thick = 0.0015*2.54*1.35 + 0.006*2.54*0.74
        dens_comp = (1.5*1.35 + 6.0*0.74)/7.5
* effective z for CH2 is 2.67, effective a is 4.67
        call loss(prt,2.67,4.67,thick,dens_comp,beta,e_loss)
        total_loss = total_loss + e_loss
      endif
*
      e_loss = total_loss
      RETURN
      END
*
      subroutine loss(electron,z,a,thick,dens,beta,e_loss)
*-------------------------------------------------------------
*-         Prototype C function 
*- 
*-
*-    Purpose and Method :  Calulate energy loss 
*-
*-    Output: -
*-    Created   1-Dec-1995  Rolf Ent
*-   
*------------------------------------------------------------------------------*
      IMPLICIT NONE
      SAVE
*
      include 'gen_data_structures.cmn'
*
      LOGICAL electron
      REAL*4 eloss,z,a,thick,dens,beta,e_loss
      REAL*4 icon_ev,me_ev
      parameter (me_ev = 510999.)
*

      e_loss = 0.0

* electron
      if (electron) then
        if(thick.gt.0.0.and.dens.gt.0.0)then
          eloss = 0.1536e-03*z/a*thick*(19.26 + log(thick/dens))
        endif
      endif
* proton
      if(.not.electron) then
        icon_ev = 16.*z**0.9
        if (z.lt.1.5) icon_ev = 21.8
        if(thick.gt.0.0.and.beta.gt.0.0.and.beta.lt.1.0)then
          eloss = log(2.*me_ev*beta*beta/icon_ev/(1.-beta*beta)) - beta*beta
          eloss = 2.*0.1536e-03*z/a*thick/beta/beta * eloss
        endif
      endif

* units should be in GeV
      e_loss = eloss
      RETURN
      END
