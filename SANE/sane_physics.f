      subroutine Bigcal_Betta(inum)
      IMPLICIT NONE
      include 'bigcal_data_structures.cmn'
      include 'sane_data_structures.cmn'
      include 'b_ntuple.cmn'
      include 'sane_ntuple.cmn'
      include 'gen_data_structures.cmn'
      integer inum
      real*4 Vector(3),Vector_r(3)
      INTEGER jj
      real*4 en,add_factor
      real*4 ebj(25), coorX, coorY,E(10),esum(10)
      real*8 Eb,theta_big, phi_big!,ccx,ccy,ccz
      common/FAKEBIG/Eb,theta_big, phi_big
      esum(inum) = 0
      E(inum) =0
      do jj=1,ncellclust(inum)
         if(BigCal_Calib_Gain(iycell(jj,inum),ixcell(jj,inum)).eq.0)
     ,        BigCal_Calib_Gain(iycell(jj,inum),ixcell(jj,inum))=1. 

         en =((eblock(jj,inum)))/
     ,        BigCal_Calib_Gain(iycell(jj,inum),ixcell(jj,inum))
         ebj(jj)=en
         E(inum)=E(inum)+ebj(jj)
         esum(inum)=sqrt(ebj(jj))+esum(inum)
      enddo
      coorX =0
      coory =0
      add_factor = 1!-Eb/1000./e(inum)
      E(inum) =0
      do jj=1,ncellclust(inum)
c         write(34,*)ixcell(jj,inum),iycell(jj,inum),add_factor
         if(iycell(jj,inum).le.55.and.ixcell(jj,inum).le.32)then
            if(iycell(jj,inum).gt.0.and.ixcell(jj,inum).gt.0)then
               E(inum)=E(inum)+ebj(jj)*add_factor
               coorX = coorX +
     ,              xcell(jj,inum)*sqrt(ebj(jj))/esum(inum)
               coorY = coorY +
     ,              ycell(jj,inum)*sqrt(ebj(jj))/esum(inum)
               
            endif
         endif
      enddo
      
      X_clust(inum) = coorX
      Y_clust(inum) = coorY
      Z_clust(inum) = Bigcal_SHIFT(3)
      E_clust(inum) = E(inum)
      Vector(1)         = X_clust(inum)
      Vector(2)         = Y_clust(inum)
      Vector(3)         = Z_clust(inum)
      call ROTATE(Vector,0.,-Bigcal_SHIFT(4)*3.1415926536/180.,0.,Vector_r)

      X_clust_r(inum) =  Vector_r(1)
      Y_clust_r(inum) =  Vector_r(2)
      Z_clust_r(inum) =  Vector_r(3)
cc
cc     OBTAIN Angles THeta and Phi Assuming the particle was Electron
c     Angles are in degree
c      write(*,*)E_clust(inum),X_clust(inum),Y_clust(inum),Z_clust(inum) 
      call CORRECT_ANGLES(
     ,     X_clust_r(inum)-slow_rast_x,
     ,     Y_clust_r(inum)-slow_rast_y-Bigcal_SHIFT(2),
     ,     Z_clust_r(inum),E_clust(inum)*1000,
     ,     SANE_IF_ELECTRON_ANGLE_THETA,
     ,     SANE_IF_ELECTRON_ANGLE_PHI)
      Theta_e(inum) = SANE_IF_ELECTRON_ANGLE_THETA
      Phi_e(inum) = SANE_IF_ELECTRON_ANGLE_PHI
      call PHYSICS_VARIABLES(inum,SANE_IF_ELECTRON_ANGLE_THETA,SANE_IF_ELECTRON_ANGLE_PHI)
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c      real function ener(e)
c      real e
c      ener=e
c      end
c
c
c     Tracker
c
c

      subroutine tracker(inum) ! inum -cluster number ! 
      IMPLICIT NONE
      include 'bigcal_data_structures.cmn'
      include 'sane_data_structures.cmn'
      include 'b_ntuple.cmn'
      include 'sane_ntuple.cmn'
      integer x1t_stat,y1t_stat,y2t_stat
      integer i,inum
      x1t_stat = 0
      y1t_stat = 0
      y2t_stat = 0
      trc_hx(inum) =0
ccccc
c
c     Geometrical matching
c
ccccc
      do i=1,x1t_hit
c
c     Very wide geometrical cut
c
         IF(abs(x1t_x(i)-TrackerX_SHIFT(1)-
     ,        TrackerX_SHIFT(3)/Bigcal_SHIFT(3)*
     ,        (xclust(inum))-Bigcal_SHIFT(1)).lt.0.6)then
c
c     TDC CUT
c
            if(abs(x1t_tdc(i)-TRACKER_SANE_XCALIBRATION(x1t_row(i))).lt.
     ,           TRACKER_SANE_XSIGMA(x1t_row(i)))then
               x1t_stat = x1t_stat+1
               trc_hx(inum) = x1t_stat
               if(x1t_stat.gt.0)then
c                 
                  X_trc(trc_hx(inum),inum)  = x1t_x(i)-TrackerX_SHIFT(1)
                  Z_trc(trc_hx(inum),inum)  = TrackerX_SHIFT(3)

               endif
            endif
         ENDIF
      enddo
      trc_hy1(inum) =0
      do i=1,y1t_hit
c
c     Very wide geometrical cut
c
         IF(abs(y1t_y(i)-TrackerY1_SHIFT(2)-
     ,        TrackerY1_SHIFT(3)/Bigcal_SHIFT(3)*
     ,        (yclust(inum))-Bigcal_SHIFT(2)).lt.0.6)then
c
c     TDC CUT
c
            if(abs(y1t_tdc(i)-TRACKER_SANE_Y1CALIBRATION(y1t_row(i))).lt.
     ,           TRACKER_SANE_Y1SIGMA(y1t_row(i)))then
               y1t_stat = y1t_stat +1
               trc_hy1(inum) = y1t_stat
              if(y1t_stat.gt.0)then

                  Y1_trc(trc_hy1(inum),inum)  = y1t_y(i)-TrackerY1_SHIFT(2)
                  Z1_trc(trc_hy1(inum),inum)  = TrackerY1_SHIFT(3)
               endif
            endif
         ENDIF
      enddo
      trc_hy2(inum) =0
      do i=1,y2t_hit
c
c     Very wide geometrical cut
c
         IF(abs(y2t_y(i)-TrackerY2_SHIFT(2)-
     ,        TrackerY2_SHIFT(3)/Bigcal_SHIFT(3)*
     ,        (yclust(inum))-Bigcal_SHIFT(2)).lt.0.6)then
c
c     TDC CUT
c
            if(abs(y2t_tdc(i)-TRACKER_SANE_Y2CALIBRATION(y2t_row(i))).lt.
     ,           TRACKER_SANE_Y1SIGMA(y1t_row(i)))then

               y2t_stat = y2t_stat +1
               trc_hy2(inum) = y2t_stat
              if(y2t_stat.gt.0)then

                  Y2_trc(trc_hy2(inum),inum)  = y2t_y(i)-TrackerY2_SHIFT(2)
                  Z2_trc(trc_hy2(inum),inum)  = TrackerY2_SHIFT(3)
               endif

            endif

         ENDIF
      enddo


      end

cccccccccccccccccccccccccccccc
c
c     Lucite 
c
ccccccccccccccccccccccccccccccccccccc

      subroutine lucite(inum)
      IMPLICIT NONE
      include 'bigcal_data_structures.cmn'
      include 'sane_data_structures.cmn'
      include 'b_ntuple.cmn'
      include 'sane_ntuple.cmn'

      integer inum,i
      integer luc
      real*4 tdc_dif, xDelta1,fluc
      real*4 Vector(3),Vector_r(3)
      luc=0
      
      if(luc_hit.gt.0)then
         do i=1,luc_hit
c     write(*,*)ltdc_pos(i),LUCITE_SANE_MEAN_POS(luc_row(i)),
c     ,           LUCITE_SANE_SIGMA_POS(luc_row(i))
c     write(*,*)'POS ',ltdc_pos(i),LUCITE_SANE_MEAN_POS(luc_row(i)),
c     ,           LUCITE_SANE_SIGMA_POS(luc_row(i))
c     write(*,*)'NEG ', ltdc_neg(i),LUCITE_SANE_MEAN_NEG(luc_row(i)),
c     ,           LUCITE_SANE_SIGMA_NEG(luc_row(i))
            If(abs(ltdc_pos(i)-LUCITE_SANE_MEAN_POS(luc_row(i))).lt.
     ,           4*LUCITE_SANE_SIGMA_POS(luc_row(i)).and.
     ,           abs(ltdc_neg(i)-LUCITE_SANE_MEAN_NEG(luc_row(i))).lt.
     ,           4*LUCITE_SANE_SIGMA_NEG(luc_row(i))
     ,           )then
               
               tdc_dif=float(ltdc_pos(i))-
     ,              float(ltdc_neg(i))
     ,              +LUCITE_SANE_MEAN_NEG(luc_row(i))
     ,              -LUCITE_SANE_MEAN_POS(luc_row(i))
c     
c     X coordinate for Lucite
c     
               
               xDelta1    = 
     ,              (tdc_dif)*LUCITE_SANE_TDC_TIMING(luc_row(i))*
     ,              29.979/1.49*0.7313
               
C     
C     Y Geometrical CUT
C     
**********
               
               IF(abs(luc_y(i)-Lucite_SHIFT(2)-
     ,              Lucite_SHIFT(3)/Bigcal_SHIFT(3)*
     ,              (yclust(inum))-Bigcal_SHIFT(2)).lt.6)then
                  call HFILL(10128,luc_y(i),Y_clust(inum),1.)
c                  if(luc_row(i).eq.10)then
                  call HFILL(10150+luc_row(i),xDelta1,
     ,                 sqrt(Lucite_SHIFT(3)**2+luc_y(i)**2)/sqrt(Bigcal_SHIFT(3)**2+
     ,                 Y_clust(inum)**2)*X_clust(inum),1.)
c                   endif
                  call HFILL(10131, float(luc_row(i)), float(ltdc_pos(i)), 1.)
                  call HFILL(10132, float(luc_row(i)), float(ltdc_neg(i)), 1.)
                  call HFILL(10135, float(luc_row(i)), float(ladc_pos(i)), 1.)
                  call HFILL(10136, float(luc_row(i)), float(ladc_neg(i)), 1.)
               endif
C     
C     X Geometrical CUT
C     
***********
c     If(abs(xDelta1-Lucite_SHIFT(1)-
c     ,                    Lucite_SHIFT(3)/Bigcal_SHIFT(3)*
c     ,                    (xclust(inum))-Bigcal_SHIFT(1)).lt.10)then
               
               luc = luc+1
               X_luc(luc,inum)   = xDelta1
               Y_luc(luc,inum)   = luc_y(i)+3/1.33
               Z_luc(luc,inum)   = sqrt(Lucite_SHIFT(3)**2-
     ,              X_luc(luc,inum)**2)
               Vector(1)         = X_luc(luc,inum)
               Vector(2)         = Y_luc(luc,inum)
               Vector(3)         = Z_luc(luc,inum)
               call ROTATE(Vector,0.,-BIGCAL_SHIFT(4)*3.1415926536/180.,0.,Vector_r)
               X_luc_r(luc,inum)   = Vector_r(1)
               Y_luc_r(luc,inum)   = Vector_r(2)
               Z_luc_r(luc,inum)   = Vector_r(3)
               
c            Endif
                     
c                  ENDIF
               endif
               
         enddo
         luc_h(inum) = luc
         X_luc_av(inum) = 0
         Y_luc_av(inum) = 0
         Z_luc_av(inum) = 0
c     do i=1,luc
c     fluc           = luc
c     X_luc_av(inum) = X_luc_av(inum) + X_luc(i,inum)/fluc
c     Y_luc_av(inum) = Y_luc_av(inum) + Y_luc(i,inum)/fluc
c     Z_luc_av(inum) = Z_luc_av(inum) + Z_luc(i,inum)/fluc
c     enddo
      endif
      end

ccccccccccccccccccccccccccccccccccccccccccc
c
c     Cerencov
c
ccccccccccc
      Subroutine  icer(inum)
      IMPLICIT NONE
      include 'bigcal_data_structures.cmn'
      include 'sane_data_structures.cmn'
      include 'b_ntuple.cmn'
      include 'sane_ntuple.cmn'
      integer inum,i
      cer_h(inum)=0
      
      do i=1, cer_hit
         if(cer_tdc(i).gt.0)then
            if(
     ,           abs(cer_tdc(i)-CER_SANE_MEAN(cer_num(i))).lt.
     ,           CER_SANE_SIGMA(cer_num(i))
     ,           )then
               cer_h(inum)=1
            endif
         endif
      enddo
      
      end
c
c
c     Particle ID
c
cccccccccccccccccccccccccccccccccccccccccc
      subroutine TrackerCoordnate(inum)
      IMPLICIT NONE
      include 'sane_data_structures.cmn'
      include 'sane_ntuple.cmn'
      real*4 TrakerVertex(3), TrakerVertex_r(3)
      real*4 TrakerTEMP_Y1,TrakerTEMP_Y2 
      integer i,inum
      logical TRAKER_STATUS
      common/TRACKER_LOGICS/TRAKER_STATUS
      TRAKER_STATUS=.FALSE.

c
c     Average coordinate on X plane
      Tr_Vertex(1,inum)        = 0
      Tr_Vertex(2,inum)        = 0
      Tr_Vertex(3,inum)        = 0
      
      Tr_Vertex_r(1,inum)        = 0
      Tr_Vertex_r(2,inum)        = 0
      Tr_Vertex_r(3,inum)        = 0
      TrakerVertex(3) = TrackerY2_SHIFT(3)
      if(trc_hx(inum).gt.0.or.(trc_hy1(inum).gt.0.or.trc_hy2(inum).gt.0))then
      TRAKER_STATUS=.TRUE.

         TrakerVertex(1)       = 0
         if(trc_hx(inum).eq.1)then
            TrakerVertex(1)    = X_trc(trc_hx(inum),inum)
         else if(trc_hx(inum).gt.1)then 
            do i=1, trc_hx(inum)
               TrakerVertex(1) = TrakerVertex(1) + X_trc(trc_hx(inum),inum)/trc_hx(inum) 
            enddo
         endif
         TrakerVertex(2)       = 0
c     
c     Average coordinate on Y1 plane
         
         TrakerTEMP_Y1         = 0
         if(trc_hy1(inum).eq.1)then
            TrakerTEMP_Y1      = Y1_trc(trc_hy1(inum),inum)
         else if(trc_hy1(inum).gt.1)then 
            do i=1, trc_hy1(inum)
               TrakerTEMP_Y1   = TrakerTEMP_Y1 + Y1_trc(trc_hy1(inum),inum)/trc_hy1(inum) 
            enddo
         endif
c     
c     Average coordinate on Y2 plane
         
         TrakerTEMP_Y2         = 0
         if(trc_hy2(inum).eq.1)then
            TrakerTEMP_Y2      = Y2_trc(trc_hy2(inum),inum)
         else if(trc_hy2(inum).gt.1)then 
            do i=1, trc_hy2(inum)
               TrakerTEMP_Y2   = TrakerTEMP_Y2 + Y2_trc(trc_hy2(inum),inum)/trc_hy2(inum) 
            enddo
         endif
         if(TrakerTEMP_Y1.gt.0.and.TrakerTEMP_Y2.gt.0)then
            TrakerVertex(2)       = (TrakerTEMP_Y1+TrakerTEMP_Y2)/2.
            TrakerVertex(3)       = (Z2_trc(trc_hy2(inum),inum)+Z1_trc(trc_hy1(inum),inum))/2.
         else if(TrakerTEMP_Y1.gt.0.and.TrakerTEMP_Y2.eq.0)then
            TrakerVertex(2)       = TrakerTEMP_Y1
            TrakerVertex(3)       = Z1_trc(trc_hy1(inum),inum)
         else if(TrakerTEMP_Y1.eq.0.and.TrakerTEMP_Y2.gt.0)then
            TrakerVertex(2)       = TrakerTEMP_Y2
            TrakerVertex(3)       = Z2_trc(trc_hy2(inum),inum)
         endif
         if(Z_trc(trc_hx(inum),inum).ne.0)then
            TrakerVertex(1)          = TrakerVertex(1)*TrakerVertex(3)/Z_trc(trc_hx(inum),inum) 
         endif
         call ROTATE(TrakerVertex,0.,-Bigcal_SHIFT(4)*3.1415926536/180.,0.,TrakerVertex_r)
C         WRITE(*,*)TrakerVertex,TrakerVertex_r
         Tr_Vertex(1,inum)        = TrakerVertex(1)
         Tr_Vertex(2,inum)        = TrakerVertex(2)
         Tr_Vertex(3,inum)        = TrakerVertex(3)
         
         Tr_Vertex_r(1,inum)        = TrakerVertex_r(1)
         Tr_Vertex_r(2,inum)        = TrakerVertex_r(2)
         Tr_Vertex_r(3,inum)        = TrakerVertex_r(3)
      endif
      end
c
c     Real Angles
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccc
      Subroutine CORRECT_ANGLES(X,Y,Z,EE,TH,PHI)
      IMPLICIT NONE
c
c     X = X(Bigcal)-X(raster)
c     Y = Y(Bigcal)-Y(raster)
c     Z = Z(Bigcal)
c     EE - energy in GEV
c     RETURNS THeta and PHI In Degree.
c
cccccccccccc      
      include 'sane_data_structures.cmn'
      real*4 omega,X,Y,Z,TH,Phi,thr,phr,EE 
      real*4 DIST
      DIST = sqrt(X**2+Y**2+Z**2)
      thr  = acos(Z/Dist)
      phr= atan2(y/Dist,x/Dist)
      call POLYNOM_CORRECTION(SANE_TRANSFORM_MATRIX_THETA, 
     ,     SANE_TRANSFORM_MATRIX_PHI,thr,
     ,     phr,EE,TH,PHI,SANE_BETA_OMEGA)
      end
ccccccc
      Subroutine POLYNOM_CORRECTION(P_th,P_phi,thr,phr,EE,TH,PHI,omega)
      IMPLICIT NONE
      
c
c     Input patameters are P(26) -transformation Matrix
c     input thr and phr angles from CALORIMETER ,THr and PHr in radians
c     EE Energy in GEV
c     Output :TH and Phi Correctes in degrees
cc      
      real*4 X,Y,TH,Phi,thr,phr,EE
      REAL*8 OMEGA
      real*4 P_th(14),P_phi(39),COSOM,SINOM
      cosom = cos(omega*3.1415926/180.)
      sinom = sin(omega*3.1415926/180.)

         th  = THR*180/3.1415926+
     ,      ((P_th(1)+P_th(2)*phr+P_th(3)*thr+P_th(4)*phr**2+P_th(5)*thr**2)/EE)*
     ,        (P_th(6)*cosom+P_th(7)*sinom)+
     ,      ((P_th(8)+P_th(9)*thr+P_th(10)*phr+
     ,        P_th(11)*phr**2+P_th(12)*thr**2)/EE**2)*
     ,       (P_th(13)*cosom+P_th(14)*sinom)



         phi = phR*180/3.1415926
     ,        +(P_phi(1)*cosom+P_phi(2)*phr*cosom+P_phi(3)*phr**2*cosom+
     ,        P_phi(4)*phr**3*cosom+P_phi(5)*thr*cosom+P_phi(6)*thr**2*cosom+
     ,        P_phi(7)*thr**3*cosom+P_phi(8)*phr*thr*cosom+
     ,        P_phi(9)*phr**2*thr*cosom+
     ,        P_phi(10)*phr*thr**2*cosom)+
     ,        ( P_phi(11)*cosom+P_phi(12)*phr*cosom+P_phi(13)*phr**2*cosom+
     ,        P_phi(14)*thr*cosom+P_phi(15)*thr**2*cosom+
     ,        P_phi(16)*phr*thr*cosom)/EE
     ,        +(P_phi(17)*sinom+P_phi(18)*phr*sinom+P_phi(19)*phr**2*sinom+
     ,        P_phi(20)*phr**3*sinom+P_phi(21)*thr*sinom+P_phi(22)*thr**2*sinom+
     ,        P_phi(23)*thr**3*sinom+P_phi(24)*phr*thr*sinom+
     ,        P_phi(25)*phr**2*thr*sinom+
     ,        P_phi(26)*phr*thr**2*sinom)+
     ,        ( P_phi(27)*sinom+P_phi(28)*phr*sinom+
     ,        P_phi(29)*phr**2*sinom+
     ,        P_phi(30)*thr*sinom+P_phi(31)*thr**2*sinom+
     ,        P_phi(32)*phr*thr*sinom)/EE+
     ,         P_phi(33)*sinom**2+P_phi(34)*sinom**3+P_phi(35)*sinom**4+
     ,        P_phi(36)*sinom**5+P_phi(37)*sinom**6+P_phi(38)*sinom**7

      end

c
c     Geometry match of the tracks
c
ccccccccccccccccccccccccccccccccccccc
      Subroutine GeometryMatch(inum)
      IMPLICIT NONE
      include 'bigcal_data_structures.cmn'
      include 'sane_data_structures.cmn'
      include 'b_ntuple.cmn'
      include 'sane_ntuple.cmn'
      integer i,j,inum
      real*8 U, U_pos, U_neg
      real*8 dl
      real*4 P_tr(3),P_big(3),P_tar(3)
      real*4 Vector(3),Vector_r(3)
      LOGICAL ok1
      logical TRAKER_STATUS
      data P1_track /0.,0.,66./
      data P2_track /0.,1.,66./
      data P3_track /1.,0.,66./
      common/TRACKER_LOGICS/TRAKER_STATUS
      if(TRAKER_STATUS)then
         P1_track(3) = Tr_Vertex(3,inum)
         P2_track(3) = Tr_Vertex(3,inum)
         P3_track(3) = Tr_Vertex(3,inum)
         dl =0.1
C         if(a_tracker.eq.0.and.b_tracker.eq.0.and.c_tracker.eq.0)then
c     c
c     Define Traker plane
c     
            call ROTATE(P1_track, 0., -Bigcal_SHIFT(4)*3.1415926536/180., 0.,P1_track_r)
            call ROTATE(P2_track, 0., -Bigcal_SHIFT(4)*3.1415926536/180., 0.,P2_track_r)
            call ROTATE(P3_track, 0., -Bigcal_SHIFT(4)*3.1415926536/180., 0.,P3_track_r)
            call Plane(P1_track_r,P2_track_r,P3_track_r,
     ,           a_tracker,b_tracker,c_tracker,d_tracker)
            
C         endif
C         if(a_bigcal.eq.0.and.b_bigcal.eq.0.and.c_bigcal.eq.0)then
c     c
c     Define Bigcal plane
c     
c     call ROTATE(P1_bigcal, 0., Bigcal_SHIFT(4)*3.141/180., 0. ,P1_bigcal_r)
c     call ROTATE(P2_bigcal, 0., Bigcal_SHIFT(4)*3.141/180., 0. ,P2_bigcal_r)
c     call ROTATE(P3_bigcal, 0., Bigcal_SHIFT(4)*3.141/180., 0. ,P3_bigcal_r)
            call Plane(P1_track,P2_track,P3_track,
     ,           a_bigcal,b_bigcal,c_bigcal,d_bigcal)
            
C         endif
c
c
c     Start Particle Identification
c
c

         Delta_Y(inum) =-100
         Delta_X(inum) =-100
         if(cer_h(inum).ge.0)then
c     
c     It's most probably electron or positron 
c     Next step is calculate distance between tracker position and 
c     Virtual position reconstructed by linear track to target.
c     
            P_tar(1) = slow_rast_x
            P_tar(2) = slow_rast_y
            P_tar(3) = 0
            P_tar(1) = 0
            P_tar(2) = 0
            P_tar(3) = 0
            
c            P_big(1) = X_clust_r(inum)
c            P_big(2) = Y_clust_r(inum)
c            P_big(3) = Z_clust_r(inum)
c            call PlaneLineIntersection(a_tracker,b_tracker,c_tracker,
c     ,           d_tracker,P_tar,P_big,P_tr)
c            Delta_Y(inum) = Tr_Vertex_r(2,inum)-P_tr(2)
c            Delta_X(inum) = Tr_Vertex_r(1,inum)-P_tr(1)

c            write(*,*) 'TRACK MATCH 1',Delta_X(inum)
            P_tar(1) = 0
            P_tar(2) = 0
            P_tar(3) = 0
            P_big(1) = X_clust(inum)
            P_big(2) = Y_clust(inum)
            P_big(3) = Z_clust(inum)
            call PlaneLineIntersection(a_bigcal,b_bigcal,c_bigcal,
     ,           d_bigcal,P_tar,P_big,P_tr)
            Delta_Y(inum) = Tr_Vertex(2,inum)-P_tr(2)
            Delta_X(inum) = Tr_Vertex(1,inum)-P_tr(1)
            call HFILL(10550,E_clust(inum),Delta_Y(inum), 1.)
            call HFILL(10551,E_clust(inum),Delta_X(inum), 1.)
c            write(*,*) 'TRACK MATCH 2',Delta_X(inum)
c            write(*,*) 'TRACK MATCH 3'
           
c     c
cc     The particle is charged. 
c     c
c     if(luc_h(inum).gt.0)then
c            do i=1,luc_h(inum)
c     call CalcMomComp(
c     ,              X_luc_r(i,inum),Y_luc_r(i,inum),Z_luc_r(i,inum),
c     ,              X_clust_r(inum),Y_clust_r(inum),Z_clust_r(inum),
c     ,              px, py, pz, E_clust(inum), 0.0005)
c     call TransformTo6Vector(
c     ,              X_luc_r(i,inum),Y_luc_r(i,inum),Z_luc_r(i,inum),
c     ,              px,py,pz,E_clust(inum),U)  
c     call EqVector(U,U_pos)
c               call EqVector(U,U_neg)
c     
c     ok1 = .TRUE.
c     call  trgTrackToPlane(U_pos,E_clust(inum),dl,
c     ,              a_tracker,b_tracker,c_tracker,d_tracker,ok1)
c               ok1 = .TRUE.
c               call  trgTrackToPlane(U_neg,-E_clust(inum),dl,
c     ,              a_tracker,b_tracker,c_tracker,d_tracker,ok1)
c     
c            enddo
c     
c     endif
         else
            
            
         endif
      endif

      end
      Subroutine PHYSICS_VARIABLES(inum,theta,phi)
      IMPLICIT NONE
      include 'sane_ntuple.cmn'
      include 'gen_data_structures.cmn'
      include 'sane_data_structures.cmn'
      include 'b_ntuple.cmn'
      real*8 Eb,theta_big, phi_big!,ccx,ccy,ccz
      common/FAKEBIG/Eb,theta_big, phi_big
      integer inum, ihistnum
      real*4 theta,phi
      real*4 thetar,phir
      real*4 deg2rad, Mp,shelicity
      Mp = 0.938272309
      deg2rad = 3.1415926536/180.
c                  WRITE(*,*)'call 3',NCLUST
      
      thetar= theta*deg2rad
      phir= phi*deg2rad
c      write(*,*)GEBeam,E_clust(inum),theta
      if(GEBeam - E_clust(inum).gt.0)then
         ENue(inum)      = GEBeam - E_clust(inum)
         Q2(inum)        = 2*GEBeam*E_clust(inum)*(1-cos(thetar))
         X_Bjorken(inum) = Q2(inum)/( 2*Mp*ENue(inum) )
         W2(inum)        = Mp**2 + 2*Mp*ENue(inum) -Q2(inum)
         ihistnum        = (Q2(inum)-2.5)+1
         call NANcheckF(ENue(inum),5)
         call NANcheckF(Q2(inum),5)
         call NANcheckF(X_Bjorken(inum),5)
         call NANcheckF(W2(inum),5)
         shelicity       = i_helicity
         if(ihistnum.gt.0.and.ihistnum.lt.5)then
            call HF1(10600+ihistnum,X_Bjorken(inum),shelicity)
            call HF1(10610+ihistnum,X_Bjorken(inum),1.)
         endif
         if(abs(X_HMS-X_clust(inum)-Bigcal_SHIFT(1)).lt.10.and.
     ,        abs(Y_HMS-Y_clust(inum)-Bigcal_SHIFT(2)).lt.10)then
c            write(*,*)2,E_clust(inum),thetar*57.3,phir*57.3
            
            call HFILL(10620,X_Bjorken(inum),Q2(inum),1.)
            call HFILL(10621,W2(inum),Q2(inum),1.)
         endif
      else
c         write(*,*)'BIG CAL CALIBRATION IS WRONG',E_clust(inum),X_clust(inum),Y_clust(inum)
      endif
      end
