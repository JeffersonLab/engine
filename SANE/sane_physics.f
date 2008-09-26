      subroutine Bigcal_Betta(inum)
      include 'bigcal_data_structures.cmn'
      include 'sane_data_structures.cmn'
      include 'b_ntuple.cmn'
      include 'sane_ntuple.cmn'
      integer inum
      real Vector(3),Vector_r(3)
      INTEGER jj
      real en
      real ebj(25), coorX, coorY,E(10),esum(10)
      esum(inum) = 0
      do jj=1,ncellclust(inum)
         en =((eblock(jj,inum)))/
     ,        BigCal_Calib_Gain(iycell(jj,inum),ixcell(jj,inum))
         ebj(jj)=en
         esum(inum)=sqrt(ebj(jj))+esum(inum)
      enddo
      coorX =0
      coory =0
      do jj=1,ncellclust(inum)
         if(iycell(jj,inum).le.55.and.ixcell(jj,inum).le.32)then
            if(iycell(jj,inum).gt.0.and.ixcell(jj,inum).gt.0)then
               E(inum)=E(inum)+ebj(jj)
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
      call ROTATE(Vector,0.,Bigcal_SHIFT(4)*3.141/180.,0.,Vector_r)
      X_clust_r(inum) =  Vector_r(1)
      Y_clust_r(inum) =  Vector_r(2)
      Z_clust_r(inum) =  Vector_r(3)
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
      include 'bigcal_data_structures.cmn'
      include 'sane_data_structures.cmn'
      include 'b_ntuple.cmn'
      include 'sane_ntuple.cmn'

      integer inum,i
      integer luc
      real tdc_dif, xDelta1,fluc
      real Vector(3),Vector_r(3)
      luc=0
      
      if(luc_hit.gt.0)then
         do i=1,luc_hit
            if(ltdc_pos(i).gt.0.AND.ltdc_neg(i).gt.0)then
               If(abs(ltdc_pos(i)-LUCITE_SANE_MEAN_POS(luc_row(i))).lt.
     ,              4*LUCITE_SANE_SIGMA_POS(luc_row(i)).and.
     ,              abs(ltdc_neg(i)-LUCITE_SANE_MEAN_NEG(luc_row(i))).lt.
     ,              4*LUCITE_SANE_SIGMA_NEG(luc_row(i))
     ,              )then
                  
                  tdc_dif=float(ltdc_pos(i))-
     ,                 float(ltdc_neg(i))
     ,                 +LUCITE_SANE_MEAN_NEG(luc_row(i))
     ,                 -LUCITE_SANE_MEAN_POS(luc_row(i))
c     
c     X coordinate for Lucite
c     
                  
                  xDelta1    = 
     ,                 (tdc_dif)*LUCITE_SANE_TDC_TIMING(luc_row(i))*
     ,                 29.979/1.49*0.7313
                  
C     
C     Y Geometrical CUT
C     
                  IF(abs(luc_y(i)-Lucite_SHIFT(2)-
     ,                 Lucite_SHIFT(3)/Bigcal_SHIFT(3)*
     ,                 (yclust(inum))-Bigcal_SHIFT(2)).lt.6)then
C     
C     X Geometrical CUT
C     
                     If(abs(xDelta1-Lucite_SHIFT(1)-
     ,                    Lucite_SHIFT(3)/Bigcal_SHIFT(3)*
     ,                    (xclust(inum))-Bigcal_SHIFT(1)).lt.10)then
                        
                        luc = luc+1
                        X_luc(luc,inum)   = xDelta1
                        Y_luc(luc,inum)   = luc_y(i)+3/1.33
                        Z_luc(luc,inum)   = sqrt(Lucite_SHIFT(3)**2-
     ,                       X_luc(luc,inum)**2)
                        Vector(1)         = X_luc(luc,inum)
                        Vector(2)         = Y_luc(luc,inum)
                        Vector(3)         = Z_luc(luc,inum)
                        call ROTATE(Vector,0.,Lucite_SHIFT(4)*3.141/180.,0.,Vector_r)
                        X_luc_r(luc,inum)   = Vector_r(1)
                        Y_luc_r(luc,inum)   = Vector_r(2)
                        Z_luc_r(luc,inum)   = Vector_r(3)
                        
                     Endif
                     
                  ENDIF
               endif
               
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
      include 'sane_data_structures.cmn'
      include 'sane_ntuple.cmn'
      real*4 TrakerVertex(3), TrakerVertex_r(3)
      real*4 TrakerTEMP_Y1,TrakerTEMP_Y2 
      integer i,inum

c
c     Average coordinate on X plane

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
      TrakerVertex(1)          = TrakerVertex(1)*TrakerVertex(3)/Z_trc(trc_hx(inum),inum)  
      call ROTATE(TrakerVertex,0.,Bigcal_SHIFT(4)*3.141/180.,0.,TrakerVertex_r)
      Tr_Vertex(1,inum)        = TrakerVertex(1)
      Tr_Vertex(2,inum)        = TrakerVertex(2)
      Tr_Vertex(3,inum)        = TrakerVertex(3)

      Tr_Vertex_r(1,inum)        = TrakerVertex_r(1)
      Tr_Vertex_r(2,inum)        = TrakerVertex_r(2)
      Tr_Vertex_r(3,inum)        = TrakerVertex_r(3)
      
      end


c
c     Geometry match of the tracks
c
ccccccccccccccccccccccccccccccccccccc
      Subroutine GeometryMatch(inum)
      include 'bigcal_data_structures.cmn'
      include 'sane_data_structures.cmn'
      include 'b_ntuple.cmn'
      include 'sane_ntuple.cmn'
      integer i,j,inum
      real*8 U, U_pos, U_neg
      real*8 dl
      LOGICAL ok1
      data P1_bigcal /0.,0.,320./
      data P2_bigcal /0.,1.,320./
      data P3_bigcal /1.,0.,320./
      data P1_track /0.,0.,66./
      data P2_track /0.,1.,66./
      data P3_track /1.,0.,66./
      dl =0.1
      if(a_tracker.eq.0.or.b_tracker.eq.0)then
cc
c     Define Traker plane
c     
         call ROTATE(P1_track, 0., 50*3.141/180., 0.,P1_track_r)
         call ROTATE(P2_track, 0., 50*3.141/180., 0.,P2_track_r)
         call ROTATE(P3_track, 0., 50*3.141/180., 0.,P3_track_r)
         call Plane(P1_track_r,P2_track_r,P3_track_r,
     ,        a_tracker,b_tracker,c_tracker,d_tracker)
         
      endif
      if(a_bigcal.eq.0.or.b_bigcal.eq.0)then
cc
c     Define Bigcal plane
c     
         call ROTATE(P1_bigcal, 0., 50*3.141/180., 0. ,P1_bigcal_r)
         call ROTATE(P2_bigcal, 0., 50*3.141/180., 0. ,P2_bigcal_r)
         call ROTATE(P3_bigcal, 0., 50*3.141/180., 0. ,P3_bigcal_r)
         call Plane(P1_bigcal_r,P2_bigcal_r,P3_bigcal_r,
     ,        a_bigcal,b_bigcal,c_bigcal,d_bigcal)
         
      endif
c
c
c     Start tracking
c
      if(cer_h(inum).gt.0)then
         
c
c     The particle is charged. 
c
         if(luc_h(inum).gt.0)then
            do i=1,luc_h(inum)
               call CalcMomComp(
     ,              X_luc_r(i,inum),Y_luc_r(i,inum),Z_luc_r(i,inum),
     ,              X_clust_r(inum),Y_clust_r(inum),Z_clust_r(inum),
     ,              px, py, pz, E_clust(inum), 0.0005)
               call TransformTo6Vector(
     ,              X_luc_r(i,inum),Y_luc_r(i,inum),Z_luc_r(i,inum),
     ,              px,py,pz,E_clust(inum),U)  
               call EqVector(U,U_pos)
               call EqVector(U,U_neg)

               ok1 = .TRUE.
               call  trgTrackToPlane(U_pos,E_clust(inum),dl,
     ,              a_tracker,b_tracker,c_tracker,d_tracker,ok1)
               ok1 = .TRUE.
               call  trgTrackToPlane(U_neg,-E_clust(inum),dl,
     ,              a_tracker,b_tracker,c_tracker,d_tracker,ok1)
               
            enddo

         endif
      else


      endif


      end
