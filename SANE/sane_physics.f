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
      real*4 ebj(25), coorX, coorY, coorX4, coorY4,ener
      real*4 coorX1,coorX2,coorX3,coorY1,coorY2,coorY3,E(10),esum(10),w,wX,wY
      real*8 Eb,theta_big, phi_big,shift!,ccx,ccy,ccz, emax,jmax
      common/FAKEBIG/Eb,theta_big, phi_big
      integer jmax(20)
      real emax(20)
      common/EBL/jmax,emax
      esum(inum) = 0
      E(inum) =0
      w=0
      wX=0
      wY=0
c         write(*,*)5,8,BigCal_Calib_Gain(8,5)
c        write(*,*)5,16,BigCal_Calib_Gain(16,5)
      emax(inum)=0
      jmax(inum)=0
      do jj=1,ncellclust(inum)
         if(BigCal_Calib_Gain(ixcell(jj,inum),iycell(jj,inum)).eq.0)
     ,        BigCal_Calib_Gain(ixcell(jj,inum),iycell(jj,inum))=1. 
         en =((eblock(jj,inum)))/
     ,        BigCal_Calib_Gain(ixcell(jj,inum),iycell(jj,inum))
         ebj(jj)=en
         if(en.gt.emax(inum))then
            emax(inum) = en
            jmax(inum) = jj
         endif
         E(inum)=E(inum)+ebj(jj)
         esum(inum)=sqrt(ebj(jj))+esum(inum)
      enddo
      ener = e(inum)
      do jj=1,ncellclust(inum)
         w =  w+log(sqrt(ebj(jj))/esum(inum))
         wX = wX+ebj(jj)
         wY = wY+log(ebj(jj)/ener)
      enddo

      coorX =0
      coory =0
c      write(*,*) 'COOR0',inum,coorX,coorY,Bigcal_SHIFT(3),e(inum)
      add_factor = 1!-Eb/1000./e(inum)
      E(inum) =0
      coorX1= 0
      coorX2= 0
      coorX3= 0
      coorY1= 0
      coorY2= 0
      coorY3= 0

      do jj=1,ncellclust(inum)
c         write(34,*)ixcell(jj,inum),iycell(jj,inum),add_factor
         if(iycell(jj,inum).le.56.and.ixcell(jj,inum).le.32)then
            if(iycell(jj,inum).gt.0.and.ixcell(jj,inum).gt.0)then
               E(inum)=E(inum)+ebj(jj)*add_factor
               coorX1 = coorX1 +
     ,              (xcell(jj,inum)-xcell(jmax(inum),inum))*sqrt(ebj(jj))/esum(inum)!*cos(atan(xcell(jj,inum)/Bigcal_SHIFT(3)))
               coorY1 = coorY1 +
     ,              (ycell(jj,inum)-ycell(jmax(inum),inum))*sqrt(ebj(jj))/esum(inum)!*cos(atan(ycell(jj,inum)/Bigcal_SHIFT(3)))
               coorX2 = coorX2 +
     ,              (xcell(jj,inum)-xcell(jmax(inum),inum))*(log(sqrt(ebj(jj))/esum(inum))/w)**2
               coorY2 = coorY2 +
     ,              (ycell(jj,inum)-ycell(jmax(inum),inum))*(log(sqrt(ebj(jj))/esum(inum))/w)**2
               coorY = coorY2
               if(abs(coorY1).gt.abs(coorY2))coorY =(coorY1+coorY2)/2. 
               call NANcheckF(coorX2,33)
               call NANcheckF(coorY2,33)
               coorX3 = coorX3 +
     ,              (xcell(jj,inum)-xcell(jmax(inum),inum))*ebj(jj)/wX !*cos(atan(xcell(jj,inum)/Bigcal_SHIFT(3)))
               coorY3 = coorY3 +
     ,              (ycell(jj,inum)-ycell(jmax(inum),inum))*ebj(jj)/wX !*cos(atan(ycell(jj,inum)/Bigcal_SHIFT(3)))

               if(abs(coorY3).gt.abs(coorY))coorY =(coorY3+coorY)/2. 

               coorX4 = coorX +
     ,              (xcell(jj,inum)-xcell(jmax(inum),inum))*(log(ebj(jj)/wx)/wy)**2
               coorY4 = coorY +
     ,              (ycell(jj,inum)-ycell(jmax(inum),inum))*(log(ebj(jj)/wx)/wy)**2
               if(abs(coorY4).gt.abs(coorY))coorY =(coorY4+coorY)/2. 

               call NANcheckF(coorX,33)
               call NANcheckF(coorY,33)
c     write(*,*)log(ebj(jj)/esum(inum)),w
               
            endif
         endif
      enddo
      if(e(inum).lt.0.6)return
c      if(e(inum).gt.4.7)then
c         do jj=1,ncellclust(inum)
c            if(ebj(jj).gt.3)then
c               write(iycell(jj,inum),*)ixcell(jj,inum),ebj(jj),eblock(jj,inum),BigCal_Calib_Gain(ixcell(jj,inum),iycell(jj,inum))
c            endif
c         enddo
c      endif

c     
c     Correcting coordinate (calculated fro pi0 calib correct for gamma)
c     

cc       call correct_coordinate(coorX,coorY,Bigcal_SHIFT(3),e(inum))

c      write(*,*)'X ',coorX1,coorX2,coorX3,(coorX1+coorX2+coorX3)/3.,ncellclust(inum)
c      coorY = coorY2
c      if(ncellclust(inum).gt.4.and.abs(coorY1).gt.abs(coorY2))then
c         write(*,*)'Y ',coorY1,coorY2,coorY3,coorY,(coory3+coory)/2.,(coorY1+coorY2+coorY3+coorY)/4.
c         coorY =coorY1 
c         write(*,*)jmax(inum),ncellclust(inum)
c      endif

c      X_clust(inum) = (coorX1+coorX2+coorX3+coorX)/4.+xcell(1,inum)
c      Y_clust(inum) = (coorY1+coorY2+coorY3+coorY)/4.+ycell(1,inum)
      
      if(ycell(jmax(inum),inum).lt.-50)then
         shift =7.5 
      elseif(ycell(jmax(inum),inum).gt.-50.and.ycell(jmax(inum),inum).lt.20)then
         shift=2.5
      else
         shift = -2.5
      endif
      
      X_clust(inum) = xcell(jmax(inum),inum)+coorX3!+(coorX1+coorX1)/2.
      Y_clust(inum) = ycell(jmax(inum),inum)+coorY3!+0.07*(ycell(jmax(inum),inum)-25)+shift +(coorY3)
      Z_clust(inum) = Bigcal_SHIFT(3)
      E_clust(inum) = E(inum)
      eclust(inum)  = E(inum)
c      xclust(inum)  = coorX
c      yclust(inum)  = coorY
      Vector(1)         = X_clust(inum)
      Vector(2)         = Y_clust(inum)
      Vector(3)         = Z_clust(inum)
      call ROTATE(Vector,0.,-Bigcal_SHIFT(4)*3.1415926536/180.,0.,Vector_r)

      X_clust_r(inum) =  Vector_r(1)
      Y_clust_r(inum) =  Vector_r(2)
      Z_clust_r(inum) =  Vector_r(3)
           call NANcheckF(E_clust(inum),33)
           call NANcheckF(X_clust(inum),33)
           call NANcheckF(Y_clust(inum),33)
           call NANcheckF(Z_clust(inum),33)
           call NANcheckF(X_clust_r(inum),33)
           call NANcheckF(Y_clust_r(inum),33)
           call NANcheckF(Z_clust_r(inum),33)
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
           call NANcheckF(Theta_e(inum),33)
           call NANcheckF(Phi_e(inum),33)

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
      integer i,inum,j
      integer i4,itrig
      logical cut1,cut2
      common /B_TIME_SHIFT/i4,itrig
      x1t_stat = 0
      y1t_stat = 0
      y2t_stat = 0
      trc_hx(inum) =0
ccccc
c
c     Geometrical matching
c
ccccc
      if(cer_h(inum).ge.0)then
         do i=1,x1t_hit
            x1t_tdc(i)=x1t_tdc(i)-BIG_TIME_SHIFT_CH(i4)
c     
c     Very wide geometrical cut
c     
            cut1 = .FALSE.
            cut1 = abs(x1t_tdc(i)-TRACKER_SANE_XCALIBRATION(x1t_row(i))).lt.
     ,           TRACKER_SANE_XSIGMA(x1t_row(i))
            cut2 = .FALSE.
            if(BIG_TIME_SHIFT_CH(3).ne.0)then
               cut2 = abs(x1t_tdc(i)-670-TRACKER_SANE_XCALIBRATION(x1t_row(i))).lt.
     ,           TRACKER_SANE_XSIGMA(x1t_row(i))
            endif
c                  call HFILL(10103,float(x1t_row(i)),float(x1t_tdc(i)),1.)
            if(cut1.or.cut2)then
               call HFILL(10106,x1t_x(i)-TrackerX_SHIFT(1),TrackerX_SHIFT(3)/Bigcal_SHIFT(3)*
     ,              (x_clust(inum))-Bigcal_SHIFT(1),1.)
                  call HFILL(10103,float(x1t_row(i)),float(x1t_tdc(i)),1.)
            endif
            IF(abs(x1t_x(i)-TrackerX_SHIFT(1)-
     ,           TrackerX_SHIFT(3)/Bigcal_SHIFT(3)*
     ,           (x_clust(inum))-Bigcal_SHIFT(1)).lt.1.5)then
c               if(cer_h(inum).gt.0)then
c               endif
               
c     
c     TDC CUT
c     
               if(cut1.or.cut2)then
                  x1t_stat = x1t_stat+1
                  trc_hx(inum) = x1t_stat
                  if(x1t_stat.gt.0)then
c     
                     X_trc(trc_hx(inum),inum)  = x1t_x(i)-TrackerX_SHIFT(1)
                     X_trc_r(trc_hx(inum),inum)  = 0
                     Z_trc_r(trc_hx(inum),inum)  = 0
                     call NANcheckF(X_trc(trc_hx(inum),inum),35)
                     Z_trc(trc_hx(inum),inum)  = TrackerX_SHIFT(3)
                     
                  endif
               endif
            ENDIF
         enddo
         trc_hy1(inum) =0
         do i=1,y1t_hit
            y1t_tdc(i)=y1t_tdc(i)-BIG_TIME_SHIFT_CH(i4)
c     
c     Very wide geometrical cut
c     
            cut1 = .FALSE.
            cut1 = abs(y1t_tdc(i)-TRACKER_SANE_Y1CALIBRATION(y1t_row(i))).lt.
     ,           TRACKER_SANE_Y1SIGMA(y1t_row(i))
            cut2 = .FALSE.
            if(BIG_TIME_SHIFT_CH(3).ne.0)then
               cut2 = abs(y1t_tdc(i)-670-TRACKER_SANE_Y1CALIBRATION(y1t_row(i))).lt.
     ,              TRACKER_SANE_Y1SIGMA(y1t_row(i))
            endif
            
            if(cut1.or.cut2)then
               call HFILL(10107,y1t_y(i)-TrackerY1_SHIFT(2),TrackerY1_SHIFT(3)/Bigcal_SHIFT(3)*
     ,              (y_clust(inum))-Bigcal_SHIFT(2),1.)
            endif

            IF(abs(y1t_y(i)-TrackerY1_SHIFT(2)-
     ,           TrackerY1_SHIFT(3)/Bigcal_SHIFT(3)*
     ,           (y_clust(inum))-Bigcal_SHIFT(2)).lt.2.)then
               if(cer_h(inum).gt.0)then
                  call HFILL(10104,float(y1t_row(i)),float(y1t_tdc(i)),1.)
               endif
c     
c     TDC CUT
c     
               if(cut1.or.cut2)then
                  y1t_stat = y1t_stat +1
                  trc_hy1(inum) = y1t_stat
                  if(y1t_stat.gt.0)then
                     
                     Y1_trc(trc_hy1(inum),inum)  = y1t_y(i)-TrackerY1_SHIFT(2)
                     Y1_trc_r(trc_hy1(inum),inum)  = 0
                      Z1_trc_r(trc_hy1(inum),inum)  = 0
                     call NANcheckF(Y1_trc(trc_hy1(inum),inum),35)
                     Z1_trc(trc_hy1(inum),inum)  = TrackerY1_SHIFT(3)
                  endif
               endif
            ENDIF
         enddo
         trc_hy2(inum) =0
         do i=1,y2t_hit
            y2t_tdc(i)=y2t_tdc(i)-BIG_TIME_SHIFT_CH(i4)-BIG_TIME_SHIFT_CH(itrig)
c     
c     Very wide geometrical cut
c     
            cut1 = .FALSE.
            cut1 = abs(y2t_tdc(i)-TRACKER_SANE_Y2CALIBRATION(y2t_row(i))).lt.
     ,           TRACKER_SANE_Y2SIGMA(y2t_row(i))
            cut2 = .FALSE.
            if(BIG_TIME_SHIFT_CH(3).ne.0)then
               cut2 = abs(y2t_tdc(i)-670-TRACKER_SANE_Y2CALIBRATION(y2t_row(i))).lt.
     ,              TRACKER_SANE_Y2SIGMA(y2t_row(i))
            endif
            
            if(cut1.or.cut2)then
               call HFILL(10108,y2t_y(i)-TrackerY2_SHIFT(2),TrackerY2_SHIFT(3)/Bigcal_SHIFT(3)*
     ,              (y_clust(inum))-Bigcal_SHIFT(2),1.)
            endif
c            write(*,*)y2t_tdc(i),TRACKER_SANE_Y2CALIBRATION(y2t_row(i)),TRACKER_SANE_Y1SIGMA(y1t_row(i))
            IF(abs(y2t_y(i)-TrackerY2_SHIFT(2)-
     ,           TrackerY2_SHIFT(3)/Bigcal_SHIFT(3)*
     ,           (y_clust(inum))-Bigcal_SHIFT(2)).lt.2.)then
               if(cer_h(inum).gt.0)then
                  call HFILL(10105,float(y2t_row(i)),float(y2t_tdc(i)),1.) 
               endif
c     
c     TDC CUT
c     
               if(cut1.or.cut2)then
                  
                  y2t_stat = y2t_stat +1
                  trc_hy2(inum) = y2t_stat
                  if(y2t_stat.gt.0)then
                     
                     Y2_trc(trc_hy2(inum),inum)  = y2t_y(i)-TrackerY2_SHIFT(2)
                     Y2_trc_r(trc_hy2(inum),inum)  = 0
                     Z2_trc_r(trc_hy2(inum),inum)  = 0
                     call NANcheckF(Y2_trc(trc_hy2(inum),inum),35)

                     Z2_trc(trc_hy2(inum),inum)  = TrackerY2_SHIFT(3)
                  endif
                  
               endif
               
            ENDIF
         enddo
      endif

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
      integer jmax(20)
      real emax(20)
      common/EBL/jmax,emax

      integer inum,i
      integer luc
      real*4 tdc_dif, xDelta1,fluc,koef,yfake,ytracker,ztracker,ip,ipi(10)
      real*4 Vector(3),Vector_r(3),meanshift,Zbigclust
      integer i4,itrig
      logical cut,cut1,cut2
      common /B_TIME_SHIFT/i4,itrig
      luc=0
      ip=0
      if(luc_hit.gt.0
c     ,     .and.cer_h(inum).ge.0
     ,     )then
         do i=1,luc_hit
            ltdc_pos(i) = ltdc_pos(i) -BIG_TIME_SHIFT_CH(i4)!-BIG_TIME_SHIFT_CH(itrig)
            ltdc_neg(i) = ltdc_neg(i) -BIG_TIME_SHIFT_CH(i4)!-BIG_TIME_SHIFT_CH(itrig)
            
c            IF(abs(luc_y(i)-Lucite_SHIFT(2)-
c     ,           Lucite_SHIFT(3)/Bigcal_SHIFT(3)*
c     ,           (y_clust(inum))-Bigcal_SHIFT(2)).lt.8.and.
c     ,           abs(xclust(inum)).lt.90)then
c               call HFILL(10121, float(luc_row(i)), float(ltdc_pos(i)), 1.)
c               call HFILL(10122, float(luc_row(i)), float(ltdc_neg(i)), 1.)
c               call HFILL(10125, float(luc_row(i)), float(ladc_pos(i)), 1.)
c               call HFILL(10126, float(luc_row(i)), float(ladc_neg(i)), 1.)
c            endif
c     write(*,*)ltdc_pos(i),LUCITE_SANE_MEAN_POS(luc_row(i)),
c     ,           LUCITE_SANE_SIGMA_POS(luc_row(i))
c     write(*,*)'POS ',ltdc_pos(i),LUCITE_SANE_MEAN_POS(luc_row(i)),
c     ,           LUCITE_SANE_SIGMA_POS(luc_row(i))
c     write(*,*)'NEG ', ltdc_neg(i),LUCITE_SANE_MEAN_NEG(luc_row(i)),
c     ,           LUCITE_SANE_SIGMA_NEG(luc_row(i))
            cut =abs(ltdc_pos(i)-LUCITE_SANE_MEAN_POS(luc_row(i))).lt.
     ,           LUCITE_SANE_SIGMA_POS(luc_row(i)).and.
     ,           abs(ltdc_neg(i)-LUCITE_SANE_MEAN_NEG(luc_row(i))).lt.
     ,           LUCITE_SANE_SIGMA_NEG(luc_row(i))
            cut1 = .FALSE.
            cut2 = .FALSE.
            if(BIG_TIME_SHIFT_CH(3).ne.0)then
               cut =(abs(ltdc_pos(i)-LUCITE_SANE_MEAN_POS(luc_row(i))).lt.
     ,              1.2*LUCITE_SANE_SIGMA_POS(luc_row(i)).and.
     ,              abs(ltdc_neg(i)-LUCITE_SANE_MEAN_NEG(luc_row(i))).lt.
     ,              1.2*LUCITE_SANE_SIGMA_NEG(luc_row(i))).or.
     ,              (abs(ltdc_pos(i)-BIG_TIME_SHIFT_CH(itrig)-LUCITE_SANE_MEAN_POS(luc_row(i))).lt.
     ,              1.2*LUCITE_SANE_SIGMA_POS(luc_row(i)).and.
     ,              abs(ltdc_neg(i)-BIG_TIME_SHIFT_CH(itrig)-LUCITE_SANE_MEAN_NEG(luc_row(i))).lt.
     ,              1.2*LUCITE_SANE_SIGMA_NEG(luc_row(i)))

               cut1 =             (abs(ltdc_pos(i)-LUCITE_SANE_MEAN_POS(luc_row(i))).lt.
     ,              1.2*LUCITE_SANE_SIGMA_POS(luc_row(i)).and.
     ,              abs(ltdc_neg(i)-BIG_TIME_SHIFT_CH(itrig)-LUCITE_SANE_MEAN_NEG(luc_row(i))).lt.
     ,              1.2*LUCITE_SANE_SIGMA_NEG(luc_row(i)))
               if(cut1)ltdc_neg(i)=ltdc_neg(i)-BIG_TIME_SHIFT_CH(itrig)
               cut2 =
     ,              (abs(ltdc_pos(i)-BIG_TIME_SHIFT_CH(itrig)-LUCITE_SANE_MEAN_POS(luc_row(i))).lt.
     ,              1.2*LUCITE_SANE_SIGMA_POS(luc_row(i)).and.
     ,              abs(ltdc_neg(i)-LUCITE_SANE_MEAN_NEG(luc_row(i))).lt.
     ,              1.2*LUCITE_SANE_SIGMA_NEG(luc_row(i)))
               if(cut2)ltdc_pos(i)=ltdc_pos(i)-BIG_TIME_SHIFT_CH(itrig)
c               if(cut)
c     ,          write(*,*)(float(ltdc_pos(i))-
c     ,              float(ltdc_neg(i))
c     ,              +LUCITE_SANE_MEAN_NEG(luc_row(i))
c     ,              -LUCITE_SANE_MEAN_POS(luc_row(i)))*LUCITE_SANE_TDC_TIMING(luc_row(i))*
c     ,              29.979/1.49*0.7313-Lucite_SHIFT(1)
            endif
            IF(abs(luc_y(i)-Lucite_SHIFT(2)-
     ,           Lucite_SHIFT(3)/Bigcal_SHIFT(3)*
     ,           (y_clust(inum))-Bigcal_SHIFT(2)).lt.8.and.
     ,           yclust(inum).gt.0.and.xclust(inum).lt.0)then
               call HFILL(10121, float(luc_row(i)), float(ltdc_pos(i)), 1.)
               call HFILL(10122, float(luc_row(i)), float(ltdc_neg(i)), 1.)
               call HFILL(10125, float(luc_row(i)), float(ladc_pos(i)), 1.)
               call HFILL(10126, float(luc_row(i)), float(ladc_neg(i)), 1.)
            endif

            If(cut.or.cut1.or.cut2
     ,           )then
               
               tdc_dif=float(ltdc_pos(i))-
     ,              float(ltdc_neg(i))
     ,              +LUCITE_SANE_MEAN_NEG(luc_row(i))
     ,              -LUCITE_SANE_MEAN_POS(luc_row(i))
c     
c     X coordinate for Lucite
c     
               
               xDelta1    = 
     ,              -(tdc_dif)*LUCITE_SANE_TDC_TIMING(luc_row(i))*
     ,              29.979/1.49*0.7313*LUCITE_SANE_COEF(luc_row(i))-LUCITE_SANE_SHIFT(luc_row(i))*Lucite_SHIFT(3)/Bigcal_SHIFT(3)
               
c               if(xclust(inum).lt.0.and.luc_row(i).gt.18)then
c               write(*,*)xDelta1-Lucite_SHIFT(1),abs(luc_y(i)-Lucite_SHIFT(2)-
c     ,              Lucite_SHIFT(3)/Bigcal_SHIFT(3)*
c     ,              (y_clust(inum))-Bigcal_SHIFT(2))
c                  call HFILL(10128,luc_y(i),Yclust(inum),1.)
c                  endif
C     
C     Y Geometrical CUT
C     
**********
               
               IF(abs(luc_y(i)-Lucite_SHIFT(2)-
     ,              Lucite_SHIFT(3)/Bigcal_SHIFT(3)*
     ,              (y_clust(inum))-Bigcal_SHIFT(2)).lt.18)then
                  call HFILL(10128,luc_y(i),Y_clust(inum),1.)
c     if(luc_row(i).eq.10)then
                  call HFILL(10150+luc_row(i),xDelta1-Lucite_SHIFT(1),
     ,                 sqrt(Lucite_SHIFT(3)**2+luc_y(i)**2)/sqrt(Bigcal_SHIFT(3)**2+
     ,                 Y_clust(inum)**2)*X_clust(inum),1.)
                  call HFILL(20150+luc_row(i),(xDelta1-Lucite_SHIFT(1))*
     ,                 Bigcal_SHIFT(3)/Lucite_SHIFT(3),
     ,                 Xclust(inum),1.)
c     endif
c     if(abs(X_clust(inum)).lt.5)then
                  call HFILL(10131, float(luc_row(i)), float(ltdc_pos(i)), 1.)
                  call HFILL(10132, float(luc_row(i)), float(ltdc_neg(i)), 1.)
                  call HFILL(10135, float(luc_row(i)), float(ladc_pos(i)), 1.)
                  call HFILL(10136, float(luc_row(i)), float(ladc_neg(i)), 1.)
c     endif
                  ip=ip+1
                  ipi(ip) = i
C     
C     X Geometrical CUT
C     
***********
                  If(abs(xDelta1-Lucite_SHIFT(1)-
     ,                 Lucite_SHIFT(3)/Bigcal_SHIFT(3)*
     ,                 (x_clust(inum))-Bigcal_SHIFT(1)).lt.20)then
                     
                     luc = luc+1
                     X_luc(luc,inum)   = xDelta1-Lucite_SHIFT(1)
                     Y_luc(luc,inum)   = luc_y(i)-Lucite_SHIFT(2)
                     Z_luc(luc,inum)   = sqrt(Lucite_SHIFT(3)**2-
     ,                    X_luc(luc,inum)**2)
                     call NANcheckF(X_luc(luc,inum),333)
                     call NANcheckF(Y_luc(luc,inum),444)
                     call NANcheckF(Z_luc(luc,inum),555)
                     Vector(1)         = X_luc(luc,inum)
                     Vector(2)         = Y_luc(luc,inum)
                     Vector(3)         = Z_luc(luc,inum)
                     call ROTATE(Vector,0.,-BIGCAL_SHIFT(4)*3.1415926536/180.,0.,Vector_r)
                     X_luc_r(luc,inum)   = Vector_r(1)
                     Y_luc_r(luc,inum)   = Vector_r(2)
                     Z_luc_r(luc,inum)   = Vector_r(3)
                     call NANcheckF(X_luc_r(luc,inum),34)
                     call NANcheckF(Y_luc_r(luc,inum),34)
                     call NANcheckF(Z_luc_r(luc,inum),34)
                  ENDIF
                  
               Endif
                     
c                  ENDIF
             
               endif
               
         enddo
         luc_h(inum) = luc
c         write(*,*)luc
         X_luc_av(inum) = 0
         Y_luc_av(inum) = 0
         Z_luc_av(inum) = 0
         if(luc_h(inum).eq.2)then
             if(abs(Y_luc(1,inum)-Y_luc(2,inum)).lt.7.and.eclust(inum).gt.0.7.and.
     ,           abs(X_luc(1,inum)-X_luc(2,inum)).lt.15.and.ncellclust(inum).gt.6)then
c               write(32,*)X_luc(1,inum),X_luc(2,inum),luc_row(ipi(1)),luc_row(ipi(2))
c           write(*,*)luc_h(inum)
            
c
c     GOOD LUCITE HIT
c
               X_luc_av(inum) =  (X_luc(1,inum)+X_luc(2,inum))/2.
               Z_luc_av(inum) =  (Z_luc(1,inum)+Z_luc(2,inum))/2.
               Y_luc_av(inum) =  (Y_luc(1,inum)+Y_luc(2,inum))/2.
c               write(*,*)Y_luc_av(inum)
               Y_luc_av(inum) =  Y_luc_av(inum)-3.5/2.*Y_luc_av(inum)/Z_luc_av(inum)
c                write(*,*)'   ',Y_luc_av(inum)
              
               if(abs(X_clust(inum)).lt.103)then
                  if((trc_hy1(inum).gt.0.or.trc_hy2(inum).gt.0))then
                     ytracker = Y2_trc(trc_hy2(inum),inum)
                     if(trc_hy2(inum).gt.0)then
                        ytracker = Y2_trc(trc_hy2(inum),inum)
                        ztracker = TrackerY2_SHIFT(3)
                     endif
                     if(trc_hy1(inum).gt.0)then
                        ytracker = Y1_trc(trc_hy1(inum),inum)
                        ztracker = TrackerY1_SHIFT(3)
                     endif

                     koef = (Y_luc_av(inum)-ytracker)/(Z_luc_av(inum)-ztracker)
                     yfake = koef*(Bigcal_SHIFT(3)-ztracker)+ytracker
                     Zbigclust = (Y_clust(inum)-ytracker)/koef+ztracker-Bigcal_SHIFT(3)
c                     write(*,*)Zbigclust
c                     write(*,*)Y_clust(inum),yfake,Y_luc_av(inum)*Bigcal_SHIFT(3)/Z_luc_av(inum)
c                     write(*,*)cer_h(inum)
                     call HFILL(10220,Yfake,yfake-Y_clust(inum),1.)
                     call HFILL(10221,Y_luc_av(inum)*Bigcal_SHIFT(3)/Z_luc_av(inum)-Yclust(inum),Yclust(inum),1.)
c                     if()then
                        call HFILL(10225,yfake-Y_clust(inum),emax(inum)/eclust(inum),1.)
                        if(yfake.gt.50)then
                           call HFILL(10222,x_clust(inum),(yfake-Y_clust(inum)),1.)
                        endif
                  endif
               endif
               if(abs(Yclust(inum)).lt.10)then
                  call HFILL(10223,X_luc_av(inum)*Bigcal_SHIFT(3)/Z_luc_av(inum)-Xclust(inum),Xclust(inum),1.)
                  call HFILL(10226,X_luc_av(inum)*Bigcal_SHIFT(3)/Z_luc_av(inum)-X_clust(inum),eclust(inum),1.)
               endif
            endif
         endif
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
      include 'gen_data_structures.cmn'
      integer inum,i,j,cer_n,k
      integer i4,itrig
      common /B_TIME_SHIFT/i4,itrig
      real amax
      
      cer_h(inum)=0
      if(ncellclust(inum).gt.4)then
         i4=1
         itrig = 1
         amax = ablock(1,inum)
         
         if(iycell(1,inum).gt.32) then
            i4=2
         else 
            i4=1
         endif
c         itrig = 3
         if(iycell(1,inum).ge.33.and.ixcell(1,inum).le.15)itrig=3
         
c     write(*,*)"Amax= ",amax,ncellclust(inum)
c     do k=1,ncellclust(inum)
c     if(ablock(k,inum).ge.amax)then
c     
c            amax = ablock(k,inum)
c     i4 = iycell(k,inum)
c     write(*,*)"i4=",i4,k,iycell(k,inum)
c     endif
c     enddo
         
         do i=1, cer_hit
            if(cer_num(i).lt.9)then
               cer_n = cer_num(i)
c     write(*,*)cer_n,CER_SANE_GEOM_CUT_LOW(cer_n),CER_SANE_GEOM_CUT_HI(cer_n)
               if(yclust(inum).gt.(CER_SANE_GEOM_CUT_LOW(cer_n)-1)*4-120..and.
     ,              yclust(inum).lt.(CER_SANE_GEOM_CUT_HI(cer_n)+1)*4-120.and.
     ,              CER_SANE_GEOM_CUT_X(cer_n)*xclust(inum).gt.-20.and.
     ,              T_trgBIG.ge.30.and.T_trgBIG.lt.41)then
c     write(*,*)BIG_TIME_SHIFT_CH(i4),iycell(1,inum)
                  if(cer_tdc(i).gt.-1650.and.itrig.eq.3)then
                     itrig=3
c                  elseif(cer_tdc(i).lt.-1650.and.itrig.eq.3)then
c                     cer_tdc(i)=0
c                     itrig=3
                  else
                    itrig=1 
                  endif
                  cer_tdc(i) = cer_tdc(i) -BIG_TIME_SHIFT_CH(i4)-BIG_TIME_SHIFT_CH(itrig)
c                  if(itrig.eq.3)then
                     call HFILL(10500+cer_num(i),float(cer_adcc(i)),float(cer_tdc(i)),1.)
c                  endif
               endif
               
               if(
     ,              abs(cer_tdc(i)-CER_SANE_MEAN(cer_num(i))).lt.
     ,              CER_SANE_SIGMA(cer_n)
     ,              )then
                  if(cer_adcc(i).gt.CER_SANE_ADC_CUT(cer_n)/40..and.T_trgBIG.ge.30.and.T_trgBIG.lt.41)then
                     if(yclust(inum).gt.(CER_SANE_GEOM_CUT_LOW(cer_n)*4-1)-120..and.
     ,                    yclust(inum).lt.(CER_SANE_GEOM_CUT_HI(cer_n)*4+1)-120.and.
     ,                    CER_SANE_GEOM_CUT_X(cer_n)*xclust(inum).gt.-20)then
                        cer_h(inum)=cer_h(inum)+1
c                        write(*,*)cer_h(inum),cer_hit,cer_n
                        do j=1, ncellclust(inum) 
                           call HFILL(10510+cer_n,float(ixcell(j,inum)),float(iycell(j,inum)), 1.)
                        enddo
                     endif
                  endif
               endif
            endif
         enddo
      endif
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
      if(trc_hx(inum).gt.0.and.(trc_hy1(inum).gt.0.or.trc_hy2(inum).gt.0))then
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
c         write(*,*)TrakerTEMP_Y1,TrakerTEMP_Y2
         if(TrakerTEMP_Y1.ne.0.and.TrakerTEMP_Y2.ne.0)then
            TrakerVertex(2)       = (TrakerTEMP_Y1+TrakerTEMP_Y2)/2.
            TrakerVertex(3)       = (TrackerY1_SHIFT(3)+TrackerY2_SHIFT(3))/2.
         else if(TrakerTEMP_Y1.ne.0.and.TrakerTEMP_Y2.eq.0)then
            TrakerVertex(2)       = TrakerTEMP_Y1
            TrakerVertex(3)       = TrackerY1_SHIFT(3)
         else if(TrakerTEMP_Y1.eq.0.and.TrakerTEMP_Y2.ne.0)then
            TrakerVertex(2)       = TrakerTEMP_Y2
            TrakerVertex(3)       = TrackerY2_SHIFT(3)
         else if(TrakerTEMP_Y1.eq.0.and.TrakerTEMP_Y2.eq.0)then
            TrakerVertex(2)       = 0
            TrakerVertex(3)       = TrackerY2_SHIFT(3)
         endif
         if(Z_trc(trc_hx(inum),inum).ne.0)then
            TrakerVertex(1)          = TrakerVertex(1)*TrakerVertex(3)/Z_trc(trc_hx(inum),inum) 
         endif
         call ROTATE(TrakerVertex,0.,-Bigcal_SHIFT(4)*3.1415926536/180.,0.,TrakerVertex_r)
c         WRITE(*,*)1,TrakerVertex,TrakerVertex_r
         Tr_Vertex(1,inum)        = TrakerVertex(1)
         Tr_Vertex(2,inum)        = TrakerVertex(2)
         Tr_Vertex(3,inum)        = TrakerVertex(3)
         
         Tr_Vertex_r(1,inum)        = TrakerVertex_r(1)
         Tr_Vertex_r(2,inum)        = TrakerVertex_r(2)
         Tr_Vertex_r(3,inum)        = TrakerVertex_r(3)
           call NANcheckF(Tr_Vertex(1,inum),33)
           call NANcheckF(Tr_Vertex(2,inum),33)
           call NANcheckF(Tr_Vertex(3,inum),33)
           call NANcheckF(Tr_Vertex_r(1,inum),33)
           call NANcheckF(Tr_Vertex_r(2,inum),33)
           call NANcheckF(Tr_Vertex_r(2,inum),33)

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
            call NANcheckF(Delta_Y(inum),4)
            call NANcheckF(Delta_X(inum),4)
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
      if(cer_h(inum).gt.0.and.
     ,     E_clust(inum).gt.1.and.
     ,     E_clust(inum).lt.GEBeam)then
         thetar= theta*deg2rad
         phir= phi*deg2rad
         
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
         call NANcheckF(shelicity,5)
         
         if(ihistnum.gt.0.and.ihistnum.lt.5)then
            call HF1(10600+ihistnum,X_Bjorken(inum),shelicity)
            call HF1(10610+ihistnum,X_Bjorken(inum),1.)
         endif
         
         call HFILL(10620,X_Bjorken(inum),Q2(inum),1.)
         call HFILL(10621,W2(inum),Q2(inum),1.)
      endif

      end

      subroutine correct_coordinate(x,y,z,e)
      IMPLICIT NONE 
c
c     Subroutine corrects coordinates 
c     z- should be 345 when bigcal located at 335 (10 cm for shower)
c
c      
c      cor1angle = 0.78-0.2/E-0.3/E**2-0.1/E**3
      real*4 x,y,z,e,d,cor1angle,phi, th,dd1
      d = sqrt(x**2+y**2+z**2)
      cor1angle = 1.1178+1.1876/E-2.379/E**2+0.50396/E**3
c      cor1angle = (2.120-1.034/E-0.6468/E**2+0.109/E**3)/2.
      phi = atan2(y/d,x/d)
      th  = acos(z/d)*180/3.141
c      write(*,*)'Angle Corr',th,cor1angle*sqrt(x**2+y**2)/61.
      dd1 = z*tan((th-cor1angle*sqrt(x**2+y**2)/61.)*3.141/180)
      x   = dd1*cos(phi)
      y   = dd1*sin(phi)
      d   = sqrt(x**2+y**2+z**2)
      end
