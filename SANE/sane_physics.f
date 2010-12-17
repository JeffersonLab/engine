      subroutine Bigcal_Betta(inum)
      IMPLICIT NONE
      include 'bigcal_data_structures.cmn'
      include 'sane_data_structures.cmn'
      include 'b_ntuple.cmn'
      include 'sane_ntuple.cmn'
      include 'gen_data_structures.cmn'
      include 'gen_run_info.cmn'
      integer inum
      real*4 Vector(3),Vector_r(3)
      real*4  coorX, coorY
      real*4 coorX1,coorX3,coorY1,coorY3
      real*8 Eb,theta_big, phi_big!,shift,ccx,ccy,ccz, emax,jmax
      
      common/FAKEBIG/Eb,theta_big, phi_big
      integer ixmax,iymax
c      real Etot3pm,Etot3mp,Etot3mm,Etot3pp,Etot,etot9
      real xmomsqr,xmom,xmomsq,ymomsqr,ymom,ymomsq
      real Emt,Etot,etot9,xx(5,5),yy(5,5),eyx(5,5)
*      double precision VectorN(12)
      double precision VectorN(27)
      double precision COORE,coorX2,coorY2
      integer jmax
      real emax
      common/EBL/jmax,emax
      double precision FixX,FixY,FixE
      common/SNEU/FixX,FixY,FixE
      real temp_phi,temp_th
      double precision sane_n100xye      


c
c     Call for NeuralParam subroutine which prepares the input parameters for Neural network
c
c      

      call       NueralParam(inum,ixmax,iymax,jmax,etot,
     ,     XX,YY,Eyx)    

*          call NueralParam(inum,Emax,Emt,Etot9,Etot,
*     ,        xmomsqr,xmom,xmomsq,ymomsqr,ymom,ymomsq,
*     ,        ixmax,iymax,jmax,XX,YY)    
      
c
c     Set the input vector for Neural Network
c     25 -energies from 5x5 cluster
c     and position of max energy cluster
c

 
         VectorN(1)   = eyx(1,1)
         VectorN(2)   = eyx(2,1)
         VectorN(3)   = eyx(3,1)
         VectorN(4)   = eyx(4,1)
         VectorN(5)   = eyx(5,1)
         VectorN(6)   = eyx(1,2)
         VectorN(7)   = eyx(2,2)
         VectorN(8)   = eyx(3,2)
         VectorN(9)   = eyx(4,2)
         VectorN(10)  = eyx(5,2)
         VectorN(11)  = eyx(1,3)
         VectorN(12)  = eyx(2,3)
         VectorN(13)  = eyx(3,3)
         VectorN(14)  = eyx(4,3)
         VectorN(15)  = eyx(5,3)
         VectorN(16)  = eyx(1,4)
         VectorN(17)  = eyx(2,4)
         VectorN(18)  = eyx(3,4)
         VectorN(19)  = eyx(4,4)
         VectorN(20)  = eyx(5,4)
         VectorN(21)  = eyx(1,5)
         VectorN(22)  = eyx(2,5)
         VectorN(23)  = eyx(3,5)
         VectorN(24)  = eyx(4,5)
         VectorN(25)  = eyx(5,5)

         VectorN(26) = DBLE(ixmax)
         VectorN(27) =DBLE(iymax)
      


c
c     sane_n100xye(VectorN,n) -is neural network function 
c     if n=0 -correction for x
c        n=1 -correction for y
c        n=2 -correction for energy
c

            COORX2 = sane_n100xye(VectorN,0)
            COORY2 = sane_n100xye(VectorN,1)
            COORE  = sane_n100xye(VectorN,2)
      

c
c     Calculations of final coordinate and energy of the cluster at z=335
c
c

      x_clust(inum) = xcell(jmax,inum)+coorX2
      Y_clust(inum) = ycell(jmax,inum)+coorY2
      xclust(inum) = x_clust(inum)
      yclust(inum) = Y_clust(inum)

c      write(*,*)iycell(jmax,inum),ycell(jmax,inum),y_clust(inum)


      if(ncellclust(inum).gt.6)then
         call HFILL(10227,y_clust(inum),Y_clust(inum)-yclust(inum),1.)
      endif
      Z_clust(inum) = Bigcal_SHIFT(3) !to be 335 cm 
      E_clust(inum) = Etot+COORE
      eclust(inum)  = e_clust(inum)


c
c     Obtaining the coordinates in Lab system
c
c
      Vector(1)         = x_clust(inum)
      Vector(2)         = y_clust(inum)
      Vector(3)         = Z_clust(inum)
      call ROTATE(Vector,0.,-Bigcal_SHIFT(4)*3.1415926536/180.,0.,Vector_r)

      X_clust_r(inum) =  Vector_r(1)
      Y_clust_r(inum) =  Vector_r(2)
      Z_clust_r(inum) =  Vector_r(3)
c
c     Cheching for NAN
c
c
      call NANcheckF(E_clust(inum),33)
      call NANcheckF(x_clust(inum),33)
      call NANcheckF(y_clust(inum),33)
      call NANcheckF(Z_clust(inum),33)
      call NANcheckF(X_clust_r(inum),33)
      call NANcheckF(Y_clust_r(inum),33)
      call NANcheckF(Z_clust_r(inum),33)





c
c     Calling cerenkov for info
c
c  
CORRECT_ANGLES(X,Y,Z,EE,TH,PHI,cer_stat,srx,sry)
    
      call icer(inum)

cc
cc     OBTAIN Angles THeta and Phi Assuming the particle was Electron
c     Angles are in degree
      if(cer_h(inum).gt.0)then
c
c     If cerenkov fired use electron corrections
c
         
         call CORRECT_ANGLES(
     ,        X_clust_r(inum),
     ,        Y_clust_r(inum)-Bigcal_SHIFT(2),
     ,        Z_clust_r(inum),E_clust(inum),
     ,        SANE_IF_ELECTRON_ANGLE_THETA,
     ,        SANE_IF_ELECTRON_ANGLE_PHI,1,slow_rast_x,slow_rast_y)
      else
c
c     else use photon corrections
c
c         
         call CORRECT_ANGLES(
     ,        X_clust_r(inum),
     ,        Y_clust_r(inum)-Bigcal_SHIFT(2),
     ,        Z_clust_r(inum),E_clust(inum),
     ,        SANE_IF_ELECTRON_ANGLE_THETA,
     ,        SANE_IF_ELECTRON_ANGLE_PHI,0,slow_rast_x,slow_rast_y)
         
      endif
c
c     Fill and check for NAN  theta_e and phi_e ntuple variables
c
c

      Theta_e(inum) = SANE_IF_ELECTRON_ANGLE_THETA
      Phi_e(inum) = SANE_IF_ELECTRON_ANGLE_PHI
      

      call NANcheckF(Theta_e(inum),33)
      call NANcheckF(Phi_e(inum),33)
c                           write(*,*)inum,x_clust(inum),xclust(inum),ixcell(jmax,inum),xcell(jmax,inum)
      
      
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
      real*4 X_trc_r(20,maxcl)
      real*4 Y1_trc_r(20,maxcl)
      real*4 Y2_trc_r(20,maxcl)
      real*4 Z_trc_r(20,maxcl)
      real*4 Z1_trc_r(20,maxcl)
      real*4 Z2_trc_r(20,maxcl)
      integer x1t_stat,y1t_stat,y2t_stat
      integer i,inum
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
c      write(*,*)'Tracker Start X',nclust
c      write(*,*)ixcell(1,inum),iycell(1,inum),ncellclust(inum)

c
c     Do the tracking only for particles wth cerenkov
c
c
      if(cer_h(inum).ge.0 .and. i4 .gt. 0)then
         do i=1,x1t_hit
c
c     for Some runs part of bigcal had timing shift. if this shift is there then correct for it
c
c
            x1t_tdc(i)=x1t_tdc(i)-BIG_TIME_SHIFT_CH(i4)
c     
c     Very wide geometrical cut
c     
            cut1 = .FALSE.
            cut1 = abs(x1t_tdc(i)-TRACKER_SANE_XCALIBRATION(x1t_row(i))).lt.
     ,           TRACKER_SANE_XSIGMA(x1t_row(i))
            cut2 = .FALSE.
c            
c     Define cuts for tracker
c
c
            if(BIG_TIME_SHIFT_CH(9).ne.0)then
               cut2 = abs(x1t_tdc(i)-BIG_TIME_SHIFT_CH(9)-TRACKER_SANE_XCALIBRATION(x1t_row(i))).lt.
     ,           TRACKER_SANE_XSIGMA(x1t_row(i))
            endif
c                  call HFILL(10103,float(x1t_row(i)),float(x1t_tdc(i)),1.)
            if(cut1.or.cut2)then
               call HFILL(10106,x1t_x(i)-TrackerX_SHIFT(1),TrackerX_SHIFT(3)/Bigcal_SHIFT(3)*
     ,              (xclust(inum))-Bigcal_SHIFT(1),1.)
               if(cer_h(inum).gt.0)then
                  call HFILL(10103,float(x1t_row(i)),float(x1t_tdc(i)),1.)
               endif
            endif
c
c     Apply the riming cuts with geometrical cut
c
c
            
            IF(abs(x1t_x(i)-TrackerX_SHIFT(1)-
     ,           TrackerX_SHIFT(3)/(Bigcal_SHIFT(3))*
     ,           (x_clust(inum)-Bigcal_SHIFT(1)) ).lt.2.)then
               
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
c      write(*,*)'Tracker Start Y1'
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
            if(BIG_TIME_SHIFT_CH(9).ne.0)then
               cut2 = abs(y1t_tdc(i)-BIG_TIME_SHIFT_CH(9)-TRACKER_SANE_Y1CALIBRATION(y1t_row(i))).lt.
     ,              TRACKER_SANE_Y1SIGMA(y1t_row(i))
            endif
            
            if(cut1.or.cut2)then
               call HFILL(10107,y1t_y(i)-TrackerY1_SHIFT(2),TrackerY1_SHIFT(3)/Bigcal_SHIFT(3)*
     ,              (y_clust(inum))-Bigcal_SHIFT(2),1.)
               if(cer_h(inum).gt.0)then
                  call HFILL(10104,float(y1t_row(i)),float(y1t_tdc(i)),1.)
               endif
            endif

            IF(abs(y1t_y(i)-TrackerY1_SHIFT(2)-
     ,           TrackerY1_SHIFT(3)/(Bigcal_SHIFT(3))*
     ,           (y_clust(inum)-Bigcal_SHIFT(2))).lt.2.)then
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
c      write(*,*)'Tracker Start Y2'
         do i=1,y2t_hit
            y2t_tdc(i)=y2t_tdc(i)-BIG_TIME_SHIFT_CH(i4)
c     
c     Very wide geometrical cut
c     
            cut1 = .FALSE.
            cut1 = abs(y2t_tdc(i)-TRACKER_SANE_Y2CALIBRATION(y2t_row(i))).lt.
     ,           TRACKER_SANE_Y2SIGMA(y2t_row(i))
            cut2 = .FALSE.
            if(BIG_TIME_SHIFT_CH(9).ne.0)then
               cut2 = abs(y2t_tdc(i)-BIG_TIME_SHIFT_CH(9)-TRACKER_SANE_Y2CALIBRATION(y2t_row(i))).lt.
     ,              TRACKER_SANE_Y2SIGMA(y2t_row(i))
            endif
            
            if(cut1.or.cut2)then
               call HFILL(10108,y2t_y(i)-TrackerY2_SHIFT(2),TrackerY2_SHIFT(3)/Bigcal_SHIFT(3)*
     ,              (y_clust(inum))-Bigcal_SHIFT(2),1.)
               call HFILL(10109,float(y2t_row(i)),TrackerY2_SHIFT(3)/Bigcal_SHIFT(3)*
     ,              (y_clust(inum))-Bigcal_SHIFT(2),1.)
               if(cer_h(inum).gt.0)then
                  call HFILL(10105,float(y2t_row(i)),float(y2t_tdc(i)),1.) 
               endif
            endif
c            write(*,*)y2t_tdc(i),TRACKER_SANE_Y2CALIBRATION(y2t_row(i)),TRACKER_SANE_Y1SIGMA(y1t_row(i))
            IF(abs(y2t_y(i)-TrackerY2_SHIFT(2)-
     ,           TrackerY2_SHIFT(3)/(Bigcal_SHIFT(3))*
     ,           (y_clust(inum)-Bigcal_SHIFT(2))).lt.2.)then
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
      integer jmax
      real emax
      common/EBL/jmax,emax

      integer inum,i,ibar
      integer luc,ip
      real*4 tdc_dif, xDelta1,koef,yfake,ytracker,ztracker,ipi(10)
      real*4 Vector(3),Vector_r(3),Zbigclust,xattrack,yattrack
      integer i4,itrig
      logical cut,cut1,cut2
      common /B_TIME_SHIFT/i4,itrig
      luc=0
      ip=0
c
c     calculations are done if cherenkov triggered.
c
c
     
      if(luc_hit.gt.0
     ,     .and.cer_h(inum).ge.0
     ,     .and. i4 .gt. 0)then
         do i=1,luc_hit
            ltdc_pos(i) = ltdc_pos(i) -BIG_TIME_SHIFT_CH(i4)!-BIG_TIME_SHIFT_CH(itrig)
            ltdc_neg(i) = ltdc_neg(i) -BIG_TIME_SHIFT_CH(i4)!-BIG_TIME_SHIFT_CH(itrig)
            
c
c
c     Define cuts for timing for lucite
c
c
            cut =abs(ltdc_pos(i)-LUCITE_SANE_MEAN_POS(luc_row(i))).lt.
     ,           3*LUCITE_SANE_SIGMA_POS(luc_row(i)).and.
     ,           abs(ltdc_neg(i)-LUCITE_SANE_MEAN_NEG(luc_row(i))).lt.
     ,           3*LUCITE_SANE_SIGMA_NEG(luc_row(i))
            cut1 = .FALSE.
            cut2 = .FALSE.
            if(BIG_TIME_SHIFT_CH(5).ne.0)then
               cut =(abs(ltdc_pos(i)-LUCITE_SANE_MEAN_POS(luc_row(i))).lt.
     ,              3.*LUCITE_SANE_SIGMA_POS(luc_row(i)).and.
     ,              abs(ltdc_neg(i)-LUCITE_SANE_MEAN_NEG(luc_row(i))).lt.
     ,              3.*LUCITE_SANE_SIGMA_NEG(luc_row(i))).or.
     ,              (abs(ltdc_pos(i)-BIG_TIME_SHIFT_CH(5)-LUCITE_SANE_MEAN_POS(luc_row(i))).lt.
     ,              3.*LUCITE_SANE_SIGMA_POS(luc_row(i)).and.
     ,              abs(ltdc_neg(i)-BIG_TIME_SHIFT_CH(5)-LUCITE_SANE_MEAN_NEG(luc_row(i))).lt.
     ,              3.*LUCITE_SANE_SIGMA_NEG(luc_row(i)))

               cut1 =             (abs(ltdc_pos(i)-LUCITE_SANE_MEAN_POS(luc_row(i))).lt.
     ,              3.*LUCITE_SANE_SIGMA_POS(luc_row(i)).and.
     ,              abs(ltdc_neg(i)-BIG_TIME_SHIFT_CH(5)-LUCITE_SANE_MEAN_NEG(luc_row(i))).lt.
     ,              3.*LUCITE_SANE_SIGMA_NEG(luc_row(i)))
               cut2 =
     ,              (abs(ltdc_pos(i)-BIG_TIME_SHIFT_CH(5)-LUCITE_SANE_MEAN_POS(luc_row(i))).lt.
     ,              3.*LUCITE_SANE_SIGMA_POS(luc_row(i)).and.
     ,              abs(ltdc_neg(i)-LUCITE_SANE_MEAN_NEG(luc_row(i))).lt.
     ,              3.*LUCITE_SANE_SIGMA_NEG(luc_row(i)))
            endif

c
c     Apply cuts and find the x coordinate of hit
c

            If(cut.or.cut1.or.cut2
     ,           )then
               
                  call HFILL(10121, float(luc_row(i)), float(ltdc_pos(i)), 1.)
                  call HFILL(10122, float(luc_row(i)), float(ltdc_neg(i)), 1.)
                  call HFILL(10125, float(luc_row(i)), float(ladc_pos(i)), 1.)
                  call HFILL(10126, float(luc_row(i)), float(ladc_neg(i)), 1.)
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
c$$$
C     
C     Y Geometrical CUT
C     
**********
               call HFILL(10128,luc_y(i),y_clust(inum),1.)
               
               IF(abs(luc_y(i)-Lucite_SHIFT(2)-
     ,              Lucite_SHIFT(3)/(Bigcal_SHIFT(3))*
     ,              (y_clust(inum)-Bigcal_SHIFT(2))).lt.30)then

c                  if(luc_row(i).eq.18)write(*,*)xDelta1-Lucite_SHIFT(1)

                  call HFILL(10150+luc_row(i),xDelta1-Lucite_SHIFT(1),
     ,                 sqrt(Lucite_SHIFT(3)**2+luc_y(i)**2)/sqrt(Bigcal_SHIFT(3)**2+
     ,                 y_clust(inum)**2)*xclust(inum),1.)
                  call HFILL(20150+luc_row(i),(xDelta1-Lucite_SHIFT(1))*
     ,                 Bigcal_SHIFT(3)/Lucite_SHIFT(3),
     ,                 Xclust(inum),1.)
c     endif
      if(abs(xclust(inum)).lt.5)then
                  call HFILL(10131, float(luc_row(i)), float(ltdc_pos(i)), 1.)
                  call HFILL(10132, float(luc_row(i)), float(ltdc_neg(i)), 1.)
                  call HFILL(10135, float(luc_row(i)), float(ladc_pos(i)), 1.)
                  call HFILL(10136, float(luc_row(i)), float(ladc_neg(i)), 1.)
      endif
                  ip=ip+1
                  ipi(ip) = i
C     
C     X Geometrical CUT
C     
***********
c                  write(*,*)luc_row(i)
                  If(abs(xDelta1-Lucite_SHIFT(1)-
     ,                 Lucite_SHIFT(3)/Bigcal_SHIFT(3)*
     ,                 (xclust(inum))-Bigcal_SHIFT(1)).lt.20)then
                     
c                     write(*,*)xDelta1
c

c
c     Fill lucite coordinates
c
c
                     luc = luc+1
                     X_luc(luc,inum)   = xDelta1-Lucite_SHIFT(1)
                     Y_luc(luc,inum)   = luc_y(i)-Lucite_SHIFT(2)
                     Z_luc(luc,inum)   = sqrt(Lucite_SHIFT(3)**2-
     ,                    X_luc(luc,inum)**2)
                     call NANcheckF(X_luc(luc,inum),333)
                     call NANcheckF(Y_luc(luc,inum),444)
                     call NANcheckF(Z_luc(luc,inum),555)
c
c
c     Rotate lucite coordinates into Lab system
c
                     
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
c
c
c     Some plots to look at two lucite hits at the same time
c
c
         if(luc_h(inum).eq.2)then
c            write(*,*)X_luc(1,inum),X_luc(2,inum)
             if(abs(Y_luc(1,inum)-Y_luc(2,inum)).lt.7.and.e_clust(inum).gt.0.7.and.
     ,           abs(X_luc(1,inum)-X_luc(2,inum)).lt.5.and.ncellclust(inum).gt.6)then
c               write(32,*)X_luc(1,inum),X_luc(2,inum),luc_row(ipi(1)),luc_row(ipi(2))
c           write(*,*)luc_h(inum)
            
c
c     GOOD LUCITE HIT
c
               X_luc_av(inum) =  (X_luc(1,inum)+X_luc(2,inum))/2.
               Z_luc_av(inum) =  (Z_luc(1,inum)+Z_luc(2,inum))/2.
               Y_luc_av(inum) =  (Y_luc(1,inum)+Y_luc(2,inum))/2.
               ibar = (Y_luc_av(inum)+82.35)/6.1+1
               
c               write(*,*)ibar
               Y_luc_av(inum) =  Y_luc_av(inum)-3.5/2.*Y_luc_av(inum)/Z_luc_av(inum)
c                write(*,*)'   ',Y_luc_av(inum)S
              
               if(abs(x_clust(inum)).lt.103)then
                  if((trc_hy1(inum).eq.1.and.trc_hy2(inum).eq.1.and.trc_hx(inum).eq.1))then
c                  write(*,*)trc_hy1(inum),trc_hy2(inum),trc_hx(inum)
                     ytracker = 0
                     
                     if(trc_hy2(inum).eq.1.and.trc_hy1(inum).eq.0)then
                        ytracker = Y2_trc(trc_hy2(inum),inum)
                        ztracker = TrackerY2_SHIFT(3)
                     endif
                     if(trc_hy1(inum).eq.1.and.trc_hy2(inum).eq.0)then
                        ytracker = Y1_trc(trc_hy1(inum),inum)
                        ztracker = TrackerY1_SHIFT(3)
                     endif
                     if(trc_hy2(inum).eq.1.and.trc_hy1(inum).eq.1.and.
     ,                    abs(Y2_trc(trc_hy2(inum),inum)-Y1_trc(trc_hy1(inum),inum)).lt.0.4)then
c                        write(*,*)Y2_trc(trc_hy2(inum),inum),Y1_trc(trc_hy1(inum),inum)
                        ytracker = 0.5*(Y2_trc(trc_hy2(inum),inum)+Y1_trc(trc_hy1(inum),inum))
                        ztracker = 0.5*(TrackerY2_SHIFT(3)+TrackerY1_SHIFT(3))

                     endif
                     if(ytracker.ne.0.and.e_clust(inum).gt.1)then
                        
                        xattrack = X_luc_av(inum)*TrackerX_SHIFT(3)/Z_luc_av(inum)
                        yattrack = y_luc_av(inum)*ztracker/Z_luc_av(inum)
                        
                        
                        koef = (Y_luc_av(inum)-ytracker)/(Z_luc_av(inum)-ztracker)
                        yfake = koef*(Bigcal_SHIFT(3)-ztracker-10)+ytracker
                        Zbigclust = (y_clust(inum)-ytracker)/koef+ztracker-Bigcal_SHIFT(3)

                        call HFILL(10220,Yfake,yfake-y_clust(inum),1.)
                        call HFILL(10221,yfake,Y_luc_av(inum)*Bigcal_SHIFT(3)/Z_luc_av(inum)-yclust(inum),1.)
                        call HFILL(10225,eclust(inum),yfake-y_clust(inum),1.)
                        call HFILL(20250+ibar,xcell(jmax,inum),ycell(jmax,inum),1.)
                        if(yfake.gt.50)then
                           call HFILL(10222,xclust(inum),(yfake-y_clust(inum)),1.)
                        endif
                     endif
                  endif
               endif
               if(abs(Y_Clust(inum)).lt.10)then
                  call HFILL(10223,X_luc_av(inum)*(Bigcal_SHIFT(3))/Z_luc_av(inum)-X_clust(inum),X_clust(inum),1.)
                  call HFILL(10226,X_luc_av(inum)*(Bigcal_SHIFT(3))/Z_luc_av(inum)-x_clust(inum),eclust(inum),1.)
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
c     Cerenkov
c
ccccccccccc
      Subroutine  icer(inum)
      IMPLICIT NONE
      include 'bigcal_data_structures.cmn'
      include 'sane_data_structures.cmn'
      include 'b_ntuple.cmn'
      include 'sane_ntuple.cmn'
      include 'gen_data_structures.cmn'
      include 'gen_run_info.cmn'
      integer inum,i,j,cer_n
      integer i4,itrig,itrigcellX,itrigcellY
      real T_trgBIG_CUT_U,T_trgBIG_CUT_D
      common /B_TIME_SHIFT/i4,itrig
      real amax,dtime
      real Brow,a64c
      integer iBnum,ibcol,ibrow
c      integer BIGCAL_CER_330_SHIT(8)
c      data BIGCAL_CER_330_SHIT/0,-3,1,1,3,3,1,3/
      
      cer_h(inum)=0
      cer_geom(inum)=0
               cerb_time(inum)=0
               cerb_adc(inum) = 0
      
      if(ncellclust(inum).gt.4)then
         i4=1
         itrig = 1
         amax = ablock(1,inum)
         
         if(iycell(1,inum).gt.32) then
            i4=2
         else 
            i4=1
         endif
         T_trgBIG_CUT_U=50
         T_trgBIG_CUT_D=30
         if(grun.le.72400)then  !!!!!!!!!!!!!! NEEDS TO BE CHANGED TO CORRECT ONE         
            T_trgBIG_CUT_U=55
            T_trgBIG_CUT_D=34
         endif
c         itrig = 3
         
c     write(*,*)"Amax= ",amax,ncellclust(inum)
c     do k=1,ncellclust(inum)
c     if(ablock(k,inum).ge.amax)then
c     
c            amax = ablock(k,inum)
c     i4 = iycell(k,inum)
c     write(*,*)"i4=",i4,k,iycell(k,inum)
c     endif
c     enddo
         do i=1, min(cer_hit,50)
            if(cer_num(i).lt.9 .and. ncell64clust(inum) .gt. 0)then
               cer_n = cer_num(i)
c                  write(*,*)1,cer_tdc(i),cer_adcc(i),cer_num(i)

               
                 iBnum = 10700+icol64hit(ncell64clust(inum),inum)*10+cer_num(i)
                 Brow=float( irow64hit(ncell64clust(inum),inum) )               
                 call HF2( iBnum,Brow,float(cer_tdc(i))+2090,1.)
              if(grun.le.72487)then
              
                 if(abs(cer_tdc(i)+1678).lt.60)cer_tdc(i)=100000
                 
                 if(grun.le.72487.and.icol64hit(ncell64clust(inum),inum).eq.1.and.
     ,                abs(BIGCAL_CER_COL1_SHIFT(irow64hit(ncell64clust(inum),inum),cer_num(i))).lt.0.0001)cer_tdc(i)=100000
                 if(grun.le.72487.and.icol64hit(ncell64clust(inum),inum).eq.2.and.
     ,              abs(BIGCAL_CER_COL2_SHIFT(irow64hit(ncell64clust(inum),inum),cer_num(i))).lt.0.0001)cer_tdc(i)=100000
                 
                 if(icol64hit(ncell64clust(inum),inum).eq.1)
     ,                cer_tdc(i) = cer_tdc(i) -BIGCAL_CER_COL1_SHIFT(irow64hit(ncell64clust(inum),inum),cer_num(i))
                 if(icol64hit(ncell64clust(inum),inum).eq.2)
     ,                cer_tdc(i) = cer_tdc(i) -BIGCAL_CER_COL2_SHIFT(irow64hit(ncell64clust(inum),inum),cer_num(i))
                 
                 iBnum = 10730+cer_num(i)
                 Brow=float( irow64hit(ncell64clust(inum),inum) )               
                 call HF2( iBnum,Brow,float(cer_tdc(i))+2090,1.)
              endif

ccccccccccccccccccccccccccc
 
               if(y_clust(inum).gt.(CER_SANE_GEOM_CUT_LOW(cer_n)*4-1)-120..and.
     ,              y_clust(inum).lt.(CER_SANE_GEOM_CUT_HI(cer_n)*4+1)-120.and.
     ,              CER_SANE_GEOM_CUT_X(cer_n)*x_clust(inum).gt.-20)then
                  if(T_trgBIG.gt.T_trgBIG_CUT_D)then
c                     write(*,*)T_trgBIG
                     call HFILL(10560+cer_num(i),T_trgBIG,float(cer_tdc(i)),1.)
                     call HFILL(10570+cer_num(i),T_trgBETA,float(cer_tdc(i)),1.)
                  endif
               endif

c
c
c     Trigger SHIFT COrrections
c
c
               if(int(T_trgBETA-45).gt.0.and. int(T_trgBETA-45) .le. 30)then
                  cer_tdc(i) = cer_tdc(i) -T_TRGBETA_SHIFT(int(T_trgBETA-45))
               endif
               if(grun.le.72487)then
                  
                  if(T_trgBIG.gt.42.and.cer_num(i).ne.5)cer_tdc(i)=100000
                  
               endif



c     write(*,*)cer_n,CER_SANE_GEOM_CUT_LOW(cer_n),CER_SANE_GEOM_CUT_HI(cer_n)
               if(y_clust(inum).gt.(CER_SANE_GEOM_CUT_LOW(cer_n)-1)*4-120..and.
     ,              y_clust(inum).lt.(CER_SANE_GEOM_CUT_HI(cer_n)+1)*4-120.and.
     ,              CER_SANE_GEOM_CUT_X(cer_n)*x_clust(inum).gt.-20.and.
     ,              T_trgBIG.ge.T_trgBIG_CUT_D.and.T_trgBIG.le.T_trgBIG_CUT_U)then

c               write(*,*)1,2,cer_tdc(i),cer_adcc(i),cer_num(i)
                     call HFILL(10580+cer_num(i),T_trgBIG,float(cer_tdc(i)),1.)
 
                  
                  if(aclust(inum).gt.500)then
                     call HFILL(10520+cer_num(i),aclust(inum),float(cer_tdc(i)),1.)
                     if(grun.le.72487)then !!!!!!!!!!!!!! NEEDS TO BE CHANGED TO CORRECT ONE
                        cer_tdc(i) = cer_tdc(i) - 
     ,                       (BIGCAL_CER_TIME_WALK_SHIFT(cer_num(i))-
     ,                       BIGCAL_CER_TIME_WALK_SLOPE(cer_num(i))/aclust(inum)-
     ,                       CER_SANE_TIME_WALK(cer_num(i))/cer_adcc(i))
                     endif
                     
                  endif
c                  write(*,*)cer_tdc(i),cer_adcc(i),cer_num(i)
                  call HFILL(10530+cer_num(i),aclust(inum),float(cer_tdc(i)),1.)
                  call HFILL(10500+cer_num(i),float(cer_adcc(i)),float(cer_tdc(i)),1.)
                  if (ncell64clust(inum) .gt. 0) then
                  ibcol = icol64hit(ncell64clust(inum),inum)
                  ibrow = irow64hit(ncell64clust(inum),inum)
                  a64c  = s64(ncell64clust(inum),inum)
                  iBnum = 17000+ibrow+100*ibcol
                  if(iBnum.eq.106)write(*,*)iBnum,a64c,cer_tdc(i)
                  call HF2(iBnum,a64c,float(cer_tdc(i)),1.)
                  endif
                  
                  
               endif

               bigc_time(inum) = tclust64(inum)!-(5000/s64(ncell64clust(inum),inum)-4)
                   if (ncell64clust(inum) .gt. 0) then
              if(grun.gt.72487.and.icol64hit(ncell64clust(inum),inum).eq.1.and.
     ,              abs(BIGCAL_CERB_COL1_SHIFT(irow64hit(ncell64clust(inum),inum),cer_num(i))).lt.0.0001)bigc_time(inum)=100000
               if(grun.gt.72487.and.icol64hit(ncell64clust(inum),inum).eq.2.and.
     ,              abs(BIGCAL_CERB_COL2_SHIFT(irow64hit(ncell64clust(inum),inum),cer_num(i))).lt.0.0001)bigc_time(inum)=100000
               
               if(icol64hit(ncell64clust(inum),inum).eq.1)
     ,              bigc_time(inum) = bigc_time(inum) -BIGCAL_CERB_COL1_SHIFT(irow64hit(ncell64clust(inum),inum),cer_num(i))
               if(icol64hit(ncell64clust(inum),inum).eq.2)
     ,              bigc_time(inum) = bigc_time(inum) -BIGCAL_CERB_COL2_SHIFT(irow64hit(ncell64clust(inum),inum),cer_num(i))
                 endif
c               write(*,*)grun,bigc_time(inum),ncell64clust(inum),inum
c               if(grun.gt.72487)then
c                  iBnum = 10700+icol64hit(ncell64clust(inum),inum)*10+cer_num(i)
c                  Brow=float( irow64hit(ncell64clust(inum),inum) )               
c                  call HF2( iBnum,Brow,float(bigc_time(inum))-330,1.)
c               endif

c               bigc_time(inum) = bigc_time(inum) -(5000/s64(ncell64clust(inum),inum)-4)

               
               bigc_adc(inum)  = s64(ncell64clust(inum),inum)
               iBnum = 18000+ibrow+100*ibcol
               call HF2(iBnum,float(bigc_adc(inum)),float(bigc_time(inum)),1.)
               if(              
     ,              T_trgBIG.ge.T_trgBIG_CUT_D.and.T_trgBIG.le.T_trgBIG_CUT_U)then
                  if(y_clust(inum).gt.(CER_SANE_GEOM_CUT_LOW(cer_n)*4-1)-120..and.
     ,                 y_clust(inum).lt.(CER_SANE_GEOM_CUT_HI(cer_n)*4+1)-120.and.
     ,                 CER_SANE_GEOM_CUT_X(cer_n)*x_clust(inum).gt.-20)then
                     cer_geom(inum)=cer_geom(inum)+1

                     if(cer_h(inum).eq.0)then
                        cerb_time(inum)=cer_tdc(i)
                        cerb_adc(inum) = cer_adcc(i)
                        cerbc_num(inum) = cer_num(i)
                       
                     endif

                     iBnum = 10740+cer_num(i)
                     Brow=float( irow64hit(ncell64clust(inum),inum) )               
                     call HF2( iBnum,Brow,float(cer_tdc(i))+2090,1.)

                     if(
     ,                    abs(cer_tdc(i)-CER_SANE_MEAN(cer_num(i))).lt.
     ,                    CER_SANE_SIGMA(cer_n)
     ,                    )then

                        cer_h(inum)=cer_h(inum)+1
                        if(cer_h(inum).eq.1)then
                           cerb_time(inum)=cer_tdc(i)
                           cerb_adc(inum) = cer_adcc(i)
                           cerbc_num(inum) = cer_num(i)*10**(cer_h(inum)-1)

                        else
                           cerb_time(inum)=cerb_time(inum)+cer_tdc(i)
                           cerb_adc(inum) = cerb_adc(inum)+cer_adcc(i)
                           cerbc_num(inum) = cer_num(i)*10**(cer_h(inum)-1)+cerbc_num(inum) 
                        endif
                           
c                        write(*,*)cer_h(inum),cer_hit,cer_n
                        call HFILL(10540+cer_num(i),float(cer_adcc(i)),float(cer_tdc(i)),1.)
                        call HF1(10113,tclust64(inum),1.)
                        call HF2(10114,tclust64(inum),float(cer_tdc(i)),1.)

                        do j=1, ncellclust(inum) 
c                           write(*,*)inum,x_clust(inum),xclust(inum),ixcell(j,inum),xcell(j,inum)
                           call HFILL(10510+cer_n,float(ixcell(j,inum)),float(iycell(j,inum)), 1.)
                        enddo
                     endif
                  endif
               endif
            endif
         enddo
         if(cer_h(inum).gt.0)cerb_time(inum)=cerb_time(inum)/float(cer_h(inum))

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
      Subroutine CORRECT_ANGLES(X,Y,Z,EE,TH,PHI,cer_stat,srx,sry)
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
      include 'gen_run_info.cmn'
      real*4 X,Y,Z,TH,Phi,thr,phr,EE 
      real*4 DIST,srx,sry
      real*8 P_th(10),P_phi(10)

      integer cer_stat
      data P_th /
     ,     -2.199987805718,      1.312318933346,      0.644032653274,      
     ,     2.001711272282  ,    4.831055345667,
     ,     0.596870277140   ,   0.237530064696 ,    -0.444891749961,
     ,     -0.668604044519  ,   -1.988327254812/
      data P_Phi /
     ,     -1.206886920591,      3.898203794202,      1.409952555564 , 
     ,     -0.737821993549,      4.693839032660,
     ,     -0.853486677346,     -3.282568717839 ,     1.891695882259,    
     ,     1.158605334109 ,    -4.578605424909/
      DIST = sqrt(X**2+Y**2+Z**2)
      thr  = acos(Z/Dist)
      phr= atan2(y/Dist,x/Dist)

      if(gen_run_number.lt.72900)then
         SANE_BETA_OMEGA=40
      else
         SANE_BETA_OMEGA=140
      endif
      if(cer_stat.gt.0)then
         call POLYNOM_CORRECTION(SANE_TRANSFORM_MATRIX_THETA, 
     ,        SANE_TRANSFORM_MATRIX_PHI,thr,
     ,        phr,EE,TH,PHI,srx,sry)
c      if(x.lt.179.and.y.gt.10)
c     .     write(*,*)thr*180/3.1415926,th,phr*180/3.1415926,phi

      else
c            phi=phr*180/3.14159
c            th = thr*180/3.14159

         DIST = sqrt((X-srx)**2+(Y-sry)**2+Z**2)
         thr  = acos(Z/Dist)
         phr= atan2((y-sry)/Dist,(x-srx)/Dist)
         th  = THR*180/3.1415926+
     ,        (P_th(1)+P_th(2)*phr+P_th(3)*thr+P_th(4)*phr**2+P_th(5)*thr**2)/EE+
     ,        (P_th(6)+P_th(7)*thr+P_th(8)*phr+P_th(9)*phr**2+P_th(10)*thr**2)/EE**2
         phi = phR*180/3.1415926
     ,        +(P_phi(1)+P_phi(2)*phr+P_phi(3)*phr**2+
     ,        P_phi(4)*phr**3+P_phi(5)*thr+P_phi(6)*thr**2+
     ,        P_phi(7)*thr**3+P_phi(8)*phr*thr+
     ,        P_phi(9)*phr**2*thr+
     ,        P_phi(10)*phr*thr**2)
      endif
  
c      write(*,*)phr*180/3.141-90,phi,SANE_BETA_OMEGA

c      write(28,*)thr*180/3.1415926,th,phr*180/3.1415926-phi,SANE_BETA_OMEGA,gen_run_number
      end
ccccccc
      Subroutine POLYNOM_CORRECTION(P_th,P_phi,thr,phr,EE,TH,PHI,srx,sry)
      IMPLICIT NONE
      
c
c     Input patameters are P(26) -transformation Matrix
c     input thr and phr angles from CALORIMETER ,THr and PHr in radians
c     EE Energy in GEV
c     Output :TH and Phi Correctes in degrees
cc      
      real*4 TH,Phi,thr,phr,EE
      REAL*8 OMEGA,srx,sry
      real*4 P_th(15),P_phi(15),COSOM,SINOM
      cosom = cos(omega*3.1415926/180.)
      sinom = sin(omega*3.1415926/180.)
c      write(*,*)P_th
      
         th  = THR*180/3.1415926+
     ,           (p_th(1)+p_th(2)*thr+p_th(3)*phr+p_th(4)*thr**2+
     ,           p_th(5)*phr**2+p_th(6)*thr*phr)*
     ,           (p_th(7)+p_th(8)/EE+p_th(9)/EE**2)*
     ,           (p_th(10)+p_th(11)*srx+p_th(12)*srx**2)*
     ,           (p_th(13)+p_th(14)*sry+p_th(15)*sry**2)
 


         phi = phR*180/3.1415926+
     ,           (p_phi(1)+p_phi(2)*thr+p_phi(3)*phr+p_phi(4)*thr**2+
     ,           p_phi(5)*phr**2+p_phi(6)*thr*phr)*
     ,           (p_phi(7)+p_phi(8)/EE+p_phi(9)/EE**2)*
     ,           (p_phi(10)+p_phi(11)*srx+p_phi(12)*srx**2)*
     ,           (p_phi(13)+p_phi(14)*sry+p_phi(15)*sry**2)

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
      integer inum
c      real*8 U, U_pos, U_neg
      real*8 dl
      real*4 P_tr(3),P_big(3),P_tar(3)
c      real*4 Vector(3),Vector_r(3)
c      LOGICAL ok1
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
            P_big(1) = x_clust(inum)
            P_big(2) = y_clust(inum)
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
      ENue(inum)      = 0
      Q2(inum)        = 0
      X_Bjorken(inum) = 0
      W2(inum)        = 0
      if(cer_h(inum).ge.0.and.
     ,     E_clust(inum).gt.0.6.and.
     ,     E_clust(inum).lt.GEBeam)then
         thetar= theta*deg2rad
         phir= phi*deg2rad
         
         ENue(inum)      = GEBeam - E_clust(inum)
         Q2(inum)        = 2*GEBeam*E_clust(inum)*(1-cos(thetar))
         X_Bjorken(inum) = Q2(inum)/( 2*Mp*ENue(inum) )
         W2(inum)        = Mp**2 + 2*Mp*ENue(inum) -Q2(inum)
         ihistnum        = (Q2(inum)-2.5)+1
c        WRITE(*,*)cer_h(inum),E_clust(inum),X_Bjorken(inum),theta,(1-cos(thetar))
         
         call NANcheckF(ENue(inum),5)
         call NANcheckF(Q2(inum),5)
         call NANcheckF(X_Bjorken(inum),5)
         call NANcheckF(W2(inum),5)
c         shelicity       = i_helicity
         if(i_helicity.gt.0)then
            shelicity=1.
         else if(i_helicity.lt.0)then
            shelicity=-1.
         endif
c         write(18,'(4I13,2F10.5)')bgid,i_helicity,inum,cer_h(inum),Q2(inum),X_Bjorken(inum)
         call NANcheckF(shelicity,5)
         
         if(ihistnum.gt.0.and.ihistnum.lt.5)then
            call HF1(10600+ihistnum,X_Bjorken(inum),shelicity)
            call HF1(10610+ihistnum,X_Bjorken(inum),1.)
         endif
         
         call HFILL(10620,X_Bjorken(inum),Q2(inum),1.)
         call HFILL(10621,W2(inum),Q2(inum),1.)
      endif

      end

