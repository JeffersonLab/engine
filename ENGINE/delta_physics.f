      subroutine delta_physics(ebeam,mp,e_elec,spvec,e_proton,hpvec,w,q2,coscm,phicm)
      IMPLICIT NONE
      real*4 ebeam,mp,q2,w,e_elec,e_proton
      real*4 spvec(4),hpvec(4)
      real*4 pxm,pym,pzm,e_miss,mm2
      real*4 z_b(4),pb,z_e(4),pe,z_p(4),pp,z_mis(4),z_q(4),z_w(4),pq,pw
      real*4 gamma,beta,cost,ecm,pcm,pl,coscm,phicm
      real*4 vdotm,vangle,tnorm(4)
c
c      E_ELEC=sqrt(spvecx**2+spvecy**2+spvecz**2)
c      Q2 = (EBEAM-SPVECZ)**2+SPVECY**2+SPVECX**2-
c     *     (EBEAM-E_ELEC)**2
c      W = -Q2+2.*(EBEAM-E_ELEC)*MP+MP**2
c      IF(W.GT.0.8) THEN
c         W=SQRT(W)
         PXM=hpvec(2)+spvec(2)
         PYM=hpvec(3)+spvec(3)
         PZM=ebeam-hpvec(4)-spvec(4)
         e_miss=ebeam+mp-e_elec-e_proton
         MM2=e_miss**2-pxm**2-pym**2-pzm**2


      Z_B(1) = 0.
      Z_B(2) = 0.
      Z_B(3) = EBEAM
      Z_B(4) = EBEAM
      PB     = EBEAM

      Z_E(1) = spvec(2)
      Z_E(2) = spvec(3)
      Z_E(3) = spvec(4)
      Z_E(4) = E_ELEC
      PE     = E_ELEC

      Z_P(1) = hpvec(2)
      Z_P(2) = hpvec(3)
      Z_P(3) = hpvec(4)
      Z_P(4) = E_PROTON
      PP     = SQRT(E_PROTON-MP**2)

      Z_MIS(1) = hpvec(2)+spvec(2)
      Z_MIS(2) = hpvec(3)+spvec(3)
      Z_MIS(3) = ebeam-hpvec(4)-spvec(4)
      Z_MIS(4) = E_MISS

      Z_Q(1)   = -Z_E(1) 
      Z_Q(2)   = -Z_E(2) 
      Z_Q(3)   =  Z_B(3)-Z_E(3) 
      Z_Q(4)   =  Z_B(4)-Z_E(4)
      PQ       =  SQRT(Z_Q(4)**2+Q2)

      Z_W(1)   = Z_Q(1) 
      Z_W(2)   = Z_Q(2) 
      Z_W(3)   = Z_Q(3)
      Z_W(4)   = SQRT(PQ**2+W**2)
      PW       = PQ

      GAMMA = Z_W(4)/W
      BETA  = PW/Z_W(4)
      COST  =(Z_W(1)*Z_P(1)+Z_W(2)*Z_P(2)+Z_W(3)*Z_P(3))/(PW*PP)
      ECM   = GAMMA*(Z_P(4)-BETA*PP*COST)
      PCM   = SQRT(ECM**2-MP**2)
      PL    = GAMMA*(PP*COST-BETA*Z_P(4))
      IF(PCM.LE.ABS(PL) .OR. ECM.LT.MP**2) THEN
          COSCM=-1.1
      ELSE
c          COSCM = SQRT(PCM**2-PL**2)/PCM
           COSCM = PL/PCM
      ENDIF

      CALL crossm(Z_Q,Z_B,TNORM)
      if (vdotm(tnorm,z_p,3).ge.0) then 
         phicm=vangle(Z_Q,Z_B,Z_Q,Z_P)
      else
         phicm=2.*3.1415-vangle(Z_Q,Z_B,Z_Q,Z_P)
      endif

c    endif
      return
      end

       subroutine crossm(a,b,c)
       real a(4),b(4),c(4)
       c(1)=a(2)*b(3)-a(3)*b(2)
       c(2)=a(3)*b(1)-a(1)*b(3)
       c(3)=a(1)*b(2)-a(2)*b(1)
       return
       end
c   
       real function vangle(a,b,c,d)
       real a(4),b(4),c(4),d(4),xm,ym,vcos
       real x(4),y(4),pi
       pi=acos(-1.0)
       call crossm(a,b,x)
       call crossm(c,d,y)
       xm=vdotm(x,x,3)
       ym=vdotm(y,y,3)
       if(xm.gt.0.0 .and. ym.gt.0.0) then
         vcos=vdotm(x,y,3)/sqrt(xm)/sqrt(ym)
         if(abs(vcos).lt.1.0) then
            vangle=acos(vcos)
         else
            if(vcos.ge.1.0)  vangle=0
            if(vcos.le.-1.0)  vangle=pi
         endif 
       else
         vangle=0
       endif
       return
       end
c
       real function vdotm(a,b,n)
       real a(n),b(n),s
       integer i,n
       s=0.0
       do i=1,3
         s=s+a(i)*b(i)
       enddo
       if(n.eq.4) s=s-a(n)*b(n)
       vdotm=s
       return
       end
