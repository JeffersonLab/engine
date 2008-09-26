      subroutine Plane(P1,P2,P3,a,b,c,d)
c
c     Finds Plane constants a,b,c,d Using three points (P1,P2,P3)
c

      real*8 P1(3),P2(3),P3(3)
      real*8 a,b,c,d
      real*8 vx(3),vy(3),vz(3)


      a =  (p2(2)-p1(2))*(p3(3)-p1(3))-(p2(3)-p1(3))*(p3(2)-p1(2))
      b =  (p2(3)-p1(3))*(p3(1)-p1(1))-(p2(1)-p1(1))*(p3(3)-p1(3))
      c =  (p2(1)-p1(1))*(p3(2)-p1(2))-(p3(1)-p1(1))*(p2(2)-p1(2))
      d = -p1(1)*a-p1(2)*b-p1(3)*c
      write(*,*)a,b,c,d

      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine PlaneV(P,DOT,a,b,c,d)
c
c     Finds Plane constants a,b,c,d Using normal vector to the plane (P)
c     and point on plane (DOT) 
c
      real*8 P(3),DOT(3)
      real*8 a,b,c,d
      a = P(1) 
      b = P(2)
      c = P(3)
      d = -(P(1)*Dot(1)+P(2)*Dot(2)+P(3)*Dot(3))
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine Rotate(P,phi,th,psi,P1)
c      iflag=1 -Rotate from P system to LAB
c      iflag=-1 -Rotate from LAB system to P
      real P(3),P1(3)
      real a(3,3)
      real phi,th,psi
      integer i,j
c      write(*,*)'Angles',phi,th,psi
      if(abs(phi).lt.0.002)phi=0
      if(abs(th).lt.0.002)th=0
      if(abs(psi).lt.0.002)psi=0
c      write(*,*)'Angles',phi,th,psi
      a(1,1) = -sin(psi)*sin(phi)+cos(th)*cos(phi)*cos(psi)
      a(1,2) = -sin(psi)*cos(phi)+cos(th)*sin(phi)*cos(psi)
      a(1,3) = -sin(th)*cos(psi)
      a(2,1) = -sin(phi)*cos(psi)-cos(th)*cos(phi)*sin(psi)
      a(2,2) =  cos(phi)*cos(psi)-cos(th)*sin(phi)*sin(psi)
      a(2,3) =  sin(th)*sin(psi)
      a(3,1) =  sin(th)*cos(phi)
      a(3,2) =  sin(th)*sin(phi)
      a(3,3) =  cos(th)
c      write(*,"(3F10.3)")a
      do 100 i=1,3
         P1(i)=0
         do 200 j=1,3
            if(abs(a(i,j)).lt.1e-7)a(i,j)=0
c     write(*,*)P1(i),a(i,j)
            P1(i)=P1(i)+a(i,j)*P(j)
 200     continue
         if(abs(P1(i)).lt.1e-3)P1(i)=0.
 100  continue
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine EqVector(V1,V2)
      real*8 V1(6),V2(6)
      do i=1,6
         V2(i)=V1(i)
      enddo
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine sub6vec(V1,V2,V3)
      real*8 V1(6),V2(6),V3(6)

      do i=1,3
         v3(i)=V1(i)-V2(i)
      enddo
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine val6vec(V,val)
c
c     finds the distance from origin for 6 Vector
c
      real*8 V(6),val
      val = sqrt(V(1)**2+V(2)**2+V(3)**2)
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine mom6vec(V,val)
c
c     finds momentum of 6 vector
c      
      real*8 V(6),val
      val = sqrt(V(4)**2+V(5)**2+V(6)**2)
      end

c
ccccccccccccccccccccccccccccccccccccccccc
      subroutine CalcMomComp(xluc,yluc,zluc,xbig,ybig,zbig,px,py,pz,E,pM)
      real*4 xluc,yluc,zluc,xbig,ybig,zbig,px,py,pz
      real*4 dist,E,pM
      dist = sqrt((xluc-xbig)**2+(yluc-ybig)**2+(zluc-zbig)**2)
      px   = sqrt(E**2-pm**2)*(xluc-xbig)/dist
      py   = sqrt(E**2-pm**2)*(yluc-ybig)/dist
      pz   = sqrt(E**2-pm**2)*(zluc-zbig)/dist

      end
