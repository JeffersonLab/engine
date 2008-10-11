      subroutine Plane(P1,P2,P3,a,b,c,d)
      IMPLICIT NONE
c
c     Finds Plane constants a,b,c,d Using three points (P1,P2,P3)
c

      real*4 P1(3),P2(3),P3(3)
      real*8 a,b,c,d


      a =  (p2(2)-p1(2))*(p3(3)-p1(3))-(p2(3)-p1(3))*(p3(2)-p1(2))
      b =  (p2(3)-p1(3))*(p3(1)-p1(1))-(p2(1)-p1(1))*(p3(3)-p1(3))
      c =  (p2(1)-p1(1))*(p3(2)-p1(2))-(p3(1)-p1(1))*(p2(2)-p1(2))
      d = -p1(1)*a-p1(2)*b-p1(3)*c

      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine PlaneV(P,DOT,a,b,c,d)
      IMPLICIT NONE
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
      IMPLICIT NONE
c      iflag=1 -Rotate from P system to LAB
c      iflag=-1 -Rotate from LAB system to P
      real*4 P(3),P1(3)
      real*8 a(3,3)
      real phi,th,psi
      real pphi,pth,ppsi
      integer i,j
      pphi =phi
      pth  =th
      ppsi =psi
      if( abs(pphi).lt.0.002) pphi=0

      if(  abs(pth).le.0.002 ) pth=0
      
      if( abs(ppsi).le.0.002 ) ppsi=0

      a(1,1) = -sin(ppsi)*sin(pphi)+cos(pth)*cos(pphi)*cos(ppsi)
      a(1,2) = -sin(ppsi)*cos(pphi)+cos(pth)*sin(pphi)*cos(ppsi)
      a(1,3) = -sin(pth)*cos(ppsi)
      a(2,1) = -sin(pphi)*cos(ppsi)-cos(pth)*cos(pphi)*sin(ppsi)
      a(2,2) =  cos(pphi)*cos(ppsi)-cos(pth)*sin(pphi)*sin(ppsi)
      a(2,3) =  sin(pth)*sin(ppsi)
      a(3,1) =  sin(pth)*cos(pphi)
      a(3,2) =  sin(pth)*sin(pphi)
      a(3,3) =  cos(pth)
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
      integer i
      do i=1,6
         V2(i)=V1(i)
      enddo
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine sub6vec(V1,V2,V3)
      IMPLICIT NONE
      real*8 V1(6),V2(6),V3(6)
      integer i

      do i=1,3
         v3(i)=V1(i)-V2(i)
      enddo
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine val6vec(V,val)
      IMPLICIT NONE
c
c     finds the distance from origin for 6 Vector
c
      real*8 V(6),val
      val = sqrt(V(1)**2+V(2)**2+V(3)**2)
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine mom6vec(V,val)
      IMPLICIT NONE
c
c     finds momentum of 6 vector
c      
      real*8 V(6),val
      val = sqrt(V(4)**2+V(5)**2+V(6)**2)
      end

c
ccccccccccccccccccccccccccccccccccccccccc
      subroutine CalcMomComp(xluc,yluc,zluc,xbig,ybig,zbig,px,py,pz,E,pM)
      IMPLICIT NONE
      real*4 xluc,yluc,zluc,xbig,ybig,zbig,px,py,pz
      real*4 dist,E,pM
      dist = sqrt((xluc-xbig)**2+(yluc-ybig)**2+(zluc-zbig)**2)
      px   = sqrt(E**2-pm**2)*(xluc-xbig)/dist
      py   = sqrt(E**2-pm**2)*(yluc-ybig)/dist
      pz   = sqrt(E**2-pm**2)*(zluc-zbig)/dist

      end
ccccccccccccccccccccccccccccc
      subroutine PlaneLineIntersection(a,b,c,d,P1,P2,P_i)
      IMPLICIT NONE
c
c     Calculates intersection point of plane with line
c     a,b,c,d- are plane parameters
c     P1, and P2 are points of Lane
c     P_i -intersection poin
c     
      real*8 a,b,c,d
      real*4 P1(3),P2(3),P_i(3)
      real*4 u
      if((a*(P1(1)-P2(1))+b*(P1(2)-P2(2))+c*(P1(3)-P2(3))).ne.0)then

         u = ( a*P1(1) + b*P1(2) + c*P1(3) + d )/
     ,        ( a*(P1(1)-P2(1)) + b*(P1(2)-P2(2)) + c*(P1(3)-P2(3)) )

         P_i(1)=(P2(1)-P1(1))*u+P1(1)

         P_i(2)=(P2(2)-P1(2))*u+P1(2)

         P_i(3)=(P2(3)-P1(3))*u+P1(3)

C         write(*,*)P_i

      else
         P_i(1)=0
         P_i(2)=0
         P_i(3)=0
         write(*,*)'Line is paralel to plane'
      endif
      end
