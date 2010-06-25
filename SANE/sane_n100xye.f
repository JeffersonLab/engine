      subroutine NueralParam(i,ixmax,iymax,jmax,etot,
     ,     XX,YY,Eyx)    
      IMPLICIT NONE
      include 'bigcal_data_structures.cmn'
      include 'sane_data_structures.cmn'
      include 'b_ntuple.cmn'
      include 'sane_ntuple.cmn'
      include 'gen_data_structures.cmn'
      include 'gen_event_info.cmn'

      real xmomsqr,xmom,xmomsq,ymomsqr,ymom,ymomsq
      integer jmax
      real emax

      real eyx(5,5),XX(5,5),YY(5,5),en,Emt,Etot9,Etot
      integer ixmax, iymax,jj,i,ii
      real Xmomf
      
      do ii=1,5

         do jj=1,5
            eyx(ii,jj)=0
            xx(ii,jj)=0
            yy(ii,jj)=0
         enddo
      enddo

      emax=0
      do jj=1,ncellclust(i)
         en = eblock(jj,i)
         if(en.gt.emax)then
            emax  = en
            ixmax = ixcell(jj,i)
            iymax = iycell(jj,i)
            jmax  = jj
         endif
      enddo

      etot =0 
      do jj=1,ncellclust(i)
         en = eblock(jj,i)
         if(en.gt.0.01.and.En.eq.En)then

            if(iycell(jj,i).lt.33.and.
     ,           iycell(jj,i).gt.0.and.ixcell(jj,i).gt.0.and.
     ,           ixcell(jj,i).lt.33.and.
     ,           iycell(jj,i)-iymax+3.gt.0.and.
     ,           iycell(jj,i)-iymax+3.lt.6.and.
     ,           ixcell(jj,i)-ixmax+3.gt.0.and.
     ,           ixcell(jj,i)-ixmax+3.lt.6
     ,           )then
               Eyx(iycell(jj,i)-iymax+3,ixcell(jj,i)-ixmax+3)=en
               XX(iycell(jj,i)-iymax+3,ixcell(jj,i)-ixmax+3)=xcell(jj,i)
               YY(iycell(jj,i)-iymax+3,ixcell(jj,i)-ixmax+3)=ycell(jj,i)
            elseif(iycell(jj,i).lt.57.and.iycell(jj,i).gt.32.and.
     ,              ixcell(jj,i).gt.0.and.ixcell(jj,i).lt.31.and.
     ,           iycell(jj,i)-iymax+3.gt.0.and.
     ,           iycell(jj,i)-iymax+3.lt.6.and.
     ,           ixcell(jj,i)-ixmax+3.gt.0.and.
     ,           ixcell(jj,i)-ixmax+3.lt.6
     ,           )then

               Eyx(iycell(jj,i)-iymax+3,ixcell(jj,i)-ixmax+3)=en
               XX(iycell(jj,i)-iymax+3,ixcell(jj,i)-ixmax+3)=xcell(jj,i)
               YY(iycell(jj,i)-iymax+3,ixcell(jj,i)-ixmax+3)=ycell(jj,i)
            endif
         endif
      enddo
      etot =eyx(1,1)+eyx(1,2)+eyx(1,3)+eyx(1,4)+eyx(1,5)+
     ,     eyx(2,1)+eyx(2,2)+eyx(2,3)+eyx(2,4)+eyx(2,5)+
     ,     eyx(3,1)+eyx(3,2)+eyx(3,3)+eyx(3,4)+eyx(3,5)+
     ,     eyx(4,1)+eyx(4,2)+eyx(4,3)+eyx(4,4)+eyx(4,5)+
     ,     eyx(5,1)+eyx(5,2)+eyx(5,3)+eyx(5,4)+eyx(5,5)


      end

      
      double precision function sane_n100xye(x, index)
      implicit double precision (a-h,n-z)
      double precision x(27),etot
      integer i

C --- Last Layer
      etot=0
      do i=1,25
         if(x(i).gt.0.01)then
            etot=etot+x(i)
         endif
      enddo
      if (index.eq.0) then
         sane_n100xye=neuron0x8ba5c78(x)
      else if (index.eq.1) then
         sane_n100xye=neuron0x8ba83b0(x)
      else if (index.eq.2) then
         if(x(27).lt.2.or.x(27).gt.55)then
            sane_n100xye=neuron0x8ba8718(x)+
     ,           0.8*exp(-10*etot)+0.18*etot-0.07
         else
            sane_n100xye=neuron0x8ba8718(x)-(0.11-0.11/3.7*etot)
         endif
         if(x(26).lt.2) sane_n100xye= sane_n100xye+
     ,        exp(-10*etot)+0.12*etot

         if(x(26).gt.31)sane_n100xye= sane_n100xye+
     ,        exp(-10*etot)+0.08*etot
         if(x(26).gt.29.and.x(27).gt.32)sane_n100xye= sane_n100xye+
     ,        exp(-10*etot)+0.12*etot

      else
          sane_n100xye=0.d0
      endif
      end
C --- First and Hidden layers
      double precision function neuron0x8b9f9f0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8b9f9f0 = (x(1) - 0d0)/1d0
      end
      double precision function neuron0x8b9fb80(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8b9fb80 = (x(2) - 0d0)/1d0
      end
      double precision function neuron0x8b9fd58(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8b9fd58 = (x(3) - 0d0)/1d0
      end
      double precision function neuron0x8b9ff30(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8b9ff30 = (x(4) - 0d0)/1d0
      end
      double precision function neuron0x8ba0108(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8ba0108 = (x(5) - 0d0)/1d0
      end
      double precision function neuron0x8ba02e0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8ba02e0 = (x(6) - 0d0)/1d0
      end
      double precision function neuron0x8ba04b8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8ba04b8 = (x(7) - 0d0)/1d0
      end
      double precision function neuron0x8ba06a8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8ba06a8 = (x(8) - 0d0)/1d0
      end
      double precision function neuron0x8ba0898(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8ba0898 = (x(9) - 0d0)/1d0
      end
      double precision function neuron0x8ba0a88(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8ba0a88 = (x(10) - 0d0)/1d0
      end
      double precision function neuron0x8ba0c78(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8ba0c78 = (x(11) - 0d0)/1d0
      end
      double precision function neuron0x8ba0e68(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8ba0e68 = (x(12) - 0d0)/1d0
      end
      double precision function neuron0x8ba1058(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8ba1058 = (x(13) - 0d0)/1d0
      end
      double precision function neuron0x8ba1248(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8ba1248 = (x(14) - 0d0)/1d0
      end
      double precision function neuron0x8ba1438(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8ba1438 = (x(15) - 0d0)/1d0
      end
      double precision function neuron0x8ba1628(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8ba1628 = (x(16) - 0d0)/1d0
      end
      double precision function neuron0x8ba1818(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8ba1818 = (x(17) - 0d0)/1d0
      end
      double precision function neuron0x8ba1b18(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8ba1b18 = (x(18) - 0d0)/1d0
      end
      double precision function neuron0x8ba1d08(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8ba1d08 = (x(19) - 0d0)/1d0
      end
      double precision function neuron0x8ba1ef8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8ba1ef8 = (x(20) - 0d0)/1d0
      end
      double precision function neuron0x8ba20e8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8ba20e8 = (x(21) - 0d0)/1d0
      end
      double precision function neuron0x8ba22d8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8ba22d8 = (x(22) - 0d0)/1d0
      end
      double precision function neuron0x8ba24c8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8ba24c8 = (x(23) - 0d0)/1d0
      end
      double precision function neuron0x8ba26b8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8ba26b8 = (x(24) - 0d0)/1d0
      end
      double precision function neuron0x8ba28a8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8ba28a8 = (x(25) - 0d0)/1d0
      end
      double precision function neuron0x8ba2a98(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8ba2a98 = (x(26) - 0d0)/1d0
      end
      double precision function neuron0x8ba2c88(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8ba2c88 = (x(27) - 0d0)/1d0
      end
      double precision function neuron0x8ba2f98(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8ba2f98 = -0.747233d0
      neuron0x8ba2f98 = neuron0x8ba2f98 + synapse0x8b81ae8(x)
      neuron0x8ba2f98 = neuron0x8ba2f98 + synapse0x89c73b8(x)
      neuron0x8ba2f98 = neuron0x8ba2f98 + synapse0x8ba3170(x)
      neuron0x8ba2f98 = neuron0x8ba2f98 + synapse0x8ba3198(x)
      neuron0x8ba2f98 = neuron0x8ba2f98 + synapse0x8ba31c0(x)
      neuron0x8ba2f98 = neuron0x8ba2f98 + synapse0x8ba31e8(x)
      neuron0x8ba2f98 = neuron0x8ba2f98 + synapse0x8ba3210(x)
      neuron0x8ba2f98 = neuron0x8ba2f98 + synapse0x8ba3238(x)
      neuron0x8ba2f98 = neuron0x8ba2f98 + synapse0x8ba3260(x)
      neuron0x8ba2f98 = neuron0x8ba2f98 + synapse0x8ba3288(x)
      neuron0x8ba2f98 = neuron0x8ba2f98 + synapse0x8ba32b0(x)
      neuron0x8ba2f98 = neuron0x8ba2f98 + synapse0x8ba32d8(x)
      neuron0x8ba2f98 = neuron0x8ba2f98 + synapse0x8ba3300(x)
      neuron0x8ba2f98 = neuron0x8ba2f98 + synapse0x8ba3328(x)
      neuron0x8ba2f98 = neuron0x8ba2f98 + synapse0x8ba3350(x)
      neuron0x8ba2f98 = neuron0x8ba2f98 + synapse0x8ba3378(x)
      neuron0x8ba2f98 = neuron0x8ba2f98 + synapse0x8ba33a0(x)
      neuron0x8ba2f98 = neuron0x8ba2f98 + synapse0x8ba3450(x)
      neuron0x8ba2f98 = neuron0x8ba2f98 + synapse0x8ba3478(x)
      neuron0x8ba2f98 = neuron0x8ba2f98 + synapse0x8ba34a0(x)
      neuron0x8ba2f98 = neuron0x8ba2f98 + synapse0x8ba34c8(x)
      neuron0x8ba2f98 = neuron0x8ba2f98 + synapse0x8ba34f0(x)
      neuron0x8ba2f98 = neuron0x8ba2f98 + synapse0x8ba3518(x)
      neuron0x8ba2f98 = neuron0x8ba2f98 + synapse0x8ba3540(x)
      neuron0x8ba2f98 = neuron0x8ba2f98 + synapse0x8ba3568(x)
      neuron0x8ba2f98 = neuron0x8ba2f98 + synapse0x8ba3590(x)
      neuron0x8ba2f98 = neuron0x8ba2f98 + synapse0x8ba35b8(x)
      neuron0x8ba2f98= (exp(-neuron0x8ba2f98*neuron0x8ba2f98))
      end
      double precision function neuron0x8ba35e0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8ba35e0 = 0.400749d0
      neuron0x8ba35e0 = neuron0x8ba35e0 + synapse0x89d14d0(x)
      neuron0x8ba35e0 = neuron0x8ba35e0 + synapse0x89d14f8(x)
      neuron0x8ba35e0 = neuron0x8ba35e0 + synapse0x8ba4788(x)
      neuron0x8ba35e0 = neuron0x8ba35e0 + synapse0x8ba47b0(x)
      neuron0x8ba35e0 = neuron0x8ba35e0 + synapse0x8ba47d8(x)
      neuron0x8ba35e0 = neuron0x8ba35e0 + synapse0x8ba4800(x)
      neuron0x8ba35e0 = neuron0x8ba35e0 + synapse0x8ba33c8(x)
      neuron0x8ba35e0 = neuron0x8ba35e0 + synapse0x8ba33f0(x)
      neuron0x8ba35e0 = neuron0x8ba35e0 + synapse0x8ba3418(x)
      neuron0x8ba35e0 = neuron0x8ba35e0 + synapse0x8ba4930(x)
      neuron0x8ba35e0 = neuron0x8ba35e0 + synapse0x8ba4958(x)
      neuron0x8ba35e0 = neuron0x8ba35e0 + synapse0x8ba4980(x)
      neuron0x8ba35e0 = neuron0x8ba35e0 + synapse0x8ba49a8(x)
      neuron0x8ba35e0 = neuron0x8ba35e0 + synapse0x8ba49d0(x)
      neuron0x8ba35e0 = neuron0x8ba35e0 + synapse0x8ba49f8(x)
      neuron0x8ba35e0 = neuron0x8ba35e0 + synapse0x8ba4a20(x)
      neuron0x8ba35e0 = neuron0x8ba35e0 + synapse0x8ba4ad0(x)
      neuron0x8ba35e0 = neuron0x8ba35e0 + synapse0x8ba4af8(x)
      neuron0x8ba35e0 = neuron0x8ba35e0 + synapse0x8ba4b20(x)
      neuron0x8ba35e0 = neuron0x8ba35e0 + synapse0x8ba4b48(x)
      neuron0x8ba35e0 = neuron0x8ba35e0 + synapse0x8ba4b70(x)
      neuron0x8ba35e0 = neuron0x8ba35e0 + synapse0x8ba4b98(x)
      neuron0x8ba35e0 = neuron0x8ba35e0 + synapse0x8ba4bc0(x)
      neuron0x8ba35e0 = neuron0x8ba35e0 + synapse0x8ba4be8(x)
      neuron0x8ba35e0 = neuron0x8ba35e0 + synapse0x8ba4c10(x)
      neuron0x8ba35e0 = neuron0x8ba35e0 + synapse0x8ba4c38(x)
      neuron0x8ba35e0 = neuron0x8ba35e0 + synapse0x8ba4c60(x)
      neuron0x8ba35e0= (exp(-neuron0x8ba35e0*neuron0x8ba35e0))
      end
      double precision function neuron0x8ba4c88(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8ba4c88 = -0.299935d0
      neuron0x8ba4c88 = neuron0x8ba4c88 + synapse0x8ba4e38(x)
      neuron0x8ba4c88 = neuron0x8ba4c88 + synapse0x8ba4e60(x)
      neuron0x8ba4c88 = neuron0x8ba4c88 + synapse0x8ba4e88(x)
      neuron0x8ba4c88 = neuron0x8ba4c88 + synapse0x8ba4eb0(x)
      neuron0x8ba4c88 = neuron0x8ba4c88 + synapse0x8ba4ed8(x)
      neuron0x8ba4c88 = neuron0x8ba4c88 + synapse0x8ba4f00(x)
      neuron0x8ba4c88 = neuron0x8ba4c88 + synapse0x8ba4f28(x)
      neuron0x8ba4c88 = neuron0x8ba4c88 + synapse0x8ba4f50(x)
      neuron0x8ba4c88 = neuron0x8ba4c88 + synapse0x8ba4f78(x)
      neuron0x8ba4c88 = neuron0x8ba4c88 + synapse0x8ba4fa0(x)
      neuron0x8ba4c88 = neuron0x8ba4c88 + synapse0x8ba4fc8(x)
      neuron0x8ba4c88 = neuron0x8ba4c88 + synapse0x89c7788(x)
      neuron0x8ba4c88 = neuron0x8ba4c88 + synapse0x89c7348(x)
      neuron0x8ba4c88 = neuron0x8ba4c88 + synapse0x89c7370(x)
      neuron0x8ba4c88 = neuron0x8ba4c88 + synapse0x89c7540(x)
      neuron0x8ba4c88 = neuron0x8ba4c88 + synapse0x89c7568(x)
      neuron0x8ba4c88 = neuron0x8ba4c88 + synapse0x8ba48b0(x)
      neuron0x8ba4c88 = neuron0x8ba4c88 + synapse0x8ba48d8(x)
      neuron0x8ba4c88 = neuron0x8ba4c88 + synapse0x8ba4900(x)
      neuron0x8ba4c88 = neuron0x8ba4c88 + synapse0x8b7dca8(x)
      neuron0x8ba4c88 = neuron0x8ba4c88 + synapse0x8b7dcd0(x)
      neuron0x8ba4c88 = neuron0x8ba4c88 + synapse0x89c7a08(x)
      neuron0x8ba4c88 = neuron0x8ba4c88 + synapse0x89c7a30(x)
      neuron0x8ba4c88 = neuron0x8ba4c88 + synapse0x89c7a58(x)
      neuron0x8ba4c88 = neuron0x8ba4c88 + synapse0x89c7a80(x)
      neuron0x8ba4c88 = neuron0x8ba4c88 + synapse0x89c7aa8(x)
      neuron0x8ba4c88 = neuron0x8ba4c88 + synapse0x89c7ad0(x)
      neuron0x8ba4c88= (exp(-neuron0x8ba4c88*neuron0x8ba4c88))
      end
      double precision function neuron0x89c7bb8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x89c7bb8 = -0.0871005d0
      neuron0x89c7bb8 = neuron0x89c7bb8 + synapse0x89c7af8(x)
      neuron0x89c7bb8 = neuron0x89c7bb8 + synapse0x8ba4ff0(x)
      neuron0x89c7bb8 = neuron0x89c7bb8 + synapse0x8ba5018(x)
      neuron0x89c7bb8 = neuron0x89c7bb8 + synapse0x8ba5040(x)
      neuron0x89c7bb8 = neuron0x89c7bb8 + synapse0x8ba5068(x)
      neuron0x89c7bb8 = neuron0x89c7bb8 + synapse0x8ba5090(x)
      neuron0x89c7bb8 = neuron0x89c7bb8 + synapse0x8ba50b8(x)
      neuron0x89c7bb8 = neuron0x89c7bb8 + synapse0x8ba50e0(x)
      neuron0x89c7bb8 = neuron0x89c7bb8 + synapse0x8ba5108(x)
      neuron0x89c7bb8 = neuron0x89c7bb8 + synapse0x8ba5130(x)
      neuron0x89c7bb8 = neuron0x89c7bb8 + synapse0x8ba5158(x)
      neuron0x89c7bb8 = neuron0x89c7bb8 + synapse0x8ba5180(x)
      neuron0x89c7bb8 = neuron0x89c7bb8 + synapse0x8ba51a8(x)
      neuron0x89c7bb8 = neuron0x89c7bb8 + synapse0x8ba51d0(x)
      neuron0x89c7bb8 = neuron0x89c7bb8 + synapse0x8ba51f8(x)
      neuron0x89c7bb8 = neuron0x89c7bb8 + synapse0x8ba5220(x)
      neuron0x89c7bb8 = neuron0x89c7bb8 + synapse0x8ba52d0(x)
      neuron0x89c7bb8 = neuron0x89c7bb8 + synapse0x8ba52f8(x)
      neuron0x89c7bb8 = neuron0x89c7bb8 + synapse0x8ba5320(x)
      neuron0x89c7bb8 = neuron0x89c7bb8 + synapse0x8ba5348(x)
      neuron0x89c7bb8 = neuron0x89c7bb8 + synapse0x8ba5370(x)
      neuron0x89c7bb8 = neuron0x89c7bb8 + synapse0x8ba5398(x)
      neuron0x89c7bb8 = neuron0x89c7bb8 + synapse0x8ba53c0(x)
      neuron0x89c7bb8 = neuron0x89c7bb8 + synapse0x8ba53e8(x)
      neuron0x89c7bb8 = neuron0x89c7bb8 + synapse0x8ba5410(x)
      neuron0x89c7bb8 = neuron0x89c7bb8 + synapse0x8ba5438(x)
      neuron0x89c7bb8 = neuron0x89c7bb8 + synapse0x8ba5460(x)
      neuron0x89c7bb8= (exp(-neuron0x89c7bb8*neuron0x89c7bb8))
      end
      double precision function neuron0x8ba5488(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8ba5488 = -0.179616d0
      neuron0x8ba5488 = neuron0x8ba5488 + synapse0x8ba5618(x)
      neuron0x8ba5488 = neuron0x8ba5488 + synapse0x8ba5640(x)
      neuron0x8ba5488 = neuron0x8ba5488 + synapse0x8ba5668(x)
      neuron0x8ba5488 = neuron0x8ba5488 + synapse0x8ba5690(x)
      neuron0x8ba5488 = neuron0x8ba5488 + synapse0x8ba56b8(x)
      neuron0x8ba5488 = neuron0x8ba5488 + synapse0x8ba56e0(x)
      neuron0x8ba5488 = neuron0x8ba5488 + synapse0x8ba5708(x)
      neuron0x8ba5488 = neuron0x8ba5488 + synapse0x8ba5730(x)
      neuron0x8ba5488 = neuron0x8ba5488 + synapse0x8ba5758(x)
      neuron0x8ba5488 = neuron0x8ba5488 + synapse0x8ba5780(x)
      neuron0x8ba5488 = neuron0x8ba5488 + synapse0x8ba57a8(x)
      neuron0x8ba5488 = neuron0x8ba5488 + synapse0x8ba57d0(x)
      neuron0x8ba5488 = neuron0x8ba5488 + synapse0x8ba57f8(x)
      neuron0x8ba5488 = neuron0x8ba5488 + synapse0x8ba5820(x)
      neuron0x8ba5488 = neuron0x8ba5488 + synapse0x8ba5848(x)
      neuron0x8ba5488 = neuron0x8ba5488 + synapse0x8ba5870(x)
      neuron0x8ba5488 = neuron0x8ba5488 + synapse0x8ba5920(x)
      neuron0x8ba5488 = neuron0x8ba5488 + synapse0x8ba5948(x)
      neuron0x8ba5488 = neuron0x8ba5488 + synapse0x8ba5970(x)
      neuron0x8ba5488 = neuron0x8ba5488 + synapse0x8ba5998(x)
      neuron0x8ba5488 = neuron0x8ba5488 + synapse0x8ba59c0(x)
      neuron0x8ba5488 = neuron0x8ba5488 + synapse0x89339c8(x)
      neuron0x8ba5488 = neuron0x8ba5488 + synapse0x89c7800(x)
      neuron0x8ba5488 = neuron0x8ba5488 + synapse0x89c7828(x)
      neuron0x8ba5488 = neuron0x8ba5488 + synapse0x89c7850(x)
      neuron0x8ba5488 = neuron0x8ba5488 + synapse0x89c7878(x)
      neuron0x8ba5488 = neuron0x8ba5488 + synapse0x89c78a0(x)
      neuron0x8ba5488= (exp(-neuron0x8ba5488*neuron0x8ba5488))
      end
      double precision function neuron0x89c78c8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x89c78c8 = 0.905333d0
      neuron0x89c78c8 = neuron0x89c78c8 + synapse0x89c79c8(x)
      neuron0x89c78c8 = neuron0x89c78c8 + synapse0x8ba1a90(x)
      neuron0x89c78c8 = neuron0x89c78c8 + synapse0x8ba1ab8(x)
      neuron0x89c78c8 = neuron0x89c78c8 + synapse0x8ba1ae0(x)
      neuron0x89c78c8 = neuron0x89c78c8 + synapse0x8ba5fd0(x)
      neuron0x89c78c8 = neuron0x89c78c8 + synapse0x8ba5ff8(x)
      neuron0x89c78c8 = neuron0x89c78c8 + synapse0x8ba6020(x)
      neuron0x89c78c8 = neuron0x89c78c8 + synapse0x8ba6048(x)
      neuron0x89c78c8 = neuron0x89c78c8 + synapse0x8ba6070(x)
      neuron0x89c78c8 = neuron0x89c78c8 + synapse0x8ba6098(x)
      neuron0x89c78c8 = neuron0x89c78c8 + synapse0x8ba60c0(x)
      neuron0x89c78c8 = neuron0x89c78c8 + synapse0x8ba60e8(x)
      neuron0x89c78c8 = neuron0x89c78c8 + synapse0x8ba6110(x)
      neuron0x89c78c8 = neuron0x89c78c8 + synapse0x8ba6138(x)
      neuron0x89c78c8 = neuron0x89c78c8 + synapse0x8ba6160(x)
      neuron0x89c78c8 = neuron0x89c78c8 + synapse0x8ba6188(x)
      neuron0x89c78c8 = neuron0x89c78c8 + synapse0x8ba6238(x)
      neuron0x89c78c8 = neuron0x89c78c8 + synapse0x8ba6260(x)
      neuron0x89c78c8 = neuron0x89c78c8 + synapse0x8ba6288(x)
      neuron0x89c78c8 = neuron0x89c78c8 + synapse0x8ba62b0(x)
      neuron0x89c78c8 = neuron0x89c78c8 + synapse0x8ba62d8(x)
      neuron0x89c78c8 = neuron0x89c78c8 + synapse0x8ba6300(x)
      neuron0x89c78c8 = neuron0x89c78c8 + synapse0x8ba6328(x)
      neuron0x89c78c8 = neuron0x89c78c8 + synapse0x8ba6350(x)
      neuron0x89c78c8 = neuron0x89c78c8 + synapse0x8ba6378(x)
      neuron0x89c78c8 = neuron0x89c78c8 + synapse0x8ba63a0(x)
      neuron0x89c78c8 = neuron0x89c78c8 + synapse0x8ba63c8(x)
      neuron0x89c78c8= (exp(-neuron0x89c78c8*neuron0x89c78c8))
      end
      double precision function neuron0x8ba63f0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8ba63f0 = 0.121249d0
      neuron0x8ba63f0 = neuron0x8ba63f0 + synapse0x8ba6580(x)
      neuron0x8ba63f0 = neuron0x8ba63f0 + synapse0x8ba65a8(x)
      neuron0x8ba63f0 = neuron0x8ba63f0 + synapse0x8ba65d0(x)
      neuron0x8ba63f0 = neuron0x8ba63f0 + synapse0x8ba65f8(x)
      neuron0x8ba63f0 = neuron0x8ba63f0 + synapse0x8ba6620(x)
      neuron0x8ba63f0 = neuron0x8ba63f0 + synapse0x8ba6648(x)
      neuron0x8ba63f0 = neuron0x8ba63f0 + synapse0x8ba6670(x)
      neuron0x8ba63f0 = neuron0x8ba63f0 + synapse0x8ba6698(x)
      neuron0x8ba63f0 = neuron0x8ba63f0 + synapse0x8ba66c0(x)
      neuron0x8ba63f0 = neuron0x8ba63f0 + synapse0x8ba66e8(x)
      neuron0x8ba63f0 = neuron0x8ba63f0 + synapse0x8ba6710(x)
      neuron0x8ba63f0 = neuron0x8ba63f0 + synapse0x8ba6738(x)
      neuron0x8ba63f0 = neuron0x8ba63f0 + synapse0x8ba6760(x)
      neuron0x8ba63f0 = neuron0x8ba63f0 + synapse0x8ba6788(x)
      neuron0x8ba63f0 = neuron0x8ba63f0 + synapse0x8ba67b0(x)
      neuron0x8ba63f0 = neuron0x8ba63f0 + synapse0x8ba67d8(x)
      neuron0x8ba63f0 = neuron0x8ba63f0 + synapse0x8ba6888(x)
      neuron0x8ba63f0 = neuron0x8ba63f0 + synapse0x8ba68b0(x)
      neuron0x8ba63f0 = neuron0x8ba63f0 + synapse0x8ba68d8(x)
      neuron0x8ba63f0 = neuron0x8ba63f0 + synapse0x8ba6900(x)
      neuron0x8ba63f0 = neuron0x8ba63f0 + synapse0x8ba6928(x)
      neuron0x8ba63f0 = neuron0x8ba63f0 + synapse0x8ba6950(x)
      neuron0x8ba63f0 = neuron0x8ba63f0 + synapse0x8ba6978(x)
      neuron0x8ba63f0 = neuron0x8ba63f0 + synapse0x8ba69a0(x)
      neuron0x8ba63f0 = neuron0x8ba63f0 + synapse0x8ba69c8(x)
      neuron0x8ba63f0 = neuron0x8ba63f0 + synapse0x8ba69f0(x)
      neuron0x8ba63f0 = neuron0x8ba63f0 + synapse0x8ba6a18(x)
      neuron0x8ba63f0= (exp(-neuron0x8ba63f0*neuron0x8ba63f0))
      end
      double precision function neuron0x8ba6a40(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8ba6a40 = 0.0532247d0
      neuron0x8ba6a40 = neuron0x8ba6a40 + synapse0x8ba6bd0(x)
      neuron0x8ba6a40 = neuron0x8ba6a40 + synapse0x8ba6bf8(x)
      neuron0x8ba6a40 = neuron0x8ba6a40 + synapse0x8ba6c20(x)
      neuron0x8ba6a40 = neuron0x8ba6a40 + synapse0x8ba6c48(x)
      neuron0x8ba6a40 = neuron0x8ba6a40 + synapse0x8ba6c70(x)
      neuron0x8ba6a40 = neuron0x8ba6a40 + synapse0x8ba6c98(x)
      neuron0x8ba6a40 = neuron0x8ba6a40 + synapse0x8ba6cc0(x)
      neuron0x8ba6a40 = neuron0x8ba6a40 + synapse0x8ba6ce8(x)
      neuron0x8ba6a40 = neuron0x8ba6a40 + synapse0x8ba6d10(x)
      neuron0x8ba6a40 = neuron0x8ba6a40 + synapse0x8ba6d38(x)
      neuron0x8ba6a40 = neuron0x8ba6a40 + synapse0x8ba6d60(x)
      neuron0x8ba6a40 = neuron0x8ba6a40 + synapse0x8ba6d88(x)
      neuron0x8ba6a40 = neuron0x8ba6a40 + synapse0x8ba6db0(x)
      neuron0x8ba6a40 = neuron0x8ba6a40 + synapse0x8ba6dd8(x)
      neuron0x8ba6a40 = neuron0x8ba6a40 + synapse0x8ba6e00(x)
      neuron0x8ba6a40 = neuron0x8ba6a40 + synapse0x8ba6e28(x)
      neuron0x8ba6a40 = neuron0x8ba6a40 + synapse0x8ba6ed8(x)
      neuron0x8ba6a40 = neuron0x8ba6a40 + synapse0x8ba6f00(x)
      neuron0x8ba6a40 = neuron0x8ba6a40 + synapse0x8ba6f28(x)
      neuron0x8ba6a40 = neuron0x8ba6a40 + synapse0x8ba6f50(x)
      neuron0x8ba6a40 = neuron0x8ba6a40 + synapse0x8ba6f78(x)
      neuron0x8ba6a40 = neuron0x8ba6a40 + synapse0x8ba6fa0(x)
      neuron0x8ba6a40 = neuron0x8ba6a40 + synapse0x8ba6fc8(x)
      neuron0x8ba6a40 = neuron0x8ba6a40 + synapse0x8ba6ff0(x)
      neuron0x8ba6a40 = neuron0x8ba6a40 + synapse0x8ba7018(x)
      neuron0x8ba6a40 = neuron0x8ba6a40 + synapse0x8ba7040(x)
      neuron0x8ba6a40 = neuron0x8ba6a40 + synapse0x8ba7068(x)
      neuron0x8ba6a40= (exp(-neuron0x8ba6a40*neuron0x8ba6a40))
      end
      double precision function neuron0x8ba7090(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8ba7090 = -0.695478d0
      neuron0x8ba7090 = neuron0x8ba7090 + synapse0x8ba7240(x)
      neuron0x8ba7090 = neuron0x8ba7090 + synapse0x8ba7268(x)
      neuron0x8ba7090 = neuron0x8ba7090 + synapse0x8ba7290(x)
      neuron0x8ba7090 = neuron0x8ba7090 + synapse0x8ba72b8(x)
      neuron0x8ba7090 = neuron0x8ba7090 + synapse0x8ba72e0(x)
      neuron0x8ba7090 = neuron0x8ba7090 + synapse0x8ba7308(x)
      neuron0x8ba7090 = neuron0x8ba7090 + synapse0x8ba7330(x)
      neuron0x8ba7090 = neuron0x8ba7090 + synapse0x8ba7358(x)
      neuron0x8ba7090 = neuron0x8ba7090 + synapse0x8ba7380(x)
      neuron0x8ba7090 = neuron0x8ba7090 + synapse0x8ba73a8(x)
      neuron0x8ba7090 = neuron0x8ba7090 + synapse0x8ba73d0(x)
      neuron0x8ba7090 = neuron0x8ba7090 + synapse0x8ba73f8(x)
      neuron0x8ba7090 = neuron0x8ba7090 + synapse0x8ba7420(x)
      neuron0x8ba7090 = neuron0x8ba7090 + synapse0x8ba7448(x)
      neuron0x8ba7090 = neuron0x8ba7090 + synapse0x8ba7470(x)
      neuron0x8ba7090 = neuron0x8ba7090 + synapse0x8ba7498(x)
      neuron0x8ba7090 = neuron0x8ba7090 + synapse0x8ba7548(x)
      neuron0x8ba7090 = neuron0x8ba7090 + synapse0x8ba7570(x)
      neuron0x8ba7090 = neuron0x8ba7090 + synapse0x8ba7598(x)
      neuron0x8ba7090 = neuron0x8ba7090 + synapse0x8ba75c0(x)
      neuron0x8ba7090 = neuron0x8ba7090 + synapse0x8ba75e8(x)
      neuron0x8ba7090 = neuron0x8ba7090 + synapse0x8ba7610(x)
      neuron0x8ba7090 = neuron0x8ba7090 + synapse0x8ba7638(x)
      neuron0x8ba7090 = neuron0x8ba7090 + synapse0x8ba7660(x)
      neuron0x8ba7090 = neuron0x8ba7090 + synapse0x8ba7688(x)
      neuron0x8ba7090 = neuron0x8ba7090 + synapse0x8ba76b0(x)
      neuron0x8ba7090 = neuron0x8ba7090 + synapse0x8ba76d8(x)
      neuron0x8ba7090= (exp(-neuron0x8ba7090*neuron0x8ba7090))
      end
      double precision function neuron0x8ba7700(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8ba7700 = 0.396814d0
      neuron0x8ba7700 = neuron0x8ba7700 + synapse0x8ba78b0(x)
      neuron0x8ba7700 = neuron0x8ba7700 + synapse0x8ba78d8(x)
      neuron0x8ba7700 = neuron0x8ba7700 + synapse0x8ba7900(x)
      neuron0x8ba7700 = neuron0x8ba7700 + synapse0x8ba7928(x)
      neuron0x8ba7700 = neuron0x8ba7700 + synapse0x8ba7950(x)
      neuron0x8ba7700 = neuron0x8ba7700 + synapse0x8ba7978(x)
      neuron0x8ba7700 = neuron0x8ba7700 + synapse0x8ba79a0(x)
      neuron0x8ba7700 = neuron0x8ba7700 + synapse0x8ba79c8(x)
      neuron0x8ba7700 = neuron0x8ba7700 + synapse0x8ba79f0(x)
      neuron0x8ba7700 = neuron0x8ba7700 + synapse0x8ba7a18(x)
      neuron0x8ba7700 = neuron0x8ba7700 + synapse0x8ba7a40(x)
      neuron0x8ba7700 = neuron0x8ba7700 + synapse0x8ba7a68(x)
      neuron0x8ba7700 = neuron0x8ba7700 + synapse0x8ba7a90(x)
      neuron0x8ba7700 = neuron0x8ba7700 + synapse0x8ba7ab8(x)
      neuron0x8ba7700 = neuron0x8ba7700 + synapse0x8ba59e8(x)
      neuron0x8ba7700 = neuron0x8ba7700 + synapse0x8ba5a10(x)
      neuron0x8ba7700 = neuron0x8ba7700 + synapse0x8ba5ac0(x)
      neuron0x8ba7700 = neuron0x8ba7700 + synapse0x8ba5ae8(x)
      neuron0x8ba7700 = neuron0x8ba7700 + synapse0x8ba5b10(x)
      neuron0x8ba7700 = neuron0x8ba7700 + synapse0x8ba5b38(x)
      neuron0x8ba7700 = neuron0x8ba7700 + synapse0x8ba5b60(x)
      neuron0x8ba7700 = neuron0x8ba7700 + synapse0x8ba5b88(x)
      neuron0x8ba7700 = neuron0x8ba7700 + synapse0x8ba5bb0(x)
      neuron0x8ba7700 = neuron0x8ba7700 + synapse0x8ba5bd8(x)
      neuron0x8ba7700 = neuron0x8ba7700 + synapse0x8ba5c00(x)
      neuron0x8ba7700 = neuron0x8ba7700 + synapse0x8ba5c28(x)
      neuron0x8ba7700 = neuron0x8ba7700 + synapse0x8ba5c50(x)
      neuron0x8ba7700= (exp(-neuron0x8ba7700*neuron0x8ba7700))
      end
      double precision function neuron0x8ba5c78(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8ba5c78 = -3.64843d0
      neuron0x8ba5c78 = neuron0x8ba5c78 + synapse0x8ba2ee8(x)
      neuron0x8ba5c78 = neuron0x8ba5c78 + synapse0x8ba2f10(x)
      neuron0x8ba5c78 = neuron0x8ba5c78 + synapse0x8ba2f38(x)
      neuron0x8ba5c78 = neuron0x8ba5c78 + synapse0x8ba5dc0(x)
      neuron0x8ba5c78 = neuron0x8ba5c78 + synapse0x8ba2f60(x)
      neuron0x8ba5c78 = neuron0x8ba5c78 + synapse0x8ba82e8(x)
      neuron0x8ba5c78 = neuron0x8ba5c78 + synapse0x8ba8310(x)
      neuron0x8ba5c78 = neuron0x8ba5c78 + synapse0x8ba8338(x)
      neuron0x8ba5c78 = neuron0x8ba5c78 + synapse0x8ba8360(x)
      neuron0x8ba5c78 = neuron0x8ba5c78 + synapse0x8ba8388(x)
      end
      double precision function neuron0x8ba83b0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8ba83b0 = -6.51084d0
      neuron0x8ba83b0 = neuron0x8ba83b0 + synapse0x8ba8588(x)
      neuron0x8ba83b0 = neuron0x8ba83b0 + synapse0x8ba85b0(x)
      neuron0x8ba83b0 = neuron0x8ba83b0 + synapse0x8ba85d8(x)
      neuron0x8ba83b0 = neuron0x8ba83b0 + synapse0x8ba8600(x)
      neuron0x8ba83b0 = neuron0x8ba83b0 + synapse0x8ba8628(x)
      neuron0x8ba83b0 = neuron0x8ba83b0 + synapse0x8ba8650(x)
      neuron0x8ba83b0 = neuron0x8ba83b0 + synapse0x8ba8678(x)
      neuron0x8ba83b0 = neuron0x8ba83b0 + synapse0x8ba86a0(x)
      neuron0x8ba83b0 = neuron0x8ba83b0 + synapse0x8ba86c8(x)
      neuron0x8ba83b0 = neuron0x8ba83b0 + synapse0x8ba86f0(x)
      end
      double precision function neuron0x8ba8718(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      neuron0x8ba8718 = 0.177408d0
      neuron0x8ba8718 = neuron0x8ba8718 + synapse0x8ba2e78(x)
      neuron0x8ba8718 = neuron0x8ba8718 + synapse0x8ba88f0(x)
      neuron0x8ba8718 = neuron0x8ba8718 + synapse0x8ba8918(x)
      neuron0x8ba8718 = neuron0x8ba8718 + synapse0x8ba8940(x)
      neuron0x8ba8718 = neuron0x8ba8718 + synapse0x8ba8968(x)
      neuron0x8ba8718 = neuron0x8ba8718 + synapse0x8ba8990(x)
      neuron0x8ba8718 = neuron0x8ba8718 + synapse0x8ba89b8(x)
      neuron0x8ba8718 = neuron0x8ba8718 + synapse0x8ba89e0(x)
      neuron0x8ba8718 = neuron0x8ba8718 + synapse0x8ba8a08(x)
      neuron0x8ba8718 = neuron0x8ba8718 + synapse0x8ba8a30(x)
      end
C --- Synapses
      double precision function synapse0x8b81ae8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8b81ae8=neuron0x8b9f9f0(x)*(0.483642)
      end

      double precision function synapse0x89c73b8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x89c73b8=neuron0x8b9fb80(x)*(-0.331845)
      end

      double precision function synapse0x8ba3170(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba3170=neuron0x8b9fd58(x)*(-0.197671)
      end

      double precision function synapse0x8ba3198(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba3198=neuron0x8b9ff30(x)*(-0.0389979)
      end

      double precision function synapse0x8ba31c0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba31c0=neuron0x8ba0108(x)*(-0.390161)
      end

      double precision function synapse0x8ba31e8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba31e8=neuron0x8ba02e0(x)*(-0.0414126)
      end

      double precision function synapse0x8ba3210(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba3210=neuron0x8ba04b8(x)*(-0.122874)
      end

      double precision function synapse0x8ba3238(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba3238=neuron0x8ba06a8(x)*(-0.132127)
      end

      double precision function synapse0x8ba3260(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba3260=neuron0x8ba0898(x)*(-0.100542)
      end

      double precision function synapse0x8ba3288(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba3288=neuron0x8ba0a88(x)*(-0.132274)
      end

      double precision function synapse0x8ba32b0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba32b0=neuron0x8ba0c78(x)*(-0.0979607)
      end

      double precision function synapse0x8ba32d8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba32d8=neuron0x8ba0e68(x)*(-0.213274)
      end

      double precision function synapse0x8ba3300(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba3300=neuron0x8ba1058(x)*(-0.143064)
      end

      double precision function synapse0x8ba3328(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba3328=neuron0x8ba1248(x)*(-0.144841)
      end

      double precision function synapse0x8ba3350(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba3350=neuron0x8ba1438(x)*(0.119818)
      end

      double precision function synapse0x8ba3378(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba3378=neuron0x8ba1628(x)*(0.195316)
      end

      double precision function synapse0x8ba33a0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba33a0=neuron0x8ba1818(x)*(-2.14051)
      end

      double precision function synapse0x8ba3450(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba3450=neuron0x8ba1b18(x)*(-5.32795)
      end

      double precision function synapse0x8ba3478(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba3478=neuron0x8ba1d08(x)*(-1.95969)
      end

      double precision function synapse0x8ba34a0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba34a0=neuron0x8ba1ef8(x)*(0.158245)
      end

      double precision function synapse0x8ba34c8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba34c8=neuron0x8ba20e8(x)*(-0.142672)
      end

      double precision function synapse0x8ba34f0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba34f0=neuron0x8ba22d8(x)*(-0.611092)
      end

      double precision function synapse0x8ba3518(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba3518=neuron0x8ba24c8(x)*(-0.304743)
      end

      double precision function synapse0x8ba3540(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba3540=neuron0x8ba26b8(x)*(0.139128)
      end

      double precision function synapse0x8ba3568(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba3568=neuron0x8ba28a8(x)*(-0.397478)
      end

      double precision function synapse0x8ba3590(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba3590=neuron0x8ba2a98(x)*(-0.0147352)
      end

      double precision function synapse0x8ba35b8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba35b8=neuron0x8ba2c88(x)*(0.000893497)
      end

      double precision function synapse0x89d14d0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x89d14d0=neuron0x8b9f9f0(x)*(-0.281456)
      end

      double precision function synapse0x89d14f8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x89d14f8=neuron0x8b9fb80(x)*(-0.141963)
      end

      double precision function synapse0x8ba4788(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba4788=neuron0x8b9fd58(x)*(-0.345699)
      end

      double precision function synapse0x8ba47b0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba47b0=neuron0x8b9ff30(x)*(-0.263971)
      end

      double precision function synapse0x8ba47d8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba47d8=neuron0x8ba0108(x)*(0.182912)
      end

      double precision function synapse0x8ba4800(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba4800=neuron0x8ba02e0(x)*(0.00114743)
      end

      double precision function synapse0x8ba33c8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba33c8=neuron0x8ba04b8(x)*(0.150389)
      end

      double precision function synapse0x8ba33f0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba33f0=neuron0x8ba06a8(x)*(-0.0681054)
      end

      double precision function synapse0x8ba3418(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba3418=neuron0x8ba0898(x)*(-0.168092)
      end

      double precision function synapse0x8ba4930(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba4930=neuron0x8ba0a88(x)*(-0.471032)
      end

      double precision function synapse0x8ba4958(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba4958=neuron0x8ba0c78(x)*(0.126851)
      end

      double precision function synapse0x8ba4980(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba4980=neuron0x8ba0e68(x)*(0.367946)
      end

      double precision function synapse0x8ba49a8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba49a8=neuron0x8ba1058(x)*(-0.198735)
      end

      double precision function synapse0x8ba49d0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba49d0=neuron0x8ba1248(x)*(0.232556)
      end

      double precision function synapse0x8ba49f8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba49f8=neuron0x8ba1438(x)*(-0.278)
      end

      double precision function synapse0x8ba4a20(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba4a20=neuron0x8ba1628(x)*(0.124528)
      end

      double precision function synapse0x8ba4ad0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba4ad0=neuron0x8ba1818(x)*(0.422353)
      end

      double precision function synapse0x8ba4af8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba4af8=neuron0x8ba1b18(x)*(-0.159403)
      end

      double precision function synapse0x8ba4b20(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba4b20=neuron0x8ba1d08(x)*(0.0333493)
      end

      double precision function synapse0x8ba4b48(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba4b48=neuron0x8ba1ef8(x)*(-0.183193)
      end

      double precision function synapse0x8ba4b70(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba4b70=neuron0x8ba20e8(x)*(-0.0531818)
      end

      double precision function synapse0x8ba4b98(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba4b98=neuron0x8ba22d8(x)*(0.417614)
      end

      double precision function synapse0x8ba4bc0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba4bc0=neuron0x8ba24c8(x)*(0.140168)
      end

      double precision function synapse0x8ba4be8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba4be8=neuron0x8ba26b8(x)*(0.447215)
      end

      double precision function synapse0x8ba4c10(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba4c10=neuron0x8ba28a8(x)*(0.0160476)
      end

      double precision function synapse0x8ba4c38(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba4c38=neuron0x8ba2a98(x)*(-0.357885)
      end

      double precision function synapse0x8ba4c60(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba4c60=neuron0x8ba2c88(x)*(-0.214542)
      end

      double precision function synapse0x8ba4e38(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba4e38=neuron0x8b9f9f0(x)*(0.421851)
      end

      double precision function synapse0x8ba4e60(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba4e60=neuron0x8b9fb80(x)*(-0.412416)
      end

      double precision function synapse0x8ba4e88(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba4e88=neuron0x8b9fd58(x)*(-0.0465749)
      end

      double precision function synapse0x8ba4eb0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba4eb0=neuron0x8b9ff30(x)*(0.0473863)
      end

      double precision function synapse0x8ba4ed8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba4ed8=neuron0x8ba0108(x)*(-0.0988909)
      end

      double precision function synapse0x8ba4f00(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba4f00=neuron0x8ba02e0(x)*(-0.446179)
      end

      double precision function synapse0x8ba4f28(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba4f28=neuron0x8ba04b8(x)*(-0.0340558)
      end

      double precision function synapse0x8ba4f50(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba4f50=neuron0x8ba06a8(x)*(-0.211673)
      end

      double precision function synapse0x8ba4f78(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba4f78=neuron0x8ba0898(x)*(-0.455498)
      end

      double precision function synapse0x8ba4fa0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba4fa0=neuron0x8ba0a88(x)*(-0.0921243)
      end

      double precision function synapse0x8ba4fc8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba4fc8=neuron0x8ba0c78(x)*(0.455445)
      end

      double precision function synapse0x89c7788(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x89c7788=neuron0x8ba0e68(x)*(0.303778)
      end

      double precision function synapse0x89c7348(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x89c7348=neuron0x8ba1058(x)*(0.00956873)
      end

      double precision function synapse0x89c7370(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x89c7370=neuron0x8ba1248(x)*(0.409565)
      end

      double precision function synapse0x89c7540(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x89c7540=neuron0x8ba1438(x)*(0.18915)
      end

      double precision function synapse0x89c7568(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x89c7568=neuron0x8ba1628(x)*(0.0801067)
      end

      double precision function synapse0x8ba48b0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba48b0=neuron0x8ba1818(x)*(-0.0761627)
      end

      double precision function synapse0x8ba48d8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba48d8=neuron0x8ba1b18(x)*(0.390446)
      end

      double precision function synapse0x8ba4900(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba4900=neuron0x8ba1d08(x)*(-0.366271)
      end

      double precision function synapse0x8b7dca8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8b7dca8=neuron0x8ba1ef8(x)*(0.301853)
      end

      double precision function synapse0x8b7dcd0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8b7dcd0=neuron0x8ba20e8(x)*(-0.203622)
      end

      double precision function synapse0x89c7a08(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x89c7a08=neuron0x8ba22d8(x)*(-0.0491364)
      end

      double precision function synapse0x89c7a30(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x89c7a30=neuron0x8ba24c8(x)*(0.233908)
      end

      double precision function synapse0x89c7a58(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x89c7a58=neuron0x8ba26b8(x)*(-0.18159)
      end

      double precision function synapse0x89c7a80(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x89c7a80=neuron0x8ba28a8(x)*(0.23617)
      end

      double precision function synapse0x89c7aa8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x89c7aa8=neuron0x8ba2a98(x)*(-1.75266)
      end

      double precision function synapse0x89c7ad0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x89c7ad0=neuron0x8ba2c88(x)*(-0.960705)
      end

      double precision function synapse0x89c7af8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x89c7af8=neuron0x8b9f9f0(x)*(0.325898)
      end

      double precision function synapse0x8ba4ff0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba4ff0=neuron0x8b9fb80(x)*(0.263696)
      end

      double precision function synapse0x8ba5018(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5018=neuron0x8b9fd58(x)*(0.132032)
      end

      double precision function synapse0x8ba5040(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5040=neuron0x8b9ff30(x)*(0.194925)
      end

      double precision function synapse0x8ba5068(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5068=neuron0x8ba0108(x)*(-0.172608)
      end

      double precision function synapse0x8ba5090(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5090=neuron0x8ba02e0(x)*(-0.0458847)
      end

      double precision function synapse0x8ba50b8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba50b8=neuron0x8ba04b8(x)*(-0.328044)
      end

      double precision function synapse0x8ba50e0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba50e0=neuron0x8ba06a8(x)*(-0.36284)
      end

      double precision function synapse0x8ba5108(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5108=neuron0x8ba0898(x)*(-0.247904)
      end

      double precision function synapse0x8ba5130(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5130=neuron0x8ba0a88(x)*(-0.122628)
      end

      double precision function synapse0x8ba5158(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5158=neuron0x8ba0c78(x)*(-0.0522122)
      end

      double precision function synapse0x8ba5180(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5180=neuron0x8ba0e68(x)*(-0.0939796)
      end

      double precision function synapse0x8ba51a8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba51a8=neuron0x8ba1058(x)*(-0.0664104)
      end

      double precision function synapse0x8ba51d0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba51d0=neuron0x8ba1248(x)*(-0.051941)
      end

      double precision function synapse0x8ba51f8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba51f8=neuron0x8ba1438(x)*(-0.0516786)
      end

      double precision function synapse0x8ba5220(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5220=neuron0x8ba1628(x)*(0.336215)
      end

      double precision function synapse0x8ba52d0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba52d0=neuron0x8ba1818(x)*(0.160129)
      end

      double precision function synapse0x8ba52f8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba52f8=neuron0x8ba1b18(x)*(0.201977)
      end

      double precision function synapse0x8ba5320(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5320=neuron0x8ba1d08(x)*(0.181614)
      end

      double precision function synapse0x8ba5348(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5348=neuron0x8ba1ef8(x)*(0.132444)
      end

      double precision function synapse0x8ba5370(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5370=neuron0x8ba20e8(x)*(0.096479)
      end

      double precision function synapse0x8ba5398(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5398=neuron0x8ba22d8(x)*(0.00953369)
      end

      double precision function synapse0x8ba53c0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba53c0=neuron0x8ba24c8(x)*(0.330827)
      end

      double precision function synapse0x8ba53e8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba53e8=neuron0x8ba26b8(x)*(0.0676924)
      end

      double precision function synapse0x8ba5410(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5410=neuron0x8ba28a8(x)*(-0.0307955)
      end

      double precision function synapse0x8ba5438(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5438=neuron0x8ba2a98(x)*(-0.0267607)
      end

      double precision function synapse0x8ba5460(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5460=neuron0x8ba2c88(x)*(-0.000553303)
      end

      double precision function synapse0x8ba5618(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5618=neuron0x8b9f9f0(x)*(-0.341355)
      end

      double precision function synapse0x8ba5640(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5640=neuron0x8b9fb80(x)*(0.281942)
      end

      double precision function synapse0x8ba5668(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5668=neuron0x8b9fd58(x)*(0.447172)
      end

      double precision function synapse0x8ba5690(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5690=neuron0x8b9ff30(x)*(-0.331888)
      end

      double precision function synapse0x8ba56b8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba56b8=neuron0x8ba0108(x)*(0.315836)
      end

      double precision function synapse0x8ba56e0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba56e0=neuron0x8ba02e0(x)*(-0.390168)
      end

      double precision function synapse0x8ba5708(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5708=neuron0x8ba04b8(x)*(-0.0601401)
      end

      double precision function synapse0x8ba5730(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5730=neuron0x8ba06a8(x)*(-0.301931)
      end

      double precision function synapse0x8ba5758(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5758=neuron0x8ba0898(x)*(-0.32006)
      end

      double precision function synapse0x8ba5780(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5780=neuron0x8ba0a88(x)*(0.432483)
      end

      double precision function synapse0x8ba57a8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba57a8=neuron0x8ba0c78(x)*(0.159891)
      end

      double precision function synapse0x8ba57d0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba57d0=neuron0x8ba0e68(x)*(-0.12196)
      end

      double precision function synapse0x8ba57f8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba57f8=neuron0x8ba1058(x)*(-0.570038)
      end

      double precision function synapse0x8ba5820(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5820=neuron0x8ba1248(x)*(-0.19222)
      end

      double precision function synapse0x8ba5848(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5848=neuron0x8ba1438(x)*(0.12489)
      end

      double precision function synapse0x8ba5870(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5870=neuron0x8ba1628(x)*(0.0177232)
      end

      double precision function synapse0x8ba5920(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5920=neuron0x8ba1818(x)*(-0.455668)
      end

      double precision function synapse0x8ba5948(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5948=neuron0x8ba1b18(x)*(-0.365744)
      end

      double precision function synapse0x8ba5970(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5970=neuron0x8ba1d08(x)*(0.0764771)
      end

      double precision function synapse0x8ba5998(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5998=neuron0x8ba1ef8(x)*(0.440436)
      end

      double precision function synapse0x8ba59c0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba59c0=neuron0x8ba20e8(x)*(-0.287184)
      end

      double precision function synapse0x89339c8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x89339c8=neuron0x8ba22d8(x)*(0.208308)
      end

      double precision function synapse0x89c7800(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x89c7800=neuron0x8ba24c8(x)*(0.0569996)
      end

      double precision function synapse0x89c7828(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x89c7828=neuron0x8ba26b8(x)*(0.257768)
      end

      double precision function synapse0x89c7850(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x89c7850=neuron0x8ba28a8(x)*(0.393571)
      end

      double precision function synapse0x89c7878(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x89c7878=neuron0x8ba2a98(x)*(-1.44881)
      end

      double precision function synapse0x89c78a0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x89c78a0=neuron0x8ba2c88(x)*(-0.496482)
      end

      double precision function synapse0x89c79c8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x89c79c8=neuron0x8b9f9f0(x)*(-0.41576)
      end

      double precision function synapse0x8ba1a90(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba1a90=neuron0x8b9fb80(x)*(0.368259)
      end

      double precision function synapse0x8ba1ab8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba1ab8=neuron0x8b9fd58(x)*(-0.498272)
      end

      double precision function synapse0x8ba1ae0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba1ae0=neuron0x8b9ff30(x)*(0.235496)
      end

      double precision function synapse0x8ba5fd0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5fd0=neuron0x8ba0108(x)*(0.188456)
      end

      double precision function synapse0x8ba5ff8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5ff8=neuron0x8ba02e0(x)*(0.224277)
      end

      double precision function synapse0x8ba6020(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6020=neuron0x8ba04b8(x)*(0.450063)
      end

      double precision function synapse0x8ba6048(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6048=neuron0x8ba06a8(x)*(-0.199829)
      end

      double precision function synapse0x8ba6070(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6070=neuron0x8ba0898(x)*(0.0574969)
      end

      double precision function synapse0x8ba6098(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6098=neuron0x8ba0a88(x)*(-0.00192779)
      end

      double precision function synapse0x8ba60c0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba60c0=neuron0x8ba0c78(x)*(-0.200774)
      end

      double precision function synapse0x8ba60e8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba60e8=neuron0x8ba0e68(x)*(-0.323178)
      end

      double precision function synapse0x8ba6110(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6110=neuron0x8ba1058(x)*(0.442817)
      end

      double precision function synapse0x8ba6138(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6138=neuron0x8ba1248(x)*(-0.507636)
      end

      double precision function synapse0x8ba6160(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6160=neuron0x8ba1438(x)*(0.336788)
      end

      double precision function synapse0x8ba6188(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6188=neuron0x8ba1628(x)*(0.40458)
      end

      double precision function synapse0x8ba6238(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6238=neuron0x8ba1818(x)*(-0.0220729)
      end

      double precision function synapse0x8ba6260(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6260=neuron0x8ba1b18(x)*(0.12406)
      end

      double precision function synapse0x8ba6288(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6288=neuron0x8ba1d08(x)*(-0.381275)
      end

      double precision function synapse0x8ba62b0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba62b0=neuron0x8ba1ef8(x)*(0.316392)
      end

      double precision function synapse0x8ba62d8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba62d8=neuron0x8ba20e8(x)*(0.196836)
      end

      double precision function synapse0x8ba6300(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6300=neuron0x8ba22d8(x)*(0.199231)
      end

      double precision function synapse0x8ba6328(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6328=neuron0x8ba24c8(x)*(0.279028)
      end

      double precision function synapse0x8ba6350(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6350=neuron0x8ba26b8(x)*(-0.0367043)
      end

      double precision function synapse0x8ba6378(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6378=neuron0x8ba28a8(x)*(-0.0211602)
      end

      double precision function synapse0x8ba63a0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba63a0=neuron0x8ba2a98(x)*(-1.06024)
      end

      double precision function synapse0x8ba63c8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba63c8=neuron0x8ba2c88(x)*(1.88141)
      end

      double precision function synapse0x8ba6580(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6580=neuron0x8b9f9f0(x)*(0.156752)
      end

      double precision function synapse0x8ba65a8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba65a8=neuron0x8b9fb80(x)*(-0.0323652)
      end

      double precision function synapse0x8ba65d0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba65d0=neuron0x8b9fd58(x)*(0.139572)
      end

      double precision function synapse0x8ba65f8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba65f8=neuron0x8b9ff30(x)*(0.0994379)
      end

      double precision function synapse0x8ba6620(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6620=neuron0x8ba0108(x)*(-0.293711)
      end

      double precision function synapse0x8ba6648(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6648=neuron0x8ba02e0(x)*(-0.433942)
      end

      double precision function synapse0x8ba6670(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6670=neuron0x8ba04b8(x)*(-0.40288)
      end

      double precision function synapse0x8ba6698(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6698=neuron0x8ba06a8(x)*(-0.28495)
      end

      double precision function synapse0x8ba66c0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba66c0=neuron0x8ba0898(x)*(0.229911)
      end

      double precision function synapse0x8ba66e8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba66e8=neuron0x8ba0a88(x)*(-0.186718)
      end

      double precision function synapse0x8ba6710(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6710=neuron0x8ba0c78(x)*(-0.204478)
      end

      double precision function synapse0x8ba6738(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6738=neuron0x8ba0e68(x)*(0.243337)
      end

      double precision function synapse0x8ba6760(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6760=neuron0x8ba1058(x)*(0.404864)
      end

      double precision function synapse0x8ba6788(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6788=neuron0x8ba1248(x)*(0.243137)
      end

      double precision function synapse0x8ba67b0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba67b0=neuron0x8ba1438(x)*(0.247726)
      end

      double precision function synapse0x8ba67d8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba67d8=neuron0x8ba1628(x)*(0.25275)
      end

      double precision function synapse0x8ba6888(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6888=neuron0x8ba1818(x)*(0.475473)
      end

      double precision function synapse0x8ba68b0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba68b0=neuron0x8ba1b18(x)*(-0.377163)
      end

      double precision function synapse0x8ba68d8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba68d8=neuron0x8ba1d08(x)*(0.378175)
      end

      double precision function synapse0x8ba6900(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6900=neuron0x8ba1ef8(x)*(-0.261134)
      end

      double precision function synapse0x8ba6928(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6928=neuron0x8ba20e8(x)*(0.184549)
      end

      double precision function synapse0x8ba6950(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6950=neuron0x8ba22d8(x)*(-0.494884)
      end

      double precision function synapse0x8ba6978(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6978=neuron0x8ba24c8(x)*(0.381179)
      end

      double precision function synapse0x8ba69a0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba69a0=neuron0x8ba26b8(x)*(0.00860764)
      end

      double precision function synapse0x8ba69c8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba69c8=neuron0x8ba28a8(x)*(0.310692)
      end

      double precision function synapse0x8ba69f0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba69f0=neuron0x8ba2a98(x)*(1.26516)
      end

      double precision function synapse0x8ba6a18(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6a18=neuron0x8ba2c88(x)*(-1.61491)
      end

      double precision function synapse0x8ba6bd0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6bd0=neuron0x8b9f9f0(x)*(-0.186545)
      end

      double precision function synapse0x8ba6bf8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6bf8=neuron0x8b9fb80(x)*(-0.254109)
      end

      double precision function synapse0x8ba6c20(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6c20=neuron0x8b9fd58(x)*(0.0269172)
      end

      double precision function synapse0x8ba6c48(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6c48=neuron0x8b9ff30(x)*(-0.172717)
      end

      double precision function synapse0x8ba6c70(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6c70=neuron0x8ba0108(x)*(-0.0934297)
      end

      double precision function synapse0x8ba6c98(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6c98=neuron0x8ba02e0(x)*(-0.499022)
      end

      double precision function synapse0x8ba6cc0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6cc0=neuron0x8ba04b8(x)*(0.213978)
      end

      double precision function synapse0x8ba6ce8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6ce8=neuron0x8ba06a8(x)*(0.0658566)
      end

      double precision function synapse0x8ba6d10(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6d10=neuron0x8ba0898(x)*(-0.078723)
      end

      double precision function synapse0x8ba6d38(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6d38=neuron0x8ba0a88(x)*(-0.0812947)
      end

      double precision function synapse0x8ba6d60(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6d60=neuron0x8ba0c78(x)*(-0.469538)
      end

      double precision function synapse0x8ba6d88(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6d88=neuron0x8ba0e68(x)*(0.232)
      end

      double precision function synapse0x8ba6db0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6db0=neuron0x8ba1058(x)*(0.0461328)
      end

      double precision function synapse0x8ba6dd8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6dd8=neuron0x8ba1248(x)*(-0.112159)
      end

      double precision function synapse0x8ba6e00(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6e00=neuron0x8ba1438(x)*(-0.118729)
      end

      double precision function synapse0x8ba6e28(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6e28=neuron0x8ba1628(x)*(-0.498684)
      end

      double precision function synapse0x8ba6ed8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6ed8=neuron0x8ba1818(x)*(0.21095)
      end

      double precision function synapse0x8ba6f00(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6f00=neuron0x8ba1b18(x)*(0.0407828)
      end

      double precision function synapse0x8ba6f28(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6f28=neuron0x8ba1d08(x)*(-0.132153)
      end

      double precision function synapse0x8ba6f50(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6f50=neuron0x8ba1ef8(x)*(-0.0744378)
      end

      double precision function synapse0x8ba6f78(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6f78=neuron0x8ba20e8(x)*(-0.169081)
      end

      double precision function synapse0x8ba6fa0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6fa0=neuron0x8ba22d8(x)*(0.18493)
      end

      double precision function synapse0x8ba6fc8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6fc8=neuron0x8ba24c8(x)*(0.00853669)
      end

      double precision function synapse0x8ba6ff0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba6ff0=neuron0x8ba26b8(x)*(-0.0612235)
      end

      double precision function synapse0x8ba7018(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba7018=neuron0x8ba28a8(x)*(-0.0627871)
      end

      double precision function synapse0x8ba7040(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba7040=neuron0x8ba2a98(x)*(-7.72112e-05)
      end

      double precision function synapse0x8ba7068(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba7068=neuron0x8ba2c88(x)*(0.0164225)
      end

      double precision function synapse0x8ba7240(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba7240=neuron0x8b9f9f0(x)*(-0.129423)
      end

      double precision function synapse0x8ba7268(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba7268=neuron0x8b9fb80(x)*(0.26977)
      end

      double precision function synapse0x8ba7290(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba7290=neuron0x8b9fd58(x)*(-0.194298)
      end

      double precision function synapse0x8ba72b8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba72b8=neuron0x8b9ff30(x)*(-0.011063)
      end

      double precision function synapse0x8ba72e0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba72e0=neuron0x8ba0108(x)*(0.129084)
      end

      double precision function synapse0x8ba7308(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba7308=neuron0x8ba02e0(x)*(0.550802)
      end

      double precision function synapse0x8ba7330(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba7330=neuron0x8ba04b8(x)*(-0.155118)
      end

      double precision function synapse0x8ba7358(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba7358=neuron0x8ba06a8(x)*(-0.141736)
      end

      double precision function synapse0x8ba7380(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba7380=neuron0x8ba0898(x)*(-1.74905)
      end

      double precision function synapse0x8ba73a8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba73a8=neuron0x8ba0a88(x)*(-0.238317)
      end

      double precision function synapse0x8ba73d0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba73d0=neuron0x8ba0c78(x)*(0.309336)
      end

      double precision function synapse0x8ba73f8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba73f8=neuron0x8ba0e68(x)*(-0.161452)
      end

      double precision function synapse0x8ba7420(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba7420=neuron0x8ba1058(x)*(-0.15577)
      end

      double precision function synapse0x8ba7448(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba7448=neuron0x8ba1248(x)*(-3.78829)
      end

      double precision function synapse0x8ba7470(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba7470=neuron0x8ba1438(x)*(-0.100874)
      end

      double precision function synapse0x8ba7498(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba7498=neuron0x8ba1628(x)*(0.551909)
      end

      double precision function synapse0x8ba7548(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba7548=neuron0x8ba1818(x)*(-0.182996)
      end

      double precision function synapse0x8ba7570(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba7570=neuron0x8ba1b18(x)*(-0.295265)
      end

      double precision function synapse0x8ba7598(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba7598=neuron0x8ba1d08(x)*(-2.19072)
      end

      double precision function synapse0x8ba75c0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba75c0=neuron0x8ba1ef8(x)*(-0.404249)
      end

      double precision function synapse0x8ba75e8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba75e8=neuron0x8ba20e8(x)*(0.275802)
      end

      double precision function synapse0x8ba7610(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba7610=neuron0x8ba22d8(x)*(-0.328414)
      end

      double precision function synapse0x8ba7638(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba7638=neuron0x8ba24c8(x)*(-0.235894)
      end

      double precision function synapse0x8ba7660(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba7660=neuron0x8ba26b8(x)*(-0.115492)
      end

      double precision function synapse0x8ba7688(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba7688=neuron0x8ba28a8(x)*(-0.303967)
      end

      double precision function synapse0x8ba76b0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba76b0=neuron0x8ba2a98(x)*(-0.000975913)
      end

      double precision function synapse0x8ba76d8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba76d8=neuron0x8ba2c88(x)*(-0.00985096)
      end

      double precision function synapse0x8ba78b0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba78b0=neuron0x8b9f9f0(x)*(-0.433113)
      end

      double precision function synapse0x8ba78d8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba78d8=neuron0x8b9fb80(x)*(-0.0912424)
      end

      double precision function synapse0x8ba7900(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba7900=neuron0x8b9fd58(x)*(0.161767)
      end

      double precision function synapse0x8ba7928(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba7928=neuron0x8b9ff30(x)*(-0.337497)
      end

      double precision function synapse0x8ba7950(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba7950=neuron0x8ba0108(x)*(-0.0898866)
      end

      double precision function synapse0x8ba7978(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba7978=neuron0x8ba02e0(x)*(-0.427305)
      end

      double precision function synapse0x8ba79a0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba79a0=neuron0x8ba04b8(x)*(-0.0576224)
      end

      double precision function synapse0x8ba79c8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba79c8=neuron0x8ba06a8(x)*(-0.238349)
      end

      double precision function synapse0x8ba79f0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba79f0=neuron0x8ba0898(x)*(0.402586)
      end

      double precision function synapse0x8ba7a18(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba7a18=neuron0x8ba0a88(x)*(-0.358019)
      end

      double precision function synapse0x8ba7a40(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba7a40=neuron0x8ba0c78(x)*(0.408022)
      end

      double precision function synapse0x8ba7a68(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba7a68=neuron0x8ba0e68(x)*(-0.444476)
      end

      double precision function synapse0x8ba7a90(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba7a90=neuron0x8ba1058(x)*(-0.32721)
      end

      double precision function synapse0x8ba7ab8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba7ab8=neuron0x8ba1248(x)*(0.295231)
      end

      double precision function synapse0x8ba59e8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba59e8=neuron0x8ba1438(x)*(0.198907)
      end

      double precision function synapse0x8ba5a10(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5a10=neuron0x8ba1628(x)*(-0.393812)
      end

      double precision function synapse0x8ba5ac0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5ac0=neuron0x8ba1818(x)*(0.349365)
      end

      double precision function synapse0x8ba5ae8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5ae8=neuron0x8ba1b18(x)*(0.228748)
      end

      double precision function synapse0x8ba5b10(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5b10=neuron0x8ba1d08(x)*(-0.0183166)
      end

      double precision function synapse0x8ba5b38(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5b38=neuron0x8ba1ef8(x)*(0.440304)
      end

      double precision function synapse0x8ba5b60(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5b60=neuron0x8ba20e8(x)*(0.254315)
      end

      double precision function synapse0x8ba5b88(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5b88=neuron0x8ba22d8(x)*(0.00929533)
      end

      double precision function synapse0x8ba5bb0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5bb0=neuron0x8ba24c8(x)*(-0.370687)
      end

      double precision function synapse0x8ba5bd8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5bd8=neuron0x8ba26b8(x)*(0.33127)
      end

      double precision function synapse0x8ba5c00(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5c00=neuron0x8ba28a8(x)*(-0.234409)
      end

      double precision function synapse0x8ba5c28(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5c28=neuron0x8ba2a98(x)*(0.806624)
      end

      double precision function synapse0x8ba5c50(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5c50=neuron0x8ba2c88(x)*(-3.0504)
      end

      double precision function synapse0x8ba2ee8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba2ee8=neuron0x8ba2f98(x)*(-5.5854)
      end

      double precision function synapse0x8ba2f10(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba2f10=neuron0x8ba35e0(x)*(-0.299975)
      end

      double precision function synapse0x8ba2f38(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba2f38=neuron0x8ba4c88(x)*(0.195642)
      end

      double precision function synapse0x8ba5dc0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba5dc0=neuron0x89c7bb8(x)*(7.10471)
      end

      double precision function synapse0x8ba2f60(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba2f60=neuron0x8ba5488(x)*(-0.260853)
      end

      double precision function synapse0x8ba82e8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba82e8=neuron0x89c78c8(x)*(-0.0280156)
      end

      double precision function synapse0x8ba8310(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba8310=neuron0x8ba63f0(x)*(-0.00626868)
      end

      double precision function synapse0x8ba8338(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba8338=neuron0x8ba6a40(x)*(-0.361893)
      end

      double precision function synapse0x8ba8360(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba8360=neuron0x8ba7090(x)*(0.575083)
      end

      double precision function synapse0x8ba8388(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba8388=neuron0x8ba7700(x)*(-0.0426063)
      end

      double precision function synapse0x8ba8588(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba8588=neuron0x8ba2f98(x)*(1.35428)
      end

      double precision function synapse0x8ba85b0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba85b0=neuron0x8ba35e0(x)*(0.11457)
      end

      double precision function synapse0x8ba85d8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba85d8=neuron0x8ba4c88(x)*(0.166879)
      end

      double precision function synapse0x8ba8600(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba8600=neuron0x89c7bb8(x)*(-0.237741)
      end

      double precision function synapse0x8ba8628(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba8628=neuron0x8ba5488(x)*(-0.0333829)
      end

      double precision function synapse0x8ba8650(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba8650=neuron0x89c78c8(x)*(0.0583778)
      end

      double precision function synapse0x8ba8678(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba8678=neuron0x8ba63f0(x)*(0.0326881)
      end

      double precision function synapse0x8ba86a0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba86a0=neuron0x8ba6a40(x)*(10.7016)
      end

      double precision function synapse0x8ba86c8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba86c8=neuron0x8ba7090(x)*(-6.77237)
      end

      double precision function synapse0x8ba86f0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba86f0=neuron0x8ba7700(x)*(0.106134)
      end

      double precision function synapse0x8ba2e78(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba2e78=neuron0x8ba2f98(x)*(-0.0101696)
      end

      double precision function synapse0x8ba88f0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba88f0=neuron0x8ba35e0(x)*(0.538128)
      end

      double precision function synapse0x8ba8918(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba8918=neuron0x8ba4c88(x)*(0.0136202)
      end

      double precision function synapse0x8ba8940(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba8940=neuron0x89c7bb8(x)*(-0.0188615)
      end

      double precision function synapse0x8ba8968(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba8968=neuron0x8ba5488(x)*(-0.199188)
      end

      double precision function synapse0x8ba8990(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba8990=neuron0x89c78c8(x)*(0.00329828)
      end

      double precision function synapse0x8ba89b8(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba89b8=neuron0x8ba63f0(x)*(-0.026726)
      end

      double precision function synapse0x8ba89e0(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba89e0=neuron0x8ba6a40(x)*(-0.00918066)
      end

      double precision function synapse0x8ba8a08(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba8a08=neuron0x8ba7090(x)*(-0.0203866)
      end

      double precision function synapse0x8ba8a30(x)
      implicit double precision (a-h,n-z)
      double precision x(27)

      synapse0x8ba8a30=neuron0x8ba7700(x)*(0.0475111)
      end



