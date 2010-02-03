      Subroutine neuraly(x, index)
      implicit double precision (a-h,n-z)
      double precision x(12)
      double precision FixX,FixY,FixE
      common/SNEU/FixX,FixY,FixE
      double precision neural


C --- Last Layer
      if (index.eq.0) then
          neural=yneuron0x99647f0(x);
          FixY=neural
       else
          neural=0.d0
      endif
      end
C --- First and Hidden layers
      double precision function yneuron0x99542f0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      yneuron0x99542f0 = (x(1) - 0d0)/1d0
      end
      double precision function yneuron0x9962890(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      yneuron0x9962890 = (x(2) - 0d0)/1d0
      end
      double precision function yneuron0x9962990(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      yneuron0x9962990 = (x(3) - 0d0)/1d0
      end
      double precision function yneuron0x9962b20(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      yneuron0x9962b20 = (x(4) - 0d0)/1d0
      end
      double precision function yneuron0x9962cf8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      yneuron0x9962cf8 = (x(5) - 0d0)/1d0
      end
      double precision function yneuron0x9962ed0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      yneuron0x9962ed0 = (x(6) - 0d0)/1d0
      end
      double precision function yneuron0x99630a8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      yneuron0x99630a8 = (x(7) - 0d0)/1d0
      end
      double precision function yneuron0x99632a0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      yneuron0x99632a0 = (x(8) - 0d0)/1d0
      end
      double precision function yneuron0x9963498(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      yneuron0x9963498 = (x(9) - 0d0)/1d0
      end
      double precision function yneuron0x9963690(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      yneuron0x9963690 = (x(10) - 0d0)/1d0
      end
      double precision function yneuron0x9963888(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      yneuron0x9963888 = (x(11) - 0d0)/1d0
      end
      double precision function yneuron0x9963a60(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      yneuron0x9963a60 = (x(12) - 0d0)/1d0
      end
      double precision function yneuron0x9963d70(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      yneuron0x9963d70 = 2.92236d0
      yneuron0x9963d70 = yneuron0x9963d70 + ysynapse0x9954170(x)
      yneuron0x9963d70 = yneuron0x9963d70 + ysynapse0x9963f00(x)
      yneuron0x9963d70 = yneuron0x9963d70 + ysynapse0x9963f28(x)
      yneuron0x9963d70 = yneuron0x9963d70 + ysynapse0x9963f50(x)
      yneuron0x9963d70 = yneuron0x9963d70 + ysynapse0x9963f78(x)
      yneuron0x9963d70 = yneuron0x9963d70 + ysynapse0x9963fa0(x)
      yneuron0x9963d70 = yneuron0x9963d70 + ysynapse0x9963fc8(x)
      yneuron0x9963d70 = yneuron0x9963d70 + ysynapse0x9963ff0(x)
      yneuron0x9963d70 = yneuron0x9963d70 + ysynapse0x9964018(x)
      yneuron0x9963d70 = yneuron0x9963d70 + ysynapse0x9964040(x)
      yneuron0x9963d70 = yneuron0x9963d70 + ysynapse0x9964068(x)
      yneuron0x9963d70 = yneuron0x9963d70 + ysynapse0x9964090(x)
      yneuron0x9963d70= (exp(-yneuron0x9963d70*yneuron0x9963d70))
      end
      double precision function yneuron0x99640b8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      yneuron0x99640b8 = -0.16224d0
      yneuron0x99640b8 = yneuron0x99640b8 + ysynapse0x9964290(x)
      yneuron0x99640b8 = yneuron0x99640b8 + ysynapse0x99642b8(x)
      yneuron0x99640b8 = yneuron0x99640b8 + ysynapse0x99642e0(x)
      yneuron0x99640b8 = yneuron0x99640b8 + ysynapse0x9964308(x)
      yneuron0x99640b8 = yneuron0x99640b8 + ysynapse0x9964330(x)
      yneuron0x99640b8 = yneuron0x99640b8 + ysynapse0x99643e0(x)
      yneuron0x99640b8 = yneuron0x99640b8 + ysynapse0x9964408(x)
      yneuron0x99640b8 = yneuron0x99640b8 + ysynapse0x9964430(x)
      yneuron0x99640b8 = yneuron0x99640b8 + ysynapse0x9964458(x)
      yneuron0x99640b8 = yneuron0x99640b8 + ysynapse0x9964480(x)
      yneuron0x99640b8 = yneuron0x99640b8 + ysynapse0x99644a8(x)
      yneuron0x99640b8 = yneuron0x99640b8 + ysynapse0x99644d0(x)
      yneuron0x99640b8= (exp(-yneuron0x99640b8*yneuron0x99640b8))
      end
      double precision function yneuron0x99644f8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      yneuron0x99644f8 = 0.572839d0
      yneuron0x99644f8 = yneuron0x99644f8 + ysynapse0x9964688(x)
      yneuron0x99644f8 = yneuron0x99644f8 + ysynapse0x99646b0(x)
      yneuron0x99644f8 = yneuron0x99644f8 + ysynapse0x99646d8(x)
      yneuron0x99644f8 = yneuron0x99644f8 + ysynapse0x9964700(x)
      yneuron0x99644f8 = yneuron0x99644f8 + ysynapse0x9964728(x)
      yneuron0x99644f8 = yneuron0x99644f8 + ysynapse0x9964750(x)
      yneuron0x99644f8 = yneuron0x99644f8 + ysynapse0x9964778(x)
      yneuron0x99644f8 = yneuron0x99644f8 + ysynapse0x99647a0(x)
      yneuron0x99644f8 = yneuron0x99644f8 + ysynapse0x99647c8(x)
      yneuron0x99644f8 = yneuron0x99644f8 + ysynapse0x9964358(x)
      yneuron0x99644f8 = yneuron0x99644f8 + ysynapse0x9964380(x)
      yneuron0x99644f8 = yneuron0x99644f8 + ysynapse0x99643a8(x)
      yneuron0x99644f8= (exp(-yneuron0x99644f8*yneuron0x99644f8))
      end
      double precision function yneuron0x99648f8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      yneuron0x99648f8 = 0.221914d0
      yneuron0x99648f8 = yneuron0x99648f8 + ysynapse0x9964ad0(x)
      yneuron0x99648f8 = yneuron0x99648f8 + ysynapse0x9964af8(x)
      yneuron0x99648f8 = yneuron0x99648f8 + ysynapse0x9964b20(x)
      yneuron0x99648f8 = yneuron0x99648f8 + ysynapse0x9964b48(x)
      yneuron0x99648f8 = yneuron0x99648f8 + ysynapse0x9964b70(x)
      yneuron0x99648f8 = yneuron0x99648f8 + ysynapse0x9964b98(x)
      yneuron0x99648f8 = yneuron0x99648f8 + ysynapse0x9964bc0(x)
      yneuron0x99648f8 = yneuron0x99648f8 + ysynapse0x9964be8(x)
      yneuron0x99648f8 = yneuron0x99648f8 + ysynapse0x9964c10(x)
      yneuron0x99648f8 = yneuron0x99648f8 + ysynapse0x9964c38(x)
      yneuron0x99648f8 = yneuron0x99648f8 + ysynapse0x9964c60(x)
      yneuron0x99648f8 = yneuron0x99648f8 + ysynapse0x9964c88(x)
      yneuron0x99648f8= (exp(-yneuron0x99648f8*yneuron0x99648f8))
      end
      double precision function yneuron0x9964cb0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      yneuron0x9964cb0 = 0.649237d0
      yneuron0x9964cb0 = yneuron0x9964cb0 + ysynapse0x9964f10(x)
      yneuron0x9964cb0 = yneuron0x9964cb0 + ysynapse0x9964f38(x)
      yneuron0x9964cb0 = yneuron0x9964cb0 + ysynapse0x9964f60(x)
      yneuron0x9964cb0 = yneuron0x9964cb0 + ysynapse0x9964f88(x)
      yneuron0x9964cb0 = yneuron0x9964cb0 + ysynapse0x9964fb0(x)
      yneuron0x9964cb0 = yneuron0x9964cb0 + ysynapse0x9964fd8(x)
      yneuron0x9964cb0 = yneuron0x9964cb0 + ysynapse0x9965000(x)
      yneuron0x9964cb0 = yneuron0x9964cb0 + ysynapse0x9965028(x)
      yneuron0x9964cb0 = yneuron0x9964cb0 + ysynapse0x9965050(x)
      yneuron0x9964cb0 = yneuron0x9964cb0 + ysynapse0x9965078(x)
      yneuron0x9964cb0 = yneuron0x9964cb0 + ysynapse0x99650a0(x)
      yneuron0x9964cb0 = yneuron0x9964cb0 + ysynapse0x99650c8(x)
      yneuron0x9964cb0= (exp(-yneuron0x9964cb0*yneuron0x9964cb0))
      end
      double precision function yneuron0x99650f0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      yneuron0x99650f0 = 0.628455d0
      yneuron0x99650f0 = yneuron0x99650f0 + ysynapse0x9965280(x)
      yneuron0x99650f0 = yneuron0x99650f0 + ysynapse0x99652a8(x)
      yneuron0x99650f0 = yneuron0x99650f0 + ysynapse0x99652d0(x)
      yneuron0x99650f0 = yneuron0x99650f0 + ysynapse0x99652f8(x)
      yneuron0x99650f0 = yneuron0x99650f0 + ysynapse0x9965320(x)
      yneuron0x99650f0 = yneuron0x99650f0 + ysynapse0x9896b30(x)
      yneuron0x99650f0 = yneuron0x99650f0 + ysynapse0x992eab0(x)
      yneuron0x99650f0 = yneuron0x99650f0 + ysynapse0x99540b8(x)
      yneuron0x99650f0 = yneuron0x99650f0 + ysynapse0x99540e0(x)
      yneuron0x99650f0 = yneuron0x99650f0 + ysynapse0x9896ba8(x)
      yneuron0x99650f0 = yneuron0x99650f0 + ysynapse0x9896bd0(x)
      yneuron0x99650f0 = yneuron0x99650f0 + ysynapse0x9896d08(x)
      yneuron0x99650f0= (exp(-yneuron0x99650f0*yneuron0x99650f0))
      end
      double precision function yneuron0x99647f0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      yneuron0x99647f0 = 3.6538d0
      yneuron0x99647f0 = yneuron0x99647f0 + ysynapse0x9963ce0(x)
      yneuron0x99647f0 = yneuron0x99647f0 + ysynapse0x9963d08(x)
      yneuron0x99647f0 = yneuron0x99647f0 + ysynapse0x9963d30(x)
      yneuron0x99647f0 = yneuron0x99647f0 + ysynapse0x9896da0(x)
      yneuron0x99647f0 = yneuron0x99647f0 + ysynapse0x9965550(x)
      yneuron0x99647f0 = yneuron0x99647f0 + ysynapse0x9965578(x)
      end
C --- Ysynapses
      double precision function ysynapse0x9954170(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9954170=yneuron0x99542f0(x)*0.84014d0
      end

      double precision function ysynapse0x9963f00(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9963f00=yneuron0x9962890(x)*(-2.42124d0)
      end

      double precision function ysynapse0x9963f28(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9963f28=yneuron0x9962990(x)*0.122826d0
      end

      double precision function ysynapse0x9963f50(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9963f50=yneuron0x9962b20(x)*(-0.320832d0)
      end

      double precision function ysynapse0x9963f78(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9963f78=yneuron0x9962cf8(x)*(-0.0576504d0)
      end

      double precision function ysynapse0x9963fa0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9963fa0=yneuron0x9962ed0(x)*0.462672d0
      end

      double precision function ysynapse0x9963fc8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9963fc8=yneuron0x99630a8(x)*(-0.299188d0)
      end

      double precision function ysynapse0x9963ff0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9963ff0=yneuron0x99632a0(x)*0.0633701d0
      end

      double precision function ysynapse0x9964018(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9964018=yneuron0x9963498(x)*(-0.034495d0)
      end

      double precision function ysynapse0x9964040(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9964040=yneuron0x9963690(x)*0.00633377d0
      end

      double precision function ysynapse0x9964068(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9964068=yneuron0x9963888(x)*(-0.00559591d0)
      end

      double precision function ysynapse0x9964090(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9964090=yneuron0x9963a60(x)*(-0.0206636d0)
      end

      double precision function ysynapse0x9964290(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9964290=yneuron0x99542f0(x)*0.0493121d0
      end

      double precision function ysynapse0x99642b8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x99642b8=yneuron0x9962890(x)*(-0.762068d0)
      end

      double precision function ysynapse0x99642e0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x99642e0=yneuron0x9962990(x)*0.123622d0
      end

      double precision function ysynapse0x9964308(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9964308=yneuron0x9962b20(x)*(-0.182683d0)
      end

      double precision function ysynapse0x9964330(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9964330=yneuron0x9962cf8(x)*0.079913d0
      end

      double precision function ysynapse0x99643e0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x99643e0=yneuron0x9962ed0(x)*(-1.6688d0)
      end

      double precision function ysynapse0x9964408(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9964408=yneuron0x99630a8(x)*0.741767d0
      end

      double precision function ysynapse0x9964430(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9964430=yneuron0x99632a0(x)*(-0.0052035d0)
      end

      double precision function ysynapse0x9964458(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9964458=yneuron0x9963498(x)*(-0.0697505d0)
      end

      double precision function ysynapse0x9964480(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9964480=yneuron0x9963690(x)*0.00288357d0
      end

      double precision function ysynapse0x99644a8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x99644a8=yneuron0x9963888(x)*0.0035394d0
      end

      double precision function ysynapse0x99644d0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x99644d0=yneuron0x9963a60(x)*0.0571609d0
      end

      double precision function ysynapse0x9964688(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9964688=yneuron0x99542f0(x)*(-0.555421d0)
      end

      double precision function ysynapse0x99646b0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x99646b0=yneuron0x9962890(x)*(-1.48975d0)
      end

      double precision function ysynapse0x99646d8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x99646d8=yneuron0x9962990(x)*(-0.225035d0)
      end

      double precision function ysynapse0x9964700(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9964700=yneuron0x9962b20(x)*0.672167d0
      end

      double precision function ysynapse0x9964728(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9964728=yneuron0x9962cf8(x)*0.25976d0
      end

      double precision function ysynapse0x9964750(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9964750=yneuron0x9962ed0(x)*1.64105d0
      end

      double precision function ysynapse0x9964778(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9964778=yneuron0x99630a8(x)*(-0.763011d0)
      end

      double precision function ysynapse0x99647a0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x99647a0=yneuron0x99632a0(x)*(-0.150097d0)
      end

      double precision function ysynapse0x99647c8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x99647c8=yneuron0x9963498(x)*(-0.64113d0)
      end

      double precision function ysynapse0x9964358(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9964358=yneuron0x9963690(x)*0.0574444d0
      end

      double precision function ysynapse0x9964380(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9964380=yneuron0x9963888(x)*2.16208d0
      end

      double precision function ysynapse0x99643a8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x99643a8=yneuron0x9963a60(x)*(-0.00224521d0)
      end

      double precision function ysynapse0x9964ad0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9964ad0=yneuron0x99542f0(x)*(-0.466552d0)
      end

      double precision function ysynapse0x9964af8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9964af8=yneuron0x9962890(x)*1.65658d0
      end

      double precision function ysynapse0x9964b20(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9964b20=yneuron0x9962990(x)*(-0.168962d0)
      end

      double precision function ysynapse0x9964b48(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9964b48=yneuron0x9962b20(x)*0.2079d0
      end

      double precision function ysynapse0x9964b70(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9964b70=yneuron0x9962cf8(x)*0.0233477d0
      end

      double precision function ysynapse0x9964b98(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9964b98=yneuron0x9962ed0(x)*(-0.200346d0)
      end

      double precision function ysynapse0x9964bc0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9964bc0=yneuron0x99630a8(x)*0.115075d0
      end

      double precision function ysynapse0x9964be8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9964be8=yneuron0x99632a0(x)*(-0.0390549d0)
      end

      double precision function ysynapse0x9964c10(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9964c10=yneuron0x9963498(x)*0.0238061d0
      end

      double precision function ysynapse0x9964c38(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9964c38=yneuron0x9963690(x)*(-0.0234839d0)
      end

      double precision function ysynapse0x9964c60(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9964c60=yneuron0x9963888(x)*0.00313816d0
      end

      double precision function ysynapse0x9964c88(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9964c88=yneuron0x9963a60(x)*0.0171926d0
      end

      double precision function ysynapse0x9964f10(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9964f10=yneuron0x99542f0(x)*(-0.309403d0)
      end

      double precision function ysynapse0x9964f38(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9964f38=yneuron0x9962890(x)*1.04939d0
      end

      double precision function ysynapse0x9964f60(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9964f60=yneuron0x9962990(x)*0.00147222d0
      end

      double precision function ysynapse0x9964f88(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9964f88=yneuron0x9962b20(x)*0.0340628d0
      end

      double precision function ysynapse0x9964fb0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9964fb0=yneuron0x9962cf8(x)*(-0.000640466d0)
      end

      double precision function ysynapse0x9964fd8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9964fd8=yneuron0x9962ed0(x)*0.013059d0
      end

      double precision function ysynapse0x9965000(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9965000=yneuron0x99630a8(x)*0.037372d0
      end

      double precision function ysynapse0x9965028(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9965028=yneuron0x99632a0(x)*(-0.0208228d0)
      end

      double precision function ysynapse0x9965050(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9965050=yneuron0x9963498(x)*0.00351809d0
      end

      double precision function ysynapse0x9965078(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9965078=yneuron0x9963690(x)*(-0.00740378d0)
      end

      double precision function ysynapse0x99650a0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x99650a0=yneuron0x9963888(x)*0.00216799d0
      end

      double precision function ysynapse0x99650c8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x99650c8=yneuron0x9963a60(x)*(-0.00451882d0)
      end

      double precision function ysynapse0x9965280(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9965280=yneuron0x99542f0(x)*(-0.0258275d0)
      end

      double precision function ysynapse0x99652a8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x99652a8=yneuron0x9962890(x)*(-0.373923d0)
      end

      double precision function ysynapse0x99652d0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x99652d0=yneuron0x9962990(x)*(-0.208665d0)
      end

      double precision function ysynapse0x99652f8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x99652f8=yneuron0x9962b20(x)*0.255024d0
      end

      double precision function ysynapse0x9965320(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9965320=yneuron0x9962cf8(x)*0.533353d0
      end

      double precision function ysynapse0x9896b30(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9896b30=yneuron0x9962ed0(x)*(-0.38576d0)
      end

      double precision function ysynapse0x992eab0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x992eab0=yneuron0x99630a8(x)*0.13661d0
      end

      double precision function ysynapse0x99540b8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x99540b8=yneuron0x99632a0(x)*(-0.0480636d0)
      end

      double precision function ysynapse0x99540e0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x99540e0=yneuron0x9963498(x)*(-0.130423d0)
      end

      double precision function ysynapse0x9896ba8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9896ba8=yneuron0x9963690(x)*(-0.0845209d0)
      end

      double precision function ysynapse0x9896bd0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9896bd0=yneuron0x9963888(x)*0.00164144d0
      end

      double precision function ysynapse0x9896d08(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9896d08=yneuron0x9963a60(x)*0.67177d0
      end

      double precision function ysynapse0x9963ce0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9963ce0=yneuron0x9963d70(x)*(-2.94957d0)
      end

      double precision function ysynapse0x9963d08(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9963d08=yneuron0x99640b8(x)*0.694062d0
      end

      double precision function ysynapse0x9963d30(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9963d30=yneuron0x99644f8(x)*(-1.38538d0)
      end

      double precision function ysynapse0x9896da0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9896da0=yneuron0x99648f8(x)*7.36586d0
      end

      double precision function ysynapse0x9965550(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9965550=yneuron0x9964cb0(x)*(-11.0034d0)
      end

      double precision function ysynapse0x9965578(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      ysynapse0x9965578=yneuron0x99650f0(x)*(-4.27204d0)
      end

