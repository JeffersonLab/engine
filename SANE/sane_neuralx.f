      Subroutine neuralx(x, index)
      implicit double precision (a-h,n-z)
      double precision x(12)
      double precision FixX,FixY,FixE
      common/SNEU/FixX,FixY,FixE
      double precision neural


C --- Last Layer
      if (index.eq.0) then
          neural=xneuron0xa6aa830(x);
          FixX=neural
        else
          neural=0.d0
      endif
      end
C --- First and Hidden layers
      double precision function xneuron0xa69a330(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xneuron0xa69a330 = (x(1) - 0d0)/1d0
      end
      double precision function xneuron0xa6a88d0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xneuron0xa6a88d0 = (x(2) - 0d0)/1d0
      end
      double precision function xneuron0xa6a89d0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xneuron0xa6a89d0 = (x(3) - 0d0)/1d0
      end
      double precision function xneuron0xa6a8b60(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xneuron0xa6a8b60 = (x(4) - 0d0)/1d0
      end
      double precision function xneuron0xa6a8d38(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xneuron0xa6a8d38 = (x(5) - 0d0)/1d0
      end
      double precision function xneuron0xa6a8f10(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xneuron0xa6a8f10 = (x(6) - 0d0)/1d0
      end
      double precision function xneuron0xa6a90e8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xneuron0xa6a90e8 = (x(7) - 0d0)/1d0
      end
      double precision function xneuron0xa6a92e0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xneuron0xa6a92e0 = (x(8) - 0d0)/1d0
      end
      double precision function xneuron0xa6a94d8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xneuron0xa6a94d8 = (x(9) - 0d0)/1d0
      end
      double precision function xneuron0xa6a96d0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xneuron0xa6a96d0 = (x(10) - 0d0)/1d0
      end
      double precision function xneuron0xa6a98c8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xneuron0xa6a98c8 = (x(11) - 0d0)/1d0
      end
      double precision function xneuron0xa6a9aa0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xneuron0xa6a9aa0 = (x(12) - 0d0)/1d0
      end
      double precision function xneuron0xa6a9db0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xneuron0xa6a9db0 = -0.00913854d0
      xneuron0xa6a9db0 = xneuron0xa6a9db0 + xsynapse0xa69a1b0(x)
      xneuron0xa6a9db0 = xneuron0xa6a9db0 + xsynapse0xa6a9f40(x)
      xneuron0xa6a9db0 = xneuron0xa6a9db0 + xsynapse0xa6a9f68(x)
      xneuron0xa6a9db0 = xneuron0xa6a9db0 + xsynapse0xa6a9f90(x)
      xneuron0xa6a9db0 = xneuron0xa6a9db0 + xsynapse0xa6a9fb8(x)
      xneuron0xa6a9db0 = xneuron0xa6a9db0 + xsynapse0xa6a9fe0(x)
      xneuron0xa6a9db0 = xneuron0xa6a9db0 + xsynapse0xa6aa008(x)
      xneuron0xa6a9db0 = xneuron0xa6a9db0 + xsynapse0xa6aa030(x)
      xneuron0xa6a9db0 = xneuron0xa6a9db0 + xsynapse0xa6aa058(x)
      xneuron0xa6a9db0 = xneuron0xa6a9db0 + xsynapse0xa6aa080(x)
      xneuron0xa6a9db0 = xneuron0xa6a9db0 + xsynapse0xa6aa0a8(x)
      xneuron0xa6a9db0 = xneuron0xa6a9db0 + xsynapse0xa6aa0d0(x)
      xneuron0xa6a9db0= (exp(-xneuron0xa6a9db0*xneuron0xa6a9db0))
      end
      double precision function xneuron0xa6aa0f8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xneuron0xa6aa0f8 = -0.388704d0
      xneuron0xa6aa0f8 = xneuron0xa6aa0f8 + xsynapse0xa6aa2d0(x)
      xneuron0xa6aa0f8 = xneuron0xa6aa0f8 + xsynapse0xa6aa2f8(x)
      xneuron0xa6aa0f8 = xneuron0xa6aa0f8 + xsynapse0xa6aa320(x)
      xneuron0xa6aa0f8 = xneuron0xa6aa0f8 + xsynapse0xa6aa348(x)
      xneuron0xa6aa0f8 = xneuron0xa6aa0f8 + xsynapse0xa6aa370(x)
      xneuron0xa6aa0f8 = xneuron0xa6aa0f8 + xsynapse0xa6aa420(x)
      xneuron0xa6aa0f8 = xneuron0xa6aa0f8 + xsynapse0xa6aa448(x)
      xneuron0xa6aa0f8 = xneuron0xa6aa0f8 + xsynapse0xa6aa470(x)
      xneuron0xa6aa0f8 = xneuron0xa6aa0f8 + xsynapse0xa6aa498(x)
      xneuron0xa6aa0f8 = xneuron0xa6aa0f8 + xsynapse0xa6aa4c0(x)
      xneuron0xa6aa0f8 = xneuron0xa6aa0f8 + xsynapse0xa6aa4e8(x)
      xneuron0xa6aa0f8 = xneuron0xa6aa0f8 + xsynapse0xa6aa510(x)
      xneuron0xa6aa0f8= (exp(-xneuron0xa6aa0f8*xneuron0xa6aa0f8))
      end
      double precision function xneuron0xa6aa538(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xneuron0xa6aa538 = 0.285332d0
      xneuron0xa6aa538 = xneuron0xa6aa538 + xsynapse0xa6aa6c8(x)
      xneuron0xa6aa538 = xneuron0xa6aa538 + xsynapse0xa6aa6f0(x)
      xneuron0xa6aa538 = xneuron0xa6aa538 + xsynapse0xa6aa718(x)
      xneuron0xa6aa538 = xneuron0xa6aa538 + xsynapse0xa6aa740(x)
      xneuron0xa6aa538 = xneuron0xa6aa538 + xsynapse0xa6aa768(x)
      xneuron0xa6aa538 = xneuron0xa6aa538 + xsynapse0xa6aa790(x)
      xneuron0xa6aa538 = xneuron0xa6aa538 + xsynapse0xa6aa7b8(x)
      xneuron0xa6aa538 = xneuron0xa6aa538 + xsynapse0xa6aa7e0(x)
      xneuron0xa6aa538 = xneuron0xa6aa538 + xsynapse0xa6aa808(x)
      xneuron0xa6aa538 = xneuron0xa6aa538 + xsynapse0xa6aa398(x)
      xneuron0xa6aa538 = xneuron0xa6aa538 + xsynapse0xa6aa3c0(x)
      xneuron0xa6aa538 = xneuron0xa6aa538 + xsynapse0xa6aa3e8(x)
      xneuron0xa6aa538= (exp(-xneuron0xa6aa538*xneuron0xa6aa538))
      end
      double precision function xneuron0xa6aa938(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xneuron0xa6aa938 = 3.38441d0
      xneuron0xa6aa938 = xneuron0xa6aa938 + xsynapse0xa6aab10(x)
      xneuron0xa6aa938 = xneuron0xa6aa938 + xsynapse0xa6aab38(x)
      xneuron0xa6aa938 = xneuron0xa6aa938 + xsynapse0xa6aab60(x)
      xneuron0xa6aa938 = xneuron0xa6aa938 + xsynapse0xa6aab88(x)
      xneuron0xa6aa938 = xneuron0xa6aa938 + xsynapse0xa6aabb0(x)
      xneuron0xa6aa938 = xneuron0xa6aa938 + xsynapse0xa6aabd8(x)
      xneuron0xa6aa938 = xneuron0xa6aa938 + xsynapse0xa6aac00(x)
      xneuron0xa6aa938 = xneuron0xa6aa938 + xsynapse0xa6aac28(x)
      xneuron0xa6aa938 = xneuron0xa6aa938 + xsynapse0xa6aac50(x)
      xneuron0xa6aa938 = xneuron0xa6aa938 + xsynapse0xa6aac78(x)
      xneuron0xa6aa938 = xneuron0xa6aa938 + xsynapse0xa6aaca0(x)
      xneuron0xa6aa938 = xneuron0xa6aa938 + xsynapse0xa6aacc8(x)
      xneuron0xa6aa938= (exp(-xneuron0xa6aa938*xneuron0xa6aa938))
      end
      double precision function xneuron0xa6aacf0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xneuron0xa6aacf0 = -0.0808855d0
      xneuron0xa6aacf0 = xneuron0xa6aacf0 + xsynapse0xa5c7670(x)
      xneuron0xa6aacf0 = xneuron0xa6aacf0 + xsynapse0xa5c7698(x)
      xneuron0xa6aacf0 = xneuron0xa6aacf0 + xsynapse0xa6aaf50(x)
      xneuron0xa6aacf0 = xneuron0xa6aacf0 + xsynapse0xa6aaf78(x)
      xneuron0xa6aacf0 = xneuron0xa6aacf0 + xsynapse0xa6aafa0(x)
      xneuron0xa6aacf0 = xneuron0xa6aacf0 + xsynapse0xa6aafc8(x)
      xneuron0xa6aacf0 = xneuron0xa6aacf0 + xsynapse0xa6aaff0(x)
      xneuron0xa6aacf0 = xneuron0xa6aacf0 + xsynapse0xa6ab018(x)
      xneuron0xa6aacf0 = xneuron0xa6aacf0 + xsynapse0xa6ab040(x)
      xneuron0xa6aacf0 = xneuron0xa6aacf0 + xsynapse0xa6ab068(x)
      xneuron0xa6aacf0 = xneuron0xa6aacf0 + xsynapse0xa6ab090(x)
      xneuron0xa6aacf0 = xneuron0xa6aacf0 + xsynapse0xa6ab0b8(x)
      xneuron0xa6aacf0= (exp(-xneuron0xa6aacf0*xneuron0xa6aacf0))
      end
      double precision function xneuron0xa6ab0e0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xneuron0xa6ab0e0 = 0.593379d0
      xneuron0xa6ab0e0 = xneuron0xa6ab0e0 + xsynapse0xa6ab2b8(x)
      xneuron0xa6ab0e0 = xneuron0xa6ab0e0 + xsynapse0xa6ab2e0(x)
      xneuron0xa6ab0e0 = xneuron0xa6ab0e0 + xsynapse0xa6ab308(x)
      xneuron0xa6ab0e0 = xneuron0xa6ab0e0 + xsynapse0xa6ab330(x)
      xneuron0xa6ab0e0 = xneuron0xa6ab0e0 + xsynapse0xa6ab358(x)
      xneuron0xa6ab0e0 = xneuron0xa6ab0e0 + xsynapse0xa5dcb70(x)
      xneuron0xa6ab0e0 = xneuron0xa6ab0e0 + xsynapse0xa674af0(x)
      xneuron0xa6ab0e0 = xneuron0xa6ab0e0 + xsynapse0xa69a0f8(x)
      xneuron0xa6ab0e0 = xneuron0xa6ab0e0 + xsynapse0xa69a120(x)
      xneuron0xa6ab0e0 = xneuron0xa6ab0e0 + xsynapse0xa5dcbe8(x)
      xneuron0xa6ab0e0 = xneuron0xa6ab0e0 + xsynapse0xa5dcc10(x)
      xneuron0xa6ab0e0 = xneuron0xa6ab0e0 + xsynapse0xa5dcd48(x)
      xneuron0xa6ab0e0= (exp(-xneuron0xa6ab0e0*xneuron0xa6ab0e0))
      end
      double precision function xneuron0xa6aa830(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xneuron0xa6aa830 = -4.23201d0
      xneuron0xa6aa830 = xneuron0xa6aa830 + xsynapse0xa6a9d20(x)
      xneuron0xa6aa830 = xneuron0xa6aa830 + xsynapse0xa6a9d48(x)
      xneuron0xa6aa830 = xneuron0xa6aa830 + xsynapse0xa6a9d70(x)
      xneuron0xa6aa830 = xneuron0xa6aa830 + xsynapse0xa5dcde0(x)
      xneuron0xa6aa830 = xneuron0xa6aa830 + xsynapse0xa6ab588(x)
      xneuron0xa6aa830 = xneuron0xa6aa830 + xsynapse0xa6ab5b0(x)
      end
C --- Xsynapses
      double precision function xsynapse0xa69a1b0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa69a1b0=xneuron0xa69a330(x)*(-0.0503392d0)
      end

      double precision function xsynapse0xa6a9f40(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6a9f40=xneuron0xa6a88d0(x)*0.27409d0
      end

      double precision function xsynapse0xa6a9f68(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6a9f68=xneuron0xa6a89d0(x)*0.17392d0
      end

      double precision function xsynapse0xa6a9f90(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6a9f90=xneuron0xa6a8b60(x)*(-0.269738d0)
      end

      double precision function xsynapse0xa6a9fb8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6a9fb8=xneuron0xa6a8d38(x)*0.0478236d0
      end

      double precision function xsynapse0xa6a9fe0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6a9fe0=xneuron0xa6a8f10(x)*(-0.246548d0)
      end

      double precision function xsynapse0xa6aa008(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aa008=xneuron0xa6a90e8(x)*0.011385d0
      end

      double precision function xsynapse0xa6aa030(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aa030=xneuron0xa6a92e0(x)*(-0.727645d0)
      end

      double precision function xsynapse0xa6aa058(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aa058=xneuron0xa6a94d8(x)*0.522848d0
      end

      double precision function xsynapse0xa6aa080(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aa080=xneuron0xa6a96d0(x)*0.135552d0
      end

      double precision function xsynapse0xa6aa0a8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aa0a8=xneuron0xa6a98c8(x)*0.00578097d0
      end

      double precision function xsynapse0xa6aa0d0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aa0d0=xneuron0xa6a9aa0(x)*(-1.01886d0)
      end

      double precision function xsynapse0xa6aa2d0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aa2d0=xneuron0xa69a330(x)*0.176288d0
      end

      double precision function xsynapse0xa6aa2f8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aa2f8=xneuron0xa6a88d0(x)*(-0.325297d0)
      end

      double precision function xsynapse0xa6aa320(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aa320=xneuron0xa6a89d0(x)*0.125128d0
      end

      double precision function xsynapse0xa6aa348(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aa348=xneuron0xa6a8b60(x)*(-0.181176d0)
      end

      double precision function xsynapse0xa6aa370(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aa370=xneuron0xa6a8d38(x)*(-0.754296d0)
      end

      double precision function xsynapse0xa6aa420(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aa420=xneuron0xa6a8f10(x)*(-0.305047d0)
      end

      double precision function xsynapse0xa6aa448(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aa448=xneuron0xa6a90e8(x)*(-0.396088d0)
      end

      double precision function xsynapse0xa6aa470(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aa470=xneuron0xa6a92e0(x)*(-0.255156d0)
      end

      double precision function xsynapse0xa6aa498(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aa498=xneuron0xa6a94d8(x)*0.141111d0
      end

      double precision function xsynapse0xa6aa4c0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aa4c0=xneuron0xa6a96d0(x)*(-0.0242659d0)
      end

      double precision function xsynapse0xa6aa4e8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aa4e8=xneuron0xa6a98c8(x)*(-0.0092789d0)
      end

      double precision function xsynapse0xa6aa510(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aa510=xneuron0xa6a9aa0(x)*1.27804d0
      end

      double precision function xsynapse0xa6aa6c8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aa6c8=xneuron0xa69a330(x)*(-0.0273203d0)
      end

      double precision function xsynapse0xa6aa6f0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aa6f0=xneuron0xa6a88d0(x)*0.086335d0
      end

      double precision function xsynapse0xa6aa718(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aa718=xneuron0xa6a89d0(x)*(-0.444821d0)
      end

      double precision function xsynapse0xa6aa740(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aa740=xneuron0xa6a8b60(x)*0.45238d0
      end

      double precision function xsynapse0xa6aa768(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aa768=xneuron0xa6a8d38(x)*7.32853e-27
      end

      double precision function xsynapse0xa6aa790(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aa790=xneuron0xa6a8f10(x)*(-1.14499e-25)
      end

      double precision function xsynapse0xa6aa7b8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aa7b8=xneuron0xa6a90e8(x)*4.44365e-24
      end

      double precision function xsynapse0xa6aa7e0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aa7e0=xneuron0xa6a92e0(x)*0.0459266d0
      end

      double precision function xsynapse0xa6aa808(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aa808=xneuron0xa6a94d8(x)*(-0.399292d0)
      end

      double precision function xsynapse0xa6aa398(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aa398=xneuron0xa6a96d0(x)*0.197827d0
      end

      double precision function xsynapse0xa6aa3c0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aa3c0=xneuron0xa6a98c8(x)*0.027554d0
      end

      double precision function xsynapse0xa6aa3e8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aa3e8=xneuron0xa6a9aa0(x)*0.000340529d0
      end

      double precision function xsynapse0xa6aab10(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aab10=xneuron0xa69a330(x)*(-0.223927d0)
      end

      double precision function xsynapse0xa6aab38(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aab38=xneuron0xa6a88d0(x)*(-0.897926d0)
      end

      double precision function xsynapse0xa6aab60(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aab60=xneuron0xa6a89d0(x)*0.0272654d0
      end

      double precision function xsynapse0xa6aab88(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aab88=xneuron0xa6a8b60(x)*0.404128d0
      end

      double precision function xsynapse0xa6aabb0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aabb0=xneuron0xa6a8d38(x)*0.00817986d0
      end

      double precision function xsynapse0xa6aabd8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aabd8=xneuron0xa6a8f10(x)*(-0.06808d0)
      end

      double precision function xsynapse0xa6aac00(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aac00=xneuron0xa6a90e8(x)*0.077987d0
      end

      double precision function xsynapse0xa6aac28(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aac28=xneuron0xa6a92e0(x)*0.105075d0
      end

      double precision function xsynapse0xa6aac50(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aac50=xneuron0xa6a94d8(x)*(-0.160608d0)
      end

      double precision function xsynapse0xa6aac78(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aac78=xneuron0xa6a96d0(x)*(-0.0419697d0)
      end

      double precision function xsynapse0xa6aaca0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aaca0=xneuron0xa6a98c8(x)*(-0.0568695d0)
      end

      double precision function xsynapse0xa6aacc8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aacc8=xneuron0xa6a9aa0(x)*0.000674584d0
      end

      double precision function xsynapse0xa5c7670(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa5c7670=xneuron0xa69a330(x)*0.417689d0
      end

      double precision function xsynapse0xa5c7698(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa5c7698=xneuron0xa6a88d0(x)*0.0212615d0
      end

      double precision function xsynapse0xa6aaf50(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aaf50=xneuron0xa6a89d0(x)*(-0.212064d0)
      end

      double precision function xsynapse0xa6aaf78(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aaf78=xneuron0xa6a8b60(x)*(-0.179474d0)
      end

      double precision function xsynapse0xa6aafa0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aafa0=xneuron0xa6a8d38(x)*(-0.0195164d0)
      end

      double precision function xsynapse0xa6aafc8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aafc8=xneuron0xa6a8f10(x)*(-0.101727d0)
      end

      double precision function xsynapse0xa6aaff0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6aaff0=xneuron0xa6a90e8(x)*(-0.313517d0)
      end

      double precision function xsynapse0xa6ab018(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6ab018=xneuron0xa6a92e0(x)*(-0.234398d0)
      end

      double precision function xsynapse0xa6ab040(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6ab040=xneuron0xa6a94d8(x)*0.160242d0
      end

      double precision function xsynapse0xa6ab068(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6ab068=xneuron0xa6a96d0(x)*(-0.134165d0)
      end

      double precision function xsynapse0xa6ab090(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6ab090=xneuron0xa6a98c8(x)*0.108124d0
      end

      double precision function xsynapse0xa6ab0b8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6ab0b8=xneuron0xa6a9aa0(x)*(-2.05161d0)
      end

      double precision function xsynapse0xa6ab2b8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6ab2b8=xneuron0xa69a330(x)*0.0919557d0
      end

      double precision function xsynapse0xa6ab2e0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6ab2e0=xneuron0xa6a88d0(x)*(-0.925804d0)
      end

      double precision function xsynapse0xa6ab308(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6ab308=xneuron0xa6a89d0(x)*(-0.172087d0)
      end

      double precision function xsynapse0xa6ab330(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6ab330=xneuron0xa6a8b60(x)*0.291603d0
      end

      double precision function xsynapse0xa6ab358(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6ab358=xneuron0xa6a8d38(x)*0.021599d0
      end

      double precision function xsynapse0xa5dcb70(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa5dcb70=xneuron0xa6a8f10(x)*(-0.159215d0)
      end

      double precision function xsynapse0xa674af0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa674af0=xneuron0xa6a90e8(x)*(-0.0313808d0)
      end

      double precision function xsynapse0xa69a0f8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa69a0f8=xneuron0xa6a92e0(x)*0.0578194d0
      end

      double precision function xsynapse0xa69a120(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa69a120=xneuron0xa6a94d8(x)*0.592353d0
      end

      double precision function xsynapse0xa5dcbe8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa5dcbe8=xneuron0xa6a96d0(x)*(-0.251381d0)
      end

      double precision function xsynapse0xa5dcc10(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa5dcc10=xneuron0xa6a98c8(x)*1.04733d0
      end

      double precision function xsynapse0xa5dcd48(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa5dcd48=xneuron0xa6a9aa0(x)*(-0.000330729d0)
      end

      double precision function xsynapse0xa6a9d20(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6a9d20=xneuron0xa6a9db0(x)*(-1.48042d0)
      end

      double precision function xsynapse0xa6a9d48(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6a9d48=xneuron0xa6aa0f8(x)*0.0280956d0
      end

      double precision function xsynapse0xa6a9d70(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6a9d70=xneuron0xa6aa538(x)*7.48644d0
      end

      double precision function xsynapse0xa5dcde0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa5dcde0=xneuron0xa6aa938(x)*5.70234d0
      end

      double precision function xsynapse0xa6ab588(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6ab588=xneuron0xa6aacf0(x)*0.892471d0
      end

      double precision function xsynapse0xa6ab5b0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      xsynapse0xa6ab5b0=xneuron0xa6ab0e0(x)*(-5.31502d0)
      end

