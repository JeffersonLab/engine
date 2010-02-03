      Subroutine neurale(x, index)
      implicit double precision (a-h,n-z)
      double precision x(12)
      double precision FixX,FixY,FixE
      common/SNEU/FixX,FixY,FixE
      double precision neural

C --- Last Layer
      if (index.eq.0) then
          neural=neuron0x9913808(x);
          FixE=neural
       else
          neural=0.d0
      endif
      end
C --- First and Hidden layers
      double precision function neuron0x9903308(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x9903308 = (x(1) - 0d0)/1d0
      end
      double precision function neuron0x99118a8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x99118a8 = (x(2) - 0d0)/1d0
      end
      double precision function neuron0x99119a8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x99119a8 = (x(3) - 0d0)/1d0
      end
      double precision function neuron0x9911b38(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x9911b38 = (x(4) - 0d0)/1d0
      end
      double precision function neuron0x9911d10(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x9911d10 = (x(5) - 0d0)/1d0
      end
      double precision function neuron0x9911ee8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x9911ee8 = (x(6) - 0d0)/1d0
      end
      double precision function neuron0x99120c0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x99120c0 = (x(7) - 0d0)/1d0
      end
      double precision function neuron0x99122b8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x99122b8 = (x(8) - 0d0)/1d0
      end
      double precision function neuron0x99124b0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x99124b0 = (x(9) - 0d0)/1d0
      end
      double precision function neuron0x99126a8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x99126a8 = (x(10) - 0d0)/1d0
      end
      double precision function neuron0x99128a0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x99128a0 = (x(11) - 0d0)/1d0
      end
      double precision function neuron0x9912a78(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x9912a78 = (x(12) - 0d0)/1d0
      end
      double precision function neuron0x9912d88(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x9912d88 = 1.18189d0
      neuron0x9912d88 = neuron0x9912d88 + synapse0x9903188(x)
      neuron0x9912d88 = neuron0x9912d88 + synapse0x9912f18(x)
      neuron0x9912d88 = neuron0x9912d88 + synapse0x9912f40(x)
      neuron0x9912d88 = neuron0x9912d88 + synapse0x9912f68(x)
      neuron0x9912d88 = neuron0x9912d88 + synapse0x9912f90(x)
      neuron0x9912d88 = neuron0x9912d88 + synapse0x9912fb8(x)
      neuron0x9912d88 = neuron0x9912d88 + synapse0x9912fe0(x)
      neuron0x9912d88 = neuron0x9912d88 + synapse0x9913008(x)
      neuron0x9912d88 = neuron0x9912d88 + synapse0x9913030(x)
      neuron0x9912d88 = neuron0x9912d88 + synapse0x9913058(x)
      neuron0x9912d88 = neuron0x9912d88 + synapse0x9913080(x)
      neuron0x9912d88 = neuron0x9912d88 + synapse0x99130a8(x)
      neuron0x9912d88= (exp(-neuron0x9912d88*neuron0x9912d88))
      end
      double precision function neuron0x99130d0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x99130d0 = 0.584752d0
      neuron0x99130d0 = neuron0x99130d0 + synapse0x99132a8(x)
      neuron0x99130d0 = neuron0x99130d0 + synapse0x99132d0(x)
      neuron0x99130d0 = neuron0x99130d0 + synapse0x99132f8(x)
      neuron0x99130d0 = neuron0x99130d0 + synapse0x9913320(x)
      neuron0x99130d0 = neuron0x99130d0 + synapse0x9913348(x)
      neuron0x99130d0 = neuron0x99130d0 + synapse0x99133f8(x)
      neuron0x99130d0 = neuron0x99130d0 + synapse0x9913420(x)
      neuron0x99130d0 = neuron0x99130d0 + synapse0x9913448(x)
      neuron0x99130d0 = neuron0x99130d0 + synapse0x9913470(x)
      neuron0x99130d0 = neuron0x99130d0 + synapse0x9913498(x)
      neuron0x99130d0 = neuron0x99130d0 + synapse0x99134c0(x)
      neuron0x99130d0 = neuron0x99130d0 + synapse0x99134e8(x)
      neuron0x99130d0= (exp(-neuron0x99130d0*neuron0x99130d0))
      end
      double precision function neuron0x9913510(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x9913510 = 0.274168d0
      neuron0x9913510 = neuron0x9913510 + synapse0x99136a0(x)
      neuron0x9913510 = neuron0x9913510 + synapse0x99136c8(x)
      neuron0x9913510 = neuron0x9913510 + synapse0x99136f0(x)
      neuron0x9913510 = neuron0x9913510 + synapse0x9913718(x)
      neuron0x9913510 = neuron0x9913510 + synapse0x9913740(x)
      neuron0x9913510 = neuron0x9913510 + synapse0x9913768(x)
      neuron0x9913510 = neuron0x9913510 + synapse0x9913790(x)
      neuron0x9913510 = neuron0x9913510 + synapse0x99137b8(x)
      neuron0x9913510 = neuron0x9913510 + synapse0x99137e0(x)
      neuron0x9913510 = neuron0x9913510 + synapse0x9913370(x)
      neuron0x9913510 = neuron0x9913510 + synapse0x9913398(x)
      neuron0x9913510 = neuron0x9913510 + synapse0x99133c0(x)
      neuron0x9913510= (exp(-neuron0x9913510*neuron0x9913510))
      end
      double precision function neuron0x9913910(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x9913910 = -0.502317d0
      neuron0x9913910 = neuron0x9913910 + synapse0x9913ae8(x)
      neuron0x9913910 = neuron0x9913910 + synapse0x9913b10(x)
      neuron0x9913910 = neuron0x9913910 + synapse0x9913b38(x)
      neuron0x9913910 = neuron0x9913910 + synapse0x9913b60(x)
      neuron0x9913910 = neuron0x9913910 + synapse0x9913b88(x)
      neuron0x9913910 = neuron0x9913910 + synapse0x9913bb0(x)
      neuron0x9913910 = neuron0x9913910 + synapse0x9913bd8(x)
      neuron0x9913910 = neuron0x9913910 + synapse0x9913c00(x)
      neuron0x9913910 = neuron0x9913910 + synapse0x9913c28(x)
      neuron0x9913910 = neuron0x9913910 + synapse0x9913c50(x)
      neuron0x9913910 = neuron0x9913910 + synapse0x9913c78(x)
      neuron0x9913910 = neuron0x9913910 + synapse0x9913ca0(x)
      neuron0x9913910= (exp(-neuron0x9913910*neuron0x9913910))
      end
      double precision function neuron0x9913cc8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x9913cc8 = 0.534638d0
      neuron0x9913cc8 = neuron0x9913cc8 + synapse0x9913f28(x)
      neuron0x9913cc8 = neuron0x9913cc8 + synapse0x9913f50(x)
      neuron0x9913cc8 = neuron0x9913cc8 + synapse0x9913f78(x)
      neuron0x9913cc8 = neuron0x9913cc8 + synapse0x9913fa0(x)
      neuron0x9913cc8 = neuron0x9913cc8 + synapse0x9913fc8(x)
      neuron0x9913cc8 = neuron0x9913cc8 + synapse0x9913ff0(x)
      neuron0x9913cc8 = neuron0x9913cc8 + synapse0x9914018(x)
      neuron0x9913cc8 = neuron0x9913cc8 + synapse0x9914040(x)
      neuron0x9913cc8 = neuron0x9913cc8 + synapse0x9914068(x)
      neuron0x9913cc8 = neuron0x9913cc8 + synapse0x9914090(x)
      neuron0x9913cc8 = neuron0x9913cc8 + synapse0x99140b8(x)
      neuron0x9913cc8 = neuron0x9913cc8 + synapse0x99140e0(x)
      neuron0x9913cc8= (exp(-neuron0x9913cc8*neuron0x9913cc8))
      end
      double precision function neuron0x9914108(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x9914108 = 0.303604d0
      neuron0x9914108 = neuron0x9914108 + synapse0x9914298(x)
      neuron0x9914108 = neuron0x9914108 + synapse0x99142c0(x)
      neuron0x9914108 = neuron0x9914108 + synapse0x99142e8(x)
      neuron0x9914108 = neuron0x9914108 + synapse0x9914310(x)
      neuron0x9914108 = neuron0x9914108 + synapse0x9914338(x)
      neuron0x9914108 = neuron0x9914108 + synapse0x9835970(x)
      neuron0x9914108 = neuron0x9914108 + synapse0x98ddac8(x)
      neuron0x9914108 = neuron0x9914108 + synapse0x99030d0(x)
      neuron0x9914108 = neuron0x9914108 + synapse0x99030f8(x)
      neuron0x9914108 = neuron0x9914108 + synapse0x98359e8(x)
      neuron0x9914108 = neuron0x9914108 + synapse0x9835a10(x)
      neuron0x9914108 = neuron0x9914108 + synapse0x9835b48(x)
      neuron0x9914108= (exp(-neuron0x9914108*neuron0x9914108))
      end
      double precision function neuron0x9913808(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      neuron0x9913808 = 0.15552d0
      neuron0x9913808 = neuron0x9913808 + synapse0x9912cf8(x)
      neuron0x9913808 = neuron0x9913808 + synapse0x9912d20(x)
      neuron0x9913808 = neuron0x9913808 + synapse0x9912d48(x)
      neuron0x9913808 = neuron0x9913808 + synapse0x9835be0(x)
      neuron0x9913808 = neuron0x9913808 + synapse0x9914568(x)
      neuron0x9913808 = neuron0x9913808 + synapse0x9914590(x)
      end
C --- Synapses
      double precision function synapse0x9903188(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9903188=neuron0x9903308(x)*(-0.142507d0)
      end

      double precision function synapse0x9912f18(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9912f18=neuron0x99118a8(x)*0.816146d0
      end

      double precision function synapse0x9912f40(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9912f40=neuron0x99119a8(x)*0.178152d0
      end

      double precision function synapse0x9912f68(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9912f68=neuron0x9911b38(x)*(-0.332306d0)
      end

      double precision function synapse0x9912f90(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9912f90=neuron0x9911d10(x)*(-0.425368d0)
      end

      double precision function synapse0x9912fb8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9912fb8=neuron0x9911ee8(x)*0.260873d0
      end

      double precision function synapse0x9912fe0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9912fe0=neuron0x99120c0(x)*(-0.23777d0)
      end

      double precision function synapse0x9913008(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9913008=neuron0x99122b8(x)*0.0380382d0
      end

      double precision function synapse0x9913030(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9913030=neuron0x99124b0(x)*(-0.289298d0)
      end

      double precision function synapse0x9913058(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9913058=neuron0x99126a8(x)*0.419208d0
      end

      double precision function synapse0x9913080(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9913080=neuron0x99128a0(x)*(-0.0641742d0)
      end

      double precision function synapse0x99130a8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x99130a8=neuron0x9912a78(x)*(-1.53453d0)
      end

      double precision function synapse0x99132a8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x99132a8=neuron0x9903308(x)*0.118948d0
      end

      double precision function synapse0x99132d0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x99132d0=neuron0x99118a8(x)*1.18956d0
      end

      double precision function synapse0x99132f8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x99132f8=neuron0x99119a8(x)*(-0.0409325d0)
      end

      double precision function synapse0x9913320(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9913320=neuron0x9911b38(x)*(-0.429456d0)
      end

      double precision function synapse0x9913348(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9913348=neuron0x9911d10(x)*0.00912285d0
      end

      double precision function synapse0x99133f8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x99133f8=neuron0x9911ee8(x)*(-0.214434d0)
      end

      double precision function synapse0x9913420(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9913420=neuron0x99120c0(x)*(-0.534453d0)
      end

      double precision function synapse0x9913448(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9913448=neuron0x99122b8(x)*(-0.515743d0)
      end

      double precision function synapse0x9913470(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9913470=neuron0x99124b0(x)*(-0.0292264d0)
      end

      double precision function synapse0x9913498(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9913498=neuron0x99126a8(x)*0.358938d0
      end

      double precision function synapse0x99134c0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x99134c0=neuron0x99128a0(x)*(-1.06336d0)
      end

      double precision function synapse0x99134e8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x99134e8=neuron0x9912a78(x)*(-1.18585d0)
      end

      double precision function synapse0x99136a0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x99136a0=neuron0x9903308(x)*(-0.275394d0)
      end

      double precision function synapse0x99136c8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x99136c8=neuron0x99118a8(x)*1.1115d0
      end

      double precision function synapse0x99136f0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x99136f0=neuron0x99119a8(x)*(-0.138849d0)
      end

      double precision function synapse0x9913718(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9913718=neuron0x9911b38(x)*0.542724d0
      end

      double precision function synapse0x9913740(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9913740=neuron0x9911d10(x)*(-0.143361d0)
      end

      double precision function synapse0x9913768(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9913768=neuron0x9911ee8(x)*(-0.0235586d0)
      end

      double precision function synapse0x9913790(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9913790=neuron0x99120c0(x)*0.0808385d0
      end

      double precision function synapse0x99137b8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x99137b8=neuron0x99122b8(x)*0.0400261d0
      end

      double precision function synapse0x99137e0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x99137e0=neuron0x99124b0(x)*(-0.00178169d0)
      end

      double precision function synapse0x9913370(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9913370=neuron0x99126a8(x)*(-0.131219d0)
      end

      double precision function synapse0x9913398(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9913398=neuron0x99128a0(x)*(-1.03172d0)
      end

      double precision function synapse0x99133c0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x99133c0=neuron0x9912a78(x)*(-1.30888d0)
      end

      double precision function synapse0x9913ae8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9913ae8=neuron0x9903308(x)*(-0.291488d0)
      end

      double precision function synapse0x9913b10(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9913b10=neuron0x99118a8(x)*0.965291d0
      end

      double precision function synapse0x9913b38(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9913b38=neuron0x99119a8(x)*0.175321d0
      end

      double precision function synapse0x9913b60(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9913b60=neuron0x9911b38(x)*0.072091d0
      end

      double precision function synapse0x9913b88(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9913b88=neuron0x9911d10(x)*0.0377011d0
      end

      double precision function synapse0x9913bb0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9913bb0=neuron0x9911ee8(x)*(-0.137764d0)
      end

      double precision function synapse0x9913bd8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9913bd8=neuron0x99120c0(x)*0.0993152d0
      end

      double precision function synapse0x9913c00(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9913c00=neuron0x99122b8(x)*(-0.226042d0)
      end

      double precision function synapse0x9913c28(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9913c28=neuron0x99124b0(x)*(-0.251275d0)
      end

      double precision function synapse0x9913c50(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9913c50=neuron0x99126a8(x)*0.341805d0
      end

      double precision function synapse0x9913c78(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9913c78=neuron0x99128a0(x)*(-1.28528d0)
      end

      double precision function synapse0x9913ca0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9913ca0=neuron0x9912a78(x)*(-0.000632487d0)
      end

      double precision function synapse0x9913f28(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9913f28=neuron0x9903308(x)*(-0.0786329d0)
      end

      double precision function synapse0x9913f50(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9913f50=neuron0x99118a8(x)*0.0417116d0
      end

      double precision function synapse0x9913f78(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9913f78=neuron0x99119a8(x)*(-0.189d0)
      end

      double precision function synapse0x9913fa0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9913fa0=neuron0x9911b38(x)*0.373821d0
      end

      double precision function synapse0x9913fc8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9913fc8=neuron0x9911d10(x)*0.173425d0
      end

      double precision function synapse0x9913ff0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9913ff0=neuron0x9911ee8(x)*0.0174389d0
      end

      double precision function synapse0x9914018(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9914018=neuron0x99120c0(x)*(-0.355365d0)
      end

      double precision function synapse0x9914040(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9914040=neuron0x99122b8(x)*0.0859413d0
      end

      double precision function synapse0x9914068(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9914068=neuron0x99124b0(x)*(-0.136445d0)
      end

      double precision function synapse0x9914090(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9914090=neuron0x99126a8(x)*0.345708d0
      end

      double precision function synapse0x99140b8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x99140b8=neuron0x99128a0(x)*0.544788d0
      end

      double precision function synapse0x99140e0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x99140e0=neuron0x9912a78(x)*0.528477d0
      end

      double precision function synapse0x9914298(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9914298=neuron0x9903308(x)*(-0.463249d0)
      end

      double precision function synapse0x99142c0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x99142c0=neuron0x99118a8(x)*1.29271d0
      end

      double precision function synapse0x99142e8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x99142e8=neuron0x99119a8(x)*0.297151d0
      end

      double precision function synapse0x9914310(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9914310=neuron0x9911b38(x)*0.00364563d0
      end

      double precision function synapse0x9914338(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9914338=neuron0x9911d10(x)*(-0.390268d0)
      end

      double precision function synapse0x9835970(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9835970=neuron0x9911ee8(x)*0.0243479d0
      end

      double precision function synapse0x98ddac8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x98ddac8=neuron0x99120c0(x)*0.0379929d0
      end

      double precision function synapse0x99030d0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x99030d0=neuron0x99122b8(x)*0.0576228d0
      end

      double precision function synapse0x99030f8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x99030f8=neuron0x99124b0(x)*(-0.146554d0)
      end

      double precision function synapse0x98359e8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x98359e8=neuron0x99126a8(x)*0.157743d0
      end

      double precision function synapse0x9835a10(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9835a10=neuron0x99128a0(x)*(-0.0287887d0)
      end

      double precision function synapse0x9835b48(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9835b48=neuron0x9912a78(x)*(-1.52603d0)
      end

      double precision function synapse0x9912cf8(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9912cf8=neuron0x9912d88(x)*(-1.85166d0)
      end

      double precision function synapse0x9912d20(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9912d20=neuron0x99130d0(x)*(-1.69095d0)
      end

      double precision function synapse0x9912d48(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9912d48=neuron0x9913510(x)*(-1.44296d0)
      end

      double precision function synapse0x9835be0(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9835be0=neuron0x9913910(x)*3.49481d0
      end

      double precision function synapse0x9914568(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9914568=neuron0x9913cc8(x)*(-0.0215353d0)
      end

      double precision function synapse0x9914590(x)
      implicit double precision (a-h,n-z)
      double precision x(12)

      synapse0x9914590=neuron0x9914108(x)*3.75209d0
      end

      subroutine NueralParam(i,Emax,Emt,Etot9,Etot,
     ,     xmomsqr,xmom,xmomsq,ymomsqr,ymom,ymomsq,ixmax,iymax,jmax,
     ,     XX,YY)    
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

c      real norma1(32,10),norma2(32,10),norma3(32,10),norma4(32,10),
c     ,     norma5(32,10),norma6(32,6),norm(56,32)
c      common/NOR1/norma1,norma2,norma3,norma4,norma5,norma6,norm
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
c     ,        (ablock(jj,i)/1000.)/
c     ,        norm(iycell(jj,i),ixcell(jj,i))
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
c         if(gen_event_ID_number.eq.2734)then
c            write(*,*)ixcell(jj,i),iycell(jj,i),eblock(jj,i),iycell(jj,i)-iymax+3,ixcell(jj,i)-ixmax+3
c         endif
c         en =(ablock(jj,i)/1000.)/
c     ,        norm(iycell(jj,i),ixcell(jj,i))
         if(en.gt.0.01.and.En.eq.En)then

            if(iycell(jj,i).lt.33.and.
     ,           iycell(jj,i).gt.0.and.ixcell(jj,i).gt.0.and.
     ,           ixcell(jj,i).lt.33.and.
     ,           iycell(jj,i)-iymax+3.gt.0.and.
     ,           iycell(jj,i)-iymax+3.lt.6.and.
     ,           ixcell(jj,i)-ixmax+3.gt.0.and.
     ,           ixcell(jj,i)-ixmax+3.lt.6
     ,           )then
c               write(*,*)iycell(jj,i)-iymax+3,ixcell(jj,i)-ixmax+3
               etot=etot+en
c               write(*,*)'Adding etot',etot
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
               etot=etot+en
c               write(*,*)'Adding etot',etot

               Eyx(iycell(jj,i)-iymax+3,ixcell(jj,i)-ixmax+3)=en
               XX(iycell(jj,i)-iymax+3,ixcell(jj,i)-ixmax+3)=xcell(jj,i)
               YY(iycell(jj,i)-iymax+3,ixcell(jj,i)-ixmax+3)=ycell(jj,i)
            endif
         endif
      enddo
c      if(eclust(i)-etot.gt.0.001)then
c         write(*,*)gen_event_ID_number,etot,eclust(i),eclust(i)-etot
c         write(*,*)'q'
c      endif
    
      etot9 =eyx(2,2)+eyx(2,3)+eyx(2,4)+
     ,     eyx(3,2)+eyx(3,3)+eyx(3,4)+
     ,     eyx(4,2)+eyx(4,3)+eyx(4,4)
      etot =eyx(1,1)+eyx(1,2)+eyx(1,3)+eyx(1,4)+eyx(1,5)+
     ,     eyx(2,1)+eyx(2,2)+eyx(2,3)+eyx(2,4)+eyx(2,5)+
     ,     eyx(3,1)+eyx(3,2)+eyx(3,3)+eyx(3,4)+eyx(3,5)+
     ,     eyx(4,1)+eyx(4,2)+eyx(4,3)+eyx(4,4)+eyx(4,5)+
     ,     eyx(5,1)+eyx(5,2)+eyx(5,3)+eyx(5,4)+eyx(5,5)

      Emt = emax/Etot
c      xx(  1,  1)=  0.000000    
c      yy(  1,  1)=  0.000000    
c      eyx(  1,  1)=  0.000000    
c      xx(  1,  2)=  0.000000    
c      yy(  1,  2)=  0.000000    
c      eyx(  1,  2)=  0.000000    
c      xx(  1,  3)=  0.000000    
c      yy(  1,  3)=  0.000000    
c      eyx(  1,  3)=  0.000000    
c      xx(  1,  4)=  0.000000    
c      yy(  1,  4)=  0.000000    
c      eyx(  1,  4)=  0.000000    
c      xx(  1,  5)=  0.000000    
c      yy(  1,  5)=  0.000000    
c      eyx(  1,  5)=  0.000000    
c      xx(  2,  1)=  0.000000    
c      yy(  2,  1)=  0.000000    
c      eyx(  2,  1)=  0.000000    
c      xx(  2,  2)= -43.15700    
c      yy(  2,  2)= -52.54000    
c      eyx(  2,  2)= 0.7824893E-01
c      xx(  2,  3)= -39.34700    
c      yy(  2,  3)= -52.54000    
c      eyx(  2,  3)= 0.6686154E-01
c      xx(  2,  4)=  0.000000    
c      yy(  2,  4)=  0.000000    
c      eyx(  2,  4)=  0.000000    
c      xx(  2,  5)=  0.000000    
c      yy(  2,  5)=  0.000000    
c      eyx(  2,  5)=  0.000000    
c      xx(  3,  1)= -46.96700    
c      yy(  3,  1)= -48.73000    
c      eyx(  3,  1)= 0.2464678E-01
c      xx(  3,  2)= -43.15700    
c      yy(  3,  2)= -48.73000    
c      eyx(  3,  2)= 0.2552722    
c      xx(  3,  3)= -39.34700    
c      yy(  3,  3)= -48.73000    
c      eyx(  3,  3)= 0.6244553    
c      xx(  3,  4)=  0.000000    
c      yy(  3,  4)=  0.000000    
c      eyx(  3,  4)=  0.000000    
c      xx(  3,  5)=  0.000000    
c      yy(  3,  5)=  0.000000    
c      eyx(  3,  5)=  0.000000    
c      xx(  4,  1)= -46.96700    
c      yy(  4,  1)= -44.92001    
c      eyx(  4,  1)= 0.1080168E-01
c      xx(  4,  2)= -43.15700    
c      yy(  4,  2)= -44.92001    
c      eyx(  4,  2)= 0.2753329E-01
c      xx(  4,  3)= -39.34700    
c      yy(  4,  3)= -44.92001    
c      eyx(  4,  3)= 0.1037672E-01
c      xx(  4,  4)= -35.53700    
c      yy(  4,  4)= -44.92001    
c      eyx(  4,  4)= 0.1347996E-01
c      xx(  4,  5)=  0.000000    
c      yy(  4,  5)=  0.000000    
c      eyx(  4,  5)=  0.000000    
c      xx(  5,  1)=  0.000000    
c      yy(  5,  1)=  0.000000    
c      eyx(  5,  1)=  0.000000    
c      xx(  5,  2)=  0.000000    
c      yy(  5,  2)=  0.000000    
c      eyx(  5,  2)=  0.000000    
c      xx(  5,  3)=  0.000000    
c      yy(  5,  3)=  0.000000    
c      eyx(  5,  3)=  0.000000    
c      xx(  5,  4)=  0.000000    
c      yy(  5,  4)=  0.000000    
c      eyx(  5,  4)=  0.000000    
c      xx(  5,  5)=  0.000000    
c      yy(  5,  5)=  0.000000    
c      eyx(  5,  5)=  0.000000    
c      xmomsqr =  -2.085386    
c      xmom =  -1.434208    
c      xmomsq = -0.5975022    
c      ymomsqr = -0.7758713E-01
c      ymom = -0.2841844    
c      ymomsq = -0.7685002E-01
      
      xmomsqr = XMomf(ixmax,iymax,eyx,xx,2);
      xmom    = XMomf(ixmax,iymax,eyx,xx,1);
      xmomsq  = XMomf(ixmax,iymax,eyx,xx,3);
      ymomsqr = XMomf(ixmax,iymax,eyx,yy,2);
      ymom    = XMomf(ixmax,iymax,eyx,yy,1);
      ymomsq  = XMomf(ixmax,iymax,eyx,yy,3);
c      write(*,*)xmomsqr,xmom,xmomsq,ymomsqr,ymom,ymomsq

      end

      real function XMomf(ix,iy,eyx,x,iflag)
      IMPLICIT NONE
      include 'bigcal_data_structures.cmn'
      include 'sane_data_structures.cmn'
      include 'b_ntuple.cmn'
      include 'sane_ntuple.cmn'
      include 'gen_data_structures.cmn'
      real eyx(5,5),x(5,5)
      
      real w(5,5),Sum,SumW,Coor
      
      Integer i,j,icx,icy,ix,iy,iflag
      Sum=0
      do i=1,5
         do j=1,5
            icx = ix+(j-3)
            icy = iy+(i-3)
            if(icx.gt.0.and.icy.gt.0.and.
     ,           icy.lt.57.and.eyx(i,j).lt.5)then
               if(iflag.eq.1.and.eyx(i,j).lt.5)Sum = Sum+eyx(i,j)
               if(iflag.eq.2.and.eyx(i,j).lt.5)Sum = Sum+sqrt(eyx(i,j))
               if(iflag.eq.3.and.eyx(i,j).lt.5)Sum = Sum+eyx(i,j)*eyx(i,j)
               icx = ix+(j-3)
               icy = iy+(i-3)
               w(i,j) = x(i,j)-x(3,3)
               
            endif
         enddo
      enddo
      
      Coor=0
      do i=1,5
         do j=1,5
            if(iflag.eq.1)Coor =Coor+w(i,j)*eyx(i,j)/Sum 
            if(iflag.eq.2)Coor =Coor+w(i,j)*sqrt(eyx(i,j))/Sum 
            if(iflag.eq.3)Coor =Coor+w(i,j)*eyx(i,j)*eyx(i,j)/Sum 
         enddo
      enddo
      XMomf=Coor
      end
