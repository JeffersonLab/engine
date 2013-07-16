c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c File param.f
c
c Contains the paraeterizations for both magnets.
c     Bneedd calculated B(p) for the dipole.
c     Ineedq calculates I(B.dl) for the quadrupoles.
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c HISTORY
c
c 9Mar1999  DWM The fitting program had the same sign problem. New fit.
c 8Mar1999  DWM fixed messed up signs on quadrupole constant parameters.
c 27Feb1999 DWM Restored disabled parameterization with the correct
c     quadratic coeficient. Thanks Maurice.
c 27Feb1999 MauriceB disabled parametrization for B>0.98T
c 20Jan1999 DWM. First seperated from the main body of the field program
c     Changed to use the arrays for holding the parameters, instead of
c     imbedding them in a bunch of explicit calculations.
c---------------------------------------------------------------------------

c============================================================================
c Function Bneedd
c     Calculate the fieldrequired for the dipole at a given current ``p''.
c============================================================================
      real*8 function Bneedd (p,silent)
      
      implicit none

      real*8 p
      integer silent ! If not zero, don't print any diagnostics

c The parameters of the magnet fits
      real*8 FIELD_D
      real*8 EFL_0, CUTOFF, QUADRATIC

c Some intermediate values.
      real*8 B0, B


c Dipole field for 1 GeV is 0.2765
      data FIELD_D   /0.2765/
      data EFL_0     /5.199/
      data CUTOFF    /0.9836/
      data QUADRATIC /-7.3137e-2/


c This is it.
c Grind through the parameterization
      B0 = p * FIELD_D

      if (B0 .le. CUTOFF) then
         B = B0
      else
c         B = B0 * (1 - (B0 - CUTOFF)**2 * QUADRATIC / EFL_0 )
        B = B0
        if(silent.eq.0)
     $       write(*,*) '###Parameterization for B>',CUTOFF
     $       ,' is disabled###'
      endif

      Bneedd = B
      return
      end


c============================================================================
c Function Ineedq
c     Calculate the current required for quadrupole number `num'
c     to reach a the given  `bl' (B.dl in Rolf's notation),
c============================================================================
      real*8 function Ineedq (bl,num)
      
      implicit none

      real*8 bl
      integer num

      integer NUM_MAGS
      parameter (NUM_MAGS=3)

c The parameters of the magnet fits
      real*8 cutoff(NUM_MAGS), constant(NUM_MAGS)
      real*8 linear(NUM_MAGS), cubic(NUM_MAGS)

c     The current needed.
      real*8 cur

c                        Q1        Q2        Q3    
c      data cutoff   /   2.114,    2.409,    2.389  /
c      data constant /   0.139,    0.277,    0.271  /
c      data linear   / 297.38,   189.89,   189.28   /
c      data cubic    /   9.55,    12.99,    12.68   /
      data cutoff   /   2.146,    2.423,    2.402  /
      data constant /  -0.139,   -0.277,   -0.271  /
      data linear   / 297.56,   190.19,   189.56   /
      data cubic    /  10.15,    13.18,    12.84   /


c This is it.
c Grind through the parameterization
      if (bl .le. cutoff(num)) then
         cur = constant(num) + linear(num) * bl
      else
         cur = constant(num) + linear(num) * bl 
     1        + (bl - cutoff(num))**3 * cubic(num)
      endif

      Ineedq = cur
      return
      end

