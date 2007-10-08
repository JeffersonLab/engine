      subroutine b_guess_ecell(nbad,maxnbad,rowbad,colbad,cellbad,eguess,E,X,Y)

      implicit none
      save

      include 'bigcal_data_structures.cmn'
      include 'bigcal_shower_parms.cmn'
      include 'bigcal_geometry.cmn'
      
      integer nbad
      integer maxnbad
      integer rowbad(maxnbad)
      integer colbad(maxnbad)
      integer cellbad(maxnbad)

      integer i

      real eguess(maxnbad)

      real cuteguess

      real shower_fit_symm
      real shower_fit_asymm

      real E,X,Y

      real xcell,ycell,xdiff,ydiff

      do i=1,nbad

         xcell = bigcal_all_xcenter(cellbad(i))
         ycell = bigcal_all_ycenter(cellbad(i))

         xdiff = xcell - X
         ydiff = ycell - Y

*     normalize xdiff and ydiff to the cell size, because the shower shape fit 
*     is based on the ratio of distance to cell size

         if(rowbad(i).le.32) then
            xdiff = xdiff / bigcal_prot_size_x
            ydiff = ydiff / bigcal_prot_size_y
         else
            xdiff = xdiff / bigcal_rcs_size_x
            ydiff = ydiff / bigcal_rcs_size_y
         endif

         if(bigcal_shape_opt.eq.1) then
            eguess(i) = E*shower_fit_asymm(xdiff,ydiff)

c$$$            write(*,*) 'guessed bad channel energy:',
c$$$     $           '(row,col,Eguess)=(',rowbad(i),',',colbad(i),
c$$$     $           ',',eguess(i),')'

            if(cellbad(i).le.1024) then
               cuteguess = b_cell_cut_prot
            else
               cuteguess = b_cell_cut_rcs
            endif

            if(eguess(i).lt.cuteguess) eguess(i) = 0.
            
            bigcal_all_good_det(cellbad(i)) = eguess(i)
            if(rowbad(i).le.32) then
               bigcal_prot_good_det(cellbad(i)) = eguess(i)
            else
               bigcal_rcs_good_det(cellbad(i)-1024) = eguess(i)
            endif
         else
            eguess(i) = E*shower_fit_symm(xdiff,ydiff)

c$$$            write(*,*) 'guessed bad channel energy'//
c$$$     $           '(row,col,Eguess)=(',rowbad(i),',',colbad(i),
c$$$     $           ',',eguess(i),')'

            if(cellbad(i).le.1024) then
               cuteguess = b_cell_cut_prot
            else
               cuteguess = b_cell_cut_rcs
            endif
            
            if(eguess(i).lt.cuteguess) eguess(i) = 0.
            
            bigcal_all_good_det(cellbad(i)) = eguess(i)
            if(rowbad(i).le.32) then
               bigcal_prot_good_det(cellbad(i)) = eguess(i)
            else
               bigcal_rcs_good_det(cellbad(i)-1024) = eguess(i)
            endif
         endif
      enddo

      return
      end

      real function gs(x,y,a,b)

      real x,y,a,b

      real PI
      data PI/3.1415926536/

      gs = a / sqrt(2.*PI) * (atan(x/b) + atan(y/b) + 
     $     atan(x*y/b/sqrt(b**2 + x**2 + y**2) ) )
      
      end

      real function ga(x,y,a,bx,by)

      real x,y,a,bx,by
      real PI
      data PI/3.1415926536/
      
      ga = a / sqrt(2.*PI) * (atan(x/bx) + atan(y/by) + 
     $     atan(x*y/bx/by/sqrt(1. + (x/bx)**2 + (y/by)**2) ) )

      end

      real function shower_fit_symm(x,y)
     
      real x,y
      real a,b,d,gs
      include 'bigcal_data_structures.cmn'
      include 'bigcal_shower_parms.cmn'
      
      a = bigcal_sshape_a ! height parameter
      b = bigcal_sshape_b ! width parameter
c      d = bigcal_sshape_d ! cell size parameter
      d = 1.0
      shower_fit_symm = gs(x+d/2.,y+d/2.,a,b) + gs(x-d/2.,y-d/2.,a,b)
     $     - gs(x+d/2.,y-d/2.,a,b) - gs(x-d/2.,y+d/2.,a,b)

      end

      real function shower_fit_asymm(x,y)

      real x,y
      real a,bx,by,d,ga
      include 'bigcal_data_structures.cmn'
      include 'bigcal_shower_parms.cmn'

      a = bigcal_ashape_a ! height parameter 
      bx = bigcal_ashape_bx ! width x parameter
      by = bigcal_ashape_by ! width y parameter
c      d = bigcal_ashape_d ! cell size parameter
      d = 1.0

      shower_fit_asymm = ga(x+d/2.,y+d/2.,a,bx,by) + ga(x-d/2.,y-d/2.,a,bx,by)
     $     - ga(x+d/2.,y-d/2.,a,bx,by) - ga(x-d/2.,y+d/2.,a,bx,by)

      end
