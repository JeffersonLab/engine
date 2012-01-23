      Subroutine SANE_DUMP_NTUP_VAR()
      include 'b_ntuple.cmn'
      include 'sane_ntuple.cmn'
      integer i,j


      nclust = 0
      y1t_hit = 0
      y2t_hit =  0
      x1t_hit = 0
      y3t_hit = 0
      cer_hit = 0
      luc_hit = 0
      do i=1,TRACKER_MAX_HITS
	 y1t_row(i) = 0
	 y1t_tdc(i) = 0
         y1t_y(i)   = 0
	 y2t_row(i) = 0
	 y2t_tdc(i) = 0
         y2t_y(i)   = 0
	 y3t_row(i) = 0
	 y3t_tdc(i) = 0
         y3t_y(i)   = 0
	 x1t_row(i) = 0
	 x1t_tdc(i) = 0
         x1t_x(i)   = 0
      enddo

      do i=1,maxnclust
         ncellclust(i) = 0
         ncellbad(i) = 0
         ncellx(i) = 0
         ncelly(i) = 0
         xmoment(i) = 0
         ymoment(i) = 0
         eclust(i) = 0
         aclust(i) = 0
         xclust(i) = 0
         yclust(i) = 0

	 cer_num(i) = 0
	 cer_tdc(i) = 0
	 cer_adc(i) = 0

	 luc_row(i) = 0
	 ladc_pos(i) = 0
         ladc_neg(i) = 0
	 ltdc_pos(i) = 0
         ltdc_neg(i) = 0
         luc_y(i) = 0
         
         do j=1,maxncellclust
            xcell(j,i) = 0
            ycell(j,i) = 0
            eblock(j,i) = 0
            ablock(j,i) = 0
         enddo
      enddo
      hms_p        = 0	
      hms_e        = 0
      hms_theta    = 0
      hms_phi      = 0
      hms_ytar     = 0
      hms_yptar    = 0
      hms_xptar    = 0
      hms_delta    = 0

      do j=1,maxcl
         E_clust(j) = 0
         X_clust(j) = 0 
         Y_clust(j) = 0
         Z_clust(j) = 0
         X_clust_r(j) = 0 
         Y_clust_r(j) = 0
         Z_clust_r(j) = 0
         luc_h(j) = 0
         trc_hx(j) = 0
         trc_hy1(j) = 0
         trc_hy2(j) = 0
         do i=1,20
            X_luc(i,j) = 0 
            Y_luc(i,j) = 0 
            Z_luc(i,j) = 0
            X_luc_r(i,j) = 0
            Y_luc_r(i,j) = 0
            Z_luc_r(i,j) = 0
            X_trc(i,j) = 0
            Z_trc(i,j) = 0
            Y1_trc(i,j) = 0
            Z1_trc(i,j) = 0
            Y2_trc(i,j) = 0
            Z2_trc(i,j) = 0
         enddo
         do i=1,3
            Tr_Vertex(i,j) = 0 
            Tr_Vertex_r(i,j) = 0
         enddo
         cer_h(j) = 0
         cerb_time(j) = 0
         cerb_adc(j) = 0
         bigc_time(j) = 0
         bigc_adc(j) = 0
         Theta_e(j) = 0
         Phi_e(j) = 0
         Delta_Y(j) = 0
         Delta_X(j) = 0
         X_Bjorken(j) = 0
         Q2(j) = 0
         W2(j) = 0
         ENue(j) = 0
 
      enddo
      return
      end
