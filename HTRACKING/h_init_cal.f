*=======================================================================
      subroutine h_init_cal(abort,errmsg)
*=======================================================================
*-
*-      Purpose: HMS Calorimeter Initialization
*-
*-      Created: 20 Mar 1994      Tsolak A. Amatuni
* $Log$
* Revision 1.1  1994/04/13 15:39:11  cdaq
* Initial revision
*
*-----------------------------------------------------------------------
*
*
      implicit none
      save
*
      logical abort
      character*(*) errmsg
      character*10 here
      parameter (here='H_INIT_CAL')
*
      integer*4 block      !Block number
      integer*4 row        !Row number
      integer*4 column     !Column number
      real*4 xi   !Temporary
      real*4 zi   !Temporary
*
      include 'gen_data_structures.cmn'
      include 'hms_calorimeter.cmn'
*
*-----Initialize the positions
      do column=1,hmax_cal_columns
         do row=1,hmax_cal_rows
            block=row+hmax_cal_rows*(column-1)
*
            if(column.eq.1) then
               hcal_block_xc(block)=hcal_1pr_top(row)+0.5*hcal_1pr_thick
               hcal_block_yc(block)=0.5*(hcal_1pr_left+hcal_1pr_right)
               hcal_block_zc(block)=hcal_1pr_zpos+0.5*hcal_1pr_thick
            else if(column.eq.2) then
               hcal_block_xc(block)=hcal_2ta_top(row)+0.5*hcal_2ta_thick
               hcal_block_yc(block)=0.5*(hcal_2ta_left+hcal_2ta_right)
               hcal_block_zc(block)=hcal_2ta_zpos+0.5*hcal_2ta_thick
            else if(column.eq.3) then
               hcal_block_xc(block)=hcal_3ta_top(row)+0.5*hcal_3ta_thick
               hcal_block_yc(block)=0.5*(hcal_3ta_left+hcal_3ta_right)
               hcal_block_zc(block)=hcal_3ta_zpos+0.5*hcal_3ta_thick
            else
               hcal_block_xc(block)=hcal_4ta_top(row)+0.5*hcal_4ta_thick
               hcal_block_yc(block)=0.5*(hcal_4ta_left+hcal_4ta_right)
               hcal_block_zc(block)=hcal_4ta_zpos+0.5*hcal_4ta_thick
            endif
         enddo   !End loop over rows
      enddo   !End loop over columns
******Temporary Section
      do block=1,hmax_cal_blocks
         hcal_ped_mean(block) =0.
         hcal_ped_rms(block)  =0.
         hcal_threshold(block)=0.
*
         hcal_cal_const(block)=1.
*
         hcal_gain_ini(block)=1.
         hcal_gain_cur(block)=1.
         hcal_gain_cor(block)=1.
      enddo
*
*      set by CTP
*      hlun_dbg_cal=20
*      open(hlun_dbg_cal,file='hms_calorimeter.dbg',status='new')
*
      hcal_block_xsize=10.
      hcal_block_ysize=70.
      hcal_block_zsize=10.
      hcal_xmax=+60.
      hcal_xmin=-60.
      hcal_ymax=+35
      hcal_ymin=-35.
      hcal_zmin=325.
      hcal_zmax=365.
      hcal_fv_xmin=hcal_xmin+5.
      hcal_fv_xmax=hcal_xmax-5.
      hcal_fv_ymin=hcal_ymin+5.
      hcal_fv_ymax=hcal_ymax-5.
      hcal_fv_zmin=hcal_zmin
      hcal_fv_zmax=hcal_zmax
*
      do row=1,hmax_cal_rows
      do column=1,hmax_cal_columns
         block=row+hmax_cal_rows*(column-1)
         xi   =hcal_block_xsize*(float(row)-0.5)
         zi   =hcal_block_zsize*(float(column)-0.5)
         hcal_block_xc(block)=hcal_xmin+xi
         hcal_block_yc(block)=0.
         hcal_block_zc(block)=hcal_zmin+zi
      enddo
      enddo
******Temporary Section
*      write(6,'('' Completed h_init_cal'')')
*
      return
      end
