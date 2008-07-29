      subroutine b_init_physics(ABORT,err)

      implicit none
      save
      
      character*14 here
      parameter(here='b_init_physics')

      logical ABORT
      character*(*) err

      real cosyaw,sinyaw,cospitch,sinpitch,cosroll,sinroll

      real rollmatrix(3,3),pitchmatrix(3,3),yawmatrix(3,3)
      real pitchrollmatrix(3,3)

      integer i,j,k,l,m,n

      include 'gen_data_structures.cmn'
      include 'bigcal_data_structures.cmn'
      include 'gen_constants.par'
      include 'gen_units.par'

      abort=.false.
      err=' '

c     BIGCAL_HEIGHT = 0.
      BIGCAL_THETA_RAD = BIGCAL_THETA_DEG * tt / 180.
      
      BIGCAL_SINTHETA = sin(BIGCAL_THETA_RAD)
      BIGCAL_COSTHETA = cos(BIGCAL_THETA_RAD)

c     initialize rotation matrix from real to ideal coordinates:

      cosyaw = cos(bigcal_yaw_deg * tt / 180.)
      sinyaw = sin(bigcal_yaw_deg * tt / 180.)

      cospitch = cos(bigcal_pitch_deg * tt / 180.)
      sinpitch = sin(bigcal_pitch_deg * tt / 180.)
      
      cosroll = cos(bigcal_roll_deg * tt / 180. )
      sinroll = sin(bigcal_roll_deg * tt / 180. )

c     "roll" matrix: rotation around the z axis (clockwise looking downstream)
      rollmatrix(1,1) = cosroll
      rollmatrix(1,2) = -sinroll
      rollmatrix(1,3) = 0.

      rollmatrix(2,1) = sinroll
      rollmatrix(2,2) = cosroll
      rollmatrix(2,3) = 0.

      rollmatrix(3,1) = 0.
      rollmatrix(3,2) = 0.
      rollmatrix(3,3) = 1.
c     "pitch" matrix: rotation around the x axis (clockwise from "beam right" (small-angle side))
      
      pitchmatrix(1,1) = 1.
      pitchmatrix(1,2) = 0.
      pitchmatrix(1,3) = 0.

      pitchmatrix(2,1) = 0.
      pitchmatrix(2,2) = cospitch
      pitchmatrix(2,3) = -sinpitch

      pitchmatrix(3,1) = 0.
      pitchmatrix(3,2) = sinpitch
      pitchmatrix(3,3) = cospitch

c     "yaw" matrix: rotation arount the y axis (clockwise from above)

      yawmatrix(1,1) = cosyaw
      yawmatrix(1,2) = 0.
      yawmatrix(1,3) = -sinyaw

      yawmatrix(2,1) = 0.
      yawmatrix(2,2) = 1.
      yawmatrix(2,3) = 0.

      yawmatrix(3,1) = sinyaw
      yawmatrix(3,2) = 0. 
      yawmatrix(3,3) = cosyaw

c     the assumed order of the rotations (from real to ideal) is roll, pitch, then yaw. 
c     this may not be exactly correct. Nonetheless, let's plow ahead with calculating 
c     the overall rotation matrix R = Ryaw * Rpitch * Rroll
c     then, in ideal coordinates, the electron position is given by 
c     (x,y,z)ideal = R * (x,y,z)real

c     multiply pitch matrix times roll matrix
      do i=1,3
         do j=1,3
            bigcal_rot_matrix(i,j) = 0.
            do m=1,3
               do n=1,3
                  bigcal_rot_matrix(i,j) = bigcal_rot_matrix(i,j) + 
     $                 yawmatrix(i,m)*pitchmatrix(m,n)*rollmatrix(n,j)
               enddo
            enddo
         enddo
      enddo
      
      write(*,*) 'BIGCAL ROTATION MATRIX BASED ON SUPPLIED ANGLES = '
      do i=1,3
         write(*,'(3F12.8)')
     $        (bigcal_rot_matrix(i,j),j=1,3)
      enddo


c     that's all for now

      return 
      end
      
      
