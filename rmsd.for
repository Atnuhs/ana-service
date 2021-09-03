      implicit real*8 (a-h,o-z)
      parameter (np=500,ndata=5000,ndt=100)
      dimension tdphix(np,ndata),tdphiy(np,ndata),tdphiz(np,ndata)
      dimension dphi2(ndt)
      character c1*2


      read(5,*)nstrun
      write(*,*)'Run no is ',nstrun
      write(c1,'(i2.2)')nstrun
      open(unit=2,file='rmsd'//c1//'-1.dat',status='replace')
      open(unit=1,file='../run'//c1//'/tdphi.dat',status='old')

      do i=1,ndt
      dphi2(i)=0.d0      
      enddo


c      read(1,'(e11.4,i4)')dt,intd
      dt=2.d-15
      intd=100
      do i=1,ndata
      read(1,*)istep
      do j=1,np
      read(1,*)tdphix(j,i),tdphiy(j,i),tdphiz(j,i)
      enddo
      enddo

      do i=1,ndata-ndt
      do k=i+1,i+ndt
      do j=1,np
      dumx=tdphix(j,k)-tdphix(j,i)
      dumy=tdphiy(j,k)-tdphiy(j,i)
      dumz=tdphiz(j,k)-tdphiz(j,i)
c      dphiphi=dumx*dumx+dumy*dumy+dumz*dumz
      dphiphi=dumx*dumx+dumy*dumy
      l=k-i
      dphi2(l)=dphi2(l)+dphiphi
      enddo
      enddo
      if (mod(i,10).eq.0) write(*,*)i,'/',ndata-ndt
      enddo

      do i=1,ndt
      dphi2(i)=dphi2(i)/dble(np)/dble(ndata-ndt)
      enddo

      do i=1,ndt
      write(2,'(2e13.5)')dt*dble(i*intd),dphi2(i)
      enddo

      stop
      end
