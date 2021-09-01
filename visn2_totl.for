      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (ndatamax=800000,IMAX=5000,NRUN=5)
      dimension strxy(ndatamax),strxz(ndatamax),stryx(ndatamax)
      dimension stryz(ndatamax),strzx(ndatamax),strzy(ndatamax)
      dimension cstrxy(nrun,0:imax),cstryz(nrun,0:imax)
      dimension cstrzx(nrun,0:imax),cstrto(nrun,0:imax)
      dimension acstxy(0:imax),acstyz(0:imax)
      dimension acstzx(0:imax),acstto(0:imax)
      dimension tstrxy(nrun),tstryz(nrun),tstrzx(nrun),tstrto(nrun)
      dimension etaxy(nrun),etayz(nrun),etazx(nrun),etato(nrun)
      DIMENSION icount(nrun)
      CHARACTER DIR*9,C1*2
      OPEN(UNIT=3,FILE='acfstress_totl.dat',STATUS='replace')
      OPEN(UNIT=9,FILE='viscosity_totl.dat',STATUS='replace')
      OPEN(UNIT=10,FILE='integvisc_totl.dat',STATUS='replace')
      open(unit=100,FILE='../mdda.inpt',STATUS='old')
*********************************************
  130 FORMAT(E13.4,4E13.5)

      read(100,*)nstep
      read(100,*)temp 
      read(100,*)dens
      read(100,*)dt
      read(100,*)
      read(100,*)pmass
      read(100,*)pnd
      close(100)

      ndata=nstep
      write(*,*)'ndata = ',ndata

      NPT=500
      ICAL=2500
      AN=6.0221367D+23
      TMASS=14.0067D-3*2.d0/AN*pmass
      DENS=DENS*pmass/pnd/pnd/pnd
      NSTRUN=2

      TKB=1.380658D-23
      RKBT=1.D0/(TKB*TEMP)
      RTEMP=1.D0/TEMP
      VOL=NPT*TMASS/DENS
      RVOL=1.D0/VOL
      TCNST=1.D0

      do K=1,NRUN
       tstrxy(k)=0.d0
       tstryz(k)=0.d0
       tstrzx(k)=0.d0
       tstrto(k)=0.d0
       etaxy(k)=0.d0
       etayz(k)=0.d0
       etazx(k)=0.d0
       etato(k)=0.d0
        do I=0,IMAX
         acstxy(i)=0.d0
         acstyz(i)=0.D0
         acstzx(i)=0.D0
         acstto(i)=0.D0
         cstrxy(k,i)=0.d0
         cstryz(k,i)=0.D0
         cstrzx(k,i)=0.D0
         cstrto(k,i)=0.D0
        enddo
      enddo

      do K=1,NRUN
       icount(K)=0
       WRITE(C1,'(I2.2)')K+NSTRUN-1
       write(*,'(a12,i2.2,a2)')' loadng run ',k+nstrun-1,' !'
       OPEN(UNIT=1,FILE='../run'//C1//'/stress.dat',STATUS='OLD')
        do I=1,NDATA
c         READ(1,*)strxy(i),strxz(i),stryx(i)
c     -           ,stryz(i),strzx(i),strzy(i)
         READ(1,*)strxy(i),strxz(i),stryz(i)
         stryx(i)=strxy(i)
         strzx(i)=strxz(i)
         strzy(i)=stryz(i)
        enddo
        do I=1,NDATA-IMAX
         icount(K)=icount(K)+1
          do J=I,I+IMAX
           cstrxy(K,J-I)=cstrxy(K,J-I)+strxy(j)*strxy(i)
           cstryz(K,J-I)=cstryz(K,J-I)+stryz(j)*stryz(i)
           cstrzx(K,J-I)=cstrzx(K,J-I)+strzx(j)*strzx(i)
          enddo
        enddo
       close(1)
        do I=0,IMAX
         cstrxy(K,I)=tcnst*cstrxy(K,I)/dble(icount(K))
         cstryz(K,I)=tcnst*cstryz(K,I)/dble(icount(K))
         cstrzx(K,I)=tcnst*cstrzx(K,I)/dble(icount(K))
         cstrto(K,I)=(cstrxy(K,I)+cstryz(K,I)+cstrzx(K,I))/3.d0
         acstxy(I)=acstxy(I)+cstrxy(K,I)
         acstyz(I)=acstyz(I)+cstryz(K,I)
         acstzx(I)=acstzx(I)+cstrzx(K,I)
         acstto(I)=acstto(I)+cstrto(K,I)
        enddo
      enddo

      dum1=0.d0
      dum2=0.d0
      dum3=0.d0
      dum4=0.d0
      do I=0,IMAX
       acstxy(I)=acstxy(I)/dble(NRUN)
       acstyz(I)=acstyz(I)/dble(NRUN)
       acstzx(I)=acstzx(I)/dble(NRUN)
       acstto(I)=acstto(I)/dble(NRUN)
       dum=1.d0
       if (i.eq.0 .or. i.eq.imax) dum=0.5d0
       dum1=dum1+acstxy(i)*dt*dum*RVOL*RKBT ! [J]*[J]*[s]*[m-3]*[J-1]=[J m-3 s]=[Pa s]
       dum2=dum2+acstyz(i)*dt*dum*RVOL*RKBT
       dum3=dum3+acstzx(i)*dt*dum*RVOL*RKBT
       dum4=dum4+acstto(i)*dt*dum*RVOL*RKBT
       WRITE(3,130)DBLE(I)*DT,acstxy(I),acstyz(I),acstzx(I),acstto(I)
       write(10,130)dble(i)*dt,dum1,dum2,dum3,dum4
      enddo

********INTEGRATION*************************
      etaave1=0.d0
      etaave2=0.d0
      etaave3=0.d0
      etaave4=0.d0
      etaav21=0.d0
      etaav22=0.d0
      etaav23=0.d0
      etaav24=0.d0

      do K=1,NRUN
        do I=0,IMAX
         IF ((I.EQ.0).OR.(I.EQ.IMAX)) THEN
          tstrxy(K)=tstrxy(K)+cstrxy(K,I)*DT*0.5D0
          tstryz(K)=tstryz(K)+cstryz(K,I)*DT*0.5D0
          tstrzx(K)=tstrzx(K)+cstrzx(K,I)*DT*0.5D0
          tstrto(K)=tstrto(K)+cstrto(K,I)*DT*0.5D0
         ELSE
          tstrxy(K)=tstrxy(K)+cstrxy(K,I)*DT
          tstryz(K)=tstryz(K)+cstryz(K,I)*DT
          tstrzx(K)=tstrzx(K)+cstrzx(K,I)*DT
          tstrto(K)=tstrto(K)+cstrto(K,I)*DT
         END IF
         IF (I.LE.ICAL) cycle
         etaxy(k)=etaxy(k)+tstrxy(K)
         etayz(k)=etayz(k)+tstryz(K)
         etazx(k)=etazx(k)+tstrzx(K)
         etato(k)=etato(k)+tstrto(K)
        enddo
       etaxy(k)=etaxy(k)/DBLE(IMAX-ICAL)
       etayz(k)=etayz(k)/DBLE(IMAX-ICAL)
       etazx(k)=etazx(k)/DBLE(IMAX-ICAL)
       etato(k)=etato(k)/DBLE(IMAX-ICAL)

       etaxy(k)=etaxy(k)*RVOL*RKBT
       etayz(k)=etayz(k)*RVOL*RKBT
       etazx(k)=etazx(k)*RVOL*RKBT
       etato(k)=etato(k)*RVOL*RKBT

       etaave1=etaave1+etaxy(k)
       etaave2=etaave2+etayz(k)
       etaave3=etaave3+etazx(k)
       etaave4=etaave4+etato(k)
       etaav21=etaav21+etaxy(k)*etaxy(k)
       etaav22=etaav22+etayz(k)*etayz(k)
       etaav23=etaav23+etazx(k)*etazx(k)
       etaav24=etaav24+etato(k)*etato(k)
      enddo

*********************************************

      etaave1=etaave1/dble(NRUN)
      etaave2=etaave2/dble(NRUN)
      etaave3=etaave3/dble(NRUN)
      etaave4=etaave4/dble(NRUN)

      etaav21=etaav21/dble(NRUN)
      etaav22=etaav22/dble(NRUN)
      etaav23=etaav23/dble(NRUN)
      etaav24=etaav24/dble(NRUN)

      sd1=dsqrt(etaav21-etaave1*etaave1)
      sd2=dsqrt(etaav22-etaave2*etaave2)
      sd3=dsqrt(etaav23-etaave3*etaave3)
      sd4=dsqrt(etaav24-etaave4*etaave4)
      se1=sd1/dsqrt(dble(NRUN-1))
      se2=sd2/dsqrt(dble(NRUN-1))
      se3=sd3/dsqrt(dble(NRUN-1))
      se4=sd4/dsqrt(dble(NRUN-1))

      sdr1=sd1/etaave1
      sdr2=sd2/etaave2
      sdr3=sd3/etaave3
      sdr4=sd4/etaave4
      ser1=se1/etaave1
      ser2=se2/etaave2
      ser3=se3/etaave3
      ser4=se4/etaave4

      WRITE(9,'(A57)')
     -'########## Shear viscosity of noble-gas liquid ##########'
      WRITE(9,*)' '
      WRITE(9,*)' MD infomation .....'
      write(9,'(A20,F8.2,A7)')'density = ',dens,' kg m-3'
      write(9,'(A20,F8.2,A2)')'temperature = ',temp,' K'
      write(9,'(A20,F8.2)')'pkei1 = ',pkei1
      write(9,'(A20,F8.2)')'pkei2 = ',pkei2
      write(9,'(A20,F8.2)')'pkei3 = ',pkei3
      write(9,'(A20,I8)')  'nstep = ',nstep
      write(9,'(A20,F8.2,A3)')'time step = ',dt*1e15,' fs'
      write(9,'(A20,F8.2,A3)')'summation time = ',imax*dt*1e12,' ps'
      write(9,'(A20,F8.2,A3)')'cutoff time = ',ical*dt*1e12,' ps'
      write(9,'(A20,I4)')'# of Run = ',nrun
      WRITE(9,*)' '
      WRITE(9,'(A40)')'viscosity, eta [unit: 10^-4 kg/(m s)]'
      WRITE(9,111) 'eta(xy) = ',etaave1*1d4,'(SE = ',se1*1d4,','
     -                                     ,'SER = ',ser1*1d2,' %)'
      WRITE(9,111) 'eta(yz) = ',etaave2*1d4,'(SE = ',se2*1d4,','
     -                                     ,'SER = ',ser2*1d2,' %)'
      WRITE(9,111) 'eta(zx) = ',etaave3*1d4,'(SE = ',se3*1d4,','
     -                                     ,'SER = ',ser3*1d2,' %)'
      WRITE(9,111)'eta(all) = ',etaave4*1d4,'(SE = ',se4*1d4,','
     -                                     ,'SER = ',ser4*1d2,' %)'
      WRITE(9,'(A57)')
     -'---------------------- (reference) ----------------------'
      WRITE(9,111) 'eta(xy) = ',etaave1*1d4,'(SD = ',sd1*1d4,','
     -                                     ,'SDR = ',sdr1*1d2,' %)'
      WRITE(9,111) 'eta(yz) = ',etaave2*1d4,'(SD = ',sd2*1d4,','
     -                                     ,'SDR = ',sdr2*1d2,' %)'
      WRITE(9,111) 'eta(zx) = ',etaave3*1d4,'(SD = ',sd3*1d4,','
     -                                     ,'SDR = ',sdr3*1d2,' %)'
      WRITE(9,111)'eta(all) = ',etaave4*1d4,'(SD = ',sd4*1d4,','
     -                                     ,'SDR = ',sdr4*1d2,' %)'
  111 FORMAT(A15,F8.5,3X,A7,F8.5,A1,A7,F6.2,A3)
      STOP
      END


