
program main
implicit none

Double Precision::A(150,150),D(150),det,B(150,150),mat(150,150)
integer::NN,N,j,i,iao,col(150),row(150)
character(len=9)::k,AA

open(unit=9, file='daig_matrix',status='old')

read(9,*)NN,iao,N
read(9,*)(col(i),i=1,N)
read(9,*)(row(i),i=1,N)
do i=1,NN
read(9,*),AA,(A(i,j),j=1,NN)

!write(*,"(6F15.10)"),(A(i,j),j=1,NN)
enddo
do i=1,NN
do j=1,NN
B(i,j)=A(i,j)/sqrt(A(i,i)*A(j,j))
enddo
enddo
print*,'normalized overlap matrix'
!do i=1,NN
!write(*,"(6F15.10)"),(B(i,j),j=1,NN)
!enddo

do i=1,N
do j=1,N
mat(i,j)=A(col(i)-iao,row(j)-iao)
enddo
enddo

do i=1,N
write(*,"(6F15.10)"),(mat(i,j),j=1,N)
enddo
call MatLDR(mat,N,D,N)
!call MatLDR(B,NN,D,NN)

det=1.0
do i=1,N
det=det*D(i)
enddo

write(*,*),'Overlap of',(col(i),i=1,N),'/',(row(i),i=1,N)
print*,'eigen:',(D(i),i=1,N),det
100 Format(a,6I3,a,6I3)
stop
end program main
!--------------------------------------------
      SUBROUTINE MatLDR(A,N,D,Ndim)
!--------------------------------------------
!
!     A = U * D * VT**T
!     by two-sides Jacobi's method
!     U,VT::: left and right eigen value of A
!     right eigenvectors are stored in transpose form
!     D::: diagonal matrix stored in one dimension array
!     N::: the length of row/column of matrix A
!     Nlt::: is the Nulity of matrix A, which is equal to the number of zero eigenvalues 
!     Up::: upper main tridiagonal
!     Low::: lowwer main tridiagonal
!     Amax::: maximum off-diagonal matrix elements
!
!      USE XMVBIO
      Implicit none
      Double Precision A(150,150),U(Ndim,Ndim),D(150),VT(Ndim,Ndim)
      Double Precision Amax,Anzo,Aii,Ajj,Aij,Aji,Aod,Adif,tmp
      integer N,Ndim,Nlt
      integer I,J,K,L,Iplus
      integer iter,Itmax,Dotri
      Double Precision Zer,One,Eps
      INTEGER IW,IER

      Data Zer,One,Eps/0.D0,1.D0,1.0D-20/

!      IW  = IVBF(1)
!      IER = IVBF(3)
!     write(0,*)"NDIM = ",Ndim
!     write(0,*)"Size = ",Size(VT,1),Size(VT,2),Size(VT)
!do i=1,N
!print*,(A(i,j),j=1,N)
!enddo

!print*,'N',N,Zer

      do i = 1, N
        D(i) = Zer
        do j = 1, N
          U(j,i) = Zer
          VT(j,i) = Zer
        enddo
        U(i,i) = One
        VT(i,i) = One
      enddo
      Nlt = 0
      if(N.le.0) Return
      if(N.eq.1) then
          D(1) = A(1,1)
          if(dabs(D(1)) .lt. 1.d-12 ) Nlt = 1
          Return
      endif
      itmax = 200
      iter = 0
      Amax = One
      do while(Amax .gt. Eps)
!print*,'Eps',Eps,Amax
          iter = iter + 1
          Amax = Zer
          do i = 2, N
          do j = 1, i - 1
            Aii = A(i,i)
            Ajj = A(j,j)
            Aij = A(i,j)
            Aji = A(j,i)
            if(dabs(Aij) .gt. Amax) Amax = dabs(Aij)
            if(dabs(Aji) .gt. Amax) Amax = dabs(Aji)
            if(dabs(Aij) .lt. Eps .and. dabs(Aji) .lt. Eps) cycle
            Call TwoSideJacobi(i,j,Aii,Ajj,Aij,Aji,U,VT,A,N,Ndim)
!print*,'Aij',i,j,Aii,Aij,Ajj,Aji
          end do
          end do
      end do
!print*,'Aij***',i,j,Aii,Aij,Ajj,Aji
!do i=1,3
!print*,'U',(U(i,j),j=1,3)
!enddo
!do i=1,3
!print*,'VT',(VT(i,j),j=1,3)
!enddo

      do i = 1,N
        D(i) = A(i,i)
      End do
      if((iter / N) .eq. itmax) then
        write(*,*)'fail in MatLDR'
        write(*,*)'Average number of sweep:::',iter / N
        Stop
      endif

!     bubble sort the eigenvalues and eigenvectors
      Do I = 1, N-1
        tmp = dabs(D(I))
        K = I
        Do J = I+1, N
          If(dabs(D(J)) .GT. tmp) then
            tmp = dabs(D(J))
            K = J
          Endif
        Enddo
        If( K .NE. I) Then
          tmp = D(I)
          D(I) = D(K)
          D(K) = tmp
          Do J = 1, N
            tmp = U(J,I)
            U(J,I) = U(J,K)
            U(J,K) = tmp
            tmp = VT(J,I)
            VT(J,I) = VT(J,K)
            VT(J,K) = tmp
          Enddo
        Endif
      Enddo
      Do I=N,1,-1
        If(Dabs(D(I)).LT.1.d-12) Then
          Nlt=Nlt+1
        Else  
          Exit
        Endif
      Enddo
      Call MatTran(VT,N,Ndim)

      Return
      End
!-------------------------------
      SUBROUTINE MatTran(A,N,Nd)
!-------------------------------
!
!
!
        implicit none
        integer N,Nd,I,J
        Double Precision A,x
        Dimension A(Nd,Nd)

        Do I=1,N-1
          Do J=I+1,N
            x=A(J,I)
            A(J,I)=A(I,J)
            A(I,J)=x
          Enddo
        Enddo

        Return
      End



!----------------------------------------------------------------
      SUBROUTINE TwoSideJacobi(i,j,Aii,Ajj,Aij,Aji,U,VT,A,N,Ndim)
!----------------------------------------------------------------
!
!     This SUBROUTINE calculate two sides Jacobi rotation to following
!     matrix
!     | Aii Aij |       | Aii~  0  |
!     |         |  -->  |          |
!     | Aji Ajj |       |  0  Ajj~ |
!
      Implicit none
      Double Precision A(150,150),U(Ndim,Ndim),D(150),VT(Ndim,Ndim)
      Double Precision t1,c1,s1
      Double Precision t2,c2,s2
      Double Precision tp,cp,sp
      Double Precision tm,cm,sm
      Double Precision Aii,Ajj,Aij,Aji,tt,Aod,Adif,tmp
      integer N,Ndim
      integer I,J,k
      Double Precision Zer,One,Eps
      Data Zer,One,Eps/0.D0,1.D0,1.0D-20/
!     write(0,*)"Size 2 = ",Size(VT,1),size(vt,2),size(vt)
!print*,'A11,A12,A21,A22',A(1,1),A(1,2),A(2,1),A(2,2)

!     angle alpha + beta
!     write(0,*)'I J = ',I,J
!     write(0,*)'aii = ',aii
!     write(0,*)'ajj = ',ajj
!     write(0,*)'aij = ',aij
!     write(0,*)'aji = ',aji
      Adif = Aii - Ajj
      Aod = Aij + Aji
      if(dabs(Adif) .lt. dabs(Aod)) then
!       tt is cot(2x)
!       if(tt > Zer) then
!         tp = dsqrt(One + tt*tt) - tt
!       else
!         tp =-dsqrt(One + tt*tt) - tt
!       endif
        tt = Adif / Aod
!       write(0,*)'alpha + beta adif < aod, tt = ',tt
        tp = dsqrt(One + tt*tt)
        if(tt .lt. Zer) tp = -tp
        tp = tp - tt
      else if (dabs(Adif).lt.Eps) then
        tp = Zer
      else
!       tt is tan(2x)
        tt = Aod / Adif
!       write(0,*)'alpha + beta adif >= aod, tt = ',tt
        if(dabs(tt) .gt. 1.d-7) then
          tp = (dsqrt(One + tt*tt) - One) / tt
        else
!         numerical stable:::Taylor expansion
          tp = 0.5d0*tt - 0.125d0*tt*tt + 0.0625d0*tt*tt*tt 
        endif
      endif
      cp = One / dsqrt(One + tp*tp)
      sp = tp * cp
!     angle alpha - beta
      Adif = Aii + Ajj
      Aod = Aji - Aij
      if(dabs(Adif) .lt. dabs(Aod)) then
        tt = Adif / Aod
!       write(0,*)'alpha - beta adif < aod, tt = ',tt
        tm = dsqrt(One + tt*tt)
        if(tt .lt. Zer) tm = -tm
        tm = tm - tt
      else if (dabs(Adif).lt.Eps) then
        tm = Zer
      else
        tt = Aod / Adif
!       write(0,*)'alpha - beta adif >= aod, tt = ',tt
        if(dabs(tt) .gt. 1.d-7) then
          tm = (dsqrt(One + tt*tt) - One) / tt
        else
!         numerical stable:::Taylor expansion
          tm = 0.5d0*tt - 0.125d0*tt*tt + 0.0625d0*tt*tt*tt 
        endif
      endif
      cm = One / dsqrt(One + tm*tm)
      sm = tm * cm
      c1 = cp*cm - sp*sm
      s1 = sp*cm + cp*sm
      c2 = cp*cm + sp*sm
      s2 = sp*cm - cp*sm
!     update U and A
      Do k = 1, N
        tmp=c1*U(k,j)-s1*U(k,i)
        U(k,i)=s1*U(k,j)+c1*U(k,i)
        U(k,j)=tmp
        tmp=c1*A(j,k)-s1*A(i,k)
!print*,A(i,k),A(j,k),s1,c1
        A(i,k)=s1*A(j,k)+c1*A(i,k)
        A(j,k)=tmp
!print*,'A_two',A(i,k),A(j,k),s1,c1,k
      enddo
!     update VT and A
      Do k = 1, N
        tmp=c2*VT(k,j)-s2*VT(k,i)
        VT(k,i)=s2*VT(k,j)+c2*VT(k,i)
        VT(k,j)=tmp
        tmp=c2*A(k,j)-s2*A(k,i)
        A(k,i)=s2*A(k,j)+c2*A(k,i)
        A(k,j)=tmp
!print*,'A_two_2',A(k,i),A(k,j)
      enddo

      A(j,i) = Zer
      A(i,j) = Zer

      return
      End
