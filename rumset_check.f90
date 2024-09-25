
program main
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
implicit none


integer::i,ii,iii,iiii,i1,i2,i3,i4,i5,i6,i7,j,Rid,orb1,orb2,sc,orbs(500),str2(2000,20),lonep,rumstrset(500,20),set_num(100),&
totrum,nae,niao,nlast,nstr
integer::qq1(5000),qq2(5000),qq(5000),bondq4(5000),m119,m20,rumset
character(5)::a

open(unit=33,file='set.dat',status='unknown')
read(33,*)nstr,nae
do i=1,nstr
read(33,*)(str2(i,j),j=1,nae)
print*,(str2(i,j),j=1,nae)
enddo
open(unit=31,file='rumset.dat',status='unknown')
totrum=2520
i7=14
niao=2
lonep=0
nlast=0
!print*,'totrum',totrum,lonep,i7
j=0
!do i=1,i7
!print*,(str2(i,i1),i1=1,nae)
!enddo
!write(23,*)nae,lonep,(perm(i),i=1,nae-lonep*2)
!write(*,*)nae,lonep,(perm(i),i=1,nae-lonep*2)
!stop

Rid=0
rewind(31)
do i=1,totrum
!print*,'set_num',i
iiii=0
read(31,*)
 do i1=1,i7
read(31,*)(rumstrset(i1,i2),i2=1,nae)
!print*,'RUM',(rumstrset(i1,i2),i2=1,nae)
 enddo
  do i1=1,nstr
!print*,'*****************************'
   do i2=1,i7
iii=0
    do i3=lonep*2+1,nae-nlast,2
     do i4=lonep*2+1,nae-nlast,2
ii=0
      do i5=i3,i3+1
      do i6=i4,i4+1
!print*,str2(i1,i6),rumstrset(i2,i5)
if(rumstrset(i2,i5).eq.str2(i1,i6))then
ii=ii+1
goto 249
endif
      enddo
249   enddo
if(ii.eq.2)then
iii=iii+ii
goto 250
endif
     enddo
250 enddo
if(iii.eq.nae-lonep*2-nlast)then
iiii=iiii+1
!print*,'iiii',iiii
goto 251
endif
   enddo
251 enddo
if(iiii.eq.nstr)then
j=j+1
Rid=1
set_num(j)=i
print*,'number of structure match with set',i,'is',iiii
!stop
!else
!print*,'number of structure match with set',i,'is',iiii
endif
252 enddo

!nrs=j

print*,'Rid',Rid
stop
if(Rid.eq.1)then
rumset=rumset+1
!write(*,*)'set number',rumset,(perm(i),i=1,nae-lonep*2)
!stop
print*,'rumset_number',i
do m119=1,i7
print*,(str2(m119,j),j=1,nae)
enddo
write(23,*),'Set_number=',rumset
write(23,*)
endif


900 format(I3,2x,I3,2x,I3,2x,I3,x,a,x,25I4)
901 format(I3,2x,I3,2x,I3,2x,I3,x,a,x,I2,a,I2,x,25I4)
909 format(I3,2x,I3,2x,I3,2x,I3,x,a,x,I3,I3,x,25I4)

300 stop
end program main
