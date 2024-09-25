program main

implicit none

integer:: argnum,l
logical :: fileexists
character(len=5)  ::lname1,lname2
character(len=70)  :: inputfilename1,inputfilename2,infname1,infname2

argnum=iargc()
if(argnum.lt.2)then
PRINT*,'input file not exist please put the inputfile name'
stop
endif
call getarg(1,inputfilename1)
INQUIRE(FILE=TRIM(inputfilename1),EXIST=fileexists)
IF (fileexists) THEN
infname1=trim(inputfilename1)
l=len(TRIM(infname1))
lname1=infname1(l-4:l)
endif

if(lname1.ne.'.fchk') then
!print*,inputfilename1,'is not a .fchk file'
stop
endif

!print*,lname1
if(lname1.eq.'.fchk')call read_fchk_file(inputfilename1)

call getarg(2,inputfilename2)
INQUIRE(FILE=TRIM(inputfilename2),EXIST=fileexists)
IF (fileexists) THEN
infname2=trim(inputfilename2)
l=len(TRIM(infname2))
lname2=infname2(l-4:l)
if(lname2.ne.'.xdat') then
!print*,inputfilename2,'in not a .xdat file'
stop
endif
!print*,'reading the file: ',gusfilename
ELSE
PRINT*,'SORRY .xdat file does not exist in this folder or you may not provide the filename at all'
stop
ENDIF

!print*,lname2
if(lname2.eq.'.xdat') call read_xdat_file(inputfilename2)

call write_chk

stop
end program main
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
subroutine read_fchk_file(fchk)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
implicit none
character(len=70)  :: fchk
character(len=90)  ::lines,lines_fchk
integer::i,j,k,iii,MDP,io,a6,b1,b2,k1,c1,c2,inl
real::mocofi,vbcofi
character(len=5)  ::a1,a2,a3,a4,a5

!common/values/vbcofi(100000),mocofi(100000),lines_fchk(10000),c1,c2,a6,inl
common/values/vbcofi(10000000),mocofi(10000000),lines_fchk(1000000),c1,c2,a6,inl

open(unit=21,file=fchk,status='old')
MDP=0
do
read(21,'(a)',iostat=io)
if(io.ne.0)exit
MDP=MDP+1
enddo
rewind(21)
do iii=1,MDP
read(21,'(a)')lines
if(lines(1:21).eq.'Alpha MO coefficients')then
i=iii
inl=iii
endif
enddo

rewind(21)
do j=1,i-1
read(21,*)
enddo
read(21,*)a1,a2,a3,a4,a5,a6
b1=a6/5
k1=1
do j=1,b1
read(21,*)(mocofi(k),k=k1,k1+4)
!print*,(mocofi(j,k),k=1,5)
k1=k1+5
enddo
if(mod(a6,5).ne.0)then
b2=mod(a6,5)
read(21,*)(mocofi(k),k=k1,k1+b2-1)
!print*,(mocofi(b1+1,k),k=1,b2)
endif
rewind(21)

do j=1,inl
read(21,'(a)')lines_fchk(j)
enddo
rewind(21)

!do j=1,a6
!print*,'mocofi',j,mocofi(j)
!enddo



return
end subroutine read_fchk_file
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
subroutine read_xdat_file(xdat)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
implicit none
character(len=70)  :: xdat
character(len=90)  ::lines,lines_fchk

integer::i,j,iii,io,MDP,c1,c2,k,k1,b1,b2,j1,j2,a6,inl
real::vbcofi,mocofi

!common/values/vbcofi(100000),mocofi(100000),lines_fchk(10000),c1,c2,a6,inl

common/values/vbcofi(10000000),mocofi(10000000),lines_fchk(1000000),c1,c2,a6,inl

open(unit=23,file=xdat,status='old')

MDP=0
do
read(23,'(a)',iostat=io)
if(io.ne.0)exit
MDP=MDP+1
enddo
rewind(23)
do iii=1,MDP
read(23,'(a)')lines
if(lines(10:45).eq.'Orbitals in terms of basis functions')then
i=iii
endif
enddo

rewind(23)

do j=1,i+1
read(23,*)
enddo
read(23,*),c1,c2
k1=1
do j1=1,c2
b1=c1/5
!print*,b1
do j2=1,b1
!print*,j2
read(23,*)(vbcofi(k),k=k1,k1+4)
!print*,(vbcofi(k),k=k1+1,k1+4)
k1=k1+5
enddo
if(mod(c1,5).ne.0) then
b2=mod(c1,5)
read(23,*)(vbcofi(k),k=k1,k1+b2-1)
!print*,'k1+b2-1',k1+b2-1
k1=k1+b2
endif
enddo
rewind(23)
!do j=1,c1*c2
!print*,'vbcofi',j,vbcofi(j)
!enddo


return
end subroutine read_xdat_file

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine write_chk
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

implicit none
integer::i,j,i1,c1,c2,a6,inl,b1,b2,k1,k
real::vbcofi,mocofi,finalcofi(10000000)
character(len=90)  ::lines,lines_fchk

common/values/vbcofi(10000000),mocofi(10000000),lines_fchk(1000000),c1,c2,a6,inl

do i=1,c1*c2
!print*,'i',i,c1*c2
finalcofi(i)=vbcofi(i)
enddo
i1=0
do j=(c1*c2)+1,a6
!print*,'j',j,a6
finalcofi(j)=mocofi(j)
enddo

do i=1,a6
!print*,i,vbcofi(i),mocofi(i),finalcofi(i)
enddo

do i=1,inl
write(*,101),lines_fchk(i)
enddo
b1=a6/5
k1=1
do i=1,b1
write(*,100)(finalcofi(k),k=k1,k1+4)

k1=k1+5
enddo
if(mod(a6,5).ne.0)then
b2=mod(a6,5)
write(*,100)(finalcofi(k),k=k1,k1+b2)
endif

100 format(x,E15.8,x,E15.8,x,E15.8,x,E15.8,x,E15.8)
101 format(a)



return
end subroutine write_chk
