program main
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

implicit none

integer::i,j,k,i1,i2,i3,nrow,stcol,edcol,rcol,argnum
real::val,av_val(300),table(2000,300),l
character(50)::name(300),inputfilename

argnum=iargc()
if(argnum.eq.0)then
PRINT*,'SORRY input file does not exist'
stop
else
call getarg(1,inputfilename)
open(unit=1,file=trim(inputfilename),status='old')
open(unit=2,file='graph.dat',status='unknown')
endif
read(1,*)nrow,stcol,edcol,rcol
read(1,*)(name(i),i=1,edcol)
print*,(name(i),i=1,edcol)
do i=1,nrow
read(1,*)(table(i,i1),i1=1,edcol)
print*,table(i,26)
enddo

do j=stcol,edcol
k=0
do i=1,300
l=0.0
val=0.0
do i1=1,nrow
if(int(table(i1,j)).eq.i)then
l=l+1.0
k=k+1
val=val+table(i1,rcol)
endif
enddo
av_val(i)=val/l
if(k.eq.nrow)goto 102
enddo
102 write(2,*)
write(2,*)
write(2,103)trim(name(j)),',',trim(name(rcol))
do i1=1,i
write(2,100)i1,',',av_val(i1)*100
enddo
100 format(I3,1x,a,1x,F9.5)
103 format(a,1x,a,1x,a)
enddo

stop
end program main

