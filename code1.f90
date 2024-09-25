!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program main
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
implicit none

integer::i,j,k,k1,k2,k3,k4,k5,k6,k7,l,l1,MNP,MDP,MNN,ind1,ind2,ind3,io
character(len=100)::line3(100000),line4(200),line1,line(30),xmifile,xmofile,file_name,file_name1,line5(15),xmioeof,&
orbfile,gusoeof,xmooeof
character(len=6)::a
logical::fileexists

!!k1= number to start reading line
k1=26
!!k2= number to end reading line for checking
k2=30
!!k3= number of the end of each structure line
k3=50
!!k4= number of structure in one set to read
k4=5
!!k5= number sets to read
k5=1518
!k6=number of lines in between two sets
k6=4
!!file_name= name of the input output file produced by the program
file_name='test_new_R4'
!!file_name1= name of the input xmi file from where the other things to
!read(except the structures set to read from 'inputfile.xmi') 
file_name1='R4_test.xmi'
!line1= inactive orbitals
line1='1:5'

open(unit=23,file=file_name1,status='old')
do
read(23,'(a)',iostat=io)

if(io.ne.0)exit
MNP=MNP+1
enddo
rewind(23)
print*,'MNP',MNP

do i=1,MNP-1
read(23,'(a)')line3(i)
if(trim(line3(i)(1:4)).eq.'$str')then
ind3=i
endif
enddo
rewind(23)
do i=1,ind3
read(23,'(a)')line5(i)
print*,line5(i)
enddo
print*,'ind3333333333333333333333',ind3
do i=ind3+1,MNP
read(23,'(a)')line4(i)
print*,line4(i)
enddo

rewind(23)
open(unit=21,file='inputfile.xmi',status='old')

do
read(21,'(a)',iostat=io)

if(io.ne.0)exit
MDP=MDP+1
enddo
rewind(21)
print*,'MDP',MDP
ind2=MDP

k7=0
do i=1,MDP-1
read(21,'(a)')line3(i)
!print*,'iiii',i
if(trim(line3(i)(k1:k2)).eq.'6   6'.and.k7.eq.0)then
k7=1
ind1=i
print*,'ind1,ind2',ind1,ind2
goto 100
endif
!if(trim(line3(i)(k1:k2)).eq.'8   8')then
!ind2=MDP
!endif
enddo

100 rewind(21)

do i=1,ind1-1
read(21,*)
enddo
l=0
l1=0
open(unit=25,file='results.dat',status='unknown')
do j=1,k5
write(25,*)'set number VBSCF',j,k5
print*,'set number',j,k5
if(j.le.9)then
write(a,'(I1)')j
endif
if(j.gt.9.and.j.lt.100)then
write(a,'(I2)')j
endif
if(j.gt.99.and.j.lt.1000)then
write(a,'(I3)')j
endif
if(j.gt.999.and.j.lt.10000)then
write(a,'(I4)')j
endif
xmifile=trim(file_name)//trim('_')//trim(a)//trim('.xmi')
orbfile=trim(file_name)//trim('_')//trim(a)//trim('.orb')
xmioeof=trim(file_name)//trim('_oeo_')//trim(a)//trim('.xmi')
gusoeof=trim(file_name)//trim('_oeo_')//trim(a)//trim('.gus')
print*,'xmifile= ',xmifile
open(unit=22,file=xmifile,status='unknown')
open(unit=32,file=xmioeof,status='unknown')
do i=1,k4
l1=l1+1
read(21,'(a)')line(i)
!print*,'linelineline',i,k4,line(i)
enddo
do i=1,ind3
write(22,*)line5(i)
enddo
do i=1,k4
write(22,999)trim(line1),line(i)(k1:k3)
enddo
do i=ind3+1,MNP
write(22,*)line4(i)
enddo
if(l1.lt.ind2)then
do i=1,k6
l1=l1+1
read(21,*)
enddo
open(unit=121,file='script',status='unknown')
write(121,111)'xmvb',xmifile
close(121)
CALL SYSTEM ("chmod +x script ")
CALL SYSTEM ("./script ")
xmofile=trim(file_name)//trim('_')//trim(a)//trim('.xmo')
xmooeof=trim(file_name)//trim('_oeo_')//trim(a)//trim('.xmo')
print*,'xmofile= ',xmofile

105 INQUIRE(FILE=xmofile,EXIST=fileexists)
IF (fileexists) THEN
open(unit=27,file=xmofile,status='unknown')

else
goto 105
ENDIF

rewind(27)
104 MNN=0
do
read(27,'(a)',iostat=io)

if(io.ne.0)exit
MNN=MNN+1
enddo
rewind(27)

k=0
do i=1,MNN
!print*,'MNNoooooooooooo',i,MNN
read(27,'(a)')line3(i)
!print*,trim(line3(i)(2:13))
!print*,line3(i)
if(trim(line3(i)(23:43)).eq.'WEIGHTS OF STRUCTURES')k=i
if(i.gt.k+1.and.i.le.k+1+5) write(25,*)line3(i)
if(trim(line3(i)(2:13)).eq.'Job Finished')goto 102
enddo
rewind(27)
goto 104
!endif
!goto 101
102 CALL SYSTEM ("rm script")
!enddo

write(25,*)'set number VBOEO',j,k5
k=0
open(unit=121,file='script',status='unknown')
write(121,112)'cp',trim(orbfile),trim(gusoeof)
write(121,112)'tail -22',xmifile,'> tail'
write(121,111)'cat head tail>>',trim(xmioeof)
write(121,111)'xmvb',xmioeof
close(121)
CALL SYSTEM ("chmod +x script ")
CALL SYSTEM ("./script ")
205 INQUIRE(FILE=xmooeof,EXIST=fileexists)
IF (fileexists) THEN
open(unit=37,file=xmooeof,status='unknown')

else
goto 205
ENDIF

rewind(37)
204 MNN=0
do
read(37,'(a)',iostat=io)

if(io.ne.0)exit
MNN=MNN+1
enddo
rewind(37)

k=0
do i=1,MNN
!print*,'MNNoooooooooooo',i,MNN
read(37,'(a)')line3(i)
!print*,trim(line3(i)(2:13))
!print*,line3(i)
if(trim(line3(i)(23:43)).eq.'WEIGHTS OF STRUCTURES')k=i
if(i.gt.k+1.and.i.le.k+1+5) write(25,*)line3(i)
if(trim(line3(i)(2:13)).eq.'Job Finished')goto 202
enddo
rewind(37)
goto 204


goto 202
endif
goto 101
202 CALL SYSTEM ("rm script")
enddo



999 format(a,2x,a)
111 format(a,x,a)
112 format(a,x,a,x,a)

101 stop
end program main

