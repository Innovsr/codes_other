program main

implicit none

integer::i,j,k,nao,niao,MDP,inl,io,ii,iii,l,orbn(100)
character(len=70)::infname,lines,lines1(1000),filename,inpname,inptname
character(len=300)::lines2(100)
character(len=2)::a
logical::result
character(len=20)::command

print*,'put the number of active orbitals'
read(*,*)nao
print*,'put the number of inactive orbitals'
read(*,*)niao
print*,'name of the input file'
read(*,'(a)')infname

open(unit=21,file=infname,status='old')

MDP=0
do
read(21,'(a)',iostat=io)
if(io.ne.0)exit
MDP=MDP+1
enddo
rewind(21)
do iii=1,MDP
read(21,'(a)')lines
if(lines(1:4).eq.'$orb')then
i=iii
print*,'i',i,MDP
inl=iii
endif
enddo
rewind(21)

do i=1,inl
read(21,'(a)')lines1(i)
enddo
read(21,*)(orbn(j),j=1,nao+niao)
write(*,*)(orbn(j),j=1,nao+niao)
do i=inl+2,MDP
read(21,'(a)')lines2(i)
enddo
rewind(21)

do ii=1,1
if(ii.le.9)then
write(a,'(I1)')ii
endif
if(ii.gt.9)then
write(a,'(I2)')ii
endif
l=len(TRIM(infname))
inpname=infname(1:l-4)
inptname=infname(l-3:l)

filename=trim(inpname)//trim('_')//trim(a)//trim(inptname)

open(unit=ii,file=filename,status='unknown')

do i=1,inl
write(ii,100),lines1(i)
enddo
if(mod(ii,2).eq.0)then
write(ii,100),'0*9 27 45 27'
endif
if(mod(ii,2).ne.0)then
write(ii,100),'27 45 27 0*9'
endif
do i=inl+2,MDP
write(ii,'(a)'),lines2(i)
enddo
rewind(23)
100 format(a)

open(unit=121,file='script',status='unknown')
write(*,*)'xmvb',filename
command='chmod +x script'
!result= SYSTEMQQ(command)
CALL SYSTEMQQ ("./script ")
!CALL EXECUTE_COMMAND_LINE("chmod +x script")
!CALL EXECUTE_COMMAND_LINE("./script")
enddo
end program main
