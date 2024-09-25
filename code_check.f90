program main

implicit none

integer::i,j,k,line,ent,tot,val(100,100),val1(100),l,add,cnt,ze,opt
character(4)::a

open(unit=7,file='val.dat',status='unknown')

read(7,*)line,ent,tot,opt
do i=1,line
read(7,*)(val(i,j),j=1,ent)
enddo
!do i=1,line
!print*,'val',(val(i,j),j=1,ent)
!enddo

if(opt.eq.1)then
ze=0
do k=1,tot
cnt=0
l=0
add=0
do i=1,100
val1(i)=0
enddo

do i=1,line
do j=1,ent
!print*,val(i,j)
if(val(i,j).eq.k.or.val(i,j).eq.-k)then
!print*,'i',i
cnt=1
l=l+1
val1(l)=val(i,j)
add=add+val(i,j)
endif
enddo
enddo
!print*,(val1(i),i=1,l),'not cancelled >',add/k
if(add.eq.0)ze=ze+1
if(cnt.eq.1)print*,k,'> not cancelled >',add/k
if(cnt.eq.0)print*,'det',k,'is not present in this','line','structures'
enddo
print*,'number of zeros',ze
endif

if(opt.eq.2)then
do i=1,100
val1(i)=0
enddo
do j=1,line-1
l=0
do i=1,ent
do k=1,ent
if(val(14,i).eq.val(j,k).or.val(14,i).eq.-val(j,k))then
l=l+1
val1(l)=i
endif
enddo
enddo
print*,'str',j,'>',(val1(i),i=1,l),'ovlp',l
enddo
endif

stop
end program main
