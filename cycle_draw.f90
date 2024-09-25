
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 program main
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
integer::i1,i2,i,n
real::ang,x,y,nx,ny,coordsc(20,3),an,x1,y1,nx1,ny1,coordsh(20,3),r1,r2

print*,'please mention number of arms'
read(*,*)n
print*,'distance between C C atoms'
read(*,*)r1
print*,'distance between C H atoms'
read(*,*)r2

ang=(360.0/n)*(3.14159265359/180.0)
x=(r1/2.0)/sin(ang/2.0)
y=0.0
x1=x+r2
y1=0.0

an=0.0
do i=1,n
an=an+ang
nx=x*sin(an)+y*cos(an)
ny=x*cos(an)-y*sin(an)
nx1=x1*sin(an)+y1*cos(an)
ny1=x1*cos(an)-y1*sin(an)

coordsc(i,1)=nx
coordsc(i,2)=ny
coordsh(i,1)=nx1
coordsh(i,2)=ny1
!x=nx
!y=ny
enddo

do i=1,n
print*,'C',(coordsc(i,j),j=1,2),'0.00000'
enddo
do i=1,n
print*,'H',(coordsh(i,j),j=1,2),'0.00000'
enddo

stop
end program main
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
