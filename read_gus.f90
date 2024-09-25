!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!***************************************************************!
!***************************************************************!
!*********This code is written by DR. SOURAV ROY****************!
!*****************Post-Doctoral Fellow *************************!
!***************    School Of Pharmacy   ***********************!
!****** Under the Supervision of Prof. Avital Shurki  **********!
!*********The Hebrew University of Jerusalem, Israle ***********!
!*******************February,2017 **********************************!
!***************************************************************!
!***************************************************************!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!****************************************************************************************************
!!! ************************************Format of the
!inputfile**************************************
!****************************************************************************************************


module commondat
implicit none

real,public::dongroup,accgroup,at_rad
real::dummy,min_grd,grd_sp,ocdi,ocm1,ocm2
!real::val(1000,1000)
integer::mono1,mono2,at_covrad,mno_at,nacc,ndon,third,fg9,fg8,fg7,fg6,fg5,fg3,fg2,fg1,mnopt,mon1,mon2&
,mnm1,mnm2,steps,MNXY,MNZ,MN,fg4,noorbdi,noorbm1,noorbm2,NMOM1,NMOM2,orbdi(350),orbm1(350),orbm2(350)&
,flg1,flg2,flg3,nmon1,nmon2,&
sdimo(1000),pdimo(1000),ddimo(1000),sdi,pdi,ddi,sm1,sm2,pm1,pm2,dm1,dm2,sm1mo(1000),pm1mo(1000),dm1mo(1000),&
sm2mo(1000),pm2mo(1000),dm2mo(1000),ddi1,ddimo1(1000),dm11,dm1mo1(1000),dm21,dm2mo1(1000),noinp,lopt(5),f(1000)&
,tngus,str_num_run(1000,1000),naelec,tnstr,tot_str(5000),c(1000),orb_no(1000),tot_str_2(5000),iheader,totnstr,tnumorb &
,atom_coef(1000),tnum_ele_str(100),str_num_run2(1000,1000),totnstr2
character(len=18)::wordstr(40)
character(15):: basis,gauss
character(90):: ingfname,infname,lname,inputfilename,gusfilename,line(100),line4(200),line_orb(100)
character(80):: val(1000,1000)
character(10):: funal
character(len=3)::d(1000)
character(len=8)::bb(1000)
character(len=2):: stt,atmdi(15),atmm1(15),atmm2(15),aa(1000),e(1000)
character(len=2),public :: accst, donst,at_list(88)
!character(len=90),public:: inputfilename,lname,gusfilename
character(len=70)::line1,line2,line3
character(len=90)::line13,line27,line28,line29
character(len=110)::line35,line36,line37
character(len=110)::inp_log(5)
character(len=35)::molname,cubname,logname,mononame1,mononame2,dimname,chknamedi,chknamem1,chknamem2&
,fchknamedi,fchknamem1,fchknamem2,cubnamedi,cubnamem1,cubnamem2,lognamem2,lognamem1,lognamedi&
,substract,integral,cubnamemodi,cubnamemom1,cubnamemom2,substract_oct,dimer_oct,mono1_oct,mono2_oct
character(len=2),public:: ch1, ch2, sp1, sp2, disc, ch0,sp0
dimension at_covrad(88),at_rad(88)

common/coord/dummy(300,3),ocdi,ocm1,ocm2,tnumorb,atom_coef,str_num_run2,tnum_ele_str,totnstr2,totnstr
common /atom/accst(100),donst(100),stt(300),min_grd(3),steps(3),grd_sp
common /options/basis,funal,ch0,sp0,ch1,sp1,ch2,sp2,gauss
common /allname/molname,cubname,logname,mononame1,mononame2,dimname,chknamedi,chknamem1,chknamem2&
,fchknamedi,fchknamem1,fchknamem2,cubnamedi,cubnamem1,cubnamem2,lognamem2,lognamem1,lognamedi&
,substract,integral,line13,cubnamemodi,cubnamemom1,cubnamemom2,substract_oct,dimer_oct,&
mono1_oct,mono2_oct,inp_log,ingfname,infname,lname,line_orb,line,line4
common /input/line1,line2,line3,wordstr,line27,line28,line29,line35,line36,line37,aa,bb,d,e,val
common /mono/mono1,mono2,MNXY,MNZ,MN,NMOM1,NMOM2
common /group/dongroup(300,3),accgroup(300,3),mon1(200),mon2(200),orbdi,orbm1,orbm2
common /dat/mno_at,nacc,ndon,third,fg9,fg8,fg7,fg6,fg5,fg4,fg3,fg2,fg1,mnopt,mnm1,mnm2,noorbdi,noorbm1,noorbm2&
,flg1,flg2,flg3,&
sdimo,pdimo,ddimo,sdi,pdi,ddi,sm1,sm2,pm1,pm2,dm1,dm2,sm1mo,pm1mo,dm1mo,&
sm2mo,pm2mo,dm2mo,ddi1,ddimo1,dm11,dm1mo1,dm21,dm2mo1,noinp,lopt,nmon1,nmon2,f,tngus,str_num_run,naelec,tnstr,&
tot_str



save
end module commondat
!!***************************************************************

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program main

use commondat
implicit none

real::finish,start,time1
integer:: argnum,k,l,ndate,j,i,l3,N
logical :: fileexists
character(len=18) :: orbin
character(len=35)::name,name2,name1
!character(len=35)::mononame1, mononame2, dimname
character(len=55)::namo
character(len=2) :: atomin,natom
character(len=8)  :: date
character(len=10) :: time
character(len=5)  :: zone,NMDI
character(len=50)  :: inplogname
integer,dimension(8) :: values



argnum=iargc()
if(argnum.eq.0)then
PRINT*,'input file not exist please put the inputfile name'
stop
endif

!if(argnum.gt.1)then
!print*,'please put only one argument here : inputfile name'
!stop
!endif

call getarg(1,inputfilename)
INQUIRE(FILE=TRIM(inputfilename),EXIST=fileexists)
IF (fileexists) THEN
infname=trim(inputfilename)
l=len(TRIM(inputfilename))
lname=inputfilename(l-3:l)

if(lname.ne.'.xmi') then
print*,inputfilename,'is not a inputfile'
stop
endif
print*,'reading the file: ',inputfilename
ELSE
PRINT*,'SORRY .xmi file does not exist in this folder or you may not provide the &
filename at all'
stop
ENDIF
if(lname.eq.'.xmi') call read_xmi

call getarg(2,gusfilename)
INQUIRE(FILE=TRIM(gusfilename),EXIST=fileexists)
IF (fileexists) THEN
ingfname=trim(gusfilename)
l=len(TRIM(ingfname))
lname=ingfname(l-3:l)
if(lname.ne.'.gus') then
print*,gusfilename,'in not a gus file'
stop
endif
print*,'reading the file: ',gusfilename
ELSE
PRINT*,'SORRY .gus file does not exist in this folder or you may not provide the &
filename at all'
stop
ENDIF

if(lname.eq.'.gus') call read_gus
call write_gus
call write_xmi

stop
end program main

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine read_xmi
!reading of .log file for molecular coordinates
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
use commondat
implicit none
integer :: k,i,i1,i2,i3,i4,i5,i6,i7,i8(1000),i9,i10,i12,i13,i14,i15,i16,a,b,ll,count(50000,10000), io, MDP,str_num(1000,100),iii,&
str_num_2(1000,100),l1,l2,l3,l4,l6,l7,l8&
,count_orb(5000),jj,count_orb1(5000),count_orb2(5000),atom_coef_2,tot_orb, &
j,inactive
!character(len=56)::lines
character(len=35)::iname,logn
character(len=70)::lines,line5,line7,line8
character(len=8)  :: date,line6(500,100)
character(len=10) :: time
character(len=5)  :: zone
integer,dimension(8) :: values



print*,'inside the read_xmi subroutine'
open(unit=21,file=infname,status='old')
MDP=0
do
read(21,'(a)',iostat=io)
if(io.ne.0)exit
MDP=MDP+1
enddo
rewind(21)

i=1
j=1
do iii=1,MDP
read(21,'(a)')lines
if(lines(1:4).eq.'$str')then
!if(iii.gt.i)i=iii
i=iii
iheader=iii
endif
if(lines(1:4).eq.'$orb')then
!if(iii.gt.i)i=iii
j=iii
endif
enddo
rewind(21)
!print*,i
do i1=1,i
read(21,'(a)')line(i1)
enddo
i3=1
i7=0
do i4=i+1,MDP-1
i7=i7+1
read(21,'(a)')line4(i7)
!read(21,*)line4(i7)
!print*,i4,line4(i7)
if(trim(line4(i7)).eq.'$end') goto 557
ll=len(trim(line4(i7)))
!print*,'llll',ll
do k=1,ll+1
line5=line4(i7)(k:k)
if(line5.eq.''.or.line5.eq.':')then
count(i7,i3)=k
!print*,'gap',count(i7,i3),i7,i3
i3=i3+1
endif
enddo
i8(i7)=i3-1
i3=1
!print*,'i8(i7)',i8(i7)
enddo

557 rewind(21)
totnstr=i7-1
!tnstr=0
!!!!!!!!!!!! read the structures without # for further arrangment and use to
!!!!!!!!!!!!arrange the gus file !!!!!!!!!!!!!
!goto 558
l6=0
do i5=1,i7-1
!print*,'iiiiiiiiiiiiiiiiiiiiiiiiiiiiiii',i5,ll
!if(line4(i5)(1:1).ne.'#')then
!tnstr=tnstr+1
!endif
!print*,'tnstr',tnstr
i9=0
do i6=1,i8(i5)-1
!print*,'line4(i5)(1:3)',line4(i5)(1:3)
if(i6.eq.1.and.line4(i5)(1:1).ne.'#')then
i9=i9+1
!print*,line4(i5)(1:count(i5,1)-1),line4(i5)(count(i5,1)+1:count(i5,2))
read(line4(i5)(1:count(i5,1)-1),'(i10)')str_num(i5,i9)
i9=i9+1
read(line4(i5)(count(i5,1)+1:count(i5,2)),'(i10)')str_num(i5,i9)
endif

if(count(i5,i6+1)-count(i5,i6).ne.1.and.line4(i5)(1:1).ne.'#'.and.i6.ne.1) then
!print*,'count',i5,i6,count(i5,i6),count(i5,i6+1),i8(i5)
!print*,i5,i6
i9=i9+1
a=count(i5,i6)
b=count(i5,i6+1)
!print*,'a,b',a,b
!print*,line4(i5)(a:b)
read(line4(i5)(a:b),'(i10)')str_num(i5,i9)
!str_num(i5,i9)=ichar('line6(i5,i9)')
endif
enddo
naelec=i9
if(i5.ne.1.and.line4(i5)(1:1).ne.'#')then
l3=0
l4=1
l1=0
l3=str_num(i5,1)
do l2=1,str_num(i5,2)-str_num(i5,1)+1
str_num_run(i5,l2)=l3
l6=l6+1
tot_str(l6)=l3
!print*,l6,tot_str(l6)
!print*,l2,str_num_run(i5,l2)
l3=l3+1
l4=l4+1
enddo
str_num_run(i5,l4)=str_num(i5,3)
l6=l6+1
tot_str(l6)=str_num(i5,3)
!print*,l6,tot_str(l6)
l1=l4
do i1=4,i9
if (str_num(i5,i1-1).ne.str_num(i5,i1))then
l1=l1+1
str_num_run(i5,l1)=str_num(i5,i1)
l6=l6+1
tot_str(l6)=str_num(i5,i1)
!print*,l6,tot_str(l6)
!print*,str_num_run(i5,l1),l1
endif
enddo
tnstr=l6
!print*,'str1',(str_num_run(i5,i1),i1=1,l1),l1
endif
enddo
i10=i9

l8=0
do l7=1,tot_str(1)-1
l8=l8+1
tot_str_2(l8)=l7
enddo

do l7=1,l6
l8=l8+1
tot_str_2(l8)=tot_str(l7)
enddo
tnstr=l8
!print*,'tnstr',tnstr
i15=0
do i5=1,i7-1
if(line4(i5)(1:2).eq.'##')then
i15=i15+1
endif
enddo
totnstr2=i15
!print*,'totnstr2',totnstr2


!!!!!!!!!!!! read the structure numbers with ## for further arrangement !!!!!!!!!!! 
i16=0
do i5=1,i7-1
print*,'iiiiiiiiiiiiiiiiiiiiiiiiiiiiiii',i5,i7-1
i9=0
do i6=1,i8(i5)-1
if(i6.eq.1.and.line4(i5)(1:2).eq.'##')then
i9=i9+1
!print*,line4(i5)(1:count(i5,1)-1),line4(i5)(count(i5,1)+1:count(i5,2))
read(line4(i5)(3:count(i5,1)-1),'(i10)')str_num_2(i5,i9)
i9=i9+1
read(line4(i5)(count(i5,1)+1:count(i5,2)),'(i10)')str_num_2(i5,i9)
endif
if(count(i5,i6+1)-count(i5,i6).ne.1.and.line4(i5)(1:2).eq.'##'.and.i6.ne.1) then
i9=i9+1
a=count(i5,i6)
b=count(i5,i6+1)
read(line4(i5)(a:b),'(i10)')str_num_2(i5,i9)
endif
enddo

!!!!!!!!!!!!arrange the structure numbers for the orbitas in xmi files !!!!!!!

!print*,'sourav'
if(i5.ne.1.and.line4(i5)(1:2).eq.'##')then
i16=i16+1
l3=0
l4=1
l1=0
l3=str_num_2(i5,1)
!print*,l3
do l2=1,str_num_2(i5,2)-str_num_2(i5,1)+1
str_num_run2(i16,l2)=l3
l3=l3+1
l4=l4+1
enddo
str_num_run2(i16,l3)=str_num_2(i5,3)
l1=l4
do i1=4,i9
if (str_num_2(i5,i1-1).ne.str_num_2(i5,i1))then
l1=l1+1
str_num_run2(i16,l1)=str_num_2(i5,i1)
endif
enddo
tnum_ele_str(i16)=l1
!print*,'str2',(str_num_run2(i5,i),i=1,l1),tnum_ele_str(i5)
print*,'str2',tnum_ele_str(i16),i16
endif
enddo


do i1=1,j
read(21,*)
enddo

i3=1
i5=1
559 read(21,'(a)')line7
if(trim(line7).eq.'$end') goto 558
ll=len(trim(line7))
!print*,'line7',line7
do k=1,ll+1
line5=line7(k:k)
if(line5.eq.'#')goto 559
if(line5.eq.'')then
count_orb(i3)=k
!print*,'orb1 ',count_orb(i3),i3
i3=i3+1
i4=i4+1
endif
if(line5.eq.'*')then
count_orb2(i5)=k
count_orb(i3)=k
!print*,'orb2',count_orb(i3),i3
!print*,'orb2 ',count_orb2(i5),i5
i3=i3+1
i5=i5+1
endif
enddo
i12=i3-1
i13=i5-1
558 rewind(21)

!print*,line7
i9=0
jj=0
if(count_orb(1).ge.2)then
i9=i9+1
read(line7(1:1),'(i10)')atom_coef(i9)
endif
do i6=1,i12-1

if(i13.ne.0)then
do i14=1,i13
if(count_orb(i6).eq.count_orb2(i14)) then
!print*,line7(count_orb(i6)+1:count_orb(i6+1)),count_orb(i6),count_orb(i6+1),i6
read(line7(count_orb(i6)+1:count_orb(i6+1)),'(i10)')atom_coef_2
jj=atom_coef_2+jj
endif
enddo

!if(i13.ne.0)then
!print*,i6
do i14=1,i13
!print*,'***********',count_orb(i6),count_orb2(i14)
if(count_orb(i6).eq.count_orb2(i14).or.count_orb(i6+1).eq.count_orb2(i14))goto 660
enddo
endif
!print*,line7
if(count_orb(i6+1)-count_orb(i6).ne.1.and.line7(1:1).ne.'#') then
!print*,'count',i5,i6,count(i5,i6),count(i5,i6+1),i8(i5)
!print*,i5,i6
i9=i9+1
a=count_orb(i6)
b=count_orb(i6+1)
!print*,'a,b',a,b
!print*,'44444',line7(a:b),count_orb(i6+1)
read(line7(a:b),'(i10)')atom_coef(i9)
!str_num(i5,i9)=ichar('line6(i5,i9)')

endif
660 enddo
print*,'atomic',(atom_coef(i),i=1,i9)
!print*,'total orbital = ',jj+i9
tot_orb=jj+i9

do i1=1,j+1
read(21,*)
enddo
do i=1,jj+i9
562 read(21,'(a)'),line8
if(line8(1:1).eq.'#') goto 562
!if(line8(1:1).eq.'   ') inactive=i
if(line8(1:1).eq.'   ') goto 562

line_orb(i)=line8
enddo
rewind(21)
tnumorb=jj+i9
do i=1,jj+i9
print*,line_orb(i)
enddo

return
end subroutine read_xmi


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine read_gus
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
use commondat
implicit none
logical::file1
integer::io,MDP,l,i,iii,count(100),g,h,i1,i2,i3,i4,i5,i6,i7,i8
!real::val(1000,1000)
character(len=50)::lines,XYZ1(5),XYZ2(5),XYZ3(5),a1,lognm
!character(len=2)::a(1000),e(1000)
!character(len=3)::d(1000)
character(len=8)::fmt1,fmt2,f2,ans
character(len=90)::logn,iname
character(len=8)  :: date
character(len=10) :: time
character(len=5)  :: zone
integer,dimension(8) :: values


print*,'inside the read_gus subroutine'
!iname=trim(inpgusname)
open(unit=23,file=ingfname,status='old')
MDP=0
do
read(23,'(a)',iostat=io)
if(io.ne.0)exit
MDP=MDP+1
enddo
rewind(23)

i=1
do iii=1,MDP
read(23,'(a)')lines
if(lines(1:9).eq.'# ORBITAL')then
!if(iii.gt.i)i=iii
count(i)=iii
!print*,i,c(i)
i=i+1
endif
enddo
rewind(23)
tngus=i-1
do i1=1,count(1)-1 
read(23,*)
enddo
do i2=1,tngus
!print*,i2
read(23,*)aa(i2),bb(i2),c(i2),d(i2),e(i2),f(i2)
!print*,aa(i2),bb(i2),c(i2),d(i2),e(i2),f(i2)
!g=mod(2*f,8)
g=2*f(i2)/8
h=f(i2)*2-g*8
!print*,g,h
i8=1
do i7=1,g
!read(23,*)(val(i2,i6),i6=i8,i8+7)
read(23,'(a)')val(i2,i8)
!print*,val(i2,i8)
i8=i8+1
enddo
if (h.ne.0)then
!read(23,*)(val(i2,i6),i6=i8,i8+h-1)
read(23,'(a)')val(i2,i8)
!print*,(val(i2,i6),i6=i8,i8+h-1)
i8=i8+1
endif
orb_no(i2)=i8-1
enddo
rewind(23)

546 return
end subroutine read_gus

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine write_gus
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
use commondat
implicit none
integer i,i1,i2,i3,str_fin_num(1000),g,h,i4,i5,i6,i7,i8
character(90)::gusname


do i=1,1000
str_fin_num(i)=0
enddo
gusname=trim(gusfilename)//trim('_new')
!print*,gusname
open(unit=19,file=gusname,status='unknown')
!print*,naelec,tnstr
!do i=1,tnstr
!print*,(str_num_run(i,i1),i1=1,naelec)
!enddo
i3=0
do i1=1,tnstr
do i2=1,tngus
!print*,c(i2),tot_str_2(i1)
if(c(i2).eq.tot_str_2(i1))then
i3=i3+1
str_fin_num(i3)=f(i2)
endif
enddo
enddo
!write(19,'(I5,I5)')(str_fin_num(i),i=1,tnstr)
write(19,900)(str_fin_num(i),i=1,i3)
900 format(5000(I5))
!write(*,900)(str_fin_num(i),i=1,i3)
i=0
do i1=1,tnstr
do i2=1,tngus
if(c(i2).eq.tot_str_2(i1))then
i=i+1
!write(19,*)aa(i2),bb(i2),c(i2),d(i2),e(i2),f(i2)
write(19,*)aa(i2),bb(i2),i,d(i2),e(i2),f(i2)
g=2*f(i2)/8
h=f(i2)*2-g*8
!print*,g,h
!i8=1
do i7=1,orb_no(i2)
!write(19,*)(val(i2,i6),i6=i8,i8+7)
write(19,'(a)')val(i2,i7)
!print*,val(i2,i7)
!i8=i8+1
enddo
!if (h.ne.0)then
!write(19,*)(val(i2,i6),i6=i8,i8+h-1)
!print*,(val(i2,i6),i6=i8,i8+h-1)
!i8=i8+1
!endif

endif
enddo
enddo

!rewind(19)

return
end subroutine write_gus

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine write_xmi
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

use commondat
implicit none
character(100)::xminame
integer i,i1,j,atom_coef_2(500)

xminame=trim(inputfilename)//trim('_new')
open(unit=17,file=xminame,status='unknown')
print*,xminame
do i=1,iheader
write(17,'(a)')line(i)
enddo
do i=1,totnstr
if(line4(i)(1:2).eq.'##')write(17,'(a)')line4(i)
enddo
write(17,'(a)')'&end'
write(17,'(a)')'&orb'
i1=0
do i=1,tnumorb
i1=i1+1
atom_coef_2(i1)=atom_coef(i)
enddo
do i=1,totnstr
do j=1,tnum_ele_str(i)
i1=i1+1
atom_coef_2(i1)=atom_coef(str_num_run2(i,j))
print*,str_num_run2(i,j)
enddo
enddo
write(17,901)(atom_coef_2(i),i=1,i1)
901 format(5000(I5))
do i=1,tnumorb
write(17,'(a)')line_orb(i)
enddo
write(17,*)
do i=1,totnstr2
print*,'444444',tnum_ele_str(i)
do j=1,tnum_ele_str(i)
write(17,'(a)')line_orb(str_num_run2(i,j))
!write(17,*),'*****',str_num_run2(i,j)
enddo
!write(17,*),'*****',i
if(i.lt.totnstr2)write(17,*)
enddo
!print*,totnstr2
write(17,'(a)')'&end'

return
end subroutine write_xmi
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine read_inst
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

use commondat
implicit none
integer i,j,k,l,m,MDP,iii

open(unit=15,file=inst.dat,status='old')
MDP=0
do
read(15,'(a)',iostat=io)
if(io.ne.0)exit
MDP=MDP+1
enddo
rewind(21)

i=0
j=0
k=0
l=0
m=0
do iii=1,MDP
read(15,'(a)')lines
if(lines(1:4).eq.'$end')then
!if(iii.gt.i)i=iii
i=iii
endif
if(lines(1:4).eq.'$orb')then
!if(iii.gt.i)i=iii
j=iii
endif
if(lines(1:7).eq.'replace'.or.lines(1:7).eq.'Replace')then
!if(iii.gt.i)i=iii
k=iii
endif
if(lines(1:5).eq.'noxmi')then
!if(iii.gt.i)i=iii
l=iii
endif
if(lines(1:5).eq.'nogus')then
!if(iii.gt.i)i=iii
m=iii
endif
enddo
rewind(15)
if(k.eq.0) flg1=1
if(l.eq.0) flg2=1
if(m.eq.0) flg3=1
if(flg1.ne.1)then
do i=1,k-1
read(15,*)
enddo
read(15,*)
endif

return
end subroutine read_inst
