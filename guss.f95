program gauss
implicit none
INTEGER::ng,ig,jg,jg1,ig2,ig3,jg3,ig4,kg
REAL::landag,sg
real,allocatable::ag(:,:),xg(:),bg(:)
READ(*,*)ng
OPEN(UNIT=12,FILE='z.txt',STATUS='old',ACTION='read')
OPEN(UNIT=10,FILE='ans.txt',STATUS='replace',ACTION='write')
ALLOCATE (ag(ng,ng+1),xg(ng),bg(ng+1))
do kg=1,ng
READ(12,*)ag(kg,:)
end do
PRINT*,"1-----------------------------------------------------------------"
 do ig=1,ng
if (ag(ig,ig)==0) then
do jg=ig+1,ng
if (ag(jg,ig)/=0) then
bg=ag(jg,:)
ag(jg,:)=ag(ig,:)
ag(ig,:)=bg
end if
end do
end if

do jg1=ig+1,ng
landag=-ag(jg1,ig)/ag(ig,ig)
ag(jg1,:)=(landag*ag(ig,:))+ag(jg1,:)
   write(10,*)ag(jg1,:)
end do
end do
  PRINT*
  PRINT*
 xg(ng)=ag(ng,ng+1)/ag(ng,ng)
 do ig3=ng-1,1,-1
 sg=0.
 do jg3=ig3+1,ng
 sg=sg+(ag(ig3,jg3)*xg(jg3))
 end do
 xg(ig3)=(ag(ig3,ng+1)-sg)/ag(ig3,ig3)
 end do
  do ig4=1,ng
  write(10,*)" x(",ig4,") = ",xg(ig4)
  END do
  PRINT*
  PRINT*
  PRINT*,"__________________________________________________________"
end