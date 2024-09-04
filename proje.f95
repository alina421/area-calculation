!---------------------------------------------------
! ALEYNA BALCI 110200201 DONEM PROJESI
!---------------------------------------------------

program proje
implicit none

integer i, j, a, b, int, top
real t, x, y, rg, dg

!------------------------------------------------------------------
! y=x Fonksiyonunun 0-50 araliginda altinda kalan alani hesaplama
!------------------------------------------------------------------

write(*,*)
!----------------------------------------------------
! a onculu - Integral hesabi ile
!----------------------------------------------------
a=50
b=0
int = a**2/2 - b**2/2
print*,"integral hesabi ile bulunan alan: ", int

write(*,*)
!----------------------------------------------------
! b onculu - Kucuk dikdortgenlere bolerek
!----------------------------------------------------

!---------------
! 5 dikdortgen
!---------------
top= 0
do i= 10,40,10
top = top + i
end do
print*,"5  dikdortgen icin alan: ",((50-0)/5)*top

!----------------
! 10 dikdortgen
!----------------
top= 0
do i = 5,45,5
top = top+i
end do
print*,"10 dikdortgen icin alan: ",((50-0)/10)*top

!----------------
! 20 dikdortgen
!----------------
top= 0
t = 50/20
do while (t<50)
top = top+t
t = t+(50/20)
end do
print*,"20 dikdortgen icin alan: ", ((50-0)/20)*top

!-----------------
! 50 dikdortgen
!-----------------
top= 0
do i=1,49,1
top = top+i
end do
print*,"50 dikdortgen icin alan: ",((50-0)/50)*top

write(*,*)
!----------------------------------------------------
! c onculu - Rastgele koordinatlar
!----------------------------------------------------

!--------------------------
! Rastgele 50 farkli 
!--------------------------
j= 0
call random_seed()   ! rastegele sayi secme komutu
do i=1,50 ! [1,50]
!-----------------------------------------------------------------
![A, B] araliginda rastgele reel sayi secme formulu: A + (B-A)*R
!-----------------------------------------------------------------
call random_number(x)
rg = 1.0 + 49.0*x
call random_number(y)
dg = 1.0 + 49.0*y
if (rg.gt.dg) then
j= j+1
endif
enddo

print*,"50   farkli rastgele koordinat icin alan: ", 2500*(j/50.0)

!--------------------------
! Rastgele 250 farkli
!--------------------------
j= 0
call random_seed()
do i=1,250
call random_number(x)
rg = 1.0+49.0*x
call random_number(y)
dg = 1.0+49.0*y
if (rg.gt.dg) then
j=j+1
endif
enddo
print*,"250  farkli rastgele koordinat icin alan: ", 2500*(j/250.0)

!--------------------------
! Rastgele 500 farkli
!--------------------------
j= 0
call random_seed()
do i=1,500
call random_number(x)
rg = 1.0+49.0*x
call random_number(y)
dg = 1.0+49.0*y
if (rg.gt.dg) then
j= j+1
endif
enddo
print*,"500  farkli rastgele koordinat icin alan: ", 2500*(j/500.0)

!--------------------------
! Rastgele 1000 farkli
!--------------------------
j= 0
call random_seed()
do i=1,1000
call random_number(x)
rg = 1.0+49.0*x
call random_number(y)
dg = 1.0+49.0*y
if (rg.gt.dg) then
j= j+1
endif
enddo
print*,"1000 farkli rastgele koordinat icin alan: ", 2500*(j/1000.0)
end program 
 
