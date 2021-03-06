double complex function DSelfUTRight(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(9)

 amplitudes(1) = (0.03125D0*CKM31*CKMC11*((0.5D0*EL2*MT*MU*SB2*B0(x, MD2, MW2))/(MW2*SW2) - (0.5D0*EL2*MT*MU*SB2*DB0(x, MD2, MW2)&
  &)/SW2 + (0.5D0*EL2*MD2*MT*MU*SB2*DB0(x, MD2, MW2))/(MW2*SW2) + (0.5D0*EL2*MT*MU*SB2*x*DB0(x, MD2, MW2))/(MW2*SW2)))/(PI2*SB2*x&
  &) - (0.03125D0*CKM31*CKMC11*((-0.5D0*EL2*MT*MU*SB2*A0(MD2))/(MW2*SW2) + (0.5D0*EL2*MT*MU*SB2*A0(MW2))/(MW2*SW2) - (0.5D0*EL2*M&
  &T*MU*SB2*B0(x, MD2, MW2))/SW2 + (0.5D0*EL2*MD2*MT*MU*SB2*B0(x, MD2, MW2))/(MW2*SW2) + (0.5D0*EL2*MT*MU*SB2*x*B0(x, MD2, MW2))/&
  &(MW2*SW2))*DBLE(x**INT(-2.D0)))/(PI2*SB2)

 amplitudes(2) = (0.03125D0*CKM32*CKMC12*((0.5D0*EL2*MT*MU*SB2*B0(x, MS2, MW2))/(MW2*SW2) - (0.5D0*EL2*MT*MU*SB2*DB0(x, MS2, MW2)&
  &)/SW2 + (0.5D0*EL2*MS2*MT*MU*SB2*DB0(x, MS2, MW2))/(MW2*SW2) + (0.5D0*EL2*MT*MU*SB2*x*DB0(x, MS2, MW2))/(MW2*SW2)))/(PI2*SB2*x&
  &) - (0.03125D0*CKM32*CKMC12*((-0.5D0*EL2*MT*MU*SB2*A0(MS2))/(MW2*SW2) + (0.5D0*EL2*MT*MU*SB2*A0(MW2))/(MW2*SW2) - (0.5D0*EL2*M&
  &T*MU*SB2*B0(x, MS2, MW2))/SW2 + (0.5D0*EL2*MS2*MT*MU*SB2*B0(x, MS2, MW2))/(MW2*SW2) + (0.5D0*EL2*MT*MU*SB2*x*B0(x, MS2, MW2))/&
  &(MW2*SW2))*DBLE(x**INT(-2.D0)))/(PI2*SB2)

 amplitudes(3) = (0.03125D0*CKM33*CKMC13*((0.5D0*EL2*MT*MU*SB2*B0(x, MB2, MW2))/(MW2*SW2) - (0.5D0*EL2*MT*MU*SB2*DB0(x, MB2, MW2)&
  &)/SW2 + (0.5D0*EL2*MB2*MT*MU*SB2*DB0(x, MB2, MW2))/(MW2*SW2) + (0.5D0*EL2*MT*MU*SB2*x*DB0(x, MB2, MW2))/(MW2*SW2)))/(PI2*SB2*x&
  &) - (0.03125D0*CKM33*CKMC13*((-0.5D0*EL2*MT*MU*SB2*A0(MB2))/(MW2*SW2) + (0.5D0*EL2*MT*MU*SB2*A0(MW2))/(MW2*SW2) - (0.5D0*EL2*M&
  &T*MU*SB2*B0(x, MB2, MW2))/SW2 + (0.5D0*EL2*MB2*MT*MU*SB2*B0(x, MB2, MW2))/(MW2*SW2) + (0.5D0*EL2*MT*MU*SB2*x*B0(x, MB2, MW2))/&
  &(MW2*SW2))*DBLE(x**INT(-2.D0)))/(PI2*SB2)

 amplitudes(4) = (0.03125D0*CKM31*CKMC11*((0.5D0*CB2*EL2*MT*MU*B0(x, MD2, MHp2))/(MW2*SW2) + (0.5D0*CB2*EL2*MD2*MT*MU*DB0(x, MD2,&
  & MHp2))/(MW2*SW2) - (0.5D0*CB2*EL2*MHp2*MT*MU*DB0(x, MD2, MHp2))/(MW2*SW2) + (0.5D0*CB2*EL2*MT*MU*x*DB0(x, MD2, MHp2))/(MW2*SW&
  &2)))/(PI2*SB2*x) - (0.03125D0*CKM31*CKMC11*((-0.5D0*CB2*EL2*MT*MU*A0(MD2))/(MW2*SW2) + (0.5D0*CB2*EL2*MT*MU*A0(MHp2))/(MW2*SW2&
  &) + (0.5D0*CB2*EL2*MD2*MT*MU*B0(x, MD2, MHp2))/(MW2*SW2) - (0.5D0*CB2*EL2*MHp2*MT*MU*B0(x, MD2, MHp2))/(MW2*SW2) + (0.5D0*CB2*&
  &EL2*MT*MU*x*B0(x, MD2, MHp2))/(MW2*SW2))*DBLE(x**INT(-2.D0)))/(PI2*SB2)

 amplitudes(5) = (0.03125D0*CKM32*CKMC12*((0.5D0*CB2*EL2*MT*MU*B0(x, MHp2, MS2))/(MW2*SW2) - (0.5D0*CB2*EL2*MHp2*MT*MU*DB0(x, MHp&
  &2, MS2))/(MW2*SW2) + (0.5D0*CB2*EL2*MS2*MT*MU*DB0(x, MHp2, MS2))/(MW2*SW2) + (0.5D0*CB2*EL2*MT*MU*x*DB0(x, MHp2, MS2))/(MW2*SW&
  &2)))/(PI2*SB2*x) - (0.03125D0*CKM32*CKMC12*((0.5D0*CB2*EL2*MT*MU*A0(MHp2))/(MW2*SW2) - (0.5D0*CB2*EL2*MT*MU*A0(MS2))/(MW2*SW2)&
  & - (0.5D0*CB2*EL2*MHp2*MT*MU*B0(x, MHp2, MS2))/(MW2*SW2) + (0.5D0*CB2*EL2*MS2*MT*MU*B0(x, MHp2, MS2))/(MW2*SW2) + (0.5D0*CB2*E&
  &L2*MT*MU*x*B0(x, MHp2, MS2))/(MW2*SW2))*DBLE(x**INT(-2.D0)))/(PI2*SB2)

 amplitudes(6) = (0.03125D0*CKM33*CKMC13*((0.5D0*CB2*EL2*MT*MU*B0(x, MB2, MHp2))/(MW2*SW2) + (0.5D0*CB2*EL2*MB2*MT*MU*DB0(x, MB2,&
  & MHp2))/(MW2*SW2) - (0.5D0*CB2*EL2*MHp2*MT*MU*DB0(x, MB2, MHp2))/(MW2*SW2) + (0.5D0*CB2*EL2*MT*MU*x*DB0(x, MB2, MHp2))/(MW2*SW&
  &2)))/(PI2*SB2*x) - (0.03125D0*CKM33*CKMC13*((-0.5D0*CB2*EL2*MT*MU*A0(MB2))/(MW2*SW2) + (0.5D0*CB2*EL2*MT*MU*A0(MHp2))/(MW2*SW2&
  &) + (0.5D0*CB2*EL2*MB2*MT*MU*B0(x, MB2, MHp2))/(MW2*SW2) - (0.5D0*CB2*EL2*MHp2*MT*MU*B0(x, MB2, MHp2))/(MW2*SW2) + (0.5D0*CB2*&
  &EL2*MT*MU*x*B0(x, MB2, MHp2))/(MW2*SW2))*DBLE(x**INT(-2.D0)))/(PI2*SB2)

 amplitudes(7) = 0.D0

 amplitudes(8) = 0.D0

 amplitudes(9) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,9
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 DSelfUTRight = totalAmplitude
end function DSelfUTRight

