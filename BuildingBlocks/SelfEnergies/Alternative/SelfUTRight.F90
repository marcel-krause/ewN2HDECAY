double complex function SelfUTRightAlter(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(9)

 amplitudes(1) = (0.03125D0*CKM31*CKMC11*((-0.5D0*EL2*MT*MU*SB2*A0(MD2))/(MW2*SW2) + (0.5D0*EL2*MT*MU*SB2*A0(MW2))/(MW2*SW2) - (0&
  &.5D0*EL2*MT*MU*SB2*B0(x, MD2, MW2))/SW2 + (0.5D0*EL2*MD2*MT*MU*SB2*B0(x, MD2, MW2))/(MW2*SW2) + (0.5D0*EL2*MT*MU*SB2*x*B0(x, M&
  &D2, MW2))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(2) = (0.03125D0*CKM32*CKMC12*((-0.5D0*EL2*MT*MU*SB2*A0(MS2))/(MW2*SW2) + (0.5D0*EL2*MT*MU*SB2*A0(MW2))/(MW2*SW2) - (0&
  &.5D0*EL2*MT*MU*SB2*B0(x, MS2, MW2))/SW2 + (0.5D0*EL2*MS2*MT*MU*SB2*B0(x, MS2, MW2))/(MW2*SW2) + (0.5D0*EL2*MT*MU*SB2*x*B0(x, M&
  &S2, MW2))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(3) = (0.03125D0*CKM33*CKMC13*((-0.5D0*EL2*MT*MU*SB2*A0(MB2))/(MW2*SW2) + (0.5D0*EL2*MT*MU*SB2*A0(MW2))/(MW2*SW2) - (0&
  &.5D0*EL2*MT*MU*SB2*B0(x, MB2, MW2))/SW2 + (0.5D0*EL2*MB2*MT*MU*SB2*B0(x, MB2, MW2))/(MW2*SW2) + (0.5D0*EL2*MT*MU*SB2*x*B0(x, M&
  &B2, MW2))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(4) = (0.03125D0*CKM31*CKMC11*((-0.5D0*CB2*EL2*MT*MU*A0(MD2))/(MW2*SW2) + (0.5D0*CB2*EL2*MT*MU*A0(MHp2))/(MW2*SW2) + (&
  &0.5D0*CB2*EL2*MD2*MT*MU*B0(x, MD2, MHp2))/(MW2*SW2) - (0.5D0*CB2*EL2*MHp2*MT*MU*B0(x, MD2, MHp2))/(MW2*SW2) + (0.5D0*CB2*EL2*M&
  &T*MU*x*B0(x, MD2, MHp2))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(5) = (0.03125D0*CKM32*CKMC12*((0.5D0*CB2*EL2*MT*MU*A0(MHp2))/(MW2*SW2) - (0.5D0*CB2*EL2*MT*MU*A0(MS2))/(MW2*SW2) - (0&
  &.5D0*CB2*EL2*MHp2*MT*MU*B0(x, MHp2, MS2))/(MW2*SW2) + (0.5D0*CB2*EL2*MS2*MT*MU*B0(x, MHp2, MS2))/(MW2*SW2) + (0.5D0*CB2*EL2*MT&
  &*MU*x*B0(x, MHp2, MS2))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(6) = (0.03125D0*CKM33*CKMC13*((-0.5D0*CB2*EL2*MT*MU*A0(MB2))/(MW2*SW2) + (0.5D0*CB2*EL2*MT*MU*A0(MHp2))/(MW2*SW2) + (&
  &0.5D0*CB2*EL2*MB2*MT*MU*B0(x, MB2, MHp2))/(MW2*SW2) - (0.5D0*CB2*EL2*MHp2*MT*MU*B0(x, MB2, MHp2))/(MW2*SW2) + (0.5D0*CB2*EL2*M&
  &T*MU*x*B0(x, MB2, MHp2))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(7) = 0.D0

 amplitudes(8) = 0.D0

 amplitudes(9) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,9
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfUTRightAlter = totalAmplitude
end function SelfUTRightAlter

