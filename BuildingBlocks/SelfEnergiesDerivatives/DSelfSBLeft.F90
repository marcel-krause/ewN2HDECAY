double complex function DSelfSBLeft(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(9)

 amplitudes(1) = (0.03125D0*CKM12*CKMC13*((0.5D0*EL2*MU2*SB2*B0(x, MU2, MW2))/(MW2*SW2) - (0.5D0*EL2*MU2*SB2*DB0(x, MU2, MW2))/SW&
  &2 + (0.5D0*EL2*MU2*SB2*x*DB0(x, MU2, MW2))/(MW2*SW2) + (0.5D0*EL2*SB2*DB0(x, MU2, MW2)*DBLE(MU**INT(4.D0)))/(MW2*SW2)))/(PI2*S&
  &B2*x) - (0.03125D0*CKM12*CKMC13*((-0.5D0*EL2*MU2*SB2*A0(MU2))/(MW2*SW2) + (0.5D0*EL2*MU2*SB2*A0(MW2))/(MW2*SW2) - (0.5D0*EL2*M&
  &U2*SB2*B0(x, MU2, MW2))/SW2 + (0.5D0*EL2*MU2*SB2*x*B0(x, MU2, MW2))/(MW2*SW2) + (0.5D0*EL2*SB2*B0(x, MU2, MW2)*DBLE(MU**INT(4.&
  &D0)))/(MW2*SW2))*DBLE(x**INT(-2.D0)))/(PI2*SB2)

 amplitudes(2) = (0.03125D0*CKM22*CKMC23*((0.5D0*EL2*MC2*SB2*B0(x, MC2, MW2))/(MW2*SW2) - (0.5D0*EL2*MC2*SB2*DB0(x, MC2, MW2))/SW&
  &2 + (0.5D0*EL2*MC2*SB2*x*DB0(x, MC2, MW2))/(MW2*SW2) + (0.5D0*EL2*SB2*DB0(x, MC2, MW2)*DBLE(MC**INT(4.D0)))/(MW2*SW2)))/(PI2*S&
  &B2*x) - (0.03125D0*CKM22*CKMC23*((-0.5D0*EL2*MC2*SB2*A0(MC2))/(MW2*SW2) + (0.5D0*EL2*MC2*SB2*A0(MW2))/(MW2*SW2) - (0.5D0*EL2*M&
  &C2*SB2*B0(x, MC2, MW2))/SW2 + (0.5D0*EL2*MC2*SB2*x*B0(x, MC2, MW2))/(MW2*SW2) + (0.5D0*EL2*SB2*B0(x, MC2, MW2)*DBLE(MC**INT(4.&
  &D0)))/(MW2*SW2))*DBLE(x**INT(-2.D0)))/(PI2*SB2)

 amplitudes(3) = (0.03125D0*CKM32*CKMC33*((0.5D0*EL2*MT2*SB2*B0(x, MT2, MW2))/(MW2*SW2) - (0.5D0*EL2*MT2*SB2*DB0(x, MT2, MW2))/SW&
  &2 + (0.5D0*EL2*MT2*SB2*x*DB0(x, MT2, MW2))/(MW2*SW2) + (0.5D0*EL2*SB2*DB0(x, MT2, MW2)*DBLE(MT**INT(4.D0)))/(MW2*SW2)))/(PI2*S&
  &B2*x) - (0.03125D0*CKM32*CKMC33*((-0.5D0*EL2*MT2*SB2*A0(MT2))/(MW2*SW2) + (0.5D0*EL2*MT2*SB2*A0(MW2))/(MW2*SW2) - (0.5D0*EL2*M&
  &T2*SB2*B0(x, MT2, MW2))/SW2 + (0.5D0*EL2*MT2*SB2*x*B0(x, MT2, MW2))/(MW2*SW2) + (0.5D0*EL2*SB2*B0(x, MT2, MW2)*DBLE(MT**INT(4.&
  &D0)))/(MW2*SW2))*DBLE(x**INT(-2.D0)))/(PI2*SB2)

 amplitudes(4) = (0.03125D0*CKM12*CKMC13*((0.5D0*CB2*EL2*MU2*B0(x, MHp2, MU2))/(MW2*SW2) - (0.5D0*CB2*EL2*MHp2*MU2*DB0(x, MHp2, M&
  &U2))/(MW2*SW2) + (0.5D0*CB2*EL2*MU2*x*DB0(x, MHp2, MU2))/(MW2*SW2) + (0.5D0*CB2*EL2*DB0(x, MHp2, MU2)*DBLE(MU**INT(4.D0)))/(MW&
  &2*SW2)))/(PI2*SB2*x) - (0.03125D0*CKM12*CKMC13*((0.5D0*CB2*EL2*MU2*A0(MHp2))/(MW2*SW2) - (0.5D0*CB2*EL2*MU2*A0(MU2))/(MW2*SW2)&
  & - (0.5D0*CB2*EL2*MHp2*MU2*B0(x, MHp2, MU2))/(MW2*SW2) + (0.5D0*CB2*EL2*MU2*x*B0(x, MHp2, MU2))/(MW2*SW2) + (0.5D0*CB2*EL2*B0(&
  &x, MHp2, MU2)*DBLE(MU**INT(4.D0)))/(MW2*SW2))*DBLE(x**INT(-2.D0)))/(PI2*SB2)

 amplitudes(5) = (0.03125D0*CKM22*CKMC23*((0.5D0*CB2*EL2*MC2*B0(x, MC2, MHp2))/(MW2*SW2) - (0.5D0*CB2*EL2*MC2*MHp2*DB0(x, MC2, MH&
  &p2))/(MW2*SW2) + (0.5D0*CB2*EL2*MC2*x*DB0(x, MC2, MHp2))/(MW2*SW2) + (0.5D0*CB2*EL2*DB0(x, MC2, MHp2)*DBLE(MC**INT(4.D0)))/(MW&
  &2*SW2)))/(PI2*SB2*x) - (0.03125D0*CKM22*CKMC23*((-0.5D0*CB2*EL2*MC2*A0(MC2))/(MW2*SW2) + (0.5D0*CB2*EL2*MC2*A0(MHp2))/(MW2*SW2&
  &) - (0.5D0*CB2*EL2*MC2*MHp2*B0(x, MC2, MHp2))/(MW2*SW2) + (0.5D0*CB2*EL2*MC2*x*B0(x, MC2, MHp2))/(MW2*SW2) + (0.5D0*CB2*EL2*B0&
  &(x, MC2, MHp2)*DBLE(MC**INT(4.D0)))/(MW2*SW2))*DBLE(x**INT(-2.D0)))/(PI2*SB2)

 amplitudes(6) = (0.03125D0*CKM32*CKMC33*((0.5D0*CB2*EL2*MT2*B0(x, MHp2, MT2))/(MW2*SW2) - (0.5D0*CB2*EL2*MHp2*MT2*DB0(x, MHp2, M&
  &T2))/(MW2*SW2) + (0.5D0*CB2*EL2*MT2*x*DB0(x, MHp2, MT2))/(MW2*SW2) + (0.5D0*CB2*EL2*DB0(x, MHp2, MT2)*DBLE(MT**INT(4.D0)))/(MW&
  &2*SW2)))/(PI2*SB2*x) - (0.03125D0*CKM32*CKMC33*((0.5D0*CB2*EL2*MT2*A0(MHp2))/(MW2*SW2) - (0.5D0*CB2*EL2*MT2*A0(MT2))/(MW2*SW2)&
  & - (0.5D0*CB2*EL2*MHp2*MT2*B0(x, MHp2, MT2))/(MW2*SW2) + (0.5D0*CB2*EL2*MT2*x*B0(x, MHp2, MT2))/(MW2*SW2) + (0.5D0*CB2*EL2*B0(&
  &x, MHp2, MT2)*DBLE(MT**INT(4.D0)))/(MW2*SW2))*DBLE(x**INT(-2.D0)))/(PI2*SB2)

 amplitudes(7) = (0.03125D0*CKM12*CKMC13*EL2*(-1.D0 + B0(x, MU2, MW2) + MU2*DB0(x, MU2, MW2) - 1.D0*MW2*DB0(x, MU2, MW2) + x*DB0(&
  &x, MU2, MW2)))/(PI2*SW2*x) - (0.03125D0*CKM12*CKMC13*EL2*(-1.D0*x - 1.D0*A0(MU2) + A0(MW2) + MU2*B0(x, MU2, MW2) - 1.D0*MW2*B0&
  &(x, MU2, MW2) + x*B0(x, MU2, MW2))* DBLE(x**INT(-2.D0)))/(PI2*SW2)

 amplitudes(8) = (0.03125D0*CKM22*CKMC23*EL2*(-1.D0 + B0(x, MC2, MW2) + MC2*DB0(x, MC2, MW2) - 1.D0*MW2*DB0(x, MC2, MW2) + x*DB0(&
  &x, MC2, MW2)))/(PI2*SW2*x) - (0.03125D0*CKM22*CKMC23*EL2*(-1.D0*x - 1.D0*A0(MC2) + A0(MW2) + MC2*B0(x, MC2, MW2) - 1.D0*MW2*B0&
  &(x, MC2, MW2) + x*B0(x, MC2, MW2))* DBLE(x**INT(-2.D0)))/(PI2*SW2)

 amplitudes(9) = (0.03125D0*CKM32*CKMC33*EL2*(-1.D0 + B0(x, MT2, MW2) + MT2*DB0(x, MT2, MW2) - 1.D0*MW2*DB0(x, MT2, MW2) + x*DB0(&
  &x, MT2, MW2)))/(PI2*SW2*x) - (0.03125D0*CKM32*CKMC33*EL2*(-1.D0*x - 1.D0*A0(MT2) + A0(MW2) + MT2*B0(x, MT2, MW2) - 1.D0*MW2*B0&
  &(x, MT2, MW2) + x*B0(x, MT2, MW2))* DBLE(x**INT(-2.D0)))/(PI2*SW2)

  totalAmplitude = (0D0,0D0)
 do j=1,9
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 DSelfSBLeft = totalAmplitude
end function DSelfSBLeft

