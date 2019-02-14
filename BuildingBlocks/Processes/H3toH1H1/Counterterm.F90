double precision function H3toH1H1CT(x)
 use constants
 use counterterms
 implicit none
#include "looptools.h"
 integer, intent(in) :: x
 double precision :: totalAmplitude
 double precision :: dRR11, dRR12, dRR13, dRR21, dRR22, dRR23, dRR31, dRR32, dRR33

 select case (x)
	case (1)
		dRR11 = -CA2*SA1*dAlpha1KanUsual() - CA1*SA2*dAlpha2KanUsual()
		dRR12 = CA1*CA2*dAlpha1KanUsual() - SA1*SA2*dAlpha2KanUsual()
		dRR13 = CA2*dAlpha2KanUsual()
		dRR21 = -CA1*CA3*dAlpha1KanUsual() - CA1*CA3*SA2*dAlpha3KanUsual() - CA1*CA2*SA3*dAlpha2KanUsual() + &
			& SA1*SA3*dAlpha3KanUsual() + SA1*SA2*SA3*dAlpha1KanUsual()
		dRR22 = -CA3*SA1*dAlpha1KanUsual() - CA3*SA1*SA2*dAlpha3KanUsual() - CA1*SA3*dAlpha3KanUsual() - &
			& CA2*SA1*SA3*dAlpha2KanUsual() - CA1*SA2*SA3*dAlpha1KanUsual()
		dRR23 = CA2*CA3*dAlpha3KanUsual() - SA2*SA3*dAlpha2KanUsual()
		dRR31 = -CA1*CA2*CA3*dAlpha2KanUsual() + CA3*SA1*dAlpha3KanUsual() + CA3*SA1*SA2*dAlpha1KanUsual() + &
			& CA1*SA3*dAlpha1KanUsual() + CA1*SA2*SA3*dAlpha3KanUsual()
		dRR32 = -CA1*CA3*dAlpha3KanUsual() - CA2*CA3*SA1*dAlpha2KanUsual() - CA1*CA3*SA2*dAlpha1KanUsual() + &
			& SA1*SA3*dAlpha1KanUsual() + SA1*SA2*SA3*dAlpha3KanUsual()
		dRR33 = -CA3*SA2*dAlpha2KanUsual() - CA2*SA3*dAlpha3KanUsual()

		totalAmplitude = CS1S1S1f311*( &
&(0.28125D0*CA1*CA3*EL*m12squared*SA2*dAlpha1KanUsual())/(CB*MW*SW) + (0.84375D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha1KanUsual())&
  &/(CB*MW*SW) - (0.1875D0*CA3*EL*MH12*SA1*SA2*dAlpha1KanUsual())/(CB*MW*SW) - (0.5625D0*CA12*CA3*EL*MH12*SA1*SA2*dAlpha1KanUsual&
  &())/(CB*MW*SW) - (0.5625D0*CA22*CA3*EL*MH12*SA1*SA2*dAlpha1KanUsual())/(CB*MW*SW) - (1.6875D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dA&
  &lpha1KanUsual())/(CB*MW*SW) - (0.09375D0*CA3*EL*MH32*SA1*SA2*dAlpha1KanUsual())/(CB*MW*SW) - (0.28125D0*CA12*CA3*EL*MH32*SA1*S&
  &A2*dAlpha1KanUsual())/(CB*MW*SW) - (0.28125D0*CA22*CA3*EL*MH32*SA1*SA2*dAlpha1KanUsual())/(CB*MW*SW) - (0.84375D0*CA12*CA22*CA&
  &3*EL*MH32*SA1*SA2*dAlpha1KanUsual())/(CB*MW*SW) - (0.125D0*CA1*EL*MH12*SA3*dAlpha1KanUsual())/(CB*MW*SW) - (0.125D0*CA1*CA22*E&
  &L*MH12*SA3*dAlpha1KanUsual())/(CB*MW*SW) - (0.0625D0*CA1*EL*MH32*SA3*dAlpha1KanUsual())/(CB*MW*SW) - (0.0625D0*CA1*CA22*EL*MH3&
  &2*SA3*dAlpha1KanUsual())/(CB*MW*SW) - (0.1875D0*EL*m12squared*SA1*SA3*dAlpha1KanUsual())/(CB*MW*SW) - (0.1875D0*CA22*EL*m12squ&
  &ared*SA1*SA3*dAlpha1KanUsual())/(CB*MW*SW) + (1.125D0*CA1*EL*MH12*SA12*SA3*dAlpha1KanUsual())/(CB*MW*SW) + (1.125D0*CA1*CA22*E&
  &L*MH12*SA12*SA3*dAlpha1KanUsual())/(CB*MW*SW) + (0.5625D0*CA1*EL*MH32*SA12*SA3*dAlpha1KanUsual())/(CB*MW*SW) + (0.5625D0*CA1*C&
  &A22*EL*MH32*SA12*SA3*dAlpha1KanUsual())/(CB*MW*SW) + (0.125D0*CA1*EL*MH12*SA22*SA3*dAlpha1KanUsual())/(CB*MW*SW) + (0.0625D0*C&
  &A1*EL*MH32*SA22*SA3*dAlpha1KanUsual())/(CB*MW*SW) + (0.1875D0*EL*m12squared*SA1*SA22*SA3*dAlpha1KanUsual())/(CB*MW*SW) - (1.12&
  &5D0*CA1*EL*MH12*SA12*SA22*SA3*dAlpha1KanUsual())/(CB*MW*SW) - (0.5625D0*CA1*EL*MH32*SA12*SA22*SA3*dAlpha1KanUsual())/(CB*MW*SW&
  &) + (0.1875D0*CA1*CA3*EL*MH12*SA2*dAlpha1KanUsual())/(MW*SB*SW) + (0.5625D0*CA1*CA22*CA3*EL*MH12*SA2*dAlpha1KanUsual())/(MW*SB&
  &*SW) + (0.09375D0*CA1*CA3*EL*MH32*SA2*dAlpha1KanUsual())/(MW*SB*SW) + (0.28125D0*CA1*CA22*CA3*EL*MH32*SA2*dAlpha1KanUsual())/(&
  &MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*SA1*SA2*dAlpha1KanUsual())/(MW*SB*SW) - (0.84375D0*CA22*CA3*EL*m12squared*SA1*SA2*dAl&
  &pha1KanUsual())/(MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA1*SA2*dAlpha1KanUsual())/(CB2*MW*SB*SW) + (0.84375D0*CA12*CA3*EL*m1&
  &2squared*SA1*SA2*dAlpha1KanUsual())/(CB2*MW*SB*SW) + (0.5625D0*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha1KanUsual())/(CB2*MW*SB*SW&
  &) + (2.53125D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha1KanUsual())/(CB2*MW*SB*SW) + (0.5625D0*CA1*CA3*EL*MH12*SA12*SA2*dAlp&
  &ha1KanUsual())/(MW*SB*SW) + (1.6875D0*CA1*CA22*CA3*EL*MH12*SA12*SA2*dAlpha1KanUsual())/(MW*SB*SW) + (0.28125D0*CA1*CA3*EL*MH32&
  &*SA12*SA2*dAlpha1KanUsual())/(MW*SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*MH32*SA12*SA2*dAlpha1KanUsual())/(MW*SB*SW) - (0.1875D0*C&
  &A1*EL*m12squared*SA3*dAlpha1KanUsual())/(MW*SB*SW) - (0.1875D0*CA1*CA22*EL*m12squared*SA3*dAlpha1KanUsual())/(MW*SB*SW) + (0.1&
  &25D0*CA1*EL*m12squared*SA3*dAlpha1KanUsual())/(CB2*MW*SB*SW) + (0.125D0*CA1*CA22*EL*m12squared*SA3*dAlpha1KanUsual())/(CB2*MW*&
  &SB*SW) - (0.125D0*EL*MH12*SA1*SA3*dAlpha1KanUsual())/(MW*SB*SW) + (1.125D0*CA12*EL*MH12*SA1*SA3*dAlpha1KanUsual())/(MW*SB*SW) &
  &- (0.125D0*CA22*EL*MH12*SA1*SA3*dAlpha1KanUsual())/(MW*SB*SW) + (1.125D0*CA12*CA22*EL*MH12*SA1*SA3*dAlpha1KanUsual())/(MW*SB*S&
  &W) - (0.0625D0*EL*MH32*SA1*SA3*dAlpha1KanUsual())/(MW*SB*SW) + (0.5625D0*CA12*EL*MH32*SA1*SA3*dAlpha1KanUsual())/(MW*SB*SW) - &
  &(0.0625D0*CA22*EL*MH32*SA1*SA3*dAlpha1KanUsual())/(MW*SB*SW) + (0.5625D0*CA12*CA22*EL*MH32*SA1*SA3*dAlpha1KanUsual())/(MW*SB*S&
  &W) - (1.6875D0*CA1*EL*m12squared*SA12*SA3*dAlpha1KanUsual())/(CB2*MW*SB*SW) - (1.6875D0*CA1*CA22*EL*m12squared*SA12*SA3*dAlpha&
  &1KanUsual())/(CB2*MW*SB*SW) + (0.1875D0*CA1*EL*m12squared*SA22*SA3*dAlpha1KanUsual())/(MW*SB*SW) - (0.125D0*CA1*EL*m12squared*&
  &SA22*SA3*dAlpha1KanUsual())/(CB2*MW*SB*SW) + (0.125D0*EL*MH12*SA1*SA22*SA3*dAlpha1KanUsual())/(MW*SB*SW) - (1.125D0*CA12*EL*MH&
  &12*SA1*SA22*SA3*dAlpha1KanUsual())/(MW*SB*SW) + (0.0625D0*EL*MH32*SA1*SA22*SA3*dAlpha1KanUsual())/(MW*SB*SW) - (0.5625D0*CA12*&
  &EL*MH32*SA1*SA22*SA3*dAlpha1KanUsual())/(MW*SB*SW) + (1.6875D0*CA1*EL*m12squared*SA12*SA22*SA3*dAlpha1KanUsual())/(CB2*MW*SB*S&
  &W) - (0.1875D0*CA1*CA3*EL*m12squared*SA2*dAlpha1KanUsual())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha1K&
  &anUsual())/(CB*MW*SB2*SW) - (0.84375D0*CA1*CA3*EL*m12squared*SA12*SA2*dAlpha1KanUsual())/(CB*MW*SB2*SW) - (2.53125D0*CA1*CA22*&
  &CA3*EL*m12squared*SA12*SA2*dAlpha1KanUsual())/(CB*MW*SB2*SW) + (0.125D0*EL*m12squared*SA1*SA3*dAlpha1KanUsual())/(CB*MW*SB2*SW&
  &) - (1.6875D0*CA12*EL*m12squared*SA1*SA3*dAlpha1KanUsual())/(CB*MW*SB2*SW) + (0.125D0*CA22*EL*m12squared*SA1*SA3*dAlpha1KanUsu&
  &al())/(CB*MW*SB2*SW) - (1.6875D0*CA12*CA22*EL*m12squared*SA1*SA3*dAlpha1KanUsual())/(CB*MW*SB2*SW) - (0.125D0*EL*m12squared*SA&
  &1*SA22*SA3*dAlpha1KanUsual())/(CB*MW*SB2*SW) + (1.6875D0*CA12*EL*m12squared*SA1*SA22*SA3*dAlpha1KanUsual())/(CB*MW*SB2*SW) - (&
  &0.09375D0*CA1*CA3*EL*m12squared*SA2*dAlpha1KanUsual())/(MW*SB*SW*TB) - (0.28125D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha1KanUsu&
  &al())/(MW*SB*SW*TB) + (0.0625D0*EL*m12squared*SA1*SA3*dAlpha1KanUsual())/(MW*SB*SW*TB) + (0.0625D0*CA22*EL*m12squared*SA1*SA3*&
  &dAlpha1KanUsual())/(MW*SB*SW*TB) - (0.0625D0*EL*m12squared*SA1*SA22*SA3*dAlpha1KanUsual())/(MW*SB*SW*TB) + (0.09375D0*CA3*EL*m&
  &12squared*SA1*SA2*TB*dAlpha1KanUsual())/(CB*MW*SW) + (0.28125D0*CA22*CA3*EL*m12squared*SA1*SA2*TB*dAlpha1KanUsual())/(CB*MW*SW&
  &) + (0.0625D0*CA1*EL*m12squared*SA3*TB*dAlpha1KanUsual())/(CB*MW*SW) + (0.0625D0*CA1*CA22*EL*m12squared*SA3*TB*dAlpha1KanUsual&
  &())/(CB*MW*SW) - (0.0625D0*CA1*EL*m12squared*SA22*SA3*TB*dAlpha1KanUsual())/(CB*MW*SW) + (0.1875D0*CA1*CA2*CA3*EL*MH12*dAlpha2&
  &KanUsual())/(CB*MW*SW) + (0.09375D0*CA1*CA2*CA3*EL*MH32*dAlpha2KanUsual())/(CB*MW*SW) + (0.28125D0*CA2*CA3*EL*m12squared*SA1*d&
  &Alpha2KanUsual())/(CB*MW*SW) - (0.1875D0*CA1*CA2*CA3*EL*MH12*SA12*dAlpha2KanUsual())/(CB*MW*SW) - (0.09375D0*CA1*CA2*CA3*EL*MH&
  &32*SA12*dAlpha2KanUsual())/(CB*MW*SW) - (1.6875D0*CA1*CA2*CA3*EL*MH12*SA22*dAlpha2KanUsual())/(CB*MW*SW) - (0.84375D0*CA1*CA2*&
  &CA3*EL*MH32*SA22*dAlpha2KanUsual())/(CB*MW*SW) - (2.53125D0*CA2*CA3*EL*m12squared*SA1*SA22*dAlpha2KanUsual())/(CB*MW*SW) + (1.&
  &6875D0*CA1*CA2*CA3*EL*MH12*SA12*SA22*dAlpha2KanUsual())/(CB*MW*SW) + (0.84375D0*CA1*CA2*CA3*EL*MH32*SA12*SA22*dAlpha2KanUsual(&
  &))/(CB*MW*SW) - (0.75D0*CA1*CA2*EL*m12squared*SA2*SA3*dAlpha2KanUsual())/(CB*MW*SW) + (0.5D0*CA2*EL*MH12*SA1*SA2*SA3*dAlpha2Ka&
  &nUsual())/(CB*MW*SW) + (1.5D0*CA12*CA2*EL*MH12*SA1*SA2*SA3*dAlpha2KanUsual())/(CB*MW*SW) + (0.25D0*CA2*EL*MH32*SA1*SA2*SA3*dAl&
  &pha2KanUsual())/(CB*MW*SW) + (0.75D0*CA12*CA2*EL*MH32*SA1*SA2*SA3*dAlpha2KanUsual())/(CB*MW*SW) + (0.28125D0*CA1*CA2*CA3*EL*m1&
  &2squared*dAlpha2KanUsual())/(MW*SB*SW) - (0.1875D0*CA1*CA2*CA3*EL*m12squared*dAlpha2KanUsual())/(CB2*MW*SB*SW) + (0.1875D0*CA2&
  &*CA3*EL*MH12*SA1*dAlpha2KanUsual())/(MW*SB*SW) - (0.1875D0*CA12*CA2*CA3*EL*MH12*SA1*dAlpha2KanUsual())/(MW*SB*SW) + (0.09375D0&
  &*CA2*CA3*EL*MH32*SA1*dAlpha2KanUsual())/(MW*SB*SW) - (0.09375D0*CA12*CA2*CA3*EL*MH32*SA1*dAlpha2KanUsual())/(MW*SB*SW) + (0.28&
  &125D0*CA1*CA2*CA3*EL*m12squared*SA12*dAlpha2KanUsual())/(CB2*MW*SB*SW) - (2.53125D0*CA1*CA2*CA3*EL*m12squared*SA22*dAlpha2KanU&
  &sual())/(MW*SB*SW) + (1.6875D0*CA1*CA2*CA3*EL*m12squared*SA22*dAlpha2KanUsual())/(CB2*MW*SB*SW) - (1.6875D0*CA2*CA3*EL*MH12*SA&
  &1*SA22*dAlpha2KanUsual())/(MW*SB*SW) + (1.6875D0*CA12*CA2*CA3*EL*MH12*SA1*SA22*dAlpha2KanUsual())/(MW*SB*SW) - (0.84375D0*CA2*&
  &CA3*EL*MH32*SA1*SA22*dAlpha2KanUsual())/(MW*SB*SW) + (0.84375D0*CA12*CA2*CA3*EL*MH32*SA1*SA22*dAlpha2KanUsual())/(MW*SB*SW) - &
  &(2.53125D0*CA1*CA2*CA3*EL*m12squared*SA12*SA22*dAlpha2KanUsual())/(CB2*MW*SB*SW) - (0.5D0*CA1*CA2*EL*MH12*SA2*SA3*dAlpha2KanUs&
  &ual())/(MW*SB*SW) - (0.25D0*CA1*CA2*EL*MH32*SA2*SA3*dAlpha2KanUsual())/(MW*SB*SW) + (0.75D0*CA2*EL*m12squared*SA1*SA2*SA3*dAlp&
  &ha2KanUsual())/(MW*SB*SW) - (0.5D0*CA2*EL*m12squared*SA1*SA2*SA3*dAlpha2KanUsual())/(CB2*MW*SB*SW) - (2.25D0*CA12*CA2*EL*m12sq&
  &uared*SA1*SA2*SA3*dAlpha2KanUsual())/(CB2*MW*SB*SW) - (1.5D0*CA1*CA2*EL*MH12*SA12*SA2*SA3*dAlpha2KanUsual())/(MW*SB*SW) - (0.7&
  &5D0*CA1*CA2*EL*MH32*SA12*SA2*SA3*dAlpha2KanUsual())/(MW*SB*SW) - (0.1875D0*CA2*CA3*EL*m12squared*SA1*dAlpha2KanUsual())/(CB*MW&
  &*SB2*SW) + (0.28125D0*CA12*CA2*CA3*EL*m12squared*SA1*dAlpha2KanUsual())/(CB*MW*SB2*SW) + (1.6875D0*CA2*CA3*EL*m12squared*SA1*S&
  &A22*dAlpha2KanUsual())/(CB*MW*SB2*SW) - (2.53125D0*CA12*CA2*CA3*EL*m12squared*SA1*SA22*dAlpha2KanUsual())/(CB*MW*SB2*SW) + (0.&
  &5D0*CA1*CA2*EL*m12squared*SA2*SA3*dAlpha2KanUsual())/(CB*MW*SB2*SW) + (2.25D0*CA1*CA2*EL*m12squared*SA12*SA2*SA3*dAlpha2KanUsu&
  &al())/(CB*MW*SB2*SW) - (0.09375D0*CA2*CA3*EL*m12squared*SA1*dAlpha2KanUsual())/(MW*SB*SW*TB) + (0.84375D0*CA2*CA3*EL*m12square&
  &d*SA1*SA22*dAlpha2KanUsual())/(MW*SB*SW*TB) + (0.25D0*CA1*CA2*EL*m12squared*SA2*SA3*dAlpha2KanUsual())/(MW*SB*SW*TB) - (0.0937&
  &5D0*CA1*CA2*CA3*EL*m12squared*TB*dAlpha2KanUsual())/(CB*MW*SW) + (0.84375D0*CA1*CA2*CA3*EL*m12squared*SA22*TB*dAlpha2KanUsual(&
  &))/(CB*MW*SW) - (0.25D0*CA2*EL*m12squared*SA1*SA2*SA3*TB*dAlpha2KanUsual())/(CB*MW*SW) + (0.5D0*CA3*MH12*SA2*dAlpha2KanUsual()&
  &)/vS - (4.5D0*CA22*CA3*MH12*SA2*dAlpha2KanUsual())/vS + (0.25D0*CA3*MH32*SA2*dAlpha2KanUsual())/vS - (2.25D0*CA22*CA3*MH32*SA2&
  &*dAlpha2KanUsual())/vS + (0.1875D0*CA1*CA3*EL*m12squared*dAlpha3KanUsual())/(CB*MW*SW) + (0.1875D0*CA1*CA22*CA3*EL*m12squared*&
  &dAlpha3KanUsual())/(CB*MW*SW) - (0.125D0*CA3*EL*MH12*SA1*dAlpha3KanUsual())/(CB*MW*SW) - (0.375D0*CA12*CA3*EL*MH12*SA1*dAlpha3&
  &KanUsual())/(CB*MW*SW) - (0.125D0*CA22*CA3*EL*MH12*SA1*dAlpha3KanUsual())/(CB*MW*SW) - (0.375D0*CA12*CA22*CA3*EL*MH12*SA1*dAlp&
  &ha3KanUsual())/(CB*MW*SW) - (0.0625D0*CA3*EL*MH32*SA1*dAlpha3KanUsual())/(CB*MW*SW) - (0.1875D0*CA12*CA3*EL*MH32*SA1*dAlpha3Ka&
  &nUsual())/(CB*MW*SW) - (0.0625D0*CA22*CA3*EL*MH32*SA1*dAlpha3KanUsual())/(CB*MW*SW) - (0.1875D0*CA12*CA22*CA3*EL*MH32*SA1*dAlp&
  &ha3KanUsual())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*m12squared*SA22*dAlpha3KanUsual())/(CB*MW*SW) + (0.125D0*CA3*EL*MH12*SA1*SA22&
  &*dAlpha3KanUsual())/(CB*MW*SW) + (0.375D0*CA12*CA3*EL*MH12*SA1*SA22*dAlpha3KanUsual())/(CB*MW*SW) + (0.0625D0*CA3*EL*MH32*SA1*&
  &SA22*dAlpha3KanUsual())/(CB*MW*SW) + (0.1875D0*CA12*CA3*EL*MH32*SA1*SA22*dAlpha3KanUsual())/(CB*MW*SW) - (0.1875D0*CA1*EL*MH12&
  &*SA2*SA3*dAlpha3KanUsual())/(CB*MW*SW) - (0.5625D0*CA1*CA22*EL*MH12*SA2*SA3*dAlpha3KanUsual())/(CB*MW*SW) - (0.09375D0*CA1*EL*&
  &MH32*SA2*SA3*dAlpha3KanUsual())/(CB*MW*SW) - (0.28125D0*CA1*CA22*EL*MH32*SA2*SA3*dAlpha3KanUsual())/(CB*MW*SW) - (0.28125D0*EL&
  &*m12squared*SA1*SA2*SA3*dAlpha3KanUsual())/(CB*MW*SW) - (0.84375D0*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha3KanUsual())/(CB*MW*SW&
  &) + (0.1875D0*CA1*EL*MH12*SA12*SA2*SA3*dAlpha3KanUsual())/(CB*MW*SW) + (0.5625D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dAlpha3KanUsual&
  &())/(CB*MW*SW) + (0.09375D0*CA1*EL*MH32*SA12*SA2*SA3*dAlpha3KanUsual())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*MH32*SA12*SA2*SA3*&
  &dAlpha3KanUsual())/(CB*MW*SW) + (0.125D0*CA1*CA3*EL*MH12*dAlpha3KanUsual())/(MW*SB*SW) + (0.125D0*CA1*CA22*CA3*EL*MH12*dAlpha3&
  &KanUsual())/(MW*SB*SW) + (0.0625D0*CA1*CA3*EL*MH32*dAlpha3KanUsual())/(MW*SB*SW) + (0.0625D0*CA1*CA22*CA3*EL*MH32*dAlpha3KanUs&
  &ual())/(MW*SB*SW) - (0.1875D0*CA3*EL*m12squared*SA1*dAlpha3KanUsual())/(MW*SB*SW) - (0.1875D0*CA22*CA3*EL*m12squared*SA1*dAlph&
  &a3KanUsual())/(MW*SB*SW) + (0.125D0*CA3*EL*m12squared*SA1*dAlpha3KanUsual())/(CB2*MW*SB*SW) + (0.5625D0*CA12*CA3*EL*m12squared&
  &*SA1*dAlpha3KanUsual())/(CB2*MW*SB*SW) + (0.125D0*CA22*CA3*EL*m12squared*SA1*dAlpha3KanUsual())/(CB2*MW*SB*SW) + (0.5625D0*CA1&
  &2*CA22*CA3*EL*m12squared*SA1*dAlpha3KanUsual())/(CB2*MW*SB*SW) + (0.375D0*CA1*CA3*EL*MH12*SA12*dAlpha3KanUsual())/(MW*SB*SW) +&
  & (0.375D0*CA1*CA22*CA3*EL*MH12*SA12*dAlpha3KanUsual())/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*MH32*SA12*dAlpha3KanUsual())/(MW*SB*S&
  &W) + (0.1875D0*CA1*CA22*CA3*EL*MH32*SA12*dAlpha3KanUsual())/(MW*SB*SW) - (0.125D0*CA1*CA3*EL*MH12*SA22*dAlpha3KanUsual())/(MW*&
  &SB*SW) - (0.0625D0*CA1*CA3*EL*MH32*SA22*dAlpha3KanUsual())/(MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA1*SA22*dAlpha3KanUsual()&
  &)/(MW*SB*SW) - (0.125D0*CA3*EL*m12squared*SA1*SA22*dAlpha3KanUsual())/(CB2*MW*SB*SW) - (0.5625D0*CA12*CA3*EL*m12squared*SA1*SA&
  &22*dAlpha3KanUsual())/(CB2*MW*SB*SW) - (0.375D0*CA1*CA3*EL*MH12*SA12*SA22*dAlpha3KanUsual())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL&
  &*MH32*SA12*SA22*dAlpha3KanUsual())/(MW*SB*SW) - (0.28125D0*CA1*EL*m12squared*SA2*SA3*dAlpha3KanUsual())/(MW*SB*SW) - (0.84375D&
  &0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha3KanUsual())/(MW*SB*SW) + (0.1875D0*CA1*EL*m12squared*SA2*SA3*dAlpha3KanUsual())/(CB2*M&
  &W*SB*SW) + (0.5625D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha3KanUsual())/(CB2*MW*SB*SW) - (0.1875D0*EL*MH12*SA1*SA2*SA3*dAlpha3K&
  &anUsual())/(MW*SB*SW) + (0.1875D0*CA12*EL*MH12*SA1*SA2*SA3*dAlpha3KanUsual())/(MW*SB*SW) - (0.5625D0*CA22*EL*MH12*SA1*SA2*SA3*&
  &dAlpha3KanUsual())/(MW*SB*SW) + (0.5625D0*CA12*CA22*EL*MH12*SA1*SA2*SA3*dAlpha3KanUsual())/(MW*SB*SW) - (0.09375D0*EL*MH32*SA1&
  &*SA2*SA3*dAlpha3KanUsual())/(MW*SB*SW) + (0.09375D0*CA12*EL*MH32*SA1*SA2*SA3*dAlpha3KanUsual())/(MW*SB*SW) - (0.28125D0*CA22*E&
  &L*MH32*SA1*SA2*SA3*dAlpha3KanUsual())/(MW*SB*SW) + (0.28125D0*CA12*CA22*EL*MH32*SA1*SA2*SA3*dAlpha3KanUsual())/(MW*SB*SW) - (0&
  &.28125D0*CA1*EL*m12squared*SA12*SA2*SA3*dAlpha3KanUsual())/(CB2*MW*SB*SW) - (0.84375D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*dAl&
  &pha3KanUsual())/(CB2*MW*SB*SW) - (0.125D0*CA1*CA3*EL*m12squared*dAlpha3KanUsual())/(CB*MW*SB2*SW) - (0.125D0*CA1*CA22*CA3*EL*m&
  &12squared*dAlpha3KanUsual())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA3*EL*m12squared*SA12*dAlpha3KanUsual())/(CB*MW*SB2*SW) - (0.5625&
  &D0*CA1*CA22*CA3*EL*m12squared*SA12*dAlpha3KanUsual())/(CB*MW*SB2*SW) + (0.125D0*CA1*CA3*EL*m12squared*SA22*dAlpha3KanUsual())/&
  &(CB*MW*SB2*SW) + (0.5625D0*CA1*CA3*EL*m12squared*SA12*SA22*dAlpha3KanUsual())/(CB*MW*SB2*SW) + (0.1875D0*EL*m12squared*SA1*SA2&
  &*SA3*dAlpha3KanUsual())/(CB*MW*SB2*SW) - (0.28125D0*CA12*EL*m12squared*SA1*SA2*SA3*dAlpha3KanUsual())/(CB*MW*SB2*SW) + (0.5625&
  &D0*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha3KanUsual())/(CB*MW*SB2*SW) - (0.84375D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha3Ka&
  &nUsual())/(CB*MW*SB2*SW) - (0.0625D0*CA1*CA3*EL*m12squared*dAlpha3KanUsual())/(MW*SB*SW*TB) - (0.0625D0*CA1*CA22*CA3*EL*m12squ&
  &ared*dAlpha3KanUsual())/(MW*SB*SW*TB) + (0.0625D0*CA1*CA3*EL*m12squared*SA22*dAlpha3KanUsual())/(MW*SB*SW*TB) + (0.09375D0*EL*&
  &m12squared*SA1*SA2*SA3*dAlpha3KanUsual())/(MW*SB*SW*TB) + (0.28125D0*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha3KanUsual())/(MW*SB*&
  &SW*TB) + (0.0625D0*CA3*EL*m12squared*SA1*TB*dAlpha3KanUsual())/(CB*MW*SW) + (0.0625D0*CA22*CA3*EL*m12squared*SA1*TB*dAlpha3Kan&
  &Usual())/(CB*MW*SW) - (0.0625D0*CA3*EL*m12squared*SA1*SA22*TB*dAlpha3KanUsual())/(CB*MW*SW) + (0.09375D0*CA1*EL*m12squared*SA2&
  &*SA3*TB*dAlpha3KanUsual())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*m12squared*SA2*SA3*TB*dAlpha3KanUsual())/(CB*MW*SW) + (0.5D0*CA&
  &2*MH12*SA3*dAlpha3KanUsual())/vS + (0.25D0*CA2*MH32*SA3*dAlpha3KanUsual())/vS + (1.5D0*CA2*MH12*SA22*SA3*dAlpha3KanUsual())/vS&
  & + (0.75D0*CA2*MH32*SA22*SA3*dAlpha3KanUsual())/vS + (0.09375D0*CA3*EL*MH12*SA1*SA2*dBeta1KanUsual())/(CB*MW*SW) - (0.09375D0*&
  &CA12*CA3*EL*MH12*SA1*SA2*dBeta1KanUsual())/(CB*MW*SW) + (0.28125D0*CA22*CA3*EL*MH12*SA1*SA2*dBeta1KanUsual())/(CB*MW*SW) - (0.&
  &28125D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dBeta1KanUsual())/(CB*MW*SW) + (0.046875D0*CA3*EL*MH32*SA1*SA2*dBeta1KanUsual())/(CB*MW*&
  &SW) - (0.046875D0*CA12*CA3*EL*MH32*SA1*SA2*dBeta1KanUsual())/(CB*MW*SW) + (0.140625D0*CA22*CA3*EL*MH32*SA1*SA2*dBeta1KanUsual(&
  &))/(CB*MW*SW) - (0.140625D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*dBeta1KanUsual())/(CB*MW*SW) + (0.0625D0*CA1*EL*MH12*SA3*dBeta1KanUs&
  &ual())/(CB*MW*SW) + (0.0625D0*CA1*CA22*EL*MH12*SA3*dBeta1KanUsual())/(CB*MW*SW) + (0.03125D0*CA1*EL*MH32*SA3*dBeta1KanUsual())&
  &/(CB*MW*SW) + (0.03125D0*CA1*CA22*EL*MH32*SA3*dBeta1KanUsual())/(CB*MW*SW) + (0.1875D0*CA1*EL*MH12*SA12*SA3*dBeta1KanUsual())/&
  &(CB*MW*SW) + (0.1875D0*CA1*CA22*EL*MH12*SA12*SA3*dBeta1KanUsual())/(CB*MW*SW) + (0.09375D0*CA1*EL*MH32*SA12*SA3*dBeta1KanUsual&
  &())/(CB*MW*SW) + (0.09375D0*CA1*CA22*EL*MH32*SA12*SA3*dBeta1KanUsual())/(CB*MW*SW) - (0.0625D0*CA1*EL*MH12*SA22*SA3*dBeta1KanU&
  &sual())/(CB*MW*SW) - (0.03125D0*CA1*EL*MH32*SA22*SA3*dBeta1KanUsual())/(CB*MW*SW) - (0.1875D0*CA1*EL*MH12*SA12*SA22*SA3*dBeta1&
  &KanUsual())/(CB*MW*SW) - (0.09375D0*CA1*EL*MH32*SA12*SA22*SA3*dBeta1KanUsual())/(CB*MW*SW) + (0.09375D0*CA3*EL*m12squared*SA1*&
  &SA2*dBeta1KanUsual())/(MW*SB*SW) + (0.28125D0*CA22*CA3*EL*m12squared*SA1*SA2*dBeta1KanUsual())/(MW*SB*SW) - (0.328125D0*CA3*EL&
  &*m12squared*SA1*SA2*dBeta1KanUsual())/(CB2*MW*SB*SW) + (0.421875D0*CA12*CA3*EL*m12squared*SA1*SA2*dBeta1KanUsual())/(CB2*MW*SB&
  &*SW) - (0.984375D0*CA22*CA3*EL*m12squared*SA1*SA2*dBeta1KanUsual())/(CB2*MW*SB*SW) + (1.265625D0*CA12*CA22*CA3*EL*m12squared*S&
  &A1*SA2*dBeta1KanUsual())/(CB2*MW*SB*SW) + (0.0625D0*CA1*EL*m12squared*SA3*dBeta1KanUsual())/(MW*SB*SW) + (0.0625D0*CA1*CA22*EL&
  &*m12squared*SA3*dBeta1KanUsual())/(MW*SB*SW) - (0.21875D0*CA1*EL*m12squared*SA3*dBeta1KanUsual())/(CB2*MW*SB*SW) - (0.21875D0*&
  &CA1*CA22*EL*m12squared*SA3*dBeta1KanUsual())/(CB2*MW*SB*SW) - (0.84375D0*CA1*EL*m12squared*SA12*SA3*dBeta1KanUsual())/(CB2*MW*&
  &SB*SW) - (0.84375D0*CA1*CA22*EL*m12squared*SA12*SA3*dBeta1KanUsual())/(CB2*MW*SB*SW) - (0.0625D0*CA1*EL*m12squared*SA22*SA3*dB&
  &eta1KanUsual())/(MW*SB*SW) + (0.21875D0*CA1*EL*m12squared*SA22*SA3*dBeta1KanUsual())/(CB2*MW*SB*SW) + (0.84375D0*CA1*EL*m12squ&
  &ared*SA12*SA22*SA3*dBeta1KanUsual())/(CB2*MW*SB*SW) + (0.09375D0*CA1*CA3*EL*m12squared*SA2*dBeta1KanUsual())/(CB*MW*SB2*SW) + &
  &(0.28125D0*CA1*CA22*CA3*EL*m12squared*SA2*dBeta1KanUsual())/(CB*MW*SB2*SW) - (0.09375D0*CA3*EL*MH12*SA1*SA2*dBeta1KanUsual())/&
  &(CB*MW*SB2*SW) + (0.09375D0*CA12*CA3*EL*MH12*SA1*SA2*dBeta1KanUsual())/(CB*MW*SB2*SW) - (0.28125D0*CA22*CA3*EL*MH12*SA1*SA2*dB&
  &eta1KanUsual())/(CB*MW*SB2*SW) + (0.28125D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dBeta1KanUsual())/(CB*MW*SB2*SW) - (0.046875D0*CA3*E&
  &L*MH32*SA1*SA2*dBeta1KanUsual())/(CB*MW*SB2*SW) + (0.046875D0*CA12*CA3*EL*MH32*SA1*SA2*dBeta1KanUsual())/(CB*MW*SB2*SW) - (0.1&
  &40625D0*CA22*CA3*EL*MH32*SA1*SA2*dBeta1KanUsual())/(CB*MW*SB2*SW) + (0.140625D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*dBeta1KanUsual()&
  &)/(CB*MW*SB2*SW) - (0.28125D0*CA1*CA3*EL*m12squared*SA12*SA2*dBeta1KanUsual())/(CB*MW*SB2*SW) - (0.84375D0*CA1*CA22*CA3*EL*m12&
  &squared*SA12*SA2*dBeta1KanUsual())/(CB*MW*SB2*SW) - (0.0625D0*CA1*EL*MH12*SA3*dBeta1KanUsual())/(CB*MW*SB2*SW) - (0.0625D0*CA1&
  &*CA22*EL*MH12*SA3*dBeta1KanUsual())/(CB*MW*SB2*SW) - (0.03125D0*CA1*EL*MH32*SA3*dBeta1KanUsual())/(CB*MW*SB2*SW) - (0.03125D0*&
  &CA1*CA22*EL*MH32*SA3*dBeta1KanUsual())/(CB*MW*SB2*SW) - (0.0625D0*EL*m12squared*SA1*SA3*dBeta1KanUsual())/(CB*MW*SB2*SW) - (0.&
  &5625D0*CA12*EL*m12squared*SA1*SA3*dBeta1KanUsual())/(CB*MW*SB2*SW) - (0.0625D0*CA22*EL*m12squared*SA1*SA3*dBeta1KanUsual())/(C&
  &B*MW*SB2*SW) - (0.5625D0*CA12*CA22*EL*m12squared*SA1*SA3*dBeta1KanUsual())/(CB*MW*SB2*SW) - (0.1875D0*CA1*EL*MH12*SA12*SA3*dBe&
  &ta1KanUsual())/(CB*MW*SB2*SW) - (0.1875D0*CA1*CA22*EL*MH12*SA12*SA3*dBeta1KanUsual())/(CB*MW*SB2*SW) - (0.09375D0*CA1*EL*MH32*&
  &SA12*SA3*dBeta1KanUsual())/(CB*MW*SB2*SW) - (0.09375D0*CA1*CA22*EL*MH32*SA12*SA3*dBeta1KanUsual())/(CB*MW*SB2*SW) + (0.0625D0*&
  &CA1*EL*MH12*SA22*SA3*dBeta1KanUsual())/(CB*MW*SB2*SW) + (0.03125D0*CA1*EL*MH32*SA22*SA3*dBeta1KanUsual())/(CB*MW*SB2*SW) + (0.&
  &0625D0*EL*m12squared*SA1*SA22*SA3*dBeta1KanUsual())/(CB*MW*SB2*SW) + (0.5625D0*CA12*EL*m12squared*SA1*SA22*SA3*dBeta1KanUsual(&
  &))/(CB*MW*SB2*SW) + (0.1875D0*CA1*EL*MH12*SA12*SA22*SA3*dBeta1KanUsual())/(CB*MW*SB2*SW) + (0.09375D0*CA1*EL*MH32*SA12*SA22*SA&
  &3*dBeta1KanUsual())/(CB*MW*SB2*SW) - (0.1875D0*CA1*CA3*EL*m12squared*SA2*dBeta1KanUsual())/(MW*SB*SW*TB) - (0.5625D0*CA1*CA22*&
  &CA3*EL*m12squared*SA2*dBeta1KanUsual())/(MW*SB*SW*TB) - (0.09375D0*CA3*EL*MH12*SA1*SA2*dBeta1KanUsual())/(MW*SB*SW*TB) + (0.09&
  &375D0*CA12*CA3*EL*MH12*SA1*SA2*dBeta1KanUsual())/(MW*SB*SW*TB) - (0.28125D0*CA22*CA3*EL*MH12*SA1*SA2*dBeta1KanUsual())/(MW*SB*&
  &SW*TB) + (0.28125D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dBeta1KanUsual())/(MW*SB*SW*TB) - (0.046875D0*CA3*EL*MH32*SA1*SA2*dBeta1KanU&
  &sual())/(MW*SB*SW*TB) + (0.046875D0*CA12*CA3*EL*MH32*SA1*SA2*dBeta1KanUsual())/(MW*SB*SW*TB) - (0.140625D0*CA22*CA3*EL*MH32*SA&
  &1*SA2*dBeta1KanUsual())/(MW*SB*SW*TB) + (0.140625D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*dBeta1KanUsual())/(MW*SB*SW*TB) - (0.0625D0*&
  &CA1*EL*MH12*SA3*dBeta1KanUsual())/(MW*SB*SW*TB) - (0.0625D0*CA1*CA22*EL*MH12*SA3*dBeta1KanUsual())/(MW*SB*SW*TB) - (0.03125D0*&
  &CA1*EL*MH32*SA3*dBeta1KanUsual())/(MW*SB*SW*TB) - (0.03125D0*CA1*CA22*EL*MH32*SA3*dBeta1KanUsual())/(MW*SB*SW*TB) + (0.125D0*E&
  &L*m12squared*SA1*SA3*dBeta1KanUsual())/(MW*SB*SW*TB) + (0.125D0*CA22*EL*m12squared*SA1*SA3*dBeta1KanUsual())/(MW*SB*SW*TB) - (&
  &0.1875D0*CA1*EL*MH12*SA12*SA3*dBeta1KanUsual())/(MW*SB*SW*TB) - (0.1875D0*CA1*CA22*EL*MH12*SA12*SA3*dBeta1KanUsual())/(MW*SB*S&
  &W*TB) - (0.09375D0*CA1*EL*MH32*SA12*SA3*dBeta1KanUsual())/(MW*SB*SW*TB) - (0.09375D0*CA1*CA22*EL*MH32*SA12*SA3*dBeta1KanUsual(&
  &))/(MW*SB*SW*TB) + (0.0625D0*CA1*EL*MH12*SA22*SA3*dBeta1KanUsual())/(MW*SB*SW*TB) + (0.03125D0*CA1*EL*MH32*SA22*SA3*dBeta1KanU&
  &sual())/(MW*SB*SW*TB) - (0.125D0*EL*m12squared*SA1*SA22*SA3*dBeta1KanUsual())/(MW*SB*SW*TB) + (0.1875D0*CA1*EL*MH12*SA12*SA22*&
  &SA3*dBeta1KanUsual())/(MW*SB*SW*TB) + (0.09375D0*CA1*EL*MH32*SA12*SA22*SA3*dBeta1KanUsual())/(MW*SB*SW*TB) + (0.1875D0*CA1*CA3&
  &*EL*MH12*SA2*TB*dBeta1KanUsual())/(CB*MW*SW) + (0.5625D0*CA1*CA22*CA3*EL*MH12*SA2*TB*dBeta1KanUsual())/(CB*MW*SW) + (0.09375D0&
  &*CA1*CA3*EL*MH32*SA2*TB*dBeta1KanUsual())/(CB*MW*SW) + (0.28125D0*CA1*CA22*CA3*EL*MH32*SA2*TB*dBeta1KanUsual())/(CB*MW*SW) + (&
  &0.328125D0*CA3*EL*m12squared*SA1*SA2*TB*dBeta1KanUsual())/(CB*MW*SW) + (0.984375D0*CA22*CA3*EL*m12squared*SA1*SA2*TB*dBeta1Kan&
  &Usual())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*MH12*SA12*SA2*TB*dBeta1KanUsual())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*EL*MH12*SA12&
  &*SA2*TB*dBeta1KanUsual())/(CB*MW*SW) - (0.09375D0*CA1*CA3*EL*MH32*SA12*SA2*TB*dBeta1KanUsual())/(CB*MW*SW) - (0.28125D0*CA1*CA&
  &22*CA3*EL*MH32*SA12*SA2*TB*dBeta1KanUsual())/(CB*MW*SW) + (0.21875D0*CA1*EL*m12squared*SA3*TB*dBeta1KanUsual())/(CB*MW*SW) + (&
  &0.21875D0*CA1*CA22*EL*m12squared*SA3*TB*dBeta1KanUsual())/(CB*MW*SW) - (0.125D0*EL*MH12*SA1*SA3*TB*dBeta1KanUsual())/(CB*MW*SW&
  &) - (0.375D0*CA12*EL*MH12*SA1*SA3*TB*dBeta1KanUsual())/(CB*MW*SW) - (0.125D0*CA22*EL*MH12*SA1*SA3*TB*dBeta1KanUsual())/(CB*MW*&
  &SW) - (0.375D0*CA12*CA22*EL*MH12*SA1*SA3*TB*dBeta1KanUsual())/(CB*MW*SW) - (0.0625D0*EL*MH32*SA1*SA3*TB*dBeta1KanUsual())/(CB*&
  &MW*SW) - (0.1875D0*CA12*EL*MH32*SA1*SA3*TB*dBeta1KanUsual())/(CB*MW*SW) - (0.0625D0*CA22*EL*MH32*SA1*SA3*TB*dBeta1KanUsual())/&
  &(CB*MW*SW) - (0.1875D0*CA12*CA22*EL*MH32*SA1*SA3*TB*dBeta1KanUsual())/(CB*MW*SW) - (0.21875D0*CA1*EL*m12squared*SA22*SA3*TB*dB&
  &eta1KanUsual())/(CB*MW*SW) + (0.125D0*EL*MH12*SA1*SA22*SA3*TB*dBeta1KanUsual())/(CB*MW*SW) + (0.375D0*CA12*EL*MH12*SA1*SA22*SA&
  &3*TB*dBeta1KanUsual())/(CB*MW*SW) + (0.0625D0*EL*MH32*SA1*SA22*SA3*TB*dBeta1KanUsual())/(CB*MW*SW) + (0.1875D0*CA12*EL*MH32*SA&
  &1*SA22*SA3*TB*dBeta1KanUsual())/(CB*MW*SW) + (0.140625D0*CA3*EL*m12squared*SA1*SA2*dBeta1KanUsual())/(MW*SB*SW*TB2) + (0.42187&
  &5D0*CA22*CA3*EL*m12squared*SA1*SA2*dBeta1KanUsual())/(MW*SB*SW*TB2) + (0.09375D0*CA1*EL*m12squared*SA3*dBeta1KanUsual())/(MW*S&
  &B*SW*TB2) + (0.09375D0*CA1*CA22*EL*m12squared*SA3*dBeta1KanUsual())/(MW*SB*SW*TB2) - (0.09375D0*CA1*EL*m12squared*SA22*SA3*dBe&
  &ta1KanUsual())/(MW*SB*SW*TB2) - (0.1875D0*CA1*CA3*EL*m12squared*SA2*TB2*dBeta1KanUsual())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*&
  &EL*m12squared*SA2*TB2*dBeta1KanUsual())/(CB*MW*SW) + (0.125D0*EL*m12squared*SA1*SA3*TB2*dBeta1KanUsual())/(CB*MW*SW) + (0.125D&
  &0*CA22*EL*m12squared*SA1*SA3*TB2*dBeta1KanUsual())/(CB*MW*SW) - (0.125D0*EL*m12squared*SA1*SA22*SA3*TB2*dBeta1KanUsual())/(CB*&
  &MW*SW) + (0.125D0*CA2*CA3*MH12*dBeta1KanUsual())/(CB*SB*vS) + (0.0625D0*CA2*CA3*MH32*dBeta1KanUsual())/(CB*SB*vS) + (0.375D0*C&
  &A2*CA3*MH12*SA22*dBeta1KanUsual())/(CB*SB*vS) + (0.1875D0*CA2*CA3*MH32*SA22*dBeta1KanUsual())/(CB*SB*vS) - (0.125D0*CA2*CA3*MH&
  &12*dBeta1KanUsual())/(TB*vS) - (0.0625D0*CA2*CA3*MH32*dBeta1KanUsual())/(TB*vS) - (0.375D0*CA2*CA3*MH12*SA22*dBeta1KanUsual())&
  &/(TB*vS) - (0.1875D0*CA2*CA3*MH32*SA22*dBeta1KanUsual())/(TB*vS) - (0.125D0*CA2*CA3*MH12*TB*dBeta1KanUsual())/vS - (0.0625D0*C&
  &A2*CA3*MH32*TB*dBeta1KanUsual())/vS - (0.375D0*CA2*CA3*MH12*SA22*TB*dBeta1KanUsual())/vS - (0.1875D0*CA2*CA3*MH32*SA22*TB*dBet&
  &a1KanUsual())/vS - (0.375D0*EL*MH12*SA3*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.375D0*CA22*EL*MH12*SA3*dAlpha1&
  &KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*EL*MH32*SA3*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.18&
  &75D0*CA22*EL*MH32*SA3*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.375D0*EL*MH12*SA22*SA3*dAlpha1KanUsual()*DBLE(CA&
  &1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*EL*MH32*SA22*SA3*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA3*EL*M&
  &H12*SA2*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA22*CA3*EL*MH12*SA2*dAlpha1KanUsual()*DBLE(CA1**INT(3.&
  &D0)))/(MW*SB*SW) - (0.09375D0*CA3*EL*MH32*SA2*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA22*CA3*EL*MH32&
  &*SA2*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.5625D0*EL*m12squared*SA3*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/&
  &(CB2*MW*SB*SW) + (0.5625D0*CA22*EL*m12squared*SA3*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*EL*m12squ&
  &ared*SA22*SA3*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.28125D0*CA3*EL*m12squared*SA2*dAlpha1KanUsual()*DBLE&
  &(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA22*CA3*EL*m12squared*SA2*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW&
  &) + (0.0625D0*CA2*CA3*EL*MH12*dAlpha2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA2*CA3*EL*MH32*dAlpha2KanUsual(&
  &)*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.5625D0*CA2*CA3*EL*MH12*SA22*dAlpha2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.28&
  &125D0*CA2*CA3*EL*MH32*SA22*dAlpha2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA2*CA3*EL*m12squared*dAlpha2KanUsu&
  &al()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.84375D0*CA2*CA3*EL*m12squared*SA22*dAlpha2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB2&
  &*MW*SB*SW) + (0.5D0*CA2*EL*MH12*SA2*SA3*dAlpha2KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.25D0*CA2*EL*MH32*SA2*SA3*dAlph&
  &a2KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.75D0*CA2*EL*m12squared*SA2*SA3*dAlpha2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*&
  &MW*SB2*SW) - (0.0625D0*EL*MH12*SA2*SA3*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA22*EL*MH12*SA2*SA3*dAl&
  &pha3KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*EL*MH32*SA2*SA3*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW&
  &) - (0.09375D0*CA22*EL*MH32*SA2*SA3*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.125D0*CA3*EL*MH12*dAlpha3KanUsual(&
  &)*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.125D0*CA22*CA3*EL*MH12*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.0625D0*&
  &CA3*EL*MH32*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.0625D0*CA22*CA3*EL*MH32*dAlpha3KanUsual()*DBLE(CA1**INT(3.&
  &D0)))/(MW*SB*SW) + (0.125D0*CA3*EL*MH12*SA22*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*CA3*EL*MH32*SA22*d&
  &Alpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*EL*m12squared*SA2*SA3*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(&
  &CB2*MW*SB*SW) + (0.28125D0*CA22*EL*m12squared*SA2*SA3*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*CA3*E&
  &L*m12squared*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*CA22*CA3*EL*m12squared*dAlpha3KanUsual()*DBLE(&
  &CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.1875D0*CA3*EL*m12squared*SA22*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0&
  &.0625D0*EL*MH12*SA3*dBeta1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.0625D0*CA22*EL*MH12*SA3*dBeta1KanUsual()*DBLE(CA1**&
  &INT(3.D0)))/(CB*MW*SW) - (0.03125D0*EL*MH32*SA3*dBeta1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*CA22*EL*MH32*SA&
  &3*dBeta1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*EL*MH12*SA22*SA3*dBeta1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW&
  &*SW) + (0.03125D0*EL*MH32*SA22*SA3*dBeta1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*EL*m12squared*SA3*dBeta1KanU&
  &sual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.28125D0*CA22*EL*m12squared*SA3*dBeta1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW&
  &*SB*SW) - (0.28125D0*EL*m12squared*SA22*SA3*dBeta1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.09375D0*CA3*EL*m12squar&
  &ed*SA2*dBeta1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA22*CA3*EL*m12squared*SA2*dBeta1KanUsual()*DBLE(CA1&
  &**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*EL*MH12*SA3*dBeta1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*CA22*EL&
  &*MH12*SA3*dBeta1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.03125D0*EL*MH32*SA3*dBeta1KanUsual()*DBLE(CA1**INT(3.D0))&
  &)/(CB*MW*SB2*SW) + (0.03125D0*CA22*EL*MH32*SA3*dBeta1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.0625D0*EL*MH12*SA22*&
  &SA3*dBeta1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.03125D0*EL*MH32*SA22*SA3*dBeta1KanUsual()*DBLE(CA1**INT(3.D0)))&
  &/(CB*MW*SB2*SW) + (0.0625D0*EL*MH12*SA3*dBeta1KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.0625D0*CA22*EL*MH12*SA3*dBet&
  &a1KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.03125D0*EL*MH32*SA3*dBeta1KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB)&
  & + (0.03125D0*CA22*EL*MH32*SA3*dBeta1KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.0625D0*EL*MH12*SA22*SA3*dBeta1KanUsua&
  &l()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.03125D0*EL*MH32*SA22*SA3*dBeta1KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (&
  &0.0625D0*CA3*EL*MH12*SA2*TB*dBeta1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA22*CA3*EL*MH12*SA2*TB*dBeta1KanUsu&
  &al()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA3*EL*MH32*SA2*TB*dBeta1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.0&
  &9375D0*CA22*CA3*EL*MH32*SA2*TB*dBeta1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA1*CA3*EL*MH12*dAlpha2KanUsual()&
  &*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA1*CA3*EL*MH32*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.84375D0&
  &*CA3*EL*m12squared*SA1*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.5625D0*CA1*CA3*EL*MH12*SA12*dAlpha2KanUsual()*D&
  &BLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA1*CA3*EL*MH32*SA12*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.8437&
  &5D0*CA1*CA3*EL*m12squared*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA1*CA3*EL*m12squared*dAlpha2KanUsual&
  &()*DBLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.5625D0*CA3*EL*MH12*SA1*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.56&
  &25D0*CA12*CA3*EL*MH12*SA1*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*CA3*EL*MH32*SA1*dAlpha2KanUsual()*DB&
  &LE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA12*CA3*EL*MH32*SA1*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) + (0.84375&
  &D0*CA1*CA3*EL*m12squared*SA12*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA3*EL*m12squared*SA1*dAlpha2&
  &KanUsual()*DBLE(CA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA3*EL*m12squared*SA1*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0))&
  &)/(CB*MW*SB2*SW) - (0.28125D0*CA3*EL*m12squared*SA1*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.28125D0*CA1*CA3&
  &*EL*m12squared*TB*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.5D0*MH12*SA3*dAlpha3KanUsual()*DBLE(CA2**INT(3.D0)))&
  &/vS - (0.25D0*MH32*SA3*dAlpha3KanUsual()*DBLE(CA2**INT(3.D0)))/vS - (0.125D0*CA3*MH12*dBeta1KanUsual()*DBLE(CA2**INT(3.D0)))/(&
  &CB*SB*vS) - (0.0625D0*CA3*MH32*dBeta1KanUsual()*DBLE(CA2**INT(3.D0)))/(CB*SB*vS) + (0.125D0*CA3*MH12*dBeta1KanUsual()*DBLE(CA2&
  &**INT(3.D0)))/(TB*vS) + (0.0625D0*CA3*MH32*dBeta1KanUsual()*DBLE(CA2**INT(3.D0)))/(TB*vS) + (0.125D0*CA3*MH12*TB*dBeta1KanUsua&
  &l()*DBLE(CA2**INT(3.D0)))/vS + (0.0625D0*CA3*MH32*TB*dBeta1KanUsual()*DBLE(CA2**INT(3.D0)))/vS + (0.1875D0*CA3*EL*MH12*dAlpha2&
  &KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA3*EL*MH32*dAlpha2KanUsual()*DBLE(CA1**INT(3.D0&
  &))*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA3*EL*m12squared*dAlpha2KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0))&
  &)/(CB2*MW*SB*SW) - (0.375D0*CA1*CA3*EL*m12squared*SA2*dBeta1KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (1.125D0*CA1*CA22*CA3*E&
  &L*m12squared*SA2*dBeta1KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.5625D0*CA1*CA3*EL*m12squared*SA12*SA2*dBeta1KanUsual()*DB&
  &LE(CB**INT(-3.D0)))/(MW*SW) + (1.6875D0*CA1*CA22*CA3*EL*m12squared*SA12*SA2*dBeta1KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (&
  &0.25D0*EL*m12squared*SA1*SA3*dBeta1KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (1.125D0*CA12*EL*m12squared*SA1*SA3*dBeta1KanUsu&
  &al()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.25D0*CA22*EL*m12squared*SA1*SA3*dBeta1KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (1.12&
  &5D0*CA12*CA22*EL*m12squared*SA1*SA3*dBeta1KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.25D0*EL*m12squared*SA1*SA22*SA3*dBeta1&
  &KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (1.125D0*CA12*EL*m12squared*SA1*SA22*SA3*dBeta1KanUsual()*DBLE(CB**INT(-3.D0)))/(MW&
  &*SW) - (0.1875D0*CA3*EL*m12squared*SA2*dBeta1KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.5625D0*CA22*CA&
  &3*EL*m12squared*SA2*dBeta1KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.1875D0*CA3*EL*MH12*SA2*dAlpha1Kan&
  &Usual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA22*CA3*EL*MH12*SA2*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) +&
  & (0.09375D0*CA3*EL*MH32*SA2*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA22*CA3*EL*MH32*SA2*dAlpha1KanUsu&
  &al()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA3*EL*m12squared*SA2*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW&
  &) - (0.84375D0*CA22*CA3*EL*m12squared*SA2*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.375D0*EL*MH12*SA3*dAlpha&
  &1KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.375D0*CA22*EL*MH12*SA3*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - &
  &(0.1875D0*EL*MH32*SA3*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA22*EL*MH32*SA3*dAlpha1KanUsual()*DBLE(S&
  &A1**INT(3.D0)))/(MW*SB*SW) + (0.375D0*EL*MH12*SA22*SA3*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*EL*MH32*&
  &SA22*SA3*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.5625D0*EL*m12squared*SA3*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0&
  &)))/(CB*MW*SB2*SW) + (0.5625D0*CA22*EL*m12squared*SA3*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.5625D0*EL*m1&
  &2squared*SA22*SA3*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.5D0*CA2*EL*MH12*SA2*SA3*dAlpha2KanUsual()*DBLE(S&
  &A1**INT(3.D0)))/(CB*MW*SW) - (0.25D0*CA2*EL*MH32*SA2*SA3*dAlpha2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA2*CA&
  &3*EL*MH12*dAlpha2KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA2*CA3*EL*MH32*dAlpha2KanUsual()*DBLE(SA1**INT(3.D0&
  &)))/(MW*SB*SW) - (0.5625D0*CA2*CA3*EL*MH12*SA22*dAlpha2KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA2*CA3*EL*MH3&
  &2*SA22*dAlpha2KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.75D0*CA2*EL*m12squared*SA2*SA3*dAlpha2KanUsual()*DBLE(SA1**INT(&
  &3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA2*CA3*EL*m12squared*dAlpha2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*&
  &CA2*CA3*EL*m12squared*SA22*dAlpha2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.125D0*CA3*EL*MH12*dAlpha3KanUsual()*DBL&
  &E(SA1**INT(3.D0)))/(CB*MW*SW) + (0.125D0*CA22*CA3*EL*MH12*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA3*E&
  &L*MH32*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA22*CA3*EL*MH32*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))&
  &/(CB*MW*SW) - (0.125D0*CA3*EL*MH12*SA22*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.0625D0*CA3*EL*MH32*SA22*dAlpha&
  &3KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA3*EL*m12squared*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*&
  &SW) - (0.1875D0*CA22*CA3*EL*m12squared*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA&
  &22*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D0*EL*MH12*SA2*SA3*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/&
  &(MW*SB*SW) - (0.1875D0*CA22*EL*MH12*SA2*SA3*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.03125D0*EL*MH32*SA2*SA3*dA&
  &lpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA22*EL*MH32*SA2*SA3*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(MW&
  &*SB*SW) + (0.09375D0*EL*m12squared*SA2*SA3*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA22*EL*m12squa&
  &red*SA2*SA3*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.03125D0*CA3*EL*MH12*SA2*dBeta1KanUsual()*DBLE(SA1**INT&
  &(3.D0)))/(CB*MW*SW) + (0.09375D0*CA22*CA3*EL*MH12*SA2*dBeta1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.015625D0*CA3*EL*M&
  &H32*SA2*dBeta1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.046875D0*CA22*CA3*EL*MH32*SA2*dBeta1KanUsual()*DBLE(SA1**INT(3.&
  &D0)))/(CB*MW*SW) - (0.140625D0*CA3*EL*m12squared*SA2*dBeta1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.421875D0*CA22*&
  &CA3*EL*m12squared*SA2*dBeta1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.03125D0*CA3*EL*MH12*SA2*dBeta1KanUsual()*DBLE&
  &(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.09375D0*CA22*CA3*EL*MH12*SA2*dBeta1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.&
  &015625D0*CA3*EL*MH32*SA2*dBeta1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.046875D0*CA22*CA3*EL*MH32*SA2*dBeta1KanUsu&
  &al()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*EL*m12squared*SA3*dBeta1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) &
  &+ (0.1875D0*CA22*EL*m12squared*SA3*dBeta1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.1875D0*EL*m12squared*SA22*SA3*dB&
  &eta1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.03125D0*CA3*EL*MH12*SA2*dBeta1KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB&
  &*SW*TB) - (0.09375D0*CA22*CA3*EL*MH12*SA2*dBeta1KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.015625D0*CA3*EL*MH32*SA2*d&
  &Beta1KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.046875D0*CA22*CA3*EL*MH32*SA2*dBeta1KanUsual()*DBLE(SA1**INT(3.D0)))/&
  &(MW*SB*SW*TB) + (0.125D0*EL*MH12*SA3*TB*dBeta1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.125D0*CA22*EL*MH12*SA3*TB*dBeta&
  &1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*EL*MH32*SA3*TB*dBeta1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0&
  &.0625D0*CA22*EL*MH32*SA3*TB*dBeta1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.125D0*EL*MH12*SA22*SA3*TB*dBeta1KanUsual()*&
  &DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.0625D0*EL*MH32*SA22*SA3*TB*dBeta1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0&
  &*CA3*EL*MH12*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA3*EL*MH32*dAlpha2KanUsual(&
  &)*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0))*&
  &DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.375D0*EL*m12squared*SA3*dBeta1KanUsual()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/&
  &(MW*SW) - (0.375D0*CA22*EL*m12squared*SA3*dBeta1KanUsual()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) + (0.375D0*EL*m1&
  &2squared*SA22*SA3*dBeta1KanUsual()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) - (0.28125D0*CA1*CA3*EL*m12squared*dAlph&
  &a1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA3*EL*MH12*SA1*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) +&
  & (0.5625D0*CA12*CA3*EL*MH12*SA1*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA3*EL*MH32*SA1*dAlpha1KanUsua&
  &l()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA12*CA3*EL*MH32*SA1*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0&
  &.1875D0*CA1*CA3*EL*MH12*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA1*CA3*EL*MH32*dAlpha1KanUsual()*DBLE&
  &(SA2**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*CA3*EL*m12squared*SA1*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.1875D0&
  &*CA3*EL*m12squared*SA1*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.84375D0*CA12*CA3*EL*m12squared*SA1*dAlpha1K&
  &anUsual()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA3*EL*MH12*SA12*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB&
  &*SW) - (0.28125D0*CA1*CA3*EL*MH32*SA12*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*m12squared*dA&
  &lpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA1*CA3*EL*m12squared*SA12*dAlpha1KanUsual()*DBLE(SA2**INT(3&
  &.D0)))/(CB*MW*SB2*SW) + (0.09375D0*CA1*CA3*EL*m12squared*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA&
  &3*EL*m12squared*SA1*TB*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (1.5D0*CA3*MH12*dAlpha2KanUsual()*DBLE(SA2**INT(3.&
  &D0)))/vS + (0.75D0*CA3*MH32*dAlpha2KanUsual()*DBLE(SA2**INT(3.D0)))/vS + (0.1875D0*CA1*EL*MH12*SA3*dAlpha3KanUsual()*DBLE(SA2*&
  &*INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1*EL*MH32*SA3*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*EL*m12squ&
  &ared*SA1*SA3*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA1*EL*MH12*SA12*SA3*dAlpha3KanUsual()*DBLE(SA2**I&
  &NT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1*EL*MH32*SA12*SA3*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA1*EL&
  &*m12squared*SA3*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA1*EL*m12squared*SA3*dAlpha3KanUsual()*DBLE(SA&
  &2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*EL*MH12*SA1*SA3*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA12*&
  &EL*MH12*SA1*SA3*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*EL*MH32*SA1*SA3*dAlpha3KanUsual()*DBLE(SA2**IN&
  &T(3.D0)))/(MW*SB*SW) - (0.09375D0*CA12*EL*MH32*SA1*SA3*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*CA1*EL*&
  &m12squared*SA12*SA3*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*EL*m12squared*SA1*SA3*dAlpha3KanUsual()&
  &*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA12*EL*m12squared*SA1*SA3*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*S&
  &B2*SW) - (0.09375D0*EL*m12squared*SA1*SA3*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA1*EL*m12squared&
  &*SA3*TB*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA3*EL*MH12*SA1*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))&
  &/(CB*MW*SW) + (0.09375D0*CA12*CA3*EL*MH12*SA1*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.046875D0*CA3*EL*MH32*SA1*&
  &dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.046875D0*CA12*CA3*EL*MH32*SA1*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/(C&
  &B*MW*SW) - (0.09375D0*CA3*EL*m12squared*SA1*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.328125D0*CA3*EL*m12squared*&
  &SA1*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.421875D0*CA12*CA3*EL*m12squared*SA1*dBeta1KanUsual()*DBLE(SA2**&
  &INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA1*CA3*EL*m12squared*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.09375&
  &D0*CA3*EL*MH12*SA1*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.09375D0*CA12*CA3*EL*MH12*SA1*dBeta1KanUsual()*DB&
  &LE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.046875D0*CA3*EL*MH32*SA1*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.04&
  &6875D0*CA12*CA3*EL*MH32*SA1*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA1*CA3*EL*m12squared*SA12*dBet&
  &a1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*CA1*CA3*EL*m12squared*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/(MW&
  &*SB*SW*TB) + (0.09375D0*CA3*EL*MH12*SA1*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA12*CA3*EL*MH12*SA1&
  &*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.046875D0*CA3*EL*MH32*SA1*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/(MW&
  &*SB*SW*TB) - (0.046875D0*CA12*CA3*EL*MH32*SA1*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.1875D0*CA1*CA3*EL*MH12&
  &*TB*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1*CA3*EL*MH32*TB*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/(&
  &CB*MW*SW) - (0.328125D0*CA3*EL*m12squared*SA1*TB*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA1*CA3*EL*MH12&
  &*SA12*TB*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1*CA3*EL*MH32*SA12*TB*dBeta1KanUsual()*DBLE(SA2**INT&
  &(3.D0)))/(CB*MW*SW) - (0.140625D0*CA3*EL*m12squared*SA1*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB2) + (0.1875D0*CA1*&
  &CA3*EL*m12squared*TB2*dBeta1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA3*EL*MH12*dAlpha1KanUsual()*DBLE(CA1**IN&
  &T(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA3*EL*MH32*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))&
  &)/(MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0&
  &625D0*EL*MH12*SA3*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*EL*MH32*SA3*dAlpha3KanU&
  &sual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*EL*m12squared*SA3*dAlpha3KanUsual()*DBLE(CA1**INT(3.&
  &D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA3*EL*m12squared*dBeta1KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3&
  &.D0)))/(CB*MW*SB2*SW) - (0.0625D0*CA3*EL*MH12*TB*dBeta1KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.0&
  &3125D0*CA3*EL*MH32*TB*dBeta1KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.375D0*CA1*CA3*EL*m12squared*&
  &dBeta1KanUsual()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) - (0.5625D0*CA1*CA3*EL*m12squared*SA12*dBeta1KanUsual()*DB&
  &LE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) + (0.1875D0*CA3*EL*m12squared*dBeta1KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(CB**&
  &INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) - (0.1875D0*CA3*EL*MH12*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))&
  &/(CB*MW*SW) - (0.09375D0*CA3*EL*MH32*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA3*&
  &EL*m12squared*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.0625D0*EL*MH12*SA3*dAlpha3KanUs&
  &ual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*EL*MH32*SA3*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0))*DB&
  &LE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*EL*m12squared*SA3*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB&
  &*MW*SB2*SW) - (0.03125D0*CA3*EL*MH12*dBeta1KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.015625D0*CA3*&
  &EL*MH32*dBeta1KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.140625D0*CA3*EL*m12squared*dBeta1KanUsual(&
  &)*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.03125D0*CA3*EL*MH12*dBeta1KanUsual()*DBLE(SA1**INT(3.D0))*DBL&
  &E(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.015625D0*CA3*EL*MH32*dBeta1KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW&
  &*SB2*SW) + (0.03125D0*CA3*EL*MH12*dBeta1KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.015625D0*CA3*&
  &EL*MH32*dBeta1KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.328125D0*CA3*EL*m12squared*SA1*SA2*dBet&
  &a1KanUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.421875D0*CA12*CA3*EL*m12squared*SA1*SA2*dBeta1KanUsual()*DBLE(SB**INT(-3.D0)))&
  &/(MW*SW) + (0.984375D0*CA22*CA3*EL*m12squared*SA1*SA2*dBeta1KanUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (1.265625D0*CA12*CA22*C&
  &A3*EL*m12squared*SA1*SA2*dBeta1KanUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.09375D0*CA3*EL*m12squared*SA1*SA2*dBeta1KanUsual(&
  &)*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.140625D0*CA12*CA3*EL*m12squared*SA1*SA2*dBeta1KanUsual()*DBLE(SB**INT(-3.D0)))/(CB2*M&
  &W*SW) + (0.28125D0*CA22*CA3*EL*m12squared*SA1*SA2*dBeta1KanUsual()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.421875D0*CA12*CA22*C&
  &A3*EL*m12squared*SA1*SA2*dBeta1KanUsual()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.21875D0*CA1*EL*m12squared*SA3*dBeta1KanUsual(&
  &)*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.21875D0*CA1*CA22*EL*m12squared*SA3*dBeta1KanUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.06&
  &25D0*CA1*EL*m12squared*SA3*dBeta1KanUsual()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.0625D0*CA1*CA22*EL*m12squared*SA3*dBeta1Kan&
  &Usual()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.84375D0*CA1*EL*m12squared*SA12*SA3*dBeta1KanUsual()*DBLE(SB**INT(-3.D0)))/(MW*S&
  &W) + (0.84375D0*CA1*CA22*EL*m12squared*SA12*SA3*dBeta1KanUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.28125D0*CA1*EL*m12squared*&
  &SA12*SA3*dBeta1KanUsual()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.28125D0*CA1*CA22*EL*m12squared*SA12*SA3*dBeta1KanUsual()*DBLE&
  &(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.21875D0*CA1*EL*m12squared*SA22*SA3*dBeta1KanUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.0625&
  &D0*CA1*EL*m12squared*SA22*SA3*dBeta1KanUsual()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.84375D0*CA1*EL*m12squared*SA12*SA22*SA3*&
  &dBeta1KanUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.28125D0*CA1*EL*m12squared*SA12*SA22*SA3*dBeta1KanUsual()*DBLE(SB**INT(-3.D&
  &0)))/(CB2*MW*SW) - (0.28125D0*EL*m12squared*SA3*dBeta1KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.28125&
  &D0*CA22*EL*m12squared*SA3*dBeta1KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.09375D0*EL*m12squared*SA3*d&
  &Beta1KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.09375D0*CA22*EL*m12squared*SA3*dBeta1KanUsual()*DB&
  &LE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.28125D0*EL*m12squared*SA22*SA3*dBeta1KanUsual()*DBLE(CA1**INT(3.D0))&
  &*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.09375D0*EL*m12squared*SA22*SA3*dBeta1KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))&
  &/(CB2*MW*SW) + (0.140625D0*CA3*EL*m12squared*SA2*dBeta1KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.4218&
  &75D0*CA22*CA3*EL*m12squared*SA2*dBeta1KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.046875D0*CA3*EL*m12sq&
  &uared*SA2*dBeta1KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.140625D0*CA22*CA3*EL*m12squared*SA2*dBe&
  &ta1KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.328125D0*CA3*EL*m12squared*SA1*dBeta1KanUsual()*DBLE&
  &(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.421875D0*CA12*CA3*EL*m12squared*SA1*dBeta1KanUsual()*DBLE(SA2**INT(3.D0))*&
  &DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.09375D0*CA3*EL*m12squared*SA1*dBeta1KanUsual()*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(&
  &CB2*MW*SW) + (0.140625D0*CA12*CA3*EL*m12squared*SA1*dBeta1KanUsual()*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - &
  &(0.140625D0*CA3*EL*m12squared*dBeta1KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.04&
  &6875D0*CA3*EL*m12squared*dBeta1KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.187&
  &5D0*CA1*CA3*MH12*SA2*dgAtMZ())/(CB*MW) + (0.5625D0*CA1*CA22*CA3*MH12*SA2*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*CA3*MH32*SA2*dgAtM&
  &Z())/(CB*MW) + (0.28125D0*CA1*CA22*CA3*MH32*SA2*dgAtMZ())/(CB*MW) + (0.28125D0*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*MW) + (0.8&
  &4375D0*CA22*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*MW) - (0.1875D0*CA1*CA3*MH12*SA12*SA2*dgAtMZ())/(CB*MW) - (0.5625D0*CA1*CA22*&
  &CA3*MH12*SA12*SA2*dgAtMZ())/(CB*MW) - (0.09375D0*CA1*CA3*MH32*SA12*SA2*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*CA3*MH32*SA12*S&
  &A2*dgAtMZ())/(CB*MW) + (0.1875D0*CA1*m12squared*SA3*dgAtMZ())/(CB*MW) + (0.1875D0*CA1*CA22*m12squared*SA3*dgAtMZ())/(CB*MW) - &
  &(0.125D0*MH12*SA1*SA3*dgAtMZ())/(CB*MW) - (0.375D0*CA12*MH12*SA1*SA3*dgAtMZ())/(CB*MW) - (0.125D0*CA22*MH12*SA1*SA3*dgAtMZ())/&
  &(CB*MW) - (0.375D0*CA12*CA22*MH12*SA1*SA3*dgAtMZ())/(CB*MW) - (0.0625D0*MH32*SA1*SA3*dgAtMZ())/(CB*MW) - (0.1875D0*CA12*MH32*S&
  &A1*SA3*dgAtMZ())/(CB*MW) - (0.0625D0*CA22*MH32*SA1*SA3*dgAtMZ())/(CB*MW) - (0.1875D0*CA12*CA22*MH32*SA1*SA3*dgAtMZ())/(CB*MW) &
  &- (0.1875D0*CA1*m12squared*SA22*SA3*dgAtMZ())/(CB*MW) + (0.125D0*MH12*SA1*SA22*SA3*dgAtMZ())/(CB*MW) + (0.375D0*CA12*MH12*SA1*&
  &SA22*SA3*dgAtMZ())/(CB*MW) + (0.0625D0*MH32*SA1*SA22*SA3*dgAtMZ())/(CB*MW) + (0.1875D0*CA12*MH32*SA1*SA22*SA3*dgAtMZ())/(CB*MW&
  &) + (0.28125D0*CA1*CA3*m12squared*SA2*dgAtMZ())/(MW*SB) + (0.84375D0*CA1*CA22*CA3*m12squared*SA2*dgAtMZ())/(MW*SB) - (0.1875D0&
  &*CA1*CA3*m12squared*SA2*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA1*CA22*CA3*m12squared*SA2*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*CA3*MH&
  &12*SA1*SA2*dgAtMZ())/(MW*SB) - (0.1875D0*CA12*CA3*MH12*SA1*SA2*dgAtMZ())/(MW*SB) + (0.5625D0*CA22*CA3*MH12*SA1*SA2*dgAtMZ())/(&
  &MW*SB) - (0.5625D0*CA12*CA22*CA3*MH12*SA1*SA2*dgAtMZ())/(MW*SB) + (0.09375D0*CA3*MH32*SA1*SA2*dgAtMZ())/(MW*SB) - (0.09375D0*C&
  &A12*CA3*MH32*SA1*SA2*dgAtMZ())/(MW*SB) + (0.28125D0*CA22*CA3*MH32*SA1*SA2*dgAtMZ())/(MW*SB) - (0.28125D0*CA12*CA22*CA3*MH32*SA&
  &1*SA2*dgAtMZ())/(MW*SB) + (0.28125D0*CA1*CA3*m12squared*SA12*SA2*dgAtMZ())/(CB2*MW*SB) + (0.84375D0*CA1*CA22*CA3*m12squared*SA&
  &12*SA2*dgAtMZ())/(CB2*MW*SB) + (0.125D0*CA1*MH12*SA3*dgAtMZ())/(MW*SB) + (0.125D0*CA1*CA22*MH12*SA3*dgAtMZ())/(MW*SB) + (0.062&
  &5D0*CA1*MH32*SA3*dgAtMZ())/(MW*SB) + (0.0625D0*CA1*CA22*MH32*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*m12squared*SA1*SA3*dgAtMZ())/(M&
  &W*SB) - (0.1875D0*CA22*m12squared*SA1*SA3*dgAtMZ())/(MW*SB) + (0.125D0*m12squared*SA1*SA3*dgAtMZ())/(CB2*MW*SB) + (0.5625D0*CA&
  &12*m12squared*SA1*SA3*dgAtMZ())/(CB2*MW*SB) + (0.125D0*CA22*m12squared*SA1*SA3*dgAtMZ())/(CB2*MW*SB) + (0.5625D0*CA12*CA22*m12&
  &squared*SA1*SA3*dgAtMZ())/(CB2*MW*SB) + (0.375D0*CA1*MH12*SA12*SA3*dgAtMZ())/(MW*SB) + (0.375D0*CA1*CA22*MH12*SA12*SA3*dgAtMZ(&
  &))/(MW*SB) + (0.1875D0*CA1*MH32*SA12*SA3*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*CA22*MH32*SA12*SA3*dgAtMZ())/(MW*SB) - (0.125D0*CA1&
  &*MH12*SA22*SA3*dgAtMZ())/(MW*SB) - (0.0625D0*CA1*MH32*SA22*SA3*dgAtMZ())/(MW*SB) + (0.1875D0*m12squared*SA1*SA22*SA3*dgAtMZ())&
  &/(MW*SB) - (0.125D0*m12squared*SA1*SA22*SA3*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA12*m12squared*SA1*SA22*SA3*dgAtMZ())/(CB2*MW*S&
  &B) - (0.375D0*CA1*MH12*SA12*SA22*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*CA1*MH32*SA12*SA22*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*CA3*m1&
  &2squared*SA1*SA2*dgAtMZ())/(CB*MW*SB2) + (0.28125D0*CA12*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA22*CA3*m12&
  &squared*SA1*SA2*dgAtMZ())/(CB*MW*SB2) + (0.84375D0*CA12*CA22*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*MW*SB2) - (0.125D0*CA1*m12sq&
  &uared*SA3*dgAtMZ())/(CB*MW*SB2) - (0.125D0*CA1*CA22*m12squared*SA3*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA1*m12squared*SA12*SA3*d&
  &gAtMZ())/(CB*MW*SB2) - (0.5625D0*CA1*CA22*m12squared*SA12*SA3*dgAtMZ())/(CB*MW*SB2) + (0.125D0*CA1*m12squared*SA22*SA3*dgAtMZ(&
  &))/(CB*MW*SB2) + (0.5625D0*CA1*m12squared*SA12*SA22*SA3*dgAtMZ())/(CB*MW*SB2) - (0.09375D0*CA3*m12squared*SA1*SA2*dgAtMZ())/(M&
  &W*SB*TB) - (0.28125D0*CA22*CA3*m12squared*SA1*SA2*dgAtMZ())/(MW*SB*TB) - (0.0625D0*CA1*m12squared*SA3*dgAtMZ())/(MW*SB*TB) - (&
  &0.0625D0*CA1*CA22*m12squared*SA3*dgAtMZ())/(MW*SB*TB) + (0.0625D0*CA1*m12squared*SA22*SA3*dgAtMZ())/(MW*SB*TB) - (0.09375D0*CA&
  &1*CA3*m12squared*SA2*TB*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*CA3*m12squared*SA2*TB*dgAtMZ())/(CB*MW) + (0.0625D0*m12squared&
  &*SA1*SA3*TB*dgAtMZ())/(CB*MW) + (0.0625D0*CA22*m12squared*SA1*SA3*TB*dgAtMZ())/(CB*MW) - (0.0625D0*m12squared*SA1*SA22*SA3*TB*&
  &dgAtMZ())/(CB*MW) + (0.0625D0*CA3*MH12*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875D0*CA22*CA3*MH12*SA2*DBLE(CA1**INT(&
  &3.D0))*dgAtMZ())/(CB*MW) + (0.03125D0*CA3*MH32*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA22*CA3*MH32*SA2*DBLE(&
  &CA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.09375D0*CA3*m12squared*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.28125D0*CA22&
  &*CA3*m12squared*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.125D0*MH12*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0&
  &.125D0*CA22*MH12*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.0625D0*MH32*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.0&
  &625D0*CA22*MH32*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.125D0*MH12*SA22*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (&
  &0.0625D0*MH32*SA22*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*m12squared*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW&
  &*SB2) + (0.1875D0*CA22*m12squared*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*m12squared*SA22*SA3*DBLE(CA1**INT&
  &(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.125D0*MH12*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.125D0*CA22*MH12*SA3*DBLE(SA1**IN&
  &T(3.D0))*dgAtMZ())/(CB*MW) + (0.0625D0*MH32*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.0625D0*CA22*MH32*SA3*DBLE(SA1**INT&
  &(3.D0))*dgAtMZ())/(CB*MW) - (0.125D0*MH12*SA22*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*MH32*SA22*SA3*DBLE(SA1**&
  &INT(3.D0))*dgAtMZ())/(CB*MW) + (0.0625D0*CA3*MH12*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA22*CA3*MH12*SA2*DBL&
  &E(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.03125D0*CA3*MH32*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA22*CA3*MH3&
  &2*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.1875D0*m12squared*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875D0&
  &*CA22*m12squared*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*m12squared*SA22*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())&
  &/(CB2*MW*SB) - (0.09375D0*CA3*m12squared*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA22*CA3*m12squared*SA2*D&
  &BLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*CA1*CA3*MH12*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.09375D0*CA1*CA3&
  &*MH32*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.28125D0*CA3*m12squared*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875D&
  &0*CA1*CA3*MH12*SA12*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*CA3*MH32*SA12*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*M&
  &W) - (0.28125D0*CA1*CA3*m12squared*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*CA3*m12squared*DBLE(SA2**INT(3.D0))*&
  &dgAtMZ())/(CB2*MW*SB) - (0.1875D0*CA3*MH12*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA12*CA3*MH12*SA1*DBLE(SA2**&
  &INT(3.D0))*dgAtMZ())/(MW*SB) - (0.09375D0*CA3*MH32*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA12*CA3*MH32*SA1*D&
  &BLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.28125D0*CA1*CA3*m12squared*SA12*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) + (0.187&
  &5D0*CA3*m12squared*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA12*CA3*m12squared*SA1*DBLE(SA2**INT(3.D0))*dg&
  &AtMZ())/(CB*MW*SB2) + (0.09375D0*CA3*m12squared*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB*TB) + (0.09375D0*CA1*CA3*m12squared*&
  &TB*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*CA3*MH12*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (&
  &0.03125D0*CA3*MH32*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA3*m12squared*DBLE(CA1**INT(3.D0)&
  &)*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.0625D0*CA3*MH12*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) &
  &- (0.03125D0*CA3*MH32*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA3*m12squared*DBLE(SA1**INT(3.&
  &D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.28125D0*CA3*EL*SA1*SA2*dm122MSBarUsual())/(CB*MW*SW) + (0.84375D0*CA22*CA&
  &3*EL*SA1*SA2*dm122MSBarUsual())/(CB*MW*SW) + (0.1875D0*CA1*EL*SA3*dm122MSBarUsual())/(CB*MW*SW) + (0.1875D0*CA1*CA22*EL*SA3*dm&
  &122MSBarUsual())/(CB*MW*SW) - (0.1875D0*CA1*EL*SA22*SA3*dm122MSBarUsual())/(CB*MW*SW) + (0.28125D0*CA1*CA3*EL*SA2*dm122MSBarUs&
  &ual())/(MW*SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*SA2*dm122MSBarUsual())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*SA2*dm122MSBarUsual())&
  &/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA22*CA3*EL*SA2*dm122MSBarUsual())/(CB2*MW*SB*SW) + (0.28125D0*CA1*CA3*EL*SA12*SA2*dm122MSBarU&
  &sual())/(CB2*MW*SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*SA12*SA2*dm122MSBarUsual())/(CB2*MW*SB*SW) - (0.1875D0*EL*SA1*SA3*dm122MSB&
  &arUsual())/(MW*SB*SW) - (0.1875D0*CA22*EL*SA1*SA3*dm122MSBarUsual())/(MW*SB*SW) + (0.125D0*EL*SA1*SA3*dm122MSBarUsual())/(CB2*&
  &MW*SB*SW) + (0.5625D0*CA12*EL*SA1*SA3*dm122MSBarUsual())/(CB2*MW*SB*SW) + (0.125D0*CA22*EL*SA1*SA3*dm122MSBarUsual())/(CB2*MW*&
  &SB*SW) + (0.5625D0*CA12*CA22*EL*SA1*SA3*dm122MSBarUsual())/(CB2*MW*SB*SW) + (0.1875D0*EL*SA1*SA22*SA3*dm122MSBarUsual())/(MW*S&
  &B*SW) - (0.125D0*EL*SA1*SA22*SA3*dm122MSBarUsual())/(CB2*MW*SB*SW) - (0.5625D0*CA12*EL*SA1*SA22*SA3*dm122MSBarUsual())/(CB2*MW&
  &*SB*SW) - (0.1875D0*CA3*EL*SA1*SA2*dm122MSBarUsual())/(CB*MW*SB2*SW) + (0.28125D0*CA12*CA3*EL*SA1*SA2*dm122MSBarUsual())/(CB*M&
  &W*SB2*SW) - (0.5625D0*CA22*CA3*EL*SA1*SA2*dm122MSBarUsual())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA22*CA3*EL*SA1*SA2*dm122MSBarUs&
  &ual())/(CB*MW*SB2*SW) - (0.125D0*CA1*EL*SA3*dm122MSBarUsual())/(CB*MW*SB2*SW) - (0.125D0*CA1*CA22*EL*SA3*dm122MSBarUsual())/(C&
  &B*MW*SB2*SW) - (0.5625D0*CA1*EL*SA12*SA3*dm122MSBarUsual())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*EL*SA12*SA3*dm122MSBarUsual())&
  &/(CB*MW*SB2*SW) + (0.125D0*CA1*EL*SA22*SA3*dm122MSBarUsual())/(CB*MW*SB2*SW) + (0.5625D0*CA1*EL*SA12*SA22*SA3*dm122MSBarUsual(&
  &))/(CB*MW*SB2*SW) - (0.09375D0*CA3*EL*SA1*SA2*dm122MSBarUsual())/(MW*SB*SW*TB) - (0.28125D0*CA22*CA3*EL*SA1*SA2*dm122MSBarUsua&
  &l())/(MW*SB*SW*TB) - (0.0625D0*CA1*EL*SA3*dm122MSBarUsual())/(MW*SB*SW*TB) - (0.0625D0*CA1*CA22*EL*SA3*dm122MSBarUsual())/(MW*&
  &SB*SW*TB) + (0.0625D0*CA1*EL*SA22*SA3*dm122MSBarUsual())/(MW*SB*SW*TB) - (0.09375D0*CA1*CA3*EL*SA2*TB*dm122MSBarUsual())/(CB*M&
  &W*SW) - (0.28125D0*CA1*CA22*CA3*EL*SA2*TB*dm122MSBarUsual())/(CB*MW*SW) + (0.0625D0*EL*SA1*SA3*TB*dm122MSBarUsual())/(CB*MW*SW&
  &) + (0.0625D0*CA22*EL*SA1*SA3*TB*dm122MSBarUsual())/(CB*MW*SW) - (0.0625D0*EL*SA1*SA22*SA3*TB*dm122MSBarUsual())/(CB*MW*SW) - &
  &(0.09375D0*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dm122MSBarUsual())/(CB2*MW*SB*SW) - (0.28125D0*CA22*CA3*EL*SA2*DBLE(CA1**INT(3.D0))&
  &*dm122MSBarUsual())/(CB2*MW*SB*SW) + (0.1875D0*EL*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SB2*SW) + (0.1875D0*CA22*&
  &EL*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SB2*SW) - (0.1875D0*EL*SA22*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarUsual())/&
  &(CB*MW*SB2*SW) - (0.1875D0*EL*SA3*DBLE(SA1**INT(3.D0))*dm122MSBarUsual())/(CB2*MW*SB*SW) - (0.1875D0*CA22*EL*SA3*DBLE(SA1**INT&
  &(3.D0))*dm122MSBarUsual())/(CB2*MW*SB*SW) + (0.1875D0*EL*SA22*SA3*DBLE(SA1**INT(3.D0))*dm122MSBarUsual())/(CB2*MW*SB*SW) - (0.&
  &09375D0*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SB2*SW) - (0.28125D0*CA22*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*dm&
  &122MSBarUsual())/(CB*MW*SB2*SW) - (0.28125D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SW) - (0.28125D0*CA1*CA&
  &3*EL*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(CB2*MW&
  &*SB*SW) - (0.28125D0*CA1*CA3*EL*SA12*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*SA1*DBLE(SA2**I&
  &NT(3.D0))*dm122MSBarUsual())/(CB*MW*SB2*SW) - (0.28125D0*CA12*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SB2*SW&
  &) + (0.09375D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(MW*SB*SW*TB) + (0.09375D0*CA1*CA3*EL*TB*DBLE(SA2**INT(3.D0)&
  &)*dm122MSBarUsual())/(CB*MW*SW) + (0.09375D0*CA3*EL*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(CB2*MW*SB*SW&
  &) + (0.09375D0*CA3*EL*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SB2*SW) + (0.1875D0*CA1*CA3*EL*SA2*d&
  &MH12OSUsual())/(CB*MW*SW) + (0.5625D0*CA1*CA22*CA3*EL*SA2*dMH12OSUsual())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*SA12*SA2*dMH12OSUs&
  &ual())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*EL*SA12*SA2*dMH12OSUsual())/(CB*MW*SW) - (0.125D0*EL*SA1*SA3*dMH12OSUsual())/(CB*MW&
  &*SW) - (0.375D0*CA12*EL*SA1*SA3*dMH12OSUsual())/(CB*MW*SW) - (0.125D0*CA22*EL*SA1*SA3*dMH12OSUsual())/(CB*MW*SW) - (0.375D0*CA&
  &12*CA22*EL*SA1*SA3*dMH12OSUsual())/(CB*MW*SW) + (0.125D0*EL*SA1*SA22*SA3*dMH12OSUsual())/(CB*MW*SW) + (0.375D0*CA12*EL*SA1*SA2&
  &2*SA3*dMH12OSUsual())/(CB*MW*SW) + (0.1875D0*CA3*EL*SA1*SA2*dMH12OSUsual())/(MW*SB*SW) - (0.1875D0*CA12*CA3*EL*SA1*SA2*dMH12OS&
  &Usual())/(MW*SB*SW) + (0.5625D0*CA22*CA3*EL*SA1*SA2*dMH12OSUsual())/(MW*SB*SW) - (0.5625D0*CA12*CA22*CA3*EL*SA1*SA2*dMH12OSUsu&
  &al())/(MW*SB*SW) + (0.125D0*CA1*EL*SA3*dMH12OSUsual())/(MW*SB*SW) + (0.125D0*CA1*CA22*EL*SA3*dMH12OSUsual())/(MW*SB*SW) + (0.3&
  &75D0*CA1*EL*SA12*SA3*dMH12OSUsual())/(MW*SB*SW) + (0.375D0*CA1*CA22*EL*SA12*SA3*dMH12OSUsual())/(MW*SB*SW) - (0.125D0*CA1*EL*S&
  &A22*SA3*dMH12OSUsual())/(MW*SB*SW) - (0.375D0*CA1*EL*SA12*SA22*SA3*dMH12OSUsual())/(MW*SB*SW) - (0.5D0*CA2*CA3*dMH12OSUsual())&
  &/vS - (1.5D0*CA2*CA3*SA22*dMH12OSUsual())/vS + (0.0625D0*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dMH12OSUsual())/(CB*MW*SW) + (0.1875D&
  &0*CA22*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dMH12OSUsual())/(CB*MW*SW) - (0.125D0*EL*SA3*DBLE(CA1**INT(3.D0))*dMH12OSUsual())/(MW*S&
  &B*SW) - (0.125D0*CA22*EL*SA3*DBLE(CA1**INT(3.D0))*dMH12OSUsual())/(MW*SB*SW) + (0.125D0*EL*SA22*SA3*DBLE(CA1**INT(3.D0))*dMH12&
  &OSUsual())/(MW*SB*SW) + (0.5D0*CA3*DBLE(CA2**INT(3.D0))*dMH12OSUsual())/vS + (0.125D0*EL*SA3*DBLE(SA1**INT(3.D0))*dMH12OSUsual&
  &())/(CB*MW*SW) + (0.125D0*CA22*EL*SA3*DBLE(SA1**INT(3.D0))*dMH12OSUsual())/(CB*MW*SW) - (0.125D0*EL*SA22*SA3*DBLE(SA1**INT(3.D&
  &0))*dMH12OSUsual())/(CB*MW*SW) + (0.0625D0*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*dMH12OSUsual())/(MW*SB*SW) + (0.1875D0*CA22*CA3*EL*&
  &SA2*DBLE(SA1**INT(3.D0))*dMH12OSUsual())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*DBLE(SA2**INT(3.D0))*dMH12OSUsual())/(CB*MW*SW) + (&
  &0.1875D0*CA1*CA3*EL*SA12*DBLE(SA2**INT(3.D0))*dMH12OSUsual())/(CB*MW*SW) - (0.1875D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dMH12OSUs&
  &ual())/(MW*SB*SW) + (0.1875D0*CA12*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dMH12OSUsual())/(MW*SB*SW) - (0.0625D0*CA3*EL*DBLE(CA1**INT&
  &(3.D0))*DBLE(SA2**INT(3.D0))*dMH12OSUsual())/(CB*MW*SW) - (0.0625D0*CA3*EL*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH12OSUs&
  &ual())/(MW*SB*SW) + (0.09375D0*CA1*CA3*EL*SA2*dMH32OSUsual())/(CB*MW*SW) + (0.28125D0*CA1*CA22*CA3*EL*SA2*dMH32OSUsual())/(CB*&
  &MW*SW) - (0.09375D0*CA1*CA3*EL*SA12*SA2*dMH32OSUsual())/(CB*MW*SW) - (0.28125D0*CA1*CA22*CA3*EL*SA12*SA2*dMH32OSUsual())/(CB*M&
  &W*SW) - (0.0625D0*EL*SA1*SA3*dMH32OSUsual())/(CB*MW*SW) - (0.1875D0*CA12*EL*SA1*SA3*dMH32OSUsual())/(CB*MW*SW) - (0.0625D0*CA2&
  &2*EL*SA1*SA3*dMH32OSUsual())/(CB*MW*SW) - (0.1875D0*CA12*CA22*EL*SA1*SA3*dMH32OSUsual())/(CB*MW*SW) + (0.0625D0*EL*SA1*SA22*SA&
  &3*dMH32OSUsual())/(CB*MW*SW) + (0.1875D0*CA12*EL*SA1*SA22*SA3*dMH32OSUsual())/(CB*MW*SW) + (0.09375D0*CA3*EL*SA1*SA2*dMH32OSUs&
  &ual())/(MW*SB*SW) - (0.09375D0*CA12*CA3*EL*SA1*SA2*dMH32OSUsual())/(MW*SB*SW) + (0.28125D0*CA22*CA3*EL*SA1*SA2*dMH32OSUsual())&
  &/(MW*SB*SW) - (0.28125D0*CA12*CA22*CA3*EL*SA1*SA2*dMH32OSUsual())/(MW*SB*SW) + (0.0625D0*CA1*EL*SA3*dMH32OSUsual())/(MW*SB*SW)&
  & + (0.0625D0*CA1*CA22*EL*SA3*dMH32OSUsual())/(MW*SB*SW) + (0.1875D0*CA1*EL*SA12*SA3*dMH32OSUsual())/(MW*SB*SW) + (0.1875D0*CA1&
  &*CA22*EL*SA12*SA3*dMH32OSUsual())/(MW*SB*SW) - (0.0625D0*CA1*EL*SA22*SA3*dMH32OSUsual())/(MW*SB*SW) - (0.1875D0*CA1*EL*SA12*SA&
  &22*SA3*dMH32OSUsual())/(MW*SB*SW) - (0.25D0*CA2*CA3*dMH32OSUsual())/vS - (0.75D0*CA2*CA3*SA22*dMH32OSUsual())/vS + (0.03125D0*&
  &CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dMH32OSUsual())/(CB*MW*SW) + (0.09375D0*CA22*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dMH32OSUsual())/(&
  &CB*MW*SW) - (0.0625D0*EL*SA3*DBLE(CA1**INT(3.D0))*dMH32OSUsual())/(MW*SB*SW) - (0.0625D0*CA22*EL*SA3*DBLE(CA1**INT(3.D0))*dMH3&
  &2OSUsual())/(MW*SB*SW) + (0.0625D0*EL*SA22*SA3*DBLE(CA1**INT(3.D0))*dMH32OSUsual())/(MW*SB*SW) + (0.25D0*CA3*DBLE(CA2**INT(3.D&
  &0))*dMH32OSUsual())/vS + (0.0625D0*EL*SA3*DBLE(SA1**INT(3.D0))*dMH32OSUsual())/(CB*MW*SW) + (0.0625D0*CA22*EL*SA3*DBLE(SA1**IN&
  &T(3.D0))*dMH32OSUsual())/(CB*MW*SW) - (0.0625D0*EL*SA22*SA3*DBLE(SA1**INT(3.D0))*dMH32OSUsual())/(CB*MW*SW) + (0.03125D0*CA3*E&
  &L*SA2*DBLE(SA1**INT(3.D0))*dMH32OSUsual())/(MW*SB*SW) + (0.09375D0*CA22*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*dMH32OSUsual())/(MW*SB&
  &*SW) - (0.09375D0*CA1*CA3*EL*DBLE(SA2**INT(3.D0))*dMH32OSUsual())/(CB*MW*SW) + (0.09375D0*CA1*CA3*EL*SA12*DBLE(SA2**INT(3.D0))&
  &*dMH32OSUsual())/(CB*MW*SW) - (0.09375D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dMH32OSUsual())/(MW*SB*SW) + (0.09375D0*CA12*CA3*EL*S&
  &A1*DBLE(SA2**INT(3.D0))*dMH32OSUsual())/(MW*SB*SW) - (0.03125D0*CA3*EL*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH32OSUsual(&
  &))/(CB*MW*SW) - (0.03125D0*CA3*EL*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH32OSUsual())/(MW*SB*SW) - (0.09375D0*CA1*CA3*EL&
  &*MH12*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.28125D0*CA1*CA22*CA3*EL*MH12*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(C&
  &B*SW) - (0.046875D0*CA1*CA3*EL*MH32*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.140625D0*CA1*CA22*CA3*EL*MH32*SA2*DBLE(&
  &MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.140625D0*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.42&
  &1875D0*CA22*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.09375D0*CA1*CA3*EL*MH12*SA12*SA2*DBLE(MW*&
  &*INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.28125D0*CA1*CA22*CA3*EL*MH12*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.04&
  &6875D0*CA1*CA3*EL*MH32*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.140625D0*CA1*CA22*CA3*EL*MH32*SA12*SA2*DBLE(MW*&
  &*INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.09375D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.09375D0*CA&
  &1*CA22*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.0625D0*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual(&
  &))/(CB*SW) + (0.1875D0*CA12*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.0625D0*CA22*EL*MH12*SA1*SA3*DBLE(MW&
  &**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.1875D0*CA12*CA22*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.03125D&
  &0*EL*MH32*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.09375D0*CA12*EL*MH32*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual()&
  &)/(CB*SW) + (0.03125D0*CA22*EL*MH32*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.09375D0*CA12*CA22*EL*MH32*SA1*SA3*D&
  &BLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.09375D0*CA1*EL*m12squared*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (&
  &0.0625D0*EL*MH12*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.1875D0*CA12*EL*MH12*SA1*SA22*SA3*DBLE(MW**INT(-3.&
  &D0))*dMW2Usual())/(CB*SW) - (0.03125D0*EL*MH32*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.09375D0*CA12*EL*MH3&
  &2*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.140625D0*CA1*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*dMW2Usua&
  &l())/(SB*SW) - (0.421875D0*CA1*CA22*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.09375D0*CA1*CA3*EL*m1&
  &2squared*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) + (0.28125D0*CA1*CA22*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*dM&
  &W2Usual())/(CB2*SB*SW) - (0.09375D0*CA3*EL*MH12*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.09375D0*CA12*CA3*EL*MH1&
  &2*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.28125D0*CA22*CA3*EL*MH12*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(S&
  &B*SW) + (0.28125D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.046875D0*CA3*EL*MH32*SA1*SA2*D&
  &BLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.046875D0*CA12*CA3*EL*MH32*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0&
  &.140625D0*CA22*CA3*EL*MH32*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.140625D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*DBLE(&
  &MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.140625D0*CA1*CA3*EL*m12squared*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW&
  &) - (0.421875D0*CA1*CA22*CA3*EL*m12squared*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) - (0.0625D0*CA1*EL*MH12*SA3*&
  &DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.0625D0*CA1*CA22*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.0312&
  &5D0*CA1*EL*MH32*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.03125D0*CA1*CA22*EL*MH32*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual&
  &())/(SB*SW) + (0.09375D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.09375D0*CA22*EL*m12squared*SA1*S&
  &A3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.0625D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) -&
  & (0.28125D0*CA12*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) - (0.0625D0*CA22*EL*m12squared*SA1*SA3*DB&
  &LE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) - (0.28125D0*CA12*CA22*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB&
  &2*SB*SW) - (0.1875D0*CA1*EL*MH12*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.1875D0*CA1*CA22*EL*MH12*SA12*SA3*DBLE&
  &(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.09375D0*CA1*EL*MH32*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.09375D0&
  &*CA1*CA22*EL*MH32*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.0625D0*CA1*EL*MH12*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW&
  &2Usual())/(SB*SW) + (0.03125D0*CA1*EL*MH32*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.09375D0*EL*m12squared*SA1*S&
  &A22*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.0625D0*EL*m12squared*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB&
  &2*SB*SW) + (0.28125D0*CA12*EL*m12squared*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) + (0.1875D0*CA1*EL*MH12*SA&
  &12*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.09375D0*CA1*EL*MH32*SA12*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())&
  &/(SB*SW) + (0.09375D0*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) - (0.140625D0*CA12*CA3*EL*m12squ&
  &ared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.28125D0*CA22*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dM&
  &W2Usual())/(CB*SB2*SW) - (0.421875D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.06&
  &25D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.0625D0*CA1*CA22*EL*m12squared*SA3*DBLE(MW**INT(-&
  &3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.28125D0*CA1*EL*m12squared*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.281&
  &25D0*CA1*CA22*EL*m12squared*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) - (0.0625D0*CA1*EL*m12squared*SA22*SA3*DBLE&
  &(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) - (0.28125D0*CA1*EL*m12squared*SA12*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*S&
  &B2*SW) + (0.046875D0*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW*TB) + (0.140625D0*CA22*CA3*EL*m12squar&
  &ed*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW*TB) + (0.03125D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(&
  &SB*SW*TB) + (0.03125D0*CA1*CA22*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW*TB) - (0.03125D0*CA1*EL*m12squared*&
  &SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW*TB) + (0.046875D0*CA1*CA3*EL*m12squared*SA2*TB*DBLE(MW**INT(-3.D0))*dMW2Usua&
  &l())/(CB*SW) + (0.140625D0*CA1*CA22*CA3*EL*m12squared*SA2*TB*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.03125D0*EL*m12squa&
  &red*SA1*SA3*TB*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.03125D0*CA22*EL*m12squared*SA1*SA3*TB*DBLE(MW**INT(-3.D0))*dMW2U&
  &sual())/(CB*SW) + (0.03125D0*EL*m12squared*SA1*SA22*SA3*TB*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.03125D0*CA3*EL*MH12*&
  &SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.09375D0*CA22*CA3*EL*MH12*SA2*DBLE(CA1**INT(3.D0))*DBLE&
  &(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.015625D0*CA3*EL*MH32*SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB&
  &*SW) - (0.046875D0*CA22*CA3*EL*MH32*SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.046875D0*CA3*EL*m1&
  &2squared*SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) + (0.140625D0*CA22*CA3*EL*m12squared*SA2*DBLE(&
  &CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) + (0.0625D0*EL*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0)&
  &)*dMW2Usual())/(SB*SW) + (0.0625D0*CA22*EL*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.03125D&
  &0*EL*MH32*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.03125D0*CA22*EL*MH32*SA3*DBLE(CA1**INT(3.D0)&
  &)*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.0625D0*EL*MH12*SA22*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual()&
  &)/(SB*SW) - (0.03125D0*EL*MH32*SA22*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.09375D0*EL*m12squa&
  &red*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) - (0.09375D0*CA22*EL*m12squared*SA3*DBLE(CA1**INT(3&
  &.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.09375D0*EL*m12squared*SA22*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D&
  &0))*dMW2Usual())/(CB*SB2*SW) - (0.0625D0*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB*SW) - (0.0625D&
  &0*CA22*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB*SW) - (0.03125D0*EL*MH32*SA3*DBLE(MW**INT(-3.D0)&
  &)*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB*SW) - (0.03125D0*CA22*EL*MH32*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual(&
  &))/(CB*SW) + (0.0625D0*EL*MH12*SA22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB*SW) + (0.03125D0*EL*MH32*SA&
  &22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB*SW) - (0.03125D0*CA3*EL*MH12*SA2*DBLE(MW**INT(-3.D0))*DBLE(S&
  &A1**INT(3.D0))*dMW2Usual())/(SB*SW) - (0.09375D0*CA22*CA3*EL*MH12*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(&
  &SB*SW) - (0.015625D0*CA3*EL*MH32*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(SB*SW) - (0.046875D0*CA22*CA3*EL*&
  &MH32*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(SB*SW) + (0.09375D0*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*DB&
  &LE(SA1**INT(3.D0))*dMW2Usual())/(CB2*SB*SW) + (0.09375D0*CA22*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2&
  &Usual())/(CB2*SB*SW) - (0.09375D0*EL*m12squared*SA22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB2*SB*SW) + &
  &(0.046875D0*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.140625D0*CA22*CA3*EL&
  &*m12squared*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.09375D0*CA1*CA3*EL*MH12*DBLE(MW**INT(-&
  &3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SW) + (0.046875D0*CA1*CA3*EL*MH32*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2U&
  &sual())/(CB*SW) + (0.140625D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SW) - (0.09375D&
  &0*CA1*CA3*EL*MH12*SA12*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SW) - (0.046875D0*CA1*CA3*EL*MH32*SA12*DBLE(&
  &MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SW) + (0.140625D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*DBLE(SA2**I&
  &NT(3.D0))*dMW2Usual())/(SB*SW) - (0.09375D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB2*&
  &SB*SW) + (0.09375D0*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(SB*SW) - (0.09375D0*CA12*CA3*EL*MH&
  &12*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(SB*SW) + (0.046875D0*CA3*EL*MH32*SA1*DBLE(MW**INT(-3.D0))*DBLE(&
  &SA2**INT(3.D0))*dMW2Usual())/(SB*SW) - (0.046875D0*CA12*CA3*EL*MH32*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())&
  &/(SB*SW) + (0.140625D0*CA1*CA3*EL*m12squared*SA12*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB2*SB*SW) - (0.0937&
  &5D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.140625D0*CA12*CA3*EL*m12squa&
  &red*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SB2*SW) - (0.046875D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3&
  &.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(SB*SW*TB) - (0.046875D0*CA1*CA3*EL*m12squared*TB*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3&
  &.D0))*dMW2Usual())/(CB*SW) + (0.03125D0*CA3*EL*MH12*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual()&
  &)/(CB*SW) + (0.015625D0*CA3*EL*MH32*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SW) - (0.0&
  &46875D0*CA3*EL*m12squared*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB2*SB*SW) + (0.03125D0&
  &*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(SB*SW) + (0.015625D0*CA3*EL*MH32*DBL&
  &E(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(SB*SW) - (0.046875D0*CA3*EL*m12squared*DBLE(MW**INT(&
  &-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SB2*SW) + 0.5D0*CA2*CA3*MH12*DBLE(vS**INT(-2.D0))*dvSMSBarU&
  &sual() + 0.25D0*CA2*CA3*MH32*DBLE(vS**INT(-2.D0))*dvSMSBarUsual() + 1.5D0*CA2*CA3*MH12*SA22*DBLE(vS**INT(-2.D0))*dvSMSBarUsual&
  &() + 0.75D0*CA2*CA3*MH32*SA22*DBLE(vS**INT(-2.D0))*dvSMSBarUsual() - 0.5D0*CA3*MH12*DBLE(CA2**INT(3.D0))*DBLE(vS**INT(-2.D0))*&
  &dvSMSBarUsual() - 0.25D0*CA3*MH32*DBLE(CA2**INT(3.D0))*DBLE(vS**INT(-2.D0))*dvSMSBarUsual() &
		& + CS1S1S1f111*dZH1H3OSUsual()/2D0 + CS1S1S1f211*dZH2H3OSUsual()/2D0 + CS1S1S1f311*dZH3H3OS()/2D0 &
		& + CS1S1S1f131*dZH1H1OS()/2D0 + CS1S1S1f231*dZH2H1OSUsual()/2D0 + CS1S1S1f331*dZH3H1OSUsual()/2D0 &
		& + CS1S1S1f131*dZH1H1OS()/2D0 + CS1S1S1f231*dZH2H1OSUsual()/2D0 + CS1S1S1f331*dZH3H1OSUsual()/2D0 &
		& )
	case (2)
		dRR11 = -CA2*SA1*dAlpha1KanUsual() - CA1*SA2*dAlpha2KanUsual()
		dRR12 = CA1*CA2*dAlpha1KanUsual() - SA1*SA2*dAlpha2KanUsual()
		dRR13 = CA2*dAlpha2KanUsual()
		dRR21 = -CA1*CA3*dAlpha1KanUsual() - CA1*CA3*SA2*dAlpha3KanUsual() - CA1*CA2*SA3*dAlpha2KanUsual() + &
			& SA1*SA3*dAlpha3KanUsual() + SA1*SA2*SA3*dAlpha1KanUsual()
		dRR22 = -CA3*SA1*dAlpha1KanUsual() - CA3*SA1*SA2*dAlpha3KanUsual() - CA1*SA3*dAlpha3KanUsual() - &
			& CA2*SA1*SA3*dAlpha2KanUsual() - CA1*SA2*SA3*dAlpha1KanUsual()
		dRR23 = CA2*CA3*dAlpha3KanUsual() - SA2*SA3*dAlpha2KanUsual()
		dRR31 = -CA1*CA2*CA3*dAlpha2KanUsual() + CA3*SA1*dAlpha3KanUsual() + CA3*SA1*SA2*dAlpha1KanUsual() + &
			& CA1*SA3*dAlpha1KanUsual() + CA1*SA2*SA3*dAlpha3KanUsual()
		dRR32 = -CA1*CA3*dAlpha3KanUsual() - CA2*CA3*SA1*dAlpha2KanUsual() - CA1*CA3*SA2*dAlpha1KanUsual() + &
			& SA1*SA3*dAlpha1KanUsual() + SA1*SA2*SA3*dAlpha3KanUsual()
		dRR33 = -CA3*SA2*dAlpha2KanUsual() - CA2*SA3*dAlpha3KanUsual()

		totalAmplitude = CS1S1S1f311*( &
&(0.28125D0*CA1*CA3*EL*m12squared*SA2*dAlpha1KanUsual())/(CB*MW*SW) + (0.84375D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha1KanUsual())&
  &/(CB*MW*SW) - (0.1875D0*CA3*EL*MH12*SA1*SA2*dAlpha1KanUsual())/(CB*MW*SW) - (0.5625D0*CA12*CA3*EL*MH12*SA1*SA2*dAlpha1KanUsual&
  &())/(CB*MW*SW) - (0.5625D0*CA22*CA3*EL*MH12*SA1*SA2*dAlpha1KanUsual())/(CB*MW*SW) - (1.6875D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dA&
  &lpha1KanUsual())/(CB*MW*SW) - (0.09375D0*CA3*EL*MH32*SA1*SA2*dAlpha1KanUsual())/(CB*MW*SW) - (0.28125D0*CA12*CA3*EL*MH32*SA1*S&
  &A2*dAlpha1KanUsual())/(CB*MW*SW) - (0.28125D0*CA22*CA3*EL*MH32*SA1*SA2*dAlpha1KanUsual())/(CB*MW*SW) - (0.84375D0*CA12*CA22*CA&
  &3*EL*MH32*SA1*SA2*dAlpha1KanUsual())/(CB*MW*SW) - (0.125D0*CA1*EL*MH12*SA3*dAlpha1KanUsual())/(CB*MW*SW) - (0.125D0*CA1*CA22*E&
  &L*MH12*SA3*dAlpha1KanUsual())/(CB*MW*SW) - (0.0625D0*CA1*EL*MH32*SA3*dAlpha1KanUsual())/(CB*MW*SW) - (0.0625D0*CA1*CA22*EL*MH3&
  &2*SA3*dAlpha1KanUsual())/(CB*MW*SW) - (0.1875D0*EL*m12squared*SA1*SA3*dAlpha1KanUsual())/(CB*MW*SW) - (0.1875D0*CA22*EL*m12squ&
  &ared*SA1*SA3*dAlpha1KanUsual())/(CB*MW*SW) + (1.125D0*CA1*EL*MH12*SA12*SA3*dAlpha1KanUsual())/(CB*MW*SW) + (1.125D0*CA1*CA22*E&
  &L*MH12*SA12*SA3*dAlpha1KanUsual())/(CB*MW*SW) + (0.5625D0*CA1*EL*MH32*SA12*SA3*dAlpha1KanUsual())/(CB*MW*SW) + (0.5625D0*CA1*C&
  &A22*EL*MH32*SA12*SA3*dAlpha1KanUsual())/(CB*MW*SW) + (0.125D0*CA1*EL*MH12*SA22*SA3*dAlpha1KanUsual())/(CB*MW*SW) + (0.0625D0*C&
  &A1*EL*MH32*SA22*SA3*dAlpha1KanUsual())/(CB*MW*SW) + (0.1875D0*EL*m12squared*SA1*SA22*SA3*dAlpha1KanUsual())/(CB*MW*SW) - (1.12&
  &5D0*CA1*EL*MH12*SA12*SA22*SA3*dAlpha1KanUsual())/(CB*MW*SW) - (0.5625D0*CA1*EL*MH32*SA12*SA22*SA3*dAlpha1KanUsual())/(CB*MW*SW&
  &) + (0.1875D0*CA1*CA3*EL*MH12*SA2*dAlpha1KanUsual())/(MW*SB*SW) + (0.5625D0*CA1*CA22*CA3*EL*MH12*SA2*dAlpha1KanUsual())/(MW*SB&
  &*SW) + (0.09375D0*CA1*CA3*EL*MH32*SA2*dAlpha1KanUsual())/(MW*SB*SW) + (0.28125D0*CA1*CA22*CA3*EL*MH32*SA2*dAlpha1KanUsual())/(&
  &MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*SA1*SA2*dAlpha1KanUsual())/(MW*SB*SW) - (0.84375D0*CA22*CA3*EL*m12squared*SA1*SA2*dAl&
  &pha1KanUsual())/(MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA1*SA2*dAlpha1KanUsual())/(CB2*MW*SB*SW) + (0.84375D0*CA12*CA3*EL*m1&
  &2squared*SA1*SA2*dAlpha1KanUsual())/(CB2*MW*SB*SW) + (0.5625D0*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha1KanUsual())/(CB2*MW*SB*SW&
  &) + (2.53125D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha1KanUsual())/(CB2*MW*SB*SW) + (0.5625D0*CA1*CA3*EL*MH12*SA12*SA2*dAlp&
  &ha1KanUsual())/(MW*SB*SW) + (1.6875D0*CA1*CA22*CA3*EL*MH12*SA12*SA2*dAlpha1KanUsual())/(MW*SB*SW) + (0.28125D0*CA1*CA3*EL*MH32&
  &*SA12*SA2*dAlpha1KanUsual())/(MW*SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*MH32*SA12*SA2*dAlpha1KanUsual())/(MW*SB*SW) - (0.1875D0*C&
  &A1*EL*m12squared*SA3*dAlpha1KanUsual())/(MW*SB*SW) - (0.1875D0*CA1*CA22*EL*m12squared*SA3*dAlpha1KanUsual())/(MW*SB*SW) + (0.1&
  &25D0*CA1*EL*m12squared*SA3*dAlpha1KanUsual())/(CB2*MW*SB*SW) + (0.125D0*CA1*CA22*EL*m12squared*SA3*dAlpha1KanUsual())/(CB2*MW*&
  &SB*SW) - (0.125D0*EL*MH12*SA1*SA3*dAlpha1KanUsual())/(MW*SB*SW) + (1.125D0*CA12*EL*MH12*SA1*SA3*dAlpha1KanUsual())/(MW*SB*SW) &
  &- (0.125D0*CA22*EL*MH12*SA1*SA3*dAlpha1KanUsual())/(MW*SB*SW) + (1.125D0*CA12*CA22*EL*MH12*SA1*SA3*dAlpha1KanUsual())/(MW*SB*S&
  &W) - (0.0625D0*EL*MH32*SA1*SA3*dAlpha1KanUsual())/(MW*SB*SW) + (0.5625D0*CA12*EL*MH32*SA1*SA3*dAlpha1KanUsual())/(MW*SB*SW) - &
  &(0.0625D0*CA22*EL*MH32*SA1*SA3*dAlpha1KanUsual())/(MW*SB*SW) + (0.5625D0*CA12*CA22*EL*MH32*SA1*SA3*dAlpha1KanUsual())/(MW*SB*S&
  &W) - (1.6875D0*CA1*EL*m12squared*SA12*SA3*dAlpha1KanUsual())/(CB2*MW*SB*SW) - (1.6875D0*CA1*CA22*EL*m12squared*SA12*SA3*dAlpha&
  &1KanUsual())/(CB2*MW*SB*SW) + (0.1875D0*CA1*EL*m12squared*SA22*SA3*dAlpha1KanUsual())/(MW*SB*SW) - (0.125D0*CA1*EL*m12squared*&
  &SA22*SA3*dAlpha1KanUsual())/(CB2*MW*SB*SW) + (0.125D0*EL*MH12*SA1*SA22*SA3*dAlpha1KanUsual())/(MW*SB*SW) - (1.125D0*CA12*EL*MH&
  &12*SA1*SA22*SA3*dAlpha1KanUsual())/(MW*SB*SW) + (0.0625D0*EL*MH32*SA1*SA22*SA3*dAlpha1KanUsual())/(MW*SB*SW) - (0.5625D0*CA12*&
  &EL*MH32*SA1*SA22*SA3*dAlpha1KanUsual())/(MW*SB*SW) + (1.6875D0*CA1*EL*m12squared*SA12*SA22*SA3*dAlpha1KanUsual())/(CB2*MW*SB*S&
  &W) - (0.1875D0*CA1*CA3*EL*m12squared*SA2*dAlpha1KanUsual())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha1K&
  &anUsual())/(CB*MW*SB2*SW) - (0.84375D0*CA1*CA3*EL*m12squared*SA12*SA2*dAlpha1KanUsual())/(CB*MW*SB2*SW) - (2.53125D0*CA1*CA22*&
  &CA3*EL*m12squared*SA12*SA2*dAlpha1KanUsual())/(CB*MW*SB2*SW) + (0.125D0*EL*m12squared*SA1*SA3*dAlpha1KanUsual())/(CB*MW*SB2*SW&
  &) - (1.6875D0*CA12*EL*m12squared*SA1*SA3*dAlpha1KanUsual())/(CB*MW*SB2*SW) + (0.125D0*CA22*EL*m12squared*SA1*SA3*dAlpha1KanUsu&
  &al())/(CB*MW*SB2*SW) - (1.6875D0*CA12*CA22*EL*m12squared*SA1*SA3*dAlpha1KanUsual())/(CB*MW*SB2*SW) - (0.125D0*EL*m12squared*SA&
  &1*SA22*SA3*dAlpha1KanUsual())/(CB*MW*SB2*SW) + (1.6875D0*CA12*EL*m12squared*SA1*SA22*SA3*dAlpha1KanUsual())/(CB*MW*SB2*SW) - (&
  &0.09375D0*CA1*CA3*EL*m12squared*SA2*dAlpha1KanUsual())/(MW*SB*SW*TB) - (0.28125D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha1KanUsu&
  &al())/(MW*SB*SW*TB) + (0.0625D0*EL*m12squared*SA1*SA3*dAlpha1KanUsual())/(MW*SB*SW*TB) + (0.0625D0*CA22*EL*m12squared*SA1*SA3*&
  &dAlpha1KanUsual())/(MW*SB*SW*TB) - (0.0625D0*EL*m12squared*SA1*SA22*SA3*dAlpha1KanUsual())/(MW*SB*SW*TB) + (0.09375D0*CA3*EL*m&
  &12squared*SA1*SA2*TB*dAlpha1KanUsual())/(CB*MW*SW) + (0.28125D0*CA22*CA3*EL*m12squared*SA1*SA2*TB*dAlpha1KanUsual())/(CB*MW*SW&
  &) + (0.0625D0*CA1*EL*m12squared*SA3*TB*dAlpha1KanUsual())/(CB*MW*SW) + (0.0625D0*CA1*CA22*EL*m12squared*SA3*TB*dAlpha1KanUsual&
  &())/(CB*MW*SW) - (0.0625D0*CA1*EL*m12squared*SA22*SA3*TB*dAlpha1KanUsual())/(CB*MW*SW) + (0.1875D0*CA1*CA2*CA3*EL*MH12*dAlpha2&
  &KanUsual())/(CB*MW*SW) + (0.09375D0*CA1*CA2*CA3*EL*MH32*dAlpha2KanUsual())/(CB*MW*SW) + (0.28125D0*CA2*CA3*EL*m12squared*SA1*d&
  &Alpha2KanUsual())/(CB*MW*SW) - (0.1875D0*CA1*CA2*CA3*EL*MH12*SA12*dAlpha2KanUsual())/(CB*MW*SW) - (0.09375D0*CA1*CA2*CA3*EL*MH&
  &32*SA12*dAlpha2KanUsual())/(CB*MW*SW) - (1.6875D0*CA1*CA2*CA3*EL*MH12*SA22*dAlpha2KanUsual())/(CB*MW*SW) - (0.84375D0*CA1*CA2*&
  &CA3*EL*MH32*SA22*dAlpha2KanUsual())/(CB*MW*SW) - (2.53125D0*CA2*CA3*EL*m12squared*SA1*SA22*dAlpha2KanUsual())/(CB*MW*SW) + (1.&
  &6875D0*CA1*CA2*CA3*EL*MH12*SA12*SA22*dAlpha2KanUsual())/(CB*MW*SW) + (0.84375D0*CA1*CA2*CA3*EL*MH32*SA12*SA22*dAlpha2KanUsual(&
  &))/(CB*MW*SW) - (0.75D0*CA1*CA2*EL*m12squared*SA2*SA3*dAlpha2KanUsual())/(CB*MW*SW) + (0.5D0*CA2*EL*MH12*SA1*SA2*SA3*dAlpha2Ka&
  &nUsual())/(CB*MW*SW) + (1.5D0*CA12*CA2*EL*MH12*SA1*SA2*SA3*dAlpha2KanUsual())/(CB*MW*SW) + (0.25D0*CA2*EL*MH32*SA1*SA2*SA3*dAl&
  &pha2KanUsual())/(CB*MW*SW) + (0.75D0*CA12*CA2*EL*MH32*SA1*SA2*SA3*dAlpha2KanUsual())/(CB*MW*SW) + (0.28125D0*CA1*CA2*CA3*EL*m1&
  &2squared*dAlpha2KanUsual())/(MW*SB*SW) - (0.1875D0*CA1*CA2*CA3*EL*m12squared*dAlpha2KanUsual())/(CB2*MW*SB*SW) + (0.1875D0*CA2&
  &*CA3*EL*MH12*SA1*dAlpha2KanUsual())/(MW*SB*SW) - (0.1875D0*CA12*CA2*CA3*EL*MH12*SA1*dAlpha2KanUsual())/(MW*SB*SW) + (0.09375D0&
  &*CA2*CA3*EL*MH32*SA1*dAlpha2KanUsual())/(MW*SB*SW) - (0.09375D0*CA12*CA2*CA3*EL*MH32*SA1*dAlpha2KanUsual())/(MW*SB*SW) + (0.28&
  &125D0*CA1*CA2*CA3*EL*m12squared*SA12*dAlpha2KanUsual())/(CB2*MW*SB*SW) - (2.53125D0*CA1*CA2*CA3*EL*m12squared*SA22*dAlpha2KanU&
  &sual())/(MW*SB*SW) + (1.6875D0*CA1*CA2*CA3*EL*m12squared*SA22*dAlpha2KanUsual())/(CB2*MW*SB*SW) - (1.6875D0*CA2*CA3*EL*MH12*SA&
  &1*SA22*dAlpha2KanUsual())/(MW*SB*SW) + (1.6875D0*CA12*CA2*CA3*EL*MH12*SA1*SA22*dAlpha2KanUsual())/(MW*SB*SW) - (0.84375D0*CA2*&
  &CA3*EL*MH32*SA1*SA22*dAlpha2KanUsual())/(MW*SB*SW) + (0.84375D0*CA12*CA2*CA3*EL*MH32*SA1*SA22*dAlpha2KanUsual())/(MW*SB*SW) - &
  &(2.53125D0*CA1*CA2*CA3*EL*m12squared*SA12*SA22*dAlpha2KanUsual())/(CB2*MW*SB*SW) - (0.5D0*CA1*CA2*EL*MH12*SA2*SA3*dAlpha2KanUs&
  &ual())/(MW*SB*SW) - (0.25D0*CA1*CA2*EL*MH32*SA2*SA3*dAlpha2KanUsual())/(MW*SB*SW) + (0.75D0*CA2*EL*m12squared*SA1*SA2*SA3*dAlp&
  &ha2KanUsual())/(MW*SB*SW) - (0.5D0*CA2*EL*m12squared*SA1*SA2*SA3*dAlpha2KanUsual())/(CB2*MW*SB*SW) - (2.25D0*CA12*CA2*EL*m12sq&
  &uared*SA1*SA2*SA3*dAlpha2KanUsual())/(CB2*MW*SB*SW) - (1.5D0*CA1*CA2*EL*MH12*SA12*SA2*SA3*dAlpha2KanUsual())/(MW*SB*SW) - (0.7&
  &5D0*CA1*CA2*EL*MH32*SA12*SA2*SA3*dAlpha2KanUsual())/(MW*SB*SW) - (0.1875D0*CA2*CA3*EL*m12squared*SA1*dAlpha2KanUsual())/(CB*MW&
  &*SB2*SW) + (0.28125D0*CA12*CA2*CA3*EL*m12squared*SA1*dAlpha2KanUsual())/(CB*MW*SB2*SW) + (1.6875D0*CA2*CA3*EL*m12squared*SA1*S&
  &A22*dAlpha2KanUsual())/(CB*MW*SB2*SW) - (2.53125D0*CA12*CA2*CA3*EL*m12squared*SA1*SA22*dAlpha2KanUsual())/(CB*MW*SB2*SW) + (0.&
  &5D0*CA1*CA2*EL*m12squared*SA2*SA3*dAlpha2KanUsual())/(CB*MW*SB2*SW) + (2.25D0*CA1*CA2*EL*m12squared*SA12*SA2*SA3*dAlpha2KanUsu&
  &al())/(CB*MW*SB2*SW) - (0.09375D0*CA2*CA3*EL*m12squared*SA1*dAlpha2KanUsual())/(MW*SB*SW*TB) + (0.84375D0*CA2*CA3*EL*m12square&
  &d*SA1*SA22*dAlpha2KanUsual())/(MW*SB*SW*TB) + (0.25D0*CA1*CA2*EL*m12squared*SA2*SA3*dAlpha2KanUsual())/(MW*SB*SW*TB) - (0.0937&
  &5D0*CA1*CA2*CA3*EL*m12squared*TB*dAlpha2KanUsual())/(CB*MW*SW) + (0.84375D0*CA1*CA2*CA3*EL*m12squared*SA22*TB*dAlpha2KanUsual(&
  &))/(CB*MW*SW) - (0.25D0*CA2*EL*m12squared*SA1*SA2*SA3*TB*dAlpha2KanUsual())/(CB*MW*SW) + (0.5D0*CA3*MH12*SA2*dAlpha2KanUsual()&
  &)/vS - (4.5D0*CA22*CA3*MH12*SA2*dAlpha2KanUsual())/vS + (0.25D0*CA3*MH32*SA2*dAlpha2KanUsual())/vS - (2.25D0*CA22*CA3*MH32*SA2&
  &*dAlpha2KanUsual())/vS + (0.1875D0*CA1*CA3*EL*m12squared*dAlpha3KanUsual())/(CB*MW*SW) + (0.1875D0*CA1*CA22*CA3*EL*m12squared*&
  &dAlpha3KanUsual())/(CB*MW*SW) - (0.125D0*CA3*EL*MH12*SA1*dAlpha3KanUsual())/(CB*MW*SW) - (0.375D0*CA12*CA3*EL*MH12*SA1*dAlpha3&
  &KanUsual())/(CB*MW*SW) - (0.125D0*CA22*CA3*EL*MH12*SA1*dAlpha3KanUsual())/(CB*MW*SW) - (0.375D0*CA12*CA22*CA3*EL*MH12*SA1*dAlp&
  &ha3KanUsual())/(CB*MW*SW) - (0.0625D0*CA3*EL*MH32*SA1*dAlpha3KanUsual())/(CB*MW*SW) - (0.1875D0*CA12*CA3*EL*MH32*SA1*dAlpha3Ka&
  &nUsual())/(CB*MW*SW) - (0.0625D0*CA22*CA3*EL*MH32*SA1*dAlpha3KanUsual())/(CB*MW*SW) - (0.1875D0*CA12*CA22*CA3*EL*MH32*SA1*dAlp&
  &ha3KanUsual())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*m12squared*SA22*dAlpha3KanUsual())/(CB*MW*SW) + (0.125D0*CA3*EL*MH12*SA1*SA22&
  &*dAlpha3KanUsual())/(CB*MW*SW) + (0.375D0*CA12*CA3*EL*MH12*SA1*SA22*dAlpha3KanUsual())/(CB*MW*SW) + (0.0625D0*CA3*EL*MH32*SA1*&
  &SA22*dAlpha3KanUsual())/(CB*MW*SW) + (0.1875D0*CA12*CA3*EL*MH32*SA1*SA22*dAlpha3KanUsual())/(CB*MW*SW) - (0.1875D0*CA1*EL*MH12&
  &*SA2*SA3*dAlpha3KanUsual())/(CB*MW*SW) - (0.5625D0*CA1*CA22*EL*MH12*SA2*SA3*dAlpha3KanUsual())/(CB*MW*SW) - (0.09375D0*CA1*EL*&
  &MH32*SA2*SA3*dAlpha3KanUsual())/(CB*MW*SW) - (0.28125D0*CA1*CA22*EL*MH32*SA2*SA3*dAlpha3KanUsual())/(CB*MW*SW) - (0.28125D0*EL&
  &*m12squared*SA1*SA2*SA3*dAlpha3KanUsual())/(CB*MW*SW) - (0.84375D0*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha3KanUsual())/(CB*MW*SW&
  &) + (0.1875D0*CA1*EL*MH12*SA12*SA2*SA3*dAlpha3KanUsual())/(CB*MW*SW) + (0.5625D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dAlpha3KanUsual&
  &())/(CB*MW*SW) + (0.09375D0*CA1*EL*MH32*SA12*SA2*SA3*dAlpha3KanUsual())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*MH32*SA12*SA2*SA3*&
  &dAlpha3KanUsual())/(CB*MW*SW) + (0.125D0*CA1*CA3*EL*MH12*dAlpha3KanUsual())/(MW*SB*SW) + (0.125D0*CA1*CA22*CA3*EL*MH12*dAlpha3&
  &KanUsual())/(MW*SB*SW) + (0.0625D0*CA1*CA3*EL*MH32*dAlpha3KanUsual())/(MW*SB*SW) + (0.0625D0*CA1*CA22*CA3*EL*MH32*dAlpha3KanUs&
  &ual())/(MW*SB*SW) - (0.1875D0*CA3*EL*m12squared*SA1*dAlpha3KanUsual())/(MW*SB*SW) - (0.1875D0*CA22*CA3*EL*m12squared*SA1*dAlph&
  &a3KanUsual())/(MW*SB*SW) + (0.125D0*CA3*EL*m12squared*SA1*dAlpha3KanUsual())/(CB2*MW*SB*SW) + (0.5625D0*CA12*CA3*EL*m12squared&
  &*SA1*dAlpha3KanUsual())/(CB2*MW*SB*SW) + (0.125D0*CA22*CA3*EL*m12squared*SA1*dAlpha3KanUsual())/(CB2*MW*SB*SW) + (0.5625D0*CA1&
  &2*CA22*CA3*EL*m12squared*SA1*dAlpha3KanUsual())/(CB2*MW*SB*SW) + (0.375D0*CA1*CA3*EL*MH12*SA12*dAlpha3KanUsual())/(MW*SB*SW) +&
  & (0.375D0*CA1*CA22*CA3*EL*MH12*SA12*dAlpha3KanUsual())/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*MH32*SA12*dAlpha3KanUsual())/(MW*SB*S&
  &W) + (0.1875D0*CA1*CA22*CA3*EL*MH32*SA12*dAlpha3KanUsual())/(MW*SB*SW) - (0.125D0*CA1*CA3*EL*MH12*SA22*dAlpha3KanUsual())/(MW*&
  &SB*SW) - (0.0625D0*CA1*CA3*EL*MH32*SA22*dAlpha3KanUsual())/(MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA1*SA22*dAlpha3KanUsual()&
  &)/(MW*SB*SW) - (0.125D0*CA3*EL*m12squared*SA1*SA22*dAlpha3KanUsual())/(CB2*MW*SB*SW) - (0.5625D0*CA12*CA3*EL*m12squared*SA1*SA&
  &22*dAlpha3KanUsual())/(CB2*MW*SB*SW) - (0.375D0*CA1*CA3*EL*MH12*SA12*SA22*dAlpha3KanUsual())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL&
  &*MH32*SA12*SA22*dAlpha3KanUsual())/(MW*SB*SW) - (0.28125D0*CA1*EL*m12squared*SA2*SA3*dAlpha3KanUsual())/(MW*SB*SW) - (0.84375D&
  &0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha3KanUsual())/(MW*SB*SW) + (0.1875D0*CA1*EL*m12squared*SA2*SA3*dAlpha3KanUsual())/(CB2*M&
  &W*SB*SW) + (0.5625D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha3KanUsual())/(CB2*MW*SB*SW) - (0.1875D0*EL*MH12*SA1*SA2*SA3*dAlpha3K&
  &anUsual())/(MW*SB*SW) + (0.1875D0*CA12*EL*MH12*SA1*SA2*SA3*dAlpha3KanUsual())/(MW*SB*SW) - (0.5625D0*CA22*EL*MH12*SA1*SA2*SA3*&
  &dAlpha3KanUsual())/(MW*SB*SW) + (0.5625D0*CA12*CA22*EL*MH12*SA1*SA2*SA3*dAlpha3KanUsual())/(MW*SB*SW) - (0.09375D0*EL*MH32*SA1&
  &*SA2*SA3*dAlpha3KanUsual())/(MW*SB*SW) + (0.09375D0*CA12*EL*MH32*SA1*SA2*SA3*dAlpha3KanUsual())/(MW*SB*SW) - (0.28125D0*CA22*E&
  &L*MH32*SA1*SA2*SA3*dAlpha3KanUsual())/(MW*SB*SW) + (0.28125D0*CA12*CA22*EL*MH32*SA1*SA2*SA3*dAlpha3KanUsual())/(MW*SB*SW) - (0&
  &.28125D0*CA1*EL*m12squared*SA12*SA2*SA3*dAlpha3KanUsual())/(CB2*MW*SB*SW) - (0.84375D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*dAl&
  &pha3KanUsual())/(CB2*MW*SB*SW) - (0.125D0*CA1*CA3*EL*m12squared*dAlpha3KanUsual())/(CB*MW*SB2*SW) - (0.125D0*CA1*CA22*CA3*EL*m&
  &12squared*dAlpha3KanUsual())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA3*EL*m12squared*SA12*dAlpha3KanUsual())/(CB*MW*SB2*SW) - (0.5625&
  &D0*CA1*CA22*CA3*EL*m12squared*SA12*dAlpha3KanUsual())/(CB*MW*SB2*SW) + (0.125D0*CA1*CA3*EL*m12squared*SA22*dAlpha3KanUsual())/&
  &(CB*MW*SB2*SW) + (0.5625D0*CA1*CA3*EL*m12squared*SA12*SA22*dAlpha3KanUsual())/(CB*MW*SB2*SW) + (0.1875D0*EL*m12squared*SA1*SA2&
  &*SA3*dAlpha3KanUsual())/(CB*MW*SB2*SW) - (0.28125D0*CA12*EL*m12squared*SA1*SA2*SA3*dAlpha3KanUsual())/(CB*MW*SB2*SW) + (0.5625&
  &D0*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha3KanUsual())/(CB*MW*SB2*SW) - (0.84375D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha3Ka&
  &nUsual())/(CB*MW*SB2*SW) - (0.0625D0*CA1*CA3*EL*m12squared*dAlpha3KanUsual())/(MW*SB*SW*TB) - (0.0625D0*CA1*CA22*CA3*EL*m12squ&
  &ared*dAlpha3KanUsual())/(MW*SB*SW*TB) + (0.0625D0*CA1*CA3*EL*m12squared*SA22*dAlpha3KanUsual())/(MW*SB*SW*TB) + (0.09375D0*EL*&
  &m12squared*SA1*SA2*SA3*dAlpha3KanUsual())/(MW*SB*SW*TB) + (0.28125D0*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha3KanUsual())/(MW*SB*&
  &SW*TB) + (0.0625D0*CA3*EL*m12squared*SA1*TB*dAlpha3KanUsual())/(CB*MW*SW) + (0.0625D0*CA22*CA3*EL*m12squared*SA1*TB*dAlpha3Kan&
  &Usual())/(CB*MW*SW) - (0.0625D0*CA3*EL*m12squared*SA1*SA22*TB*dAlpha3KanUsual())/(CB*MW*SW) + (0.09375D0*CA1*EL*m12squared*SA2&
  &*SA3*TB*dAlpha3KanUsual())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*m12squared*SA2*SA3*TB*dAlpha3KanUsual())/(CB*MW*SW) + (0.5D0*CA&
  &2*MH12*SA3*dAlpha3KanUsual())/vS + (0.25D0*CA2*MH32*SA3*dAlpha3KanUsual())/vS + (1.5D0*CA2*MH12*SA22*SA3*dAlpha3KanUsual())/vS&
  & + (0.75D0*CA2*MH32*SA22*SA3*dAlpha3KanUsual())/vS + (0.09375D0*CA3*EL*MH12*SA1*SA2*dBeta2KanUsual())/(CB*MW*SW) - (0.09375D0*&
  &CA12*CA3*EL*MH12*SA1*SA2*dBeta2KanUsual())/(CB*MW*SW) + (0.28125D0*CA22*CA3*EL*MH12*SA1*SA2*dBeta2KanUsual())/(CB*MW*SW) - (0.&
  &28125D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dBeta2KanUsual())/(CB*MW*SW) + (0.046875D0*CA3*EL*MH32*SA1*SA2*dBeta2KanUsual())/(CB*MW*&
  &SW) - (0.046875D0*CA12*CA3*EL*MH32*SA1*SA2*dBeta2KanUsual())/(CB*MW*SW) + (0.140625D0*CA22*CA3*EL*MH32*SA1*SA2*dBeta2KanUsual(&
  &))/(CB*MW*SW) - (0.140625D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*dBeta2KanUsual())/(CB*MW*SW) + (0.0625D0*CA1*EL*MH12*SA3*dBeta2KanUs&
  &ual())/(CB*MW*SW) + (0.0625D0*CA1*CA22*EL*MH12*SA3*dBeta2KanUsual())/(CB*MW*SW) + (0.03125D0*CA1*EL*MH32*SA3*dBeta2KanUsual())&
  &/(CB*MW*SW) + (0.03125D0*CA1*CA22*EL*MH32*SA3*dBeta2KanUsual())/(CB*MW*SW) + (0.1875D0*CA1*EL*MH12*SA12*SA3*dBeta2KanUsual())/&
  &(CB*MW*SW) + (0.1875D0*CA1*CA22*EL*MH12*SA12*SA3*dBeta2KanUsual())/(CB*MW*SW) + (0.09375D0*CA1*EL*MH32*SA12*SA3*dBeta2KanUsual&
  &())/(CB*MW*SW) + (0.09375D0*CA1*CA22*EL*MH32*SA12*SA3*dBeta2KanUsual())/(CB*MW*SW) - (0.0625D0*CA1*EL*MH12*SA22*SA3*dBeta2KanU&
  &sual())/(CB*MW*SW) - (0.03125D0*CA1*EL*MH32*SA22*SA3*dBeta2KanUsual())/(CB*MW*SW) - (0.1875D0*CA1*EL*MH12*SA12*SA22*SA3*dBeta2&
  &KanUsual())/(CB*MW*SW) - (0.09375D0*CA1*EL*MH32*SA12*SA22*SA3*dBeta2KanUsual())/(CB*MW*SW) + (0.09375D0*CA3*EL*m12squared*SA1*&
  &SA2*dBeta2KanUsual())/(MW*SB*SW) + (0.28125D0*CA22*CA3*EL*m12squared*SA1*SA2*dBeta2KanUsual())/(MW*SB*SW) - (0.328125D0*CA3*EL&
  &*m12squared*SA1*SA2*dBeta2KanUsual())/(CB2*MW*SB*SW) + (0.421875D0*CA12*CA3*EL*m12squared*SA1*SA2*dBeta2KanUsual())/(CB2*MW*SB&
  &*SW) - (0.984375D0*CA22*CA3*EL*m12squared*SA1*SA2*dBeta2KanUsual())/(CB2*MW*SB*SW) + (1.265625D0*CA12*CA22*CA3*EL*m12squared*S&
  &A1*SA2*dBeta2KanUsual())/(CB2*MW*SB*SW) + (0.0625D0*CA1*EL*m12squared*SA3*dBeta2KanUsual())/(MW*SB*SW) + (0.0625D0*CA1*CA22*EL&
  &*m12squared*SA3*dBeta2KanUsual())/(MW*SB*SW) - (0.21875D0*CA1*EL*m12squared*SA3*dBeta2KanUsual())/(CB2*MW*SB*SW) - (0.21875D0*&
  &CA1*CA22*EL*m12squared*SA3*dBeta2KanUsual())/(CB2*MW*SB*SW) - (0.84375D0*CA1*EL*m12squared*SA12*SA3*dBeta2KanUsual())/(CB2*MW*&
  &SB*SW) - (0.84375D0*CA1*CA22*EL*m12squared*SA12*SA3*dBeta2KanUsual())/(CB2*MW*SB*SW) - (0.0625D0*CA1*EL*m12squared*SA22*SA3*dB&
  &eta2KanUsual())/(MW*SB*SW) + (0.21875D0*CA1*EL*m12squared*SA22*SA3*dBeta2KanUsual())/(CB2*MW*SB*SW) + (0.84375D0*CA1*EL*m12squ&
  &ared*SA12*SA22*SA3*dBeta2KanUsual())/(CB2*MW*SB*SW) + (0.09375D0*CA1*CA3*EL*m12squared*SA2*dBeta2KanUsual())/(CB*MW*SB2*SW) + &
  &(0.28125D0*CA1*CA22*CA3*EL*m12squared*SA2*dBeta2KanUsual())/(CB*MW*SB2*SW) - (0.09375D0*CA3*EL*MH12*SA1*SA2*dBeta2KanUsual())/&
  &(CB*MW*SB2*SW) + (0.09375D0*CA12*CA3*EL*MH12*SA1*SA2*dBeta2KanUsual())/(CB*MW*SB2*SW) - (0.28125D0*CA22*CA3*EL*MH12*SA1*SA2*dB&
  &eta2KanUsual())/(CB*MW*SB2*SW) + (0.28125D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dBeta2KanUsual())/(CB*MW*SB2*SW) - (0.046875D0*CA3*E&
  &L*MH32*SA1*SA2*dBeta2KanUsual())/(CB*MW*SB2*SW) + (0.046875D0*CA12*CA3*EL*MH32*SA1*SA2*dBeta2KanUsual())/(CB*MW*SB2*SW) - (0.1&
  &40625D0*CA22*CA3*EL*MH32*SA1*SA2*dBeta2KanUsual())/(CB*MW*SB2*SW) + (0.140625D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*dBeta2KanUsual()&
  &)/(CB*MW*SB2*SW) - (0.28125D0*CA1*CA3*EL*m12squared*SA12*SA2*dBeta2KanUsual())/(CB*MW*SB2*SW) - (0.84375D0*CA1*CA22*CA3*EL*m12&
  &squared*SA12*SA2*dBeta2KanUsual())/(CB*MW*SB2*SW) - (0.0625D0*CA1*EL*MH12*SA3*dBeta2KanUsual())/(CB*MW*SB2*SW) - (0.0625D0*CA1&
  &*CA22*EL*MH12*SA3*dBeta2KanUsual())/(CB*MW*SB2*SW) - (0.03125D0*CA1*EL*MH32*SA3*dBeta2KanUsual())/(CB*MW*SB2*SW) - (0.03125D0*&
  &CA1*CA22*EL*MH32*SA3*dBeta2KanUsual())/(CB*MW*SB2*SW) - (0.0625D0*EL*m12squared*SA1*SA3*dBeta2KanUsual())/(CB*MW*SB2*SW) - (0.&
  &5625D0*CA12*EL*m12squared*SA1*SA3*dBeta2KanUsual())/(CB*MW*SB2*SW) - (0.0625D0*CA22*EL*m12squared*SA1*SA3*dBeta2KanUsual())/(C&
  &B*MW*SB2*SW) - (0.5625D0*CA12*CA22*EL*m12squared*SA1*SA3*dBeta2KanUsual())/(CB*MW*SB2*SW) - (0.1875D0*CA1*EL*MH12*SA12*SA3*dBe&
  &ta2KanUsual())/(CB*MW*SB2*SW) - (0.1875D0*CA1*CA22*EL*MH12*SA12*SA3*dBeta2KanUsual())/(CB*MW*SB2*SW) - (0.09375D0*CA1*EL*MH32*&
  &SA12*SA3*dBeta2KanUsual())/(CB*MW*SB2*SW) - (0.09375D0*CA1*CA22*EL*MH32*SA12*SA3*dBeta2KanUsual())/(CB*MW*SB2*SW) + (0.0625D0*&
  &CA1*EL*MH12*SA22*SA3*dBeta2KanUsual())/(CB*MW*SB2*SW) + (0.03125D0*CA1*EL*MH32*SA22*SA3*dBeta2KanUsual())/(CB*MW*SB2*SW) + (0.&
  &0625D0*EL*m12squared*SA1*SA22*SA3*dBeta2KanUsual())/(CB*MW*SB2*SW) + (0.5625D0*CA12*EL*m12squared*SA1*SA22*SA3*dBeta2KanUsual(&
  &))/(CB*MW*SB2*SW) + (0.1875D0*CA1*EL*MH12*SA12*SA22*SA3*dBeta2KanUsual())/(CB*MW*SB2*SW) + (0.09375D0*CA1*EL*MH32*SA12*SA22*SA&
  &3*dBeta2KanUsual())/(CB*MW*SB2*SW) - (0.1875D0*CA1*CA3*EL*m12squared*SA2*dBeta2KanUsual())/(MW*SB*SW*TB) - (0.5625D0*CA1*CA22*&
  &CA3*EL*m12squared*SA2*dBeta2KanUsual())/(MW*SB*SW*TB) - (0.09375D0*CA3*EL*MH12*SA1*SA2*dBeta2KanUsual())/(MW*SB*SW*TB) + (0.09&
  &375D0*CA12*CA3*EL*MH12*SA1*SA2*dBeta2KanUsual())/(MW*SB*SW*TB) - (0.28125D0*CA22*CA3*EL*MH12*SA1*SA2*dBeta2KanUsual())/(MW*SB*&
  &SW*TB) + (0.28125D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dBeta2KanUsual())/(MW*SB*SW*TB) - (0.046875D0*CA3*EL*MH32*SA1*SA2*dBeta2KanU&
  &sual())/(MW*SB*SW*TB) + (0.046875D0*CA12*CA3*EL*MH32*SA1*SA2*dBeta2KanUsual())/(MW*SB*SW*TB) - (0.140625D0*CA22*CA3*EL*MH32*SA&
  &1*SA2*dBeta2KanUsual())/(MW*SB*SW*TB) + (0.140625D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*dBeta2KanUsual())/(MW*SB*SW*TB) - (0.0625D0*&
  &CA1*EL*MH12*SA3*dBeta2KanUsual())/(MW*SB*SW*TB) - (0.0625D0*CA1*CA22*EL*MH12*SA3*dBeta2KanUsual())/(MW*SB*SW*TB) - (0.03125D0*&
  &CA1*EL*MH32*SA3*dBeta2KanUsual())/(MW*SB*SW*TB) - (0.03125D0*CA1*CA22*EL*MH32*SA3*dBeta2KanUsual())/(MW*SB*SW*TB) + (0.125D0*E&
  &L*m12squared*SA1*SA3*dBeta2KanUsual())/(MW*SB*SW*TB) + (0.125D0*CA22*EL*m12squared*SA1*SA3*dBeta2KanUsual())/(MW*SB*SW*TB) - (&
  &0.1875D0*CA1*EL*MH12*SA12*SA3*dBeta2KanUsual())/(MW*SB*SW*TB) - (0.1875D0*CA1*CA22*EL*MH12*SA12*SA3*dBeta2KanUsual())/(MW*SB*S&
  &W*TB) - (0.09375D0*CA1*EL*MH32*SA12*SA3*dBeta2KanUsual())/(MW*SB*SW*TB) - (0.09375D0*CA1*CA22*EL*MH32*SA12*SA3*dBeta2KanUsual(&
  &))/(MW*SB*SW*TB) + (0.0625D0*CA1*EL*MH12*SA22*SA3*dBeta2KanUsual())/(MW*SB*SW*TB) + (0.03125D0*CA1*EL*MH32*SA22*SA3*dBeta2KanU&
  &sual())/(MW*SB*SW*TB) - (0.125D0*EL*m12squared*SA1*SA22*SA3*dBeta2KanUsual())/(MW*SB*SW*TB) + (0.1875D0*CA1*EL*MH12*SA12*SA22*&
  &SA3*dBeta2KanUsual())/(MW*SB*SW*TB) + (0.09375D0*CA1*EL*MH32*SA12*SA22*SA3*dBeta2KanUsual())/(MW*SB*SW*TB) + (0.1875D0*CA1*CA3&
  &*EL*MH12*SA2*TB*dBeta2KanUsual())/(CB*MW*SW) + (0.5625D0*CA1*CA22*CA3*EL*MH12*SA2*TB*dBeta2KanUsual())/(CB*MW*SW) + (0.09375D0&
  &*CA1*CA3*EL*MH32*SA2*TB*dBeta2KanUsual())/(CB*MW*SW) + (0.28125D0*CA1*CA22*CA3*EL*MH32*SA2*TB*dBeta2KanUsual())/(CB*MW*SW) + (&
  &0.328125D0*CA3*EL*m12squared*SA1*SA2*TB*dBeta2KanUsual())/(CB*MW*SW) + (0.984375D0*CA22*CA3*EL*m12squared*SA1*SA2*TB*dBeta2Kan&
  &Usual())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*MH12*SA12*SA2*TB*dBeta2KanUsual())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*EL*MH12*SA12&
  &*SA2*TB*dBeta2KanUsual())/(CB*MW*SW) - (0.09375D0*CA1*CA3*EL*MH32*SA12*SA2*TB*dBeta2KanUsual())/(CB*MW*SW) - (0.28125D0*CA1*CA&
  &22*CA3*EL*MH32*SA12*SA2*TB*dBeta2KanUsual())/(CB*MW*SW) + (0.21875D0*CA1*EL*m12squared*SA3*TB*dBeta2KanUsual())/(CB*MW*SW) + (&
  &0.21875D0*CA1*CA22*EL*m12squared*SA3*TB*dBeta2KanUsual())/(CB*MW*SW) - (0.125D0*EL*MH12*SA1*SA3*TB*dBeta2KanUsual())/(CB*MW*SW&
  &) - (0.375D0*CA12*EL*MH12*SA1*SA3*TB*dBeta2KanUsual())/(CB*MW*SW) - (0.125D0*CA22*EL*MH12*SA1*SA3*TB*dBeta2KanUsual())/(CB*MW*&
  &SW) - (0.375D0*CA12*CA22*EL*MH12*SA1*SA3*TB*dBeta2KanUsual())/(CB*MW*SW) - (0.0625D0*EL*MH32*SA1*SA3*TB*dBeta2KanUsual())/(CB*&
  &MW*SW) - (0.1875D0*CA12*EL*MH32*SA1*SA3*TB*dBeta2KanUsual())/(CB*MW*SW) - (0.0625D0*CA22*EL*MH32*SA1*SA3*TB*dBeta2KanUsual())/&
  &(CB*MW*SW) - (0.1875D0*CA12*CA22*EL*MH32*SA1*SA3*TB*dBeta2KanUsual())/(CB*MW*SW) - (0.21875D0*CA1*EL*m12squared*SA22*SA3*TB*dB&
  &eta2KanUsual())/(CB*MW*SW) + (0.125D0*EL*MH12*SA1*SA22*SA3*TB*dBeta2KanUsual())/(CB*MW*SW) + (0.375D0*CA12*EL*MH12*SA1*SA22*SA&
  &3*TB*dBeta2KanUsual())/(CB*MW*SW) + (0.0625D0*EL*MH32*SA1*SA22*SA3*TB*dBeta2KanUsual())/(CB*MW*SW) + (0.1875D0*CA12*EL*MH32*SA&
  &1*SA22*SA3*TB*dBeta2KanUsual())/(CB*MW*SW) + (0.140625D0*CA3*EL*m12squared*SA1*SA2*dBeta2KanUsual())/(MW*SB*SW*TB2) + (0.42187&
  &5D0*CA22*CA3*EL*m12squared*SA1*SA2*dBeta2KanUsual())/(MW*SB*SW*TB2) + (0.09375D0*CA1*EL*m12squared*SA3*dBeta2KanUsual())/(MW*S&
  &B*SW*TB2) + (0.09375D0*CA1*CA22*EL*m12squared*SA3*dBeta2KanUsual())/(MW*SB*SW*TB2) - (0.09375D0*CA1*EL*m12squared*SA22*SA3*dBe&
  &ta2KanUsual())/(MW*SB*SW*TB2) - (0.1875D0*CA1*CA3*EL*m12squared*SA2*TB2*dBeta2KanUsual())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*&
  &EL*m12squared*SA2*TB2*dBeta2KanUsual())/(CB*MW*SW) + (0.125D0*EL*m12squared*SA1*SA3*TB2*dBeta2KanUsual())/(CB*MW*SW) + (0.125D&
  &0*CA22*EL*m12squared*SA1*SA3*TB2*dBeta2KanUsual())/(CB*MW*SW) - (0.125D0*EL*m12squared*SA1*SA22*SA3*TB2*dBeta2KanUsual())/(CB*&
  &MW*SW) + (0.125D0*CA2*CA3*MH12*dBeta2KanUsual())/(CB*SB*vS) + (0.0625D0*CA2*CA3*MH32*dBeta2KanUsual())/(CB*SB*vS) + (0.375D0*C&
  &A2*CA3*MH12*SA22*dBeta2KanUsual())/(CB*SB*vS) + (0.1875D0*CA2*CA3*MH32*SA22*dBeta2KanUsual())/(CB*SB*vS) - (0.125D0*CA2*CA3*MH&
  &12*dBeta2KanUsual())/(TB*vS) - (0.0625D0*CA2*CA3*MH32*dBeta2KanUsual())/(TB*vS) - (0.375D0*CA2*CA3*MH12*SA22*dBeta2KanUsual())&
  &/(TB*vS) - (0.1875D0*CA2*CA3*MH32*SA22*dBeta2KanUsual())/(TB*vS) - (0.125D0*CA2*CA3*MH12*TB*dBeta2KanUsual())/vS - (0.0625D0*C&
  &A2*CA3*MH32*TB*dBeta2KanUsual())/vS - (0.375D0*CA2*CA3*MH12*SA22*TB*dBeta2KanUsual())/vS - (0.1875D0*CA2*CA3*MH32*SA22*TB*dBet&
  &a2KanUsual())/vS - (0.375D0*EL*MH12*SA3*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.375D0*CA22*EL*MH12*SA3*dAlpha1&
  &KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*EL*MH32*SA3*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.18&
  &75D0*CA22*EL*MH32*SA3*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.375D0*EL*MH12*SA22*SA3*dAlpha1KanUsual()*DBLE(CA&
  &1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*EL*MH32*SA22*SA3*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA3*EL*M&
  &H12*SA2*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA22*CA3*EL*MH12*SA2*dAlpha1KanUsual()*DBLE(CA1**INT(3.&
  &D0)))/(MW*SB*SW) - (0.09375D0*CA3*EL*MH32*SA2*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA22*CA3*EL*MH32&
  &*SA2*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.5625D0*EL*m12squared*SA3*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/&
  &(CB2*MW*SB*SW) + (0.5625D0*CA22*EL*m12squared*SA3*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*EL*m12squ&
  &ared*SA22*SA3*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.28125D0*CA3*EL*m12squared*SA2*dAlpha1KanUsual()*DBLE&
  &(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA22*CA3*EL*m12squared*SA2*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW&
  &) + (0.0625D0*CA2*CA3*EL*MH12*dAlpha2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA2*CA3*EL*MH32*dAlpha2KanUsual(&
  &)*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.5625D0*CA2*CA3*EL*MH12*SA22*dAlpha2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.28&
  &125D0*CA2*CA3*EL*MH32*SA22*dAlpha2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA2*CA3*EL*m12squared*dAlpha2KanUsu&
  &al()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.84375D0*CA2*CA3*EL*m12squared*SA22*dAlpha2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB2&
  &*MW*SB*SW) + (0.5D0*CA2*EL*MH12*SA2*SA3*dAlpha2KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.25D0*CA2*EL*MH32*SA2*SA3*dAlph&
  &a2KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.75D0*CA2*EL*m12squared*SA2*SA3*dAlpha2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*&
  &MW*SB2*SW) - (0.0625D0*EL*MH12*SA2*SA3*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA22*EL*MH12*SA2*SA3*dAl&
  &pha3KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*EL*MH32*SA2*SA3*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW&
  &) - (0.09375D0*CA22*EL*MH32*SA2*SA3*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.125D0*CA3*EL*MH12*dAlpha3KanUsual(&
  &)*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.125D0*CA22*CA3*EL*MH12*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.0625D0*&
  &CA3*EL*MH32*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.0625D0*CA22*CA3*EL*MH32*dAlpha3KanUsual()*DBLE(CA1**INT(3.&
  &D0)))/(MW*SB*SW) + (0.125D0*CA3*EL*MH12*SA22*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*CA3*EL*MH32*SA22*d&
  &Alpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*EL*m12squared*SA2*SA3*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(&
  &CB2*MW*SB*SW) + (0.28125D0*CA22*EL*m12squared*SA2*SA3*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*CA3*E&
  &L*m12squared*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*CA22*CA3*EL*m12squared*dAlpha3KanUsual()*DBLE(&
  &CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.1875D0*CA3*EL*m12squared*SA22*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0&
  &.0625D0*EL*MH12*SA3*dBeta2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.0625D0*CA22*EL*MH12*SA3*dBeta2KanUsual()*DBLE(CA1**&
  &INT(3.D0)))/(CB*MW*SW) - (0.03125D0*EL*MH32*SA3*dBeta2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*CA22*EL*MH32*SA&
  &3*dBeta2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*EL*MH12*SA22*SA3*dBeta2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW&
  &*SW) + (0.03125D0*EL*MH32*SA22*SA3*dBeta2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*EL*m12squared*SA3*dBeta2KanU&
  &sual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.28125D0*CA22*EL*m12squared*SA3*dBeta2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW&
  &*SB*SW) - (0.28125D0*EL*m12squared*SA22*SA3*dBeta2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.09375D0*CA3*EL*m12squar&
  &ed*SA2*dBeta2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA22*CA3*EL*m12squared*SA2*dBeta2KanUsual()*DBLE(CA1&
  &**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*EL*MH12*SA3*dBeta2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*CA22*EL&
  &*MH12*SA3*dBeta2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.03125D0*EL*MH32*SA3*dBeta2KanUsual()*DBLE(CA1**INT(3.D0))&
  &)/(CB*MW*SB2*SW) + (0.03125D0*CA22*EL*MH32*SA3*dBeta2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.0625D0*EL*MH12*SA22*&
  &SA3*dBeta2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.03125D0*EL*MH32*SA22*SA3*dBeta2KanUsual()*DBLE(CA1**INT(3.D0)))&
  &/(CB*MW*SB2*SW) + (0.0625D0*EL*MH12*SA3*dBeta2KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.0625D0*CA22*EL*MH12*SA3*dBet&
  &a2KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.03125D0*EL*MH32*SA3*dBeta2KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB)&
  & + (0.03125D0*CA22*EL*MH32*SA3*dBeta2KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.0625D0*EL*MH12*SA22*SA3*dBeta2KanUsua&
  &l()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.03125D0*EL*MH32*SA22*SA3*dBeta2KanUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (&
  &0.0625D0*CA3*EL*MH12*SA2*TB*dBeta2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA22*CA3*EL*MH12*SA2*TB*dBeta2KanUsu&
  &al()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA3*EL*MH32*SA2*TB*dBeta2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.0&
  &9375D0*CA22*CA3*EL*MH32*SA2*TB*dBeta2KanUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA1*CA3*EL*MH12*dAlpha2KanUsual()&
  &*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA1*CA3*EL*MH32*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.84375D0&
  &*CA3*EL*m12squared*SA1*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.5625D0*CA1*CA3*EL*MH12*SA12*dAlpha2KanUsual()*D&
  &BLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA1*CA3*EL*MH32*SA12*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.8437&
  &5D0*CA1*CA3*EL*m12squared*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA1*CA3*EL*m12squared*dAlpha2KanUsual&
  &()*DBLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.5625D0*CA3*EL*MH12*SA1*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.56&
  &25D0*CA12*CA3*EL*MH12*SA1*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*CA3*EL*MH32*SA1*dAlpha2KanUsual()*DB&
  &LE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA12*CA3*EL*MH32*SA1*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) + (0.84375&
  &D0*CA1*CA3*EL*m12squared*SA12*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA3*EL*m12squared*SA1*dAlpha2&
  &KanUsual()*DBLE(CA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA3*EL*m12squared*SA1*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0))&
  &)/(CB*MW*SB2*SW) - (0.28125D0*CA3*EL*m12squared*SA1*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.28125D0*CA1*CA3&
  &*EL*m12squared*TB*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.5D0*MH12*SA3*dAlpha3KanUsual()*DBLE(CA2**INT(3.D0)))&
  &/vS - (0.25D0*MH32*SA3*dAlpha3KanUsual()*DBLE(CA2**INT(3.D0)))/vS - (0.125D0*CA3*MH12*dBeta2KanUsual()*DBLE(CA2**INT(3.D0)))/(&
  &CB*SB*vS) - (0.0625D0*CA3*MH32*dBeta2KanUsual()*DBLE(CA2**INT(3.D0)))/(CB*SB*vS) + (0.125D0*CA3*MH12*dBeta2KanUsual()*DBLE(CA2&
  &**INT(3.D0)))/(TB*vS) + (0.0625D0*CA3*MH32*dBeta2KanUsual()*DBLE(CA2**INT(3.D0)))/(TB*vS) + (0.125D0*CA3*MH12*TB*dBeta2KanUsua&
  &l()*DBLE(CA2**INT(3.D0)))/vS + (0.0625D0*CA3*MH32*TB*dBeta2KanUsual()*DBLE(CA2**INT(3.D0)))/vS + (0.1875D0*CA3*EL*MH12*dAlpha2&
  &KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA3*EL*MH32*dAlpha2KanUsual()*DBLE(CA1**INT(3.D0&
  &))*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA3*EL*m12squared*dAlpha2KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0))&
  &)/(CB2*MW*SB*SW) - (0.375D0*CA1*CA3*EL*m12squared*SA2*dBeta2KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (1.125D0*CA1*CA22*CA3*E&
  &L*m12squared*SA2*dBeta2KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.5625D0*CA1*CA3*EL*m12squared*SA12*SA2*dBeta2KanUsual()*DB&
  &LE(CB**INT(-3.D0)))/(MW*SW) + (1.6875D0*CA1*CA22*CA3*EL*m12squared*SA12*SA2*dBeta2KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (&
  &0.25D0*EL*m12squared*SA1*SA3*dBeta2KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (1.125D0*CA12*EL*m12squared*SA1*SA3*dBeta2KanUsu&
  &al()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.25D0*CA22*EL*m12squared*SA1*SA3*dBeta2KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (1.12&
  &5D0*CA12*CA22*EL*m12squared*SA1*SA3*dBeta2KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.25D0*EL*m12squared*SA1*SA22*SA3*dBeta2&
  &KanUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (1.125D0*CA12*EL*m12squared*SA1*SA22*SA3*dBeta2KanUsual()*DBLE(CB**INT(-3.D0)))/(MW&
  &*SW) - (0.1875D0*CA3*EL*m12squared*SA2*dBeta2KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.5625D0*CA22*CA&
  &3*EL*m12squared*SA2*dBeta2KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.1875D0*CA3*EL*MH12*SA2*dAlpha1Kan&
  &Usual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA22*CA3*EL*MH12*SA2*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) +&
  & (0.09375D0*CA3*EL*MH32*SA2*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA22*CA3*EL*MH32*SA2*dAlpha1KanUsu&
  &al()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA3*EL*m12squared*SA2*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW&
  &) - (0.84375D0*CA22*CA3*EL*m12squared*SA2*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.375D0*EL*MH12*SA3*dAlpha&
  &1KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.375D0*CA22*EL*MH12*SA3*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - &
  &(0.1875D0*EL*MH32*SA3*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA22*EL*MH32*SA3*dAlpha1KanUsual()*DBLE(S&
  &A1**INT(3.D0)))/(MW*SB*SW) + (0.375D0*EL*MH12*SA22*SA3*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*EL*MH32*&
  &SA22*SA3*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.5625D0*EL*m12squared*SA3*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0&
  &)))/(CB*MW*SB2*SW) + (0.5625D0*CA22*EL*m12squared*SA3*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.5625D0*EL*m1&
  &2squared*SA22*SA3*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.5D0*CA2*EL*MH12*SA2*SA3*dAlpha2KanUsual()*DBLE(S&
  &A1**INT(3.D0)))/(CB*MW*SW) - (0.25D0*CA2*EL*MH32*SA2*SA3*dAlpha2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA2*CA&
  &3*EL*MH12*dAlpha2KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA2*CA3*EL*MH32*dAlpha2KanUsual()*DBLE(SA1**INT(3.D0&
  &)))/(MW*SB*SW) - (0.5625D0*CA2*CA3*EL*MH12*SA22*dAlpha2KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA2*CA3*EL*MH3&
  &2*SA22*dAlpha2KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.75D0*CA2*EL*m12squared*SA2*SA3*dAlpha2KanUsual()*DBLE(SA1**INT(&
  &3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA2*CA3*EL*m12squared*dAlpha2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*&
  &CA2*CA3*EL*m12squared*SA22*dAlpha2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.125D0*CA3*EL*MH12*dAlpha3KanUsual()*DBL&
  &E(SA1**INT(3.D0)))/(CB*MW*SW) + (0.125D0*CA22*CA3*EL*MH12*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA3*E&
  &L*MH32*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA22*CA3*EL*MH32*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))&
  &/(CB*MW*SW) - (0.125D0*CA3*EL*MH12*SA22*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.0625D0*CA3*EL*MH32*SA22*dAlpha&
  &3KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA3*EL*m12squared*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*&
  &SW) - (0.1875D0*CA22*CA3*EL*m12squared*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA&
  &22*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D0*EL*MH12*SA2*SA3*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/&
  &(MW*SB*SW) - (0.1875D0*CA22*EL*MH12*SA2*SA3*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.03125D0*EL*MH32*SA2*SA3*dA&
  &lpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA22*EL*MH32*SA2*SA3*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(MW&
  &*SB*SW) + (0.09375D0*EL*m12squared*SA2*SA3*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA22*EL*m12squa&
  &red*SA2*SA3*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.03125D0*CA3*EL*MH12*SA2*dBeta2KanUsual()*DBLE(SA1**INT&
  &(3.D0)))/(CB*MW*SW) + (0.09375D0*CA22*CA3*EL*MH12*SA2*dBeta2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.015625D0*CA3*EL*M&
  &H32*SA2*dBeta2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.046875D0*CA22*CA3*EL*MH32*SA2*dBeta2KanUsual()*DBLE(SA1**INT(3.&
  &D0)))/(CB*MW*SW) - (0.140625D0*CA3*EL*m12squared*SA2*dBeta2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.421875D0*CA22*&
  &CA3*EL*m12squared*SA2*dBeta2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.03125D0*CA3*EL*MH12*SA2*dBeta2KanUsual()*DBLE&
  &(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.09375D0*CA22*CA3*EL*MH12*SA2*dBeta2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.&
  &015625D0*CA3*EL*MH32*SA2*dBeta2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.046875D0*CA22*CA3*EL*MH32*SA2*dBeta2KanUsu&
  &al()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*EL*m12squared*SA3*dBeta2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) &
  &+ (0.1875D0*CA22*EL*m12squared*SA3*dBeta2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.1875D0*EL*m12squared*SA22*SA3*dB&
  &eta2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.03125D0*CA3*EL*MH12*SA2*dBeta2KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB&
  &*SW*TB) - (0.09375D0*CA22*CA3*EL*MH12*SA2*dBeta2KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.015625D0*CA3*EL*MH32*SA2*d&
  &Beta2KanUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.046875D0*CA22*CA3*EL*MH32*SA2*dBeta2KanUsual()*DBLE(SA1**INT(3.D0)))/&
  &(MW*SB*SW*TB) + (0.125D0*EL*MH12*SA3*TB*dBeta2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.125D0*CA22*EL*MH12*SA3*TB*dBeta&
  &2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*EL*MH32*SA3*TB*dBeta2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0&
  &.0625D0*CA22*EL*MH32*SA3*TB*dBeta2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.125D0*EL*MH12*SA22*SA3*TB*dBeta2KanUsual()*&
  &DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.0625D0*EL*MH32*SA22*SA3*TB*dBeta2KanUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0&
  &*CA3*EL*MH12*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA3*EL*MH32*dAlpha2KanUsual(&
  &)*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*dAlpha2KanUsual()*DBLE(CA2**INT(3.D0))*&
  &DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.375D0*EL*m12squared*SA3*dBeta2KanUsual()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/&
  &(MW*SW) - (0.375D0*CA22*EL*m12squared*SA3*dBeta2KanUsual()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) + (0.375D0*EL*m1&
  &2squared*SA22*SA3*dBeta2KanUsual()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) - (0.28125D0*CA1*CA3*EL*m12squared*dAlph&
  &a1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA3*EL*MH12*SA1*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) +&
  & (0.5625D0*CA12*CA3*EL*MH12*SA1*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA3*EL*MH32*SA1*dAlpha1KanUsua&
  &l()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA12*CA3*EL*MH32*SA1*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0&
  &.1875D0*CA1*CA3*EL*MH12*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA1*CA3*EL*MH32*dAlpha1KanUsual()*DBLE&
  &(SA2**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*CA3*EL*m12squared*SA1*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.1875D0&
  &*CA3*EL*m12squared*SA1*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.84375D0*CA12*CA3*EL*m12squared*SA1*dAlpha1K&
  &anUsual()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA3*EL*MH12*SA12*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB&
  &*SW) - (0.28125D0*CA1*CA3*EL*MH32*SA12*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*m12squared*dA&
  &lpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA1*CA3*EL*m12squared*SA12*dAlpha1KanUsual()*DBLE(SA2**INT(3&
  &.D0)))/(CB*MW*SB2*SW) + (0.09375D0*CA1*CA3*EL*m12squared*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA&
  &3*EL*m12squared*SA1*TB*dAlpha1KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (1.5D0*CA3*MH12*dAlpha2KanUsual()*DBLE(SA2**INT(3.&
  &D0)))/vS + (0.75D0*CA3*MH32*dAlpha2KanUsual()*DBLE(SA2**INT(3.D0)))/vS + (0.1875D0*CA1*EL*MH12*SA3*dAlpha3KanUsual()*DBLE(SA2*&
  &*INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1*EL*MH32*SA3*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*EL*m12squ&
  &ared*SA1*SA3*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA1*EL*MH12*SA12*SA3*dAlpha3KanUsual()*DBLE(SA2**I&
  &NT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1*EL*MH32*SA12*SA3*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA1*EL&
  &*m12squared*SA3*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA1*EL*m12squared*SA3*dAlpha3KanUsual()*DBLE(SA&
  &2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*EL*MH12*SA1*SA3*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA12*&
  &EL*MH12*SA1*SA3*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*EL*MH32*SA1*SA3*dAlpha3KanUsual()*DBLE(SA2**IN&
  &T(3.D0)))/(MW*SB*SW) - (0.09375D0*CA12*EL*MH32*SA1*SA3*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*CA1*EL*&
  &m12squared*SA12*SA3*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*EL*m12squared*SA1*SA3*dAlpha3KanUsual()&
  &*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA12*EL*m12squared*SA1*SA3*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*S&
  &B2*SW) - (0.09375D0*EL*m12squared*SA1*SA3*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA1*EL*m12squared&
  &*SA3*TB*dAlpha3KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA3*EL*MH12*SA1*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))&
  &/(CB*MW*SW) + (0.09375D0*CA12*CA3*EL*MH12*SA1*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.046875D0*CA3*EL*MH32*SA1*&
  &dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.046875D0*CA12*CA3*EL*MH32*SA1*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/(C&
  &B*MW*SW) - (0.09375D0*CA3*EL*m12squared*SA1*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.328125D0*CA3*EL*m12squared*&
  &SA1*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.421875D0*CA12*CA3*EL*m12squared*SA1*dBeta2KanUsual()*DBLE(SA2**&
  &INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA1*CA3*EL*m12squared*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.09375&
  &D0*CA3*EL*MH12*SA1*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.09375D0*CA12*CA3*EL*MH12*SA1*dBeta2KanUsual()*DB&
  &LE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.046875D0*CA3*EL*MH32*SA1*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.04&
  &6875D0*CA12*CA3*EL*MH32*SA1*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA1*CA3*EL*m12squared*SA12*dBet&
  &a2KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*CA1*CA3*EL*m12squared*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/(MW&
  &*SB*SW*TB) + (0.09375D0*CA3*EL*MH12*SA1*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA12*CA3*EL*MH12*SA1&
  &*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.046875D0*CA3*EL*MH32*SA1*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/(MW&
  &*SB*SW*TB) - (0.046875D0*CA12*CA3*EL*MH32*SA1*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.1875D0*CA1*CA3*EL*MH12&
  &*TB*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1*CA3*EL*MH32*TB*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/(&
  &CB*MW*SW) - (0.328125D0*CA3*EL*m12squared*SA1*TB*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA1*CA3*EL*MH12&
  &*SA12*TB*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1*CA3*EL*MH32*SA12*TB*dBeta2KanUsual()*DBLE(SA2**INT&
  &(3.D0)))/(CB*MW*SW) - (0.140625D0*CA3*EL*m12squared*SA1*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB2) + (0.1875D0*CA1*&
  &CA3*EL*m12squared*TB2*dBeta2KanUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA3*EL*MH12*dAlpha1KanUsual()*DBLE(CA1**IN&
  &T(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA3*EL*MH32*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))&
  &)/(MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*dAlpha1KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0&
  &625D0*EL*MH12*SA3*dAlpha3KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*EL*MH32*SA3*dAlpha3KanU&
  &sual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*EL*m12squared*SA3*dAlpha3KanUsual()*DBLE(CA1**INT(3.&
  &D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA3*EL*m12squared*dBeta2KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3&
  &.D0)))/(CB*MW*SB2*SW) - (0.0625D0*CA3*EL*MH12*TB*dBeta2KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.0&
  &3125D0*CA3*EL*MH32*TB*dBeta2KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.375D0*CA1*CA3*EL*m12squared*&
  &dBeta2KanUsual()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) - (0.5625D0*CA1*CA3*EL*m12squared*SA12*dBeta2KanUsual()*DB&
  &LE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) + (0.1875D0*CA3*EL*m12squared*dBeta2KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(CB**&
  &INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) - (0.1875D0*CA3*EL*MH12*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))&
  &/(CB*MW*SW) - (0.09375D0*CA3*EL*MH32*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA3*&
  &EL*m12squared*dAlpha1KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.0625D0*EL*MH12*SA3*dAlpha3KanUs&
  &ual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*EL*MH32*SA3*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0))*DB&
  &LE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*EL*m12squared*SA3*dAlpha3KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB&
  &*MW*SB2*SW) - (0.03125D0*CA3*EL*MH12*dBeta2KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.015625D0*CA3*&
  &EL*MH32*dBeta2KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.140625D0*CA3*EL*m12squared*dBeta2KanUsual(&
  &)*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.03125D0*CA3*EL*MH12*dBeta2KanUsual()*DBLE(SA1**INT(3.D0))*DBL&
  &E(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.015625D0*CA3*EL*MH32*dBeta2KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW&
  &*SB2*SW) + (0.03125D0*CA3*EL*MH12*dBeta2KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.015625D0*CA3*&
  &EL*MH32*dBeta2KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.328125D0*CA3*EL*m12squared*SA1*SA2*dBet&
  &a2KanUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.421875D0*CA12*CA3*EL*m12squared*SA1*SA2*dBeta2KanUsual()*DBLE(SB**INT(-3.D0)))&
  &/(MW*SW) + (0.984375D0*CA22*CA3*EL*m12squared*SA1*SA2*dBeta2KanUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (1.265625D0*CA12*CA22*C&
  &A3*EL*m12squared*SA1*SA2*dBeta2KanUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.09375D0*CA3*EL*m12squared*SA1*SA2*dBeta2KanUsual(&
  &)*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.140625D0*CA12*CA3*EL*m12squared*SA1*SA2*dBeta2KanUsual()*DBLE(SB**INT(-3.D0)))/(CB2*M&
  &W*SW) + (0.28125D0*CA22*CA3*EL*m12squared*SA1*SA2*dBeta2KanUsual()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.421875D0*CA12*CA22*C&
  &A3*EL*m12squared*SA1*SA2*dBeta2KanUsual()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.21875D0*CA1*EL*m12squared*SA3*dBeta2KanUsual(&
  &)*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.21875D0*CA1*CA22*EL*m12squared*SA3*dBeta2KanUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.06&
  &25D0*CA1*EL*m12squared*SA3*dBeta2KanUsual()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.0625D0*CA1*CA22*EL*m12squared*SA3*dBeta2Kan&
  &Usual()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.84375D0*CA1*EL*m12squared*SA12*SA3*dBeta2KanUsual()*DBLE(SB**INT(-3.D0)))/(MW*S&
  &W) + (0.84375D0*CA1*CA22*EL*m12squared*SA12*SA3*dBeta2KanUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.28125D0*CA1*EL*m12squared*&
  &SA12*SA3*dBeta2KanUsual()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.28125D0*CA1*CA22*EL*m12squared*SA12*SA3*dBeta2KanUsual()*DBLE&
  &(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.21875D0*CA1*EL*m12squared*SA22*SA3*dBeta2KanUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.0625&
  &D0*CA1*EL*m12squared*SA22*SA3*dBeta2KanUsual()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.84375D0*CA1*EL*m12squared*SA12*SA22*SA3*&
  &dBeta2KanUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.28125D0*CA1*EL*m12squared*SA12*SA22*SA3*dBeta2KanUsual()*DBLE(SB**INT(-3.D&
  &0)))/(CB2*MW*SW) - (0.28125D0*EL*m12squared*SA3*dBeta2KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.28125&
  &D0*CA22*EL*m12squared*SA3*dBeta2KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.09375D0*EL*m12squared*SA3*d&
  &Beta2KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.09375D0*CA22*EL*m12squared*SA3*dBeta2KanUsual()*DB&
  &LE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.28125D0*EL*m12squared*SA22*SA3*dBeta2KanUsual()*DBLE(CA1**INT(3.D0))&
  &*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.09375D0*EL*m12squared*SA22*SA3*dBeta2KanUsual()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))&
  &/(CB2*MW*SW) + (0.140625D0*CA3*EL*m12squared*SA2*dBeta2KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.4218&
  &75D0*CA22*CA3*EL*m12squared*SA2*dBeta2KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.046875D0*CA3*EL*m12sq&
  &uared*SA2*dBeta2KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.140625D0*CA22*CA3*EL*m12squared*SA2*dBe&
  &ta2KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.328125D0*CA3*EL*m12squared*SA1*dBeta2KanUsual()*DBLE&
  &(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.421875D0*CA12*CA3*EL*m12squared*SA1*dBeta2KanUsual()*DBLE(SA2**INT(3.D0))*&
  &DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.09375D0*CA3*EL*m12squared*SA1*dBeta2KanUsual()*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(&
  &CB2*MW*SW) + (0.140625D0*CA12*CA3*EL*m12squared*SA1*dBeta2KanUsual()*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - &
  &(0.140625D0*CA3*EL*m12squared*dBeta2KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.04&
  &6875D0*CA3*EL*m12squared*dBeta2KanUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.187&
  &5D0*CA1*CA3*MH12*SA2*dgAtMZ())/(CB*MW) + (0.5625D0*CA1*CA22*CA3*MH12*SA2*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*CA3*MH32*SA2*dgAtM&
  &Z())/(CB*MW) + (0.28125D0*CA1*CA22*CA3*MH32*SA2*dgAtMZ())/(CB*MW) + (0.28125D0*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*MW) + (0.8&
  &4375D0*CA22*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*MW) - (0.1875D0*CA1*CA3*MH12*SA12*SA2*dgAtMZ())/(CB*MW) - (0.5625D0*CA1*CA22*&
  &CA3*MH12*SA12*SA2*dgAtMZ())/(CB*MW) - (0.09375D0*CA1*CA3*MH32*SA12*SA2*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*CA3*MH32*SA12*S&
  &A2*dgAtMZ())/(CB*MW) + (0.1875D0*CA1*m12squared*SA3*dgAtMZ())/(CB*MW) + (0.1875D0*CA1*CA22*m12squared*SA3*dgAtMZ())/(CB*MW) - &
  &(0.125D0*MH12*SA1*SA3*dgAtMZ())/(CB*MW) - (0.375D0*CA12*MH12*SA1*SA3*dgAtMZ())/(CB*MW) - (0.125D0*CA22*MH12*SA1*SA3*dgAtMZ())/&
  &(CB*MW) - (0.375D0*CA12*CA22*MH12*SA1*SA3*dgAtMZ())/(CB*MW) - (0.0625D0*MH32*SA1*SA3*dgAtMZ())/(CB*MW) - (0.1875D0*CA12*MH32*S&
  &A1*SA3*dgAtMZ())/(CB*MW) - (0.0625D0*CA22*MH32*SA1*SA3*dgAtMZ())/(CB*MW) - (0.1875D0*CA12*CA22*MH32*SA1*SA3*dgAtMZ())/(CB*MW) &
  &- (0.1875D0*CA1*m12squared*SA22*SA3*dgAtMZ())/(CB*MW) + (0.125D0*MH12*SA1*SA22*SA3*dgAtMZ())/(CB*MW) + (0.375D0*CA12*MH12*SA1*&
  &SA22*SA3*dgAtMZ())/(CB*MW) + (0.0625D0*MH32*SA1*SA22*SA3*dgAtMZ())/(CB*MW) + (0.1875D0*CA12*MH32*SA1*SA22*SA3*dgAtMZ())/(CB*MW&
  &) + (0.28125D0*CA1*CA3*m12squared*SA2*dgAtMZ())/(MW*SB) + (0.84375D0*CA1*CA22*CA3*m12squared*SA2*dgAtMZ())/(MW*SB) - (0.1875D0&
  &*CA1*CA3*m12squared*SA2*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA1*CA22*CA3*m12squared*SA2*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*CA3*MH&
  &12*SA1*SA2*dgAtMZ())/(MW*SB) - (0.1875D0*CA12*CA3*MH12*SA1*SA2*dgAtMZ())/(MW*SB) + (0.5625D0*CA22*CA3*MH12*SA1*SA2*dgAtMZ())/(&
  &MW*SB) - (0.5625D0*CA12*CA22*CA3*MH12*SA1*SA2*dgAtMZ())/(MW*SB) + (0.09375D0*CA3*MH32*SA1*SA2*dgAtMZ())/(MW*SB) - (0.09375D0*C&
  &A12*CA3*MH32*SA1*SA2*dgAtMZ())/(MW*SB) + (0.28125D0*CA22*CA3*MH32*SA1*SA2*dgAtMZ())/(MW*SB) - (0.28125D0*CA12*CA22*CA3*MH32*SA&
  &1*SA2*dgAtMZ())/(MW*SB) + (0.28125D0*CA1*CA3*m12squared*SA12*SA2*dgAtMZ())/(CB2*MW*SB) + (0.84375D0*CA1*CA22*CA3*m12squared*SA&
  &12*SA2*dgAtMZ())/(CB2*MW*SB) + (0.125D0*CA1*MH12*SA3*dgAtMZ())/(MW*SB) + (0.125D0*CA1*CA22*MH12*SA3*dgAtMZ())/(MW*SB) + (0.062&
  &5D0*CA1*MH32*SA3*dgAtMZ())/(MW*SB) + (0.0625D0*CA1*CA22*MH32*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*m12squared*SA1*SA3*dgAtMZ())/(M&
  &W*SB) - (0.1875D0*CA22*m12squared*SA1*SA3*dgAtMZ())/(MW*SB) + (0.125D0*m12squared*SA1*SA3*dgAtMZ())/(CB2*MW*SB) + (0.5625D0*CA&
  &12*m12squared*SA1*SA3*dgAtMZ())/(CB2*MW*SB) + (0.125D0*CA22*m12squared*SA1*SA3*dgAtMZ())/(CB2*MW*SB) + (0.5625D0*CA12*CA22*m12&
  &squared*SA1*SA3*dgAtMZ())/(CB2*MW*SB) + (0.375D0*CA1*MH12*SA12*SA3*dgAtMZ())/(MW*SB) + (0.375D0*CA1*CA22*MH12*SA12*SA3*dgAtMZ(&
  &))/(MW*SB) + (0.1875D0*CA1*MH32*SA12*SA3*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*CA22*MH32*SA12*SA3*dgAtMZ())/(MW*SB) - (0.125D0*CA1&
  &*MH12*SA22*SA3*dgAtMZ())/(MW*SB) - (0.0625D0*CA1*MH32*SA22*SA3*dgAtMZ())/(MW*SB) + (0.1875D0*m12squared*SA1*SA22*SA3*dgAtMZ())&
  &/(MW*SB) - (0.125D0*m12squared*SA1*SA22*SA3*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA12*m12squared*SA1*SA22*SA3*dgAtMZ())/(CB2*MW*S&
  &B) - (0.375D0*CA1*MH12*SA12*SA22*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*CA1*MH32*SA12*SA22*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*CA3*m1&
  &2squared*SA1*SA2*dgAtMZ())/(CB*MW*SB2) + (0.28125D0*CA12*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA22*CA3*m12&
  &squared*SA1*SA2*dgAtMZ())/(CB*MW*SB2) + (0.84375D0*CA12*CA22*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*MW*SB2) - (0.125D0*CA1*m12sq&
  &uared*SA3*dgAtMZ())/(CB*MW*SB2) - (0.125D0*CA1*CA22*m12squared*SA3*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA1*m12squared*SA12*SA3*d&
  &gAtMZ())/(CB*MW*SB2) - (0.5625D0*CA1*CA22*m12squared*SA12*SA3*dgAtMZ())/(CB*MW*SB2) + (0.125D0*CA1*m12squared*SA22*SA3*dgAtMZ(&
  &))/(CB*MW*SB2) + (0.5625D0*CA1*m12squared*SA12*SA22*SA3*dgAtMZ())/(CB*MW*SB2) - (0.09375D0*CA3*m12squared*SA1*SA2*dgAtMZ())/(M&
  &W*SB*TB) - (0.28125D0*CA22*CA3*m12squared*SA1*SA2*dgAtMZ())/(MW*SB*TB) - (0.0625D0*CA1*m12squared*SA3*dgAtMZ())/(MW*SB*TB) - (&
  &0.0625D0*CA1*CA22*m12squared*SA3*dgAtMZ())/(MW*SB*TB) + (0.0625D0*CA1*m12squared*SA22*SA3*dgAtMZ())/(MW*SB*TB) - (0.09375D0*CA&
  &1*CA3*m12squared*SA2*TB*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*CA3*m12squared*SA2*TB*dgAtMZ())/(CB*MW) + (0.0625D0*m12squared&
  &*SA1*SA3*TB*dgAtMZ())/(CB*MW) + (0.0625D0*CA22*m12squared*SA1*SA3*TB*dgAtMZ())/(CB*MW) - (0.0625D0*m12squared*SA1*SA22*SA3*TB*&
  &dgAtMZ())/(CB*MW) + (0.0625D0*CA3*MH12*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875D0*CA22*CA3*MH12*SA2*DBLE(CA1**INT(&
  &3.D0))*dgAtMZ())/(CB*MW) + (0.03125D0*CA3*MH32*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA22*CA3*MH32*SA2*DBLE(&
  &CA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.09375D0*CA3*m12squared*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.28125D0*CA22&
  &*CA3*m12squared*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.125D0*MH12*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0&
  &.125D0*CA22*MH12*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.0625D0*MH32*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.0&
  &625D0*CA22*MH32*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.125D0*MH12*SA22*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (&
  &0.0625D0*MH32*SA22*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*m12squared*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW&
  &*SB2) + (0.1875D0*CA22*m12squared*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*m12squared*SA22*SA3*DBLE(CA1**INT&
  &(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.125D0*MH12*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.125D0*CA22*MH12*SA3*DBLE(SA1**IN&
  &T(3.D0))*dgAtMZ())/(CB*MW) + (0.0625D0*MH32*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.0625D0*CA22*MH32*SA3*DBLE(SA1**INT&
  &(3.D0))*dgAtMZ())/(CB*MW) - (0.125D0*MH12*SA22*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*MH32*SA22*SA3*DBLE(SA1**&
  &INT(3.D0))*dgAtMZ())/(CB*MW) + (0.0625D0*CA3*MH12*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA22*CA3*MH12*SA2*DBL&
  &E(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.03125D0*CA3*MH32*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA22*CA3*MH3&
  &2*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.1875D0*m12squared*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875D0&
  &*CA22*m12squared*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*m12squared*SA22*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())&
  &/(CB2*MW*SB) - (0.09375D0*CA3*m12squared*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA22*CA3*m12squared*SA2*D&
  &BLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*CA1*CA3*MH12*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.09375D0*CA1*CA3&
  &*MH32*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.28125D0*CA3*m12squared*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875D&
  &0*CA1*CA3*MH12*SA12*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*CA3*MH32*SA12*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*M&
  &W) - (0.28125D0*CA1*CA3*m12squared*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*CA3*m12squared*DBLE(SA2**INT(3.D0))*&
  &dgAtMZ())/(CB2*MW*SB) - (0.1875D0*CA3*MH12*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA12*CA3*MH12*SA1*DBLE(SA2**&
  &INT(3.D0))*dgAtMZ())/(MW*SB) - (0.09375D0*CA3*MH32*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA12*CA3*MH32*SA1*D&
  &BLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.28125D0*CA1*CA3*m12squared*SA12*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) + (0.187&
  &5D0*CA3*m12squared*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA12*CA3*m12squared*SA1*DBLE(SA2**INT(3.D0))*dg&
  &AtMZ())/(CB*MW*SB2) + (0.09375D0*CA3*m12squared*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB*TB) + (0.09375D0*CA1*CA3*m12squared*&
  &TB*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*CA3*MH12*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (&
  &0.03125D0*CA3*MH32*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA3*m12squared*DBLE(CA1**INT(3.D0)&
  &)*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.0625D0*CA3*MH12*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) &
  &- (0.03125D0*CA3*MH32*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA3*m12squared*DBLE(SA1**INT(3.&
  &D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.28125D0*CA3*EL*SA1*SA2*dm122MSBarUsual())/(CB*MW*SW) + (0.84375D0*CA22*CA&
  &3*EL*SA1*SA2*dm122MSBarUsual())/(CB*MW*SW) + (0.1875D0*CA1*EL*SA3*dm122MSBarUsual())/(CB*MW*SW) + (0.1875D0*CA1*CA22*EL*SA3*dm&
  &122MSBarUsual())/(CB*MW*SW) - (0.1875D0*CA1*EL*SA22*SA3*dm122MSBarUsual())/(CB*MW*SW) + (0.28125D0*CA1*CA3*EL*SA2*dm122MSBarUs&
  &ual())/(MW*SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*SA2*dm122MSBarUsual())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*SA2*dm122MSBarUsual())&
  &/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA22*CA3*EL*SA2*dm122MSBarUsual())/(CB2*MW*SB*SW) + (0.28125D0*CA1*CA3*EL*SA12*SA2*dm122MSBarU&
  &sual())/(CB2*MW*SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*SA12*SA2*dm122MSBarUsual())/(CB2*MW*SB*SW) - (0.1875D0*EL*SA1*SA3*dm122MSB&
  &arUsual())/(MW*SB*SW) - (0.1875D0*CA22*EL*SA1*SA3*dm122MSBarUsual())/(MW*SB*SW) + (0.125D0*EL*SA1*SA3*dm122MSBarUsual())/(CB2*&
  &MW*SB*SW) + (0.5625D0*CA12*EL*SA1*SA3*dm122MSBarUsual())/(CB2*MW*SB*SW) + (0.125D0*CA22*EL*SA1*SA3*dm122MSBarUsual())/(CB2*MW*&
  &SB*SW) + (0.5625D0*CA12*CA22*EL*SA1*SA3*dm122MSBarUsual())/(CB2*MW*SB*SW) + (0.1875D0*EL*SA1*SA22*SA3*dm122MSBarUsual())/(MW*S&
  &B*SW) - (0.125D0*EL*SA1*SA22*SA3*dm122MSBarUsual())/(CB2*MW*SB*SW) - (0.5625D0*CA12*EL*SA1*SA22*SA3*dm122MSBarUsual())/(CB2*MW&
  &*SB*SW) - (0.1875D0*CA3*EL*SA1*SA2*dm122MSBarUsual())/(CB*MW*SB2*SW) + (0.28125D0*CA12*CA3*EL*SA1*SA2*dm122MSBarUsual())/(CB*M&
  &W*SB2*SW) - (0.5625D0*CA22*CA3*EL*SA1*SA2*dm122MSBarUsual())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA22*CA3*EL*SA1*SA2*dm122MSBarUs&
  &ual())/(CB*MW*SB2*SW) - (0.125D0*CA1*EL*SA3*dm122MSBarUsual())/(CB*MW*SB2*SW) - (0.125D0*CA1*CA22*EL*SA3*dm122MSBarUsual())/(C&
  &B*MW*SB2*SW) - (0.5625D0*CA1*EL*SA12*SA3*dm122MSBarUsual())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*EL*SA12*SA3*dm122MSBarUsual())&
  &/(CB*MW*SB2*SW) + (0.125D0*CA1*EL*SA22*SA3*dm122MSBarUsual())/(CB*MW*SB2*SW) + (0.5625D0*CA1*EL*SA12*SA22*SA3*dm122MSBarUsual(&
  &))/(CB*MW*SB2*SW) - (0.09375D0*CA3*EL*SA1*SA2*dm122MSBarUsual())/(MW*SB*SW*TB) - (0.28125D0*CA22*CA3*EL*SA1*SA2*dm122MSBarUsua&
  &l())/(MW*SB*SW*TB) - (0.0625D0*CA1*EL*SA3*dm122MSBarUsual())/(MW*SB*SW*TB) - (0.0625D0*CA1*CA22*EL*SA3*dm122MSBarUsual())/(MW*&
  &SB*SW*TB) + (0.0625D0*CA1*EL*SA22*SA3*dm122MSBarUsual())/(MW*SB*SW*TB) - (0.09375D0*CA1*CA3*EL*SA2*TB*dm122MSBarUsual())/(CB*M&
  &W*SW) - (0.28125D0*CA1*CA22*CA3*EL*SA2*TB*dm122MSBarUsual())/(CB*MW*SW) + (0.0625D0*EL*SA1*SA3*TB*dm122MSBarUsual())/(CB*MW*SW&
  &) + (0.0625D0*CA22*EL*SA1*SA3*TB*dm122MSBarUsual())/(CB*MW*SW) - (0.0625D0*EL*SA1*SA22*SA3*TB*dm122MSBarUsual())/(CB*MW*SW) - &
  &(0.09375D0*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dm122MSBarUsual())/(CB2*MW*SB*SW) - (0.28125D0*CA22*CA3*EL*SA2*DBLE(CA1**INT(3.D0))&
  &*dm122MSBarUsual())/(CB2*MW*SB*SW) + (0.1875D0*EL*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SB2*SW) + (0.1875D0*CA22*&
  &EL*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SB2*SW) - (0.1875D0*EL*SA22*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarUsual())/&
  &(CB*MW*SB2*SW) - (0.1875D0*EL*SA3*DBLE(SA1**INT(3.D0))*dm122MSBarUsual())/(CB2*MW*SB*SW) - (0.1875D0*CA22*EL*SA3*DBLE(SA1**INT&
  &(3.D0))*dm122MSBarUsual())/(CB2*MW*SB*SW) + (0.1875D0*EL*SA22*SA3*DBLE(SA1**INT(3.D0))*dm122MSBarUsual())/(CB2*MW*SB*SW) - (0.&
  &09375D0*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SB2*SW) - (0.28125D0*CA22*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*dm&
  &122MSBarUsual())/(CB*MW*SB2*SW) - (0.28125D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SW) - (0.28125D0*CA1*CA&
  &3*EL*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(CB2*MW&
  &*SB*SW) - (0.28125D0*CA1*CA3*EL*SA12*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*SA1*DBLE(SA2**I&
  &NT(3.D0))*dm122MSBarUsual())/(CB*MW*SB2*SW) - (0.28125D0*CA12*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SB2*SW&
  &) + (0.09375D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(MW*SB*SW*TB) + (0.09375D0*CA1*CA3*EL*TB*DBLE(SA2**INT(3.D0)&
  &)*dm122MSBarUsual())/(CB*MW*SW) + (0.09375D0*CA3*EL*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(CB2*MW*SB*SW&
  &) + (0.09375D0*CA3*EL*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SB2*SW) + (0.1875D0*CA1*CA3*EL*SA2*d&
  &MH12OSUsual())/(CB*MW*SW) + (0.5625D0*CA1*CA22*CA3*EL*SA2*dMH12OSUsual())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*SA12*SA2*dMH12OSUs&
  &ual())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*EL*SA12*SA2*dMH12OSUsual())/(CB*MW*SW) - (0.125D0*EL*SA1*SA3*dMH12OSUsual())/(CB*MW&
  &*SW) - (0.375D0*CA12*EL*SA1*SA3*dMH12OSUsual())/(CB*MW*SW) - (0.125D0*CA22*EL*SA1*SA3*dMH12OSUsual())/(CB*MW*SW) - (0.375D0*CA&
  &12*CA22*EL*SA1*SA3*dMH12OSUsual())/(CB*MW*SW) + (0.125D0*EL*SA1*SA22*SA3*dMH12OSUsual())/(CB*MW*SW) + (0.375D0*CA12*EL*SA1*SA2&
  &2*SA3*dMH12OSUsual())/(CB*MW*SW) + (0.1875D0*CA3*EL*SA1*SA2*dMH12OSUsual())/(MW*SB*SW) - (0.1875D0*CA12*CA3*EL*SA1*SA2*dMH12OS&
  &Usual())/(MW*SB*SW) + (0.5625D0*CA22*CA3*EL*SA1*SA2*dMH12OSUsual())/(MW*SB*SW) - (0.5625D0*CA12*CA22*CA3*EL*SA1*SA2*dMH12OSUsu&
  &al())/(MW*SB*SW) + (0.125D0*CA1*EL*SA3*dMH12OSUsual())/(MW*SB*SW) + (0.125D0*CA1*CA22*EL*SA3*dMH12OSUsual())/(MW*SB*SW) + (0.3&
  &75D0*CA1*EL*SA12*SA3*dMH12OSUsual())/(MW*SB*SW) + (0.375D0*CA1*CA22*EL*SA12*SA3*dMH12OSUsual())/(MW*SB*SW) - (0.125D0*CA1*EL*S&
  &A22*SA3*dMH12OSUsual())/(MW*SB*SW) - (0.375D0*CA1*EL*SA12*SA22*SA3*dMH12OSUsual())/(MW*SB*SW) - (0.5D0*CA2*CA3*dMH12OSUsual())&
  &/vS - (1.5D0*CA2*CA3*SA22*dMH12OSUsual())/vS + (0.0625D0*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dMH12OSUsual())/(CB*MW*SW) + (0.1875D&
  &0*CA22*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dMH12OSUsual())/(CB*MW*SW) - (0.125D0*EL*SA3*DBLE(CA1**INT(3.D0))*dMH12OSUsual())/(MW*S&
  &B*SW) - (0.125D0*CA22*EL*SA3*DBLE(CA1**INT(3.D0))*dMH12OSUsual())/(MW*SB*SW) + (0.125D0*EL*SA22*SA3*DBLE(CA1**INT(3.D0))*dMH12&
  &OSUsual())/(MW*SB*SW) + (0.5D0*CA3*DBLE(CA2**INT(3.D0))*dMH12OSUsual())/vS + (0.125D0*EL*SA3*DBLE(SA1**INT(3.D0))*dMH12OSUsual&
  &())/(CB*MW*SW) + (0.125D0*CA22*EL*SA3*DBLE(SA1**INT(3.D0))*dMH12OSUsual())/(CB*MW*SW) - (0.125D0*EL*SA22*SA3*DBLE(SA1**INT(3.D&
  &0))*dMH12OSUsual())/(CB*MW*SW) + (0.0625D0*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*dMH12OSUsual())/(MW*SB*SW) + (0.1875D0*CA22*CA3*EL*&
  &SA2*DBLE(SA1**INT(3.D0))*dMH12OSUsual())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*DBLE(SA2**INT(3.D0))*dMH12OSUsual())/(CB*MW*SW) + (&
  &0.1875D0*CA1*CA3*EL*SA12*DBLE(SA2**INT(3.D0))*dMH12OSUsual())/(CB*MW*SW) - (0.1875D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dMH12OSUs&
  &ual())/(MW*SB*SW) + (0.1875D0*CA12*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dMH12OSUsual())/(MW*SB*SW) - (0.0625D0*CA3*EL*DBLE(CA1**INT&
  &(3.D0))*DBLE(SA2**INT(3.D0))*dMH12OSUsual())/(CB*MW*SW) - (0.0625D0*CA3*EL*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH12OSUs&
  &ual())/(MW*SB*SW) + (0.09375D0*CA1*CA3*EL*SA2*dMH32OSUsual())/(CB*MW*SW) + (0.28125D0*CA1*CA22*CA3*EL*SA2*dMH32OSUsual())/(CB*&
  &MW*SW) - (0.09375D0*CA1*CA3*EL*SA12*SA2*dMH32OSUsual())/(CB*MW*SW) - (0.28125D0*CA1*CA22*CA3*EL*SA12*SA2*dMH32OSUsual())/(CB*M&
  &W*SW) - (0.0625D0*EL*SA1*SA3*dMH32OSUsual())/(CB*MW*SW) - (0.1875D0*CA12*EL*SA1*SA3*dMH32OSUsual())/(CB*MW*SW) - (0.0625D0*CA2&
  &2*EL*SA1*SA3*dMH32OSUsual())/(CB*MW*SW) - (0.1875D0*CA12*CA22*EL*SA1*SA3*dMH32OSUsual())/(CB*MW*SW) + (0.0625D0*EL*SA1*SA22*SA&
  &3*dMH32OSUsual())/(CB*MW*SW) + (0.1875D0*CA12*EL*SA1*SA22*SA3*dMH32OSUsual())/(CB*MW*SW) + (0.09375D0*CA3*EL*SA1*SA2*dMH32OSUs&
  &ual())/(MW*SB*SW) - (0.09375D0*CA12*CA3*EL*SA1*SA2*dMH32OSUsual())/(MW*SB*SW) + (0.28125D0*CA22*CA3*EL*SA1*SA2*dMH32OSUsual())&
  &/(MW*SB*SW) - (0.28125D0*CA12*CA22*CA3*EL*SA1*SA2*dMH32OSUsual())/(MW*SB*SW) + (0.0625D0*CA1*EL*SA3*dMH32OSUsual())/(MW*SB*SW)&
  & + (0.0625D0*CA1*CA22*EL*SA3*dMH32OSUsual())/(MW*SB*SW) + (0.1875D0*CA1*EL*SA12*SA3*dMH32OSUsual())/(MW*SB*SW) + (0.1875D0*CA1&
  &*CA22*EL*SA12*SA3*dMH32OSUsual())/(MW*SB*SW) - (0.0625D0*CA1*EL*SA22*SA3*dMH32OSUsual())/(MW*SB*SW) - (0.1875D0*CA1*EL*SA12*SA&
  &22*SA3*dMH32OSUsual())/(MW*SB*SW) - (0.25D0*CA2*CA3*dMH32OSUsual())/vS - (0.75D0*CA2*CA3*SA22*dMH32OSUsual())/vS + (0.03125D0*&
  &CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dMH32OSUsual())/(CB*MW*SW) + (0.09375D0*CA22*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dMH32OSUsual())/(&
  &CB*MW*SW) - (0.0625D0*EL*SA3*DBLE(CA1**INT(3.D0))*dMH32OSUsual())/(MW*SB*SW) - (0.0625D0*CA22*EL*SA3*DBLE(CA1**INT(3.D0))*dMH3&
  &2OSUsual())/(MW*SB*SW) + (0.0625D0*EL*SA22*SA3*DBLE(CA1**INT(3.D0))*dMH32OSUsual())/(MW*SB*SW) + (0.25D0*CA3*DBLE(CA2**INT(3.D&
  &0))*dMH32OSUsual())/vS + (0.0625D0*EL*SA3*DBLE(SA1**INT(3.D0))*dMH32OSUsual())/(CB*MW*SW) + (0.0625D0*CA22*EL*SA3*DBLE(SA1**IN&
  &T(3.D0))*dMH32OSUsual())/(CB*MW*SW) - (0.0625D0*EL*SA22*SA3*DBLE(SA1**INT(3.D0))*dMH32OSUsual())/(CB*MW*SW) + (0.03125D0*CA3*E&
  &L*SA2*DBLE(SA1**INT(3.D0))*dMH32OSUsual())/(MW*SB*SW) + (0.09375D0*CA22*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*dMH32OSUsual())/(MW*SB&
  &*SW) - (0.09375D0*CA1*CA3*EL*DBLE(SA2**INT(3.D0))*dMH32OSUsual())/(CB*MW*SW) + (0.09375D0*CA1*CA3*EL*SA12*DBLE(SA2**INT(3.D0))&
  &*dMH32OSUsual())/(CB*MW*SW) - (0.09375D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dMH32OSUsual())/(MW*SB*SW) + (0.09375D0*CA12*CA3*EL*S&
  &A1*DBLE(SA2**INT(3.D0))*dMH32OSUsual())/(MW*SB*SW) - (0.03125D0*CA3*EL*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH32OSUsual(&
  &))/(CB*MW*SW) - (0.03125D0*CA3*EL*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH32OSUsual())/(MW*SB*SW) - (0.09375D0*CA1*CA3*EL&
  &*MH12*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.28125D0*CA1*CA22*CA3*EL*MH12*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(C&
  &B*SW) - (0.046875D0*CA1*CA3*EL*MH32*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.140625D0*CA1*CA22*CA3*EL*MH32*SA2*DBLE(&
  &MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.140625D0*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.42&
  &1875D0*CA22*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.09375D0*CA1*CA3*EL*MH12*SA12*SA2*DBLE(MW*&
  &*INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.28125D0*CA1*CA22*CA3*EL*MH12*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.04&
  &6875D0*CA1*CA3*EL*MH32*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.140625D0*CA1*CA22*CA3*EL*MH32*SA12*SA2*DBLE(MW*&
  &*INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.09375D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.09375D0*CA&
  &1*CA22*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.0625D0*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual(&
  &))/(CB*SW) + (0.1875D0*CA12*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.0625D0*CA22*EL*MH12*SA1*SA3*DBLE(MW&
  &**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.1875D0*CA12*CA22*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.03125D&
  &0*EL*MH32*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.09375D0*CA12*EL*MH32*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual()&
  &)/(CB*SW) + (0.03125D0*CA22*EL*MH32*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.09375D0*CA12*CA22*EL*MH32*SA1*SA3*D&
  &BLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.09375D0*CA1*EL*m12squared*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (&
  &0.0625D0*EL*MH12*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.1875D0*CA12*EL*MH12*SA1*SA22*SA3*DBLE(MW**INT(-3.&
  &D0))*dMW2Usual())/(CB*SW) - (0.03125D0*EL*MH32*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.09375D0*CA12*EL*MH3&
  &2*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.140625D0*CA1*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*dMW2Usua&
  &l())/(SB*SW) - (0.421875D0*CA1*CA22*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.09375D0*CA1*CA3*EL*m1&
  &2squared*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) + (0.28125D0*CA1*CA22*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*dM&
  &W2Usual())/(CB2*SB*SW) - (0.09375D0*CA3*EL*MH12*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.09375D0*CA12*CA3*EL*MH1&
  &2*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.28125D0*CA22*CA3*EL*MH12*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(S&
  &B*SW) + (0.28125D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.046875D0*CA3*EL*MH32*SA1*SA2*D&
  &BLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.046875D0*CA12*CA3*EL*MH32*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0&
  &.140625D0*CA22*CA3*EL*MH32*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.140625D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*DBLE(&
  &MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.140625D0*CA1*CA3*EL*m12squared*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW&
  &) - (0.421875D0*CA1*CA22*CA3*EL*m12squared*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) - (0.0625D0*CA1*EL*MH12*SA3*&
  &DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.0625D0*CA1*CA22*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.0312&
  &5D0*CA1*EL*MH32*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.03125D0*CA1*CA22*EL*MH32*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual&
  &())/(SB*SW) + (0.09375D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.09375D0*CA22*EL*m12squared*SA1*S&
  &A3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.0625D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) -&
  & (0.28125D0*CA12*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) - (0.0625D0*CA22*EL*m12squared*SA1*SA3*DB&
  &LE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) - (0.28125D0*CA12*CA22*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB&
  &2*SB*SW) - (0.1875D0*CA1*EL*MH12*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.1875D0*CA1*CA22*EL*MH12*SA12*SA3*DBLE&
  &(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.09375D0*CA1*EL*MH32*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.09375D0&
  &*CA1*CA22*EL*MH32*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.0625D0*CA1*EL*MH12*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW&
  &2Usual())/(SB*SW) + (0.03125D0*CA1*EL*MH32*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.09375D0*EL*m12squared*SA1*S&
  &A22*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.0625D0*EL*m12squared*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB&
  &2*SB*SW) + (0.28125D0*CA12*EL*m12squared*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) + (0.1875D0*CA1*EL*MH12*SA&
  &12*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.09375D0*CA1*EL*MH32*SA12*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())&
  &/(SB*SW) + (0.09375D0*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) - (0.140625D0*CA12*CA3*EL*m12squ&
  &ared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.28125D0*CA22*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dM&
  &W2Usual())/(CB*SB2*SW) - (0.421875D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.06&
  &25D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.0625D0*CA1*CA22*EL*m12squared*SA3*DBLE(MW**INT(-&
  &3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.28125D0*CA1*EL*m12squared*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.281&
  &25D0*CA1*CA22*EL*m12squared*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) - (0.0625D0*CA1*EL*m12squared*SA22*SA3*DBLE&
  &(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) - (0.28125D0*CA1*EL*m12squared*SA12*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*S&
  &B2*SW) + (0.046875D0*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW*TB) + (0.140625D0*CA22*CA3*EL*m12squar&
  &ed*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW*TB) + (0.03125D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(&
  &SB*SW*TB) + (0.03125D0*CA1*CA22*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW*TB) - (0.03125D0*CA1*EL*m12squared*&
  &SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW*TB) + (0.046875D0*CA1*CA3*EL*m12squared*SA2*TB*DBLE(MW**INT(-3.D0))*dMW2Usua&
  &l())/(CB*SW) + (0.140625D0*CA1*CA22*CA3*EL*m12squared*SA2*TB*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.03125D0*EL*m12squa&
  &red*SA1*SA3*TB*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.03125D0*CA22*EL*m12squared*SA1*SA3*TB*DBLE(MW**INT(-3.D0))*dMW2U&
  &sual())/(CB*SW) + (0.03125D0*EL*m12squared*SA1*SA22*SA3*TB*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.03125D0*CA3*EL*MH12*&
  &SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.09375D0*CA22*CA3*EL*MH12*SA2*DBLE(CA1**INT(3.D0))*DBLE&
  &(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.015625D0*CA3*EL*MH32*SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB&
  &*SW) - (0.046875D0*CA22*CA3*EL*MH32*SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.046875D0*CA3*EL*m1&
  &2squared*SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) + (0.140625D0*CA22*CA3*EL*m12squared*SA2*DBLE(&
  &CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) + (0.0625D0*EL*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0)&
  &)*dMW2Usual())/(SB*SW) + (0.0625D0*CA22*EL*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.03125D&
  &0*EL*MH32*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.03125D0*CA22*EL*MH32*SA3*DBLE(CA1**INT(3.D0)&
  &)*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.0625D0*EL*MH12*SA22*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual()&
  &)/(SB*SW) - (0.03125D0*EL*MH32*SA22*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.09375D0*EL*m12squa&
  &red*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) - (0.09375D0*CA22*EL*m12squared*SA3*DBLE(CA1**INT(3&
  &.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.09375D0*EL*m12squared*SA22*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D&
  &0))*dMW2Usual())/(CB*SB2*SW) - (0.0625D0*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB*SW) - (0.0625D&
  &0*CA22*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB*SW) - (0.03125D0*EL*MH32*SA3*DBLE(MW**INT(-3.D0)&
  &)*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB*SW) - (0.03125D0*CA22*EL*MH32*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual(&
  &))/(CB*SW) + (0.0625D0*EL*MH12*SA22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB*SW) + (0.03125D0*EL*MH32*SA&
  &22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB*SW) - (0.03125D0*CA3*EL*MH12*SA2*DBLE(MW**INT(-3.D0))*DBLE(S&
  &A1**INT(3.D0))*dMW2Usual())/(SB*SW) - (0.09375D0*CA22*CA3*EL*MH12*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(&
  &SB*SW) - (0.015625D0*CA3*EL*MH32*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(SB*SW) - (0.046875D0*CA22*CA3*EL*&
  &MH32*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(SB*SW) + (0.09375D0*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*DB&
  &LE(SA1**INT(3.D0))*dMW2Usual())/(CB2*SB*SW) + (0.09375D0*CA22*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2&
  &Usual())/(CB2*SB*SW) - (0.09375D0*EL*m12squared*SA22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB2*SB*SW) + &
  &(0.046875D0*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.140625D0*CA22*CA3*EL&
  &*m12squared*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.09375D0*CA1*CA3*EL*MH12*DBLE(MW**INT(-&
  &3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SW) + (0.046875D0*CA1*CA3*EL*MH32*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2U&
  &sual())/(CB*SW) + (0.140625D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SW) - (0.09375D&
  &0*CA1*CA3*EL*MH12*SA12*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SW) - (0.046875D0*CA1*CA3*EL*MH32*SA12*DBLE(&
  &MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SW) + (0.140625D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*DBLE(SA2**I&
  &NT(3.D0))*dMW2Usual())/(SB*SW) - (0.09375D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB2*&
  &SB*SW) + (0.09375D0*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(SB*SW) - (0.09375D0*CA12*CA3*EL*MH&
  &12*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(SB*SW) + (0.046875D0*CA3*EL*MH32*SA1*DBLE(MW**INT(-3.D0))*DBLE(&
  &SA2**INT(3.D0))*dMW2Usual())/(SB*SW) - (0.046875D0*CA12*CA3*EL*MH32*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())&
  &/(SB*SW) + (0.140625D0*CA1*CA3*EL*m12squared*SA12*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB2*SB*SW) - (0.0937&
  &5D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.140625D0*CA12*CA3*EL*m12squa&
  &red*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SB2*SW) - (0.046875D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3&
  &.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(SB*SW*TB) - (0.046875D0*CA1*CA3*EL*m12squared*TB*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3&
  &.D0))*dMW2Usual())/(CB*SW) + (0.03125D0*CA3*EL*MH12*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual()&
  &)/(CB*SW) + (0.015625D0*CA3*EL*MH32*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SW) - (0.0&
  &46875D0*CA3*EL*m12squared*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB2*SB*SW) + (0.03125D0&
  &*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(SB*SW) + (0.015625D0*CA3*EL*MH32*DBL&
  &E(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(SB*SW) - (0.046875D0*CA3*EL*m12squared*DBLE(MW**INT(&
  &-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SB2*SW) + 0.5D0*CA2*CA3*MH12*DBLE(vS**INT(-2.D0))*dvSMSBarU&
  &sual() + 0.25D0*CA2*CA3*MH32*DBLE(vS**INT(-2.D0))*dvSMSBarUsual() + 1.5D0*CA2*CA3*MH12*SA22*DBLE(vS**INT(-2.D0))*dvSMSBarUsual&
  &() + 0.75D0*CA2*CA3*MH32*SA22*DBLE(vS**INT(-2.D0))*dvSMSBarUsual() - 0.5D0*CA3*MH12*DBLE(CA2**INT(3.D0))*DBLE(vS**INT(-2.D0))*&
  &dvSMSBarUsual() - 0.25D0*CA3*MH32*DBLE(CA2**INT(3.D0))*DBLE(vS**INT(-2.D0))*dvSMSBarUsual() &
		& + CS1S1S1f111*dZH1H3OSUsual()/2D0 + CS1S1S1f211*dZH2H3OSUsual()/2D0 + CS1S1S1f311*dZH3H3OS()/2D0 &
		& + CS1S1S1f131*dZH1H1OS()/2D0 + CS1S1S1f231*dZH2H1OSUsual()/2D0 + CS1S1S1f331*dZH3H1OSUsual()/2D0 &
		& + CS1S1S1f131*dZH1H1OS()/2D0 + CS1S1S1f231*dZH2H1OSUsual()/2D0 + CS1S1S1f331*dZH3H1OSUsual()/2D0 &
		& )
	case (3)
		dRR11 = -CA2*SA1*dAlpha1KanAlter() - CA1*SA2*dAlpha2KanAlter()
		dRR12 = CA1*CA2*dAlpha1KanAlter() - SA1*SA2*dAlpha2KanAlter()
		dRR13 = CA2*dAlpha2KanAlter()
		dRR21 = -CA1*CA3*dAlpha1KanAlter() - CA1*CA3*SA2*dAlpha3KanAlter() - CA1*CA2*SA3*dAlpha2KanAlter() + &
			& SA1*SA3*dAlpha3KanAlter() + SA1*SA2*SA3*dAlpha1KanAlter()
		dRR22 = -CA3*SA1*dAlpha1KanAlter() - CA3*SA1*SA2*dAlpha3KanAlter() - CA1*SA3*dAlpha3KanAlter() - &
			& CA2*SA1*SA3*dAlpha2KanAlter() - CA1*SA2*SA3*dAlpha1KanAlter()
		dRR23 = CA2*CA3*dAlpha3KanAlter() - SA2*SA3*dAlpha2KanAlter()
		dRR31 = -CA1*CA2*CA3*dAlpha2KanAlter() + CA3*SA1*dAlpha3KanAlter() + CA3*SA1*SA2*dAlpha1KanAlter() + &
			& CA1*SA3*dAlpha1KanAlter() + CA1*SA2*SA3*dAlpha3KanAlter()
		dRR32 = -CA1*CA3*dAlpha3KanAlter() - CA2*CA3*SA1*dAlpha2KanAlter() - CA1*CA3*SA2*dAlpha1KanAlter() + &
			& SA1*SA3*dAlpha1KanAlter() + SA1*SA2*SA3*dAlpha3KanAlter()
		dRR33 = -CA3*SA2*dAlpha2KanAlter() - CA2*SA3*dAlpha3KanAlter()

		totalAmplitude = CS1S1S1f311*( &
&(0.28125D0*CA1*CA3*EL*m12squared*SA2*dAlpha1KanAlter())/(CB*MW*SW) + (0.84375D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha1KanAlter())&
  &/(CB*MW*SW) - (0.1875D0*CA3*EL*MH12*SA1*SA2*dAlpha1KanAlter())/(CB*MW*SW) - (0.5625D0*CA12*CA3*EL*MH12*SA1*SA2*dAlpha1KanAlter&
  &())/(CB*MW*SW) - (0.5625D0*CA22*CA3*EL*MH12*SA1*SA2*dAlpha1KanAlter())/(CB*MW*SW) - (1.6875D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dA&
  &lpha1KanAlter())/(CB*MW*SW) - (0.09375D0*CA3*EL*MH32*SA1*SA2*dAlpha1KanAlter())/(CB*MW*SW) - (0.28125D0*CA12*CA3*EL*MH32*SA1*S&
  &A2*dAlpha1KanAlter())/(CB*MW*SW) - (0.28125D0*CA22*CA3*EL*MH32*SA1*SA2*dAlpha1KanAlter())/(CB*MW*SW) - (0.84375D0*CA12*CA22*CA&
  &3*EL*MH32*SA1*SA2*dAlpha1KanAlter())/(CB*MW*SW) - (0.125D0*CA1*EL*MH12*SA3*dAlpha1KanAlter())/(CB*MW*SW) - (0.125D0*CA1*CA22*E&
  &L*MH12*SA3*dAlpha1KanAlter())/(CB*MW*SW) - (0.0625D0*CA1*EL*MH32*SA3*dAlpha1KanAlter())/(CB*MW*SW) - (0.0625D0*CA1*CA22*EL*MH3&
  &2*SA3*dAlpha1KanAlter())/(CB*MW*SW) - (0.1875D0*EL*m12squared*SA1*SA3*dAlpha1KanAlter())/(CB*MW*SW) - (0.1875D0*CA22*EL*m12squ&
  &ared*SA1*SA3*dAlpha1KanAlter())/(CB*MW*SW) + (1.125D0*CA1*EL*MH12*SA12*SA3*dAlpha1KanAlter())/(CB*MW*SW) + (1.125D0*CA1*CA22*E&
  &L*MH12*SA12*SA3*dAlpha1KanAlter())/(CB*MW*SW) + (0.5625D0*CA1*EL*MH32*SA12*SA3*dAlpha1KanAlter())/(CB*MW*SW) + (0.5625D0*CA1*C&
  &A22*EL*MH32*SA12*SA3*dAlpha1KanAlter())/(CB*MW*SW) + (0.125D0*CA1*EL*MH12*SA22*SA3*dAlpha1KanAlter())/(CB*MW*SW) + (0.0625D0*C&
  &A1*EL*MH32*SA22*SA3*dAlpha1KanAlter())/(CB*MW*SW) + (0.1875D0*EL*m12squared*SA1*SA22*SA3*dAlpha1KanAlter())/(CB*MW*SW) - (1.12&
  &5D0*CA1*EL*MH12*SA12*SA22*SA3*dAlpha1KanAlter())/(CB*MW*SW) - (0.5625D0*CA1*EL*MH32*SA12*SA22*SA3*dAlpha1KanAlter())/(CB*MW*SW&
  &) + (0.1875D0*CA1*CA3*EL*MH12*SA2*dAlpha1KanAlter())/(MW*SB*SW) + (0.5625D0*CA1*CA22*CA3*EL*MH12*SA2*dAlpha1KanAlter())/(MW*SB&
  &*SW) + (0.09375D0*CA1*CA3*EL*MH32*SA2*dAlpha1KanAlter())/(MW*SB*SW) + (0.28125D0*CA1*CA22*CA3*EL*MH32*SA2*dAlpha1KanAlter())/(&
  &MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*SA1*SA2*dAlpha1KanAlter())/(MW*SB*SW) - (0.84375D0*CA22*CA3*EL*m12squared*SA1*SA2*dAl&
  &pha1KanAlter())/(MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA1*SA2*dAlpha1KanAlter())/(CB2*MW*SB*SW) + (0.84375D0*CA12*CA3*EL*m1&
  &2squared*SA1*SA2*dAlpha1KanAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha1KanAlter())/(CB2*MW*SB*SW&
  &) + (2.53125D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha1KanAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA1*CA3*EL*MH12*SA12*SA2*dAlp&
  &ha1KanAlter())/(MW*SB*SW) + (1.6875D0*CA1*CA22*CA3*EL*MH12*SA12*SA2*dAlpha1KanAlter())/(MW*SB*SW) + (0.28125D0*CA1*CA3*EL*MH32&
  &*SA12*SA2*dAlpha1KanAlter())/(MW*SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*MH32*SA12*SA2*dAlpha1KanAlter())/(MW*SB*SW) - (0.1875D0*C&
  &A1*EL*m12squared*SA3*dAlpha1KanAlter())/(MW*SB*SW) - (0.1875D0*CA1*CA22*EL*m12squared*SA3*dAlpha1KanAlter())/(MW*SB*SW) + (0.1&
  &25D0*CA1*EL*m12squared*SA3*dAlpha1KanAlter())/(CB2*MW*SB*SW) + (0.125D0*CA1*CA22*EL*m12squared*SA3*dAlpha1KanAlter())/(CB2*MW*&
  &SB*SW) - (0.125D0*EL*MH12*SA1*SA3*dAlpha1KanAlter())/(MW*SB*SW) + (1.125D0*CA12*EL*MH12*SA1*SA3*dAlpha1KanAlter())/(MW*SB*SW) &
  &- (0.125D0*CA22*EL*MH12*SA1*SA3*dAlpha1KanAlter())/(MW*SB*SW) + (1.125D0*CA12*CA22*EL*MH12*SA1*SA3*dAlpha1KanAlter())/(MW*SB*S&
  &W) - (0.0625D0*EL*MH32*SA1*SA3*dAlpha1KanAlter())/(MW*SB*SW) + (0.5625D0*CA12*EL*MH32*SA1*SA3*dAlpha1KanAlter())/(MW*SB*SW) - &
  &(0.0625D0*CA22*EL*MH32*SA1*SA3*dAlpha1KanAlter())/(MW*SB*SW) + (0.5625D0*CA12*CA22*EL*MH32*SA1*SA3*dAlpha1KanAlter())/(MW*SB*S&
  &W) - (1.6875D0*CA1*EL*m12squared*SA12*SA3*dAlpha1KanAlter())/(CB2*MW*SB*SW) - (1.6875D0*CA1*CA22*EL*m12squared*SA12*SA3*dAlpha&
  &1KanAlter())/(CB2*MW*SB*SW) + (0.1875D0*CA1*EL*m12squared*SA22*SA3*dAlpha1KanAlter())/(MW*SB*SW) - (0.125D0*CA1*EL*m12squared*&
  &SA22*SA3*dAlpha1KanAlter())/(CB2*MW*SB*SW) + (0.125D0*EL*MH12*SA1*SA22*SA3*dAlpha1KanAlter())/(MW*SB*SW) - (1.125D0*CA12*EL*MH&
  &12*SA1*SA22*SA3*dAlpha1KanAlter())/(MW*SB*SW) + (0.0625D0*EL*MH32*SA1*SA22*SA3*dAlpha1KanAlter())/(MW*SB*SW) - (0.5625D0*CA12*&
  &EL*MH32*SA1*SA22*SA3*dAlpha1KanAlter())/(MW*SB*SW) + (1.6875D0*CA1*EL*m12squared*SA12*SA22*SA3*dAlpha1KanAlter())/(CB2*MW*SB*S&
  &W) - (0.1875D0*CA1*CA3*EL*m12squared*SA2*dAlpha1KanAlter())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha1K&
  &anAlter())/(CB*MW*SB2*SW) - (0.84375D0*CA1*CA3*EL*m12squared*SA12*SA2*dAlpha1KanAlter())/(CB*MW*SB2*SW) - (2.53125D0*CA1*CA22*&
  &CA3*EL*m12squared*SA12*SA2*dAlpha1KanAlter())/(CB*MW*SB2*SW) + (0.125D0*EL*m12squared*SA1*SA3*dAlpha1KanAlter())/(CB*MW*SB2*SW&
  &) - (1.6875D0*CA12*EL*m12squared*SA1*SA3*dAlpha1KanAlter())/(CB*MW*SB2*SW) + (0.125D0*CA22*EL*m12squared*SA1*SA3*dAlpha1KanAlt&
  &er())/(CB*MW*SB2*SW) - (1.6875D0*CA12*CA22*EL*m12squared*SA1*SA3*dAlpha1KanAlter())/(CB*MW*SB2*SW) - (0.125D0*EL*m12squared*SA&
  &1*SA22*SA3*dAlpha1KanAlter())/(CB*MW*SB2*SW) + (1.6875D0*CA12*EL*m12squared*SA1*SA22*SA3*dAlpha1KanAlter())/(CB*MW*SB2*SW) - (&
  &0.09375D0*CA1*CA3*EL*m12squared*SA2*dAlpha1KanAlter())/(MW*SB*SW*TB) - (0.28125D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha1KanAlt&
  &er())/(MW*SB*SW*TB) + (0.0625D0*EL*m12squared*SA1*SA3*dAlpha1KanAlter())/(MW*SB*SW*TB) + (0.0625D0*CA22*EL*m12squared*SA1*SA3*&
  &dAlpha1KanAlter())/(MW*SB*SW*TB) - (0.0625D0*EL*m12squared*SA1*SA22*SA3*dAlpha1KanAlter())/(MW*SB*SW*TB) + (0.09375D0*CA3*EL*m&
  &12squared*SA1*SA2*TB*dAlpha1KanAlter())/(CB*MW*SW) + (0.28125D0*CA22*CA3*EL*m12squared*SA1*SA2*TB*dAlpha1KanAlter())/(CB*MW*SW&
  &) + (0.0625D0*CA1*EL*m12squared*SA3*TB*dAlpha1KanAlter())/(CB*MW*SW) + (0.0625D0*CA1*CA22*EL*m12squared*SA3*TB*dAlpha1KanAlter&
  &())/(CB*MW*SW) - (0.0625D0*CA1*EL*m12squared*SA22*SA3*TB*dAlpha1KanAlter())/(CB*MW*SW) + (0.1875D0*CA1*CA2*CA3*EL*MH12*dAlpha2&
  &KanAlter())/(CB*MW*SW) + (0.09375D0*CA1*CA2*CA3*EL*MH32*dAlpha2KanAlter())/(CB*MW*SW) + (0.28125D0*CA2*CA3*EL*m12squared*SA1*d&
  &Alpha2KanAlter())/(CB*MW*SW) - (0.1875D0*CA1*CA2*CA3*EL*MH12*SA12*dAlpha2KanAlter())/(CB*MW*SW) - (0.09375D0*CA1*CA2*CA3*EL*MH&
  &32*SA12*dAlpha2KanAlter())/(CB*MW*SW) - (1.6875D0*CA1*CA2*CA3*EL*MH12*SA22*dAlpha2KanAlter())/(CB*MW*SW) - (0.84375D0*CA1*CA2*&
  &CA3*EL*MH32*SA22*dAlpha2KanAlter())/(CB*MW*SW) - (2.53125D0*CA2*CA3*EL*m12squared*SA1*SA22*dAlpha2KanAlter())/(CB*MW*SW) + (1.&
  &6875D0*CA1*CA2*CA3*EL*MH12*SA12*SA22*dAlpha2KanAlter())/(CB*MW*SW) + (0.84375D0*CA1*CA2*CA3*EL*MH32*SA12*SA22*dAlpha2KanAlter(&
  &))/(CB*MW*SW) - (0.75D0*CA1*CA2*EL*m12squared*SA2*SA3*dAlpha2KanAlter())/(CB*MW*SW) + (0.5D0*CA2*EL*MH12*SA1*SA2*SA3*dAlpha2Ka&
  &nAlter())/(CB*MW*SW) + (1.5D0*CA12*CA2*EL*MH12*SA1*SA2*SA3*dAlpha2KanAlter())/(CB*MW*SW) + (0.25D0*CA2*EL*MH32*SA1*SA2*SA3*dAl&
  &pha2KanAlter())/(CB*MW*SW) + (0.75D0*CA12*CA2*EL*MH32*SA1*SA2*SA3*dAlpha2KanAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA2*CA3*EL*m1&
  &2squared*dAlpha2KanAlter())/(MW*SB*SW) - (0.1875D0*CA1*CA2*CA3*EL*m12squared*dAlpha2KanAlter())/(CB2*MW*SB*SW) + (0.1875D0*CA2&
  &*CA3*EL*MH12*SA1*dAlpha2KanAlter())/(MW*SB*SW) - (0.1875D0*CA12*CA2*CA3*EL*MH12*SA1*dAlpha2KanAlter())/(MW*SB*SW) + (0.09375D0&
  &*CA2*CA3*EL*MH32*SA1*dAlpha2KanAlter())/(MW*SB*SW) - (0.09375D0*CA12*CA2*CA3*EL*MH32*SA1*dAlpha2KanAlter())/(MW*SB*SW) + (0.28&
  &125D0*CA1*CA2*CA3*EL*m12squared*SA12*dAlpha2KanAlter())/(CB2*MW*SB*SW) - (2.53125D0*CA1*CA2*CA3*EL*m12squared*SA22*dAlpha2KanA&
  &lter())/(MW*SB*SW) + (1.6875D0*CA1*CA2*CA3*EL*m12squared*SA22*dAlpha2KanAlter())/(CB2*MW*SB*SW) - (1.6875D0*CA2*CA3*EL*MH12*SA&
  &1*SA22*dAlpha2KanAlter())/(MW*SB*SW) + (1.6875D0*CA12*CA2*CA3*EL*MH12*SA1*SA22*dAlpha2KanAlter())/(MW*SB*SW) - (0.84375D0*CA2*&
  &CA3*EL*MH32*SA1*SA22*dAlpha2KanAlter())/(MW*SB*SW) + (0.84375D0*CA12*CA2*CA3*EL*MH32*SA1*SA22*dAlpha2KanAlter())/(MW*SB*SW) - &
  &(2.53125D0*CA1*CA2*CA3*EL*m12squared*SA12*SA22*dAlpha2KanAlter())/(CB2*MW*SB*SW) - (0.5D0*CA1*CA2*EL*MH12*SA2*SA3*dAlpha2KanAl&
  &ter())/(MW*SB*SW) - (0.25D0*CA1*CA2*EL*MH32*SA2*SA3*dAlpha2KanAlter())/(MW*SB*SW) + (0.75D0*CA2*EL*m12squared*SA1*SA2*SA3*dAlp&
  &ha2KanAlter())/(MW*SB*SW) - (0.5D0*CA2*EL*m12squared*SA1*SA2*SA3*dAlpha2KanAlter())/(CB2*MW*SB*SW) - (2.25D0*CA12*CA2*EL*m12sq&
  &uared*SA1*SA2*SA3*dAlpha2KanAlter())/(CB2*MW*SB*SW) - (1.5D0*CA1*CA2*EL*MH12*SA12*SA2*SA3*dAlpha2KanAlter())/(MW*SB*SW) - (0.7&
  &5D0*CA1*CA2*EL*MH32*SA12*SA2*SA3*dAlpha2KanAlter())/(MW*SB*SW) - (0.1875D0*CA2*CA3*EL*m12squared*SA1*dAlpha2KanAlter())/(CB*MW&
  &*SB2*SW) + (0.28125D0*CA12*CA2*CA3*EL*m12squared*SA1*dAlpha2KanAlter())/(CB*MW*SB2*SW) + (1.6875D0*CA2*CA3*EL*m12squared*SA1*S&
  &A22*dAlpha2KanAlter())/(CB*MW*SB2*SW) - (2.53125D0*CA12*CA2*CA3*EL*m12squared*SA1*SA22*dAlpha2KanAlter())/(CB*MW*SB2*SW) + (0.&
  &5D0*CA1*CA2*EL*m12squared*SA2*SA3*dAlpha2KanAlter())/(CB*MW*SB2*SW) + (2.25D0*CA1*CA2*EL*m12squared*SA12*SA2*SA3*dAlpha2KanAlt&
  &er())/(CB*MW*SB2*SW) - (0.09375D0*CA2*CA3*EL*m12squared*SA1*dAlpha2KanAlter())/(MW*SB*SW*TB) + (0.84375D0*CA2*CA3*EL*m12square&
  &d*SA1*SA22*dAlpha2KanAlter())/(MW*SB*SW*TB) + (0.25D0*CA1*CA2*EL*m12squared*SA2*SA3*dAlpha2KanAlter())/(MW*SB*SW*TB) - (0.0937&
  &5D0*CA1*CA2*CA3*EL*m12squared*TB*dAlpha2KanAlter())/(CB*MW*SW) + (0.84375D0*CA1*CA2*CA3*EL*m12squared*SA22*TB*dAlpha2KanAlter(&
  &))/(CB*MW*SW) - (0.25D0*CA2*EL*m12squared*SA1*SA2*SA3*TB*dAlpha2KanAlter())/(CB*MW*SW) + (0.5D0*CA3*MH12*SA2*dAlpha2KanAlter()&
  &)/vS - (4.5D0*CA22*CA3*MH12*SA2*dAlpha2KanAlter())/vS + (0.25D0*CA3*MH32*SA2*dAlpha2KanAlter())/vS - (2.25D0*CA22*CA3*MH32*SA2&
  &*dAlpha2KanAlter())/vS + (0.1875D0*CA1*CA3*EL*m12squared*dAlpha3KanAlter())/(CB*MW*SW) + (0.1875D0*CA1*CA22*CA3*EL*m12squared*&
  &dAlpha3KanAlter())/(CB*MW*SW) - (0.125D0*CA3*EL*MH12*SA1*dAlpha3KanAlter())/(CB*MW*SW) - (0.375D0*CA12*CA3*EL*MH12*SA1*dAlpha3&
  &KanAlter())/(CB*MW*SW) - (0.125D0*CA22*CA3*EL*MH12*SA1*dAlpha3KanAlter())/(CB*MW*SW) - (0.375D0*CA12*CA22*CA3*EL*MH12*SA1*dAlp&
  &ha3KanAlter())/(CB*MW*SW) - (0.0625D0*CA3*EL*MH32*SA1*dAlpha3KanAlter())/(CB*MW*SW) - (0.1875D0*CA12*CA3*EL*MH32*SA1*dAlpha3Ka&
  &nAlter())/(CB*MW*SW) - (0.0625D0*CA22*CA3*EL*MH32*SA1*dAlpha3KanAlter())/(CB*MW*SW) - (0.1875D0*CA12*CA22*CA3*EL*MH32*SA1*dAlp&
  &ha3KanAlter())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*m12squared*SA22*dAlpha3KanAlter())/(CB*MW*SW) + (0.125D0*CA3*EL*MH12*SA1*SA22&
  &*dAlpha3KanAlter())/(CB*MW*SW) + (0.375D0*CA12*CA3*EL*MH12*SA1*SA22*dAlpha3KanAlter())/(CB*MW*SW) + (0.0625D0*CA3*EL*MH32*SA1*&
  &SA22*dAlpha3KanAlter())/(CB*MW*SW) + (0.1875D0*CA12*CA3*EL*MH32*SA1*SA22*dAlpha3KanAlter())/(CB*MW*SW) - (0.1875D0*CA1*EL*MH12&
  &*SA2*SA3*dAlpha3KanAlter())/(CB*MW*SW) - (0.5625D0*CA1*CA22*EL*MH12*SA2*SA3*dAlpha3KanAlter())/(CB*MW*SW) - (0.09375D0*CA1*EL*&
  &MH32*SA2*SA3*dAlpha3KanAlter())/(CB*MW*SW) - (0.28125D0*CA1*CA22*EL*MH32*SA2*SA3*dAlpha3KanAlter())/(CB*MW*SW) - (0.28125D0*EL&
  &*m12squared*SA1*SA2*SA3*dAlpha3KanAlter())/(CB*MW*SW) - (0.84375D0*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha3KanAlter())/(CB*MW*SW&
  &) + (0.1875D0*CA1*EL*MH12*SA12*SA2*SA3*dAlpha3KanAlter())/(CB*MW*SW) + (0.5625D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dAlpha3KanAlter&
  &())/(CB*MW*SW) + (0.09375D0*CA1*EL*MH32*SA12*SA2*SA3*dAlpha3KanAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*MH32*SA12*SA2*SA3*&
  &dAlpha3KanAlter())/(CB*MW*SW) + (0.125D0*CA1*CA3*EL*MH12*dAlpha3KanAlter())/(MW*SB*SW) + (0.125D0*CA1*CA22*CA3*EL*MH12*dAlpha3&
  &KanAlter())/(MW*SB*SW) + (0.0625D0*CA1*CA3*EL*MH32*dAlpha3KanAlter())/(MW*SB*SW) + (0.0625D0*CA1*CA22*CA3*EL*MH32*dAlpha3KanAl&
  &ter())/(MW*SB*SW) - (0.1875D0*CA3*EL*m12squared*SA1*dAlpha3KanAlter())/(MW*SB*SW) - (0.1875D0*CA22*CA3*EL*m12squared*SA1*dAlph&
  &a3KanAlter())/(MW*SB*SW) + (0.125D0*CA3*EL*m12squared*SA1*dAlpha3KanAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA12*CA3*EL*m12squared&
  &*SA1*dAlpha3KanAlter())/(CB2*MW*SB*SW) + (0.125D0*CA22*CA3*EL*m12squared*SA1*dAlpha3KanAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA1&
  &2*CA22*CA3*EL*m12squared*SA1*dAlpha3KanAlter())/(CB2*MW*SB*SW) + (0.375D0*CA1*CA3*EL*MH12*SA12*dAlpha3KanAlter())/(MW*SB*SW) +&
  & (0.375D0*CA1*CA22*CA3*EL*MH12*SA12*dAlpha3KanAlter())/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*MH32*SA12*dAlpha3KanAlter())/(MW*SB*S&
  &W) + (0.1875D0*CA1*CA22*CA3*EL*MH32*SA12*dAlpha3KanAlter())/(MW*SB*SW) - (0.125D0*CA1*CA3*EL*MH12*SA22*dAlpha3KanAlter())/(MW*&
  &SB*SW) - (0.0625D0*CA1*CA3*EL*MH32*SA22*dAlpha3KanAlter())/(MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA1*SA22*dAlpha3KanAlter()&
  &)/(MW*SB*SW) - (0.125D0*CA3*EL*m12squared*SA1*SA22*dAlpha3KanAlter())/(CB2*MW*SB*SW) - (0.5625D0*CA12*CA3*EL*m12squared*SA1*SA&
  &22*dAlpha3KanAlter())/(CB2*MW*SB*SW) - (0.375D0*CA1*CA3*EL*MH12*SA12*SA22*dAlpha3KanAlter())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL&
  &*MH32*SA12*SA22*dAlpha3KanAlter())/(MW*SB*SW) - (0.28125D0*CA1*EL*m12squared*SA2*SA3*dAlpha3KanAlter())/(MW*SB*SW) - (0.84375D&
  &0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha3KanAlter())/(MW*SB*SW) + (0.1875D0*CA1*EL*m12squared*SA2*SA3*dAlpha3KanAlter())/(CB2*M&
  &W*SB*SW) + (0.5625D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha3KanAlter())/(CB2*MW*SB*SW) - (0.1875D0*EL*MH12*SA1*SA2*SA3*dAlpha3K&
  &anAlter())/(MW*SB*SW) + (0.1875D0*CA12*EL*MH12*SA1*SA2*SA3*dAlpha3KanAlter())/(MW*SB*SW) - (0.5625D0*CA22*EL*MH12*SA1*SA2*SA3*&
  &dAlpha3KanAlter())/(MW*SB*SW) + (0.5625D0*CA12*CA22*EL*MH12*SA1*SA2*SA3*dAlpha3KanAlter())/(MW*SB*SW) - (0.09375D0*EL*MH32*SA1&
  &*SA2*SA3*dAlpha3KanAlter())/(MW*SB*SW) + (0.09375D0*CA12*EL*MH32*SA1*SA2*SA3*dAlpha3KanAlter())/(MW*SB*SW) - (0.28125D0*CA22*E&
  &L*MH32*SA1*SA2*SA3*dAlpha3KanAlter())/(MW*SB*SW) + (0.28125D0*CA12*CA22*EL*MH32*SA1*SA2*SA3*dAlpha3KanAlter())/(MW*SB*SW) - (0&
  &.28125D0*CA1*EL*m12squared*SA12*SA2*SA3*dAlpha3KanAlter())/(CB2*MW*SB*SW) - (0.84375D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*dAl&
  &pha3KanAlter())/(CB2*MW*SB*SW) - (0.125D0*CA1*CA3*EL*m12squared*dAlpha3KanAlter())/(CB*MW*SB2*SW) - (0.125D0*CA1*CA22*CA3*EL*m&
  &12squared*dAlpha3KanAlter())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA3*EL*m12squared*SA12*dAlpha3KanAlter())/(CB*MW*SB2*SW) - (0.5625&
  &D0*CA1*CA22*CA3*EL*m12squared*SA12*dAlpha3KanAlter())/(CB*MW*SB2*SW) + (0.125D0*CA1*CA3*EL*m12squared*SA22*dAlpha3KanAlter())/&
  &(CB*MW*SB2*SW) + (0.5625D0*CA1*CA3*EL*m12squared*SA12*SA22*dAlpha3KanAlter())/(CB*MW*SB2*SW) + (0.1875D0*EL*m12squared*SA1*SA2&
  &*SA3*dAlpha3KanAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA12*EL*m12squared*SA1*SA2*SA3*dAlpha3KanAlter())/(CB*MW*SB2*SW) + (0.5625&
  &D0*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha3KanAlter())/(CB*MW*SB2*SW) - (0.84375D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha3Ka&
  &nAlter())/(CB*MW*SB2*SW) - (0.0625D0*CA1*CA3*EL*m12squared*dAlpha3KanAlter())/(MW*SB*SW*TB) - (0.0625D0*CA1*CA22*CA3*EL*m12squ&
  &ared*dAlpha3KanAlter())/(MW*SB*SW*TB) + (0.0625D0*CA1*CA3*EL*m12squared*SA22*dAlpha3KanAlter())/(MW*SB*SW*TB) + (0.09375D0*EL*&
  &m12squared*SA1*SA2*SA3*dAlpha3KanAlter())/(MW*SB*SW*TB) + (0.28125D0*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha3KanAlter())/(MW*SB*&
  &SW*TB) + (0.0625D0*CA3*EL*m12squared*SA1*TB*dAlpha3KanAlter())/(CB*MW*SW) + (0.0625D0*CA22*CA3*EL*m12squared*SA1*TB*dAlpha3Kan&
  &Alter())/(CB*MW*SW) - (0.0625D0*CA3*EL*m12squared*SA1*SA22*TB*dAlpha3KanAlter())/(CB*MW*SW) + (0.09375D0*CA1*EL*m12squared*SA2&
  &*SA3*TB*dAlpha3KanAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*m12squared*SA2*SA3*TB*dAlpha3KanAlter())/(CB*MW*SW) + (0.5D0*CA&
  &2*MH12*SA3*dAlpha3KanAlter())/vS + (0.25D0*CA2*MH32*SA3*dAlpha3KanAlter())/vS + (1.5D0*CA2*MH12*SA22*SA3*dAlpha3KanAlter())/vS&
  & + (0.75D0*CA2*MH32*SA22*SA3*dAlpha3KanAlter())/vS + (0.09375D0*CA3*EL*MH12*SA1*SA2*dBeta1KanAlter())/(CB*MW*SW) - (0.09375D0*&
  &CA12*CA3*EL*MH12*SA1*SA2*dBeta1KanAlter())/(CB*MW*SW) + (0.28125D0*CA22*CA3*EL*MH12*SA1*SA2*dBeta1KanAlter())/(CB*MW*SW) - (0.&
  &28125D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dBeta1KanAlter())/(CB*MW*SW) + (0.046875D0*CA3*EL*MH32*SA1*SA2*dBeta1KanAlter())/(CB*MW*&
  &SW) - (0.046875D0*CA12*CA3*EL*MH32*SA1*SA2*dBeta1KanAlter())/(CB*MW*SW) + (0.140625D0*CA22*CA3*EL*MH32*SA1*SA2*dBeta1KanAlter(&
  &))/(CB*MW*SW) - (0.140625D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*dBeta1KanAlter())/(CB*MW*SW) + (0.0625D0*CA1*EL*MH12*SA3*dBeta1KanAl&
  &ter())/(CB*MW*SW) + (0.0625D0*CA1*CA22*EL*MH12*SA3*dBeta1KanAlter())/(CB*MW*SW) + (0.03125D0*CA1*EL*MH32*SA3*dBeta1KanAlter())&
  &/(CB*MW*SW) + (0.03125D0*CA1*CA22*EL*MH32*SA3*dBeta1KanAlter())/(CB*MW*SW) + (0.1875D0*CA1*EL*MH12*SA12*SA3*dBeta1KanAlter())/&
  &(CB*MW*SW) + (0.1875D0*CA1*CA22*EL*MH12*SA12*SA3*dBeta1KanAlter())/(CB*MW*SW) + (0.09375D0*CA1*EL*MH32*SA12*SA3*dBeta1KanAlter&
  &())/(CB*MW*SW) + (0.09375D0*CA1*CA22*EL*MH32*SA12*SA3*dBeta1KanAlter())/(CB*MW*SW) - (0.0625D0*CA1*EL*MH12*SA22*SA3*dBeta1KanA&
  &lter())/(CB*MW*SW) - (0.03125D0*CA1*EL*MH32*SA22*SA3*dBeta1KanAlter())/(CB*MW*SW) - (0.1875D0*CA1*EL*MH12*SA12*SA22*SA3*dBeta1&
  &KanAlter())/(CB*MW*SW) - (0.09375D0*CA1*EL*MH32*SA12*SA22*SA3*dBeta1KanAlter())/(CB*MW*SW) + (0.09375D0*CA3*EL*m12squared*SA1*&
  &SA2*dBeta1KanAlter())/(MW*SB*SW) + (0.28125D0*CA22*CA3*EL*m12squared*SA1*SA2*dBeta1KanAlter())/(MW*SB*SW) - (0.328125D0*CA3*EL&
  &*m12squared*SA1*SA2*dBeta1KanAlter())/(CB2*MW*SB*SW) + (0.421875D0*CA12*CA3*EL*m12squared*SA1*SA2*dBeta1KanAlter())/(CB2*MW*SB&
  &*SW) - (0.984375D0*CA22*CA3*EL*m12squared*SA1*SA2*dBeta1KanAlter())/(CB2*MW*SB*SW) + (1.265625D0*CA12*CA22*CA3*EL*m12squared*S&
  &A1*SA2*dBeta1KanAlter())/(CB2*MW*SB*SW) + (0.0625D0*CA1*EL*m12squared*SA3*dBeta1KanAlter())/(MW*SB*SW) + (0.0625D0*CA1*CA22*EL&
  &*m12squared*SA3*dBeta1KanAlter())/(MW*SB*SW) - (0.21875D0*CA1*EL*m12squared*SA3*dBeta1KanAlter())/(CB2*MW*SB*SW) - (0.21875D0*&
  &CA1*CA22*EL*m12squared*SA3*dBeta1KanAlter())/(CB2*MW*SB*SW) - (0.84375D0*CA1*EL*m12squared*SA12*SA3*dBeta1KanAlter())/(CB2*MW*&
  &SB*SW) - (0.84375D0*CA1*CA22*EL*m12squared*SA12*SA3*dBeta1KanAlter())/(CB2*MW*SB*SW) - (0.0625D0*CA1*EL*m12squared*SA22*SA3*dB&
  &eta1KanAlter())/(MW*SB*SW) + (0.21875D0*CA1*EL*m12squared*SA22*SA3*dBeta1KanAlter())/(CB2*MW*SB*SW) + (0.84375D0*CA1*EL*m12squ&
  &ared*SA12*SA22*SA3*dBeta1KanAlter())/(CB2*MW*SB*SW) + (0.09375D0*CA1*CA3*EL*m12squared*SA2*dBeta1KanAlter())/(CB*MW*SB2*SW) + &
  &(0.28125D0*CA1*CA22*CA3*EL*m12squared*SA2*dBeta1KanAlter())/(CB*MW*SB2*SW) - (0.09375D0*CA3*EL*MH12*SA1*SA2*dBeta1KanAlter())/&
  &(CB*MW*SB2*SW) + (0.09375D0*CA12*CA3*EL*MH12*SA1*SA2*dBeta1KanAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA22*CA3*EL*MH12*SA1*SA2*dB&
  &eta1KanAlter())/(CB*MW*SB2*SW) + (0.28125D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dBeta1KanAlter())/(CB*MW*SB2*SW) - (0.046875D0*CA3*E&
  &L*MH32*SA1*SA2*dBeta1KanAlter())/(CB*MW*SB2*SW) + (0.046875D0*CA12*CA3*EL*MH32*SA1*SA2*dBeta1KanAlter())/(CB*MW*SB2*SW) - (0.1&
  &40625D0*CA22*CA3*EL*MH32*SA1*SA2*dBeta1KanAlter())/(CB*MW*SB2*SW) + (0.140625D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*dBeta1KanAlter()&
  &)/(CB*MW*SB2*SW) - (0.28125D0*CA1*CA3*EL*m12squared*SA12*SA2*dBeta1KanAlter())/(CB*MW*SB2*SW) - (0.84375D0*CA1*CA22*CA3*EL*m12&
  &squared*SA12*SA2*dBeta1KanAlter())/(CB*MW*SB2*SW) - (0.0625D0*CA1*EL*MH12*SA3*dBeta1KanAlter())/(CB*MW*SB2*SW) - (0.0625D0*CA1&
  &*CA22*EL*MH12*SA3*dBeta1KanAlter())/(CB*MW*SB2*SW) - (0.03125D0*CA1*EL*MH32*SA3*dBeta1KanAlter())/(CB*MW*SB2*SW) - (0.03125D0*&
  &CA1*CA22*EL*MH32*SA3*dBeta1KanAlter())/(CB*MW*SB2*SW) - (0.0625D0*EL*m12squared*SA1*SA3*dBeta1KanAlter())/(CB*MW*SB2*SW) - (0.&
  &5625D0*CA12*EL*m12squared*SA1*SA3*dBeta1KanAlter())/(CB*MW*SB2*SW) - (0.0625D0*CA22*EL*m12squared*SA1*SA3*dBeta1KanAlter())/(C&
  &B*MW*SB2*SW) - (0.5625D0*CA12*CA22*EL*m12squared*SA1*SA3*dBeta1KanAlter())/(CB*MW*SB2*SW) - (0.1875D0*CA1*EL*MH12*SA12*SA3*dBe&
  &ta1KanAlter())/(CB*MW*SB2*SW) - (0.1875D0*CA1*CA22*EL*MH12*SA12*SA3*dBeta1KanAlter())/(CB*MW*SB2*SW) - (0.09375D0*CA1*EL*MH32*&
  &SA12*SA3*dBeta1KanAlter())/(CB*MW*SB2*SW) - (0.09375D0*CA1*CA22*EL*MH32*SA12*SA3*dBeta1KanAlter())/(CB*MW*SB2*SW) + (0.0625D0*&
  &CA1*EL*MH12*SA22*SA3*dBeta1KanAlter())/(CB*MW*SB2*SW) + (0.03125D0*CA1*EL*MH32*SA22*SA3*dBeta1KanAlter())/(CB*MW*SB2*SW) + (0.&
  &0625D0*EL*m12squared*SA1*SA22*SA3*dBeta1KanAlter())/(CB*MW*SB2*SW) + (0.5625D0*CA12*EL*m12squared*SA1*SA22*SA3*dBeta1KanAlter(&
  &))/(CB*MW*SB2*SW) + (0.1875D0*CA1*EL*MH12*SA12*SA22*SA3*dBeta1KanAlter())/(CB*MW*SB2*SW) + (0.09375D0*CA1*EL*MH32*SA12*SA22*SA&
  &3*dBeta1KanAlter())/(CB*MW*SB2*SW) - (0.1875D0*CA1*CA3*EL*m12squared*SA2*dBeta1KanAlter())/(MW*SB*SW*TB) - (0.5625D0*CA1*CA22*&
  &CA3*EL*m12squared*SA2*dBeta1KanAlter())/(MW*SB*SW*TB) - (0.09375D0*CA3*EL*MH12*SA1*SA2*dBeta1KanAlter())/(MW*SB*SW*TB) + (0.09&
  &375D0*CA12*CA3*EL*MH12*SA1*SA2*dBeta1KanAlter())/(MW*SB*SW*TB) - (0.28125D0*CA22*CA3*EL*MH12*SA1*SA2*dBeta1KanAlter())/(MW*SB*&
  &SW*TB) + (0.28125D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dBeta1KanAlter())/(MW*SB*SW*TB) - (0.046875D0*CA3*EL*MH32*SA1*SA2*dBeta1KanA&
  &lter())/(MW*SB*SW*TB) + (0.046875D0*CA12*CA3*EL*MH32*SA1*SA2*dBeta1KanAlter())/(MW*SB*SW*TB) - (0.140625D0*CA22*CA3*EL*MH32*SA&
  &1*SA2*dBeta1KanAlter())/(MW*SB*SW*TB) + (0.140625D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*dBeta1KanAlter())/(MW*SB*SW*TB) - (0.0625D0*&
  &CA1*EL*MH12*SA3*dBeta1KanAlter())/(MW*SB*SW*TB) - (0.0625D0*CA1*CA22*EL*MH12*SA3*dBeta1KanAlter())/(MW*SB*SW*TB) - (0.03125D0*&
  &CA1*EL*MH32*SA3*dBeta1KanAlter())/(MW*SB*SW*TB) - (0.03125D0*CA1*CA22*EL*MH32*SA3*dBeta1KanAlter())/(MW*SB*SW*TB) + (0.125D0*E&
  &L*m12squared*SA1*SA3*dBeta1KanAlter())/(MW*SB*SW*TB) + (0.125D0*CA22*EL*m12squared*SA1*SA3*dBeta1KanAlter())/(MW*SB*SW*TB) - (&
  &0.1875D0*CA1*EL*MH12*SA12*SA3*dBeta1KanAlter())/(MW*SB*SW*TB) - (0.1875D0*CA1*CA22*EL*MH12*SA12*SA3*dBeta1KanAlter())/(MW*SB*S&
  &W*TB) - (0.09375D0*CA1*EL*MH32*SA12*SA3*dBeta1KanAlter())/(MW*SB*SW*TB) - (0.09375D0*CA1*CA22*EL*MH32*SA12*SA3*dBeta1KanAlter(&
  &))/(MW*SB*SW*TB) + (0.0625D0*CA1*EL*MH12*SA22*SA3*dBeta1KanAlter())/(MW*SB*SW*TB) + (0.03125D0*CA1*EL*MH32*SA22*SA3*dBeta1KanA&
  &lter())/(MW*SB*SW*TB) - (0.125D0*EL*m12squared*SA1*SA22*SA3*dBeta1KanAlter())/(MW*SB*SW*TB) + (0.1875D0*CA1*EL*MH12*SA12*SA22*&
  &SA3*dBeta1KanAlter())/(MW*SB*SW*TB) + (0.09375D0*CA1*EL*MH32*SA12*SA22*SA3*dBeta1KanAlter())/(MW*SB*SW*TB) + (0.1875D0*CA1*CA3&
  &*EL*MH12*SA2*TB*dBeta1KanAlter())/(CB*MW*SW) + (0.5625D0*CA1*CA22*CA3*EL*MH12*SA2*TB*dBeta1KanAlter())/(CB*MW*SW) + (0.09375D0&
  &*CA1*CA3*EL*MH32*SA2*TB*dBeta1KanAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA22*CA3*EL*MH32*SA2*TB*dBeta1KanAlter())/(CB*MW*SW) + (&
  &0.328125D0*CA3*EL*m12squared*SA1*SA2*TB*dBeta1KanAlter())/(CB*MW*SW) + (0.984375D0*CA22*CA3*EL*m12squared*SA1*SA2*TB*dBeta1Kan&
  &Alter())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*MH12*SA12*SA2*TB*dBeta1KanAlter())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*EL*MH12*SA12&
  &*SA2*TB*dBeta1KanAlter())/(CB*MW*SW) - (0.09375D0*CA1*CA3*EL*MH32*SA12*SA2*TB*dBeta1KanAlter())/(CB*MW*SW) - (0.28125D0*CA1*CA&
  &22*CA3*EL*MH32*SA12*SA2*TB*dBeta1KanAlter())/(CB*MW*SW) + (0.21875D0*CA1*EL*m12squared*SA3*TB*dBeta1KanAlter())/(CB*MW*SW) + (&
  &0.21875D0*CA1*CA22*EL*m12squared*SA3*TB*dBeta1KanAlter())/(CB*MW*SW) - (0.125D0*EL*MH12*SA1*SA3*TB*dBeta1KanAlter())/(CB*MW*SW&
  &) - (0.375D0*CA12*EL*MH12*SA1*SA3*TB*dBeta1KanAlter())/(CB*MW*SW) - (0.125D0*CA22*EL*MH12*SA1*SA3*TB*dBeta1KanAlter())/(CB*MW*&
  &SW) - (0.375D0*CA12*CA22*EL*MH12*SA1*SA3*TB*dBeta1KanAlter())/(CB*MW*SW) - (0.0625D0*EL*MH32*SA1*SA3*TB*dBeta1KanAlter())/(CB*&
  &MW*SW) - (0.1875D0*CA12*EL*MH32*SA1*SA3*TB*dBeta1KanAlter())/(CB*MW*SW) - (0.0625D0*CA22*EL*MH32*SA1*SA3*TB*dBeta1KanAlter())/&
  &(CB*MW*SW) - (0.1875D0*CA12*CA22*EL*MH32*SA1*SA3*TB*dBeta1KanAlter())/(CB*MW*SW) - (0.21875D0*CA1*EL*m12squared*SA22*SA3*TB*dB&
  &eta1KanAlter())/(CB*MW*SW) + (0.125D0*EL*MH12*SA1*SA22*SA3*TB*dBeta1KanAlter())/(CB*MW*SW) + (0.375D0*CA12*EL*MH12*SA1*SA22*SA&
  &3*TB*dBeta1KanAlter())/(CB*MW*SW) + (0.0625D0*EL*MH32*SA1*SA22*SA3*TB*dBeta1KanAlter())/(CB*MW*SW) + (0.1875D0*CA12*EL*MH32*SA&
  &1*SA22*SA3*TB*dBeta1KanAlter())/(CB*MW*SW) + (0.140625D0*CA3*EL*m12squared*SA1*SA2*dBeta1KanAlter())/(MW*SB*SW*TB2) + (0.42187&
  &5D0*CA22*CA3*EL*m12squared*SA1*SA2*dBeta1KanAlter())/(MW*SB*SW*TB2) + (0.09375D0*CA1*EL*m12squared*SA3*dBeta1KanAlter())/(MW*S&
  &B*SW*TB2) + (0.09375D0*CA1*CA22*EL*m12squared*SA3*dBeta1KanAlter())/(MW*SB*SW*TB2) - (0.09375D0*CA1*EL*m12squared*SA22*SA3*dBe&
  &ta1KanAlter())/(MW*SB*SW*TB2) - (0.1875D0*CA1*CA3*EL*m12squared*SA2*TB2*dBeta1KanAlter())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*&
  &EL*m12squared*SA2*TB2*dBeta1KanAlter())/(CB*MW*SW) + (0.125D0*EL*m12squared*SA1*SA3*TB2*dBeta1KanAlter())/(CB*MW*SW) + (0.125D&
  &0*CA22*EL*m12squared*SA1*SA3*TB2*dBeta1KanAlter())/(CB*MW*SW) - (0.125D0*EL*m12squared*SA1*SA22*SA3*TB2*dBeta1KanAlter())/(CB*&
  &MW*SW) + (0.125D0*CA2*CA3*MH12*dBeta1KanAlter())/(CB*SB*vS) + (0.0625D0*CA2*CA3*MH32*dBeta1KanAlter())/(CB*SB*vS) + (0.375D0*C&
  &A2*CA3*MH12*SA22*dBeta1KanAlter())/(CB*SB*vS) + (0.1875D0*CA2*CA3*MH32*SA22*dBeta1KanAlter())/(CB*SB*vS) - (0.125D0*CA2*CA3*MH&
  &12*dBeta1KanAlter())/(TB*vS) - (0.0625D0*CA2*CA3*MH32*dBeta1KanAlter())/(TB*vS) - (0.375D0*CA2*CA3*MH12*SA22*dBeta1KanAlter())&
  &/(TB*vS) - (0.1875D0*CA2*CA3*MH32*SA22*dBeta1KanAlter())/(TB*vS) - (0.125D0*CA2*CA3*MH12*TB*dBeta1KanAlter())/vS - (0.0625D0*C&
  &A2*CA3*MH32*TB*dBeta1KanAlter())/vS - (0.375D0*CA2*CA3*MH12*SA22*TB*dBeta1KanAlter())/vS - (0.1875D0*CA2*CA3*MH32*SA22*TB*dBet&
  &a1KanAlter())/vS - (0.375D0*EL*MH12*SA3*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.375D0*CA22*EL*MH12*SA3*dAlpha1&
  &KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*EL*MH32*SA3*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.18&
  &75D0*CA22*EL*MH32*SA3*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.375D0*EL*MH12*SA22*SA3*dAlpha1KanAlter()*DBLE(CA&
  &1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*EL*MH32*SA22*SA3*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA3*EL*M&
  &H12*SA2*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA22*CA3*EL*MH12*SA2*dAlpha1KanAlter()*DBLE(CA1**INT(3.&
  &D0)))/(MW*SB*SW) - (0.09375D0*CA3*EL*MH32*SA2*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA22*CA3*EL*MH32&
  &*SA2*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.5625D0*EL*m12squared*SA3*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/&
  &(CB2*MW*SB*SW) + (0.5625D0*CA22*EL*m12squared*SA3*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*EL*m12squ&
  &ared*SA22*SA3*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.28125D0*CA3*EL*m12squared*SA2*dAlpha1KanAlter()*DBLE&
  &(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA22*CA3*EL*m12squared*SA2*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW&
  &) + (0.0625D0*CA2*CA3*EL*MH12*dAlpha2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA2*CA3*EL*MH32*dAlpha2KanAlter(&
  &)*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.5625D0*CA2*CA3*EL*MH12*SA22*dAlpha2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.28&
  &125D0*CA2*CA3*EL*MH32*SA22*dAlpha2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA2*CA3*EL*m12squared*dAlpha2KanAlt&
  &er()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.84375D0*CA2*CA3*EL*m12squared*SA22*dAlpha2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB2&
  &*MW*SB*SW) + (0.5D0*CA2*EL*MH12*SA2*SA3*dAlpha2KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.25D0*CA2*EL*MH32*SA2*SA3*dAlph&
  &a2KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.75D0*CA2*EL*m12squared*SA2*SA3*dAlpha2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*&
  &MW*SB2*SW) - (0.0625D0*EL*MH12*SA2*SA3*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA22*EL*MH12*SA2*SA3*dAl&
  &pha3KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*EL*MH32*SA2*SA3*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW&
  &) - (0.09375D0*CA22*EL*MH32*SA2*SA3*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.125D0*CA3*EL*MH12*dAlpha3KanAlter(&
  &)*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.125D0*CA22*CA3*EL*MH12*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.0625D0*&
  &CA3*EL*MH32*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.0625D0*CA22*CA3*EL*MH32*dAlpha3KanAlter()*DBLE(CA1**INT(3.&
  &D0)))/(MW*SB*SW) + (0.125D0*CA3*EL*MH12*SA22*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*CA3*EL*MH32*SA22*d&
  &Alpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*EL*m12squared*SA2*SA3*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(&
  &CB2*MW*SB*SW) + (0.28125D0*CA22*EL*m12squared*SA2*SA3*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*CA3*E&
  &L*m12squared*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*CA22*CA3*EL*m12squared*dAlpha3KanAlter()*DBLE(&
  &CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.1875D0*CA3*EL*m12squared*SA22*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0&
  &.0625D0*EL*MH12*SA3*dBeta1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.0625D0*CA22*EL*MH12*SA3*dBeta1KanAlter()*DBLE(CA1**&
  &INT(3.D0)))/(CB*MW*SW) - (0.03125D0*EL*MH32*SA3*dBeta1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*CA22*EL*MH32*SA&
  &3*dBeta1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*EL*MH12*SA22*SA3*dBeta1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW&
  &*SW) + (0.03125D0*EL*MH32*SA22*SA3*dBeta1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*EL*m12squared*SA3*dBeta1KanA&
  &lter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.28125D0*CA22*EL*m12squared*SA3*dBeta1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW&
  &*SB*SW) - (0.28125D0*EL*m12squared*SA22*SA3*dBeta1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.09375D0*CA3*EL*m12squar&
  &ed*SA2*dBeta1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA22*CA3*EL*m12squared*SA2*dBeta1KanAlter()*DBLE(CA1&
  &**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*EL*MH12*SA3*dBeta1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*CA22*EL&
  &*MH12*SA3*dBeta1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.03125D0*EL*MH32*SA3*dBeta1KanAlter()*DBLE(CA1**INT(3.D0))&
  &)/(CB*MW*SB2*SW) + (0.03125D0*CA22*EL*MH32*SA3*dBeta1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.0625D0*EL*MH12*SA22*&
  &SA3*dBeta1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.03125D0*EL*MH32*SA22*SA3*dBeta1KanAlter()*DBLE(CA1**INT(3.D0)))&
  &/(CB*MW*SB2*SW) + (0.0625D0*EL*MH12*SA3*dBeta1KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.0625D0*CA22*EL*MH12*SA3*dBet&
  &a1KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.03125D0*EL*MH32*SA3*dBeta1KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB)&
  & + (0.03125D0*CA22*EL*MH32*SA3*dBeta1KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.0625D0*EL*MH12*SA22*SA3*dBeta1KanAlte&
  &r()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.03125D0*EL*MH32*SA22*SA3*dBeta1KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (&
  &0.0625D0*CA3*EL*MH12*SA2*TB*dBeta1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA22*CA3*EL*MH12*SA2*TB*dBeta1KanAlt&
  &er()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA3*EL*MH32*SA2*TB*dBeta1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.0&
  &9375D0*CA22*CA3*EL*MH32*SA2*TB*dBeta1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA1*CA3*EL*MH12*dAlpha2KanAlter()&
  &*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA1*CA3*EL*MH32*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.84375D0&
  &*CA3*EL*m12squared*SA1*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.5625D0*CA1*CA3*EL*MH12*SA12*dAlpha2KanAlter()*D&
  &BLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA1*CA3*EL*MH32*SA12*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.8437&
  &5D0*CA1*CA3*EL*m12squared*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA1*CA3*EL*m12squared*dAlpha2KanAlter&
  &()*DBLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.5625D0*CA3*EL*MH12*SA1*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.56&
  &25D0*CA12*CA3*EL*MH12*SA1*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*CA3*EL*MH32*SA1*dAlpha2KanAlter()*DB&
  &LE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA12*CA3*EL*MH32*SA1*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) + (0.84375&
  &D0*CA1*CA3*EL*m12squared*SA12*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA3*EL*m12squared*SA1*dAlpha2&
  &KanAlter()*DBLE(CA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA3*EL*m12squared*SA1*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0))&
  &)/(CB*MW*SB2*SW) - (0.28125D0*CA3*EL*m12squared*SA1*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.28125D0*CA1*CA3&
  &*EL*m12squared*TB*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.5D0*MH12*SA3*dAlpha3KanAlter()*DBLE(CA2**INT(3.D0)))&
  &/vS - (0.25D0*MH32*SA3*dAlpha3KanAlter()*DBLE(CA2**INT(3.D0)))/vS - (0.125D0*CA3*MH12*dBeta1KanAlter()*DBLE(CA2**INT(3.D0)))/(&
  &CB*SB*vS) - (0.0625D0*CA3*MH32*dBeta1KanAlter()*DBLE(CA2**INT(3.D0)))/(CB*SB*vS) + (0.125D0*CA3*MH12*dBeta1KanAlter()*DBLE(CA2&
  &**INT(3.D0)))/(TB*vS) + (0.0625D0*CA3*MH32*dBeta1KanAlter()*DBLE(CA2**INT(3.D0)))/(TB*vS) + (0.125D0*CA3*MH12*TB*dBeta1KanAlte&
  &r()*DBLE(CA2**INT(3.D0)))/vS + (0.0625D0*CA3*MH32*TB*dBeta1KanAlter()*DBLE(CA2**INT(3.D0)))/vS + (0.1875D0*CA3*EL*MH12*dAlpha2&
  &KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA3*EL*MH32*dAlpha2KanAlter()*DBLE(CA1**INT(3.D0&
  &))*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA3*EL*m12squared*dAlpha2KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0))&
  &)/(CB2*MW*SB*SW) - (0.375D0*CA1*CA3*EL*m12squared*SA2*dBeta1KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (1.125D0*CA1*CA22*CA3*E&
  &L*m12squared*SA2*dBeta1KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.5625D0*CA1*CA3*EL*m12squared*SA12*SA2*dBeta1KanAlter()*DB&
  &LE(CB**INT(-3.D0)))/(MW*SW) + (1.6875D0*CA1*CA22*CA3*EL*m12squared*SA12*SA2*dBeta1KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (&
  &0.25D0*EL*m12squared*SA1*SA3*dBeta1KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (1.125D0*CA12*EL*m12squared*SA1*SA3*dBeta1KanAlt&
  &er()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.25D0*CA22*EL*m12squared*SA1*SA3*dBeta1KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (1.12&
  &5D0*CA12*CA22*EL*m12squared*SA1*SA3*dBeta1KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.25D0*EL*m12squared*SA1*SA22*SA3*dBeta1&
  &KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (1.125D0*CA12*EL*m12squared*SA1*SA22*SA3*dBeta1KanAlter()*DBLE(CB**INT(-3.D0)))/(MW&
  &*SW) - (0.1875D0*CA3*EL*m12squared*SA2*dBeta1KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.5625D0*CA22*CA&
  &3*EL*m12squared*SA2*dBeta1KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.1875D0*CA3*EL*MH12*SA2*dAlpha1Kan&
  &Alter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA22*CA3*EL*MH12*SA2*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) +&
  & (0.09375D0*CA3*EL*MH32*SA2*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA22*CA3*EL*MH32*SA2*dAlpha1KanAlt&
  &er()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA3*EL*m12squared*SA2*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW&
  &) - (0.84375D0*CA22*CA3*EL*m12squared*SA2*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.375D0*EL*MH12*SA3*dAlpha&
  &1KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.375D0*CA22*EL*MH12*SA3*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - &
  &(0.1875D0*EL*MH32*SA3*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA22*EL*MH32*SA3*dAlpha1KanAlter()*DBLE(S&
  &A1**INT(3.D0)))/(MW*SB*SW) + (0.375D0*EL*MH12*SA22*SA3*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*EL*MH32*&
  &SA22*SA3*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.5625D0*EL*m12squared*SA3*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0&
  &)))/(CB*MW*SB2*SW) + (0.5625D0*CA22*EL*m12squared*SA3*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.5625D0*EL*m1&
  &2squared*SA22*SA3*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.5D0*CA2*EL*MH12*SA2*SA3*dAlpha2KanAlter()*DBLE(S&
  &A1**INT(3.D0)))/(CB*MW*SW) - (0.25D0*CA2*EL*MH32*SA2*SA3*dAlpha2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA2*CA&
  &3*EL*MH12*dAlpha2KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA2*CA3*EL*MH32*dAlpha2KanAlter()*DBLE(SA1**INT(3.D0&
  &)))/(MW*SB*SW) - (0.5625D0*CA2*CA3*EL*MH12*SA22*dAlpha2KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA2*CA3*EL*MH3&
  &2*SA22*dAlpha2KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.75D0*CA2*EL*m12squared*SA2*SA3*dAlpha2KanAlter()*DBLE(SA1**INT(&
  &3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA2*CA3*EL*m12squared*dAlpha2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*&
  &CA2*CA3*EL*m12squared*SA22*dAlpha2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.125D0*CA3*EL*MH12*dAlpha3KanAlter()*DBL&
  &E(SA1**INT(3.D0)))/(CB*MW*SW) + (0.125D0*CA22*CA3*EL*MH12*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA3*E&
  &L*MH32*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA22*CA3*EL*MH32*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))&
  &/(CB*MW*SW) - (0.125D0*CA3*EL*MH12*SA22*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.0625D0*CA3*EL*MH32*SA22*dAlpha&
  &3KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA3*EL*m12squared*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*&
  &SW) - (0.1875D0*CA22*CA3*EL*m12squared*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA&
  &22*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D0*EL*MH12*SA2*SA3*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/&
  &(MW*SB*SW) - (0.1875D0*CA22*EL*MH12*SA2*SA3*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.03125D0*EL*MH32*SA2*SA3*dA&
  &lpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA22*EL*MH32*SA2*SA3*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(MW&
  &*SB*SW) + (0.09375D0*EL*m12squared*SA2*SA3*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA22*EL*m12squa&
  &red*SA2*SA3*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.03125D0*CA3*EL*MH12*SA2*dBeta1KanAlter()*DBLE(SA1**INT&
  &(3.D0)))/(CB*MW*SW) + (0.09375D0*CA22*CA3*EL*MH12*SA2*dBeta1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.015625D0*CA3*EL*M&
  &H32*SA2*dBeta1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.046875D0*CA22*CA3*EL*MH32*SA2*dBeta1KanAlter()*DBLE(SA1**INT(3.&
  &D0)))/(CB*MW*SW) - (0.140625D0*CA3*EL*m12squared*SA2*dBeta1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.421875D0*CA22*&
  &CA3*EL*m12squared*SA2*dBeta1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.03125D0*CA3*EL*MH12*SA2*dBeta1KanAlter()*DBLE&
  &(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.09375D0*CA22*CA3*EL*MH12*SA2*dBeta1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.&
  &015625D0*CA3*EL*MH32*SA2*dBeta1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.046875D0*CA22*CA3*EL*MH32*SA2*dBeta1KanAlt&
  &er()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*EL*m12squared*SA3*dBeta1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) &
  &+ (0.1875D0*CA22*EL*m12squared*SA3*dBeta1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.1875D0*EL*m12squared*SA22*SA3*dB&
  &eta1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.03125D0*CA3*EL*MH12*SA2*dBeta1KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB&
  &*SW*TB) - (0.09375D0*CA22*CA3*EL*MH12*SA2*dBeta1KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.015625D0*CA3*EL*MH32*SA2*d&
  &Beta1KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.046875D0*CA22*CA3*EL*MH32*SA2*dBeta1KanAlter()*DBLE(SA1**INT(3.D0)))/&
  &(MW*SB*SW*TB) + (0.125D0*EL*MH12*SA3*TB*dBeta1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.125D0*CA22*EL*MH12*SA3*TB*dBeta&
  &1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*EL*MH32*SA3*TB*dBeta1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0&
  &.0625D0*CA22*EL*MH32*SA3*TB*dBeta1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.125D0*EL*MH12*SA22*SA3*TB*dBeta1KanAlter()*&
  &DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.0625D0*EL*MH32*SA22*SA3*TB*dBeta1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0&
  &*CA3*EL*MH12*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA3*EL*MH32*dAlpha2KanAlter(&
  &)*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0))*&
  &DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.375D0*EL*m12squared*SA3*dBeta1KanAlter()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/&
  &(MW*SW) - (0.375D0*CA22*EL*m12squared*SA3*dBeta1KanAlter()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) + (0.375D0*EL*m1&
  &2squared*SA22*SA3*dBeta1KanAlter()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) - (0.28125D0*CA1*CA3*EL*m12squared*dAlph&
  &a1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA3*EL*MH12*SA1*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) +&
  & (0.5625D0*CA12*CA3*EL*MH12*SA1*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA3*EL*MH32*SA1*dAlpha1KanAlte&
  &r()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA12*CA3*EL*MH32*SA1*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0&
  &.1875D0*CA1*CA3*EL*MH12*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA1*CA3*EL*MH32*dAlpha1KanAlter()*DBLE&
  &(SA2**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*CA3*EL*m12squared*SA1*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.1875D0&
  &*CA3*EL*m12squared*SA1*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.84375D0*CA12*CA3*EL*m12squared*SA1*dAlpha1K&
  &anAlter()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA3*EL*MH12*SA12*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB&
  &*SW) - (0.28125D0*CA1*CA3*EL*MH32*SA12*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*m12squared*dA&
  &lpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA1*CA3*EL*m12squared*SA12*dAlpha1KanAlter()*DBLE(SA2**INT(3&
  &.D0)))/(CB*MW*SB2*SW) + (0.09375D0*CA1*CA3*EL*m12squared*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA&
  &3*EL*m12squared*SA1*TB*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (1.5D0*CA3*MH12*dAlpha2KanAlter()*DBLE(SA2**INT(3.&
  &D0)))/vS + (0.75D0*CA3*MH32*dAlpha2KanAlter()*DBLE(SA2**INT(3.D0)))/vS + (0.1875D0*CA1*EL*MH12*SA3*dAlpha3KanAlter()*DBLE(SA2*&
  &*INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1*EL*MH32*SA3*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*EL*m12squ&
  &ared*SA1*SA3*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA1*EL*MH12*SA12*SA3*dAlpha3KanAlter()*DBLE(SA2**I&
  &NT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1*EL*MH32*SA12*SA3*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA1*EL&
  &*m12squared*SA3*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA1*EL*m12squared*SA3*dAlpha3KanAlter()*DBLE(SA&
  &2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*EL*MH12*SA1*SA3*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA12*&
  &EL*MH12*SA1*SA3*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*EL*MH32*SA1*SA3*dAlpha3KanAlter()*DBLE(SA2**IN&
  &T(3.D0)))/(MW*SB*SW) - (0.09375D0*CA12*EL*MH32*SA1*SA3*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*CA1*EL*&
  &m12squared*SA12*SA3*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*EL*m12squared*SA1*SA3*dAlpha3KanAlter()&
  &*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA12*EL*m12squared*SA1*SA3*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*S&
  &B2*SW) - (0.09375D0*EL*m12squared*SA1*SA3*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA1*EL*m12squared&
  &*SA3*TB*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA3*EL*MH12*SA1*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))&
  &/(CB*MW*SW) + (0.09375D0*CA12*CA3*EL*MH12*SA1*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.046875D0*CA3*EL*MH32*SA1*&
  &dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.046875D0*CA12*CA3*EL*MH32*SA1*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/(C&
  &B*MW*SW) - (0.09375D0*CA3*EL*m12squared*SA1*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.328125D0*CA3*EL*m12squared*&
  &SA1*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.421875D0*CA12*CA3*EL*m12squared*SA1*dBeta1KanAlter()*DBLE(SA2**&
  &INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA1*CA3*EL*m12squared*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.09375&
  &D0*CA3*EL*MH12*SA1*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.09375D0*CA12*CA3*EL*MH12*SA1*dBeta1KanAlter()*DB&
  &LE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.046875D0*CA3*EL*MH32*SA1*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.04&
  &6875D0*CA12*CA3*EL*MH32*SA1*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA1*CA3*EL*m12squared*SA12*dBet&
  &a1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*CA1*CA3*EL*m12squared*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/(MW&
  &*SB*SW*TB) + (0.09375D0*CA3*EL*MH12*SA1*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA12*CA3*EL*MH12*SA1&
  &*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.046875D0*CA3*EL*MH32*SA1*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/(MW&
  &*SB*SW*TB) - (0.046875D0*CA12*CA3*EL*MH32*SA1*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.1875D0*CA1*CA3*EL*MH12&
  &*TB*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1*CA3*EL*MH32*TB*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/(&
  &CB*MW*SW) - (0.328125D0*CA3*EL*m12squared*SA1*TB*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA1*CA3*EL*MH12&
  &*SA12*TB*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1*CA3*EL*MH32*SA12*TB*dBeta1KanAlter()*DBLE(SA2**INT&
  &(3.D0)))/(CB*MW*SW) - (0.140625D0*CA3*EL*m12squared*SA1*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB2) + (0.1875D0*CA1*&
  &CA3*EL*m12squared*TB2*dBeta1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA3*EL*MH12*dAlpha1KanAlter()*DBLE(CA1**IN&
  &T(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA3*EL*MH32*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))&
  &)/(MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0&
  &625D0*EL*MH12*SA3*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*EL*MH32*SA3*dAlpha3KanA&
  &lter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*EL*m12squared*SA3*dAlpha3KanAlter()*DBLE(CA1**INT(3.&
  &D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA3*EL*m12squared*dBeta1KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3&
  &.D0)))/(CB*MW*SB2*SW) - (0.0625D0*CA3*EL*MH12*TB*dBeta1KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.0&
  &3125D0*CA3*EL*MH32*TB*dBeta1KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.375D0*CA1*CA3*EL*m12squared*&
  &dBeta1KanAlter()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) - (0.5625D0*CA1*CA3*EL*m12squared*SA12*dBeta1KanAlter()*DB&
  &LE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) + (0.1875D0*CA3*EL*m12squared*dBeta1KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(CB**&
  &INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) - (0.1875D0*CA3*EL*MH12*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))&
  &/(CB*MW*SW) - (0.09375D0*CA3*EL*MH32*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA3*&
  &EL*m12squared*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.0625D0*EL*MH12*SA3*dAlpha3KanAl&
  &ter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*EL*MH32*SA3*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0))*DB&
  &LE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*EL*m12squared*SA3*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB&
  &*MW*SB2*SW) - (0.03125D0*CA3*EL*MH12*dBeta1KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.015625D0*CA3*&
  &EL*MH32*dBeta1KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.140625D0*CA3*EL*m12squared*dBeta1KanAlter(&
  &)*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.03125D0*CA3*EL*MH12*dBeta1KanAlter()*DBLE(SA1**INT(3.D0))*DBL&
  &E(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.015625D0*CA3*EL*MH32*dBeta1KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW&
  &*SB2*SW) + (0.03125D0*CA3*EL*MH12*dBeta1KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.015625D0*CA3*&
  &EL*MH32*dBeta1KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.328125D0*CA3*EL*m12squared*SA1*SA2*dBet&
  &a1KanAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.421875D0*CA12*CA3*EL*m12squared*SA1*SA2*dBeta1KanAlter()*DBLE(SB**INT(-3.D0)))&
  &/(MW*SW) + (0.984375D0*CA22*CA3*EL*m12squared*SA1*SA2*dBeta1KanAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (1.265625D0*CA12*CA22*C&
  &A3*EL*m12squared*SA1*SA2*dBeta1KanAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.09375D0*CA3*EL*m12squared*SA1*SA2*dBeta1KanAlter(&
  &)*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.140625D0*CA12*CA3*EL*m12squared*SA1*SA2*dBeta1KanAlter()*DBLE(SB**INT(-3.D0)))/(CB2*M&
  &W*SW) + (0.28125D0*CA22*CA3*EL*m12squared*SA1*SA2*dBeta1KanAlter()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.421875D0*CA12*CA22*C&
  &A3*EL*m12squared*SA1*SA2*dBeta1KanAlter()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.21875D0*CA1*EL*m12squared*SA3*dBeta1KanAlter(&
  &)*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.21875D0*CA1*CA22*EL*m12squared*SA3*dBeta1KanAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.06&
  &25D0*CA1*EL*m12squared*SA3*dBeta1KanAlter()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.0625D0*CA1*CA22*EL*m12squared*SA3*dBeta1Kan&
  &Alter()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.84375D0*CA1*EL*m12squared*SA12*SA3*dBeta1KanAlter()*DBLE(SB**INT(-3.D0)))/(MW*S&
  &W) + (0.84375D0*CA1*CA22*EL*m12squared*SA12*SA3*dBeta1KanAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.28125D0*CA1*EL*m12squared*&
  &SA12*SA3*dBeta1KanAlter()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.28125D0*CA1*CA22*EL*m12squared*SA12*SA3*dBeta1KanAlter()*DBLE&
  &(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.21875D0*CA1*EL*m12squared*SA22*SA3*dBeta1KanAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.0625&
  &D0*CA1*EL*m12squared*SA22*SA3*dBeta1KanAlter()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.84375D0*CA1*EL*m12squared*SA12*SA22*SA3*&
  &dBeta1KanAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.28125D0*CA1*EL*m12squared*SA12*SA22*SA3*dBeta1KanAlter()*DBLE(SB**INT(-3.D&
  &0)))/(CB2*MW*SW) - (0.28125D0*EL*m12squared*SA3*dBeta1KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.28125&
  &D0*CA22*EL*m12squared*SA3*dBeta1KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.09375D0*EL*m12squared*SA3*d&
  &Beta1KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.09375D0*CA22*EL*m12squared*SA3*dBeta1KanAlter()*DB&
  &LE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.28125D0*EL*m12squared*SA22*SA3*dBeta1KanAlter()*DBLE(CA1**INT(3.D0))&
  &*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.09375D0*EL*m12squared*SA22*SA3*dBeta1KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))&
  &/(CB2*MW*SW) + (0.140625D0*CA3*EL*m12squared*SA2*dBeta1KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.4218&
  &75D0*CA22*CA3*EL*m12squared*SA2*dBeta1KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.046875D0*CA3*EL*m12sq&
  &uared*SA2*dBeta1KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.140625D0*CA22*CA3*EL*m12squared*SA2*dBe&
  &ta1KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.328125D0*CA3*EL*m12squared*SA1*dBeta1KanAlter()*DBLE&
  &(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.421875D0*CA12*CA3*EL*m12squared*SA1*dBeta1KanAlter()*DBLE(SA2**INT(3.D0))*&
  &DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.09375D0*CA3*EL*m12squared*SA1*dBeta1KanAlter()*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(&
  &CB2*MW*SW) + (0.140625D0*CA12*CA3*EL*m12squared*SA1*dBeta1KanAlter()*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - &
  &(0.140625D0*CA3*EL*m12squared*dBeta1KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.04&
  &6875D0*CA3*EL*m12squared*dBeta1KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.187&
  &5D0*CA1*CA3*MH12*SA2*dgAtMZ())/(CB*MW) + (0.5625D0*CA1*CA22*CA3*MH12*SA2*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*CA3*MH32*SA2*dgAtM&
  &Z())/(CB*MW) + (0.28125D0*CA1*CA22*CA3*MH32*SA2*dgAtMZ())/(CB*MW) + (0.28125D0*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*MW) + (0.8&
  &4375D0*CA22*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*MW) - (0.1875D0*CA1*CA3*MH12*SA12*SA2*dgAtMZ())/(CB*MW) - (0.5625D0*CA1*CA22*&
  &CA3*MH12*SA12*SA2*dgAtMZ())/(CB*MW) - (0.09375D0*CA1*CA3*MH32*SA12*SA2*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*CA3*MH32*SA12*S&
  &A2*dgAtMZ())/(CB*MW) + (0.1875D0*CA1*m12squared*SA3*dgAtMZ())/(CB*MW) + (0.1875D0*CA1*CA22*m12squared*SA3*dgAtMZ())/(CB*MW) - &
  &(0.125D0*MH12*SA1*SA3*dgAtMZ())/(CB*MW) - (0.375D0*CA12*MH12*SA1*SA3*dgAtMZ())/(CB*MW) - (0.125D0*CA22*MH12*SA1*SA3*dgAtMZ())/&
  &(CB*MW) - (0.375D0*CA12*CA22*MH12*SA1*SA3*dgAtMZ())/(CB*MW) - (0.0625D0*MH32*SA1*SA3*dgAtMZ())/(CB*MW) - (0.1875D0*CA12*MH32*S&
  &A1*SA3*dgAtMZ())/(CB*MW) - (0.0625D0*CA22*MH32*SA1*SA3*dgAtMZ())/(CB*MW) - (0.1875D0*CA12*CA22*MH32*SA1*SA3*dgAtMZ())/(CB*MW) &
  &- (0.1875D0*CA1*m12squared*SA22*SA3*dgAtMZ())/(CB*MW) + (0.125D0*MH12*SA1*SA22*SA3*dgAtMZ())/(CB*MW) + (0.375D0*CA12*MH12*SA1*&
  &SA22*SA3*dgAtMZ())/(CB*MW) + (0.0625D0*MH32*SA1*SA22*SA3*dgAtMZ())/(CB*MW) + (0.1875D0*CA12*MH32*SA1*SA22*SA3*dgAtMZ())/(CB*MW&
  &) + (0.28125D0*CA1*CA3*m12squared*SA2*dgAtMZ())/(MW*SB) + (0.84375D0*CA1*CA22*CA3*m12squared*SA2*dgAtMZ())/(MW*SB) - (0.1875D0&
  &*CA1*CA3*m12squared*SA2*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA1*CA22*CA3*m12squared*SA2*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*CA3*MH&
  &12*SA1*SA2*dgAtMZ())/(MW*SB) - (0.1875D0*CA12*CA3*MH12*SA1*SA2*dgAtMZ())/(MW*SB) + (0.5625D0*CA22*CA3*MH12*SA1*SA2*dgAtMZ())/(&
  &MW*SB) - (0.5625D0*CA12*CA22*CA3*MH12*SA1*SA2*dgAtMZ())/(MW*SB) + (0.09375D0*CA3*MH32*SA1*SA2*dgAtMZ())/(MW*SB) - (0.09375D0*C&
  &A12*CA3*MH32*SA1*SA2*dgAtMZ())/(MW*SB) + (0.28125D0*CA22*CA3*MH32*SA1*SA2*dgAtMZ())/(MW*SB) - (0.28125D0*CA12*CA22*CA3*MH32*SA&
  &1*SA2*dgAtMZ())/(MW*SB) + (0.28125D0*CA1*CA3*m12squared*SA12*SA2*dgAtMZ())/(CB2*MW*SB) + (0.84375D0*CA1*CA22*CA3*m12squared*SA&
  &12*SA2*dgAtMZ())/(CB2*MW*SB) + (0.125D0*CA1*MH12*SA3*dgAtMZ())/(MW*SB) + (0.125D0*CA1*CA22*MH12*SA3*dgAtMZ())/(MW*SB) + (0.062&
  &5D0*CA1*MH32*SA3*dgAtMZ())/(MW*SB) + (0.0625D0*CA1*CA22*MH32*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*m12squared*SA1*SA3*dgAtMZ())/(M&
  &W*SB) - (0.1875D0*CA22*m12squared*SA1*SA3*dgAtMZ())/(MW*SB) + (0.125D0*m12squared*SA1*SA3*dgAtMZ())/(CB2*MW*SB) + (0.5625D0*CA&
  &12*m12squared*SA1*SA3*dgAtMZ())/(CB2*MW*SB) + (0.125D0*CA22*m12squared*SA1*SA3*dgAtMZ())/(CB2*MW*SB) + (0.5625D0*CA12*CA22*m12&
  &squared*SA1*SA3*dgAtMZ())/(CB2*MW*SB) + (0.375D0*CA1*MH12*SA12*SA3*dgAtMZ())/(MW*SB) + (0.375D0*CA1*CA22*MH12*SA12*SA3*dgAtMZ(&
  &))/(MW*SB) + (0.1875D0*CA1*MH32*SA12*SA3*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*CA22*MH32*SA12*SA3*dgAtMZ())/(MW*SB) - (0.125D0*CA1&
  &*MH12*SA22*SA3*dgAtMZ())/(MW*SB) - (0.0625D0*CA1*MH32*SA22*SA3*dgAtMZ())/(MW*SB) + (0.1875D0*m12squared*SA1*SA22*SA3*dgAtMZ())&
  &/(MW*SB) - (0.125D0*m12squared*SA1*SA22*SA3*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA12*m12squared*SA1*SA22*SA3*dgAtMZ())/(CB2*MW*S&
  &B) - (0.375D0*CA1*MH12*SA12*SA22*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*CA1*MH32*SA12*SA22*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*CA3*m1&
  &2squared*SA1*SA2*dgAtMZ())/(CB*MW*SB2) + (0.28125D0*CA12*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA22*CA3*m12&
  &squared*SA1*SA2*dgAtMZ())/(CB*MW*SB2) + (0.84375D0*CA12*CA22*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*MW*SB2) - (0.125D0*CA1*m12sq&
  &uared*SA3*dgAtMZ())/(CB*MW*SB2) - (0.125D0*CA1*CA22*m12squared*SA3*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA1*m12squared*SA12*SA3*d&
  &gAtMZ())/(CB*MW*SB2) - (0.5625D0*CA1*CA22*m12squared*SA12*SA3*dgAtMZ())/(CB*MW*SB2) + (0.125D0*CA1*m12squared*SA22*SA3*dgAtMZ(&
  &))/(CB*MW*SB2) + (0.5625D0*CA1*m12squared*SA12*SA22*SA3*dgAtMZ())/(CB*MW*SB2) - (0.09375D0*CA3*m12squared*SA1*SA2*dgAtMZ())/(M&
  &W*SB*TB) - (0.28125D0*CA22*CA3*m12squared*SA1*SA2*dgAtMZ())/(MW*SB*TB) - (0.0625D0*CA1*m12squared*SA3*dgAtMZ())/(MW*SB*TB) - (&
  &0.0625D0*CA1*CA22*m12squared*SA3*dgAtMZ())/(MW*SB*TB) + (0.0625D0*CA1*m12squared*SA22*SA3*dgAtMZ())/(MW*SB*TB) - (0.09375D0*CA&
  &1*CA3*m12squared*SA2*TB*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*CA3*m12squared*SA2*TB*dgAtMZ())/(CB*MW) + (0.0625D0*m12squared&
  &*SA1*SA3*TB*dgAtMZ())/(CB*MW) + (0.0625D0*CA22*m12squared*SA1*SA3*TB*dgAtMZ())/(CB*MW) - (0.0625D0*m12squared*SA1*SA22*SA3*TB*&
  &dgAtMZ())/(CB*MW) + (0.0625D0*CA3*MH12*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875D0*CA22*CA3*MH12*SA2*DBLE(CA1**INT(&
  &3.D0))*dgAtMZ())/(CB*MW) + (0.03125D0*CA3*MH32*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA22*CA3*MH32*SA2*DBLE(&
  &CA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.09375D0*CA3*m12squared*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.28125D0*CA22&
  &*CA3*m12squared*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.125D0*MH12*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0&
  &.125D0*CA22*MH12*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.0625D0*MH32*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.0&
  &625D0*CA22*MH32*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.125D0*MH12*SA22*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (&
  &0.0625D0*MH32*SA22*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*m12squared*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW&
  &*SB2) + (0.1875D0*CA22*m12squared*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*m12squared*SA22*SA3*DBLE(CA1**INT&
  &(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.125D0*MH12*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.125D0*CA22*MH12*SA3*DBLE(SA1**IN&
  &T(3.D0))*dgAtMZ())/(CB*MW) + (0.0625D0*MH32*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.0625D0*CA22*MH32*SA3*DBLE(SA1**INT&
  &(3.D0))*dgAtMZ())/(CB*MW) - (0.125D0*MH12*SA22*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*MH32*SA22*SA3*DBLE(SA1**&
  &INT(3.D0))*dgAtMZ())/(CB*MW) + (0.0625D0*CA3*MH12*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA22*CA3*MH12*SA2*DBL&
  &E(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.03125D0*CA3*MH32*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA22*CA3*MH3&
  &2*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.1875D0*m12squared*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875D0&
  &*CA22*m12squared*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*m12squared*SA22*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())&
  &/(CB2*MW*SB) - (0.09375D0*CA3*m12squared*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA22*CA3*m12squared*SA2*D&
  &BLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*CA1*CA3*MH12*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.09375D0*CA1*CA3&
  &*MH32*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.28125D0*CA3*m12squared*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875D&
  &0*CA1*CA3*MH12*SA12*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*CA3*MH32*SA12*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*M&
  &W) - (0.28125D0*CA1*CA3*m12squared*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*CA3*m12squared*DBLE(SA2**INT(3.D0))*&
  &dgAtMZ())/(CB2*MW*SB) - (0.1875D0*CA3*MH12*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA12*CA3*MH12*SA1*DBLE(SA2**&
  &INT(3.D0))*dgAtMZ())/(MW*SB) - (0.09375D0*CA3*MH32*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA12*CA3*MH32*SA1*D&
  &BLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.28125D0*CA1*CA3*m12squared*SA12*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) + (0.187&
  &5D0*CA3*m12squared*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA12*CA3*m12squared*SA1*DBLE(SA2**INT(3.D0))*dg&
  &AtMZ())/(CB*MW*SB2) + (0.09375D0*CA3*m12squared*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB*TB) + (0.09375D0*CA1*CA3*m12squared*&
  &TB*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*CA3*MH12*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (&
  &0.03125D0*CA3*MH32*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA3*m12squared*DBLE(CA1**INT(3.D0)&
  &)*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.0625D0*CA3*MH12*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) &
  &- (0.03125D0*CA3*MH32*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA3*m12squared*DBLE(SA1**INT(3.&
  &D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.28125D0*CA3*EL*SA1*SA2*dm122MSBarAlter())/(CB*MW*SW) + (0.84375D0*CA22*CA&
  &3*EL*SA1*SA2*dm122MSBarAlter())/(CB*MW*SW) + (0.1875D0*CA1*EL*SA3*dm122MSBarAlter())/(CB*MW*SW) + (0.1875D0*CA1*CA22*EL*SA3*dm&
  &122MSBarAlter())/(CB*MW*SW) - (0.1875D0*CA1*EL*SA22*SA3*dm122MSBarAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA3*EL*SA2*dm122MSBarAl&
  &ter())/(MW*SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*SA2*dm122MSBarAlter())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*SA2*dm122MSBarAlter())&
  &/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA22*CA3*EL*SA2*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.28125D0*CA1*CA3*EL*SA12*SA2*dm122MSBarA&
  &lter())/(CB2*MW*SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*SA12*SA2*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.1875D0*EL*SA1*SA3*dm122MSB&
  &arAlter())/(MW*SB*SW) - (0.1875D0*CA22*EL*SA1*SA3*dm122MSBarAlter())/(MW*SB*SW) + (0.125D0*EL*SA1*SA3*dm122MSBarAlter())/(CB2*&
  &MW*SB*SW) + (0.5625D0*CA12*EL*SA1*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.125D0*CA22*EL*SA1*SA3*dm122MSBarAlter())/(CB2*MW*&
  &SB*SW) + (0.5625D0*CA12*CA22*EL*SA1*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.1875D0*EL*SA1*SA22*SA3*dm122MSBarAlter())/(MW*S&
  &B*SW) - (0.125D0*EL*SA1*SA22*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.5625D0*CA12*EL*SA1*SA22*SA3*dm122MSBarAlter())/(CB2*MW&
  &*SB*SW) - (0.1875D0*CA3*EL*SA1*SA2*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.28125D0*CA12*CA3*EL*SA1*SA2*dm122MSBarAlter())/(CB*M&
  &W*SB2*SW) - (0.5625D0*CA22*CA3*EL*SA1*SA2*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA22*CA3*EL*SA1*SA2*dm122MSBarAl&
  &ter())/(CB*MW*SB2*SW) - (0.125D0*CA1*EL*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.125D0*CA1*CA22*EL*SA3*dm122MSBarAlter())/(C&
  &B*MW*SB2*SW) - (0.5625D0*CA1*EL*SA12*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*EL*SA12*SA3*dm122MSBarAlter())&
  &/(CB*MW*SB2*SW) + (0.125D0*CA1*EL*SA22*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.5625D0*CA1*EL*SA12*SA22*SA3*dm122MSBarAlter(&
  &))/(CB*MW*SB2*SW) - (0.09375D0*CA3*EL*SA1*SA2*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.28125D0*CA22*CA3*EL*SA1*SA2*dm122MSBarAlte&
  &r())/(MW*SB*SW*TB) - (0.0625D0*CA1*EL*SA3*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.0625D0*CA1*CA22*EL*SA3*dm122MSBarAlter())/(MW*&
  &SB*SW*TB) + (0.0625D0*CA1*EL*SA22*SA3*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.09375D0*CA1*CA3*EL*SA2*TB*dm122MSBarAlter())/(CB*M&
  &W*SW) - (0.28125D0*CA1*CA22*CA3*EL*SA2*TB*dm122MSBarAlter())/(CB*MW*SW) + (0.0625D0*EL*SA1*SA3*TB*dm122MSBarAlter())/(CB*MW*SW&
  &) + (0.0625D0*CA22*EL*SA1*SA3*TB*dm122MSBarAlter())/(CB*MW*SW) - (0.0625D0*EL*SA1*SA22*SA3*TB*dm122MSBarAlter())/(CB*MW*SW) - &
  &(0.09375D0*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.28125D0*CA22*CA3*EL*SA2*DBLE(CA1**INT(3.D0))&
  &*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.1875D0*EL*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.1875D0*CA22*&
  &EL*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.1875D0*EL*SA22*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/&
  &(CB*MW*SB2*SW) - (0.1875D0*EL*SA3*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.1875D0*CA22*EL*SA3*DBLE(SA1**INT&
  &(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.1875D0*EL*SA22*SA3*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.&
  &09375D0*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA22*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*dm&
  &122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SW) - (0.28125D0*CA1*CA&
  &3*EL*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB2*MW&
  &*SB*SW) - (0.28125D0*CA1*CA3*EL*SA12*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*SA1*DBLE(SA2**I&
  &NT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA12*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW&
  &) + (0.09375D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(MW*SB*SW*TB) + (0.09375D0*CA1*CA3*EL*TB*DBLE(SA2**INT(3.D0)&
  &)*dm122MSBarAlter())/(CB*MW*SW) + (0.09375D0*CA3*EL*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW&
  &) + (0.09375D0*CA3*EL*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.1875D0*CA1*CA3*EL*SA2*d&
  &MH12OSAlter())/(CB*MW*SW) + (0.5625D0*CA1*CA22*CA3*EL*SA2*dMH12OSAlter())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*SA12*SA2*dMH12OSAl&
  &ter())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*EL*SA12*SA2*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*EL*SA1*SA3*dMH12OSAlter())/(CB*MW&
  &*SW) - (0.375D0*CA12*EL*SA1*SA3*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*CA22*EL*SA1*SA3*dMH12OSAlter())/(CB*MW*SW) - (0.375D0*CA&
  &12*CA22*EL*SA1*SA3*dMH12OSAlter())/(CB*MW*SW) + (0.125D0*EL*SA1*SA22*SA3*dMH12OSAlter())/(CB*MW*SW) + (0.375D0*CA12*EL*SA1*SA2&
  &2*SA3*dMH12OSAlter())/(CB*MW*SW) + (0.1875D0*CA3*EL*SA1*SA2*dMH12OSAlter())/(MW*SB*SW) - (0.1875D0*CA12*CA3*EL*SA1*SA2*dMH12OS&
  &Alter())/(MW*SB*SW) + (0.5625D0*CA22*CA3*EL*SA1*SA2*dMH12OSAlter())/(MW*SB*SW) - (0.5625D0*CA12*CA22*CA3*EL*SA1*SA2*dMH12OSAlt&
  &er())/(MW*SB*SW) + (0.125D0*CA1*EL*SA3*dMH12OSAlter())/(MW*SB*SW) + (0.125D0*CA1*CA22*EL*SA3*dMH12OSAlter())/(MW*SB*SW) + (0.3&
  &75D0*CA1*EL*SA12*SA3*dMH12OSAlter())/(MW*SB*SW) + (0.375D0*CA1*CA22*EL*SA12*SA3*dMH12OSAlter())/(MW*SB*SW) - (0.125D0*CA1*EL*S&
  &A22*SA3*dMH12OSAlter())/(MW*SB*SW) - (0.375D0*CA1*EL*SA12*SA22*SA3*dMH12OSAlter())/(MW*SB*SW) - (0.5D0*CA2*CA3*dMH12OSAlter())&
  &/vS - (1.5D0*CA2*CA3*SA22*dMH12OSAlter())/vS + (0.0625D0*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) + (0.1875D&
  &0*CA22*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*EL*SA3*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(MW*S&
  &B*SW) - (0.125D0*CA22*EL*SA3*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.125D0*EL*SA22*SA3*DBLE(CA1**INT(3.D0))*dMH12&
  &OSAlter())/(MW*SB*SW) + (0.5D0*CA3*DBLE(CA2**INT(3.D0))*dMH12OSAlter())/vS + (0.125D0*EL*SA3*DBLE(SA1**INT(3.D0))*dMH12OSAlter&
  &())/(CB*MW*SW) + (0.125D0*CA22*EL*SA3*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*EL*SA22*SA3*DBLE(SA1**INT(3.D&
  &0))*dMH12OSAlter())/(CB*MW*SW) + (0.0625D0*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.1875D0*CA22*CA3*EL*&
  &SA2*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) + (&
  &0.1875D0*CA1*CA3*EL*SA12*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.1875D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dMH12OSAl&
  &ter())/(MW*SB*SW) + (0.1875D0*CA12*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) - (0.0625D0*CA3*EL*DBLE(CA1**INT&
  &(3.D0))*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.0625D0*CA3*EL*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH12OSAl&
  &ter())/(MW*SB*SW) + (0.09375D0*CA1*CA3*EL*SA2*dMH32OSAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA22*CA3*EL*SA2*dMH32OSAlter())/(CB*&
  &MW*SW) - (0.09375D0*CA1*CA3*EL*SA12*SA2*dMH32OSAlter())/(CB*MW*SW) - (0.28125D0*CA1*CA22*CA3*EL*SA12*SA2*dMH32OSAlter())/(CB*M&
  &W*SW) - (0.0625D0*EL*SA1*SA3*dMH32OSAlter())/(CB*MW*SW) - (0.1875D0*CA12*EL*SA1*SA3*dMH32OSAlter())/(CB*MW*SW) - (0.0625D0*CA2&
  &2*EL*SA1*SA3*dMH32OSAlter())/(CB*MW*SW) - (0.1875D0*CA12*CA22*EL*SA1*SA3*dMH32OSAlter())/(CB*MW*SW) + (0.0625D0*EL*SA1*SA22*SA&
  &3*dMH32OSAlter())/(CB*MW*SW) + (0.1875D0*CA12*EL*SA1*SA22*SA3*dMH32OSAlter())/(CB*MW*SW) + (0.09375D0*CA3*EL*SA1*SA2*dMH32OSAl&
  &ter())/(MW*SB*SW) - (0.09375D0*CA12*CA3*EL*SA1*SA2*dMH32OSAlter())/(MW*SB*SW) + (0.28125D0*CA22*CA3*EL*SA1*SA2*dMH32OSAlter())&
  &/(MW*SB*SW) - (0.28125D0*CA12*CA22*CA3*EL*SA1*SA2*dMH32OSAlter())/(MW*SB*SW) + (0.0625D0*CA1*EL*SA3*dMH32OSAlter())/(MW*SB*SW)&
  & + (0.0625D0*CA1*CA22*EL*SA3*dMH32OSAlter())/(MW*SB*SW) + (0.1875D0*CA1*EL*SA12*SA3*dMH32OSAlter())/(MW*SB*SW) + (0.1875D0*CA1&
  &*CA22*EL*SA12*SA3*dMH32OSAlter())/(MW*SB*SW) - (0.0625D0*CA1*EL*SA22*SA3*dMH32OSAlter())/(MW*SB*SW) - (0.1875D0*CA1*EL*SA12*SA&
  &22*SA3*dMH32OSAlter())/(MW*SB*SW) - (0.25D0*CA2*CA3*dMH32OSAlter())/vS - (0.75D0*CA2*CA3*SA22*dMH32OSAlter())/vS + (0.03125D0*&
  &CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) + (0.09375D0*CA22*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dMH32OSAlter())/(&
  &CB*MW*SW) - (0.0625D0*EL*SA3*DBLE(CA1**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) - (0.0625D0*CA22*EL*SA3*DBLE(CA1**INT(3.D0))*dMH3&
  &2OSAlter())/(MW*SB*SW) + (0.0625D0*EL*SA22*SA3*DBLE(CA1**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) + (0.25D0*CA3*DBLE(CA2**INT(3.D&
  &0))*dMH32OSAlter())/vS + (0.0625D0*EL*SA3*DBLE(SA1**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) + (0.0625D0*CA22*EL*SA3*DBLE(SA1**IN&
  &T(3.D0))*dMH32OSAlter())/(CB*MW*SW) - (0.0625D0*EL*SA22*SA3*DBLE(SA1**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) + (0.03125D0*CA3*E&
  &L*SA2*DBLE(SA1**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) + (0.09375D0*CA22*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*dMH32OSAlter())/(MW*SB&
  &*SW) - (0.09375D0*CA1*CA3*EL*DBLE(SA2**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) + (0.09375D0*CA1*CA3*EL*SA12*DBLE(SA2**INT(3.D0))&
  &*dMH32OSAlter())/(CB*MW*SW) - (0.09375D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) + (0.09375D0*CA12*CA3*EL*S&
  &A1*DBLE(SA2**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) - (0.03125D0*CA3*EL*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH32OSAlter(&
  &))/(CB*MW*SW) - (0.03125D0*CA3*EL*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) - (0.09375D0*CA1*CA3*EL&
  &*MH12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.28125D0*CA1*CA22*CA3*EL*MH12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(C&
  &B*SW) - (0.046875D0*CA1*CA3*EL*MH32*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.140625D0*CA1*CA22*CA3*EL*MH32*SA2*DBLE(&
  &MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.140625D0*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.42&
  &1875D0*CA22*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.09375D0*CA1*CA3*EL*MH12*SA12*SA2*DBLE(MW*&
  &*INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.28125D0*CA1*CA22*CA3*EL*MH12*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.04&
  &6875D0*CA1*CA3*EL*MH32*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*CA1*CA22*CA3*EL*MH32*SA12*SA2*DBLE(MW*&
  &*INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA&
  &1*CA22*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0625D0*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter(&
  &))/(CB*SW) + (0.1875D0*CA12*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0625D0*CA22*EL*MH12*SA1*SA3*DBLE(MW&
  &**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.1875D0*CA12*CA22*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.03125D&
  &0*EL*MH32*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.09375D0*CA12*EL*MH32*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter()&
  &)/(CB*SW) + (0.03125D0*CA22*EL*MH32*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.09375D0*CA12*CA22*EL*MH32*SA1*SA3*D&
  &BLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.09375D0*CA1*EL*m12squared*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (&
  &0.0625D0*EL*MH12*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.1875D0*CA12*EL*MH12*SA1*SA22*SA3*DBLE(MW**INT(-3.&
  &D0))*dMW2Alter())/(CB*SW) - (0.03125D0*EL*MH32*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA12*EL*MH3&
  &2*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.140625D0*CA1*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*dMW2Alte&
  &r())/(SB*SW) - (0.421875D0*CA1*CA22*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA1*CA3*EL*m1&
  &2squared*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.28125D0*CA1*CA22*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*dM&
  &W2Alter())/(CB2*SB*SW) - (0.09375D0*CA3*EL*MH12*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA12*CA3*EL*MH1&
  &2*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.28125D0*CA22*CA3*EL*MH12*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(S&
  &B*SW) + (0.28125D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*CA3*EL*MH32*SA1*SA2*D&
  &BLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.046875D0*CA12*CA3*EL*MH32*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0&
  &.140625D0*CA22*CA3*EL*MH32*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.140625D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*DBLE(&
  &MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.140625D0*CA1*CA3*EL*m12squared*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW&
  &) - (0.421875D0*CA1*CA22*CA3*EL*m12squared*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.0625D0*CA1*EL*MH12*SA3*&
  &DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.0625D0*CA1*CA22*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.0312&
  &5D0*CA1*EL*MH32*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.03125D0*CA1*CA22*EL*MH32*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter&
  &())/(SB*SW) + (0.09375D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA22*EL*m12squared*SA1*S&
  &A3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.0625D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) -&
  & (0.28125D0*CA12*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.0625D0*CA22*EL*m12squared*SA1*SA3*DB&
  &LE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.28125D0*CA12*CA22*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB&
  &2*SB*SW) - (0.1875D0*CA1*EL*MH12*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.1875D0*CA1*CA22*EL*MH12*SA12*SA3*DBLE&
  &(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*CA1*EL*MH32*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0&
  &*CA1*CA22*EL*MH32*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.0625D0*CA1*EL*MH12*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW&
  &2Alter())/(SB*SW) + (0.03125D0*CA1*EL*MH32*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*EL*m12squared*SA1*S&
  &A22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.0625D0*EL*m12squared*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB&
  &2*SB*SW) + (0.28125D0*CA12*EL*m12squared*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.1875D0*CA1*EL*MH12*SA&
  &12*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA1*EL*MH32*SA12*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())&
  &/(SB*SW) + (0.09375D0*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.140625D0*CA12*CA3*EL*m12squ&
  &ared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.28125D0*CA22*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dM&
  &W2Alter())/(CB*SB2*SW) - (0.421875D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.06&
  &25D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.0625D0*CA1*CA22*EL*m12squared*SA3*DBLE(MW**INT(-&
  &3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.28125D0*CA1*EL*m12squared*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.281&
  &25D0*CA1*CA22*EL*m12squared*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.0625D0*CA1*EL*m12squared*SA22*SA3*DBLE&
  &(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.28125D0*CA1*EL*m12squared*SA12*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*S&
  &B2*SW) + (0.046875D0*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.140625D0*CA22*CA3*EL*m12squar&
  &ed*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.03125D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(&
  &SB*SW*TB) + (0.03125D0*CA1*CA22*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) - (0.03125D0*CA1*EL*m12squared*&
  &SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.046875D0*CA1*CA3*EL*m12squared*SA2*TB*DBLE(MW**INT(-3.D0))*dMW2Alte&
  &r())/(CB*SW) + (0.140625D0*CA1*CA22*CA3*EL*m12squared*SA2*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*EL*m12squa&
  &red*SA1*SA3*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA22*EL*m12squared*SA1*SA3*TB*DBLE(MW**INT(-3.D0))*dMW2A&
  &lter())/(CB*SW) + (0.03125D0*EL*m12squared*SA1*SA22*SA3*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA3*EL*MH12*&
  &SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA22*CA3*EL*MH12*SA2*DBLE(CA1**INT(3.D0))*DBLE&
  &(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.015625D0*CA3*EL*MH32*SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB&
  &*SW) - (0.046875D0*CA22*CA3*EL*MH32*SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.046875D0*CA3*EL*m1&
  &2squared*SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.140625D0*CA22*CA3*EL*m12squared*SA2*DBLE(&
  &CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.0625D0*EL*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0)&
  &)*dMW2Alter())/(SB*SW) + (0.0625D0*CA22*EL*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.03125D&
  &0*EL*MH32*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.03125D0*CA22*EL*MH32*SA3*DBLE(CA1**INT(3.D0)&
  &)*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.0625D0*EL*MH12*SA22*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter()&
  &)/(SB*SW) - (0.03125D0*EL*MH32*SA22*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*EL*m12squa&
  &red*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.09375D0*CA22*EL*m12squared*SA3*DBLE(CA1**INT(3&
  &.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.09375D0*EL*m12squared*SA22*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D&
  &0))*dMW2Alter())/(CB*SB2*SW) - (0.0625D0*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.0625D&
  &0*CA22*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*EL*MH32*SA3*DBLE(MW**INT(-3.D0)&
  &)*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA22*EL*MH32*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter(&
  &))/(CB*SW) + (0.0625D0*EL*MH12*SA22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*EL*MH32*SA&
  &22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA3*EL*MH12*SA2*DBLE(MW**INT(-3.D0))*DBLE(S&
  &A1**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*CA22*CA3*EL*MH12*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(&
  &SB*SW) - (0.015625D0*CA3*EL*MH32*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*CA22*CA3*EL*&
  &MH32*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*DB&
  &LE(SA1**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.09375D0*CA22*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2&
  &Alter())/(CB2*SB*SW) - (0.09375D0*EL*m12squared*SA22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) + &
  &(0.046875D0*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.140625D0*CA22*CA3*EL&
  &*m12squared*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.09375D0*CA1*CA3*EL*MH12*DBLE(MW**INT(-&
  &3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.046875D0*CA1*CA3*EL*MH32*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2A&
  &lter())/(CB*SW) + (0.140625D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.09375D&
  &0*CA1*CA3*EL*MH12*SA12*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.046875D0*CA1*CA3*EL*MH32*SA12*DBLE(&
  &MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*DBLE(SA2**I&
  &NT(3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB2*&
  &SB*SW) + (0.09375D0*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*CA12*CA3*EL*MH&
  &12*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.046875D0*CA3*EL*MH32*SA1*DBLE(MW**INT(-3.D0))*DBLE(&
  &SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*CA12*CA3*EL*MH32*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())&
  &/(SB*SW) + (0.140625D0*CA1*CA3*EL*m12squared*SA12*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.0937&
  &5D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.140625D0*CA12*CA3*EL*m12squa&
  &red*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.046875D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3&
  &.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW*TB) - (0.046875D0*CA1*CA3*EL*m12squared*TB*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3&
  &.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*CA3*EL*MH12*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter()&
  &)/(CB*SW) + (0.015625D0*CA3*EL*MH32*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.0&
  &46875D0*CA3*EL*m12squared*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.03125D0&
  &*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.015625D0*CA3*EL*MH32*DBL&
  &E(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*CA3*EL*m12squared*DBLE(MW**INT(&
  &-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + 0.5D0*CA2*CA3*MH12*DBLE(vS**INT(-2.D0))*dvSMSBarA&
  &lter() + 0.25D0*CA2*CA3*MH32*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() + 1.5D0*CA2*CA3*MH12*SA22*DBLE(vS**INT(-2.D0))*dvSMSBarAlter&
  &() + 0.75D0*CA2*CA3*MH32*SA22*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() - 0.5D0*CA3*MH12*DBLE(CA2**INT(3.D0))*DBLE(vS**INT(-2.D0))*&
  &dvSMSBarAlter() - 0.25D0*CA3*MH32*DBLE(CA2**INT(3.D0))*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() &
		& + CS1S1S1f111*dZH1H3OSAlter()/2D0 + CS1S1S1f211*dZH2H3OSAlter()/2D0 + CS1S1S1f311*dZH3H3OS()/2D0 &
		& + CS1S1S1f131*dZH1H1OS()/2D0 + CS1S1S1f231*dZH2H1OSAlter()/2D0 + CS1S1S1f331*dZH3H1OSAlter()/2D0 &
		& + CS1S1S1f131*dZH1H1OS()/2D0 + CS1S1S1f231*dZH2H1OSAlter()/2D0 + CS1S1S1f331*dZH3H1OSAlter()/2D0 &
		& )
	case (4)
		dRR11 = -CA2*SA1*dAlpha1KanAlter() - CA1*SA2*dAlpha2KanAlter()
		dRR12 = CA1*CA2*dAlpha1KanAlter() - SA1*SA2*dAlpha2KanAlter()
		dRR13 = CA2*dAlpha2KanAlter()
		dRR21 = -CA1*CA3*dAlpha1KanAlter() - CA1*CA3*SA2*dAlpha3KanAlter() - CA1*CA2*SA3*dAlpha2KanAlter() + &
			& SA1*SA3*dAlpha3KanAlter() + SA1*SA2*SA3*dAlpha1KanAlter()
		dRR22 = -CA3*SA1*dAlpha1KanAlter() - CA3*SA1*SA2*dAlpha3KanAlter() - CA1*SA3*dAlpha3KanAlter() - &
			& CA2*SA1*SA3*dAlpha2KanAlter() - CA1*SA2*SA3*dAlpha1KanAlter()
		dRR23 = CA2*CA3*dAlpha3KanAlter() - SA2*SA3*dAlpha2KanAlter()
		dRR31 = -CA1*CA2*CA3*dAlpha2KanAlter() + CA3*SA1*dAlpha3KanAlter() + CA3*SA1*SA2*dAlpha1KanAlter() + &
			& CA1*SA3*dAlpha1KanAlter() + CA1*SA2*SA3*dAlpha3KanAlter()
		dRR32 = -CA1*CA3*dAlpha3KanAlter() - CA2*CA3*SA1*dAlpha2KanAlter() - CA1*CA3*SA2*dAlpha1KanAlter() + &
			& SA1*SA3*dAlpha1KanAlter() + SA1*SA2*SA3*dAlpha3KanAlter()
		dRR33 = -CA3*SA2*dAlpha2KanAlter() - CA2*SA3*dAlpha3KanAlter()

		totalAmplitude = CS1S1S1f311*( &
&(0.28125D0*CA1*CA3*EL*m12squared*SA2*dAlpha1KanAlter())/(CB*MW*SW) + (0.84375D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha1KanAlter())&
  &/(CB*MW*SW) - (0.1875D0*CA3*EL*MH12*SA1*SA2*dAlpha1KanAlter())/(CB*MW*SW) - (0.5625D0*CA12*CA3*EL*MH12*SA1*SA2*dAlpha1KanAlter&
  &())/(CB*MW*SW) - (0.5625D0*CA22*CA3*EL*MH12*SA1*SA2*dAlpha1KanAlter())/(CB*MW*SW) - (1.6875D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dA&
  &lpha1KanAlter())/(CB*MW*SW) - (0.09375D0*CA3*EL*MH32*SA1*SA2*dAlpha1KanAlter())/(CB*MW*SW) - (0.28125D0*CA12*CA3*EL*MH32*SA1*S&
  &A2*dAlpha1KanAlter())/(CB*MW*SW) - (0.28125D0*CA22*CA3*EL*MH32*SA1*SA2*dAlpha1KanAlter())/(CB*MW*SW) - (0.84375D0*CA12*CA22*CA&
  &3*EL*MH32*SA1*SA2*dAlpha1KanAlter())/(CB*MW*SW) - (0.125D0*CA1*EL*MH12*SA3*dAlpha1KanAlter())/(CB*MW*SW) - (0.125D0*CA1*CA22*E&
  &L*MH12*SA3*dAlpha1KanAlter())/(CB*MW*SW) - (0.0625D0*CA1*EL*MH32*SA3*dAlpha1KanAlter())/(CB*MW*SW) - (0.0625D0*CA1*CA22*EL*MH3&
  &2*SA3*dAlpha1KanAlter())/(CB*MW*SW) - (0.1875D0*EL*m12squared*SA1*SA3*dAlpha1KanAlter())/(CB*MW*SW) - (0.1875D0*CA22*EL*m12squ&
  &ared*SA1*SA3*dAlpha1KanAlter())/(CB*MW*SW) + (1.125D0*CA1*EL*MH12*SA12*SA3*dAlpha1KanAlter())/(CB*MW*SW) + (1.125D0*CA1*CA22*E&
  &L*MH12*SA12*SA3*dAlpha1KanAlter())/(CB*MW*SW) + (0.5625D0*CA1*EL*MH32*SA12*SA3*dAlpha1KanAlter())/(CB*MW*SW) + (0.5625D0*CA1*C&
  &A22*EL*MH32*SA12*SA3*dAlpha1KanAlter())/(CB*MW*SW) + (0.125D0*CA1*EL*MH12*SA22*SA3*dAlpha1KanAlter())/(CB*MW*SW) + (0.0625D0*C&
  &A1*EL*MH32*SA22*SA3*dAlpha1KanAlter())/(CB*MW*SW) + (0.1875D0*EL*m12squared*SA1*SA22*SA3*dAlpha1KanAlter())/(CB*MW*SW) - (1.12&
  &5D0*CA1*EL*MH12*SA12*SA22*SA3*dAlpha1KanAlter())/(CB*MW*SW) - (0.5625D0*CA1*EL*MH32*SA12*SA22*SA3*dAlpha1KanAlter())/(CB*MW*SW&
  &) + (0.1875D0*CA1*CA3*EL*MH12*SA2*dAlpha1KanAlter())/(MW*SB*SW) + (0.5625D0*CA1*CA22*CA3*EL*MH12*SA2*dAlpha1KanAlter())/(MW*SB&
  &*SW) + (0.09375D0*CA1*CA3*EL*MH32*SA2*dAlpha1KanAlter())/(MW*SB*SW) + (0.28125D0*CA1*CA22*CA3*EL*MH32*SA2*dAlpha1KanAlter())/(&
  &MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*SA1*SA2*dAlpha1KanAlter())/(MW*SB*SW) - (0.84375D0*CA22*CA3*EL*m12squared*SA1*SA2*dAl&
  &pha1KanAlter())/(MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA1*SA2*dAlpha1KanAlter())/(CB2*MW*SB*SW) + (0.84375D0*CA12*CA3*EL*m1&
  &2squared*SA1*SA2*dAlpha1KanAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha1KanAlter())/(CB2*MW*SB*SW&
  &) + (2.53125D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha1KanAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA1*CA3*EL*MH12*SA12*SA2*dAlp&
  &ha1KanAlter())/(MW*SB*SW) + (1.6875D0*CA1*CA22*CA3*EL*MH12*SA12*SA2*dAlpha1KanAlter())/(MW*SB*SW) + (0.28125D0*CA1*CA3*EL*MH32&
  &*SA12*SA2*dAlpha1KanAlter())/(MW*SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*MH32*SA12*SA2*dAlpha1KanAlter())/(MW*SB*SW) - (0.1875D0*C&
  &A1*EL*m12squared*SA3*dAlpha1KanAlter())/(MW*SB*SW) - (0.1875D0*CA1*CA22*EL*m12squared*SA3*dAlpha1KanAlter())/(MW*SB*SW) + (0.1&
  &25D0*CA1*EL*m12squared*SA3*dAlpha1KanAlter())/(CB2*MW*SB*SW) + (0.125D0*CA1*CA22*EL*m12squared*SA3*dAlpha1KanAlter())/(CB2*MW*&
  &SB*SW) - (0.125D0*EL*MH12*SA1*SA3*dAlpha1KanAlter())/(MW*SB*SW) + (1.125D0*CA12*EL*MH12*SA1*SA3*dAlpha1KanAlter())/(MW*SB*SW) &
  &- (0.125D0*CA22*EL*MH12*SA1*SA3*dAlpha1KanAlter())/(MW*SB*SW) + (1.125D0*CA12*CA22*EL*MH12*SA1*SA3*dAlpha1KanAlter())/(MW*SB*S&
  &W) - (0.0625D0*EL*MH32*SA1*SA3*dAlpha1KanAlter())/(MW*SB*SW) + (0.5625D0*CA12*EL*MH32*SA1*SA3*dAlpha1KanAlter())/(MW*SB*SW) - &
  &(0.0625D0*CA22*EL*MH32*SA1*SA3*dAlpha1KanAlter())/(MW*SB*SW) + (0.5625D0*CA12*CA22*EL*MH32*SA1*SA3*dAlpha1KanAlter())/(MW*SB*S&
  &W) - (1.6875D0*CA1*EL*m12squared*SA12*SA3*dAlpha1KanAlter())/(CB2*MW*SB*SW) - (1.6875D0*CA1*CA22*EL*m12squared*SA12*SA3*dAlpha&
  &1KanAlter())/(CB2*MW*SB*SW) + (0.1875D0*CA1*EL*m12squared*SA22*SA3*dAlpha1KanAlter())/(MW*SB*SW) - (0.125D0*CA1*EL*m12squared*&
  &SA22*SA3*dAlpha1KanAlter())/(CB2*MW*SB*SW) + (0.125D0*EL*MH12*SA1*SA22*SA3*dAlpha1KanAlter())/(MW*SB*SW) - (1.125D0*CA12*EL*MH&
  &12*SA1*SA22*SA3*dAlpha1KanAlter())/(MW*SB*SW) + (0.0625D0*EL*MH32*SA1*SA22*SA3*dAlpha1KanAlter())/(MW*SB*SW) - (0.5625D0*CA12*&
  &EL*MH32*SA1*SA22*SA3*dAlpha1KanAlter())/(MW*SB*SW) + (1.6875D0*CA1*EL*m12squared*SA12*SA22*SA3*dAlpha1KanAlter())/(CB2*MW*SB*S&
  &W) - (0.1875D0*CA1*CA3*EL*m12squared*SA2*dAlpha1KanAlter())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha1K&
  &anAlter())/(CB*MW*SB2*SW) - (0.84375D0*CA1*CA3*EL*m12squared*SA12*SA2*dAlpha1KanAlter())/(CB*MW*SB2*SW) - (2.53125D0*CA1*CA22*&
  &CA3*EL*m12squared*SA12*SA2*dAlpha1KanAlter())/(CB*MW*SB2*SW) + (0.125D0*EL*m12squared*SA1*SA3*dAlpha1KanAlter())/(CB*MW*SB2*SW&
  &) - (1.6875D0*CA12*EL*m12squared*SA1*SA3*dAlpha1KanAlter())/(CB*MW*SB2*SW) + (0.125D0*CA22*EL*m12squared*SA1*SA3*dAlpha1KanAlt&
  &er())/(CB*MW*SB2*SW) - (1.6875D0*CA12*CA22*EL*m12squared*SA1*SA3*dAlpha1KanAlter())/(CB*MW*SB2*SW) - (0.125D0*EL*m12squared*SA&
  &1*SA22*SA3*dAlpha1KanAlter())/(CB*MW*SB2*SW) + (1.6875D0*CA12*EL*m12squared*SA1*SA22*SA3*dAlpha1KanAlter())/(CB*MW*SB2*SW) - (&
  &0.09375D0*CA1*CA3*EL*m12squared*SA2*dAlpha1KanAlter())/(MW*SB*SW*TB) - (0.28125D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha1KanAlt&
  &er())/(MW*SB*SW*TB) + (0.0625D0*EL*m12squared*SA1*SA3*dAlpha1KanAlter())/(MW*SB*SW*TB) + (0.0625D0*CA22*EL*m12squared*SA1*SA3*&
  &dAlpha1KanAlter())/(MW*SB*SW*TB) - (0.0625D0*EL*m12squared*SA1*SA22*SA3*dAlpha1KanAlter())/(MW*SB*SW*TB) + (0.09375D0*CA3*EL*m&
  &12squared*SA1*SA2*TB*dAlpha1KanAlter())/(CB*MW*SW) + (0.28125D0*CA22*CA3*EL*m12squared*SA1*SA2*TB*dAlpha1KanAlter())/(CB*MW*SW&
  &) + (0.0625D0*CA1*EL*m12squared*SA3*TB*dAlpha1KanAlter())/(CB*MW*SW) + (0.0625D0*CA1*CA22*EL*m12squared*SA3*TB*dAlpha1KanAlter&
  &())/(CB*MW*SW) - (0.0625D0*CA1*EL*m12squared*SA22*SA3*TB*dAlpha1KanAlter())/(CB*MW*SW) + (0.1875D0*CA1*CA2*CA3*EL*MH12*dAlpha2&
  &KanAlter())/(CB*MW*SW) + (0.09375D0*CA1*CA2*CA3*EL*MH32*dAlpha2KanAlter())/(CB*MW*SW) + (0.28125D0*CA2*CA3*EL*m12squared*SA1*d&
  &Alpha2KanAlter())/(CB*MW*SW) - (0.1875D0*CA1*CA2*CA3*EL*MH12*SA12*dAlpha2KanAlter())/(CB*MW*SW) - (0.09375D0*CA1*CA2*CA3*EL*MH&
  &32*SA12*dAlpha2KanAlter())/(CB*MW*SW) - (1.6875D0*CA1*CA2*CA3*EL*MH12*SA22*dAlpha2KanAlter())/(CB*MW*SW) - (0.84375D0*CA1*CA2*&
  &CA3*EL*MH32*SA22*dAlpha2KanAlter())/(CB*MW*SW) - (2.53125D0*CA2*CA3*EL*m12squared*SA1*SA22*dAlpha2KanAlter())/(CB*MW*SW) + (1.&
  &6875D0*CA1*CA2*CA3*EL*MH12*SA12*SA22*dAlpha2KanAlter())/(CB*MW*SW) + (0.84375D0*CA1*CA2*CA3*EL*MH32*SA12*SA22*dAlpha2KanAlter(&
  &))/(CB*MW*SW) - (0.75D0*CA1*CA2*EL*m12squared*SA2*SA3*dAlpha2KanAlter())/(CB*MW*SW) + (0.5D0*CA2*EL*MH12*SA1*SA2*SA3*dAlpha2Ka&
  &nAlter())/(CB*MW*SW) + (1.5D0*CA12*CA2*EL*MH12*SA1*SA2*SA3*dAlpha2KanAlter())/(CB*MW*SW) + (0.25D0*CA2*EL*MH32*SA1*SA2*SA3*dAl&
  &pha2KanAlter())/(CB*MW*SW) + (0.75D0*CA12*CA2*EL*MH32*SA1*SA2*SA3*dAlpha2KanAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA2*CA3*EL*m1&
  &2squared*dAlpha2KanAlter())/(MW*SB*SW) - (0.1875D0*CA1*CA2*CA3*EL*m12squared*dAlpha2KanAlter())/(CB2*MW*SB*SW) + (0.1875D0*CA2&
  &*CA3*EL*MH12*SA1*dAlpha2KanAlter())/(MW*SB*SW) - (0.1875D0*CA12*CA2*CA3*EL*MH12*SA1*dAlpha2KanAlter())/(MW*SB*SW) + (0.09375D0&
  &*CA2*CA3*EL*MH32*SA1*dAlpha2KanAlter())/(MW*SB*SW) - (0.09375D0*CA12*CA2*CA3*EL*MH32*SA1*dAlpha2KanAlter())/(MW*SB*SW) + (0.28&
  &125D0*CA1*CA2*CA3*EL*m12squared*SA12*dAlpha2KanAlter())/(CB2*MW*SB*SW) - (2.53125D0*CA1*CA2*CA3*EL*m12squared*SA22*dAlpha2KanA&
  &lter())/(MW*SB*SW) + (1.6875D0*CA1*CA2*CA3*EL*m12squared*SA22*dAlpha2KanAlter())/(CB2*MW*SB*SW) - (1.6875D0*CA2*CA3*EL*MH12*SA&
  &1*SA22*dAlpha2KanAlter())/(MW*SB*SW) + (1.6875D0*CA12*CA2*CA3*EL*MH12*SA1*SA22*dAlpha2KanAlter())/(MW*SB*SW) - (0.84375D0*CA2*&
  &CA3*EL*MH32*SA1*SA22*dAlpha2KanAlter())/(MW*SB*SW) + (0.84375D0*CA12*CA2*CA3*EL*MH32*SA1*SA22*dAlpha2KanAlter())/(MW*SB*SW) - &
  &(2.53125D0*CA1*CA2*CA3*EL*m12squared*SA12*SA22*dAlpha2KanAlter())/(CB2*MW*SB*SW) - (0.5D0*CA1*CA2*EL*MH12*SA2*SA3*dAlpha2KanAl&
  &ter())/(MW*SB*SW) - (0.25D0*CA1*CA2*EL*MH32*SA2*SA3*dAlpha2KanAlter())/(MW*SB*SW) + (0.75D0*CA2*EL*m12squared*SA1*SA2*SA3*dAlp&
  &ha2KanAlter())/(MW*SB*SW) - (0.5D0*CA2*EL*m12squared*SA1*SA2*SA3*dAlpha2KanAlter())/(CB2*MW*SB*SW) - (2.25D0*CA12*CA2*EL*m12sq&
  &uared*SA1*SA2*SA3*dAlpha2KanAlter())/(CB2*MW*SB*SW) - (1.5D0*CA1*CA2*EL*MH12*SA12*SA2*SA3*dAlpha2KanAlter())/(MW*SB*SW) - (0.7&
  &5D0*CA1*CA2*EL*MH32*SA12*SA2*SA3*dAlpha2KanAlter())/(MW*SB*SW) - (0.1875D0*CA2*CA3*EL*m12squared*SA1*dAlpha2KanAlter())/(CB*MW&
  &*SB2*SW) + (0.28125D0*CA12*CA2*CA3*EL*m12squared*SA1*dAlpha2KanAlter())/(CB*MW*SB2*SW) + (1.6875D0*CA2*CA3*EL*m12squared*SA1*S&
  &A22*dAlpha2KanAlter())/(CB*MW*SB2*SW) - (2.53125D0*CA12*CA2*CA3*EL*m12squared*SA1*SA22*dAlpha2KanAlter())/(CB*MW*SB2*SW) + (0.&
  &5D0*CA1*CA2*EL*m12squared*SA2*SA3*dAlpha2KanAlter())/(CB*MW*SB2*SW) + (2.25D0*CA1*CA2*EL*m12squared*SA12*SA2*SA3*dAlpha2KanAlt&
  &er())/(CB*MW*SB2*SW) - (0.09375D0*CA2*CA3*EL*m12squared*SA1*dAlpha2KanAlter())/(MW*SB*SW*TB) + (0.84375D0*CA2*CA3*EL*m12square&
  &d*SA1*SA22*dAlpha2KanAlter())/(MW*SB*SW*TB) + (0.25D0*CA1*CA2*EL*m12squared*SA2*SA3*dAlpha2KanAlter())/(MW*SB*SW*TB) - (0.0937&
  &5D0*CA1*CA2*CA3*EL*m12squared*TB*dAlpha2KanAlter())/(CB*MW*SW) + (0.84375D0*CA1*CA2*CA3*EL*m12squared*SA22*TB*dAlpha2KanAlter(&
  &))/(CB*MW*SW) - (0.25D0*CA2*EL*m12squared*SA1*SA2*SA3*TB*dAlpha2KanAlter())/(CB*MW*SW) + (0.5D0*CA3*MH12*SA2*dAlpha2KanAlter()&
  &)/vS - (4.5D0*CA22*CA3*MH12*SA2*dAlpha2KanAlter())/vS + (0.25D0*CA3*MH32*SA2*dAlpha2KanAlter())/vS - (2.25D0*CA22*CA3*MH32*SA2&
  &*dAlpha2KanAlter())/vS + (0.1875D0*CA1*CA3*EL*m12squared*dAlpha3KanAlter())/(CB*MW*SW) + (0.1875D0*CA1*CA22*CA3*EL*m12squared*&
  &dAlpha3KanAlter())/(CB*MW*SW) - (0.125D0*CA3*EL*MH12*SA1*dAlpha3KanAlter())/(CB*MW*SW) - (0.375D0*CA12*CA3*EL*MH12*SA1*dAlpha3&
  &KanAlter())/(CB*MW*SW) - (0.125D0*CA22*CA3*EL*MH12*SA1*dAlpha3KanAlter())/(CB*MW*SW) - (0.375D0*CA12*CA22*CA3*EL*MH12*SA1*dAlp&
  &ha3KanAlter())/(CB*MW*SW) - (0.0625D0*CA3*EL*MH32*SA1*dAlpha3KanAlter())/(CB*MW*SW) - (0.1875D0*CA12*CA3*EL*MH32*SA1*dAlpha3Ka&
  &nAlter())/(CB*MW*SW) - (0.0625D0*CA22*CA3*EL*MH32*SA1*dAlpha3KanAlter())/(CB*MW*SW) - (0.1875D0*CA12*CA22*CA3*EL*MH32*SA1*dAlp&
  &ha3KanAlter())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*m12squared*SA22*dAlpha3KanAlter())/(CB*MW*SW) + (0.125D0*CA3*EL*MH12*SA1*SA22&
  &*dAlpha3KanAlter())/(CB*MW*SW) + (0.375D0*CA12*CA3*EL*MH12*SA1*SA22*dAlpha3KanAlter())/(CB*MW*SW) + (0.0625D0*CA3*EL*MH32*SA1*&
  &SA22*dAlpha3KanAlter())/(CB*MW*SW) + (0.1875D0*CA12*CA3*EL*MH32*SA1*SA22*dAlpha3KanAlter())/(CB*MW*SW) - (0.1875D0*CA1*EL*MH12&
  &*SA2*SA3*dAlpha3KanAlter())/(CB*MW*SW) - (0.5625D0*CA1*CA22*EL*MH12*SA2*SA3*dAlpha3KanAlter())/(CB*MW*SW) - (0.09375D0*CA1*EL*&
  &MH32*SA2*SA3*dAlpha3KanAlter())/(CB*MW*SW) - (0.28125D0*CA1*CA22*EL*MH32*SA2*SA3*dAlpha3KanAlter())/(CB*MW*SW) - (0.28125D0*EL&
  &*m12squared*SA1*SA2*SA3*dAlpha3KanAlter())/(CB*MW*SW) - (0.84375D0*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha3KanAlter())/(CB*MW*SW&
  &) + (0.1875D0*CA1*EL*MH12*SA12*SA2*SA3*dAlpha3KanAlter())/(CB*MW*SW) + (0.5625D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dAlpha3KanAlter&
  &())/(CB*MW*SW) + (0.09375D0*CA1*EL*MH32*SA12*SA2*SA3*dAlpha3KanAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*MH32*SA12*SA2*SA3*&
  &dAlpha3KanAlter())/(CB*MW*SW) + (0.125D0*CA1*CA3*EL*MH12*dAlpha3KanAlter())/(MW*SB*SW) + (0.125D0*CA1*CA22*CA3*EL*MH12*dAlpha3&
  &KanAlter())/(MW*SB*SW) + (0.0625D0*CA1*CA3*EL*MH32*dAlpha3KanAlter())/(MW*SB*SW) + (0.0625D0*CA1*CA22*CA3*EL*MH32*dAlpha3KanAl&
  &ter())/(MW*SB*SW) - (0.1875D0*CA3*EL*m12squared*SA1*dAlpha3KanAlter())/(MW*SB*SW) - (0.1875D0*CA22*CA3*EL*m12squared*SA1*dAlph&
  &a3KanAlter())/(MW*SB*SW) + (0.125D0*CA3*EL*m12squared*SA1*dAlpha3KanAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA12*CA3*EL*m12squared&
  &*SA1*dAlpha3KanAlter())/(CB2*MW*SB*SW) + (0.125D0*CA22*CA3*EL*m12squared*SA1*dAlpha3KanAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA1&
  &2*CA22*CA3*EL*m12squared*SA1*dAlpha3KanAlter())/(CB2*MW*SB*SW) + (0.375D0*CA1*CA3*EL*MH12*SA12*dAlpha3KanAlter())/(MW*SB*SW) +&
  & (0.375D0*CA1*CA22*CA3*EL*MH12*SA12*dAlpha3KanAlter())/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*MH32*SA12*dAlpha3KanAlter())/(MW*SB*S&
  &W) + (0.1875D0*CA1*CA22*CA3*EL*MH32*SA12*dAlpha3KanAlter())/(MW*SB*SW) - (0.125D0*CA1*CA3*EL*MH12*SA22*dAlpha3KanAlter())/(MW*&
  &SB*SW) - (0.0625D0*CA1*CA3*EL*MH32*SA22*dAlpha3KanAlter())/(MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA1*SA22*dAlpha3KanAlter()&
  &)/(MW*SB*SW) - (0.125D0*CA3*EL*m12squared*SA1*SA22*dAlpha3KanAlter())/(CB2*MW*SB*SW) - (0.5625D0*CA12*CA3*EL*m12squared*SA1*SA&
  &22*dAlpha3KanAlter())/(CB2*MW*SB*SW) - (0.375D0*CA1*CA3*EL*MH12*SA12*SA22*dAlpha3KanAlter())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL&
  &*MH32*SA12*SA22*dAlpha3KanAlter())/(MW*SB*SW) - (0.28125D0*CA1*EL*m12squared*SA2*SA3*dAlpha3KanAlter())/(MW*SB*SW) - (0.84375D&
  &0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha3KanAlter())/(MW*SB*SW) + (0.1875D0*CA1*EL*m12squared*SA2*SA3*dAlpha3KanAlter())/(CB2*M&
  &W*SB*SW) + (0.5625D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha3KanAlter())/(CB2*MW*SB*SW) - (0.1875D0*EL*MH12*SA1*SA2*SA3*dAlpha3K&
  &anAlter())/(MW*SB*SW) + (0.1875D0*CA12*EL*MH12*SA1*SA2*SA3*dAlpha3KanAlter())/(MW*SB*SW) - (0.5625D0*CA22*EL*MH12*SA1*SA2*SA3*&
  &dAlpha3KanAlter())/(MW*SB*SW) + (0.5625D0*CA12*CA22*EL*MH12*SA1*SA2*SA3*dAlpha3KanAlter())/(MW*SB*SW) - (0.09375D0*EL*MH32*SA1&
  &*SA2*SA3*dAlpha3KanAlter())/(MW*SB*SW) + (0.09375D0*CA12*EL*MH32*SA1*SA2*SA3*dAlpha3KanAlter())/(MW*SB*SW) - (0.28125D0*CA22*E&
  &L*MH32*SA1*SA2*SA3*dAlpha3KanAlter())/(MW*SB*SW) + (0.28125D0*CA12*CA22*EL*MH32*SA1*SA2*SA3*dAlpha3KanAlter())/(MW*SB*SW) - (0&
  &.28125D0*CA1*EL*m12squared*SA12*SA2*SA3*dAlpha3KanAlter())/(CB2*MW*SB*SW) - (0.84375D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*dAl&
  &pha3KanAlter())/(CB2*MW*SB*SW) - (0.125D0*CA1*CA3*EL*m12squared*dAlpha3KanAlter())/(CB*MW*SB2*SW) - (0.125D0*CA1*CA22*CA3*EL*m&
  &12squared*dAlpha3KanAlter())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA3*EL*m12squared*SA12*dAlpha3KanAlter())/(CB*MW*SB2*SW) - (0.5625&
  &D0*CA1*CA22*CA3*EL*m12squared*SA12*dAlpha3KanAlter())/(CB*MW*SB2*SW) + (0.125D0*CA1*CA3*EL*m12squared*SA22*dAlpha3KanAlter())/&
  &(CB*MW*SB2*SW) + (0.5625D0*CA1*CA3*EL*m12squared*SA12*SA22*dAlpha3KanAlter())/(CB*MW*SB2*SW) + (0.1875D0*EL*m12squared*SA1*SA2&
  &*SA3*dAlpha3KanAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA12*EL*m12squared*SA1*SA2*SA3*dAlpha3KanAlter())/(CB*MW*SB2*SW) + (0.5625&
  &D0*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha3KanAlter())/(CB*MW*SB2*SW) - (0.84375D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha3Ka&
  &nAlter())/(CB*MW*SB2*SW) - (0.0625D0*CA1*CA3*EL*m12squared*dAlpha3KanAlter())/(MW*SB*SW*TB) - (0.0625D0*CA1*CA22*CA3*EL*m12squ&
  &ared*dAlpha3KanAlter())/(MW*SB*SW*TB) + (0.0625D0*CA1*CA3*EL*m12squared*SA22*dAlpha3KanAlter())/(MW*SB*SW*TB) + (0.09375D0*EL*&
  &m12squared*SA1*SA2*SA3*dAlpha3KanAlter())/(MW*SB*SW*TB) + (0.28125D0*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha3KanAlter())/(MW*SB*&
  &SW*TB) + (0.0625D0*CA3*EL*m12squared*SA1*TB*dAlpha3KanAlter())/(CB*MW*SW) + (0.0625D0*CA22*CA3*EL*m12squared*SA1*TB*dAlpha3Kan&
  &Alter())/(CB*MW*SW) - (0.0625D0*CA3*EL*m12squared*SA1*SA22*TB*dAlpha3KanAlter())/(CB*MW*SW) + (0.09375D0*CA1*EL*m12squared*SA2&
  &*SA3*TB*dAlpha3KanAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*m12squared*SA2*SA3*TB*dAlpha3KanAlter())/(CB*MW*SW) + (0.5D0*CA&
  &2*MH12*SA3*dAlpha3KanAlter())/vS + (0.25D0*CA2*MH32*SA3*dAlpha3KanAlter())/vS + (1.5D0*CA2*MH12*SA22*SA3*dAlpha3KanAlter())/vS&
  & + (0.75D0*CA2*MH32*SA22*SA3*dAlpha3KanAlter())/vS + (0.09375D0*CA3*EL*MH12*SA1*SA2*dBeta2KanAlter())/(CB*MW*SW) - (0.09375D0*&
  &CA12*CA3*EL*MH12*SA1*SA2*dBeta2KanAlter())/(CB*MW*SW) + (0.28125D0*CA22*CA3*EL*MH12*SA1*SA2*dBeta2KanAlter())/(CB*MW*SW) - (0.&
  &28125D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dBeta2KanAlter())/(CB*MW*SW) + (0.046875D0*CA3*EL*MH32*SA1*SA2*dBeta2KanAlter())/(CB*MW*&
  &SW) - (0.046875D0*CA12*CA3*EL*MH32*SA1*SA2*dBeta2KanAlter())/(CB*MW*SW) + (0.140625D0*CA22*CA3*EL*MH32*SA1*SA2*dBeta2KanAlter(&
  &))/(CB*MW*SW) - (0.140625D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*dBeta2KanAlter())/(CB*MW*SW) + (0.0625D0*CA1*EL*MH12*SA3*dBeta2KanAl&
  &ter())/(CB*MW*SW) + (0.0625D0*CA1*CA22*EL*MH12*SA3*dBeta2KanAlter())/(CB*MW*SW) + (0.03125D0*CA1*EL*MH32*SA3*dBeta2KanAlter())&
  &/(CB*MW*SW) + (0.03125D0*CA1*CA22*EL*MH32*SA3*dBeta2KanAlter())/(CB*MW*SW) + (0.1875D0*CA1*EL*MH12*SA12*SA3*dBeta2KanAlter())/&
  &(CB*MW*SW) + (0.1875D0*CA1*CA22*EL*MH12*SA12*SA3*dBeta2KanAlter())/(CB*MW*SW) + (0.09375D0*CA1*EL*MH32*SA12*SA3*dBeta2KanAlter&
  &())/(CB*MW*SW) + (0.09375D0*CA1*CA22*EL*MH32*SA12*SA3*dBeta2KanAlter())/(CB*MW*SW) - (0.0625D0*CA1*EL*MH12*SA22*SA3*dBeta2KanA&
  &lter())/(CB*MW*SW) - (0.03125D0*CA1*EL*MH32*SA22*SA3*dBeta2KanAlter())/(CB*MW*SW) - (0.1875D0*CA1*EL*MH12*SA12*SA22*SA3*dBeta2&
  &KanAlter())/(CB*MW*SW) - (0.09375D0*CA1*EL*MH32*SA12*SA22*SA3*dBeta2KanAlter())/(CB*MW*SW) + (0.09375D0*CA3*EL*m12squared*SA1*&
  &SA2*dBeta2KanAlter())/(MW*SB*SW) + (0.28125D0*CA22*CA3*EL*m12squared*SA1*SA2*dBeta2KanAlter())/(MW*SB*SW) - (0.328125D0*CA3*EL&
  &*m12squared*SA1*SA2*dBeta2KanAlter())/(CB2*MW*SB*SW) + (0.421875D0*CA12*CA3*EL*m12squared*SA1*SA2*dBeta2KanAlter())/(CB2*MW*SB&
  &*SW) - (0.984375D0*CA22*CA3*EL*m12squared*SA1*SA2*dBeta2KanAlter())/(CB2*MW*SB*SW) + (1.265625D0*CA12*CA22*CA3*EL*m12squared*S&
  &A1*SA2*dBeta2KanAlter())/(CB2*MW*SB*SW) + (0.0625D0*CA1*EL*m12squared*SA3*dBeta2KanAlter())/(MW*SB*SW) + (0.0625D0*CA1*CA22*EL&
  &*m12squared*SA3*dBeta2KanAlter())/(MW*SB*SW) - (0.21875D0*CA1*EL*m12squared*SA3*dBeta2KanAlter())/(CB2*MW*SB*SW) - (0.21875D0*&
  &CA1*CA22*EL*m12squared*SA3*dBeta2KanAlter())/(CB2*MW*SB*SW) - (0.84375D0*CA1*EL*m12squared*SA12*SA3*dBeta2KanAlter())/(CB2*MW*&
  &SB*SW) - (0.84375D0*CA1*CA22*EL*m12squared*SA12*SA3*dBeta2KanAlter())/(CB2*MW*SB*SW) - (0.0625D0*CA1*EL*m12squared*SA22*SA3*dB&
  &eta2KanAlter())/(MW*SB*SW) + (0.21875D0*CA1*EL*m12squared*SA22*SA3*dBeta2KanAlter())/(CB2*MW*SB*SW) + (0.84375D0*CA1*EL*m12squ&
  &ared*SA12*SA22*SA3*dBeta2KanAlter())/(CB2*MW*SB*SW) + (0.09375D0*CA1*CA3*EL*m12squared*SA2*dBeta2KanAlter())/(CB*MW*SB2*SW) + &
  &(0.28125D0*CA1*CA22*CA3*EL*m12squared*SA2*dBeta2KanAlter())/(CB*MW*SB2*SW) - (0.09375D0*CA3*EL*MH12*SA1*SA2*dBeta2KanAlter())/&
  &(CB*MW*SB2*SW) + (0.09375D0*CA12*CA3*EL*MH12*SA1*SA2*dBeta2KanAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA22*CA3*EL*MH12*SA1*SA2*dB&
  &eta2KanAlter())/(CB*MW*SB2*SW) + (0.28125D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dBeta2KanAlter())/(CB*MW*SB2*SW) - (0.046875D0*CA3*E&
  &L*MH32*SA1*SA2*dBeta2KanAlter())/(CB*MW*SB2*SW) + (0.046875D0*CA12*CA3*EL*MH32*SA1*SA2*dBeta2KanAlter())/(CB*MW*SB2*SW) - (0.1&
  &40625D0*CA22*CA3*EL*MH32*SA1*SA2*dBeta2KanAlter())/(CB*MW*SB2*SW) + (0.140625D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*dBeta2KanAlter()&
  &)/(CB*MW*SB2*SW) - (0.28125D0*CA1*CA3*EL*m12squared*SA12*SA2*dBeta2KanAlter())/(CB*MW*SB2*SW) - (0.84375D0*CA1*CA22*CA3*EL*m12&
  &squared*SA12*SA2*dBeta2KanAlter())/(CB*MW*SB2*SW) - (0.0625D0*CA1*EL*MH12*SA3*dBeta2KanAlter())/(CB*MW*SB2*SW) - (0.0625D0*CA1&
  &*CA22*EL*MH12*SA3*dBeta2KanAlter())/(CB*MW*SB2*SW) - (0.03125D0*CA1*EL*MH32*SA3*dBeta2KanAlter())/(CB*MW*SB2*SW) - (0.03125D0*&
  &CA1*CA22*EL*MH32*SA3*dBeta2KanAlter())/(CB*MW*SB2*SW) - (0.0625D0*EL*m12squared*SA1*SA3*dBeta2KanAlter())/(CB*MW*SB2*SW) - (0.&
  &5625D0*CA12*EL*m12squared*SA1*SA3*dBeta2KanAlter())/(CB*MW*SB2*SW) - (0.0625D0*CA22*EL*m12squared*SA1*SA3*dBeta2KanAlter())/(C&
  &B*MW*SB2*SW) - (0.5625D0*CA12*CA22*EL*m12squared*SA1*SA3*dBeta2KanAlter())/(CB*MW*SB2*SW) - (0.1875D0*CA1*EL*MH12*SA12*SA3*dBe&
  &ta2KanAlter())/(CB*MW*SB2*SW) - (0.1875D0*CA1*CA22*EL*MH12*SA12*SA3*dBeta2KanAlter())/(CB*MW*SB2*SW) - (0.09375D0*CA1*EL*MH32*&
  &SA12*SA3*dBeta2KanAlter())/(CB*MW*SB2*SW) - (0.09375D0*CA1*CA22*EL*MH32*SA12*SA3*dBeta2KanAlter())/(CB*MW*SB2*SW) + (0.0625D0*&
  &CA1*EL*MH12*SA22*SA3*dBeta2KanAlter())/(CB*MW*SB2*SW) + (0.03125D0*CA1*EL*MH32*SA22*SA3*dBeta2KanAlter())/(CB*MW*SB2*SW) + (0.&
  &0625D0*EL*m12squared*SA1*SA22*SA3*dBeta2KanAlter())/(CB*MW*SB2*SW) + (0.5625D0*CA12*EL*m12squared*SA1*SA22*SA3*dBeta2KanAlter(&
  &))/(CB*MW*SB2*SW) + (0.1875D0*CA1*EL*MH12*SA12*SA22*SA3*dBeta2KanAlter())/(CB*MW*SB2*SW) + (0.09375D0*CA1*EL*MH32*SA12*SA22*SA&
  &3*dBeta2KanAlter())/(CB*MW*SB2*SW) - (0.1875D0*CA1*CA3*EL*m12squared*SA2*dBeta2KanAlter())/(MW*SB*SW*TB) - (0.5625D0*CA1*CA22*&
  &CA3*EL*m12squared*SA2*dBeta2KanAlter())/(MW*SB*SW*TB) - (0.09375D0*CA3*EL*MH12*SA1*SA2*dBeta2KanAlter())/(MW*SB*SW*TB) + (0.09&
  &375D0*CA12*CA3*EL*MH12*SA1*SA2*dBeta2KanAlter())/(MW*SB*SW*TB) - (0.28125D0*CA22*CA3*EL*MH12*SA1*SA2*dBeta2KanAlter())/(MW*SB*&
  &SW*TB) + (0.28125D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dBeta2KanAlter())/(MW*SB*SW*TB) - (0.046875D0*CA3*EL*MH32*SA1*SA2*dBeta2KanA&
  &lter())/(MW*SB*SW*TB) + (0.046875D0*CA12*CA3*EL*MH32*SA1*SA2*dBeta2KanAlter())/(MW*SB*SW*TB) - (0.140625D0*CA22*CA3*EL*MH32*SA&
  &1*SA2*dBeta2KanAlter())/(MW*SB*SW*TB) + (0.140625D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*dBeta2KanAlter())/(MW*SB*SW*TB) - (0.0625D0*&
  &CA1*EL*MH12*SA3*dBeta2KanAlter())/(MW*SB*SW*TB) - (0.0625D0*CA1*CA22*EL*MH12*SA3*dBeta2KanAlter())/(MW*SB*SW*TB) - (0.03125D0*&
  &CA1*EL*MH32*SA3*dBeta2KanAlter())/(MW*SB*SW*TB) - (0.03125D0*CA1*CA22*EL*MH32*SA3*dBeta2KanAlter())/(MW*SB*SW*TB) + (0.125D0*E&
  &L*m12squared*SA1*SA3*dBeta2KanAlter())/(MW*SB*SW*TB) + (0.125D0*CA22*EL*m12squared*SA1*SA3*dBeta2KanAlter())/(MW*SB*SW*TB) - (&
  &0.1875D0*CA1*EL*MH12*SA12*SA3*dBeta2KanAlter())/(MW*SB*SW*TB) - (0.1875D0*CA1*CA22*EL*MH12*SA12*SA3*dBeta2KanAlter())/(MW*SB*S&
  &W*TB) - (0.09375D0*CA1*EL*MH32*SA12*SA3*dBeta2KanAlter())/(MW*SB*SW*TB) - (0.09375D0*CA1*CA22*EL*MH32*SA12*SA3*dBeta2KanAlter(&
  &))/(MW*SB*SW*TB) + (0.0625D0*CA1*EL*MH12*SA22*SA3*dBeta2KanAlter())/(MW*SB*SW*TB) + (0.03125D0*CA1*EL*MH32*SA22*SA3*dBeta2KanA&
  &lter())/(MW*SB*SW*TB) - (0.125D0*EL*m12squared*SA1*SA22*SA3*dBeta2KanAlter())/(MW*SB*SW*TB) + (0.1875D0*CA1*EL*MH12*SA12*SA22*&
  &SA3*dBeta2KanAlter())/(MW*SB*SW*TB) + (0.09375D0*CA1*EL*MH32*SA12*SA22*SA3*dBeta2KanAlter())/(MW*SB*SW*TB) + (0.1875D0*CA1*CA3&
  &*EL*MH12*SA2*TB*dBeta2KanAlter())/(CB*MW*SW) + (0.5625D0*CA1*CA22*CA3*EL*MH12*SA2*TB*dBeta2KanAlter())/(CB*MW*SW) + (0.09375D0&
  &*CA1*CA3*EL*MH32*SA2*TB*dBeta2KanAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA22*CA3*EL*MH32*SA2*TB*dBeta2KanAlter())/(CB*MW*SW) + (&
  &0.328125D0*CA3*EL*m12squared*SA1*SA2*TB*dBeta2KanAlter())/(CB*MW*SW) + (0.984375D0*CA22*CA3*EL*m12squared*SA1*SA2*TB*dBeta2Kan&
  &Alter())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*MH12*SA12*SA2*TB*dBeta2KanAlter())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*EL*MH12*SA12&
  &*SA2*TB*dBeta2KanAlter())/(CB*MW*SW) - (0.09375D0*CA1*CA3*EL*MH32*SA12*SA2*TB*dBeta2KanAlter())/(CB*MW*SW) - (0.28125D0*CA1*CA&
  &22*CA3*EL*MH32*SA12*SA2*TB*dBeta2KanAlter())/(CB*MW*SW) + (0.21875D0*CA1*EL*m12squared*SA3*TB*dBeta2KanAlter())/(CB*MW*SW) + (&
  &0.21875D0*CA1*CA22*EL*m12squared*SA3*TB*dBeta2KanAlter())/(CB*MW*SW) - (0.125D0*EL*MH12*SA1*SA3*TB*dBeta2KanAlter())/(CB*MW*SW&
  &) - (0.375D0*CA12*EL*MH12*SA1*SA3*TB*dBeta2KanAlter())/(CB*MW*SW) - (0.125D0*CA22*EL*MH12*SA1*SA3*TB*dBeta2KanAlter())/(CB*MW*&
  &SW) - (0.375D0*CA12*CA22*EL*MH12*SA1*SA3*TB*dBeta2KanAlter())/(CB*MW*SW) - (0.0625D0*EL*MH32*SA1*SA3*TB*dBeta2KanAlter())/(CB*&
  &MW*SW) - (0.1875D0*CA12*EL*MH32*SA1*SA3*TB*dBeta2KanAlter())/(CB*MW*SW) - (0.0625D0*CA22*EL*MH32*SA1*SA3*TB*dBeta2KanAlter())/&
  &(CB*MW*SW) - (0.1875D0*CA12*CA22*EL*MH32*SA1*SA3*TB*dBeta2KanAlter())/(CB*MW*SW) - (0.21875D0*CA1*EL*m12squared*SA22*SA3*TB*dB&
  &eta2KanAlter())/(CB*MW*SW) + (0.125D0*EL*MH12*SA1*SA22*SA3*TB*dBeta2KanAlter())/(CB*MW*SW) + (0.375D0*CA12*EL*MH12*SA1*SA22*SA&
  &3*TB*dBeta2KanAlter())/(CB*MW*SW) + (0.0625D0*EL*MH32*SA1*SA22*SA3*TB*dBeta2KanAlter())/(CB*MW*SW) + (0.1875D0*CA12*EL*MH32*SA&
  &1*SA22*SA3*TB*dBeta2KanAlter())/(CB*MW*SW) + (0.140625D0*CA3*EL*m12squared*SA1*SA2*dBeta2KanAlter())/(MW*SB*SW*TB2) + (0.42187&
  &5D0*CA22*CA3*EL*m12squared*SA1*SA2*dBeta2KanAlter())/(MW*SB*SW*TB2) + (0.09375D0*CA1*EL*m12squared*SA3*dBeta2KanAlter())/(MW*S&
  &B*SW*TB2) + (0.09375D0*CA1*CA22*EL*m12squared*SA3*dBeta2KanAlter())/(MW*SB*SW*TB2) - (0.09375D0*CA1*EL*m12squared*SA22*SA3*dBe&
  &ta2KanAlter())/(MW*SB*SW*TB2) - (0.1875D0*CA1*CA3*EL*m12squared*SA2*TB2*dBeta2KanAlter())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*&
  &EL*m12squared*SA2*TB2*dBeta2KanAlter())/(CB*MW*SW) + (0.125D0*EL*m12squared*SA1*SA3*TB2*dBeta2KanAlter())/(CB*MW*SW) + (0.125D&
  &0*CA22*EL*m12squared*SA1*SA3*TB2*dBeta2KanAlter())/(CB*MW*SW) - (0.125D0*EL*m12squared*SA1*SA22*SA3*TB2*dBeta2KanAlter())/(CB*&
  &MW*SW) + (0.125D0*CA2*CA3*MH12*dBeta2KanAlter())/(CB*SB*vS) + (0.0625D0*CA2*CA3*MH32*dBeta2KanAlter())/(CB*SB*vS) + (0.375D0*C&
  &A2*CA3*MH12*SA22*dBeta2KanAlter())/(CB*SB*vS) + (0.1875D0*CA2*CA3*MH32*SA22*dBeta2KanAlter())/(CB*SB*vS) - (0.125D0*CA2*CA3*MH&
  &12*dBeta2KanAlter())/(TB*vS) - (0.0625D0*CA2*CA3*MH32*dBeta2KanAlter())/(TB*vS) - (0.375D0*CA2*CA3*MH12*SA22*dBeta2KanAlter())&
  &/(TB*vS) - (0.1875D0*CA2*CA3*MH32*SA22*dBeta2KanAlter())/(TB*vS) - (0.125D0*CA2*CA3*MH12*TB*dBeta2KanAlter())/vS - (0.0625D0*C&
  &A2*CA3*MH32*TB*dBeta2KanAlter())/vS - (0.375D0*CA2*CA3*MH12*SA22*TB*dBeta2KanAlter())/vS - (0.1875D0*CA2*CA3*MH32*SA22*TB*dBet&
  &a2KanAlter())/vS - (0.375D0*EL*MH12*SA3*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.375D0*CA22*EL*MH12*SA3*dAlpha1&
  &KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*EL*MH32*SA3*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.18&
  &75D0*CA22*EL*MH32*SA3*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.375D0*EL*MH12*SA22*SA3*dAlpha1KanAlter()*DBLE(CA&
  &1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*EL*MH32*SA22*SA3*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA3*EL*M&
  &H12*SA2*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA22*CA3*EL*MH12*SA2*dAlpha1KanAlter()*DBLE(CA1**INT(3.&
  &D0)))/(MW*SB*SW) - (0.09375D0*CA3*EL*MH32*SA2*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA22*CA3*EL*MH32&
  &*SA2*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.5625D0*EL*m12squared*SA3*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/&
  &(CB2*MW*SB*SW) + (0.5625D0*CA22*EL*m12squared*SA3*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*EL*m12squ&
  &ared*SA22*SA3*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.28125D0*CA3*EL*m12squared*SA2*dAlpha1KanAlter()*DBLE&
  &(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA22*CA3*EL*m12squared*SA2*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW&
  &) + (0.0625D0*CA2*CA3*EL*MH12*dAlpha2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA2*CA3*EL*MH32*dAlpha2KanAlter(&
  &)*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.5625D0*CA2*CA3*EL*MH12*SA22*dAlpha2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.28&
  &125D0*CA2*CA3*EL*MH32*SA22*dAlpha2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA2*CA3*EL*m12squared*dAlpha2KanAlt&
  &er()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.84375D0*CA2*CA3*EL*m12squared*SA22*dAlpha2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB2&
  &*MW*SB*SW) + (0.5D0*CA2*EL*MH12*SA2*SA3*dAlpha2KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.25D0*CA2*EL*MH32*SA2*SA3*dAlph&
  &a2KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.75D0*CA2*EL*m12squared*SA2*SA3*dAlpha2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*&
  &MW*SB2*SW) - (0.0625D0*EL*MH12*SA2*SA3*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA22*EL*MH12*SA2*SA3*dAl&
  &pha3KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*EL*MH32*SA2*SA3*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW&
  &) - (0.09375D0*CA22*EL*MH32*SA2*SA3*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.125D0*CA3*EL*MH12*dAlpha3KanAlter(&
  &)*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.125D0*CA22*CA3*EL*MH12*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.0625D0*&
  &CA3*EL*MH32*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.0625D0*CA22*CA3*EL*MH32*dAlpha3KanAlter()*DBLE(CA1**INT(3.&
  &D0)))/(MW*SB*SW) + (0.125D0*CA3*EL*MH12*SA22*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*CA3*EL*MH32*SA22*d&
  &Alpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*EL*m12squared*SA2*SA3*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(&
  &CB2*MW*SB*SW) + (0.28125D0*CA22*EL*m12squared*SA2*SA3*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*CA3*E&
  &L*m12squared*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*CA22*CA3*EL*m12squared*dAlpha3KanAlter()*DBLE(&
  &CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.1875D0*CA3*EL*m12squared*SA22*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0&
  &.0625D0*EL*MH12*SA3*dBeta2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.0625D0*CA22*EL*MH12*SA3*dBeta2KanAlter()*DBLE(CA1**&
  &INT(3.D0)))/(CB*MW*SW) - (0.03125D0*EL*MH32*SA3*dBeta2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*CA22*EL*MH32*SA&
  &3*dBeta2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*EL*MH12*SA22*SA3*dBeta2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW&
  &*SW) + (0.03125D0*EL*MH32*SA22*SA3*dBeta2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*EL*m12squared*SA3*dBeta2KanA&
  &lter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.28125D0*CA22*EL*m12squared*SA3*dBeta2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW&
  &*SB*SW) - (0.28125D0*EL*m12squared*SA22*SA3*dBeta2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.09375D0*CA3*EL*m12squar&
  &ed*SA2*dBeta2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA22*CA3*EL*m12squared*SA2*dBeta2KanAlter()*DBLE(CA1&
  &**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*EL*MH12*SA3*dBeta2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*CA22*EL&
  &*MH12*SA3*dBeta2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.03125D0*EL*MH32*SA3*dBeta2KanAlter()*DBLE(CA1**INT(3.D0))&
  &)/(CB*MW*SB2*SW) + (0.03125D0*CA22*EL*MH32*SA3*dBeta2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.0625D0*EL*MH12*SA22*&
  &SA3*dBeta2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.03125D0*EL*MH32*SA22*SA3*dBeta2KanAlter()*DBLE(CA1**INT(3.D0)))&
  &/(CB*MW*SB2*SW) + (0.0625D0*EL*MH12*SA3*dBeta2KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.0625D0*CA22*EL*MH12*SA3*dBet&
  &a2KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.03125D0*EL*MH32*SA3*dBeta2KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB)&
  & + (0.03125D0*CA22*EL*MH32*SA3*dBeta2KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.0625D0*EL*MH12*SA22*SA3*dBeta2KanAlte&
  &r()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.03125D0*EL*MH32*SA22*SA3*dBeta2KanAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (&
  &0.0625D0*CA3*EL*MH12*SA2*TB*dBeta2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA22*CA3*EL*MH12*SA2*TB*dBeta2KanAlt&
  &er()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA3*EL*MH32*SA2*TB*dBeta2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.0&
  &9375D0*CA22*CA3*EL*MH32*SA2*TB*dBeta2KanAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA1*CA3*EL*MH12*dAlpha2KanAlter()&
  &*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA1*CA3*EL*MH32*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.84375D0&
  &*CA3*EL*m12squared*SA1*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.5625D0*CA1*CA3*EL*MH12*SA12*dAlpha2KanAlter()*D&
  &BLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA1*CA3*EL*MH32*SA12*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.8437&
  &5D0*CA1*CA3*EL*m12squared*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA1*CA3*EL*m12squared*dAlpha2KanAlter&
  &()*DBLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.5625D0*CA3*EL*MH12*SA1*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.56&
  &25D0*CA12*CA3*EL*MH12*SA1*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*CA3*EL*MH32*SA1*dAlpha2KanAlter()*DB&
  &LE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA12*CA3*EL*MH32*SA1*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) + (0.84375&
  &D0*CA1*CA3*EL*m12squared*SA12*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA3*EL*m12squared*SA1*dAlpha2&
  &KanAlter()*DBLE(CA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA3*EL*m12squared*SA1*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0))&
  &)/(CB*MW*SB2*SW) - (0.28125D0*CA3*EL*m12squared*SA1*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.28125D0*CA1*CA3&
  &*EL*m12squared*TB*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.5D0*MH12*SA3*dAlpha3KanAlter()*DBLE(CA2**INT(3.D0)))&
  &/vS - (0.25D0*MH32*SA3*dAlpha3KanAlter()*DBLE(CA2**INT(3.D0)))/vS - (0.125D0*CA3*MH12*dBeta2KanAlter()*DBLE(CA2**INT(3.D0)))/(&
  &CB*SB*vS) - (0.0625D0*CA3*MH32*dBeta2KanAlter()*DBLE(CA2**INT(3.D0)))/(CB*SB*vS) + (0.125D0*CA3*MH12*dBeta2KanAlter()*DBLE(CA2&
  &**INT(3.D0)))/(TB*vS) + (0.0625D0*CA3*MH32*dBeta2KanAlter()*DBLE(CA2**INT(3.D0)))/(TB*vS) + (0.125D0*CA3*MH12*TB*dBeta2KanAlte&
  &r()*DBLE(CA2**INT(3.D0)))/vS + (0.0625D0*CA3*MH32*TB*dBeta2KanAlter()*DBLE(CA2**INT(3.D0)))/vS + (0.1875D0*CA3*EL*MH12*dAlpha2&
  &KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA3*EL*MH32*dAlpha2KanAlter()*DBLE(CA1**INT(3.D0&
  &))*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA3*EL*m12squared*dAlpha2KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0))&
  &)/(CB2*MW*SB*SW) - (0.375D0*CA1*CA3*EL*m12squared*SA2*dBeta2KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (1.125D0*CA1*CA22*CA3*E&
  &L*m12squared*SA2*dBeta2KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.5625D0*CA1*CA3*EL*m12squared*SA12*SA2*dBeta2KanAlter()*DB&
  &LE(CB**INT(-3.D0)))/(MW*SW) + (1.6875D0*CA1*CA22*CA3*EL*m12squared*SA12*SA2*dBeta2KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (&
  &0.25D0*EL*m12squared*SA1*SA3*dBeta2KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (1.125D0*CA12*EL*m12squared*SA1*SA3*dBeta2KanAlt&
  &er()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.25D0*CA22*EL*m12squared*SA1*SA3*dBeta2KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (1.12&
  &5D0*CA12*CA22*EL*m12squared*SA1*SA3*dBeta2KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.25D0*EL*m12squared*SA1*SA22*SA3*dBeta2&
  &KanAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (1.125D0*CA12*EL*m12squared*SA1*SA22*SA3*dBeta2KanAlter()*DBLE(CB**INT(-3.D0)))/(MW&
  &*SW) - (0.1875D0*CA3*EL*m12squared*SA2*dBeta2KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.5625D0*CA22*CA&
  &3*EL*m12squared*SA2*dBeta2KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.1875D0*CA3*EL*MH12*SA2*dAlpha1Kan&
  &Alter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA22*CA3*EL*MH12*SA2*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) +&
  & (0.09375D0*CA3*EL*MH32*SA2*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA22*CA3*EL*MH32*SA2*dAlpha1KanAlt&
  &er()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA3*EL*m12squared*SA2*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW&
  &) - (0.84375D0*CA22*CA3*EL*m12squared*SA2*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.375D0*EL*MH12*SA3*dAlpha&
  &1KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.375D0*CA22*EL*MH12*SA3*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - &
  &(0.1875D0*EL*MH32*SA3*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA22*EL*MH32*SA3*dAlpha1KanAlter()*DBLE(S&
  &A1**INT(3.D0)))/(MW*SB*SW) + (0.375D0*EL*MH12*SA22*SA3*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*EL*MH32*&
  &SA22*SA3*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.5625D0*EL*m12squared*SA3*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0&
  &)))/(CB*MW*SB2*SW) + (0.5625D0*CA22*EL*m12squared*SA3*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.5625D0*EL*m1&
  &2squared*SA22*SA3*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.5D0*CA2*EL*MH12*SA2*SA3*dAlpha2KanAlter()*DBLE(S&
  &A1**INT(3.D0)))/(CB*MW*SW) - (0.25D0*CA2*EL*MH32*SA2*SA3*dAlpha2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA2*CA&
  &3*EL*MH12*dAlpha2KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA2*CA3*EL*MH32*dAlpha2KanAlter()*DBLE(SA1**INT(3.D0&
  &)))/(MW*SB*SW) - (0.5625D0*CA2*CA3*EL*MH12*SA22*dAlpha2KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA2*CA3*EL*MH3&
  &2*SA22*dAlpha2KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.75D0*CA2*EL*m12squared*SA2*SA3*dAlpha2KanAlter()*DBLE(SA1**INT(&
  &3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA2*CA3*EL*m12squared*dAlpha2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*&
  &CA2*CA3*EL*m12squared*SA22*dAlpha2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.125D0*CA3*EL*MH12*dAlpha3KanAlter()*DBL&
  &E(SA1**INT(3.D0)))/(CB*MW*SW) + (0.125D0*CA22*CA3*EL*MH12*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA3*E&
  &L*MH32*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA22*CA3*EL*MH32*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))&
  &/(CB*MW*SW) - (0.125D0*CA3*EL*MH12*SA22*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.0625D0*CA3*EL*MH32*SA22*dAlpha&
  &3KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA3*EL*m12squared*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*&
  &SW) - (0.1875D0*CA22*CA3*EL*m12squared*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA&
  &22*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D0*EL*MH12*SA2*SA3*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/&
  &(MW*SB*SW) - (0.1875D0*CA22*EL*MH12*SA2*SA3*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.03125D0*EL*MH32*SA2*SA3*dA&
  &lpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA22*EL*MH32*SA2*SA3*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(MW&
  &*SB*SW) + (0.09375D0*EL*m12squared*SA2*SA3*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA22*EL*m12squa&
  &red*SA2*SA3*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.03125D0*CA3*EL*MH12*SA2*dBeta2KanAlter()*DBLE(SA1**INT&
  &(3.D0)))/(CB*MW*SW) + (0.09375D0*CA22*CA3*EL*MH12*SA2*dBeta2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.015625D0*CA3*EL*M&
  &H32*SA2*dBeta2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.046875D0*CA22*CA3*EL*MH32*SA2*dBeta2KanAlter()*DBLE(SA1**INT(3.&
  &D0)))/(CB*MW*SW) - (0.140625D0*CA3*EL*m12squared*SA2*dBeta2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.421875D0*CA22*&
  &CA3*EL*m12squared*SA2*dBeta2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.03125D0*CA3*EL*MH12*SA2*dBeta2KanAlter()*DBLE&
  &(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.09375D0*CA22*CA3*EL*MH12*SA2*dBeta2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.&
  &015625D0*CA3*EL*MH32*SA2*dBeta2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.046875D0*CA22*CA3*EL*MH32*SA2*dBeta2KanAlt&
  &er()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*EL*m12squared*SA3*dBeta2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) &
  &+ (0.1875D0*CA22*EL*m12squared*SA3*dBeta2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.1875D0*EL*m12squared*SA22*SA3*dB&
  &eta2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.03125D0*CA3*EL*MH12*SA2*dBeta2KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB&
  &*SW*TB) - (0.09375D0*CA22*CA3*EL*MH12*SA2*dBeta2KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.015625D0*CA3*EL*MH32*SA2*d&
  &Beta2KanAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.046875D0*CA22*CA3*EL*MH32*SA2*dBeta2KanAlter()*DBLE(SA1**INT(3.D0)))/&
  &(MW*SB*SW*TB) + (0.125D0*EL*MH12*SA3*TB*dBeta2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.125D0*CA22*EL*MH12*SA3*TB*dBeta&
  &2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*EL*MH32*SA3*TB*dBeta2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0&
  &.0625D0*CA22*EL*MH32*SA3*TB*dBeta2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.125D0*EL*MH12*SA22*SA3*TB*dBeta2KanAlter()*&
  &DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.0625D0*EL*MH32*SA22*SA3*TB*dBeta2KanAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0&
  &*CA3*EL*MH12*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA3*EL*MH32*dAlpha2KanAlter(&
  &)*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*dAlpha2KanAlter()*DBLE(CA2**INT(3.D0))*&
  &DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.375D0*EL*m12squared*SA3*dBeta2KanAlter()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/&
  &(MW*SW) - (0.375D0*CA22*EL*m12squared*SA3*dBeta2KanAlter()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) + (0.375D0*EL*m1&
  &2squared*SA22*SA3*dBeta2KanAlter()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) - (0.28125D0*CA1*CA3*EL*m12squared*dAlph&
  &a1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA3*EL*MH12*SA1*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) +&
  & (0.5625D0*CA12*CA3*EL*MH12*SA1*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA3*EL*MH32*SA1*dAlpha1KanAlte&
  &r()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA12*CA3*EL*MH32*SA1*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0&
  &.1875D0*CA1*CA3*EL*MH12*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA1*CA3*EL*MH32*dAlpha1KanAlter()*DBLE&
  &(SA2**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*CA3*EL*m12squared*SA1*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.1875D0&
  &*CA3*EL*m12squared*SA1*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.84375D0*CA12*CA3*EL*m12squared*SA1*dAlpha1K&
  &anAlter()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA3*EL*MH12*SA12*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB&
  &*SW) - (0.28125D0*CA1*CA3*EL*MH32*SA12*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*m12squared*dA&
  &lpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA1*CA3*EL*m12squared*SA12*dAlpha1KanAlter()*DBLE(SA2**INT(3&
  &.D0)))/(CB*MW*SB2*SW) + (0.09375D0*CA1*CA3*EL*m12squared*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA&
  &3*EL*m12squared*SA1*TB*dAlpha1KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (1.5D0*CA3*MH12*dAlpha2KanAlter()*DBLE(SA2**INT(3.&
  &D0)))/vS + (0.75D0*CA3*MH32*dAlpha2KanAlter()*DBLE(SA2**INT(3.D0)))/vS + (0.1875D0*CA1*EL*MH12*SA3*dAlpha3KanAlter()*DBLE(SA2*&
  &*INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1*EL*MH32*SA3*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*EL*m12squ&
  &ared*SA1*SA3*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA1*EL*MH12*SA12*SA3*dAlpha3KanAlter()*DBLE(SA2**I&
  &NT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1*EL*MH32*SA12*SA3*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA1*EL&
  &*m12squared*SA3*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA1*EL*m12squared*SA3*dAlpha3KanAlter()*DBLE(SA&
  &2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*EL*MH12*SA1*SA3*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA12*&
  &EL*MH12*SA1*SA3*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*EL*MH32*SA1*SA3*dAlpha3KanAlter()*DBLE(SA2**IN&
  &T(3.D0)))/(MW*SB*SW) - (0.09375D0*CA12*EL*MH32*SA1*SA3*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*CA1*EL*&
  &m12squared*SA12*SA3*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*EL*m12squared*SA1*SA3*dAlpha3KanAlter()&
  &*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA12*EL*m12squared*SA1*SA3*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*S&
  &B2*SW) - (0.09375D0*EL*m12squared*SA1*SA3*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA1*EL*m12squared&
  &*SA3*TB*dAlpha3KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA3*EL*MH12*SA1*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))&
  &/(CB*MW*SW) + (0.09375D0*CA12*CA3*EL*MH12*SA1*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.046875D0*CA3*EL*MH32*SA1*&
  &dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.046875D0*CA12*CA3*EL*MH32*SA1*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/(C&
  &B*MW*SW) - (0.09375D0*CA3*EL*m12squared*SA1*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.328125D0*CA3*EL*m12squared*&
  &SA1*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.421875D0*CA12*CA3*EL*m12squared*SA1*dBeta2KanAlter()*DBLE(SA2**&
  &INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA1*CA3*EL*m12squared*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.09375&
  &D0*CA3*EL*MH12*SA1*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.09375D0*CA12*CA3*EL*MH12*SA1*dBeta2KanAlter()*DB&
  &LE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.046875D0*CA3*EL*MH32*SA1*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.04&
  &6875D0*CA12*CA3*EL*MH32*SA1*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA1*CA3*EL*m12squared*SA12*dBet&
  &a2KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*CA1*CA3*EL*m12squared*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/(MW&
  &*SB*SW*TB) + (0.09375D0*CA3*EL*MH12*SA1*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA12*CA3*EL*MH12*SA1&
  &*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.046875D0*CA3*EL*MH32*SA1*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/(MW&
  &*SB*SW*TB) - (0.046875D0*CA12*CA3*EL*MH32*SA1*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.1875D0*CA1*CA3*EL*MH12&
  &*TB*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1*CA3*EL*MH32*TB*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/(&
  &CB*MW*SW) - (0.328125D0*CA3*EL*m12squared*SA1*TB*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA1*CA3*EL*MH12&
  &*SA12*TB*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1*CA3*EL*MH32*SA12*TB*dBeta2KanAlter()*DBLE(SA2**INT&
  &(3.D0)))/(CB*MW*SW) - (0.140625D0*CA3*EL*m12squared*SA1*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB2) + (0.1875D0*CA1*&
  &CA3*EL*m12squared*TB2*dBeta2KanAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA3*EL*MH12*dAlpha1KanAlter()*DBLE(CA1**IN&
  &T(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA3*EL*MH32*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))&
  &)/(MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*dAlpha1KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0&
  &625D0*EL*MH12*SA3*dAlpha3KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*EL*MH32*SA3*dAlpha3KanA&
  &lter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*EL*m12squared*SA3*dAlpha3KanAlter()*DBLE(CA1**INT(3.&
  &D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA3*EL*m12squared*dBeta2KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3&
  &.D0)))/(CB*MW*SB2*SW) - (0.0625D0*CA3*EL*MH12*TB*dBeta2KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.0&
  &3125D0*CA3*EL*MH32*TB*dBeta2KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.375D0*CA1*CA3*EL*m12squared*&
  &dBeta2KanAlter()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) - (0.5625D0*CA1*CA3*EL*m12squared*SA12*dBeta2KanAlter()*DB&
  &LE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) + (0.1875D0*CA3*EL*m12squared*dBeta2KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(CB**&
  &INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) - (0.1875D0*CA3*EL*MH12*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))&
  &/(CB*MW*SW) - (0.09375D0*CA3*EL*MH32*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA3*&
  &EL*m12squared*dAlpha1KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.0625D0*EL*MH12*SA3*dAlpha3KanAl&
  &ter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*EL*MH32*SA3*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0))*DB&
  &LE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*EL*m12squared*SA3*dAlpha3KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB&
  &*MW*SB2*SW) - (0.03125D0*CA3*EL*MH12*dBeta2KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.015625D0*CA3*&
  &EL*MH32*dBeta2KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.140625D0*CA3*EL*m12squared*dBeta2KanAlter(&
  &)*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.03125D0*CA3*EL*MH12*dBeta2KanAlter()*DBLE(SA1**INT(3.D0))*DBL&
  &E(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.015625D0*CA3*EL*MH32*dBeta2KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW&
  &*SB2*SW) + (0.03125D0*CA3*EL*MH12*dBeta2KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.015625D0*CA3*&
  &EL*MH32*dBeta2KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.328125D0*CA3*EL*m12squared*SA1*SA2*dBet&
  &a2KanAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.421875D0*CA12*CA3*EL*m12squared*SA1*SA2*dBeta2KanAlter()*DBLE(SB**INT(-3.D0)))&
  &/(MW*SW) + (0.984375D0*CA22*CA3*EL*m12squared*SA1*SA2*dBeta2KanAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (1.265625D0*CA12*CA22*C&
  &A3*EL*m12squared*SA1*SA2*dBeta2KanAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.09375D0*CA3*EL*m12squared*SA1*SA2*dBeta2KanAlter(&
  &)*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.140625D0*CA12*CA3*EL*m12squared*SA1*SA2*dBeta2KanAlter()*DBLE(SB**INT(-3.D0)))/(CB2*M&
  &W*SW) + (0.28125D0*CA22*CA3*EL*m12squared*SA1*SA2*dBeta2KanAlter()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.421875D0*CA12*CA22*C&
  &A3*EL*m12squared*SA1*SA2*dBeta2KanAlter()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.21875D0*CA1*EL*m12squared*SA3*dBeta2KanAlter(&
  &)*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.21875D0*CA1*CA22*EL*m12squared*SA3*dBeta2KanAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.06&
  &25D0*CA1*EL*m12squared*SA3*dBeta2KanAlter()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.0625D0*CA1*CA22*EL*m12squared*SA3*dBeta2Kan&
  &Alter()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.84375D0*CA1*EL*m12squared*SA12*SA3*dBeta2KanAlter()*DBLE(SB**INT(-3.D0)))/(MW*S&
  &W) + (0.84375D0*CA1*CA22*EL*m12squared*SA12*SA3*dBeta2KanAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.28125D0*CA1*EL*m12squared*&
  &SA12*SA3*dBeta2KanAlter()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.28125D0*CA1*CA22*EL*m12squared*SA12*SA3*dBeta2KanAlter()*DBLE&
  &(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.21875D0*CA1*EL*m12squared*SA22*SA3*dBeta2KanAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.0625&
  &D0*CA1*EL*m12squared*SA22*SA3*dBeta2KanAlter()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.84375D0*CA1*EL*m12squared*SA12*SA22*SA3*&
  &dBeta2KanAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.28125D0*CA1*EL*m12squared*SA12*SA22*SA3*dBeta2KanAlter()*DBLE(SB**INT(-3.D&
  &0)))/(CB2*MW*SW) - (0.28125D0*EL*m12squared*SA3*dBeta2KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.28125&
  &D0*CA22*EL*m12squared*SA3*dBeta2KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.09375D0*EL*m12squared*SA3*d&
  &Beta2KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.09375D0*CA22*EL*m12squared*SA3*dBeta2KanAlter()*DB&
  &LE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.28125D0*EL*m12squared*SA22*SA3*dBeta2KanAlter()*DBLE(CA1**INT(3.D0))&
  &*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.09375D0*EL*m12squared*SA22*SA3*dBeta2KanAlter()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))&
  &/(CB2*MW*SW) + (0.140625D0*CA3*EL*m12squared*SA2*dBeta2KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.4218&
  &75D0*CA22*CA3*EL*m12squared*SA2*dBeta2KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.046875D0*CA3*EL*m12sq&
  &uared*SA2*dBeta2KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.140625D0*CA22*CA3*EL*m12squared*SA2*dBe&
  &ta2KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.328125D0*CA3*EL*m12squared*SA1*dBeta2KanAlter()*DBLE&
  &(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.421875D0*CA12*CA3*EL*m12squared*SA1*dBeta2KanAlter()*DBLE(SA2**INT(3.D0))*&
  &DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.09375D0*CA3*EL*m12squared*SA1*dBeta2KanAlter()*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(&
  &CB2*MW*SW) + (0.140625D0*CA12*CA3*EL*m12squared*SA1*dBeta2KanAlter()*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - &
  &(0.140625D0*CA3*EL*m12squared*dBeta2KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.04&
  &6875D0*CA3*EL*m12squared*dBeta2KanAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.187&
  &5D0*CA1*CA3*MH12*SA2*dgAtMZ())/(CB*MW) + (0.5625D0*CA1*CA22*CA3*MH12*SA2*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*CA3*MH32*SA2*dgAtM&
  &Z())/(CB*MW) + (0.28125D0*CA1*CA22*CA3*MH32*SA2*dgAtMZ())/(CB*MW) + (0.28125D0*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*MW) + (0.8&
  &4375D0*CA22*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*MW) - (0.1875D0*CA1*CA3*MH12*SA12*SA2*dgAtMZ())/(CB*MW) - (0.5625D0*CA1*CA22*&
  &CA3*MH12*SA12*SA2*dgAtMZ())/(CB*MW) - (0.09375D0*CA1*CA3*MH32*SA12*SA2*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*CA3*MH32*SA12*S&
  &A2*dgAtMZ())/(CB*MW) + (0.1875D0*CA1*m12squared*SA3*dgAtMZ())/(CB*MW) + (0.1875D0*CA1*CA22*m12squared*SA3*dgAtMZ())/(CB*MW) - &
  &(0.125D0*MH12*SA1*SA3*dgAtMZ())/(CB*MW) - (0.375D0*CA12*MH12*SA1*SA3*dgAtMZ())/(CB*MW) - (0.125D0*CA22*MH12*SA1*SA3*dgAtMZ())/&
  &(CB*MW) - (0.375D0*CA12*CA22*MH12*SA1*SA3*dgAtMZ())/(CB*MW) - (0.0625D0*MH32*SA1*SA3*dgAtMZ())/(CB*MW) - (0.1875D0*CA12*MH32*S&
  &A1*SA3*dgAtMZ())/(CB*MW) - (0.0625D0*CA22*MH32*SA1*SA3*dgAtMZ())/(CB*MW) - (0.1875D0*CA12*CA22*MH32*SA1*SA3*dgAtMZ())/(CB*MW) &
  &- (0.1875D0*CA1*m12squared*SA22*SA3*dgAtMZ())/(CB*MW) + (0.125D0*MH12*SA1*SA22*SA3*dgAtMZ())/(CB*MW) + (0.375D0*CA12*MH12*SA1*&
  &SA22*SA3*dgAtMZ())/(CB*MW) + (0.0625D0*MH32*SA1*SA22*SA3*dgAtMZ())/(CB*MW) + (0.1875D0*CA12*MH32*SA1*SA22*SA3*dgAtMZ())/(CB*MW&
  &) + (0.28125D0*CA1*CA3*m12squared*SA2*dgAtMZ())/(MW*SB) + (0.84375D0*CA1*CA22*CA3*m12squared*SA2*dgAtMZ())/(MW*SB) - (0.1875D0&
  &*CA1*CA3*m12squared*SA2*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA1*CA22*CA3*m12squared*SA2*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*CA3*MH&
  &12*SA1*SA2*dgAtMZ())/(MW*SB) - (0.1875D0*CA12*CA3*MH12*SA1*SA2*dgAtMZ())/(MW*SB) + (0.5625D0*CA22*CA3*MH12*SA1*SA2*dgAtMZ())/(&
  &MW*SB) - (0.5625D0*CA12*CA22*CA3*MH12*SA1*SA2*dgAtMZ())/(MW*SB) + (0.09375D0*CA3*MH32*SA1*SA2*dgAtMZ())/(MW*SB) - (0.09375D0*C&
  &A12*CA3*MH32*SA1*SA2*dgAtMZ())/(MW*SB) + (0.28125D0*CA22*CA3*MH32*SA1*SA2*dgAtMZ())/(MW*SB) - (0.28125D0*CA12*CA22*CA3*MH32*SA&
  &1*SA2*dgAtMZ())/(MW*SB) + (0.28125D0*CA1*CA3*m12squared*SA12*SA2*dgAtMZ())/(CB2*MW*SB) + (0.84375D0*CA1*CA22*CA3*m12squared*SA&
  &12*SA2*dgAtMZ())/(CB2*MW*SB) + (0.125D0*CA1*MH12*SA3*dgAtMZ())/(MW*SB) + (0.125D0*CA1*CA22*MH12*SA3*dgAtMZ())/(MW*SB) + (0.062&
  &5D0*CA1*MH32*SA3*dgAtMZ())/(MW*SB) + (0.0625D0*CA1*CA22*MH32*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*m12squared*SA1*SA3*dgAtMZ())/(M&
  &W*SB) - (0.1875D0*CA22*m12squared*SA1*SA3*dgAtMZ())/(MW*SB) + (0.125D0*m12squared*SA1*SA3*dgAtMZ())/(CB2*MW*SB) + (0.5625D0*CA&
  &12*m12squared*SA1*SA3*dgAtMZ())/(CB2*MW*SB) + (0.125D0*CA22*m12squared*SA1*SA3*dgAtMZ())/(CB2*MW*SB) + (0.5625D0*CA12*CA22*m12&
  &squared*SA1*SA3*dgAtMZ())/(CB2*MW*SB) + (0.375D0*CA1*MH12*SA12*SA3*dgAtMZ())/(MW*SB) + (0.375D0*CA1*CA22*MH12*SA12*SA3*dgAtMZ(&
  &))/(MW*SB) + (0.1875D0*CA1*MH32*SA12*SA3*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*CA22*MH32*SA12*SA3*dgAtMZ())/(MW*SB) - (0.125D0*CA1&
  &*MH12*SA22*SA3*dgAtMZ())/(MW*SB) - (0.0625D0*CA1*MH32*SA22*SA3*dgAtMZ())/(MW*SB) + (0.1875D0*m12squared*SA1*SA22*SA3*dgAtMZ())&
  &/(MW*SB) - (0.125D0*m12squared*SA1*SA22*SA3*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA12*m12squared*SA1*SA22*SA3*dgAtMZ())/(CB2*MW*S&
  &B) - (0.375D0*CA1*MH12*SA12*SA22*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*CA1*MH32*SA12*SA22*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*CA3*m1&
  &2squared*SA1*SA2*dgAtMZ())/(CB*MW*SB2) + (0.28125D0*CA12*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA22*CA3*m12&
  &squared*SA1*SA2*dgAtMZ())/(CB*MW*SB2) + (0.84375D0*CA12*CA22*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*MW*SB2) - (0.125D0*CA1*m12sq&
  &uared*SA3*dgAtMZ())/(CB*MW*SB2) - (0.125D0*CA1*CA22*m12squared*SA3*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA1*m12squared*SA12*SA3*d&
  &gAtMZ())/(CB*MW*SB2) - (0.5625D0*CA1*CA22*m12squared*SA12*SA3*dgAtMZ())/(CB*MW*SB2) + (0.125D0*CA1*m12squared*SA22*SA3*dgAtMZ(&
  &))/(CB*MW*SB2) + (0.5625D0*CA1*m12squared*SA12*SA22*SA3*dgAtMZ())/(CB*MW*SB2) - (0.09375D0*CA3*m12squared*SA1*SA2*dgAtMZ())/(M&
  &W*SB*TB) - (0.28125D0*CA22*CA3*m12squared*SA1*SA2*dgAtMZ())/(MW*SB*TB) - (0.0625D0*CA1*m12squared*SA3*dgAtMZ())/(MW*SB*TB) - (&
  &0.0625D0*CA1*CA22*m12squared*SA3*dgAtMZ())/(MW*SB*TB) + (0.0625D0*CA1*m12squared*SA22*SA3*dgAtMZ())/(MW*SB*TB) - (0.09375D0*CA&
  &1*CA3*m12squared*SA2*TB*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*CA3*m12squared*SA2*TB*dgAtMZ())/(CB*MW) + (0.0625D0*m12squared&
  &*SA1*SA3*TB*dgAtMZ())/(CB*MW) + (0.0625D0*CA22*m12squared*SA1*SA3*TB*dgAtMZ())/(CB*MW) - (0.0625D0*m12squared*SA1*SA22*SA3*TB*&
  &dgAtMZ())/(CB*MW) + (0.0625D0*CA3*MH12*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875D0*CA22*CA3*MH12*SA2*DBLE(CA1**INT(&
  &3.D0))*dgAtMZ())/(CB*MW) + (0.03125D0*CA3*MH32*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA22*CA3*MH32*SA2*DBLE(&
  &CA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.09375D0*CA3*m12squared*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.28125D0*CA22&
  &*CA3*m12squared*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.125D0*MH12*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0&
  &.125D0*CA22*MH12*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.0625D0*MH32*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.0&
  &625D0*CA22*MH32*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.125D0*MH12*SA22*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (&
  &0.0625D0*MH32*SA22*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*m12squared*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW&
  &*SB2) + (0.1875D0*CA22*m12squared*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*m12squared*SA22*SA3*DBLE(CA1**INT&
  &(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.125D0*MH12*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.125D0*CA22*MH12*SA3*DBLE(SA1**IN&
  &T(3.D0))*dgAtMZ())/(CB*MW) + (0.0625D0*MH32*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.0625D0*CA22*MH32*SA3*DBLE(SA1**INT&
  &(3.D0))*dgAtMZ())/(CB*MW) - (0.125D0*MH12*SA22*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*MH32*SA22*SA3*DBLE(SA1**&
  &INT(3.D0))*dgAtMZ())/(CB*MW) + (0.0625D0*CA3*MH12*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA22*CA3*MH12*SA2*DBL&
  &E(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.03125D0*CA3*MH32*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA22*CA3*MH3&
  &2*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.1875D0*m12squared*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875D0&
  &*CA22*m12squared*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*m12squared*SA22*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())&
  &/(CB2*MW*SB) - (0.09375D0*CA3*m12squared*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA22*CA3*m12squared*SA2*D&
  &BLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*CA1*CA3*MH12*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.09375D0*CA1*CA3&
  &*MH32*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.28125D0*CA3*m12squared*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875D&
  &0*CA1*CA3*MH12*SA12*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*CA3*MH32*SA12*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*M&
  &W) - (0.28125D0*CA1*CA3*m12squared*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*CA3*m12squared*DBLE(SA2**INT(3.D0))*&
  &dgAtMZ())/(CB2*MW*SB) - (0.1875D0*CA3*MH12*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA12*CA3*MH12*SA1*DBLE(SA2**&
  &INT(3.D0))*dgAtMZ())/(MW*SB) - (0.09375D0*CA3*MH32*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA12*CA3*MH32*SA1*D&
  &BLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.28125D0*CA1*CA3*m12squared*SA12*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) + (0.187&
  &5D0*CA3*m12squared*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA12*CA3*m12squared*SA1*DBLE(SA2**INT(3.D0))*dg&
  &AtMZ())/(CB*MW*SB2) + (0.09375D0*CA3*m12squared*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB*TB) + (0.09375D0*CA1*CA3*m12squared*&
  &TB*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*CA3*MH12*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (&
  &0.03125D0*CA3*MH32*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA3*m12squared*DBLE(CA1**INT(3.D0)&
  &)*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.0625D0*CA3*MH12*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) &
  &- (0.03125D0*CA3*MH32*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA3*m12squared*DBLE(SA1**INT(3.&
  &D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.28125D0*CA3*EL*SA1*SA2*dm122MSBarAlter())/(CB*MW*SW) + (0.84375D0*CA22*CA&
  &3*EL*SA1*SA2*dm122MSBarAlter())/(CB*MW*SW) + (0.1875D0*CA1*EL*SA3*dm122MSBarAlter())/(CB*MW*SW) + (0.1875D0*CA1*CA22*EL*SA3*dm&
  &122MSBarAlter())/(CB*MW*SW) - (0.1875D0*CA1*EL*SA22*SA3*dm122MSBarAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA3*EL*SA2*dm122MSBarAl&
  &ter())/(MW*SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*SA2*dm122MSBarAlter())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*SA2*dm122MSBarAlter())&
  &/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA22*CA3*EL*SA2*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.28125D0*CA1*CA3*EL*SA12*SA2*dm122MSBarA&
  &lter())/(CB2*MW*SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*SA12*SA2*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.1875D0*EL*SA1*SA3*dm122MSB&
  &arAlter())/(MW*SB*SW) - (0.1875D0*CA22*EL*SA1*SA3*dm122MSBarAlter())/(MW*SB*SW) + (0.125D0*EL*SA1*SA3*dm122MSBarAlter())/(CB2*&
  &MW*SB*SW) + (0.5625D0*CA12*EL*SA1*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.125D0*CA22*EL*SA1*SA3*dm122MSBarAlter())/(CB2*MW*&
  &SB*SW) + (0.5625D0*CA12*CA22*EL*SA1*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.1875D0*EL*SA1*SA22*SA3*dm122MSBarAlter())/(MW*S&
  &B*SW) - (0.125D0*EL*SA1*SA22*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.5625D0*CA12*EL*SA1*SA22*SA3*dm122MSBarAlter())/(CB2*MW&
  &*SB*SW) - (0.1875D0*CA3*EL*SA1*SA2*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.28125D0*CA12*CA3*EL*SA1*SA2*dm122MSBarAlter())/(CB*M&
  &W*SB2*SW) - (0.5625D0*CA22*CA3*EL*SA1*SA2*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA22*CA3*EL*SA1*SA2*dm122MSBarAl&
  &ter())/(CB*MW*SB2*SW) - (0.125D0*CA1*EL*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.125D0*CA1*CA22*EL*SA3*dm122MSBarAlter())/(C&
  &B*MW*SB2*SW) - (0.5625D0*CA1*EL*SA12*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*EL*SA12*SA3*dm122MSBarAlter())&
  &/(CB*MW*SB2*SW) + (0.125D0*CA1*EL*SA22*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.5625D0*CA1*EL*SA12*SA22*SA3*dm122MSBarAlter(&
  &))/(CB*MW*SB2*SW) - (0.09375D0*CA3*EL*SA1*SA2*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.28125D0*CA22*CA3*EL*SA1*SA2*dm122MSBarAlte&
  &r())/(MW*SB*SW*TB) - (0.0625D0*CA1*EL*SA3*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.0625D0*CA1*CA22*EL*SA3*dm122MSBarAlter())/(MW*&
  &SB*SW*TB) + (0.0625D0*CA1*EL*SA22*SA3*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.09375D0*CA1*CA3*EL*SA2*TB*dm122MSBarAlter())/(CB*M&
  &W*SW) - (0.28125D0*CA1*CA22*CA3*EL*SA2*TB*dm122MSBarAlter())/(CB*MW*SW) + (0.0625D0*EL*SA1*SA3*TB*dm122MSBarAlter())/(CB*MW*SW&
  &) + (0.0625D0*CA22*EL*SA1*SA3*TB*dm122MSBarAlter())/(CB*MW*SW) - (0.0625D0*EL*SA1*SA22*SA3*TB*dm122MSBarAlter())/(CB*MW*SW) - &
  &(0.09375D0*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.28125D0*CA22*CA3*EL*SA2*DBLE(CA1**INT(3.D0))&
  &*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.1875D0*EL*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.1875D0*CA22*&
  &EL*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.1875D0*EL*SA22*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/&
  &(CB*MW*SB2*SW) - (0.1875D0*EL*SA3*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.1875D0*CA22*EL*SA3*DBLE(SA1**INT&
  &(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.1875D0*EL*SA22*SA3*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.&
  &09375D0*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA22*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*dm&
  &122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SW) - (0.28125D0*CA1*CA&
  &3*EL*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB2*MW&
  &*SB*SW) - (0.28125D0*CA1*CA3*EL*SA12*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*SA1*DBLE(SA2**I&
  &NT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA12*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW&
  &) + (0.09375D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(MW*SB*SW*TB) + (0.09375D0*CA1*CA3*EL*TB*DBLE(SA2**INT(3.D0)&
  &)*dm122MSBarAlter())/(CB*MW*SW) + (0.09375D0*CA3*EL*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW&
  &) + (0.09375D0*CA3*EL*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.1875D0*CA1*CA3*EL*SA2*d&
  &MH12OSAlter())/(CB*MW*SW) + (0.5625D0*CA1*CA22*CA3*EL*SA2*dMH12OSAlter())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*SA12*SA2*dMH12OSAl&
  &ter())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*EL*SA12*SA2*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*EL*SA1*SA3*dMH12OSAlter())/(CB*MW&
  &*SW) - (0.375D0*CA12*EL*SA1*SA3*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*CA22*EL*SA1*SA3*dMH12OSAlter())/(CB*MW*SW) - (0.375D0*CA&
  &12*CA22*EL*SA1*SA3*dMH12OSAlter())/(CB*MW*SW) + (0.125D0*EL*SA1*SA22*SA3*dMH12OSAlter())/(CB*MW*SW) + (0.375D0*CA12*EL*SA1*SA2&
  &2*SA3*dMH12OSAlter())/(CB*MW*SW) + (0.1875D0*CA3*EL*SA1*SA2*dMH12OSAlter())/(MW*SB*SW) - (0.1875D0*CA12*CA3*EL*SA1*SA2*dMH12OS&
  &Alter())/(MW*SB*SW) + (0.5625D0*CA22*CA3*EL*SA1*SA2*dMH12OSAlter())/(MW*SB*SW) - (0.5625D0*CA12*CA22*CA3*EL*SA1*SA2*dMH12OSAlt&
  &er())/(MW*SB*SW) + (0.125D0*CA1*EL*SA3*dMH12OSAlter())/(MW*SB*SW) + (0.125D0*CA1*CA22*EL*SA3*dMH12OSAlter())/(MW*SB*SW) + (0.3&
  &75D0*CA1*EL*SA12*SA3*dMH12OSAlter())/(MW*SB*SW) + (0.375D0*CA1*CA22*EL*SA12*SA3*dMH12OSAlter())/(MW*SB*SW) - (0.125D0*CA1*EL*S&
  &A22*SA3*dMH12OSAlter())/(MW*SB*SW) - (0.375D0*CA1*EL*SA12*SA22*SA3*dMH12OSAlter())/(MW*SB*SW) - (0.5D0*CA2*CA3*dMH12OSAlter())&
  &/vS - (1.5D0*CA2*CA3*SA22*dMH12OSAlter())/vS + (0.0625D0*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) + (0.1875D&
  &0*CA22*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*EL*SA3*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(MW*S&
  &B*SW) - (0.125D0*CA22*EL*SA3*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.125D0*EL*SA22*SA3*DBLE(CA1**INT(3.D0))*dMH12&
  &OSAlter())/(MW*SB*SW) + (0.5D0*CA3*DBLE(CA2**INT(3.D0))*dMH12OSAlter())/vS + (0.125D0*EL*SA3*DBLE(SA1**INT(3.D0))*dMH12OSAlter&
  &())/(CB*MW*SW) + (0.125D0*CA22*EL*SA3*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*EL*SA22*SA3*DBLE(SA1**INT(3.D&
  &0))*dMH12OSAlter())/(CB*MW*SW) + (0.0625D0*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.1875D0*CA22*CA3*EL*&
  &SA2*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) + (&
  &0.1875D0*CA1*CA3*EL*SA12*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.1875D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dMH12OSAl&
  &ter())/(MW*SB*SW) + (0.1875D0*CA12*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) - (0.0625D0*CA3*EL*DBLE(CA1**INT&
  &(3.D0))*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.0625D0*CA3*EL*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH12OSAl&
  &ter())/(MW*SB*SW) + (0.09375D0*CA1*CA3*EL*SA2*dMH32OSAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA22*CA3*EL*SA2*dMH32OSAlter())/(CB*&
  &MW*SW) - (0.09375D0*CA1*CA3*EL*SA12*SA2*dMH32OSAlter())/(CB*MW*SW) - (0.28125D0*CA1*CA22*CA3*EL*SA12*SA2*dMH32OSAlter())/(CB*M&
  &W*SW) - (0.0625D0*EL*SA1*SA3*dMH32OSAlter())/(CB*MW*SW) - (0.1875D0*CA12*EL*SA1*SA3*dMH32OSAlter())/(CB*MW*SW) - (0.0625D0*CA2&
  &2*EL*SA1*SA3*dMH32OSAlter())/(CB*MW*SW) - (0.1875D0*CA12*CA22*EL*SA1*SA3*dMH32OSAlter())/(CB*MW*SW) + (0.0625D0*EL*SA1*SA22*SA&
  &3*dMH32OSAlter())/(CB*MW*SW) + (0.1875D0*CA12*EL*SA1*SA22*SA3*dMH32OSAlter())/(CB*MW*SW) + (0.09375D0*CA3*EL*SA1*SA2*dMH32OSAl&
  &ter())/(MW*SB*SW) - (0.09375D0*CA12*CA3*EL*SA1*SA2*dMH32OSAlter())/(MW*SB*SW) + (0.28125D0*CA22*CA3*EL*SA1*SA2*dMH32OSAlter())&
  &/(MW*SB*SW) - (0.28125D0*CA12*CA22*CA3*EL*SA1*SA2*dMH32OSAlter())/(MW*SB*SW) + (0.0625D0*CA1*EL*SA3*dMH32OSAlter())/(MW*SB*SW)&
  & + (0.0625D0*CA1*CA22*EL*SA3*dMH32OSAlter())/(MW*SB*SW) + (0.1875D0*CA1*EL*SA12*SA3*dMH32OSAlter())/(MW*SB*SW) + (0.1875D0*CA1&
  &*CA22*EL*SA12*SA3*dMH32OSAlter())/(MW*SB*SW) - (0.0625D0*CA1*EL*SA22*SA3*dMH32OSAlter())/(MW*SB*SW) - (0.1875D0*CA1*EL*SA12*SA&
  &22*SA3*dMH32OSAlter())/(MW*SB*SW) - (0.25D0*CA2*CA3*dMH32OSAlter())/vS - (0.75D0*CA2*CA3*SA22*dMH32OSAlter())/vS + (0.03125D0*&
  &CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) + (0.09375D0*CA22*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dMH32OSAlter())/(&
  &CB*MW*SW) - (0.0625D0*EL*SA3*DBLE(CA1**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) - (0.0625D0*CA22*EL*SA3*DBLE(CA1**INT(3.D0))*dMH3&
  &2OSAlter())/(MW*SB*SW) + (0.0625D0*EL*SA22*SA3*DBLE(CA1**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) + (0.25D0*CA3*DBLE(CA2**INT(3.D&
  &0))*dMH32OSAlter())/vS + (0.0625D0*EL*SA3*DBLE(SA1**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) + (0.0625D0*CA22*EL*SA3*DBLE(SA1**IN&
  &T(3.D0))*dMH32OSAlter())/(CB*MW*SW) - (0.0625D0*EL*SA22*SA3*DBLE(SA1**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) + (0.03125D0*CA3*E&
  &L*SA2*DBLE(SA1**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) + (0.09375D0*CA22*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*dMH32OSAlter())/(MW*SB&
  &*SW) - (0.09375D0*CA1*CA3*EL*DBLE(SA2**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) + (0.09375D0*CA1*CA3*EL*SA12*DBLE(SA2**INT(3.D0))&
  &*dMH32OSAlter())/(CB*MW*SW) - (0.09375D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) + (0.09375D0*CA12*CA3*EL*S&
  &A1*DBLE(SA2**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) - (0.03125D0*CA3*EL*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH32OSAlter(&
  &))/(CB*MW*SW) - (0.03125D0*CA3*EL*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) - (0.09375D0*CA1*CA3*EL&
  &*MH12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.28125D0*CA1*CA22*CA3*EL*MH12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(C&
  &B*SW) - (0.046875D0*CA1*CA3*EL*MH32*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.140625D0*CA1*CA22*CA3*EL*MH32*SA2*DBLE(&
  &MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.140625D0*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.42&
  &1875D0*CA22*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.09375D0*CA1*CA3*EL*MH12*SA12*SA2*DBLE(MW*&
  &*INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.28125D0*CA1*CA22*CA3*EL*MH12*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.04&
  &6875D0*CA1*CA3*EL*MH32*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*CA1*CA22*CA3*EL*MH32*SA12*SA2*DBLE(MW*&
  &*INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA&
  &1*CA22*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0625D0*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter(&
  &))/(CB*SW) + (0.1875D0*CA12*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0625D0*CA22*EL*MH12*SA1*SA3*DBLE(MW&
  &**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.1875D0*CA12*CA22*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.03125D&
  &0*EL*MH32*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.09375D0*CA12*EL*MH32*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter()&
  &)/(CB*SW) + (0.03125D0*CA22*EL*MH32*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.09375D0*CA12*CA22*EL*MH32*SA1*SA3*D&
  &BLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.09375D0*CA1*EL*m12squared*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (&
  &0.0625D0*EL*MH12*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.1875D0*CA12*EL*MH12*SA1*SA22*SA3*DBLE(MW**INT(-3.&
  &D0))*dMW2Alter())/(CB*SW) - (0.03125D0*EL*MH32*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA12*EL*MH3&
  &2*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.140625D0*CA1*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*dMW2Alte&
  &r())/(SB*SW) - (0.421875D0*CA1*CA22*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA1*CA3*EL*m1&
  &2squared*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.28125D0*CA1*CA22*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*dM&
  &W2Alter())/(CB2*SB*SW) - (0.09375D0*CA3*EL*MH12*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA12*CA3*EL*MH1&
  &2*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.28125D0*CA22*CA3*EL*MH12*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(S&
  &B*SW) + (0.28125D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*CA3*EL*MH32*SA1*SA2*D&
  &BLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.046875D0*CA12*CA3*EL*MH32*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0&
  &.140625D0*CA22*CA3*EL*MH32*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.140625D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*DBLE(&
  &MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.140625D0*CA1*CA3*EL*m12squared*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW&
  &) - (0.421875D0*CA1*CA22*CA3*EL*m12squared*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.0625D0*CA1*EL*MH12*SA3*&
  &DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.0625D0*CA1*CA22*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.0312&
  &5D0*CA1*EL*MH32*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.03125D0*CA1*CA22*EL*MH32*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter&
  &())/(SB*SW) + (0.09375D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA22*EL*m12squared*SA1*S&
  &A3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.0625D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) -&
  & (0.28125D0*CA12*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.0625D0*CA22*EL*m12squared*SA1*SA3*DB&
  &LE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.28125D0*CA12*CA22*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB&
  &2*SB*SW) - (0.1875D0*CA1*EL*MH12*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.1875D0*CA1*CA22*EL*MH12*SA12*SA3*DBLE&
  &(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*CA1*EL*MH32*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0&
  &*CA1*CA22*EL*MH32*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.0625D0*CA1*EL*MH12*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW&
  &2Alter())/(SB*SW) + (0.03125D0*CA1*EL*MH32*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*EL*m12squared*SA1*S&
  &A22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.0625D0*EL*m12squared*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB&
  &2*SB*SW) + (0.28125D0*CA12*EL*m12squared*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.1875D0*CA1*EL*MH12*SA&
  &12*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA1*EL*MH32*SA12*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())&
  &/(SB*SW) + (0.09375D0*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.140625D0*CA12*CA3*EL*m12squ&
  &ared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.28125D0*CA22*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dM&
  &W2Alter())/(CB*SB2*SW) - (0.421875D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.06&
  &25D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.0625D0*CA1*CA22*EL*m12squared*SA3*DBLE(MW**INT(-&
  &3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.28125D0*CA1*EL*m12squared*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.281&
  &25D0*CA1*CA22*EL*m12squared*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.0625D0*CA1*EL*m12squared*SA22*SA3*DBLE&
  &(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.28125D0*CA1*EL*m12squared*SA12*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*S&
  &B2*SW) + (0.046875D0*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.140625D0*CA22*CA3*EL*m12squar&
  &ed*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.03125D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(&
  &SB*SW*TB) + (0.03125D0*CA1*CA22*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) - (0.03125D0*CA1*EL*m12squared*&
  &SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.046875D0*CA1*CA3*EL*m12squared*SA2*TB*DBLE(MW**INT(-3.D0))*dMW2Alte&
  &r())/(CB*SW) + (0.140625D0*CA1*CA22*CA3*EL*m12squared*SA2*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*EL*m12squa&
  &red*SA1*SA3*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA22*EL*m12squared*SA1*SA3*TB*DBLE(MW**INT(-3.D0))*dMW2A&
  &lter())/(CB*SW) + (0.03125D0*EL*m12squared*SA1*SA22*SA3*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA3*EL*MH12*&
  &SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA22*CA3*EL*MH12*SA2*DBLE(CA1**INT(3.D0))*DBLE&
  &(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.015625D0*CA3*EL*MH32*SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB&
  &*SW) - (0.046875D0*CA22*CA3*EL*MH32*SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.046875D0*CA3*EL*m1&
  &2squared*SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.140625D0*CA22*CA3*EL*m12squared*SA2*DBLE(&
  &CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.0625D0*EL*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0)&
  &)*dMW2Alter())/(SB*SW) + (0.0625D0*CA22*EL*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.03125D&
  &0*EL*MH32*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.03125D0*CA22*EL*MH32*SA3*DBLE(CA1**INT(3.D0)&
  &)*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.0625D0*EL*MH12*SA22*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter()&
  &)/(SB*SW) - (0.03125D0*EL*MH32*SA22*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*EL*m12squa&
  &red*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.09375D0*CA22*EL*m12squared*SA3*DBLE(CA1**INT(3&
  &.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.09375D0*EL*m12squared*SA22*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D&
  &0))*dMW2Alter())/(CB*SB2*SW) - (0.0625D0*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.0625D&
  &0*CA22*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*EL*MH32*SA3*DBLE(MW**INT(-3.D0)&
  &)*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA22*EL*MH32*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter(&
  &))/(CB*SW) + (0.0625D0*EL*MH12*SA22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*EL*MH32*SA&
  &22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA3*EL*MH12*SA2*DBLE(MW**INT(-3.D0))*DBLE(S&
  &A1**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*CA22*CA3*EL*MH12*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(&
  &SB*SW) - (0.015625D0*CA3*EL*MH32*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*CA22*CA3*EL*&
  &MH32*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*DB&
  &LE(SA1**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.09375D0*CA22*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2&
  &Alter())/(CB2*SB*SW) - (0.09375D0*EL*m12squared*SA22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) + &
  &(0.046875D0*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.140625D0*CA22*CA3*EL&
  &*m12squared*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.09375D0*CA1*CA3*EL*MH12*DBLE(MW**INT(-&
  &3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.046875D0*CA1*CA3*EL*MH32*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2A&
  &lter())/(CB*SW) + (0.140625D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.09375D&
  &0*CA1*CA3*EL*MH12*SA12*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.046875D0*CA1*CA3*EL*MH32*SA12*DBLE(&
  &MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*DBLE(SA2**I&
  &NT(3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB2*&
  &SB*SW) + (0.09375D0*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*CA12*CA3*EL*MH&
  &12*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.046875D0*CA3*EL*MH32*SA1*DBLE(MW**INT(-3.D0))*DBLE(&
  &SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*CA12*CA3*EL*MH32*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())&
  &/(SB*SW) + (0.140625D0*CA1*CA3*EL*m12squared*SA12*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.0937&
  &5D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.140625D0*CA12*CA3*EL*m12squa&
  &red*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.046875D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3&
  &.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW*TB) - (0.046875D0*CA1*CA3*EL*m12squared*TB*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3&
  &.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*CA3*EL*MH12*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter()&
  &)/(CB*SW) + (0.015625D0*CA3*EL*MH32*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.0&
  &46875D0*CA3*EL*m12squared*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.03125D0&
  &*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.015625D0*CA3*EL*MH32*DBL&
  &E(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*CA3*EL*m12squared*DBLE(MW**INT(&
  &-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + 0.5D0*CA2*CA3*MH12*DBLE(vS**INT(-2.D0))*dvSMSBarA&
  &lter() + 0.25D0*CA2*CA3*MH32*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() + 1.5D0*CA2*CA3*MH12*SA22*DBLE(vS**INT(-2.D0))*dvSMSBarAlter&
  &() + 0.75D0*CA2*CA3*MH32*SA22*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() - 0.5D0*CA3*MH12*DBLE(CA2**INT(3.D0))*DBLE(vS**INT(-2.D0))*&
  &dvSMSBarAlter() - 0.25D0*CA3*MH32*DBLE(CA2**INT(3.D0))*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() &
		& + CS1S1S1f111*dZH1H3OSAlter()/2D0 + CS1S1S1f211*dZH2H3OSAlter()/2D0 + CS1S1S1f311*dZH3H3OS()/2D0 &
		& + CS1S1S1f131*dZH1H1OS()/2D0 + CS1S1S1f231*dZH2H1OSAlter()/2D0 + CS1S1S1f331*dZH3H1OSAlter()/2D0 &
		& + CS1S1S1f131*dZH1H1OS()/2D0 + CS1S1S1f231*dZH2H1OSAlter()/2D0 + CS1S1S1f331*dZH3H1OSAlter()/2D0 &
		& )
	case (5)
		dRR11 = -CA2*SA1*dAlpha1PinchPStar() - CA1*SA2*dAlpha2PinchPStar()
		dRR12 = CA1*CA2*dAlpha1PinchPStar() - SA1*SA2*dAlpha2PinchPStar()
		dRR13 = CA2*dAlpha2PinchPStar()
		dRR21 = -CA1*CA3*dAlpha1PinchPStar() - CA1*CA3*SA2*dAlpha3PinchPStar() - CA1*CA2*SA3*dAlpha2PinchPStar() + &
			& SA1*SA3*dAlpha3PinchPStar() + SA1*SA2*SA3*dAlpha1PinchPStar()
		dRR22 = -CA3*SA1*dAlpha1PinchPStar() - CA3*SA1*SA2*dAlpha3PinchPStar() - CA1*SA3*dAlpha3PinchPStar() - &
			& CA2*SA1*SA3*dAlpha2PinchPStar() - CA1*SA2*SA3*dAlpha1PinchPStar()
		dRR23 = CA2*CA3*dAlpha3PinchPStar() - SA2*SA3*dAlpha2PinchPStar()
		dRR31 = -CA1*CA2*CA3*dAlpha2PinchPStar() + CA3*SA1*dAlpha3PinchPStar() + CA3*SA1*SA2*dAlpha1PinchPStar() + &
			& CA1*SA3*dAlpha1PinchPStar() + CA1*SA2*SA3*dAlpha3PinchPStar()
		dRR32 = -CA1*CA3*dAlpha3PinchPStar() - CA2*CA3*SA1*dAlpha2PinchPStar() - CA1*CA3*SA2*dAlpha1PinchPStar() + &
			& SA1*SA3*dAlpha1PinchPStar() + SA1*SA2*SA3*dAlpha3PinchPStar()
		dRR33 = -CA3*SA2*dAlpha2PinchPStar() - CA2*SA3*dAlpha3PinchPStar()

		totalAmplitude = CS1S1S1f311*( &
&(0.28125D0*CA1*CA3*EL*m12squared*SA2*dAlpha1PinchPStar())/(CB*MW*SW) + (0.84375D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha1PinchPSta&
  &r())/(CB*MW*SW) - (0.1875D0*CA3*EL*MH12*SA1*SA2*dAlpha1PinchPStar())/(CB*MW*SW) - (0.5625D0*CA12*CA3*EL*MH12*SA1*SA2*dAlpha1Pi&
  &nchPStar())/(CB*MW*SW) - (0.5625D0*CA22*CA3*EL*MH12*SA1*SA2*dAlpha1PinchPStar())/(CB*MW*SW) - (1.6875D0*CA12*CA22*CA3*EL*MH12*&
  &SA1*SA2*dAlpha1PinchPStar())/(CB*MW*SW) - (0.09375D0*CA3*EL*MH32*SA1*SA2*dAlpha1PinchPStar())/(CB*MW*SW) - (0.28125D0*CA12*CA3&
  &*EL*MH32*SA1*SA2*dAlpha1PinchPStar())/(CB*MW*SW) - (0.28125D0*CA22*CA3*EL*MH32*SA1*SA2*dAlpha1PinchPStar())/(CB*MW*SW) - (0.84&
  &375D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*dAlpha1PinchPStar())/(CB*MW*SW) - (0.125D0*CA1*EL*MH12*SA3*dAlpha1PinchPStar())/(CB*MW*SW)&
  & - (0.125D0*CA1*CA22*EL*MH12*SA3*dAlpha1PinchPStar())/(CB*MW*SW) - (0.0625D0*CA1*EL*MH32*SA3*dAlpha1PinchPStar())/(CB*MW*SW) -&
  & (0.0625D0*CA1*CA22*EL*MH32*SA3*dAlpha1PinchPStar())/(CB*MW*SW) - (0.1875D0*EL*m12squared*SA1*SA3*dAlpha1PinchPStar())/(CB*MW*&
  &SW) - (0.1875D0*CA22*EL*m12squared*SA1*SA3*dAlpha1PinchPStar())/(CB*MW*SW) + (1.125D0*CA1*EL*MH12*SA12*SA3*dAlpha1PinchPStar()&
  &)/(CB*MW*SW) + (1.125D0*CA1*CA22*EL*MH12*SA12*SA3*dAlpha1PinchPStar())/(CB*MW*SW) + (0.5625D0*CA1*EL*MH32*SA12*SA3*dAlpha1Pinc&
  &hPStar())/(CB*MW*SW) + (0.5625D0*CA1*CA22*EL*MH32*SA12*SA3*dAlpha1PinchPStar())/(CB*MW*SW) + (0.125D0*CA1*EL*MH12*SA22*SA3*dAl&
  &pha1PinchPStar())/(CB*MW*SW) + (0.0625D0*CA1*EL*MH32*SA22*SA3*dAlpha1PinchPStar())/(CB*MW*SW) + (0.1875D0*EL*m12squared*SA1*SA&
  &22*SA3*dAlpha1PinchPStar())/(CB*MW*SW) - (1.125D0*CA1*EL*MH12*SA12*SA22*SA3*dAlpha1PinchPStar())/(CB*MW*SW) - (0.5625D0*CA1*EL&
  &*MH32*SA12*SA22*SA3*dAlpha1PinchPStar())/(CB*MW*SW) + (0.1875D0*CA1*CA3*EL*MH12*SA2*dAlpha1PinchPStar())/(MW*SB*SW) + (0.5625D&
  &0*CA1*CA22*CA3*EL*MH12*SA2*dAlpha1PinchPStar())/(MW*SB*SW) + (0.09375D0*CA1*CA3*EL*MH32*SA2*dAlpha1PinchPStar())/(MW*SB*SW) + &
  &(0.28125D0*CA1*CA22*CA3*EL*MH32*SA2*dAlpha1PinchPStar())/(MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*SA1*SA2*dAlpha1PinchPStar()&
  &)/(MW*SB*SW) - (0.84375D0*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha1PinchPStar())/(MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA1*SA2&
  &*dAlpha1PinchPStar())/(CB2*MW*SB*SW) + (0.84375D0*CA12*CA3*EL*m12squared*SA1*SA2*dAlpha1PinchPStar())/(CB2*MW*SB*SW) + (0.5625&
  &D0*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha1PinchPStar())/(CB2*MW*SB*SW) + (2.53125D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha1&
  &PinchPStar())/(CB2*MW*SB*SW) + (0.5625D0*CA1*CA3*EL*MH12*SA12*SA2*dAlpha1PinchPStar())/(MW*SB*SW) + (1.6875D0*CA1*CA22*CA3*EL*&
  &MH12*SA12*SA2*dAlpha1PinchPStar())/(MW*SB*SW) + (0.28125D0*CA1*CA3*EL*MH32*SA12*SA2*dAlpha1PinchPStar())/(MW*SB*SW) + (0.84375&
  &D0*CA1*CA22*CA3*EL*MH32*SA12*SA2*dAlpha1PinchPStar())/(MW*SB*SW) - (0.1875D0*CA1*EL*m12squared*SA3*dAlpha1PinchPStar())/(MW*SB&
  &*SW) - (0.1875D0*CA1*CA22*EL*m12squared*SA3*dAlpha1PinchPStar())/(MW*SB*SW) + (0.125D0*CA1*EL*m12squared*SA3*dAlpha1PinchPStar&
  &())/(CB2*MW*SB*SW) + (0.125D0*CA1*CA22*EL*m12squared*SA3*dAlpha1PinchPStar())/(CB2*MW*SB*SW) - (0.125D0*EL*MH12*SA1*SA3*dAlpha&
  &1PinchPStar())/(MW*SB*SW) + (1.125D0*CA12*EL*MH12*SA1*SA3*dAlpha1PinchPStar())/(MW*SB*SW) - (0.125D0*CA22*EL*MH12*SA1*SA3*dAlp&
  &ha1PinchPStar())/(MW*SB*SW) + (1.125D0*CA12*CA22*EL*MH12*SA1*SA3*dAlpha1PinchPStar())/(MW*SB*SW) - (0.0625D0*EL*MH32*SA1*SA3*d&
  &Alpha1PinchPStar())/(MW*SB*SW) + (0.5625D0*CA12*EL*MH32*SA1*SA3*dAlpha1PinchPStar())/(MW*SB*SW) - (0.0625D0*CA22*EL*MH32*SA1*S&
  &A3*dAlpha1PinchPStar())/(MW*SB*SW) + (0.5625D0*CA12*CA22*EL*MH32*SA1*SA3*dAlpha1PinchPStar())/(MW*SB*SW) - (1.6875D0*CA1*EL*m1&
  &2squared*SA12*SA3*dAlpha1PinchPStar())/(CB2*MW*SB*SW) - (1.6875D0*CA1*CA22*EL*m12squared*SA12*SA3*dAlpha1PinchPStar())/(CB2*MW&
  &*SB*SW) + (0.1875D0*CA1*EL*m12squared*SA22*SA3*dAlpha1PinchPStar())/(MW*SB*SW) - (0.125D0*CA1*EL*m12squared*SA22*SA3*dAlpha1Pi&
  &nchPStar())/(CB2*MW*SB*SW) + (0.125D0*EL*MH12*SA1*SA22*SA3*dAlpha1PinchPStar())/(MW*SB*SW) - (1.125D0*CA12*EL*MH12*SA1*SA22*SA&
  &3*dAlpha1PinchPStar())/(MW*SB*SW) + (0.0625D0*EL*MH32*SA1*SA22*SA3*dAlpha1PinchPStar())/(MW*SB*SW) - (0.5625D0*CA12*EL*MH32*SA&
  &1*SA22*SA3*dAlpha1PinchPStar())/(MW*SB*SW) + (1.6875D0*CA1*EL*m12squared*SA12*SA22*SA3*dAlpha1PinchPStar())/(CB2*MW*SB*SW) - (&
  &0.1875D0*CA1*CA3*EL*m12squared*SA2*dAlpha1PinchPStar())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha1Pinch&
  &PStar())/(CB*MW*SB2*SW) - (0.84375D0*CA1*CA3*EL*m12squared*SA12*SA2*dAlpha1PinchPStar())/(CB*MW*SB2*SW) - (2.53125D0*CA1*CA22*&
  &CA3*EL*m12squared*SA12*SA2*dAlpha1PinchPStar())/(CB*MW*SB2*SW) + (0.125D0*EL*m12squared*SA1*SA3*dAlpha1PinchPStar())/(CB*MW*SB&
  &2*SW) - (1.6875D0*CA12*EL*m12squared*SA1*SA3*dAlpha1PinchPStar())/(CB*MW*SB2*SW) + (0.125D0*CA22*EL*m12squared*SA1*SA3*dAlpha1&
  &PinchPStar())/(CB*MW*SB2*SW) - (1.6875D0*CA12*CA22*EL*m12squared*SA1*SA3*dAlpha1PinchPStar())/(CB*MW*SB2*SW) - (0.125D0*EL*m12&
  &squared*SA1*SA22*SA3*dAlpha1PinchPStar())/(CB*MW*SB2*SW) + (1.6875D0*CA12*EL*m12squared*SA1*SA22*SA3*dAlpha1PinchPStar())/(CB*&
  &MW*SB2*SW) - (0.09375D0*CA1*CA3*EL*m12squared*SA2*dAlpha1PinchPStar())/(MW*SB*SW*TB) - (0.28125D0*CA1*CA22*CA3*EL*m12squared*S&
  &A2*dAlpha1PinchPStar())/(MW*SB*SW*TB) + (0.0625D0*EL*m12squared*SA1*SA3*dAlpha1PinchPStar())/(MW*SB*SW*TB) + (0.0625D0*CA22*EL&
  &*m12squared*SA1*SA3*dAlpha1PinchPStar())/(MW*SB*SW*TB) - (0.0625D0*EL*m12squared*SA1*SA22*SA3*dAlpha1PinchPStar())/(MW*SB*SW*T&
  &B) + (0.09375D0*CA3*EL*m12squared*SA1*SA2*TB*dAlpha1PinchPStar())/(CB*MW*SW) + (0.28125D0*CA22*CA3*EL*m12squared*SA1*SA2*TB*dA&
  &lpha1PinchPStar())/(CB*MW*SW) + (0.0625D0*CA1*EL*m12squared*SA3*TB*dAlpha1PinchPStar())/(CB*MW*SW) + (0.0625D0*CA1*CA22*EL*m12&
  &squared*SA3*TB*dAlpha1PinchPStar())/(CB*MW*SW) - (0.0625D0*CA1*EL*m12squared*SA22*SA3*TB*dAlpha1PinchPStar())/(CB*MW*SW) + (0.&
  &1875D0*CA1*CA2*CA3*EL*MH12*dAlpha2PinchPStar())/(CB*MW*SW) + (0.09375D0*CA1*CA2*CA3*EL*MH32*dAlpha2PinchPStar())/(CB*MW*SW) + &
  &(0.28125D0*CA2*CA3*EL*m12squared*SA1*dAlpha2PinchPStar())/(CB*MW*SW) - (0.1875D0*CA1*CA2*CA3*EL*MH12*SA12*dAlpha2PinchPStar())&
  &/(CB*MW*SW) - (0.09375D0*CA1*CA2*CA3*EL*MH32*SA12*dAlpha2PinchPStar())/(CB*MW*SW) - (1.6875D0*CA1*CA2*CA3*EL*MH12*SA22*dAlpha2&
  &PinchPStar())/(CB*MW*SW) - (0.84375D0*CA1*CA2*CA3*EL*MH32*SA22*dAlpha2PinchPStar())/(CB*MW*SW) - (2.53125D0*CA2*CA3*EL*m12squa&
  &red*SA1*SA22*dAlpha2PinchPStar())/(CB*MW*SW) + (1.6875D0*CA1*CA2*CA3*EL*MH12*SA12*SA22*dAlpha2PinchPStar())/(CB*MW*SW) + (0.84&
  &375D0*CA1*CA2*CA3*EL*MH32*SA12*SA22*dAlpha2PinchPStar())/(CB*MW*SW) - (0.75D0*CA1*CA2*EL*m12squared*SA2*SA3*dAlpha2PinchPStar(&
  &))/(CB*MW*SW) + (0.5D0*CA2*EL*MH12*SA1*SA2*SA3*dAlpha2PinchPStar())/(CB*MW*SW) + (1.5D0*CA12*CA2*EL*MH12*SA1*SA2*SA3*dAlpha2Pi&
  &nchPStar())/(CB*MW*SW) + (0.25D0*CA2*EL*MH32*SA1*SA2*SA3*dAlpha2PinchPStar())/(CB*MW*SW) + (0.75D0*CA12*CA2*EL*MH32*SA1*SA2*SA&
  &3*dAlpha2PinchPStar())/(CB*MW*SW) + (0.28125D0*CA1*CA2*CA3*EL*m12squared*dAlpha2PinchPStar())/(MW*SB*SW) - (0.1875D0*CA1*CA2*C&
  &A3*EL*m12squared*dAlpha2PinchPStar())/(CB2*MW*SB*SW) + (0.1875D0*CA2*CA3*EL*MH12*SA1*dAlpha2PinchPStar())/(MW*SB*SW) - (0.1875&
  &D0*CA12*CA2*CA3*EL*MH12*SA1*dAlpha2PinchPStar())/(MW*SB*SW) + (0.09375D0*CA2*CA3*EL*MH32*SA1*dAlpha2PinchPStar())/(MW*SB*SW) -&
  & (0.09375D0*CA12*CA2*CA3*EL*MH32*SA1*dAlpha2PinchPStar())/(MW*SB*SW) + (0.28125D0*CA1*CA2*CA3*EL*m12squared*SA12*dAlpha2PinchP&
  &Star())/(CB2*MW*SB*SW) - (2.53125D0*CA1*CA2*CA3*EL*m12squared*SA22*dAlpha2PinchPStar())/(MW*SB*SW) + (1.6875D0*CA1*CA2*CA3*EL*&
  &m12squared*SA22*dAlpha2PinchPStar())/(CB2*MW*SB*SW) - (1.6875D0*CA2*CA3*EL*MH12*SA1*SA22*dAlpha2PinchPStar())/(MW*SB*SW) + (1.&
  &6875D0*CA12*CA2*CA3*EL*MH12*SA1*SA22*dAlpha2PinchPStar())/(MW*SB*SW) - (0.84375D0*CA2*CA3*EL*MH32*SA1*SA22*dAlpha2PinchPStar()&
  &)/(MW*SB*SW) + (0.84375D0*CA12*CA2*CA3*EL*MH32*SA1*SA22*dAlpha2PinchPStar())/(MW*SB*SW) - (2.53125D0*CA1*CA2*CA3*EL*m12squared&
  &*SA12*SA22*dAlpha2PinchPStar())/(CB2*MW*SB*SW) - (0.5D0*CA1*CA2*EL*MH12*SA2*SA3*dAlpha2PinchPStar())/(MW*SB*SW) - (0.25D0*CA1*&
  &CA2*EL*MH32*SA2*SA3*dAlpha2PinchPStar())/(MW*SB*SW) + (0.75D0*CA2*EL*m12squared*SA1*SA2*SA3*dAlpha2PinchPStar())/(MW*SB*SW) - &
  &(0.5D0*CA2*EL*m12squared*SA1*SA2*SA3*dAlpha2PinchPStar())/(CB2*MW*SB*SW) - (2.25D0*CA12*CA2*EL*m12squared*SA1*SA2*SA3*dAlpha2P&
  &inchPStar())/(CB2*MW*SB*SW) - (1.5D0*CA1*CA2*EL*MH12*SA12*SA2*SA3*dAlpha2PinchPStar())/(MW*SB*SW) - (0.75D0*CA1*CA2*EL*MH32*SA&
  &12*SA2*SA3*dAlpha2PinchPStar())/(MW*SB*SW) - (0.1875D0*CA2*CA3*EL*m12squared*SA1*dAlpha2PinchPStar())/(CB*MW*SB2*SW) + (0.2812&
  &5D0*CA12*CA2*CA3*EL*m12squared*SA1*dAlpha2PinchPStar())/(CB*MW*SB2*SW) + (1.6875D0*CA2*CA3*EL*m12squared*SA1*SA22*dAlpha2Pinch&
  &PStar())/(CB*MW*SB2*SW) - (2.53125D0*CA12*CA2*CA3*EL*m12squared*SA1*SA22*dAlpha2PinchPStar())/(CB*MW*SB2*SW) + (0.5D0*CA1*CA2*&
  &EL*m12squared*SA2*SA3*dAlpha2PinchPStar())/(CB*MW*SB2*SW) + (2.25D0*CA1*CA2*EL*m12squared*SA12*SA2*SA3*dAlpha2PinchPStar())/(C&
  &B*MW*SB2*SW) - (0.09375D0*CA2*CA3*EL*m12squared*SA1*dAlpha2PinchPStar())/(MW*SB*SW*TB) + (0.84375D0*CA2*CA3*EL*m12squared*SA1*&
  &SA22*dAlpha2PinchPStar())/(MW*SB*SW*TB) + (0.25D0*CA1*CA2*EL*m12squared*SA2*SA3*dAlpha2PinchPStar())/(MW*SB*SW*TB) - (0.09375D&
  &0*CA1*CA2*CA3*EL*m12squared*TB*dAlpha2PinchPStar())/(CB*MW*SW) + (0.84375D0*CA1*CA2*CA3*EL*m12squared*SA22*TB*dAlpha2PinchPSta&
  &r())/(CB*MW*SW) - (0.25D0*CA2*EL*m12squared*SA1*SA2*SA3*TB*dAlpha2PinchPStar())/(CB*MW*SW) + (0.5D0*CA3*MH12*SA2*dAlpha2PinchP&
  &Star())/vS - (4.5D0*CA22*CA3*MH12*SA2*dAlpha2PinchPStar())/vS + (0.25D0*CA3*MH32*SA2*dAlpha2PinchPStar())/vS - (2.25D0*CA22*CA&
  &3*MH32*SA2*dAlpha2PinchPStar())/vS + (0.1875D0*CA1*CA3*EL*m12squared*dAlpha3PinchPStar())/(CB*MW*SW) + (0.1875D0*CA1*CA22*CA3*&
  &EL*m12squared*dAlpha3PinchPStar())/(CB*MW*SW) - (0.125D0*CA3*EL*MH12*SA1*dAlpha3PinchPStar())/(CB*MW*SW) - (0.375D0*CA12*CA3*E&
  &L*MH12*SA1*dAlpha3PinchPStar())/(CB*MW*SW) - (0.125D0*CA22*CA3*EL*MH12*SA1*dAlpha3PinchPStar())/(CB*MW*SW) - (0.375D0*CA12*CA2&
  &2*CA3*EL*MH12*SA1*dAlpha3PinchPStar())/(CB*MW*SW) - (0.0625D0*CA3*EL*MH32*SA1*dAlpha3PinchPStar())/(CB*MW*SW) - (0.1875D0*CA12&
  &*CA3*EL*MH32*SA1*dAlpha3PinchPStar())/(CB*MW*SW) - (0.0625D0*CA22*CA3*EL*MH32*SA1*dAlpha3PinchPStar())/(CB*MW*SW) - (0.1875D0*&
  &CA12*CA22*CA3*EL*MH32*SA1*dAlpha3PinchPStar())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*m12squared*SA22*dAlpha3PinchPStar())/(CB*MW*S&
  &W) + (0.125D0*CA3*EL*MH12*SA1*SA22*dAlpha3PinchPStar())/(CB*MW*SW) + (0.375D0*CA12*CA3*EL*MH12*SA1*SA22*dAlpha3PinchPStar())/(&
  &CB*MW*SW) + (0.0625D0*CA3*EL*MH32*SA1*SA22*dAlpha3PinchPStar())/(CB*MW*SW) + (0.1875D0*CA12*CA3*EL*MH32*SA1*SA22*dAlpha3PinchP&
  &Star())/(CB*MW*SW) - (0.1875D0*CA1*EL*MH12*SA2*SA3*dAlpha3PinchPStar())/(CB*MW*SW) - (0.5625D0*CA1*CA22*EL*MH12*SA2*SA3*dAlpha&
  &3PinchPStar())/(CB*MW*SW) - (0.09375D0*CA1*EL*MH32*SA2*SA3*dAlpha3PinchPStar())/(CB*MW*SW) - (0.28125D0*CA1*CA22*EL*MH32*SA2*S&
  &A3*dAlpha3PinchPStar())/(CB*MW*SW) - (0.28125D0*EL*m12squared*SA1*SA2*SA3*dAlpha3PinchPStar())/(CB*MW*SW) - (0.84375D0*CA22*EL&
  &*m12squared*SA1*SA2*SA3*dAlpha3PinchPStar())/(CB*MW*SW) + (0.1875D0*CA1*EL*MH12*SA12*SA2*SA3*dAlpha3PinchPStar())/(CB*MW*SW) +&
  & (0.5625D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dAlpha3PinchPStar())/(CB*MW*SW) + (0.09375D0*CA1*EL*MH32*SA12*SA2*SA3*dAlpha3PinchPSt&
  &ar())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*MH32*SA12*SA2*SA3*dAlpha3PinchPStar())/(CB*MW*SW) + (0.125D0*CA1*CA3*EL*MH12*dAlpha3&
  &PinchPStar())/(MW*SB*SW) + (0.125D0*CA1*CA22*CA3*EL*MH12*dAlpha3PinchPStar())/(MW*SB*SW) + (0.0625D0*CA1*CA3*EL*MH32*dAlpha3Pi&
  &nchPStar())/(MW*SB*SW) + (0.0625D0*CA1*CA22*CA3*EL*MH32*dAlpha3PinchPStar())/(MW*SB*SW) - (0.1875D0*CA3*EL*m12squared*SA1*dAlp&
  &ha3PinchPStar())/(MW*SB*SW) - (0.1875D0*CA22*CA3*EL*m12squared*SA1*dAlpha3PinchPStar())/(MW*SB*SW) + (0.125D0*CA3*EL*m12square&
  &d*SA1*dAlpha3PinchPStar())/(CB2*MW*SB*SW) + (0.5625D0*CA12*CA3*EL*m12squared*SA1*dAlpha3PinchPStar())/(CB2*MW*SB*SW) + (0.125D&
  &0*CA22*CA3*EL*m12squared*SA1*dAlpha3PinchPStar())/(CB2*MW*SB*SW) + (0.5625D0*CA12*CA22*CA3*EL*m12squared*SA1*dAlpha3PinchPStar&
  &())/(CB2*MW*SB*SW) + (0.375D0*CA1*CA3*EL*MH12*SA12*dAlpha3PinchPStar())/(MW*SB*SW) + (0.375D0*CA1*CA22*CA3*EL*MH12*SA12*dAlpha&
  &3PinchPStar())/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*MH32*SA12*dAlpha3PinchPStar())/(MW*SB*SW) + (0.1875D0*CA1*CA22*CA3*EL*MH32*SA&
  &12*dAlpha3PinchPStar())/(MW*SB*SW) - (0.125D0*CA1*CA3*EL*MH12*SA22*dAlpha3PinchPStar())/(MW*SB*SW) - (0.0625D0*CA1*CA3*EL*MH32&
  &*SA22*dAlpha3PinchPStar())/(MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA1*SA22*dAlpha3PinchPStar())/(MW*SB*SW) - (0.125D0*CA3*EL&
  &*m12squared*SA1*SA22*dAlpha3PinchPStar())/(CB2*MW*SB*SW) - (0.5625D0*CA12*CA3*EL*m12squared*SA1*SA22*dAlpha3PinchPStar())/(CB2&
  &*MW*SB*SW) - (0.375D0*CA1*CA3*EL*MH12*SA12*SA22*dAlpha3PinchPStar())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*MH32*SA12*SA22*dAlpha3P&
  &inchPStar())/(MW*SB*SW) - (0.28125D0*CA1*EL*m12squared*SA2*SA3*dAlpha3PinchPStar())/(MW*SB*SW) - (0.84375D0*CA1*CA22*EL*m12squ&
  &ared*SA2*SA3*dAlpha3PinchPStar())/(MW*SB*SW) + (0.1875D0*CA1*EL*m12squared*SA2*SA3*dAlpha3PinchPStar())/(CB2*MW*SB*SW) + (0.56&
  &25D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha3PinchPStar())/(CB2*MW*SB*SW) - (0.1875D0*EL*MH12*SA1*SA2*SA3*dAlpha3PinchPStar())/(&
  &MW*SB*SW) + (0.1875D0*CA12*EL*MH12*SA1*SA2*SA3*dAlpha3PinchPStar())/(MW*SB*SW) - (0.5625D0*CA22*EL*MH12*SA1*SA2*SA3*dAlpha3Pin&
  &chPStar())/(MW*SB*SW) + (0.5625D0*CA12*CA22*EL*MH12*SA1*SA2*SA3*dAlpha3PinchPStar())/(MW*SB*SW) - (0.09375D0*EL*MH32*SA1*SA2*S&
  &A3*dAlpha3PinchPStar())/(MW*SB*SW) + (0.09375D0*CA12*EL*MH32*SA1*SA2*SA3*dAlpha3PinchPStar())/(MW*SB*SW) - (0.28125D0*CA22*EL*&
  &MH32*SA1*SA2*SA3*dAlpha3PinchPStar())/(MW*SB*SW) + (0.28125D0*CA12*CA22*EL*MH32*SA1*SA2*SA3*dAlpha3PinchPStar())/(MW*SB*SW) - &
  &(0.28125D0*CA1*EL*m12squared*SA12*SA2*SA3*dAlpha3PinchPStar())/(CB2*MW*SB*SW) - (0.84375D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3&
  &*dAlpha3PinchPStar())/(CB2*MW*SB*SW) - (0.125D0*CA1*CA3*EL*m12squared*dAlpha3PinchPStar())/(CB*MW*SB2*SW) - (0.125D0*CA1*CA22*&
  &CA3*EL*m12squared*dAlpha3PinchPStar())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA3*EL*m12squared*SA12*dAlpha3PinchPStar())/(CB*MW*SB2*S&
  &W) - (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA12*dAlpha3PinchPStar())/(CB*MW*SB2*SW) + (0.125D0*CA1*CA3*EL*m12squared*SA22*dAlph&
  &a3PinchPStar())/(CB*MW*SB2*SW) + (0.5625D0*CA1*CA3*EL*m12squared*SA12*SA22*dAlpha3PinchPStar())/(CB*MW*SB2*SW) + (0.1875D0*EL*&
  &m12squared*SA1*SA2*SA3*dAlpha3PinchPStar())/(CB*MW*SB2*SW) - (0.28125D0*CA12*EL*m12squared*SA1*SA2*SA3*dAlpha3PinchPStar())/(C&
  &B*MW*SB2*SW) + (0.5625D0*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha3PinchPStar())/(CB*MW*SB2*SW) - (0.84375D0*CA12*CA22*EL*m12squar&
  &ed*SA1*SA2*SA3*dAlpha3PinchPStar())/(CB*MW*SB2*SW) - (0.0625D0*CA1*CA3*EL*m12squared*dAlpha3PinchPStar())/(MW*SB*SW*TB) - (0.0&
  &625D0*CA1*CA22*CA3*EL*m12squared*dAlpha3PinchPStar())/(MW*SB*SW*TB) + (0.0625D0*CA1*CA3*EL*m12squared*SA22*dAlpha3PinchPStar()&
  &)/(MW*SB*SW*TB) + (0.09375D0*EL*m12squared*SA1*SA2*SA3*dAlpha3PinchPStar())/(MW*SB*SW*TB) + (0.28125D0*CA22*EL*m12squared*SA1*&
  &SA2*SA3*dAlpha3PinchPStar())/(MW*SB*SW*TB) + (0.0625D0*CA3*EL*m12squared*SA1*TB*dAlpha3PinchPStar())/(CB*MW*SW) + (0.0625D0*CA&
  &22*CA3*EL*m12squared*SA1*TB*dAlpha3PinchPStar())/(CB*MW*SW) - (0.0625D0*CA3*EL*m12squared*SA1*SA22*TB*dAlpha3PinchPStar())/(CB&
  &*MW*SW) + (0.09375D0*CA1*EL*m12squared*SA2*SA3*TB*dAlpha3PinchPStar())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*m12squared*SA2*SA3*&
  &TB*dAlpha3PinchPStar())/(CB*MW*SW) + (0.5D0*CA2*MH12*SA3*dAlpha3PinchPStar())/vS + (0.25D0*CA2*MH32*SA3*dAlpha3PinchPStar())/v&
  &S + (1.5D0*CA2*MH12*SA22*SA3*dAlpha3PinchPStar())/vS + (0.75D0*CA2*MH32*SA22*SA3*dAlpha3PinchPStar())/vS + (0.09375D0*CA3*EL*M&
  &H12*SA1*SA2*dBeta1PinchPStar())/(CB*MW*SW) - (0.09375D0*CA12*CA3*EL*MH12*SA1*SA2*dBeta1PinchPStar())/(CB*MW*SW) + (0.28125D0*C&
  &A22*CA3*EL*MH12*SA1*SA2*dBeta1PinchPStar())/(CB*MW*SW) - (0.28125D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dBeta1PinchPStar())/(CB*MW*S&
  &W) + (0.046875D0*CA3*EL*MH32*SA1*SA2*dBeta1PinchPStar())/(CB*MW*SW) - (0.046875D0*CA12*CA3*EL*MH32*SA1*SA2*dBeta1PinchPStar())&
  &/(CB*MW*SW) + (0.140625D0*CA22*CA3*EL*MH32*SA1*SA2*dBeta1PinchPStar())/(CB*MW*SW) - (0.140625D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*&
  &dBeta1PinchPStar())/(CB*MW*SW) + (0.0625D0*CA1*EL*MH12*SA3*dBeta1PinchPStar())/(CB*MW*SW) + (0.0625D0*CA1*CA22*EL*MH12*SA3*dBe&
  &ta1PinchPStar())/(CB*MW*SW) + (0.03125D0*CA1*EL*MH32*SA3*dBeta1PinchPStar())/(CB*MW*SW) + (0.03125D0*CA1*CA22*EL*MH32*SA3*dBet&
  &a1PinchPStar())/(CB*MW*SW) + (0.1875D0*CA1*EL*MH12*SA12*SA3*dBeta1PinchPStar())/(CB*MW*SW) + (0.1875D0*CA1*CA22*EL*MH12*SA12*S&
  &A3*dBeta1PinchPStar())/(CB*MW*SW) + (0.09375D0*CA1*EL*MH32*SA12*SA3*dBeta1PinchPStar())/(CB*MW*SW) + (0.09375D0*CA1*CA22*EL*MH&
  &32*SA12*SA3*dBeta1PinchPStar())/(CB*MW*SW) - (0.0625D0*CA1*EL*MH12*SA22*SA3*dBeta1PinchPStar())/(CB*MW*SW) - (0.03125D0*CA1*EL&
  &*MH32*SA22*SA3*dBeta1PinchPStar())/(CB*MW*SW) - (0.1875D0*CA1*EL*MH12*SA12*SA22*SA3*dBeta1PinchPStar())/(CB*MW*SW) - (0.09375D&
  &0*CA1*EL*MH32*SA12*SA22*SA3*dBeta1PinchPStar())/(CB*MW*SW) + (0.09375D0*CA3*EL*m12squared*SA1*SA2*dBeta1PinchPStar())/(MW*SB*S&
  &W) + (0.28125D0*CA22*CA3*EL*m12squared*SA1*SA2*dBeta1PinchPStar())/(MW*SB*SW) - (0.328125D0*CA3*EL*m12squared*SA1*SA2*dBeta1Pi&
  &nchPStar())/(CB2*MW*SB*SW) + (0.421875D0*CA12*CA3*EL*m12squared*SA1*SA2*dBeta1PinchPStar())/(CB2*MW*SB*SW) - (0.984375D0*CA22*&
  &CA3*EL*m12squared*SA1*SA2*dBeta1PinchPStar())/(CB2*MW*SB*SW) + (1.265625D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*dBeta1PinchPSta&
  &r())/(CB2*MW*SB*SW) + (0.0625D0*CA1*EL*m12squared*SA3*dBeta1PinchPStar())/(MW*SB*SW) + (0.0625D0*CA1*CA22*EL*m12squared*SA3*dB&
  &eta1PinchPStar())/(MW*SB*SW) - (0.21875D0*CA1*EL*m12squared*SA3*dBeta1PinchPStar())/(CB2*MW*SB*SW) - (0.21875D0*CA1*CA22*EL*m1&
  &2squared*SA3*dBeta1PinchPStar())/(CB2*MW*SB*SW) - (0.84375D0*CA1*EL*m12squared*SA12*SA3*dBeta1PinchPStar())/(CB2*MW*SB*SW) - (&
  &0.84375D0*CA1*CA22*EL*m12squared*SA12*SA3*dBeta1PinchPStar())/(CB2*MW*SB*SW) - (0.0625D0*CA1*EL*m12squared*SA22*SA3*dBeta1Pinc&
  &hPStar())/(MW*SB*SW) + (0.21875D0*CA1*EL*m12squared*SA22*SA3*dBeta1PinchPStar())/(CB2*MW*SB*SW) + (0.84375D0*CA1*EL*m12squared&
  &*SA12*SA22*SA3*dBeta1PinchPStar())/(CB2*MW*SB*SW) + (0.09375D0*CA1*CA3*EL*m12squared*SA2*dBeta1PinchPStar())/(CB*MW*SB2*SW) + &
  &(0.28125D0*CA1*CA22*CA3*EL*m12squared*SA2*dBeta1PinchPStar())/(CB*MW*SB2*SW) - (0.09375D0*CA3*EL*MH12*SA1*SA2*dBeta1PinchPStar&
  &())/(CB*MW*SB2*SW) + (0.09375D0*CA12*CA3*EL*MH12*SA1*SA2*dBeta1PinchPStar())/(CB*MW*SB2*SW) - (0.28125D0*CA22*CA3*EL*MH12*SA1*&
  &SA2*dBeta1PinchPStar())/(CB*MW*SB2*SW) + (0.28125D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dBeta1PinchPStar())/(CB*MW*SB2*SW) - (0.0468&
  &75D0*CA3*EL*MH32*SA1*SA2*dBeta1PinchPStar())/(CB*MW*SB2*SW) + (0.046875D0*CA12*CA3*EL*MH32*SA1*SA2*dBeta1PinchPStar())/(CB*MW*&
  &SB2*SW) - (0.140625D0*CA22*CA3*EL*MH32*SA1*SA2*dBeta1PinchPStar())/(CB*MW*SB2*SW) + (0.140625D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*&
  &dBeta1PinchPStar())/(CB*MW*SB2*SW) - (0.28125D0*CA1*CA3*EL*m12squared*SA12*SA2*dBeta1PinchPStar())/(CB*MW*SB2*SW) - (0.84375D0&
  &*CA1*CA22*CA3*EL*m12squared*SA12*SA2*dBeta1PinchPStar())/(CB*MW*SB2*SW) - (0.0625D0*CA1*EL*MH12*SA3*dBeta1PinchPStar())/(CB*MW&
  &*SB2*SW) - (0.0625D0*CA1*CA22*EL*MH12*SA3*dBeta1PinchPStar())/(CB*MW*SB2*SW) - (0.03125D0*CA1*EL*MH32*SA3*dBeta1PinchPStar())/&
  &(CB*MW*SB2*SW) - (0.03125D0*CA1*CA22*EL*MH32*SA3*dBeta1PinchPStar())/(CB*MW*SB2*SW) - (0.0625D0*EL*m12squared*SA1*SA3*dBeta1Pi&
  &nchPStar())/(CB*MW*SB2*SW) - (0.5625D0*CA12*EL*m12squared*SA1*SA3*dBeta1PinchPStar())/(CB*MW*SB2*SW) - (0.0625D0*CA22*EL*m12sq&
  &uared*SA1*SA3*dBeta1PinchPStar())/(CB*MW*SB2*SW) - (0.5625D0*CA12*CA22*EL*m12squared*SA1*SA3*dBeta1PinchPStar())/(CB*MW*SB2*SW&
  &) - (0.1875D0*CA1*EL*MH12*SA12*SA3*dBeta1PinchPStar())/(CB*MW*SB2*SW) - (0.1875D0*CA1*CA22*EL*MH12*SA12*SA3*dBeta1PinchPStar()&
  &)/(CB*MW*SB2*SW) - (0.09375D0*CA1*EL*MH32*SA12*SA3*dBeta1PinchPStar())/(CB*MW*SB2*SW) - (0.09375D0*CA1*CA22*EL*MH32*SA12*SA3*d&
  &Beta1PinchPStar())/(CB*MW*SB2*SW) + (0.0625D0*CA1*EL*MH12*SA22*SA3*dBeta1PinchPStar())/(CB*MW*SB2*SW) + (0.03125D0*CA1*EL*MH32&
  &*SA22*SA3*dBeta1PinchPStar())/(CB*MW*SB2*SW) + (0.0625D0*EL*m12squared*SA1*SA22*SA3*dBeta1PinchPStar())/(CB*MW*SB2*SW) + (0.56&
  &25D0*CA12*EL*m12squared*SA1*SA22*SA3*dBeta1PinchPStar())/(CB*MW*SB2*SW) + (0.1875D0*CA1*EL*MH12*SA12*SA22*SA3*dBeta1PinchPStar&
  &())/(CB*MW*SB2*SW) + (0.09375D0*CA1*EL*MH32*SA12*SA22*SA3*dBeta1PinchPStar())/(CB*MW*SB2*SW) - (0.1875D0*CA1*CA3*EL*m12squared&
  &*SA2*dBeta1PinchPStar())/(MW*SB*SW*TB) - (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA2*dBeta1PinchPStar())/(MW*SB*SW*TB) - (0.09375&
  &D0*CA3*EL*MH12*SA1*SA2*dBeta1PinchPStar())/(MW*SB*SW*TB) + (0.09375D0*CA12*CA3*EL*MH12*SA1*SA2*dBeta1PinchPStar())/(MW*SB*SW*T&
  &B) - (0.28125D0*CA22*CA3*EL*MH12*SA1*SA2*dBeta1PinchPStar())/(MW*SB*SW*TB) + (0.28125D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dBeta1Pi&
  &nchPStar())/(MW*SB*SW*TB) - (0.046875D0*CA3*EL*MH32*SA1*SA2*dBeta1PinchPStar())/(MW*SB*SW*TB) + (0.046875D0*CA12*CA3*EL*MH32*S&
  &A1*SA2*dBeta1PinchPStar())/(MW*SB*SW*TB) - (0.140625D0*CA22*CA3*EL*MH32*SA1*SA2*dBeta1PinchPStar())/(MW*SB*SW*TB) + (0.140625D&
  &0*CA12*CA22*CA3*EL*MH32*SA1*SA2*dBeta1PinchPStar())/(MW*SB*SW*TB) - (0.0625D0*CA1*EL*MH12*SA3*dBeta1PinchPStar())/(MW*SB*SW*TB&
  &) - (0.0625D0*CA1*CA22*EL*MH12*SA3*dBeta1PinchPStar())/(MW*SB*SW*TB) - (0.03125D0*CA1*EL*MH32*SA3*dBeta1PinchPStar())/(MW*SB*S&
  &W*TB) - (0.03125D0*CA1*CA22*EL*MH32*SA3*dBeta1PinchPStar())/(MW*SB*SW*TB) + (0.125D0*EL*m12squared*SA1*SA3*dBeta1PinchPStar())&
  &/(MW*SB*SW*TB) + (0.125D0*CA22*EL*m12squared*SA1*SA3*dBeta1PinchPStar())/(MW*SB*SW*TB) - (0.1875D0*CA1*EL*MH12*SA12*SA3*dBeta1&
  &PinchPStar())/(MW*SB*SW*TB) - (0.1875D0*CA1*CA22*EL*MH12*SA12*SA3*dBeta1PinchPStar())/(MW*SB*SW*TB) - (0.09375D0*CA1*EL*MH32*S&
  &A12*SA3*dBeta1PinchPStar())/(MW*SB*SW*TB) - (0.09375D0*CA1*CA22*EL*MH32*SA12*SA3*dBeta1PinchPStar())/(MW*SB*SW*TB) + (0.0625D0&
  &*CA1*EL*MH12*SA22*SA3*dBeta1PinchPStar())/(MW*SB*SW*TB) + (0.03125D0*CA1*EL*MH32*SA22*SA3*dBeta1PinchPStar())/(MW*SB*SW*TB) - &
  &(0.125D0*EL*m12squared*SA1*SA22*SA3*dBeta1PinchPStar())/(MW*SB*SW*TB) + (0.1875D0*CA1*EL*MH12*SA12*SA22*SA3*dBeta1PinchPStar()&
  &)/(MW*SB*SW*TB) + (0.09375D0*CA1*EL*MH32*SA12*SA22*SA3*dBeta1PinchPStar())/(MW*SB*SW*TB) + (0.1875D0*CA1*CA3*EL*MH12*SA2*TB*dB&
  &eta1PinchPStar())/(CB*MW*SW) + (0.5625D0*CA1*CA22*CA3*EL*MH12*SA2*TB*dBeta1PinchPStar())/(CB*MW*SW) + (0.09375D0*CA1*CA3*EL*MH&
  &32*SA2*TB*dBeta1PinchPStar())/(CB*MW*SW) + (0.28125D0*CA1*CA22*CA3*EL*MH32*SA2*TB*dBeta1PinchPStar())/(CB*MW*SW) + (0.328125D0&
  &*CA3*EL*m12squared*SA1*SA2*TB*dBeta1PinchPStar())/(CB*MW*SW) + (0.984375D0*CA22*CA3*EL*m12squared*SA1*SA2*TB*dBeta1PinchPStar(&
  &))/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*MH12*SA12*SA2*TB*dBeta1PinchPStar())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*EL*MH12*SA12*SA2&
  &*TB*dBeta1PinchPStar())/(CB*MW*SW) - (0.09375D0*CA1*CA3*EL*MH32*SA12*SA2*TB*dBeta1PinchPStar())/(CB*MW*SW) - (0.28125D0*CA1*CA&
  &22*CA3*EL*MH32*SA12*SA2*TB*dBeta1PinchPStar())/(CB*MW*SW) + (0.21875D0*CA1*EL*m12squared*SA3*TB*dBeta1PinchPStar())/(CB*MW*SW)&
  & + (0.21875D0*CA1*CA22*EL*m12squared*SA3*TB*dBeta1PinchPStar())/(CB*MW*SW) - (0.125D0*EL*MH12*SA1*SA3*TB*dBeta1PinchPStar())/(&
  &CB*MW*SW) - (0.375D0*CA12*EL*MH12*SA1*SA3*TB*dBeta1PinchPStar())/(CB*MW*SW) - (0.125D0*CA22*EL*MH12*SA1*SA3*TB*dBeta1PinchPSta&
  &r())/(CB*MW*SW) - (0.375D0*CA12*CA22*EL*MH12*SA1*SA3*TB*dBeta1PinchPStar())/(CB*MW*SW) - (0.0625D0*EL*MH32*SA1*SA3*TB*dBeta1Pi&
  &nchPStar())/(CB*MW*SW) - (0.1875D0*CA12*EL*MH32*SA1*SA3*TB*dBeta1PinchPStar())/(CB*MW*SW) - (0.0625D0*CA22*EL*MH32*SA1*SA3*TB*&
  &dBeta1PinchPStar())/(CB*MW*SW) - (0.1875D0*CA12*CA22*EL*MH32*SA1*SA3*TB*dBeta1PinchPStar())/(CB*MW*SW) - (0.21875D0*CA1*EL*m12&
  &squared*SA22*SA3*TB*dBeta1PinchPStar())/(CB*MW*SW) + (0.125D0*EL*MH12*SA1*SA22*SA3*TB*dBeta1PinchPStar())/(CB*MW*SW) + (0.375D&
  &0*CA12*EL*MH12*SA1*SA22*SA3*TB*dBeta1PinchPStar())/(CB*MW*SW) + (0.0625D0*EL*MH32*SA1*SA22*SA3*TB*dBeta1PinchPStar())/(CB*MW*S&
  &W) + (0.1875D0*CA12*EL*MH32*SA1*SA22*SA3*TB*dBeta1PinchPStar())/(CB*MW*SW) + (0.140625D0*CA3*EL*m12squared*SA1*SA2*dBeta1Pinch&
  &PStar())/(MW*SB*SW*TB2) + (0.421875D0*CA22*CA3*EL*m12squared*SA1*SA2*dBeta1PinchPStar())/(MW*SB*SW*TB2) + (0.09375D0*CA1*EL*m1&
  &2squared*SA3*dBeta1PinchPStar())/(MW*SB*SW*TB2) + (0.09375D0*CA1*CA22*EL*m12squared*SA3*dBeta1PinchPStar())/(MW*SB*SW*TB2) - (&
  &0.09375D0*CA1*EL*m12squared*SA22*SA3*dBeta1PinchPStar())/(MW*SB*SW*TB2) - (0.1875D0*CA1*CA3*EL*m12squared*SA2*TB2*dBeta1PinchP&
  &Star())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA2*TB2*dBeta1PinchPStar())/(CB*MW*SW) + (0.125D0*EL*m12squared*SA1*&
  &SA3*TB2*dBeta1PinchPStar())/(CB*MW*SW) + (0.125D0*CA22*EL*m12squared*SA1*SA3*TB2*dBeta1PinchPStar())/(CB*MW*SW) - (0.125D0*EL*&
  &m12squared*SA1*SA22*SA3*TB2*dBeta1PinchPStar())/(CB*MW*SW) + (0.125D0*CA2*CA3*MH12*dBeta1PinchPStar())/(CB*SB*vS) + (0.0625D0*&
  &CA2*CA3*MH32*dBeta1PinchPStar())/(CB*SB*vS) + (0.375D0*CA2*CA3*MH12*SA22*dBeta1PinchPStar())/(CB*SB*vS) + (0.1875D0*CA2*CA3*MH&
  &32*SA22*dBeta1PinchPStar())/(CB*SB*vS) - (0.125D0*CA2*CA3*MH12*dBeta1PinchPStar())/(TB*vS) - (0.0625D0*CA2*CA3*MH32*dBeta1Pinc&
  &hPStar())/(TB*vS) - (0.375D0*CA2*CA3*MH12*SA22*dBeta1PinchPStar())/(TB*vS) - (0.1875D0*CA2*CA3*MH32*SA22*dBeta1PinchPStar())/(&
  &TB*vS) - (0.125D0*CA2*CA3*MH12*TB*dBeta1PinchPStar())/vS - (0.0625D0*CA2*CA3*MH32*TB*dBeta1PinchPStar())/vS - (0.375D0*CA2*CA3&
  &*MH12*SA22*TB*dBeta1PinchPStar())/vS - (0.1875D0*CA2*CA3*MH32*SA22*TB*dBeta1PinchPStar())/vS - (0.375D0*EL*MH12*SA3*dAlpha1Pin&
  &chPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.375D0*CA22*EL*MH12*SA3*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - &
  &(0.1875D0*EL*MH32*SA3*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA22*EL*MH32*SA3*dAlpha1PinchPStar()*DB&
  &LE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.375D0*EL*MH12*SA22*SA3*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*EL&
  &*MH32*SA22*SA3*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA3*EL*MH12*SA2*dAlpha1PinchPStar()*DBLE(CA1**&
  &INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA22*CA3*EL*MH12*SA2*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA3*&
  &EL*MH32*SA2*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA22*CA3*EL*MH32*SA2*dAlpha1PinchPStar()*DBLE(CA&
  &1**INT(3.D0)))/(MW*SB*SW) + (0.5625D0*EL*m12squared*SA3*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.5625D0*C&
  &A22*EL*m12squared*SA3*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*EL*m12squared*SA22*SA3*dAlpha1Pinch&
  &PStar()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.28125D0*CA3*EL*m12squared*SA2*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*&
  &MW*SB2*SW) + (0.84375D0*CA22*CA3*EL*m12squared*SA2*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*CA2*CA&
  &3*EL*MH12*dAlpha2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA2*CA3*EL*MH32*dAlpha2PinchPStar()*DBLE(CA1**INT(&
  &3.D0)))/(CB*MW*SW) - (0.5625D0*CA2*CA3*EL*MH12*SA22*dAlpha2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA2*CA3*&
  &EL*MH32*SA22*dAlpha2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA2*CA3*EL*m12squared*dAlpha2PinchPStar()*DBLE(&
  &CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.84375D0*CA2*CA3*EL*m12squared*SA22*dAlpha2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*S&
  &W) + (0.5D0*CA2*EL*MH12*SA2*SA3*dAlpha2PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.25D0*CA2*EL*MH32*SA2*SA3*dAlpha2Pinc&
  &hPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.75D0*CA2*EL*m12squared*SA2*SA3*dAlpha2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW&
  &*SB2*SW) - (0.0625D0*EL*MH12*SA2*SA3*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA22*EL*MH12*SA2*SA3*dAl&
  &pha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*EL*MH32*SA2*SA3*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*M&
  &W*SW) - (0.09375D0*CA22*EL*MH32*SA2*SA3*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.125D0*CA3*EL*MH12*dAlpha3Pin&
  &chPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.125D0*CA22*CA3*EL*MH12*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - &
  &(0.0625D0*CA3*EL*MH32*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.0625D0*CA22*CA3*EL*MH32*dAlpha3PinchPStar()*DB&
  &LE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.125D0*CA3*EL*MH12*SA22*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*CA&
  &3*EL*MH32*SA22*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*EL*m12squared*SA2*SA3*dAlpha3PinchPStar()*DBL&
  &E(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.28125D0*CA22*EL*m12squared*SA2*SA3*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB&
  &*SW) + (0.1875D0*CA3*EL*m12squared*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*CA22*CA3*EL*m12squared&
  &*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.1875D0*CA3*EL*m12squared*SA22*dAlpha3PinchPStar()*DBLE(CA1**INT&
  &(3.D0)))/(CB*MW*SB2*SW) - (0.0625D0*EL*MH12*SA3*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.0625D0*CA22*EL*MH12*S&
  &A3*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*EL*MH32*SA3*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*M&
  &W*SW) - (0.03125D0*CA22*EL*MH32*SA3*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*EL*MH12*SA22*SA3*dBeta1Pin&
  &chPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*EL*MH32*SA22*SA3*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) +&
  & (0.28125D0*EL*m12squared*SA3*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.28125D0*CA22*EL*m12squared*SA3*dBet&
  &a1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*EL*m12squared*SA22*SA3*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0)&
  &))/(CB2*MW*SB*SW) + (0.09375D0*CA3*EL*m12squared*SA2*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA22&
  &*CA3*EL*m12squared*SA2*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*EL*MH12*SA3*dBeta1PinchPStar()*DBLE&
  &(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*CA22*EL*MH12*SA3*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.031&
  &25D0*EL*MH32*SA3*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.03125D0*CA22*EL*MH32*SA3*dBeta1PinchPStar()*DBLE&
  &(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.0625D0*EL*MH12*SA22*SA3*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.031&
  &25D0*EL*MH32*SA22*SA3*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*EL*MH12*SA3*dBeta1PinchPStar()*DBLE(&
  &CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.0625D0*CA22*EL*MH12*SA3*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.03125D&
  &0*EL*MH32*SA3*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.03125D0*CA22*EL*MH32*SA3*dBeta1PinchPStar()*DBLE(CA1&
  &**INT(3.D0)))/(MW*SB*SW*TB) - (0.0625D0*EL*MH12*SA22*SA3*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.03125D0*E&
  &L*MH32*SA22*SA3*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.0625D0*CA3*EL*MH12*SA2*TB*dBeta1PinchPStar()*DBLE(&
  &CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA22*CA3*EL*MH12*SA2*TB*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125&
  &D0*CA3*EL*MH32*SA2*TB*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA22*CA3*EL*MH32*SA2*TB*dBeta1PinchPSta&
  &r()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA1*CA3*EL*MH12*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.281&
  &25D0*CA1*CA3*EL*MH32*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.84375D0*CA3*EL*m12squared*SA1*dAlpha2PinchPStar&
  &()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.5625D0*CA1*CA3*EL*MH12*SA12*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0&
  &.28125D0*CA1*CA3*EL*MH32*SA12*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.84375D0*CA1*CA3*EL*m12squared*dAlpha2P&
  &inchPStar()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA1*CA3*EL*m12squared*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(CB2*&
  &MW*SB*SW) + (0.5625D0*CA3*EL*MH12*SA1*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA12*CA3*EL*MH12*SA1*dA&
  &lpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*CA3*EL*MH32*SA1*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(MW*&
  &SB*SW) - (0.28125D0*CA12*CA3*EL*MH32*SA1*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) + (0.84375D0*CA1*CA3*EL*m12squar&
  &ed*SA12*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA3*EL*m12squared*SA1*dAlpha2PinchPStar()*DBLE(CA&
  &2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA3*EL*m12squared*SA1*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(CB*MW*SB2*SW)&
  & - (0.28125D0*CA3*EL*m12squared*SA1*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.28125D0*CA1*CA3*EL*m12squared&
  &*TB*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.5D0*MH12*SA3*dAlpha3PinchPStar()*DBLE(CA2**INT(3.D0)))/vS - (0.2&
  &5D0*MH32*SA3*dAlpha3PinchPStar()*DBLE(CA2**INT(3.D0)))/vS - (0.125D0*CA3*MH12*dBeta1PinchPStar()*DBLE(CA2**INT(3.D0)))/(CB*SB*&
  &vS) - (0.0625D0*CA3*MH32*dBeta1PinchPStar()*DBLE(CA2**INT(3.D0)))/(CB*SB*vS) + (0.125D0*CA3*MH12*dBeta1PinchPStar()*DBLE(CA2**&
  &INT(3.D0)))/(TB*vS) + (0.0625D0*CA3*MH32*dBeta1PinchPStar()*DBLE(CA2**INT(3.D0)))/(TB*vS) + (0.125D0*CA3*MH12*TB*dBeta1PinchPS&
  &tar()*DBLE(CA2**INT(3.D0)))/vS + (0.0625D0*CA3*MH32*TB*dBeta1PinchPStar()*DBLE(CA2**INT(3.D0)))/vS + (0.1875D0*CA3*EL*MH12*dAl&
  &pha2PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA3*EL*MH32*dAlpha2PinchPStar()*DBLE(CA1**&
  &INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA3*EL*m12squared*dAlpha2PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(CA2**&
  &INT(3.D0)))/(CB2*MW*SB*SW) - (0.375D0*CA1*CA3*EL*m12squared*SA2*dBeta1PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (1.125D0*CA&
  &1*CA22*CA3*EL*m12squared*SA2*dBeta1PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.5625D0*CA1*CA3*EL*m12squared*SA12*SA2*dBeta&
  &1PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (1.6875D0*CA1*CA22*CA3*EL*m12squared*SA12*SA2*dBeta1PinchPStar()*DBLE(CB**INT(-3&
  &.D0)))/(MW*SW) + (0.25D0*EL*m12squared*SA1*SA3*dBeta1PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (1.125D0*CA12*EL*m12squared*&
  &SA1*SA3*dBeta1PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.25D0*CA22*EL*m12squared*SA1*SA3*dBeta1PinchPStar()*DBLE(CB**INT(&
  &-3.D0)))/(MW*SW) + (1.125D0*CA12*CA22*EL*m12squared*SA1*SA3*dBeta1PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.25D0*EL*m12s&
  &quared*SA1*SA22*SA3*dBeta1PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (1.125D0*CA12*EL*m12squared*SA1*SA22*SA3*dBeta1PinchPSt&
  &ar()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.1875D0*CA3*EL*m12squared*SA2*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D&
  &0)))/(MW*SW) - (0.5625D0*CA22*CA3*EL*m12squared*SA2*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0&
  &.1875D0*CA3*EL*MH12*SA2*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA22*CA3*EL*MH12*SA2*dAlpha1PinchPSta&
  &r()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA3*EL*MH32*SA2*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.28&
  &125D0*CA22*CA3*EL*MH32*SA2*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA3*EL*m12squared*SA2*dAlpha1Pinc&
  &hPStar()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.84375D0*CA22*CA3*EL*m12squared*SA2*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0))&
  &)/(CB2*MW*SB*SW) - (0.375D0*EL*MH12*SA3*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.375D0*CA22*EL*MH12*SA3*dAlph&
  &a1PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*EL*MH32*SA3*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) -&
  & (0.1875D0*CA22*EL*MH32*SA3*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.375D0*EL*MH12*SA22*SA3*dAlpha1PinchPStar&
  &()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*EL*MH32*SA22*SA3*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.562&
  &5D0*EL*m12squared*SA3*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.5625D0*CA22*EL*m12squared*SA3*dAlpha1Pinch&
  &PStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.5625D0*EL*m12squared*SA22*SA3*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*&
  &MW*SB2*SW) - (0.5D0*CA2*EL*MH12*SA2*SA3*dAlpha2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.25D0*CA2*EL*MH32*SA2*SA3*dAl&
  &pha2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA2*CA3*EL*MH12*dAlpha2PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB&
  &*SW) + (0.03125D0*CA2*CA3*EL*MH32*dAlpha2PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA2*CA3*EL*MH12*SA22*dAlpha&
  &2PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA2*CA3*EL*MH32*SA22*dAlpha2PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW&
  &*SB*SW) + (0.75D0*CA2*EL*m12squared*SA2*SA3*dAlpha2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA2*CA3*EL*m&
  &12squared*dAlpha2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA2*CA3*EL*m12squared*SA22*dAlpha2PinchPStar()&
  &*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.125D0*CA3*EL*MH12*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.125D0*C&
  &A22*CA3*EL*MH12*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA3*EL*MH32*dAlpha3PinchPStar()*DBLE(SA1**INT&
  &(3.D0)))/(CB*MW*SW) + (0.0625D0*CA22*CA3*EL*MH32*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.125D0*CA3*EL*MH12*S&
  &A22*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.0625D0*CA3*EL*MH32*SA22*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0))&
  &)/(CB*MW*SW) - (0.1875D0*CA3*EL*m12squared*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*CA22*CA3*EL*m1&
  &2squared*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA22*dAlpha3PinchPStar()*DBLE(&
  &SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D0*EL*MH12*SA2*SA3*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*C&
  &A22*EL*MH12*SA2*SA3*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.03125D0*EL*MH32*SA2*SA3*dAlpha3PinchPStar()*DBLE&
  &(SA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA22*EL*MH32*SA2*SA3*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.09375&
  &D0*EL*m12squared*SA2*SA3*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA22*EL*m12squared*SA2*SA3*dAlp&
  &ha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.03125D0*CA3*EL*MH12*SA2*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB&
  &*MW*SW) + (0.09375D0*CA22*CA3*EL*MH12*SA2*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.015625D0*CA3*EL*MH32*SA2*dB&
  &eta1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.046875D0*CA22*CA3*EL*MH32*SA2*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0)))/&
  &(CB*MW*SW) - (0.140625D0*CA3*EL*m12squared*SA2*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.421875D0*CA22*CA3*&
  &EL*m12squared*SA2*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.03125D0*CA3*EL*MH12*SA2*dBeta1PinchPStar()*DBLE&
  &(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.09375D0*CA22*CA3*EL*MH12*SA2*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (&
  &0.015625D0*CA3*EL*MH32*SA2*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.046875D0*CA22*CA3*EL*MH32*SA2*dBeta1Pi&
  &nchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*EL*m12squared*SA3*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*&
  &SB2*SW) + (0.1875D0*CA22*EL*m12squared*SA3*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.1875D0*EL*m12squared*S&
  &A22*SA3*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.03125D0*CA3*EL*MH12*SA2*dBeta1PinchPStar()*DBLE(SA1**INT(&
  &3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA22*CA3*EL*MH12*SA2*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.015625D0*C&
  &A3*EL*MH32*SA2*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.046875D0*CA22*CA3*EL*MH32*SA2*dBeta1PinchPStar()*DB&
  &LE(SA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.125D0*EL*MH12*SA3*TB*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.125D0*CA2&
  &2*EL*MH12*SA3*TB*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*EL*MH32*SA3*TB*dBeta1PinchPStar()*DBLE(SA1**I&
  &NT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA22*EL*MH32*SA3*TB*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.125D0*EL*MH12*S&
  &A22*SA3*TB*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.0625D0*EL*MH32*SA22*SA3*TB*dBeta1PinchPStar()*DBLE(SA1**IN&
  &T(3.D0)))/(CB*MW*SW) + (0.1875D0*CA3*EL*MH12*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.09&
  &375D0*CA3*EL*MH32*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*dAl&
  &pha2PinchPStar()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.375D0*EL*m12squared*SA3*dBeta1PinchPStar()*DBL&
  &E(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) - (0.375D0*CA22*EL*m12squared*SA3*dBeta1PinchPStar()*DBLE(CB**INT(-3.D0))*DBLE&
  &(SA1**INT(3.D0)))/(MW*SW) + (0.375D0*EL*m12squared*SA22*SA3*dBeta1PinchPStar()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*&
  &SW) - (0.28125D0*CA1*CA3*EL*m12squared*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA3*EL*MH12*SA1*dAlpha&
  &1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA12*CA3*EL*MH12*SA1*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*&
  &MW*SW) + (0.09375D0*CA3*EL*MH32*SA1*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA12*CA3*EL*MH32*SA1*dAl&
  &pha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*MH12*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB&
  &*SW) - (0.09375D0*CA1*CA3*EL*MH32*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*CA3*EL*m12squared*SA1*dAlp&
  &ha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA3*EL*m12squared*SA1*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(&
  &CB2*MW*SB*SW) - (0.84375D0*CA12*CA3*EL*m12squared*SA1*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA1&
  &*CA3*EL*MH12*SA12*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA1*CA3*EL*MH32*SA12*dAlpha1PinchPStar()*D&
  &BLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*m12squared*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (&
  &0.84375D0*CA1*CA3*EL*m12squared*SA12*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.09375D0*CA1*CA3*EL*m12squar&
  &ed*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA3*EL*m12squared*SA1*TB*dAlpha1PinchPStar()*DBLE(SA2*&
  &*INT(3.D0)))/(CB*MW*SW) + (1.5D0*CA3*MH12*dAlpha2PinchPStar()*DBLE(SA2**INT(3.D0)))/vS + (0.75D0*CA3*MH32*dAlpha2PinchPStar()*&
  &DBLE(SA2**INT(3.D0)))/vS + (0.1875D0*CA1*EL*MH12*SA3*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1*EL*&
  &MH32*SA3*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*EL*m12squared*SA1*SA3*dAlpha3PinchPStar()*DBLE(SA2*&
  &*INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA1*EL*MH12*SA12*SA3*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1&
  &*EL*MH32*SA12*SA3*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA1*EL*m12squared*SA3*dAlpha3PinchPStar()*&
  &DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA1*EL*m12squared*SA3*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + &
  &(0.1875D0*EL*MH12*SA1*SA3*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA12*EL*MH12*SA1*SA3*dAlpha3PinchPS&
  &tar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*EL*MH32*SA1*SA3*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.&
  &09375D0*CA12*EL*MH32*SA1*SA3*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*CA1*EL*m12squared*SA12*SA3*dAlp&
  &ha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*EL*m12squared*SA1*SA3*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)&
  &))/(CB*MW*SB2*SW) + (0.28125D0*CA12*EL*m12squared*SA1*SA3*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.09375D&
  &0*EL*m12squared*SA1*SA3*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA1*EL*m12squared*SA3*TB*dAlpha3P&
  &inchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA3*EL*MH12*SA1*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) &
  &+ (0.09375D0*CA12*CA3*EL*MH12*SA1*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.046875D0*CA3*EL*MH32*SA1*dBeta1Pinc&
  &hPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.046875D0*CA12*CA3*EL*MH32*SA1*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*S&
  &W) - (0.09375D0*CA3*EL*m12squared*SA1*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.328125D0*CA3*EL*m12squared*SA1*&
  &dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.421875D0*CA12*CA3*EL*m12squared*SA1*dBeta1PinchPStar()*DBLE(SA2**&
  &INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA1*CA3*EL*m12squared*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.093&
  &75D0*CA3*EL*MH12*SA1*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.09375D0*CA12*CA3*EL*MH12*SA1*dBeta1PinchPSta&
  &r()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.046875D0*CA3*EL*MH32*SA1*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW)&
  & - (0.046875D0*CA12*CA3*EL*MH32*SA1*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA1*CA3*EL*m12squared&
  &*SA12*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*CA1*CA3*EL*m12squared*dBeta1PinchPStar()*DBLE(SA2**I&
  &NT(3.D0)))/(MW*SB*SW*TB) + (0.09375D0*CA3*EL*MH12*SA1*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA12&
  &*CA3*EL*MH12*SA1*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.046875D0*CA3*EL*MH32*SA1*dBeta1PinchPStar()*DBLE(&
  &SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.046875D0*CA12*CA3*EL*MH32*SA1*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.&
  &1875D0*CA1*CA3*EL*MH12*TB*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1*CA3*EL*MH32*TB*dBeta1PinchPStar&
  &()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.328125D0*CA3*EL*m12squared*SA1*TB*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW)&
  & + (0.1875D0*CA1*CA3*EL*MH12*SA12*TB*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1*CA3*EL*MH32*SA12*TB*&
  &dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.140625D0*CA3*EL*m12squared*SA1*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)&
  &))/(MW*SB*SW*TB2) + (0.1875D0*CA1*CA3*EL*m12squared*TB2*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA3*EL&
  &*MH12*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA3*EL*MH32*dAlpha1PinchPStar()*D&
  &BLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0))*D&
  &BLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*EL*MH12*SA3*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB&
  &*MW*SW) + (0.03125D0*EL*MH32*SA3*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*EL*m12&
  &squared*SA3*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA3*EL*m12squared*dBeta&
  &1PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.0625D0*CA3*EL*MH12*TB*dBeta1PinchPStar()*DBLE(CA1&
  &**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*CA3*EL*MH32*TB*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SA2**IN&
  &T(3.D0)))/(CB*MW*SW) + (0.375D0*CA1*CA3*EL*m12squared*dBeta1PinchPStar()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) - &
  &(0.5625D0*CA1*CA3*EL*m12squared*SA12*dBeta1PinchPStar()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) + (0.1875D0*CA3*EL*&
  &m12squared*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) - (0.1875D0*CA3*EL*MH12*&
  &dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA3*EL*MH32*dAlpha1PinchPStar()*DBLE(SA&
  &1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA3*EL*m12squared*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SA&
  &2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.0625D0*EL*MH12*SA3*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW&
  &) + (0.03125D0*EL*MH32*SA3*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*EL*m12square&
  &d*SA3*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.03125D0*CA3*EL*MH12*dBeta1PinchPStar(&
  &)*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.015625D0*CA3*EL*MH32*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE&
  &(SA2**INT(3.D0)))/(CB*MW*SW) + (0.140625D0*CA3*EL*m12squared*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB&
  &2*MW*SB*SW) + (0.03125D0*CA3*EL*MH12*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.015625D&
  &0*CA3*EL*MH32*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.03125D0*CA3*EL*MH12*dBeta1Pinc&
  &hPStar()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.015625D0*CA3*EL*MH32*dBeta1PinchPStar()*DBLE(SA1**INT(3&
  &.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.328125D0*CA3*EL*m12squared*SA1*SA2*dBeta1PinchPStar()*DBLE(SB**INT(-3.D0)))/(MW&
  &*SW) - (0.421875D0*CA12*CA3*EL*m12squared*SA1*SA2*dBeta1PinchPStar()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.984375D0*CA22*CA3*EL*m&
  &12squared*SA1*SA2*dBeta1PinchPStar()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (1.265625D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*dBeta1Pin&
  &chPStar()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.09375D0*CA3*EL*m12squared*SA1*SA2*dBeta1PinchPStar()*DBLE(SB**INT(-3.D0)))/(CB2*M&
  &W*SW) - (0.140625D0*CA12*CA3*EL*m12squared*SA1*SA2*dBeta1PinchPStar()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.28125D0*CA22*CA3*&
  &EL*m12squared*SA1*SA2*dBeta1PinchPStar()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.421875D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*d&
  &Beta1PinchPStar()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.21875D0*CA1*EL*m12squared*SA3*dBeta1PinchPStar()*DBLE(SB**INT(-3.D0))&
  &)/(MW*SW) + (0.21875D0*CA1*CA22*EL*m12squared*SA3*dBeta1PinchPStar()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.0625D0*CA1*EL*m12squar&
  &ed*SA3*dBeta1PinchPStar()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.0625D0*CA1*CA22*EL*m12squared*SA3*dBeta1PinchPStar()*DBLE(SB*&
  &*INT(-3.D0)))/(CB2*MW*SW) + (0.84375D0*CA1*EL*m12squared*SA12*SA3*dBeta1PinchPStar()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.84375D&
  &0*CA1*CA22*EL*m12squared*SA12*SA3*dBeta1PinchPStar()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.28125D0*CA1*EL*m12squared*SA12*SA3*dBe&
  &ta1PinchPStar()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.28125D0*CA1*CA22*EL*m12squared*SA12*SA3*dBeta1PinchPStar()*DBLE(SB**INT&
  &(-3.D0)))/(CB2*MW*SW) - (0.21875D0*CA1*EL*m12squared*SA22*SA3*dBeta1PinchPStar()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.0625D0*CA1&
  &*EL*m12squared*SA22*SA3*dBeta1PinchPStar()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.84375D0*CA1*EL*m12squared*SA12*SA22*SA3*dBet&
  &a1PinchPStar()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.28125D0*CA1*EL*m12squared*SA12*SA22*SA3*dBeta1PinchPStar()*DBLE(SB**INT(-3.D&
  &0)))/(CB2*MW*SW) - (0.28125D0*EL*m12squared*SA3*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.281&
  &25D0*CA22*EL*m12squared*SA3*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.09375D0*EL*m12squared*S&
  &A3*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.09375D0*CA22*EL*m12squared*SA3*dBeta1PinchPS&
  &tar()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.28125D0*EL*m12squared*SA22*SA3*dBeta1PinchPStar()*DBLE(CA1**&
  &INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.09375D0*EL*m12squared*SA22*SA3*dBeta1PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SB**&
  &INT(-3.D0)))/(CB2*MW*SW) + (0.140625D0*CA3*EL*m12squared*SA2*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW&
  &*SW) + (0.421875D0*CA22*CA3*EL*m12squared*SA2*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.04687&
  &5D0*CA3*EL*m12squared*SA2*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.140625D0*CA22*CA3*EL*&
  &m12squared*SA2*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.328125D0*CA3*EL*m12squared*SA1*d&
  &Beta1PinchPStar()*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.421875D0*CA12*CA3*EL*m12squared*SA1*dBeta1PinchPStar&
  &()*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.09375D0*CA3*EL*m12squared*SA1*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0&
  &))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.140625D0*CA12*CA3*EL*m12squared*SA1*dBeta1PinchPStar()*DBLE(SA2**INT(3.D0))*DBLE(SB*&
  &*INT(-3.D0)))/(CB2*MW*SW) - (0.140625D0*CA3*EL*m12squared*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*DBLE(SB&
  &**INT(-3.D0)))/(MW*SW) - (0.046875D0*CA3*EL*m12squared*dBeta1PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*DBLE(SB**I&
  &NT(-3.D0)))/(CB2*MW*SW) + (0.1875D0*CA1*CA3*MH12*SA2*dgAtMZ())/(CB*MW) + (0.5625D0*CA1*CA22*CA3*MH12*SA2*dgAtMZ())/(CB*MW) + (&
  &0.09375D0*CA1*CA3*MH32*SA2*dgAtMZ())/(CB*MW) + (0.28125D0*CA1*CA22*CA3*MH32*SA2*dgAtMZ())/(CB*MW) + (0.28125D0*CA3*m12squared*&
  &SA1*SA2*dgAtMZ())/(CB*MW) + (0.84375D0*CA22*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*MW) - (0.1875D0*CA1*CA3*MH12*SA12*SA2*dgAtMZ(&
  &))/(CB*MW) - (0.5625D0*CA1*CA22*CA3*MH12*SA12*SA2*dgAtMZ())/(CB*MW) - (0.09375D0*CA1*CA3*MH32*SA12*SA2*dgAtMZ())/(CB*MW) - (0.&
  &28125D0*CA1*CA22*CA3*MH32*SA12*SA2*dgAtMZ())/(CB*MW) + (0.1875D0*CA1*m12squared*SA3*dgAtMZ())/(CB*MW) + (0.1875D0*CA1*CA22*m12&
  &squared*SA3*dgAtMZ())/(CB*MW) - (0.125D0*MH12*SA1*SA3*dgAtMZ())/(CB*MW) - (0.375D0*CA12*MH12*SA1*SA3*dgAtMZ())/(CB*MW) - (0.12&
  &5D0*CA22*MH12*SA1*SA3*dgAtMZ())/(CB*MW) - (0.375D0*CA12*CA22*MH12*SA1*SA3*dgAtMZ())/(CB*MW) - (0.0625D0*MH32*SA1*SA3*dgAtMZ())&
  &/(CB*MW) - (0.1875D0*CA12*MH32*SA1*SA3*dgAtMZ())/(CB*MW) - (0.0625D0*CA22*MH32*SA1*SA3*dgAtMZ())/(CB*MW) - (0.1875D0*CA12*CA22&
  &*MH32*SA1*SA3*dgAtMZ())/(CB*MW) - (0.1875D0*CA1*m12squared*SA22*SA3*dgAtMZ())/(CB*MW) + (0.125D0*MH12*SA1*SA22*SA3*dgAtMZ())/(&
  &CB*MW) + (0.375D0*CA12*MH12*SA1*SA22*SA3*dgAtMZ())/(CB*MW) + (0.0625D0*MH32*SA1*SA22*SA3*dgAtMZ())/(CB*MW) + (0.1875D0*CA12*MH&
  &32*SA1*SA22*SA3*dgAtMZ())/(CB*MW) + (0.28125D0*CA1*CA3*m12squared*SA2*dgAtMZ())/(MW*SB) + (0.84375D0*CA1*CA22*CA3*m12squared*S&
  &A2*dgAtMZ())/(MW*SB) - (0.1875D0*CA1*CA3*m12squared*SA2*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA1*CA22*CA3*m12squared*SA2*dgAtMZ()&
  &)/(CB2*MW*SB) + (0.1875D0*CA3*MH12*SA1*SA2*dgAtMZ())/(MW*SB) - (0.1875D0*CA12*CA3*MH12*SA1*SA2*dgAtMZ())/(MW*SB) + (0.5625D0*C&
  &A22*CA3*MH12*SA1*SA2*dgAtMZ())/(MW*SB) - (0.5625D0*CA12*CA22*CA3*MH12*SA1*SA2*dgAtMZ())/(MW*SB) + (0.09375D0*CA3*MH32*SA1*SA2*&
  &dgAtMZ())/(MW*SB) - (0.09375D0*CA12*CA3*MH32*SA1*SA2*dgAtMZ())/(MW*SB) + (0.28125D0*CA22*CA3*MH32*SA1*SA2*dgAtMZ())/(MW*SB) - &
  &(0.28125D0*CA12*CA22*CA3*MH32*SA1*SA2*dgAtMZ())/(MW*SB) + (0.28125D0*CA1*CA3*m12squared*SA12*SA2*dgAtMZ())/(CB2*MW*SB) + (0.84&
  &375D0*CA1*CA22*CA3*m12squared*SA12*SA2*dgAtMZ())/(CB2*MW*SB) + (0.125D0*CA1*MH12*SA3*dgAtMZ())/(MW*SB) + (0.125D0*CA1*CA22*MH1&
  &2*SA3*dgAtMZ())/(MW*SB) + (0.0625D0*CA1*MH32*SA3*dgAtMZ())/(MW*SB) + (0.0625D0*CA1*CA22*MH32*SA3*dgAtMZ())/(MW*SB) - (0.1875D0&
  &*m12squared*SA1*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*CA22*m12squared*SA1*SA3*dgAtMZ())/(MW*SB) + (0.125D0*m12squared*SA1*SA3*dgAt&
  &MZ())/(CB2*MW*SB) + (0.5625D0*CA12*m12squared*SA1*SA3*dgAtMZ())/(CB2*MW*SB) + (0.125D0*CA22*m12squared*SA1*SA3*dgAtMZ())/(CB2*&
  &MW*SB) + (0.5625D0*CA12*CA22*m12squared*SA1*SA3*dgAtMZ())/(CB2*MW*SB) + (0.375D0*CA1*MH12*SA12*SA3*dgAtMZ())/(MW*SB) + (0.375D&
  &0*CA1*CA22*MH12*SA12*SA3*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*MH32*SA12*SA3*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*CA22*MH32*SA12*SA3*&
  &dgAtMZ())/(MW*SB) - (0.125D0*CA1*MH12*SA22*SA3*dgAtMZ())/(MW*SB) - (0.0625D0*CA1*MH32*SA22*SA3*dgAtMZ())/(MW*SB) + (0.1875D0*m&
  &12squared*SA1*SA22*SA3*dgAtMZ())/(MW*SB) - (0.125D0*m12squared*SA1*SA22*SA3*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA12*m12squared*&
  &SA1*SA22*SA3*dgAtMZ())/(CB2*MW*SB) - (0.375D0*CA1*MH12*SA12*SA22*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*CA1*MH32*SA12*SA22*SA3*dgAt&
  &MZ())/(MW*SB) - (0.1875D0*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*MW*SB2) + (0.28125D0*CA12*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*&
  &MW*SB2) - (0.5625D0*CA22*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*MW*SB2) + (0.84375D0*CA12*CA22*CA3*m12squared*SA1*SA2*dgAtMZ())/&
  &(CB*MW*SB2) - (0.125D0*CA1*m12squared*SA3*dgAtMZ())/(CB*MW*SB2) - (0.125D0*CA1*CA22*m12squared*SA3*dgAtMZ())/(CB*MW*SB2) - (0.&
  &5625D0*CA1*m12squared*SA12*SA3*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA1*CA22*m12squared*SA12*SA3*dgAtMZ())/(CB*MW*SB2) + (0.125D0&
  &*CA1*m12squared*SA22*SA3*dgAtMZ())/(CB*MW*SB2) + (0.5625D0*CA1*m12squared*SA12*SA22*SA3*dgAtMZ())/(CB*MW*SB2) - (0.09375D0*CA3&
  &*m12squared*SA1*SA2*dgAtMZ())/(MW*SB*TB) - (0.28125D0*CA22*CA3*m12squared*SA1*SA2*dgAtMZ())/(MW*SB*TB) - (0.0625D0*CA1*m12squa&
  &red*SA3*dgAtMZ())/(MW*SB*TB) - (0.0625D0*CA1*CA22*m12squared*SA3*dgAtMZ())/(MW*SB*TB) + (0.0625D0*CA1*m12squared*SA22*SA3*dgAt&
  &MZ())/(MW*SB*TB) - (0.09375D0*CA1*CA3*m12squared*SA2*TB*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*CA3*m12squared*SA2*TB*dgAtMZ()&
  &)/(CB*MW) + (0.0625D0*m12squared*SA1*SA3*TB*dgAtMZ())/(CB*MW) + (0.0625D0*CA22*m12squared*SA1*SA3*TB*dgAtMZ())/(CB*MW) - (0.06&
  &25D0*m12squared*SA1*SA22*SA3*TB*dgAtMZ())/(CB*MW) + (0.0625D0*CA3*MH12*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875D0*&
  &CA22*CA3*MH12*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.03125D0*CA3*MH32*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0&
  &.09375D0*CA22*CA3*MH32*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.09375D0*CA3*m12squared*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ(&
  &))/(CB2*MW*SB) - (0.28125D0*CA22*CA3*m12squared*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.125D0*MH12*SA3*DBLE(CA1**I&
  &NT(3.D0))*dgAtMZ())/(MW*SB) - (0.125D0*CA22*MH12*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.0625D0*MH32*SA3*DBLE(CA1**INT&
  &(3.D0))*dgAtMZ())/(MW*SB) - (0.0625D0*CA22*MH32*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.125D0*MH12*SA22*SA3*DBLE(CA1**&
  &INT(3.D0))*dgAtMZ())/(MW*SB) + (0.0625D0*MH32*SA22*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*m12squared*SA3*DBLE(&
  &CA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.1875D0*CA22*m12squared*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*m&
  &12squared*SA22*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.125D0*MH12*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.&
  &125D0*CA22*MH12*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.0625D0*MH32*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.06&
  &25D0*CA22*MH32*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.125D0*MH12*SA22*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0&
  &.0625D0*MH32*SA22*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.0625D0*CA3*MH12*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) +&
  & (0.1875D0*CA22*CA3*MH12*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.03125D0*CA3*MH32*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(&
  &MW*SB) + (0.09375D0*CA22*CA3*MH32*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.1875D0*m12squared*SA3*DBLE(SA1**INT(3.D0))*d&
  &gAtMZ())/(CB2*MW*SB) - (0.1875D0*CA22*m12squared*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*m12squared*SA22*SA&
  &3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.09375D0*CA3*m12squared*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.2&
  &8125D0*CA22*CA3*m12squared*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*CA1*CA3*MH12*DBLE(SA2**INT(3.D0))*dgAtMZ&
  &())/(CB*MW) - (0.09375D0*CA1*CA3*MH32*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.28125D0*CA3*m12squared*SA1*DBLE(SA2**INT(3.D&
  &0))*dgAtMZ())/(CB*MW) + (0.1875D0*CA1*CA3*MH12*SA12*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*CA3*MH32*SA12*DBLE&
  &(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA3*m12squared*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*CA3*&
  &m12squared*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875D0*CA3*MH12*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.187&
  &5D0*CA12*CA3*MH12*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.09375D0*CA3*MH32*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) &
  &+ (0.09375D0*CA12*CA3*MH32*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.28125D0*CA1*CA3*m12squared*SA12*DBLE(SA2**INT(3.D0)&
  &)*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*CA3*m12squared*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA12*CA3*m12squ&
  &ared*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.09375D0*CA3*m12squared*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB*TB) &
  &+ (0.09375D0*CA1*CA3*m12squared*TB*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*CA3*MH12*DBLE(CA1**INT(3.D0))*DBLE(SA2**&
  &INT(3.D0))*dgAtMZ())/(CB*MW) - (0.03125D0*CA3*MH32*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA&
  &3*m12squared*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.0625D0*CA3*MH12*DBLE(SA1**INT(3.D0))*DBLE(SA&
  &2**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.03125D0*CA3*MH32*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0&
  &*CA3*m12squared*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.28125D0*CA3*EL*SA1*SA2*dm122MSBarAlter())&
  &/(CB*MW*SW) + (0.84375D0*CA22*CA3*EL*SA1*SA2*dm122MSBarAlter())/(CB*MW*SW) + (0.1875D0*CA1*EL*SA3*dm122MSBarAlter())/(CB*MW*SW&
  &) + (0.1875D0*CA1*CA22*EL*SA3*dm122MSBarAlter())/(CB*MW*SW) - (0.1875D0*CA1*EL*SA22*SA3*dm122MSBarAlter())/(CB*MW*SW) + (0.281&
  &25D0*CA1*CA3*EL*SA2*dm122MSBarAlter())/(MW*SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*SA2*dm122MSBarAlter())/(MW*SB*SW) - (0.1875D0*C&
  &A1*CA3*EL*SA2*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA22*CA3*EL*SA2*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.28125D0&
  &*CA1*CA3*EL*SA12*SA2*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*SA12*SA2*dm122MSBarAlter())/(CB2*MW*SB*SW)&
  & - (0.1875D0*EL*SA1*SA3*dm122MSBarAlter())/(MW*SB*SW) - (0.1875D0*CA22*EL*SA1*SA3*dm122MSBarAlter())/(MW*SB*SW) + (0.125D0*EL*&
  &SA1*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA12*EL*SA1*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.125D0*CA22*EL*SA1&
  &*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA12*CA22*EL*SA1*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.1875D0*EL*SA1*S&
  &A22*SA3*dm122MSBarAlter())/(MW*SB*SW) - (0.125D0*EL*SA1*SA22*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.5625D0*CA12*EL*SA1*SA2&
  &2*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.1875D0*CA3*EL*SA1*SA2*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.28125D0*CA12*CA3*EL*&
  &SA1*SA2*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.5625D0*CA22*CA3*EL*SA1*SA2*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.84375D0*CA12*&
  &CA22*CA3*EL*SA1*SA2*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.125D0*CA1*EL*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.125D0*CA1*C&
  &A22*EL*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.5625D0*CA1*EL*SA12*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA2&
  &2*EL*SA12*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.125D0*CA1*EL*SA22*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.5625D0*CA1*E&
  &L*SA12*SA22*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.09375D0*CA3*EL*SA1*SA2*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.28125D0*CA&
  &22*CA3*EL*SA1*SA2*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.0625D0*CA1*EL*SA3*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.0625D0*CA1*CA2&
  &2*EL*SA3*dm122MSBarAlter())/(MW*SB*SW*TB) + (0.0625D0*CA1*EL*SA22*SA3*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.09375D0*CA1*CA3*EL&
  &*SA2*TB*dm122MSBarAlter())/(CB*MW*SW) - (0.28125D0*CA1*CA22*CA3*EL*SA2*TB*dm122MSBarAlter())/(CB*MW*SW) + (0.0625D0*EL*SA1*SA3&
  &*TB*dm122MSBarAlter())/(CB*MW*SW) + (0.0625D0*CA22*EL*SA1*SA3*TB*dm122MSBarAlter())/(CB*MW*SW) - (0.0625D0*EL*SA1*SA22*SA3*TB*&
  &dm122MSBarAlter())/(CB*MW*SW) - (0.09375D0*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.28125D0*CA22&
  &*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.1875D0*EL*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/&
  &(CB*MW*SB2*SW) + (0.1875D0*CA22*EL*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.1875D0*EL*SA22*SA3*DBLE(CA1&
  &**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.1875D0*EL*SA3*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.&
  &1875D0*CA22*EL*SA3*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.1875D0*EL*SA22*SA3*DBLE(SA1**INT(3.D0))*dm122MS&
  &BarAlter())/(CB2*MW*SB*SW) - (0.09375D0*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA22*CA&
  &3*EL*SA2*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dm122MSBarAlter()&
  &)/(CB*MW*SW) - (0.28125D0*CA1*CA3*EL*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*DBLE(SA2**INT(3&
  &.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.28125D0*CA1*CA3*EL*SA12*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) + &
  &(0.1875D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA12*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*&
  &dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.09375D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(MW*SB*SW*TB) + (0.09375D0*C&
  &A1*CA3*EL*TB*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SW) + (0.09375D0*CA3*EL*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*&
  &dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.09375D0*CA3*EL*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*&
  &SW) + (0.1875D0*CA1*CA3*EL*SA2*dMH12OSAlter())/(CB*MW*SW) + (0.5625D0*CA1*CA22*CA3*EL*SA2*dMH12OSAlter())/(CB*MW*SW) - (0.1875&
  &D0*CA1*CA3*EL*SA12*SA2*dMH12OSAlter())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*EL*SA12*SA2*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*E&
  &L*SA1*SA3*dMH12OSAlter())/(CB*MW*SW) - (0.375D0*CA12*EL*SA1*SA3*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*CA22*EL*SA1*SA3*dMH12OSA&
  &lter())/(CB*MW*SW) - (0.375D0*CA12*CA22*EL*SA1*SA3*dMH12OSAlter())/(CB*MW*SW) + (0.125D0*EL*SA1*SA22*SA3*dMH12OSAlter())/(CB*M&
  &W*SW) + (0.375D0*CA12*EL*SA1*SA22*SA3*dMH12OSAlter())/(CB*MW*SW) + (0.1875D0*CA3*EL*SA1*SA2*dMH12OSAlter())/(MW*SB*SW) - (0.18&
  &75D0*CA12*CA3*EL*SA1*SA2*dMH12OSAlter())/(MW*SB*SW) + (0.5625D0*CA22*CA3*EL*SA1*SA2*dMH12OSAlter())/(MW*SB*SW) - (0.5625D0*CA1&
  &2*CA22*CA3*EL*SA1*SA2*dMH12OSAlter())/(MW*SB*SW) + (0.125D0*CA1*EL*SA3*dMH12OSAlter())/(MW*SB*SW) + (0.125D0*CA1*CA22*EL*SA3*d&
  &MH12OSAlter())/(MW*SB*SW) + (0.375D0*CA1*EL*SA12*SA3*dMH12OSAlter())/(MW*SB*SW) + (0.375D0*CA1*CA22*EL*SA12*SA3*dMH12OSAlter()&
  &)/(MW*SB*SW) - (0.125D0*CA1*EL*SA22*SA3*dMH12OSAlter())/(MW*SB*SW) - (0.375D0*CA1*EL*SA12*SA22*SA3*dMH12OSAlter())/(MW*SB*SW) &
  &- (0.5D0*CA2*CA3*dMH12OSAlter())/vS - (1.5D0*CA2*CA3*SA22*dMH12OSAlter())/vS + (0.0625D0*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dMH12&
  &OSAlter())/(CB*MW*SW) + (0.1875D0*CA22*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*EL*SA3*DBLE(CA1**&
  &INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) - (0.125D0*CA22*EL*SA3*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.125D0*EL*SA2&
  &2*SA3*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.5D0*CA3*DBLE(CA2**INT(3.D0))*dMH12OSAlter())/vS + (0.125D0*EL*SA3*D&
  &BLE(SA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) + (0.125D0*CA22*EL*SA3*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.125&
  &D0*EL*SA22*SA3*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) + (0.0625D0*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(MW&
  &*SB*SW) + (0.1875D0*CA22*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*DBLE(SA2**INT(3.D0)&
  &)*dMH12OSAlter())/(CB*MW*SW) + (0.1875D0*CA1*CA3*EL*SA12*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.1875D0*CA3*EL*SA&
  &1*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.1875D0*CA12*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) &
  &- (0.0625D0*CA3*EL*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.0625D0*CA3*EL*DBLE(SA1**INT(3.D0)&
  &)*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.09375D0*CA1*CA3*EL*SA2*dMH32OSAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA22&
  &*CA3*EL*SA2*dMH32OSAlter())/(CB*MW*SW) - (0.09375D0*CA1*CA3*EL*SA12*SA2*dMH32OSAlter())/(CB*MW*SW) - (0.28125D0*CA1*CA22*CA3*E&
  &L*SA12*SA2*dMH32OSAlter())/(CB*MW*SW) - (0.0625D0*EL*SA1*SA3*dMH32OSAlter())/(CB*MW*SW) - (0.1875D0*CA12*EL*SA1*SA3*dMH32OSAlt&
  &er())/(CB*MW*SW) - (0.0625D0*CA22*EL*SA1*SA3*dMH32OSAlter())/(CB*MW*SW) - (0.1875D0*CA12*CA22*EL*SA1*SA3*dMH32OSAlter())/(CB*M&
  &W*SW) + (0.0625D0*EL*SA1*SA22*SA3*dMH32OSAlter())/(CB*MW*SW) + (0.1875D0*CA12*EL*SA1*SA22*SA3*dMH32OSAlter())/(CB*MW*SW) + (0.&
  &09375D0*CA3*EL*SA1*SA2*dMH32OSAlter())/(MW*SB*SW) - (0.09375D0*CA12*CA3*EL*SA1*SA2*dMH32OSAlter())/(MW*SB*SW) + (0.28125D0*CA2&
  &2*CA3*EL*SA1*SA2*dMH32OSAlter())/(MW*SB*SW) - (0.28125D0*CA12*CA22*CA3*EL*SA1*SA2*dMH32OSAlter())/(MW*SB*SW) + (0.0625D0*CA1*E&
  &L*SA3*dMH32OSAlter())/(MW*SB*SW) + (0.0625D0*CA1*CA22*EL*SA3*dMH32OSAlter())/(MW*SB*SW) + (0.1875D0*CA1*EL*SA12*SA3*dMH32OSAlt&
  &er())/(MW*SB*SW) + (0.1875D0*CA1*CA22*EL*SA12*SA3*dMH32OSAlter())/(MW*SB*SW) - (0.0625D0*CA1*EL*SA22*SA3*dMH32OSAlter())/(MW*S&
  &B*SW) - (0.1875D0*CA1*EL*SA12*SA22*SA3*dMH32OSAlter())/(MW*SB*SW) - (0.25D0*CA2*CA3*dMH32OSAlter())/vS - (0.75D0*CA2*CA3*SA22*&
  &dMH32OSAlter())/vS + (0.03125D0*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) + (0.09375D0*CA22*CA3*EL*SA2*DBLE(C&
  &A1**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) - (0.0625D0*EL*SA3*DBLE(CA1**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) - (0.0625D0*CA22*&
  &EL*SA3*DBLE(CA1**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) + (0.0625D0*EL*SA22*SA3*DBLE(CA1**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW)&
  & + (0.25D0*CA3*DBLE(CA2**INT(3.D0))*dMH32OSAlter())/vS + (0.0625D0*EL*SA3*DBLE(SA1**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) + (0&
  &.0625D0*CA22*EL*SA3*DBLE(SA1**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) - (0.0625D0*EL*SA22*SA3*DBLE(SA1**INT(3.D0))*dMH32OSAlter(&
  &))/(CB*MW*SW) + (0.03125D0*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) + (0.09375D0*CA22*CA3*EL*SA2*DBLE(SA1**I&
  &NT(3.D0))*dMH32OSAlter())/(MW*SB*SW) - (0.09375D0*CA1*CA3*EL*DBLE(SA2**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) + (0.09375D0*CA1*&
  &CA3*EL*SA12*DBLE(SA2**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) - (0.09375D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dMH32OSAlter())/(MW*S&
  &B*SW) + (0.09375D0*CA12*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) - (0.03125D0*CA3*EL*DBLE(CA1**INT(3.D0))*DB&
  &LE(SA2**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) - (0.03125D0*CA3*EL*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH32OSAlter())/(M&
  &W*SB*SW) - (0.09375D0*CA1*CA3*EL*MH12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.28125D0*CA1*CA22*CA3*EL*MH12*SA2*DBLE&
  &(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.046875D0*CA1*CA3*EL*MH32*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.140625D&
  &0*CA1*CA22*CA3*EL*MH32*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.140625D0*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D&
  &0))*dMW2Alter())/(CB*SW) - (0.421875D0*CA22*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.09375D0*C&
  &A1*CA3*EL*MH12*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.28125D0*CA1*CA22*CA3*EL*MH12*SA12*SA2*DBLE(MW**INT(-3.D&
  &0))*dMW2Alter())/(CB*SW) + (0.046875D0*CA1*CA3*EL*MH32*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*CA1*CA&
  &22*CA3*EL*MH32*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2&
  &Alter())/(CB*SW) - (0.09375D0*CA1*CA22*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0625D0*EL*MH12*SA1*SA3&
  &*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.1875D0*CA12*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.062&
  &5D0*CA22*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.1875D0*CA12*CA22*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*&
  &dMW2Alter())/(CB*SW) + (0.03125D0*EL*MH32*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.09375D0*CA12*EL*MH32*SA1*SA3*&
  &DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*CA22*EL*MH32*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.093&
  &75D0*CA12*CA22*EL*MH32*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.09375D0*CA1*EL*m12squared*SA22*SA3*DBLE(MW**INT(&
  &-3.D0))*dMW2Alter())/(CB*SW) - (0.0625D0*EL*MH12*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.1875D0*CA12*EL*MH&
  &12*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*EL*MH32*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/&
  &(CB*SW) - (0.09375D0*CA12*EL*MH32*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.140625D0*CA1*CA3*EL*m12squared*S&
  &A2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.421875D0*CA1*CA22*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(S&
  &B*SW) + (0.09375D0*CA1*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.28125D0*CA1*CA22*CA3*EL*m12squ&
  &ared*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.09375D0*CA3*EL*MH12*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*&
  &SW) + (0.09375D0*CA12*CA3*EL*MH12*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.28125D0*CA22*CA3*EL*MH12*SA1*SA2*DBLE&
  &(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.28125D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (&
  &0.046875D0*CA3*EL*MH32*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.046875D0*CA12*CA3*EL*MH32*SA1*SA2*DBLE(MW**INT(-&
  &3.D0))*dMW2Alter())/(SB*SW) - (0.140625D0*CA22*CA3*EL*MH32*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.140625D0*CA1&
  &2*CA22*CA3*EL*MH32*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.140625D0*CA1*CA3*EL*m12squared*SA12*SA2*DBLE(MW**INT&
  &(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.421875D0*CA1*CA22*CA3*EL*m12squared*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*&
  &SW) - (0.0625D0*CA1*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.0625D0*CA1*CA22*EL*MH12*SA3*DBLE(MW**INT(-3.D0)&
  &)*dMW2Alter())/(SB*SW) - (0.03125D0*CA1*EL*MH32*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.03125D0*CA1*CA22*EL*MH32*SA&
  &3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.&
  &09375D0*CA22*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.0625D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3&
  &.D0))*dMW2Alter())/(CB2*SB*SW) - (0.28125D0*CA12*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.0625&
  &D0*CA22*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.28125D0*CA12*CA22*EL*m12squared*SA1*SA3*DBLE(&
  &MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.1875D0*CA1*EL*MH12*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.1875D&
  &0*CA1*CA22*EL*MH12*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*CA1*EL*MH32*SA12*SA3*DBLE(MW**INT(-3.D0))*d&
  &MW2Alter())/(SB*SW) - (0.09375D0*CA1*CA22*EL*MH32*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.0625D0*CA1*EL*MH12*S&
  &A22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.03125D0*CA1*EL*MH32*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) &
  &- (0.09375D0*EL*m12squared*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.0625D0*EL*m12squared*SA1*SA22*SA3*DBLE(&
  &MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.28125D0*CA12*EL*m12squared*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*S&
  &B*SW) + (0.1875D0*CA1*EL*MH12*SA12*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA1*EL*MH32*SA12*SA22*SA3*D&
  &BLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) &
  &- (0.140625D0*CA12*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.28125D0*CA22*CA3*EL*m12squared&
  &*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.421875D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*&
  &dMW2Alter())/(CB*SB2*SW) + (0.0625D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.0625D0*CA1*CA22*&
  &EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.28125D0*CA1*EL*m12squared*SA12*SA3*DBLE(MW**INT(-3.D0))*d&
  &MW2Alter())/(CB*SB2*SW) + (0.28125D0*CA1*CA22*EL*m12squared*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.0625D0&
  &*CA1*EL*m12squared*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.28125D0*CA1*EL*m12squared*SA12*SA22*SA3*DBLE(MW&
  &**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.046875D0*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + &
  &(0.140625D0*CA22*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.03125D0*CA1*EL*m12squared*SA3*DBL&
  &E(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.03125D0*CA1*CA22*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB)&
  & - (0.03125D0*CA1*EL*m12squared*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.046875D0*CA1*CA3*EL*m12squared*SA2*&
  &TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*CA1*CA22*CA3*EL*m12squared*SA2*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())&
  &/(CB*SW) - (0.03125D0*EL*m12squared*SA1*SA3*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA22*EL*m12squared*SA1*S&
  &A3*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*EL*m12squared*SA1*SA22*SA3*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(&
  &CB*SW) - (0.03125D0*CA3*EL*MH12*SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA22*CA3*EL*MH&
  &12*SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.015625D0*CA3*EL*MH32*SA2*DBLE(CA1**INT(3.D0))*DBLE(&
  &MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.046875D0*CA22*CA3*EL*MH32*SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())&
  &/(CB*SW) + (0.046875D0*CA3*EL*m12squared*SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.140625D0*&
  &CA22*CA3*EL*m12squared*SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.0625D0*EL*MH12*SA3*DBLE(CA1&
  &**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.0625D0*CA22*EL*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*&
  &dMW2Alter())/(SB*SW) + (0.03125D0*EL*MH32*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.03125D0*CA22&
  &*EL*MH32*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.0625D0*EL*MH12*SA22*SA3*DBLE(CA1**INT(3.D0))*&
  &DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.03125D0*EL*MH32*SA22*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())&
  &/(SB*SW) - (0.09375D0*EL*m12squared*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.09375D0*CA22*E&
  &L*m12squared*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.09375D0*EL*m12squared*SA22*SA3*DBLE(C&
  &A1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.0625D0*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))&
  &*dMW2Alter())/(CB*SW) - (0.0625D0*CA22*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0&
  &*EL*MH32*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA22*EL*MH32*SA3*DBLE(MW**INT(-3.D0))&
  &*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.0625D0*EL*MH12*SA22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())&
  &/(CB*SW) + (0.03125D0*EL*MH32*SA22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA3*EL*MH12&
  &*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*CA22*CA3*EL*MH12*SA2*DBLE(MW**INT(-3.D0))*DBL&
  &E(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.015625D0*CA3*EL*MH32*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(S&
  &B*SW) - (0.046875D0*CA22*CA3*EL*MH32*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*EL*m12squ&
  &ared*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.09375D0*CA22*EL*m12squared*SA3*DBLE(MW**INT(-&
  &3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.09375D0*EL*m12squared*SA22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.&
  &D0))*dMW2Alter())/(CB2*SB*SW) + (0.046875D0*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*S&
  &B2*SW) + (0.140625D0*CA22*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.09375D&
  &0*CA1*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.046875D0*CA1*CA3*EL*MH32*DBLE(MW**INT(-3&
  &.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*&
  &dMW2Alter())/(CB*SW) - (0.09375D0*CA1*CA3*EL*MH12*SA12*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.046&
  &875D0*CA1*CA3*EL*MH32*SA12*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*CA1*CA3*EL*m12squared*&
  &DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*DBLE(SA&
  &2**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.09375D0*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB&
  &*SW) - (0.09375D0*CA12*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.046875D0*CA3*EL*MH3&
  &2*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*CA12*CA3*EL*MH32*SA1*DBLE(MW**INT(-3.D0))*D&
  &BLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.140625D0*CA1*CA3*EL*m12squared*SA12*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dM&
  &W2Alter())/(CB2*SB*SW) - (0.09375D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) +&
  & (0.140625D0*CA12*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.046875D0*CA3*E&
  &L*m12squared*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW*TB) - (0.046875D0*CA1*CA3*EL*m12squared*TB*DBLE&
  &(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*CA3*EL*MH12*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*&
  &DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.015625D0*CA3*EL*MH32*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.&
  &D0))*dMW2Alter())/(CB*SW) - (0.046875D0*CA3*EL*m12squared*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2A&
  &lter())/(CB2*SB*SW) + (0.03125D0*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*S&
  &W) + (0.015625D0*CA3*EL*MH32*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0&
  &*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + 0.5D0*CA2*CA3*MH1&
  &2*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() + 0.25D0*CA2*CA3*MH32*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() + 1.5D0*CA2*CA3*MH12*SA22*DB&
  &LE(vS**INT(-2.D0))*dvSMSBarAlter() + 0.75D0*CA2*CA3*MH32*SA22*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() - 0.5D0*CA3*MH12*DBLE(CA2**&
  &INT(3.D0))*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() - 0.25D0*CA3*MH32*DBLE(CA2**INT(3.D0))*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() &
		& + CS1S1S1f111*dZH1H3OSAlter()/2D0 + CS1S1S1f211*dZH2H3OSAlter()/2D0 + CS1S1S1f311*dZH3H3OS()/2D0 &
		& + CS1S1S1f131*dZH1H1OS()/2D0 + CS1S1S1f231*dZH2H1OSAlter()/2D0 + CS1S1S1f331*dZH3H1OSAlter()/2D0 &
		& + CS1S1S1f131*dZH1H1OS()/2D0 + CS1S1S1f231*dZH2H1OSAlter()/2D0 + CS1S1S1f331*dZH3H1OSAlter()/2D0 &
		& )
	case (6)
		dRR11 = -CA2*SA1*dAlpha1PinchPStar() - CA1*SA2*dAlpha2PinchPStar()
		dRR12 = CA1*CA2*dAlpha1PinchPStar() - SA1*SA2*dAlpha2PinchPStar()
		dRR13 = CA2*dAlpha2PinchPStar()
		dRR21 = -CA1*CA3*dAlpha1PinchPStar() - CA1*CA3*SA2*dAlpha3PinchPStar() - CA1*CA2*SA3*dAlpha2PinchPStar() + &
			& SA1*SA3*dAlpha3PinchPStar() + SA1*SA2*SA3*dAlpha1PinchPStar()
		dRR22 = -CA3*SA1*dAlpha1PinchPStar() - CA3*SA1*SA2*dAlpha3PinchPStar() - CA1*SA3*dAlpha3PinchPStar() - &
			& CA2*SA1*SA3*dAlpha2PinchPStar() - CA1*SA2*SA3*dAlpha1PinchPStar()
		dRR23 = CA2*CA3*dAlpha3PinchPStar() - SA2*SA3*dAlpha2PinchPStar()
		dRR31 = -CA1*CA2*CA3*dAlpha2PinchPStar() + CA3*SA1*dAlpha3PinchPStar() + CA3*SA1*SA2*dAlpha1PinchPStar() + &
			& CA1*SA3*dAlpha1PinchPStar() + CA1*SA2*SA3*dAlpha3PinchPStar()
		dRR32 = -CA1*CA3*dAlpha3PinchPStar() - CA2*CA3*SA1*dAlpha2PinchPStar() - CA1*CA3*SA2*dAlpha1PinchPStar() + &
			& SA1*SA3*dAlpha1PinchPStar() + SA1*SA2*SA3*dAlpha3PinchPStar()
		dRR33 = -CA3*SA2*dAlpha2PinchPStar() - CA2*SA3*dAlpha3PinchPStar()

		totalAmplitude = CS1S1S1f311*( &
&(0.28125D0*CA1*CA3*EL*m12squared*SA2*dAlpha1PinchPStar())/(CB*MW*SW) + (0.84375D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha1PinchPSta&
  &r())/(CB*MW*SW) - (0.1875D0*CA3*EL*MH12*SA1*SA2*dAlpha1PinchPStar())/(CB*MW*SW) - (0.5625D0*CA12*CA3*EL*MH12*SA1*SA2*dAlpha1Pi&
  &nchPStar())/(CB*MW*SW) - (0.5625D0*CA22*CA3*EL*MH12*SA1*SA2*dAlpha1PinchPStar())/(CB*MW*SW) - (1.6875D0*CA12*CA22*CA3*EL*MH12*&
  &SA1*SA2*dAlpha1PinchPStar())/(CB*MW*SW) - (0.09375D0*CA3*EL*MH32*SA1*SA2*dAlpha1PinchPStar())/(CB*MW*SW) - (0.28125D0*CA12*CA3&
  &*EL*MH32*SA1*SA2*dAlpha1PinchPStar())/(CB*MW*SW) - (0.28125D0*CA22*CA3*EL*MH32*SA1*SA2*dAlpha1PinchPStar())/(CB*MW*SW) - (0.84&
  &375D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*dAlpha1PinchPStar())/(CB*MW*SW) - (0.125D0*CA1*EL*MH12*SA3*dAlpha1PinchPStar())/(CB*MW*SW)&
  & - (0.125D0*CA1*CA22*EL*MH12*SA3*dAlpha1PinchPStar())/(CB*MW*SW) - (0.0625D0*CA1*EL*MH32*SA3*dAlpha1PinchPStar())/(CB*MW*SW) -&
  & (0.0625D0*CA1*CA22*EL*MH32*SA3*dAlpha1PinchPStar())/(CB*MW*SW) - (0.1875D0*EL*m12squared*SA1*SA3*dAlpha1PinchPStar())/(CB*MW*&
  &SW) - (0.1875D0*CA22*EL*m12squared*SA1*SA3*dAlpha1PinchPStar())/(CB*MW*SW) + (1.125D0*CA1*EL*MH12*SA12*SA3*dAlpha1PinchPStar()&
  &)/(CB*MW*SW) + (1.125D0*CA1*CA22*EL*MH12*SA12*SA3*dAlpha1PinchPStar())/(CB*MW*SW) + (0.5625D0*CA1*EL*MH32*SA12*SA3*dAlpha1Pinc&
  &hPStar())/(CB*MW*SW) + (0.5625D0*CA1*CA22*EL*MH32*SA12*SA3*dAlpha1PinchPStar())/(CB*MW*SW) + (0.125D0*CA1*EL*MH12*SA22*SA3*dAl&
  &pha1PinchPStar())/(CB*MW*SW) + (0.0625D0*CA1*EL*MH32*SA22*SA3*dAlpha1PinchPStar())/(CB*MW*SW) + (0.1875D0*EL*m12squared*SA1*SA&
  &22*SA3*dAlpha1PinchPStar())/(CB*MW*SW) - (1.125D0*CA1*EL*MH12*SA12*SA22*SA3*dAlpha1PinchPStar())/(CB*MW*SW) - (0.5625D0*CA1*EL&
  &*MH32*SA12*SA22*SA3*dAlpha1PinchPStar())/(CB*MW*SW) + (0.1875D0*CA1*CA3*EL*MH12*SA2*dAlpha1PinchPStar())/(MW*SB*SW) + (0.5625D&
  &0*CA1*CA22*CA3*EL*MH12*SA2*dAlpha1PinchPStar())/(MW*SB*SW) + (0.09375D0*CA1*CA3*EL*MH32*SA2*dAlpha1PinchPStar())/(MW*SB*SW) + &
  &(0.28125D0*CA1*CA22*CA3*EL*MH32*SA2*dAlpha1PinchPStar())/(MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*SA1*SA2*dAlpha1PinchPStar()&
  &)/(MW*SB*SW) - (0.84375D0*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha1PinchPStar())/(MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA1*SA2&
  &*dAlpha1PinchPStar())/(CB2*MW*SB*SW) + (0.84375D0*CA12*CA3*EL*m12squared*SA1*SA2*dAlpha1PinchPStar())/(CB2*MW*SB*SW) + (0.5625&
  &D0*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha1PinchPStar())/(CB2*MW*SB*SW) + (2.53125D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha1&
  &PinchPStar())/(CB2*MW*SB*SW) + (0.5625D0*CA1*CA3*EL*MH12*SA12*SA2*dAlpha1PinchPStar())/(MW*SB*SW) + (1.6875D0*CA1*CA22*CA3*EL*&
  &MH12*SA12*SA2*dAlpha1PinchPStar())/(MW*SB*SW) + (0.28125D0*CA1*CA3*EL*MH32*SA12*SA2*dAlpha1PinchPStar())/(MW*SB*SW) + (0.84375&
  &D0*CA1*CA22*CA3*EL*MH32*SA12*SA2*dAlpha1PinchPStar())/(MW*SB*SW) - (0.1875D0*CA1*EL*m12squared*SA3*dAlpha1PinchPStar())/(MW*SB&
  &*SW) - (0.1875D0*CA1*CA22*EL*m12squared*SA3*dAlpha1PinchPStar())/(MW*SB*SW) + (0.125D0*CA1*EL*m12squared*SA3*dAlpha1PinchPStar&
  &())/(CB2*MW*SB*SW) + (0.125D0*CA1*CA22*EL*m12squared*SA3*dAlpha1PinchPStar())/(CB2*MW*SB*SW) - (0.125D0*EL*MH12*SA1*SA3*dAlpha&
  &1PinchPStar())/(MW*SB*SW) + (1.125D0*CA12*EL*MH12*SA1*SA3*dAlpha1PinchPStar())/(MW*SB*SW) - (0.125D0*CA22*EL*MH12*SA1*SA3*dAlp&
  &ha1PinchPStar())/(MW*SB*SW) + (1.125D0*CA12*CA22*EL*MH12*SA1*SA3*dAlpha1PinchPStar())/(MW*SB*SW) - (0.0625D0*EL*MH32*SA1*SA3*d&
  &Alpha1PinchPStar())/(MW*SB*SW) + (0.5625D0*CA12*EL*MH32*SA1*SA3*dAlpha1PinchPStar())/(MW*SB*SW) - (0.0625D0*CA22*EL*MH32*SA1*S&
  &A3*dAlpha1PinchPStar())/(MW*SB*SW) + (0.5625D0*CA12*CA22*EL*MH32*SA1*SA3*dAlpha1PinchPStar())/(MW*SB*SW) - (1.6875D0*CA1*EL*m1&
  &2squared*SA12*SA3*dAlpha1PinchPStar())/(CB2*MW*SB*SW) - (1.6875D0*CA1*CA22*EL*m12squared*SA12*SA3*dAlpha1PinchPStar())/(CB2*MW&
  &*SB*SW) + (0.1875D0*CA1*EL*m12squared*SA22*SA3*dAlpha1PinchPStar())/(MW*SB*SW) - (0.125D0*CA1*EL*m12squared*SA22*SA3*dAlpha1Pi&
  &nchPStar())/(CB2*MW*SB*SW) + (0.125D0*EL*MH12*SA1*SA22*SA3*dAlpha1PinchPStar())/(MW*SB*SW) - (1.125D0*CA12*EL*MH12*SA1*SA22*SA&
  &3*dAlpha1PinchPStar())/(MW*SB*SW) + (0.0625D0*EL*MH32*SA1*SA22*SA3*dAlpha1PinchPStar())/(MW*SB*SW) - (0.5625D0*CA12*EL*MH32*SA&
  &1*SA22*SA3*dAlpha1PinchPStar())/(MW*SB*SW) + (1.6875D0*CA1*EL*m12squared*SA12*SA22*SA3*dAlpha1PinchPStar())/(CB2*MW*SB*SW) - (&
  &0.1875D0*CA1*CA3*EL*m12squared*SA2*dAlpha1PinchPStar())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha1Pinch&
  &PStar())/(CB*MW*SB2*SW) - (0.84375D0*CA1*CA3*EL*m12squared*SA12*SA2*dAlpha1PinchPStar())/(CB*MW*SB2*SW) - (2.53125D0*CA1*CA22*&
  &CA3*EL*m12squared*SA12*SA2*dAlpha1PinchPStar())/(CB*MW*SB2*SW) + (0.125D0*EL*m12squared*SA1*SA3*dAlpha1PinchPStar())/(CB*MW*SB&
  &2*SW) - (1.6875D0*CA12*EL*m12squared*SA1*SA3*dAlpha1PinchPStar())/(CB*MW*SB2*SW) + (0.125D0*CA22*EL*m12squared*SA1*SA3*dAlpha1&
  &PinchPStar())/(CB*MW*SB2*SW) - (1.6875D0*CA12*CA22*EL*m12squared*SA1*SA3*dAlpha1PinchPStar())/(CB*MW*SB2*SW) - (0.125D0*EL*m12&
  &squared*SA1*SA22*SA3*dAlpha1PinchPStar())/(CB*MW*SB2*SW) + (1.6875D0*CA12*EL*m12squared*SA1*SA22*SA3*dAlpha1PinchPStar())/(CB*&
  &MW*SB2*SW) - (0.09375D0*CA1*CA3*EL*m12squared*SA2*dAlpha1PinchPStar())/(MW*SB*SW*TB) - (0.28125D0*CA1*CA22*CA3*EL*m12squared*S&
  &A2*dAlpha1PinchPStar())/(MW*SB*SW*TB) + (0.0625D0*EL*m12squared*SA1*SA3*dAlpha1PinchPStar())/(MW*SB*SW*TB) + (0.0625D0*CA22*EL&
  &*m12squared*SA1*SA3*dAlpha1PinchPStar())/(MW*SB*SW*TB) - (0.0625D0*EL*m12squared*SA1*SA22*SA3*dAlpha1PinchPStar())/(MW*SB*SW*T&
  &B) + (0.09375D0*CA3*EL*m12squared*SA1*SA2*TB*dAlpha1PinchPStar())/(CB*MW*SW) + (0.28125D0*CA22*CA3*EL*m12squared*SA1*SA2*TB*dA&
  &lpha1PinchPStar())/(CB*MW*SW) + (0.0625D0*CA1*EL*m12squared*SA3*TB*dAlpha1PinchPStar())/(CB*MW*SW) + (0.0625D0*CA1*CA22*EL*m12&
  &squared*SA3*TB*dAlpha1PinchPStar())/(CB*MW*SW) - (0.0625D0*CA1*EL*m12squared*SA22*SA3*TB*dAlpha1PinchPStar())/(CB*MW*SW) + (0.&
  &1875D0*CA1*CA2*CA3*EL*MH12*dAlpha2PinchPStar())/(CB*MW*SW) + (0.09375D0*CA1*CA2*CA3*EL*MH32*dAlpha2PinchPStar())/(CB*MW*SW) + &
  &(0.28125D0*CA2*CA3*EL*m12squared*SA1*dAlpha2PinchPStar())/(CB*MW*SW) - (0.1875D0*CA1*CA2*CA3*EL*MH12*SA12*dAlpha2PinchPStar())&
  &/(CB*MW*SW) - (0.09375D0*CA1*CA2*CA3*EL*MH32*SA12*dAlpha2PinchPStar())/(CB*MW*SW) - (1.6875D0*CA1*CA2*CA3*EL*MH12*SA22*dAlpha2&
  &PinchPStar())/(CB*MW*SW) - (0.84375D0*CA1*CA2*CA3*EL*MH32*SA22*dAlpha2PinchPStar())/(CB*MW*SW) - (2.53125D0*CA2*CA3*EL*m12squa&
  &red*SA1*SA22*dAlpha2PinchPStar())/(CB*MW*SW) + (1.6875D0*CA1*CA2*CA3*EL*MH12*SA12*SA22*dAlpha2PinchPStar())/(CB*MW*SW) + (0.84&
  &375D0*CA1*CA2*CA3*EL*MH32*SA12*SA22*dAlpha2PinchPStar())/(CB*MW*SW) - (0.75D0*CA1*CA2*EL*m12squared*SA2*SA3*dAlpha2PinchPStar(&
  &))/(CB*MW*SW) + (0.5D0*CA2*EL*MH12*SA1*SA2*SA3*dAlpha2PinchPStar())/(CB*MW*SW) + (1.5D0*CA12*CA2*EL*MH12*SA1*SA2*SA3*dAlpha2Pi&
  &nchPStar())/(CB*MW*SW) + (0.25D0*CA2*EL*MH32*SA1*SA2*SA3*dAlpha2PinchPStar())/(CB*MW*SW) + (0.75D0*CA12*CA2*EL*MH32*SA1*SA2*SA&
  &3*dAlpha2PinchPStar())/(CB*MW*SW) + (0.28125D0*CA1*CA2*CA3*EL*m12squared*dAlpha2PinchPStar())/(MW*SB*SW) - (0.1875D0*CA1*CA2*C&
  &A3*EL*m12squared*dAlpha2PinchPStar())/(CB2*MW*SB*SW) + (0.1875D0*CA2*CA3*EL*MH12*SA1*dAlpha2PinchPStar())/(MW*SB*SW) - (0.1875&
  &D0*CA12*CA2*CA3*EL*MH12*SA1*dAlpha2PinchPStar())/(MW*SB*SW) + (0.09375D0*CA2*CA3*EL*MH32*SA1*dAlpha2PinchPStar())/(MW*SB*SW) -&
  & (0.09375D0*CA12*CA2*CA3*EL*MH32*SA1*dAlpha2PinchPStar())/(MW*SB*SW) + (0.28125D0*CA1*CA2*CA3*EL*m12squared*SA12*dAlpha2PinchP&
  &Star())/(CB2*MW*SB*SW) - (2.53125D0*CA1*CA2*CA3*EL*m12squared*SA22*dAlpha2PinchPStar())/(MW*SB*SW) + (1.6875D0*CA1*CA2*CA3*EL*&
  &m12squared*SA22*dAlpha2PinchPStar())/(CB2*MW*SB*SW) - (1.6875D0*CA2*CA3*EL*MH12*SA1*SA22*dAlpha2PinchPStar())/(MW*SB*SW) + (1.&
  &6875D0*CA12*CA2*CA3*EL*MH12*SA1*SA22*dAlpha2PinchPStar())/(MW*SB*SW) - (0.84375D0*CA2*CA3*EL*MH32*SA1*SA22*dAlpha2PinchPStar()&
  &)/(MW*SB*SW) + (0.84375D0*CA12*CA2*CA3*EL*MH32*SA1*SA22*dAlpha2PinchPStar())/(MW*SB*SW) - (2.53125D0*CA1*CA2*CA3*EL*m12squared&
  &*SA12*SA22*dAlpha2PinchPStar())/(CB2*MW*SB*SW) - (0.5D0*CA1*CA2*EL*MH12*SA2*SA3*dAlpha2PinchPStar())/(MW*SB*SW) - (0.25D0*CA1*&
  &CA2*EL*MH32*SA2*SA3*dAlpha2PinchPStar())/(MW*SB*SW) + (0.75D0*CA2*EL*m12squared*SA1*SA2*SA3*dAlpha2PinchPStar())/(MW*SB*SW) - &
  &(0.5D0*CA2*EL*m12squared*SA1*SA2*SA3*dAlpha2PinchPStar())/(CB2*MW*SB*SW) - (2.25D0*CA12*CA2*EL*m12squared*SA1*SA2*SA3*dAlpha2P&
  &inchPStar())/(CB2*MW*SB*SW) - (1.5D0*CA1*CA2*EL*MH12*SA12*SA2*SA3*dAlpha2PinchPStar())/(MW*SB*SW) - (0.75D0*CA1*CA2*EL*MH32*SA&
  &12*SA2*SA3*dAlpha2PinchPStar())/(MW*SB*SW) - (0.1875D0*CA2*CA3*EL*m12squared*SA1*dAlpha2PinchPStar())/(CB*MW*SB2*SW) + (0.2812&
  &5D0*CA12*CA2*CA3*EL*m12squared*SA1*dAlpha2PinchPStar())/(CB*MW*SB2*SW) + (1.6875D0*CA2*CA3*EL*m12squared*SA1*SA22*dAlpha2Pinch&
  &PStar())/(CB*MW*SB2*SW) - (2.53125D0*CA12*CA2*CA3*EL*m12squared*SA1*SA22*dAlpha2PinchPStar())/(CB*MW*SB2*SW) + (0.5D0*CA1*CA2*&
  &EL*m12squared*SA2*SA3*dAlpha2PinchPStar())/(CB*MW*SB2*SW) + (2.25D0*CA1*CA2*EL*m12squared*SA12*SA2*SA3*dAlpha2PinchPStar())/(C&
  &B*MW*SB2*SW) - (0.09375D0*CA2*CA3*EL*m12squared*SA1*dAlpha2PinchPStar())/(MW*SB*SW*TB) + (0.84375D0*CA2*CA3*EL*m12squared*SA1*&
  &SA22*dAlpha2PinchPStar())/(MW*SB*SW*TB) + (0.25D0*CA1*CA2*EL*m12squared*SA2*SA3*dAlpha2PinchPStar())/(MW*SB*SW*TB) - (0.09375D&
  &0*CA1*CA2*CA3*EL*m12squared*TB*dAlpha2PinchPStar())/(CB*MW*SW) + (0.84375D0*CA1*CA2*CA3*EL*m12squared*SA22*TB*dAlpha2PinchPSta&
  &r())/(CB*MW*SW) - (0.25D0*CA2*EL*m12squared*SA1*SA2*SA3*TB*dAlpha2PinchPStar())/(CB*MW*SW) + (0.5D0*CA3*MH12*SA2*dAlpha2PinchP&
  &Star())/vS - (4.5D0*CA22*CA3*MH12*SA2*dAlpha2PinchPStar())/vS + (0.25D0*CA3*MH32*SA2*dAlpha2PinchPStar())/vS - (2.25D0*CA22*CA&
  &3*MH32*SA2*dAlpha2PinchPStar())/vS + (0.1875D0*CA1*CA3*EL*m12squared*dAlpha3PinchPStar())/(CB*MW*SW) + (0.1875D0*CA1*CA22*CA3*&
  &EL*m12squared*dAlpha3PinchPStar())/(CB*MW*SW) - (0.125D0*CA3*EL*MH12*SA1*dAlpha3PinchPStar())/(CB*MW*SW) - (0.375D0*CA12*CA3*E&
  &L*MH12*SA1*dAlpha3PinchPStar())/(CB*MW*SW) - (0.125D0*CA22*CA3*EL*MH12*SA1*dAlpha3PinchPStar())/(CB*MW*SW) - (0.375D0*CA12*CA2&
  &2*CA3*EL*MH12*SA1*dAlpha3PinchPStar())/(CB*MW*SW) - (0.0625D0*CA3*EL*MH32*SA1*dAlpha3PinchPStar())/(CB*MW*SW) - (0.1875D0*CA12&
  &*CA3*EL*MH32*SA1*dAlpha3PinchPStar())/(CB*MW*SW) - (0.0625D0*CA22*CA3*EL*MH32*SA1*dAlpha3PinchPStar())/(CB*MW*SW) - (0.1875D0*&
  &CA12*CA22*CA3*EL*MH32*SA1*dAlpha3PinchPStar())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*m12squared*SA22*dAlpha3PinchPStar())/(CB*MW*S&
  &W) + (0.125D0*CA3*EL*MH12*SA1*SA22*dAlpha3PinchPStar())/(CB*MW*SW) + (0.375D0*CA12*CA3*EL*MH12*SA1*SA22*dAlpha3PinchPStar())/(&
  &CB*MW*SW) + (0.0625D0*CA3*EL*MH32*SA1*SA22*dAlpha3PinchPStar())/(CB*MW*SW) + (0.1875D0*CA12*CA3*EL*MH32*SA1*SA22*dAlpha3PinchP&
  &Star())/(CB*MW*SW) - (0.1875D0*CA1*EL*MH12*SA2*SA3*dAlpha3PinchPStar())/(CB*MW*SW) - (0.5625D0*CA1*CA22*EL*MH12*SA2*SA3*dAlpha&
  &3PinchPStar())/(CB*MW*SW) - (0.09375D0*CA1*EL*MH32*SA2*SA3*dAlpha3PinchPStar())/(CB*MW*SW) - (0.28125D0*CA1*CA22*EL*MH32*SA2*S&
  &A3*dAlpha3PinchPStar())/(CB*MW*SW) - (0.28125D0*EL*m12squared*SA1*SA2*SA3*dAlpha3PinchPStar())/(CB*MW*SW) - (0.84375D0*CA22*EL&
  &*m12squared*SA1*SA2*SA3*dAlpha3PinchPStar())/(CB*MW*SW) + (0.1875D0*CA1*EL*MH12*SA12*SA2*SA3*dAlpha3PinchPStar())/(CB*MW*SW) +&
  & (0.5625D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dAlpha3PinchPStar())/(CB*MW*SW) + (0.09375D0*CA1*EL*MH32*SA12*SA2*SA3*dAlpha3PinchPSt&
  &ar())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*MH32*SA12*SA2*SA3*dAlpha3PinchPStar())/(CB*MW*SW) + (0.125D0*CA1*CA3*EL*MH12*dAlpha3&
  &PinchPStar())/(MW*SB*SW) + (0.125D0*CA1*CA22*CA3*EL*MH12*dAlpha3PinchPStar())/(MW*SB*SW) + (0.0625D0*CA1*CA3*EL*MH32*dAlpha3Pi&
  &nchPStar())/(MW*SB*SW) + (0.0625D0*CA1*CA22*CA3*EL*MH32*dAlpha3PinchPStar())/(MW*SB*SW) - (0.1875D0*CA3*EL*m12squared*SA1*dAlp&
  &ha3PinchPStar())/(MW*SB*SW) - (0.1875D0*CA22*CA3*EL*m12squared*SA1*dAlpha3PinchPStar())/(MW*SB*SW) + (0.125D0*CA3*EL*m12square&
  &d*SA1*dAlpha3PinchPStar())/(CB2*MW*SB*SW) + (0.5625D0*CA12*CA3*EL*m12squared*SA1*dAlpha3PinchPStar())/(CB2*MW*SB*SW) + (0.125D&
  &0*CA22*CA3*EL*m12squared*SA1*dAlpha3PinchPStar())/(CB2*MW*SB*SW) + (0.5625D0*CA12*CA22*CA3*EL*m12squared*SA1*dAlpha3PinchPStar&
  &())/(CB2*MW*SB*SW) + (0.375D0*CA1*CA3*EL*MH12*SA12*dAlpha3PinchPStar())/(MW*SB*SW) + (0.375D0*CA1*CA22*CA3*EL*MH12*SA12*dAlpha&
  &3PinchPStar())/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*MH32*SA12*dAlpha3PinchPStar())/(MW*SB*SW) + (0.1875D0*CA1*CA22*CA3*EL*MH32*SA&
  &12*dAlpha3PinchPStar())/(MW*SB*SW) - (0.125D0*CA1*CA3*EL*MH12*SA22*dAlpha3PinchPStar())/(MW*SB*SW) - (0.0625D0*CA1*CA3*EL*MH32&
  &*SA22*dAlpha3PinchPStar())/(MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA1*SA22*dAlpha3PinchPStar())/(MW*SB*SW) - (0.125D0*CA3*EL&
  &*m12squared*SA1*SA22*dAlpha3PinchPStar())/(CB2*MW*SB*SW) - (0.5625D0*CA12*CA3*EL*m12squared*SA1*SA22*dAlpha3PinchPStar())/(CB2&
  &*MW*SB*SW) - (0.375D0*CA1*CA3*EL*MH12*SA12*SA22*dAlpha3PinchPStar())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*MH32*SA12*SA22*dAlpha3P&
  &inchPStar())/(MW*SB*SW) - (0.28125D0*CA1*EL*m12squared*SA2*SA3*dAlpha3PinchPStar())/(MW*SB*SW) - (0.84375D0*CA1*CA22*EL*m12squ&
  &ared*SA2*SA3*dAlpha3PinchPStar())/(MW*SB*SW) + (0.1875D0*CA1*EL*m12squared*SA2*SA3*dAlpha3PinchPStar())/(CB2*MW*SB*SW) + (0.56&
  &25D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha3PinchPStar())/(CB2*MW*SB*SW) - (0.1875D0*EL*MH12*SA1*SA2*SA3*dAlpha3PinchPStar())/(&
  &MW*SB*SW) + (0.1875D0*CA12*EL*MH12*SA1*SA2*SA3*dAlpha3PinchPStar())/(MW*SB*SW) - (0.5625D0*CA22*EL*MH12*SA1*SA2*SA3*dAlpha3Pin&
  &chPStar())/(MW*SB*SW) + (0.5625D0*CA12*CA22*EL*MH12*SA1*SA2*SA3*dAlpha3PinchPStar())/(MW*SB*SW) - (0.09375D0*EL*MH32*SA1*SA2*S&
  &A3*dAlpha3PinchPStar())/(MW*SB*SW) + (0.09375D0*CA12*EL*MH32*SA1*SA2*SA3*dAlpha3PinchPStar())/(MW*SB*SW) - (0.28125D0*CA22*EL*&
  &MH32*SA1*SA2*SA3*dAlpha3PinchPStar())/(MW*SB*SW) + (0.28125D0*CA12*CA22*EL*MH32*SA1*SA2*SA3*dAlpha3PinchPStar())/(MW*SB*SW) - &
  &(0.28125D0*CA1*EL*m12squared*SA12*SA2*SA3*dAlpha3PinchPStar())/(CB2*MW*SB*SW) - (0.84375D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3&
  &*dAlpha3PinchPStar())/(CB2*MW*SB*SW) - (0.125D0*CA1*CA3*EL*m12squared*dAlpha3PinchPStar())/(CB*MW*SB2*SW) - (0.125D0*CA1*CA22*&
  &CA3*EL*m12squared*dAlpha3PinchPStar())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA3*EL*m12squared*SA12*dAlpha3PinchPStar())/(CB*MW*SB2*S&
  &W) - (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA12*dAlpha3PinchPStar())/(CB*MW*SB2*SW) + (0.125D0*CA1*CA3*EL*m12squared*SA22*dAlph&
  &a3PinchPStar())/(CB*MW*SB2*SW) + (0.5625D0*CA1*CA3*EL*m12squared*SA12*SA22*dAlpha3PinchPStar())/(CB*MW*SB2*SW) + (0.1875D0*EL*&
  &m12squared*SA1*SA2*SA3*dAlpha3PinchPStar())/(CB*MW*SB2*SW) - (0.28125D0*CA12*EL*m12squared*SA1*SA2*SA3*dAlpha3PinchPStar())/(C&
  &B*MW*SB2*SW) + (0.5625D0*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha3PinchPStar())/(CB*MW*SB2*SW) - (0.84375D0*CA12*CA22*EL*m12squar&
  &ed*SA1*SA2*SA3*dAlpha3PinchPStar())/(CB*MW*SB2*SW) - (0.0625D0*CA1*CA3*EL*m12squared*dAlpha3PinchPStar())/(MW*SB*SW*TB) - (0.0&
  &625D0*CA1*CA22*CA3*EL*m12squared*dAlpha3PinchPStar())/(MW*SB*SW*TB) + (0.0625D0*CA1*CA3*EL*m12squared*SA22*dAlpha3PinchPStar()&
  &)/(MW*SB*SW*TB) + (0.09375D0*EL*m12squared*SA1*SA2*SA3*dAlpha3PinchPStar())/(MW*SB*SW*TB) + (0.28125D0*CA22*EL*m12squared*SA1*&
  &SA2*SA3*dAlpha3PinchPStar())/(MW*SB*SW*TB) + (0.0625D0*CA3*EL*m12squared*SA1*TB*dAlpha3PinchPStar())/(CB*MW*SW) + (0.0625D0*CA&
  &22*CA3*EL*m12squared*SA1*TB*dAlpha3PinchPStar())/(CB*MW*SW) - (0.0625D0*CA3*EL*m12squared*SA1*SA22*TB*dAlpha3PinchPStar())/(CB&
  &*MW*SW) + (0.09375D0*CA1*EL*m12squared*SA2*SA3*TB*dAlpha3PinchPStar())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*m12squared*SA2*SA3*&
  &TB*dAlpha3PinchPStar())/(CB*MW*SW) + (0.5D0*CA2*MH12*SA3*dAlpha3PinchPStar())/vS + (0.25D0*CA2*MH32*SA3*dAlpha3PinchPStar())/v&
  &S + (1.5D0*CA2*MH12*SA22*SA3*dAlpha3PinchPStar())/vS + (0.75D0*CA2*MH32*SA22*SA3*dAlpha3PinchPStar())/vS + (0.09375D0*CA3*EL*M&
  &H12*SA1*SA2*dBeta2PinchPStar())/(CB*MW*SW) - (0.09375D0*CA12*CA3*EL*MH12*SA1*SA2*dBeta2PinchPStar())/(CB*MW*SW) + (0.28125D0*C&
  &A22*CA3*EL*MH12*SA1*SA2*dBeta2PinchPStar())/(CB*MW*SW) - (0.28125D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dBeta2PinchPStar())/(CB*MW*S&
  &W) + (0.046875D0*CA3*EL*MH32*SA1*SA2*dBeta2PinchPStar())/(CB*MW*SW) - (0.046875D0*CA12*CA3*EL*MH32*SA1*SA2*dBeta2PinchPStar())&
  &/(CB*MW*SW) + (0.140625D0*CA22*CA3*EL*MH32*SA1*SA2*dBeta2PinchPStar())/(CB*MW*SW) - (0.140625D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*&
  &dBeta2PinchPStar())/(CB*MW*SW) + (0.0625D0*CA1*EL*MH12*SA3*dBeta2PinchPStar())/(CB*MW*SW) + (0.0625D0*CA1*CA22*EL*MH12*SA3*dBe&
  &ta2PinchPStar())/(CB*MW*SW) + (0.03125D0*CA1*EL*MH32*SA3*dBeta2PinchPStar())/(CB*MW*SW) + (0.03125D0*CA1*CA22*EL*MH32*SA3*dBet&
  &a2PinchPStar())/(CB*MW*SW) + (0.1875D0*CA1*EL*MH12*SA12*SA3*dBeta2PinchPStar())/(CB*MW*SW) + (0.1875D0*CA1*CA22*EL*MH12*SA12*S&
  &A3*dBeta2PinchPStar())/(CB*MW*SW) + (0.09375D0*CA1*EL*MH32*SA12*SA3*dBeta2PinchPStar())/(CB*MW*SW) + (0.09375D0*CA1*CA22*EL*MH&
  &32*SA12*SA3*dBeta2PinchPStar())/(CB*MW*SW) - (0.0625D0*CA1*EL*MH12*SA22*SA3*dBeta2PinchPStar())/(CB*MW*SW) - (0.03125D0*CA1*EL&
  &*MH32*SA22*SA3*dBeta2PinchPStar())/(CB*MW*SW) - (0.1875D0*CA1*EL*MH12*SA12*SA22*SA3*dBeta2PinchPStar())/(CB*MW*SW) - (0.09375D&
  &0*CA1*EL*MH32*SA12*SA22*SA3*dBeta2PinchPStar())/(CB*MW*SW) + (0.09375D0*CA3*EL*m12squared*SA1*SA2*dBeta2PinchPStar())/(MW*SB*S&
  &W) + (0.28125D0*CA22*CA3*EL*m12squared*SA1*SA2*dBeta2PinchPStar())/(MW*SB*SW) - (0.328125D0*CA3*EL*m12squared*SA1*SA2*dBeta2Pi&
  &nchPStar())/(CB2*MW*SB*SW) + (0.421875D0*CA12*CA3*EL*m12squared*SA1*SA2*dBeta2PinchPStar())/(CB2*MW*SB*SW) - (0.984375D0*CA22*&
  &CA3*EL*m12squared*SA1*SA2*dBeta2PinchPStar())/(CB2*MW*SB*SW) + (1.265625D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*dBeta2PinchPSta&
  &r())/(CB2*MW*SB*SW) + (0.0625D0*CA1*EL*m12squared*SA3*dBeta2PinchPStar())/(MW*SB*SW) + (0.0625D0*CA1*CA22*EL*m12squared*SA3*dB&
  &eta2PinchPStar())/(MW*SB*SW) - (0.21875D0*CA1*EL*m12squared*SA3*dBeta2PinchPStar())/(CB2*MW*SB*SW) - (0.21875D0*CA1*CA22*EL*m1&
  &2squared*SA3*dBeta2PinchPStar())/(CB2*MW*SB*SW) - (0.84375D0*CA1*EL*m12squared*SA12*SA3*dBeta2PinchPStar())/(CB2*MW*SB*SW) - (&
  &0.84375D0*CA1*CA22*EL*m12squared*SA12*SA3*dBeta2PinchPStar())/(CB2*MW*SB*SW) - (0.0625D0*CA1*EL*m12squared*SA22*SA3*dBeta2Pinc&
  &hPStar())/(MW*SB*SW) + (0.21875D0*CA1*EL*m12squared*SA22*SA3*dBeta2PinchPStar())/(CB2*MW*SB*SW) + (0.84375D0*CA1*EL*m12squared&
  &*SA12*SA22*SA3*dBeta2PinchPStar())/(CB2*MW*SB*SW) + (0.09375D0*CA1*CA3*EL*m12squared*SA2*dBeta2PinchPStar())/(CB*MW*SB2*SW) + &
  &(0.28125D0*CA1*CA22*CA3*EL*m12squared*SA2*dBeta2PinchPStar())/(CB*MW*SB2*SW) - (0.09375D0*CA3*EL*MH12*SA1*SA2*dBeta2PinchPStar&
  &())/(CB*MW*SB2*SW) + (0.09375D0*CA12*CA3*EL*MH12*SA1*SA2*dBeta2PinchPStar())/(CB*MW*SB2*SW) - (0.28125D0*CA22*CA3*EL*MH12*SA1*&
  &SA2*dBeta2PinchPStar())/(CB*MW*SB2*SW) + (0.28125D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dBeta2PinchPStar())/(CB*MW*SB2*SW) - (0.0468&
  &75D0*CA3*EL*MH32*SA1*SA2*dBeta2PinchPStar())/(CB*MW*SB2*SW) + (0.046875D0*CA12*CA3*EL*MH32*SA1*SA2*dBeta2PinchPStar())/(CB*MW*&
  &SB2*SW) - (0.140625D0*CA22*CA3*EL*MH32*SA1*SA2*dBeta2PinchPStar())/(CB*MW*SB2*SW) + (0.140625D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*&
  &dBeta2PinchPStar())/(CB*MW*SB2*SW) - (0.28125D0*CA1*CA3*EL*m12squared*SA12*SA2*dBeta2PinchPStar())/(CB*MW*SB2*SW) - (0.84375D0&
  &*CA1*CA22*CA3*EL*m12squared*SA12*SA2*dBeta2PinchPStar())/(CB*MW*SB2*SW) - (0.0625D0*CA1*EL*MH12*SA3*dBeta2PinchPStar())/(CB*MW&
  &*SB2*SW) - (0.0625D0*CA1*CA22*EL*MH12*SA3*dBeta2PinchPStar())/(CB*MW*SB2*SW) - (0.03125D0*CA1*EL*MH32*SA3*dBeta2PinchPStar())/&
  &(CB*MW*SB2*SW) - (0.03125D0*CA1*CA22*EL*MH32*SA3*dBeta2PinchPStar())/(CB*MW*SB2*SW) - (0.0625D0*EL*m12squared*SA1*SA3*dBeta2Pi&
  &nchPStar())/(CB*MW*SB2*SW) - (0.5625D0*CA12*EL*m12squared*SA1*SA3*dBeta2PinchPStar())/(CB*MW*SB2*SW) - (0.0625D0*CA22*EL*m12sq&
  &uared*SA1*SA3*dBeta2PinchPStar())/(CB*MW*SB2*SW) - (0.5625D0*CA12*CA22*EL*m12squared*SA1*SA3*dBeta2PinchPStar())/(CB*MW*SB2*SW&
  &) - (0.1875D0*CA1*EL*MH12*SA12*SA3*dBeta2PinchPStar())/(CB*MW*SB2*SW) - (0.1875D0*CA1*CA22*EL*MH12*SA12*SA3*dBeta2PinchPStar()&
  &)/(CB*MW*SB2*SW) - (0.09375D0*CA1*EL*MH32*SA12*SA3*dBeta2PinchPStar())/(CB*MW*SB2*SW) - (0.09375D0*CA1*CA22*EL*MH32*SA12*SA3*d&
  &Beta2PinchPStar())/(CB*MW*SB2*SW) + (0.0625D0*CA1*EL*MH12*SA22*SA3*dBeta2PinchPStar())/(CB*MW*SB2*SW) + (0.03125D0*CA1*EL*MH32&
  &*SA22*SA3*dBeta2PinchPStar())/(CB*MW*SB2*SW) + (0.0625D0*EL*m12squared*SA1*SA22*SA3*dBeta2PinchPStar())/(CB*MW*SB2*SW) + (0.56&
  &25D0*CA12*EL*m12squared*SA1*SA22*SA3*dBeta2PinchPStar())/(CB*MW*SB2*SW) + (0.1875D0*CA1*EL*MH12*SA12*SA22*SA3*dBeta2PinchPStar&
  &())/(CB*MW*SB2*SW) + (0.09375D0*CA1*EL*MH32*SA12*SA22*SA3*dBeta2PinchPStar())/(CB*MW*SB2*SW) - (0.1875D0*CA1*CA3*EL*m12squared&
  &*SA2*dBeta2PinchPStar())/(MW*SB*SW*TB) - (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA2*dBeta2PinchPStar())/(MW*SB*SW*TB) - (0.09375&
  &D0*CA3*EL*MH12*SA1*SA2*dBeta2PinchPStar())/(MW*SB*SW*TB) + (0.09375D0*CA12*CA3*EL*MH12*SA1*SA2*dBeta2PinchPStar())/(MW*SB*SW*T&
  &B) - (0.28125D0*CA22*CA3*EL*MH12*SA1*SA2*dBeta2PinchPStar())/(MW*SB*SW*TB) + (0.28125D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dBeta2Pi&
  &nchPStar())/(MW*SB*SW*TB) - (0.046875D0*CA3*EL*MH32*SA1*SA2*dBeta2PinchPStar())/(MW*SB*SW*TB) + (0.046875D0*CA12*CA3*EL*MH32*S&
  &A1*SA2*dBeta2PinchPStar())/(MW*SB*SW*TB) - (0.140625D0*CA22*CA3*EL*MH32*SA1*SA2*dBeta2PinchPStar())/(MW*SB*SW*TB) + (0.140625D&
  &0*CA12*CA22*CA3*EL*MH32*SA1*SA2*dBeta2PinchPStar())/(MW*SB*SW*TB) - (0.0625D0*CA1*EL*MH12*SA3*dBeta2PinchPStar())/(MW*SB*SW*TB&
  &) - (0.0625D0*CA1*CA22*EL*MH12*SA3*dBeta2PinchPStar())/(MW*SB*SW*TB) - (0.03125D0*CA1*EL*MH32*SA3*dBeta2PinchPStar())/(MW*SB*S&
  &W*TB) - (0.03125D0*CA1*CA22*EL*MH32*SA3*dBeta2PinchPStar())/(MW*SB*SW*TB) + (0.125D0*EL*m12squared*SA1*SA3*dBeta2PinchPStar())&
  &/(MW*SB*SW*TB) + (0.125D0*CA22*EL*m12squared*SA1*SA3*dBeta2PinchPStar())/(MW*SB*SW*TB) - (0.1875D0*CA1*EL*MH12*SA12*SA3*dBeta2&
  &PinchPStar())/(MW*SB*SW*TB) - (0.1875D0*CA1*CA22*EL*MH12*SA12*SA3*dBeta2PinchPStar())/(MW*SB*SW*TB) - (0.09375D0*CA1*EL*MH32*S&
  &A12*SA3*dBeta2PinchPStar())/(MW*SB*SW*TB) - (0.09375D0*CA1*CA22*EL*MH32*SA12*SA3*dBeta2PinchPStar())/(MW*SB*SW*TB) + (0.0625D0&
  &*CA1*EL*MH12*SA22*SA3*dBeta2PinchPStar())/(MW*SB*SW*TB) + (0.03125D0*CA1*EL*MH32*SA22*SA3*dBeta2PinchPStar())/(MW*SB*SW*TB) - &
  &(0.125D0*EL*m12squared*SA1*SA22*SA3*dBeta2PinchPStar())/(MW*SB*SW*TB) + (0.1875D0*CA1*EL*MH12*SA12*SA22*SA3*dBeta2PinchPStar()&
  &)/(MW*SB*SW*TB) + (0.09375D0*CA1*EL*MH32*SA12*SA22*SA3*dBeta2PinchPStar())/(MW*SB*SW*TB) + (0.1875D0*CA1*CA3*EL*MH12*SA2*TB*dB&
  &eta2PinchPStar())/(CB*MW*SW) + (0.5625D0*CA1*CA22*CA3*EL*MH12*SA2*TB*dBeta2PinchPStar())/(CB*MW*SW) + (0.09375D0*CA1*CA3*EL*MH&
  &32*SA2*TB*dBeta2PinchPStar())/(CB*MW*SW) + (0.28125D0*CA1*CA22*CA3*EL*MH32*SA2*TB*dBeta2PinchPStar())/(CB*MW*SW) + (0.328125D0&
  &*CA3*EL*m12squared*SA1*SA2*TB*dBeta2PinchPStar())/(CB*MW*SW) + (0.984375D0*CA22*CA3*EL*m12squared*SA1*SA2*TB*dBeta2PinchPStar(&
  &))/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*MH12*SA12*SA2*TB*dBeta2PinchPStar())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*EL*MH12*SA12*SA2&
  &*TB*dBeta2PinchPStar())/(CB*MW*SW) - (0.09375D0*CA1*CA3*EL*MH32*SA12*SA2*TB*dBeta2PinchPStar())/(CB*MW*SW) - (0.28125D0*CA1*CA&
  &22*CA3*EL*MH32*SA12*SA2*TB*dBeta2PinchPStar())/(CB*MW*SW) + (0.21875D0*CA1*EL*m12squared*SA3*TB*dBeta2PinchPStar())/(CB*MW*SW)&
  & + (0.21875D0*CA1*CA22*EL*m12squared*SA3*TB*dBeta2PinchPStar())/(CB*MW*SW) - (0.125D0*EL*MH12*SA1*SA3*TB*dBeta2PinchPStar())/(&
  &CB*MW*SW) - (0.375D0*CA12*EL*MH12*SA1*SA3*TB*dBeta2PinchPStar())/(CB*MW*SW) - (0.125D0*CA22*EL*MH12*SA1*SA3*TB*dBeta2PinchPSta&
  &r())/(CB*MW*SW) - (0.375D0*CA12*CA22*EL*MH12*SA1*SA3*TB*dBeta2PinchPStar())/(CB*MW*SW) - (0.0625D0*EL*MH32*SA1*SA3*TB*dBeta2Pi&
  &nchPStar())/(CB*MW*SW) - (0.1875D0*CA12*EL*MH32*SA1*SA3*TB*dBeta2PinchPStar())/(CB*MW*SW) - (0.0625D0*CA22*EL*MH32*SA1*SA3*TB*&
  &dBeta2PinchPStar())/(CB*MW*SW) - (0.1875D0*CA12*CA22*EL*MH32*SA1*SA3*TB*dBeta2PinchPStar())/(CB*MW*SW) - (0.21875D0*CA1*EL*m12&
  &squared*SA22*SA3*TB*dBeta2PinchPStar())/(CB*MW*SW) + (0.125D0*EL*MH12*SA1*SA22*SA3*TB*dBeta2PinchPStar())/(CB*MW*SW) + (0.375D&
  &0*CA12*EL*MH12*SA1*SA22*SA3*TB*dBeta2PinchPStar())/(CB*MW*SW) + (0.0625D0*EL*MH32*SA1*SA22*SA3*TB*dBeta2PinchPStar())/(CB*MW*S&
  &W) + (0.1875D0*CA12*EL*MH32*SA1*SA22*SA3*TB*dBeta2PinchPStar())/(CB*MW*SW) + (0.140625D0*CA3*EL*m12squared*SA1*SA2*dBeta2Pinch&
  &PStar())/(MW*SB*SW*TB2) + (0.421875D0*CA22*CA3*EL*m12squared*SA1*SA2*dBeta2PinchPStar())/(MW*SB*SW*TB2) + (0.09375D0*CA1*EL*m1&
  &2squared*SA3*dBeta2PinchPStar())/(MW*SB*SW*TB2) + (0.09375D0*CA1*CA22*EL*m12squared*SA3*dBeta2PinchPStar())/(MW*SB*SW*TB2) - (&
  &0.09375D0*CA1*EL*m12squared*SA22*SA3*dBeta2PinchPStar())/(MW*SB*SW*TB2) - (0.1875D0*CA1*CA3*EL*m12squared*SA2*TB2*dBeta2PinchP&
  &Star())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA2*TB2*dBeta2PinchPStar())/(CB*MW*SW) + (0.125D0*EL*m12squared*SA1*&
  &SA3*TB2*dBeta2PinchPStar())/(CB*MW*SW) + (0.125D0*CA22*EL*m12squared*SA1*SA3*TB2*dBeta2PinchPStar())/(CB*MW*SW) - (0.125D0*EL*&
  &m12squared*SA1*SA22*SA3*TB2*dBeta2PinchPStar())/(CB*MW*SW) + (0.125D0*CA2*CA3*MH12*dBeta2PinchPStar())/(CB*SB*vS) + (0.0625D0*&
  &CA2*CA3*MH32*dBeta2PinchPStar())/(CB*SB*vS) + (0.375D0*CA2*CA3*MH12*SA22*dBeta2PinchPStar())/(CB*SB*vS) + (0.1875D0*CA2*CA3*MH&
  &32*SA22*dBeta2PinchPStar())/(CB*SB*vS) - (0.125D0*CA2*CA3*MH12*dBeta2PinchPStar())/(TB*vS) - (0.0625D0*CA2*CA3*MH32*dBeta2Pinc&
  &hPStar())/(TB*vS) - (0.375D0*CA2*CA3*MH12*SA22*dBeta2PinchPStar())/(TB*vS) - (0.1875D0*CA2*CA3*MH32*SA22*dBeta2PinchPStar())/(&
  &TB*vS) - (0.125D0*CA2*CA3*MH12*TB*dBeta2PinchPStar())/vS - (0.0625D0*CA2*CA3*MH32*TB*dBeta2PinchPStar())/vS - (0.375D0*CA2*CA3&
  &*MH12*SA22*TB*dBeta2PinchPStar())/vS - (0.1875D0*CA2*CA3*MH32*SA22*TB*dBeta2PinchPStar())/vS - (0.375D0*EL*MH12*SA3*dAlpha1Pin&
  &chPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.375D0*CA22*EL*MH12*SA3*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - &
  &(0.1875D0*EL*MH32*SA3*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA22*EL*MH32*SA3*dAlpha1PinchPStar()*DB&
  &LE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.375D0*EL*MH12*SA22*SA3*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*EL&
  &*MH32*SA22*SA3*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA3*EL*MH12*SA2*dAlpha1PinchPStar()*DBLE(CA1**&
  &INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA22*CA3*EL*MH12*SA2*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA3*&
  &EL*MH32*SA2*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA22*CA3*EL*MH32*SA2*dAlpha1PinchPStar()*DBLE(CA&
  &1**INT(3.D0)))/(MW*SB*SW) + (0.5625D0*EL*m12squared*SA3*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.5625D0*C&
  &A22*EL*m12squared*SA3*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*EL*m12squared*SA22*SA3*dAlpha1Pinch&
  &PStar()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.28125D0*CA3*EL*m12squared*SA2*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*&
  &MW*SB2*SW) + (0.84375D0*CA22*CA3*EL*m12squared*SA2*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*CA2*CA&
  &3*EL*MH12*dAlpha2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA2*CA3*EL*MH32*dAlpha2PinchPStar()*DBLE(CA1**INT(&
  &3.D0)))/(CB*MW*SW) - (0.5625D0*CA2*CA3*EL*MH12*SA22*dAlpha2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA2*CA3*&
  &EL*MH32*SA22*dAlpha2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA2*CA3*EL*m12squared*dAlpha2PinchPStar()*DBLE(&
  &CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.84375D0*CA2*CA3*EL*m12squared*SA22*dAlpha2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*S&
  &W) + (0.5D0*CA2*EL*MH12*SA2*SA3*dAlpha2PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.25D0*CA2*EL*MH32*SA2*SA3*dAlpha2Pinc&
  &hPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.75D0*CA2*EL*m12squared*SA2*SA3*dAlpha2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW&
  &*SB2*SW) - (0.0625D0*EL*MH12*SA2*SA3*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA22*EL*MH12*SA2*SA3*dAl&
  &pha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*EL*MH32*SA2*SA3*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*M&
  &W*SW) - (0.09375D0*CA22*EL*MH32*SA2*SA3*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.125D0*CA3*EL*MH12*dAlpha3Pin&
  &chPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.125D0*CA22*CA3*EL*MH12*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - &
  &(0.0625D0*CA3*EL*MH32*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.0625D0*CA22*CA3*EL*MH32*dAlpha3PinchPStar()*DB&
  &LE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.125D0*CA3*EL*MH12*SA22*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*CA&
  &3*EL*MH32*SA22*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*EL*m12squared*SA2*SA3*dAlpha3PinchPStar()*DBL&
  &E(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.28125D0*CA22*EL*m12squared*SA2*SA3*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB&
  &*SW) + (0.1875D0*CA3*EL*m12squared*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*CA22*CA3*EL*m12squared&
  &*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.1875D0*CA3*EL*m12squared*SA22*dAlpha3PinchPStar()*DBLE(CA1**INT&
  &(3.D0)))/(CB*MW*SB2*SW) - (0.0625D0*EL*MH12*SA3*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.0625D0*CA22*EL*MH12*S&
  &A3*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*EL*MH32*SA3*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*M&
  &W*SW) - (0.03125D0*CA22*EL*MH32*SA3*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*EL*MH12*SA22*SA3*dBeta2Pin&
  &chPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*EL*MH32*SA22*SA3*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) +&
  & (0.28125D0*EL*m12squared*SA3*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.28125D0*CA22*EL*m12squared*SA3*dBet&
  &a2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*EL*m12squared*SA22*SA3*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0)&
  &))/(CB2*MW*SB*SW) + (0.09375D0*CA3*EL*m12squared*SA2*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA22&
  &*CA3*EL*m12squared*SA2*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*EL*MH12*SA3*dBeta2PinchPStar()*DBLE&
  &(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*CA22*EL*MH12*SA3*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.031&
  &25D0*EL*MH32*SA3*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.03125D0*CA22*EL*MH32*SA3*dBeta2PinchPStar()*DBLE&
  &(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.0625D0*EL*MH12*SA22*SA3*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.031&
  &25D0*EL*MH32*SA22*SA3*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*EL*MH12*SA3*dBeta2PinchPStar()*DBLE(&
  &CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.0625D0*CA22*EL*MH12*SA3*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.03125D&
  &0*EL*MH32*SA3*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.03125D0*CA22*EL*MH32*SA3*dBeta2PinchPStar()*DBLE(CA1&
  &**INT(3.D0)))/(MW*SB*SW*TB) - (0.0625D0*EL*MH12*SA22*SA3*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.03125D0*E&
  &L*MH32*SA22*SA3*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.0625D0*CA3*EL*MH12*SA2*TB*dBeta2PinchPStar()*DBLE(&
  &CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA22*CA3*EL*MH12*SA2*TB*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125&
  &D0*CA3*EL*MH32*SA2*TB*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA22*CA3*EL*MH32*SA2*TB*dBeta2PinchPSta&
  &r()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA1*CA3*EL*MH12*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.281&
  &25D0*CA1*CA3*EL*MH32*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.84375D0*CA3*EL*m12squared*SA1*dAlpha2PinchPStar&
  &()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.5625D0*CA1*CA3*EL*MH12*SA12*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0&
  &.28125D0*CA1*CA3*EL*MH32*SA12*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.84375D0*CA1*CA3*EL*m12squared*dAlpha2P&
  &inchPStar()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA1*CA3*EL*m12squared*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(CB2*&
  &MW*SB*SW) + (0.5625D0*CA3*EL*MH12*SA1*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA12*CA3*EL*MH12*SA1*dA&
  &lpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*CA3*EL*MH32*SA1*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(MW*&
  &SB*SW) - (0.28125D0*CA12*CA3*EL*MH32*SA1*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) + (0.84375D0*CA1*CA3*EL*m12squar&
  &ed*SA12*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA3*EL*m12squared*SA1*dAlpha2PinchPStar()*DBLE(CA&
  &2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA3*EL*m12squared*SA1*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(CB*MW*SB2*SW)&
  & - (0.28125D0*CA3*EL*m12squared*SA1*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.28125D0*CA1*CA3*EL*m12squared&
  &*TB*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.5D0*MH12*SA3*dAlpha3PinchPStar()*DBLE(CA2**INT(3.D0)))/vS - (0.2&
  &5D0*MH32*SA3*dAlpha3PinchPStar()*DBLE(CA2**INT(3.D0)))/vS - (0.125D0*CA3*MH12*dBeta2PinchPStar()*DBLE(CA2**INT(3.D0)))/(CB*SB*&
  &vS) - (0.0625D0*CA3*MH32*dBeta2PinchPStar()*DBLE(CA2**INT(3.D0)))/(CB*SB*vS) + (0.125D0*CA3*MH12*dBeta2PinchPStar()*DBLE(CA2**&
  &INT(3.D0)))/(TB*vS) + (0.0625D0*CA3*MH32*dBeta2PinchPStar()*DBLE(CA2**INT(3.D0)))/(TB*vS) + (0.125D0*CA3*MH12*TB*dBeta2PinchPS&
  &tar()*DBLE(CA2**INT(3.D0)))/vS + (0.0625D0*CA3*MH32*TB*dBeta2PinchPStar()*DBLE(CA2**INT(3.D0)))/vS + (0.1875D0*CA3*EL*MH12*dAl&
  &pha2PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA3*EL*MH32*dAlpha2PinchPStar()*DBLE(CA1**&
  &INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA3*EL*m12squared*dAlpha2PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(CA2**&
  &INT(3.D0)))/(CB2*MW*SB*SW) - (0.375D0*CA1*CA3*EL*m12squared*SA2*dBeta2PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (1.125D0*CA&
  &1*CA22*CA3*EL*m12squared*SA2*dBeta2PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.5625D0*CA1*CA3*EL*m12squared*SA12*SA2*dBeta&
  &2PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (1.6875D0*CA1*CA22*CA3*EL*m12squared*SA12*SA2*dBeta2PinchPStar()*DBLE(CB**INT(-3&
  &.D0)))/(MW*SW) + (0.25D0*EL*m12squared*SA1*SA3*dBeta2PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (1.125D0*CA12*EL*m12squared*&
  &SA1*SA3*dBeta2PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.25D0*CA22*EL*m12squared*SA1*SA3*dBeta2PinchPStar()*DBLE(CB**INT(&
  &-3.D0)))/(MW*SW) + (1.125D0*CA12*CA22*EL*m12squared*SA1*SA3*dBeta2PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.25D0*EL*m12s&
  &quared*SA1*SA22*SA3*dBeta2PinchPStar()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (1.125D0*CA12*EL*m12squared*SA1*SA22*SA3*dBeta2PinchPSt&
  &ar()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.1875D0*CA3*EL*m12squared*SA2*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D&
  &0)))/(MW*SW) - (0.5625D0*CA22*CA3*EL*m12squared*SA2*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0&
  &.1875D0*CA3*EL*MH12*SA2*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA22*CA3*EL*MH12*SA2*dAlpha1PinchPSta&
  &r()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA3*EL*MH32*SA2*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.28&
  &125D0*CA22*CA3*EL*MH32*SA2*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA3*EL*m12squared*SA2*dAlpha1Pinc&
  &hPStar()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.84375D0*CA22*CA3*EL*m12squared*SA2*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0))&
  &)/(CB2*MW*SB*SW) - (0.375D0*EL*MH12*SA3*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.375D0*CA22*EL*MH12*SA3*dAlph&
  &a1PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*EL*MH32*SA3*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) -&
  & (0.1875D0*CA22*EL*MH32*SA3*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.375D0*EL*MH12*SA22*SA3*dAlpha1PinchPStar&
  &()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*EL*MH32*SA22*SA3*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.562&
  &5D0*EL*m12squared*SA3*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.5625D0*CA22*EL*m12squared*SA3*dAlpha1Pinch&
  &PStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.5625D0*EL*m12squared*SA22*SA3*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*&
  &MW*SB2*SW) - (0.5D0*CA2*EL*MH12*SA2*SA3*dAlpha2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.25D0*CA2*EL*MH32*SA2*SA3*dAl&
  &pha2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA2*CA3*EL*MH12*dAlpha2PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB&
  &*SW) + (0.03125D0*CA2*CA3*EL*MH32*dAlpha2PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA2*CA3*EL*MH12*SA22*dAlpha&
  &2PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA2*CA3*EL*MH32*SA22*dAlpha2PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW&
  &*SB*SW) + (0.75D0*CA2*EL*m12squared*SA2*SA3*dAlpha2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA2*CA3*EL*m&
  &12squared*dAlpha2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA2*CA3*EL*m12squared*SA22*dAlpha2PinchPStar()&
  &*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.125D0*CA3*EL*MH12*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.125D0*C&
  &A22*CA3*EL*MH12*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA3*EL*MH32*dAlpha3PinchPStar()*DBLE(SA1**INT&
  &(3.D0)))/(CB*MW*SW) + (0.0625D0*CA22*CA3*EL*MH32*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.125D0*CA3*EL*MH12*S&
  &A22*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.0625D0*CA3*EL*MH32*SA22*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0))&
  &)/(CB*MW*SW) - (0.1875D0*CA3*EL*m12squared*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*CA22*CA3*EL*m1&
  &2squared*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA22*dAlpha3PinchPStar()*DBLE(&
  &SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D0*EL*MH12*SA2*SA3*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*C&
  &A22*EL*MH12*SA2*SA3*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.03125D0*EL*MH32*SA2*SA3*dAlpha3PinchPStar()*DBLE&
  &(SA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA22*EL*MH32*SA2*SA3*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.09375&
  &D0*EL*m12squared*SA2*SA3*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA22*EL*m12squared*SA2*SA3*dAlp&
  &ha3PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.03125D0*CA3*EL*MH12*SA2*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB&
  &*MW*SW) + (0.09375D0*CA22*CA3*EL*MH12*SA2*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.015625D0*CA3*EL*MH32*SA2*dB&
  &eta2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.046875D0*CA22*CA3*EL*MH32*SA2*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0)))/&
  &(CB*MW*SW) - (0.140625D0*CA3*EL*m12squared*SA2*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.421875D0*CA22*CA3*&
  &EL*m12squared*SA2*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.03125D0*CA3*EL*MH12*SA2*dBeta2PinchPStar()*DBLE&
  &(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.09375D0*CA22*CA3*EL*MH12*SA2*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (&
  &0.015625D0*CA3*EL*MH32*SA2*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.046875D0*CA22*CA3*EL*MH32*SA2*dBeta2Pi&
  &nchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*EL*m12squared*SA3*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*&
  &SB2*SW) + (0.1875D0*CA22*EL*m12squared*SA3*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.1875D0*EL*m12squared*S&
  &A22*SA3*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.03125D0*CA3*EL*MH12*SA2*dBeta2PinchPStar()*DBLE(SA1**INT(&
  &3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA22*CA3*EL*MH12*SA2*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.015625D0*C&
  &A3*EL*MH32*SA2*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.046875D0*CA22*CA3*EL*MH32*SA2*dBeta2PinchPStar()*DB&
  &LE(SA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.125D0*EL*MH12*SA3*TB*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.125D0*CA2&
  &2*EL*MH12*SA3*TB*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*EL*MH32*SA3*TB*dBeta2PinchPStar()*DBLE(SA1**I&
  &NT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA22*EL*MH32*SA3*TB*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.125D0*EL*MH12*S&
  &A22*SA3*TB*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.0625D0*EL*MH32*SA22*SA3*TB*dBeta2PinchPStar()*DBLE(SA1**IN&
  &T(3.D0)))/(CB*MW*SW) + (0.1875D0*CA3*EL*MH12*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.09&
  &375D0*CA3*EL*MH32*dAlpha2PinchPStar()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*dAl&
  &pha2PinchPStar()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.375D0*EL*m12squared*SA3*dBeta2PinchPStar()*DBL&
  &E(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) - (0.375D0*CA22*EL*m12squared*SA3*dBeta2PinchPStar()*DBLE(CB**INT(-3.D0))*DBLE&
  &(SA1**INT(3.D0)))/(MW*SW) + (0.375D0*EL*m12squared*SA22*SA3*dBeta2PinchPStar()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*&
  &SW) - (0.28125D0*CA1*CA3*EL*m12squared*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA3*EL*MH12*SA1*dAlpha&
  &1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA12*CA3*EL*MH12*SA1*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*&
  &MW*SW) + (0.09375D0*CA3*EL*MH32*SA1*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA12*CA3*EL*MH32*SA1*dAl&
  &pha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*MH12*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB&
  &*SW) - (0.09375D0*CA1*CA3*EL*MH32*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*CA3*EL*m12squared*SA1*dAlp&
  &ha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA3*EL*m12squared*SA1*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(&
  &CB2*MW*SB*SW) - (0.84375D0*CA12*CA3*EL*m12squared*SA1*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA1&
  &*CA3*EL*MH12*SA12*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA1*CA3*EL*MH32*SA12*dAlpha1PinchPStar()*D&
  &BLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*m12squared*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (&
  &0.84375D0*CA1*CA3*EL*m12squared*SA12*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.09375D0*CA1*CA3*EL*m12squar&
  &ed*dAlpha1PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA3*EL*m12squared*SA1*TB*dAlpha1PinchPStar()*DBLE(SA2*&
  &*INT(3.D0)))/(CB*MW*SW) + (1.5D0*CA3*MH12*dAlpha2PinchPStar()*DBLE(SA2**INT(3.D0)))/vS + (0.75D0*CA3*MH32*dAlpha2PinchPStar()*&
  &DBLE(SA2**INT(3.D0)))/vS + (0.1875D0*CA1*EL*MH12*SA3*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1*EL*&
  &MH32*SA3*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*EL*m12squared*SA1*SA3*dAlpha3PinchPStar()*DBLE(SA2*&
  &*INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA1*EL*MH12*SA12*SA3*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1&
  &*EL*MH32*SA12*SA3*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA1*EL*m12squared*SA3*dAlpha3PinchPStar()*&
  &DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA1*EL*m12squared*SA3*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + &
  &(0.1875D0*EL*MH12*SA1*SA3*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA12*EL*MH12*SA1*SA3*dAlpha3PinchPS&
  &tar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*EL*MH32*SA1*SA3*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.&
  &09375D0*CA12*EL*MH32*SA1*SA3*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*CA1*EL*m12squared*SA12*SA3*dAlp&
  &ha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*EL*m12squared*SA1*SA3*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)&
  &))/(CB*MW*SB2*SW) + (0.28125D0*CA12*EL*m12squared*SA1*SA3*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.09375D&
  &0*EL*m12squared*SA1*SA3*dAlpha3PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA1*EL*m12squared*SA3*TB*dAlpha3P&
  &inchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA3*EL*MH12*SA1*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) &
  &+ (0.09375D0*CA12*CA3*EL*MH12*SA1*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.046875D0*CA3*EL*MH32*SA1*dBeta2Pinc&
  &hPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.046875D0*CA12*CA3*EL*MH32*SA1*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*S&
  &W) - (0.09375D0*CA3*EL*m12squared*SA1*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.328125D0*CA3*EL*m12squared*SA1*&
  &dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.421875D0*CA12*CA3*EL*m12squared*SA1*dBeta2PinchPStar()*DBLE(SA2**&
  &INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA1*CA3*EL*m12squared*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.093&
  &75D0*CA3*EL*MH12*SA1*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.09375D0*CA12*CA3*EL*MH12*SA1*dBeta2PinchPSta&
  &r()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.046875D0*CA3*EL*MH32*SA1*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW)&
  & - (0.046875D0*CA12*CA3*EL*MH32*SA1*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA1*CA3*EL*m12squared&
  &*SA12*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*CA1*CA3*EL*m12squared*dBeta2PinchPStar()*DBLE(SA2**I&
  &NT(3.D0)))/(MW*SB*SW*TB) + (0.09375D0*CA3*EL*MH12*SA1*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA12&
  &*CA3*EL*MH12*SA1*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.046875D0*CA3*EL*MH32*SA1*dBeta2PinchPStar()*DBLE(&
  &SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.046875D0*CA12*CA3*EL*MH32*SA1*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.&
  &1875D0*CA1*CA3*EL*MH12*TB*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1*CA3*EL*MH32*TB*dBeta2PinchPStar&
  &()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.328125D0*CA3*EL*m12squared*SA1*TB*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW)&
  & + (0.1875D0*CA1*CA3*EL*MH12*SA12*TB*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1*CA3*EL*MH32*SA12*TB*&
  &dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.140625D0*CA3*EL*m12squared*SA1*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)&
  &))/(MW*SB*SW*TB2) + (0.1875D0*CA1*CA3*EL*m12squared*TB2*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA3*EL&
  &*MH12*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA3*EL*MH32*dAlpha1PinchPStar()*D&
  &BLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*dAlpha1PinchPStar()*DBLE(CA1**INT(3.D0))*D&
  &BLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*EL*MH12*SA3*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB&
  &*MW*SW) + (0.03125D0*EL*MH32*SA3*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*EL*m12&
  &squared*SA3*dAlpha3PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA3*EL*m12squared*dBeta&
  &2PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.0625D0*CA3*EL*MH12*TB*dBeta2PinchPStar()*DBLE(CA1&
  &**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*CA3*EL*MH32*TB*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SA2**IN&
  &T(3.D0)))/(CB*MW*SW) + (0.375D0*CA1*CA3*EL*m12squared*dBeta2PinchPStar()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) - &
  &(0.5625D0*CA1*CA3*EL*m12squared*SA12*dBeta2PinchPStar()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) + (0.1875D0*CA3*EL*&
  &m12squared*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) - (0.1875D0*CA3*EL*MH12*&
  &dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA3*EL*MH32*dAlpha1PinchPStar()*DBLE(SA&
  &1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA3*EL*m12squared*dAlpha1PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SA&
  &2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.0625D0*EL*MH12*SA3*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW&
  &) + (0.03125D0*EL*MH32*SA3*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*EL*m12square&
  &d*SA3*dAlpha3PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.03125D0*CA3*EL*MH12*dBeta2PinchPStar(&
  &)*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.015625D0*CA3*EL*MH32*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE&
  &(SA2**INT(3.D0)))/(CB*MW*SW) + (0.140625D0*CA3*EL*m12squared*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB&
  &2*MW*SB*SW) + (0.03125D0*CA3*EL*MH12*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.015625D&
  &0*CA3*EL*MH32*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.03125D0*CA3*EL*MH12*dBeta2Pinc&
  &hPStar()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.015625D0*CA3*EL*MH32*dBeta2PinchPStar()*DBLE(SA1**INT(3&
  &.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.328125D0*CA3*EL*m12squared*SA1*SA2*dBeta2PinchPStar()*DBLE(SB**INT(-3.D0)))/(MW&
  &*SW) - (0.421875D0*CA12*CA3*EL*m12squared*SA1*SA2*dBeta2PinchPStar()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.984375D0*CA22*CA3*EL*m&
  &12squared*SA1*SA2*dBeta2PinchPStar()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (1.265625D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*dBeta2Pin&
  &chPStar()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.09375D0*CA3*EL*m12squared*SA1*SA2*dBeta2PinchPStar()*DBLE(SB**INT(-3.D0)))/(CB2*M&
  &W*SW) - (0.140625D0*CA12*CA3*EL*m12squared*SA1*SA2*dBeta2PinchPStar()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.28125D0*CA22*CA3*&
  &EL*m12squared*SA1*SA2*dBeta2PinchPStar()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.421875D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*d&
  &Beta2PinchPStar()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.21875D0*CA1*EL*m12squared*SA3*dBeta2PinchPStar()*DBLE(SB**INT(-3.D0))&
  &)/(MW*SW) + (0.21875D0*CA1*CA22*EL*m12squared*SA3*dBeta2PinchPStar()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.0625D0*CA1*EL*m12squar&
  &ed*SA3*dBeta2PinchPStar()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.0625D0*CA1*CA22*EL*m12squared*SA3*dBeta2PinchPStar()*DBLE(SB*&
  &*INT(-3.D0)))/(CB2*MW*SW) + (0.84375D0*CA1*EL*m12squared*SA12*SA3*dBeta2PinchPStar()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.84375D&
  &0*CA1*CA22*EL*m12squared*SA12*SA3*dBeta2PinchPStar()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.28125D0*CA1*EL*m12squared*SA12*SA3*dBe&
  &ta2PinchPStar()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.28125D0*CA1*CA22*EL*m12squared*SA12*SA3*dBeta2PinchPStar()*DBLE(SB**INT&
  &(-3.D0)))/(CB2*MW*SW) - (0.21875D0*CA1*EL*m12squared*SA22*SA3*dBeta2PinchPStar()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.0625D0*CA1&
  &*EL*m12squared*SA22*SA3*dBeta2PinchPStar()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.84375D0*CA1*EL*m12squared*SA12*SA22*SA3*dBet&
  &a2PinchPStar()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.28125D0*CA1*EL*m12squared*SA12*SA22*SA3*dBeta2PinchPStar()*DBLE(SB**INT(-3.D&
  &0)))/(CB2*MW*SW) - (0.28125D0*EL*m12squared*SA3*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.281&
  &25D0*CA22*EL*m12squared*SA3*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.09375D0*EL*m12squared*S&
  &A3*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.09375D0*CA22*EL*m12squared*SA3*dBeta2PinchPS&
  &tar()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.28125D0*EL*m12squared*SA22*SA3*dBeta2PinchPStar()*DBLE(CA1**&
  &INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.09375D0*EL*m12squared*SA22*SA3*dBeta2PinchPStar()*DBLE(CA1**INT(3.D0))*DBLE(SB**&
  &INT(-3.D0)))/(CB2*MW*SW) + (0.140625D0*CA3*EL*m12squared*SA2*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW&
  &*SW) + (0.421875D0*CA22*CA3*EL*m12squared*SA2*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.04687&
  &5D0*CA3*EL*m12squared*SA2*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.140625D0*CA22*CA3*EL*&
  &m12squared*SA2*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.328125D0*CA3*EL*m12squared*SA1*d&
  &Beta2PinchPStar()*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.421875D0*CA12*CA3*EL*m12squared*SA1*dBeta2PinchPStar&
  &()*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.09375D0*CA3*EL*m12squared*SA1*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0&
  &))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.140625D0*CA12*CA3*EL*m12squared*SA1*dBeta2PinchPStar()*DBLE(SA2**INT(3.D0))*DBLE(SB*&
  &*INT(-3.D0)))/(CB2*MW*SW) - (0.140625D0*CA3*EL*m12squared*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*DBLE(SB&
  &**INT(-3.D0)))/(MW*SW) - (0.046875D0*CA3*EL*m12squared*dBeta2PinchPStar()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*DBLE(SB**I&
  &NT(-3.D0)))/(CB2*MW*SW) + (0.1875D0*CA1*CA3*MH12*SA2*dgAtMZ())/(CB*MW) + (0.5625D0*CA1*CA22*CA3*MH12*SA2*dgAtMZ())/(CB*MW) + (&
  &0.09375D0*CA1*CA3*MH32*SA2*dgAtMZ())/(CB*MW) + (0.28125D0*CA1*CA22*CA3*MH32*SA2*dgAtMZ())/(CB*MW) + (0.28125D0*CA3*m12squared*&
  &SA1*SA2*dgAtMZ())/(CB*MW) + (0.84375D0*CA22*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*MW) - (0.1875D0*CA1*CA3*MH12*SA12*SA2*dgAtMZ(&
  &))/(CB*MW) - (0.5625D0*CA1*CA22*CA3*MH12*SA12*SA2*dgAtMZ())/(CB*MW) - (0.09375D0*CA1*CA3*MH32*SA12*SA2*dgAtMZ())/(CB*MW) - (0.&
  &28125D0*CA1*CA22*CA3*MH32*SA12*SA2*dgAtMZ())/(CB*MW) + (0.1875D0*CA1*m12squared*SA3*dgAtMZ())/(CB*MW) + (0.1875D0*CA1*CA22*m12&
  &squared*SA3*dgAtMZ())/(CB*MW) - (0.125D0*MH12*SA1*SA3*dgAtMZ())/(CB*MW) - (0.375D0*CA12*MH12*SA1*SA3*dgAtMZ())/(CB*MW) - (0.12&
  &5D0*CA22*MH12*SA1*SA3*dgAtMZ())/(CB*MW) - (0.375D0*CA12*CA22*MH12*SA1*SA3*dgAtMZ())/(CB*MW) - (0.0625D0*MH32*SA1*SA3*dgAtMZ())&
  &/(CB*MW) - (0.1875D0*CA12*MH32*SA1*SA3*dgAtMZ())/(CB*MW) - (0.0625D0*CA22*MH32*SA1*SA3*dgAtMZ())/(CB*MW) - (0.1875D0*CA12*CA22&
  &*MH32*SA1*SA3*dgAtMZ())/(CB*MW) - (0.1875D0*CA1*m12squared*SA22*SA3*dgAtMZ())/(CB*MW) + (0.125D0*MH12*SA1*SA22*SA3*dgAtMZ())/(&
  &CB*MW) + (0.375D0*CA12*MH12*SA1*SA22*SA3*dgAtMZ())/(CB*MW) + (0.0625D0*MH32*SA1*SA22*SA3*dgAtMZ())/(CB*MW) + (0.1875D0*CA12*MH&
  &32*SA1*SA22*SA3*dgAtMZ())/(CB*MW) + (0.28125D0*CA1*CA3*m12squared*SA2*dgAtMZ())/(MW*SB) + (0.84375D0*CA1*CA22*CA3*m12squared*S&
  &A2*dgAtMZ())/(MW*SB) - (0.1875D0*CA1*CA3*m12squared*SA2*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA1*CA22*CA3*m12squared*SA2*dgAtMZ()&
  &)/(CB2*MW*SB) + (0.1875D0*CA3*MH12*SA1*SA2*dgAtMZ())/(MW*SB) - (0.1875D0*CA12*CA3*MH12*SA1*SA2*dgAtMZ())/(MW*SB) + (0.5625D0*C&
  &A22*CA3*MH12*SA1*SA2*dgAtMZ())/(MW*SB) - (0.5625D0*CA12*CA22*CA3*MH12*SA1*SA2*dgAtMZ())/(MW*SB) + (0.09375D0*CA3*MH32*SA1*SA2*&
  &dgAtMZ())/(MW*SB) - (0.09375D0*CA12*CA3*MH32*SA1*SA2*dgAtMZ())/(MW*SB) + (0.28125D0*CA22*CA3*MH32*SA1*SA2*dgAtMZ())/(MW*SB) - &
  &(0.28125D0*CA12*CA22*CA3*MH32*SA1*SA2*dgAtMZ())/(MW*SB) + (0.28125D0*CA1*CA3*m12squared*SA12*SA2*dgAtMZ())/(CB2*MW*SB) + (0.84&
  &375D0*CA1*CA22*CA3*m12squared*SA12*SA2*dgAtMZ())/(CB2*MW*SB) + (0.125D0*CA1*MH12*SA3*dgAtMZ())/(MW*SB) + (0.125D0*CA1*CA22*MH1&
  &2*SA3*dgAtMZ())/(MW*SB) + (0.0625D0*CA1*MH32*SA3*dgAtMZ())/(MW*SB) + (0.0625D0*CA1*CA22*MH32*SA3*dgAtMZ())/(MW*SB) - (0.1875D0&
  &*m12squared*SA1*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*CA22*m12squared*SA1*SA3*dgAtMZ())/(MW*SB) + (0.125D0*m12squared*SA1*SA3*dgAt&
  &MZ())/(CB2*MW*SB) + (0.5625D0*CA12*m12squared*SA1*SA3*dgAtMZ())/(CB2*MW*SB) + (0.125D0*CA22*m12squared*SA1*SA3*dgAtMZ())/(CB2*&
  &MW*SB) + (0.5625D0*CA12*CA22*m12squared*SA1*SA3*dgAtMZ())/(CB2*MW*SB) + (0.375D0*CA1*MH12*SA12*SA3*dgAtMZ())/(MW*SB) + (0.375D&
  &0*CA1*CA22*MH12*SA12*SA3*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*MH32*SA12*SA3*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*CA22*MH32*SA12*SA3*&
  &dgAtMZ())/(MW*SB) - (0.125D0*CA1*MH12*SA22*SA3*dgAtMZ())/(MW*SB) - (0.0625D0*CA1*MH32*SA22*SA3*dgAtMZ())/(MW*SB) + (0.1875D0*m&
  &12squared*SA1*SA22*SA3*dgAtMZ())/(MW*SB) - (0.125D0*m12squared*SA1*SA22*SA3*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA12*m12squared*&
  &SA1*SA22*SA3*dgAtMZ())/(CB2*MW*SB) - (0.375D0*CA1*MH12*SA12*SA22*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*CA1*MH32*SA12*SA22*SA3*dgAt&
  &MZ())/(MW*SB) - (0.1875D0*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*MW*SB2) + (0.28125D0*CA12*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*&
  &MW*SB2) - (0.5625D0*CA22*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*MW*SB2) + (0.84375D0*CA12*CA22*CA3*m12squared*SA1*SA2*dgAtMZ())/&
  &(CB*MW*SB2) - (0.125D0*CA1*m12squared*SA3*dgAtMZ())/(CB*MW*SB2) - (0.125D0*CA1*CA22*m12squared*SA3*dgAtMZ())/(CB*MW*SB2) - (0.&
  &5625D0*CA1*m12squared*SA12*SA3*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA1*CA22*m12squared*SA12*SA3*dgAtMZ())/(CB*MW*SB2) + (0.125D0&
  &*CA1*m12squared*SA22*SA3*dgAtMZ())/(CB*MW*SB2) + (0.5625D0*CA1*m12squared*SA12*SA22*SA3*dgAtMZ())/(CB*MW*SB2) - (0.09375D0*CA3&
  &*m12squared*SA1*SA2*dgAtMZ())/(MW*SB*TB) - (0.28125D0*CA22*CA3*m12squared*SA1*SA2*dgAtMZ())/(MW*SB*TB) - (0.0625D0*CA1*m12squa&
  &red*SA3*dgAtMZ())/(MW*SB*TB) - (0.0625D0*CA1*CA22*m12squared*SA3*dgAtMZ())/(MW*SB*TB) + (0.0625D0*CA1*m12squared*SA22*SA3*dgAt&
  &MZ())/(MW*SB*TB) - (0.09375D0*CA1*CA3*m12squared*SA2*TB*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*CA3*m12squared*SA2*TB*dgAtMZ()&
  &)/(CB*MW) + (0.0625D0*m12squared*SA1*SA3*TB*dgAtMZ())/(CB*MW) + (0.0625D0*CA22*m12squared*SA1*SA3*TB*dgAtMZ())/(CB*MW) - (0.06&
  &25D0*m12squared*SA1*SA22*SA3*TB*dgAtMZ())/(CB*MW) + (0.0625D0*CA3*MH12*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875D0*&
  &CA22*CA3*MH12*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.03125D0*CA3*MH32*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0&
  &.09375D0*CA22*CA3*MH32*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.09375D0*CA3*m12squared*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ(&
  &))/(CB2*MW*SB) - (0.28125D0*CA22*CA3*m12squared*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.125D0*MH12*SA3*DBLE(CA1**I&
  &NT(3.D0))*dgAtMZ())/(MW*SB) - (0.125D0*CA22*MH12*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.0625D0*MH32*SA3*DBLE(CA1**INT&
  &(3.D0))*dgAtMZ())/(MW*SB) - (0.0625D0*CA22*MH32*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.125D0*MH12*SA22*SA3*DBLE(CA1**&
  &INT(3.D0))*dgAtMZ())/(MW*SB) + (0.0625D0*MH32*SA22*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*m12squared*SA3*DBLE(&
  &CA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.1875D0*CA22*m12squared*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*m&
  &12squared*SA22*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.125D0*MH12*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.&
  &125D0*CA22*MH12*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.0625D0*MH32*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.06&
  &25D0*CA22*MH32*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.125D0*MH12*SA22*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0&
  &.0625D0*MH32*SA22*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.0625D0*CA3*MH12*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) +&
  & (0.1875D0*CA22*CA3*MH12*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.03125D0*CA3*MH32*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(&
  &MW*SB) + (0.09375D0*CA22*CA3*MH32*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.1875D0*m12squared*SA3*DBLE(SA1**INT(3.D0))*d&
  &gAtMZ())/(CB2*MW*SB) - (0.1875D0*CA22*m12squared*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*m12squared*SA22*SA&
  &3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.09375D0*CA3*m12squared*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.2&
  &8125D0*CA22*CA3*m12squared*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*CA1*CA3*MH12*DBLE(SA2**INT(3.D0))*dgAtMZ&
  &())/(CB*MW) - (0.09375D0*CA1*CA3*MH32*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.28125D0*CA3*m12squared*SA1*DBLE(SA2**INT(3.D&
  &0))*dgAtMZ())/(CB*MW) + (0.1875D0*CA1*CA3*MH12*SA12*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*CA3*MH32*SA12*DBLE&
  &(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA3*m12squared*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*CA3*&
  &m12squared*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875D0*CA3*MH12*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.187&
  &5D0*CA12*CA3*MH12*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.09375D0*CA3*MH32*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) &
  &+ (0.09375D0*CA12*CA3*MH32*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.28125D0*CA1*CA3*m12squared*SA12*DBLE(SA2**INT(3.D0)&
  &)*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*CA3*m12squared*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA12*CA3*m12squ&
  &ared*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.09375D0*CA3*m12squared*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB*TB) &
  &+ (0.09375D0*CA1*CA3*m12squared*TB*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*CA3*MH12*DBLE(CA1**INT(3.D0))*DBLE(SA2**&
  &INT(3.D0))*dgAtMZ())/(CB*MW) - (0.03125D0*CA3*MH32*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA&
  &3*m12squared*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.0625D0*CA3*MH12*DBLE(SA1**INT(3.D0))*DBLE(SA&
  &2**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.03125D0*CA3*MH32*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0&
  &*CA3*m12squared*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.28125D0*CA3*EL*SA1*SA2*dm122MSBarAlter())&
  &/(CB*MW*SW) + (0.84375D0*CA22*CA3*EL*SA1*SA2*dm122MSBarAlter())/(CB*MW*SW) + (0.1875D0*CA1*EL*SA3*dm122MSBarAlter())/(CB*MW*SW&
  &) + (0.1875D0*CA1*CA22*EL*SA3*dm122MSBarAlter())/(CB*MW*SW) - (0.1875D0*CA1*EL*SA22*SA3*dm122MSBarAlter())/(CB*MW*SW) + (0.281&
  &25D0*CA1*CA3*EL*SA2*dm122MSBarAlter())/(MW*SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*SA2*dm122MSBarAlter())/(MW*SB*SW) - (0.1875D0*C&
  &A1*CA3*EL*SA2*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA22*CA3*EL*SA2*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.28125D0&
  &*CA1*CA3*EL*SA12*SA2*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*SA12*SA2*dm122MSBarAlter())/(CB2*MW*SB*SW)&
  & - (0.1875D0*EL*SA1*SA3*dm122MSBarAlter())/(MW*SB*SW) - (0.1875D0*CA22*EL*SA1*SA3*dm122MSBarAlter())/(MW*SB*SW) + (0.125D0*EL*&
  &SA1*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA12*EL*SA1*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.125D0*CA22*EL*SA1&
  &*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA12*CA22*EL*SA1*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.1875D0*EL*SA1*S&
  &A22*SA3*dm122MSBarAlter())/(MW*SB*SW) - (0.125D0*EL*SA1*SA22*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.5625D0*CA12*EL*SA1*SA2&
  &2*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.1875D0*CA3*EL*SA1*SA2*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.28125D0*CA12*CA3*EL*&
  &SA1*SA2*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.5625D0*CA22*CA3*EL*SA1*SA2*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.84375D0*CA12*&
  &CA22*CA3*EL*SA1*SA2*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.125D0*CA1*EL*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.125D0*CA1*C&
  &A22*EL*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.5625D0*CA1*EL*SA12*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA2&
  &2*EL*SA12*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.125D0*CA1*EL*SA22*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.5625D0*CA1*E&
  &L*SA12*SA22*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.09375D0*CA3*EL*SA1*SA2*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.28125D0*CA&
  &22*CA3*EL*SA1*SA2*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.0625D0*CA1*EL*SA3*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.0625D0*CA1*CA2&
  &2*EL*SA3*dm122MSBarAlter())/(MW*SB*SW*TB) + (0.0625D0*CA1*EL*SA22*SA3*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.09375D0*CA1*CA3*EL&
  &*SA2*TB*dm122MSBarAlter())/(CB*MW*SW) - (0.28125D0*CA1*CA22*CA3*EL*SA2*TB*dm122MSBarAlter())/(CB*MW*SW) + (0.0625D0*EL*SA1*SA3&
  &*TB*dm122MSBarAlter())/(CB*MW*SW) + (0.0625D0*CA22*EL*SA1*SA3*TB*dm122MSBarAlter())/(CB*MW*SW) - (0.0625D0*EL*SA1*SA22*SA3*TB*&
  &dm122MSBarAlter())/(CB*MW*SW) - (0.09375D0*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.28125D0*CA22&
  &*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.1875D0*EL*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/&
  &(CB*MW*SB2*SW) + (0.1875D0*CA22*EL*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.1875D0*EL*SA22*SA3*DBLE(CA1&
  &**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.1875D0*EL*SA3*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.&
  &1875D0*CA22*EL*SA3*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.1875D0*EL*SA22*SA3*DBLE(SA1**INT(3.D0))*dm122MS&
  &BarAlter())/(CB2*MW*SB*SW) - (0.09375D0*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA22*CA&
  &3*EL*SA2*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dm122MSBarAlter()&
  &)/(CB*MW*SW) - (0.28125D0*CA1*CA3*EL*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*DBLE(SA2**INT(3&
  &.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.28125D0*CA1*CA3*EL*SA12*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) + &
  &(0.1875D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA12*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*&
  &dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.09375D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(MW*SB*SW*TB) + (0.09375D0*C&
  &A1*CA3*EL*TB*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SW) + (0.09375D0*CA3*EL*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*&
  &dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.09375D0*CA3*EL*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*&
  &SW) + (0.1875D0*CA1*CA3*EL*SA2*dMH12OSAlter())/(CB*MW*SW) + (0.5625D0*CA1*CA22*CA3*EL*SA2*dMH12OSAlter())/(CB*MW*SW) - (0.1875&
  &D0*CA1*CA3*EL*SA12*SA2*dMH12OSAlter())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*EL*SA12*SA2*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*E&
  &L*SA1*SA3*dMH12OSAlter())/(CB*MW*SW) - (0.375D0*CA12*EL*SA1*SA3*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*CA22*EL*SA1*SA3*dMH12OSA&
  &lter())/(CB*MW*SW) - (0.375D0*CA12*CA22*EL*SA1*SA3*dMH12OSAlter())/(CB*MW*SW) + (0.125D0*EL*SA1*SA22*SA3*dMH12OSAlter())/(CB*M&
  &W*SW) + (0.375D0*CA12*EL*SA1*SA22*SA3*dMH12OSAlter())/(CB*MW*SW) + (0.1875D0*CA3*EL*SA1*SA2*dMH12OSAlter())/(MW*SB*SW) - (0.18&
  &75D0*CA12*CA3*EL*SA1*SA2*dMH12OSAlter())/(MW*SB*SW) + (0.5625D0*CA22*CA3*EL*SA1*SA2*dMH12OSAlter())/(MW*SB*SW) - (0.5625D0*CA1&
  &2*CA22*CA3*EL*SA1*SA2*dMH12OSAlter())/(MW*SB*SW) + (0.125D0*CA1*EL*SA3*dMH12OSAlter())/(MW*SB*SW) + (0.125D0*CA1*CA22*EL*SA3*d&
  &MH12OSAlter())/(MW*SB*SW) + (0.375D0*CA1*EL*SA12*SA3*dMH12OSAlter())/(MW*SB*SW) + (0.375D0*CA1*CA22*EL*SA12*SA3*dMH12OSAlter()&
  &)/(MW*SB*SW) - (0.125D0*CA1*EL*SA22*SA3*dMH12OSAlter())/(MW*SB*SW) - (0.375D0*CA1*EL*SA12*SA22*SA3*dMH12OSAlter())/(MW*SB*SW) &
  &- (0.5D0*CA2*CA3*dMH12OSAlter())/vS - (1.5D0*CA2*CA3*SA22*dMH12OSAlter())/vS + (0.0625D0*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dMH12&
  &OSAlter())/(CB*MW*SW) + (0.1875D0*CA22*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*EL*SA3*DBLE(CA1**&
  &INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) - (0.125D0*CA22*EL*SA3*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.125D0*EL*SA2&
  &2*SA3*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.5D0*CA3*DBLE(CA2**INT(3.D0))*dMH12OSAlter())/vS + (0.125D0*EL*SA3*D&
  &BLE(SA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) + (0.125D0*CA22*EL*SA3*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.125&
  &D0*EL*SA22*SA3*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) + (0.0625D0*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(MW&
  &*SB*SW) + (0.1875D0*CA22*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*DBLE(SA2**INT(3.D0)&
  &)*dMH12OSAlter())/(CB*MW*SW) + (0.1875D0*CA1*CA3*EL*SA12*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.1875D0*CA3*EL*SA&
  &1*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.1875D0*CA12*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) &
  &- (0.0625D0*CA3*EL*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.0625D0*CA3*EL*DBLE(SA1**INT(3.D0)&
  &)*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.09375D0*CA1*CA3*EL*SA2*dMH32OSAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA22&
  &*CA3*EL*SA2*dMH32OSAlter())/(CB*MW*SW) - (0.09375D0*CA1*CA3*EL*SA12*SA2*dMH32OSAlter())/(CB*MW*SW) - (0.28125D0*CA1*CA22*CA3*E&
  &L*SA12*SA2*dMH32OSAlter())/(CB*MW*SW) - (0.0625D0*EL*SA1*SA3*dMH32OSAlter())/(CB*MW*SW) - (0.1875D0*CA12*EL*SA1*SA3*dMH32OSAlt&
  &er())/(CB*MW*SW) - (0.0625D0*CA22*EL*SA1*SA3*dMH32OSAlter())/(CB*MW*SW) - (0.1875D0*CA12*CA22*EL*SA1*SA3*dMH32OSAlter())/(CB*M&
  &W*SW) + (0.0625D0*EL*SA1*SA22*SA3*dMH32OSAlter())/(CB*MW*SW) + (0.1875D0*CA12*EL*SA1*SA22*SA3*dMH32OSAlter())/(CB*MW*SW) + (0.&
  &09375D0*CA3*EL*SA1*SA2*dMH32OSAlter())/(MW*SB*SW) - (0.09375D0*CA12*CA3*EL*SA1*SA2*dMH32OSAlter())/(MW*SB*SW) + (0.28125D0*CA2&
  &2*CA3*EL*SA1*SA2*dMH32OSAlter())/(MW*SB*SW) - (0.28125D0*CA12*CA22*CA3*EL*SA1*SA2*dMH32OSAlter())/(MW*SB*SW) + (0.0625D0*CA1*E&
  &L*SA3*dMH32OSAlter())/(MW*SB*SW) + (0.0625D0*CA1*CA22*EL*SA3*dMH32OSAlter())/(MW*SB*SW) + (0.1875D0*CA1*EL*SA12*SA3*dMH32OSAlt&
  &er())/(MW*SB*SW) + (0.1875D0*CA1*CA22*EL*SA12*SA3*dMH32OSAlter())/(MW*SB*SW) - (0.0625D0*CA1*EL*SA22*SA3*dMH32OSAlter())/(MW*S&
  &B*SW) - (0.1875D0*CA1*EL*SA12*SA22*SA3*dMH32OSAlter())/(MW*SB*SW) - (0.25D0*CA2*CA3*dMH32OSAlter())/vS - (0.75D0*CA2*CA3*SA22*&
  &dMH32OSAlter())/vS + (0.03125D0*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) + (0.09375D0*CA22*CA3*EL*SA2*DBLE(C&
  &A1**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) - (0.0625D0*EL*SA3*DBLE(CA1**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) - (0.0625D0*CA22*&
  &EL*SA3*DBLE(CA1**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) + (0.0625D0*EL*SA22*SA3*DBLE(CA1**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW)&
  & + (0.25D0*CA3*DBLE(CA2**INT(3.D0))*dMH32OSAlter())/vS + (0.0625D0*EL*SA3*DBLE(SA1**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) + (0&
  &.0625D0*CA22*EL*SA3*DBLE(SA1**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) - (0.0625D0*EL*SA22*SA3*DBLE(SA1**INT(3.D0))*dMH32OSAlter(&
  &))/(CB*MW*SW) + (0.03125D0*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) + (0.09375D0*CA22*CA3*EL*SA2*DBLE(SA1**I&
  &NT(3.D0))*dMH32OSAlter())/(MW*SB*SW) - (0.09375D0*CA1*CA3*EL*DBLE(SA2**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) + (0.09375D0*CA1*&
  &CA3*EL*SA12*DBLE(SA2**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) - (0.09375D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dMH32OSAlter())/(MW*S&
  &B*SW) + (0.09375D0*CA12*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) - (0.03125D0*CA3*EL*DBLE(CA1**INT(3.D0))*DB&
  &LE(SA2**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) - (0.03125D0*CA3*EL*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH32OSAlter())/(M&
  &W*SB*SW) - (0.09375D0*CA1*CA3*EL*MH12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.28125D0*CA1*CA22*CA3*EL*MH12*SA2*DBLE&
  &(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.046875D0*CA1*CA3*EL*MH32*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.140625D&
  &0*CA1*CA22*CA3*EL*MH32*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.140625D0*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D&
  &0))*dMW2Alter())/(CB*SW) - (0.421875D0*CA22*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.09375D0*C&
  &A1*CA3*EL*MH12*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.28125D0*CA1*CA22*CA3*EL*MH12*SA12*SA2*DBLE(MW**INT(-3.D&
  &0))*dMW2Alter())/(CB*SW) + (0.046875D0*CA1*CA3*EL*MH32*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*CA1*CA&
  &22*CA3*EL*MH32*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2&
  &Alter())/(CB*SW) - (0.09375D0*CA1*CA22*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0625D0*EL*MH12*SA1*SA3&
  &*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.1875D0*CA12*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.062&
  &5D0*CA22*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.1875D0*CA12*CA22*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*&
  &dMW2Alter())/(CB*SW) + (0.03125D0*EL*MH32*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.09375D0*CA12*EL*MH32*SA1*SA3*&
  &DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*CA22*EL*MH32*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.093&
  &75D0*CA12*CA22*EL*MH32*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.09375D0*CA1*EL*m12squared*SA22*SA3*DBLE(MW**INT(&
  &-3.D0))*dMW2Alter())/(CB*SW) - (0.0625D0*EL*MH12*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.1875D0*CA12*EL*MH&
  &12*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*EL*MH32*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/&
  &(CB*SW) - (0.09375D0*CA12*EL*MH32*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.140625D0*CA1*CA3*EL*m12squared*S&
  &A2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.421875D0*CA1*CA22*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(S&
  &B*SW) + (0.09375D0*CA1*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.28125D0*CA1*CA22*CA3*EL*m12squ&
  &ared*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.09375D0*CA3*EL*MH12*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*&
  &SW) + (0.09375D0*CA12*CA3*EL*MH12*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.28125D0*CA22*CA3*EL*MH12*SA1*SA2*DBLE&
  &(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.28125D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (&
  &0.046875D0*CA3*EL*MH32*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.046875D0*CA12*CA3*EL*MH32*SA1*SA2*DBLE(MW**INT(-&
  &3.D0))*dMW2Alter())/(SB*SW) - (0.140625D0*CA22*CA3*EL*MH32*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.140625D0*CA1&
  &2*CA22*CA3*EL*MH32*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.140625D0*CA1*CA3*EL*m12squared*SA12*SA2*DBLE(MW**INT&
  &(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.421875D0*CA1*CA22*CA3*EL*m12squared*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*&
  &SW) - (0.0625D0*CA1*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.0625D0*CA1*CA22*EL*MH12*SA3*DBLE(MW**INT(-3.D0)&
  &)*dMW2Alter())/(SB*SW) - (0.03125D0*CA1*EL*MH32*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.03125D0*CA1*CA22*EL*MH32*SA&
  &3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.&
  &09375D0*CA22*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.0625D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3&
  &.D0))*dMW2Alter())/(CB2*SB*SW) - (0.28125D0*CA12*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.0625&
  &D0*CA22*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.28125D0*CA12*CA22*EL*m12squared*SA1*SA3*DBLE(&
  &MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.1875D0*CA1*EL*MH12*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.1875D&
  &0*CA1*CA22*EL*MH12*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*CA1*EL*MH32*SA12*SA3*DBLE(MW**INT(-3.D0))*d&
  &MW2Alter())/(SB*SW) - (0.09375D0*CA1*CA22*EL*MH32*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.0625D0*CA1*EL*MH12*S&
  &A22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.03125D0*CA1*EL*MH32*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) &
  &- (0.09375D0*EL*m12squared*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.0625D0*EL*m12squared*SA1*SA22*SA3*DBLE(&
  &MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.28125D0*CA12*EL*m12squared*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*S&
  &B*SW) + (0.1875D0*CA1*EL*MH12*SA12*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA1*EL*MH32*SA12*SA22*SA3*D&
  &BLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) &
  &- (0.140625D0*CA12*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.28125D0*CA22*CA3*EL*m12squared&
  &*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.421875D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*&
  &dMW2Alter())/(CB*SB2*SW) + (0.0625D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.0625D0*CA1*CA22*&
  &EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.28125D0*CA1*EL*m12squared*SA12*SA3*DBLE(MW**INT(-3.D0))*d&
  &MW2Alter())/(CB*SB2*SW) + (0.28125D0*CA1*CA22*EL*m12squared*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.0625D0&
  &*CA1*EL*m12squared*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.28125D0*CA1*EL*m12squared*SA12*SA22*SA3*DBLE(MW&
  &**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.046875D0*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + &
  &(0.140625D0*CA22*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.03125D0*CA1*EL*m12squared*SA3*DBL&
  &E(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.03125D0*CA1*CA22*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB)&
  & - (0.03125D0*CA1*EL*m12squared*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.046875D0*CA1*CA3*EL*m12squared*SA2*&
  &TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*CA1*CA22*CA3*EL*m12squared*SA2*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())&
  &/(CB*SW) - (0.03125D0*EL*m12squared*SA1*SA3*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA22*EL*m12squared*SA1*S&
  &A3*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*EL*m12squared*SA1*SA22*SA3*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(&
  &CB*SW) - (0.03125D0*CA3*EL*MH12*SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA22*CA3*EL*MH&
  &12*SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.015625D0*CA3*EL*MH32*SA2*DBLE(CA1**INT(3.D0))*DBLE(&
  &MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.046875D0*CA22*CA3*EL*MH32*SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())&
  &/(CB*SW) + (0.046875D0*CA3*EL*m12squared*SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.140625D0*&
  &CA22*CA3*EL*m12squared*SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.0625D0*EL*MH12*SA3*DBLE(CA1&
  &**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.0625D0*CA22*EL*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*&
  &dMW2Alter())/(SB*SW) + (0.03125D0*EL*MH32*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.03125D0*CA22&
  &*EL*MH32*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.0625D0*EL*MH12*SA22*SA3*DBLE(CA1**INT(3.D0))*&
  &DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.03125D0*EL*MH32*SA22*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())&
  &/(SB*SW) - (0.09375D0*EL*m12squared*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.09375D0*CA22*E&
  &L*m12squared*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.09375D0*EL*m12squared*SA22*SA3*DBLE(C&
  &A1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.0625D0*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))&
  &*dMW2Alter())/(CB*SW) - (0.0625D0*CA22*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0&
  &*EL*MH32*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA22*EL*MH32*SA3*DBLE(MW**INT(-3.D0))&
  &*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.0625D0*EL*MH12*SA22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())&
  &/(CB*SW) + (0.03125D0*EL*MH32*SA22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA3*EL*MH12&
  &*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*CA22*CA3*EL*MH12*SA2*DBLE(MW**INT(-3.D0))*DBL&
  &E(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.015625D0*CA3*EL*MH32*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(S&
  &B*SW) - (0.046875D0*CA22*CA3*EL*MH32*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*EL*m12squ&
  &ared*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.09375D0*CA22*EL*m12squared*SA3*DBLE(MW**INT(-&
  &3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.09375D0*EL*m12squared*SA22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.&
  &D0))*dMW2Alter())/(CB2*SB*SW) + (0.046875D0*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*S&
  &B2*SW) + (0.140625D0*CA22*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.09375D&
  &0*CA1*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.046875D0*CA1*CA3*EL*MH32*DBLE(MW**INT(-3&
  &.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*&
  &dMW2Alter())/(CB*SW) - (0.09375D0*CA1*CA3*EL*MH12*SA12*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.046&
  &875D0*CA1*CA3*EL*MH32*SA12*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*CA1*CA3*EL*m12squared*&
  &DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*DBLE(SA&
  &2**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.09375D0*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB&
  &*SW) - (0.09375D0*CA12*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.046875D0*CA3*EL*MH3&
  &2*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*CA12*CA3*EL*MH32*SA1*DBLE(MW**INT(-3.D0))*D&
  &BLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.140625D0*CA1*CA3*EL*m12squared*SA12*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dM&
  &W2Alter())/(CB2*SB*SW) - (0.09375D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) +&
  & (0.140625D0*CA12*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.046875D0*CA3*E&
  &L*m12squared*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW*TB) - (0.046875D0*CA1*CA3*EL*m12squared*TB*DBLE&
  &(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*CA3*EL*MH12*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*&
  &DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.015625D0*CA3*EL*MH32*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.&
  &D0))*dMW2Alter())/(CB*SW) - (0.046875D0*CA3*EL*m12squared*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2A&
  &lter())/(CB2*SB*SW) + (0.03125D0*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*S&
  &W) + (0.015625D0*CA3*EL*MH32*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0&
  &*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + 0.5D0*CA2*CA3*MH1&
  &2*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() + 0.25D0*CA2*CA3*MH32*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() + 1.5D0*CA2*CA3*MH12*SA22*DB&
  &LE(vS**INT(-2.D0))*dvSMSBarAlter() + 0.75D0*CA2*CA3*MH32*SA22*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() - 0.5D0*CA3*MH12*DBLE(CA2**&
  &INT(3.D0))*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() - 0.25D0*CA3*MH32*DBLE(CA2**INT(3.D0))*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() &
		& + CS1S1S1f111*dZH1H3OSAlter()/2D0 + CS1S1S1f211*dZH2H3OSAlter()/2D0 + CS1S1S1f311*dZH3H3OS()/2D0 &
		& + CS1S1S1f131*dZH1H1OS()/2D0 + CS1S1S1f231*dZH2H1OSAlter()/2D0 + CS1S1S1f331*dZH3H1OSAlter()/2D0 &
		& + CS1S1S1f131*dZH1H1OS()/2D0 + CS1S1S1f231*dZH2H1OSAlter()/2D0 + CS1S1S1f331*dZH3H1OSAlter()/2D0 &
		& )
	case (7)
		dRR11 = -CA2*SA1*dAlpha1PinchOS() - CA1*SA2*dAlpha2PinchOS()
		dRR12 = CA1*CA2*dAlpha1PinchOS() - SA1*SA2*dAlpha2PinchOS()
		dRR13 = CA2*dAlpha2PinchOS()
		dRR21 = -CA1*CA3*dAlpha1PinchOS() - CA1*CA3*SA2*dAlpha3PinchOS() - CA1*CA2*SA3*dAlpha2PinchOS() + &
			& SA1*SA3*dAlpha3PinchOS() + SA1*SA2*SA3*dAlpha1PinchOS()
		dRR22 = -CA3*SA1*dAlpha1PinchOS() - CA3*SA1*SA2*dAlpha3PinchOS() - CA1*SA3*dAlpha3PinchOS() - &
			& CA2*SA1*SA3*dAlpha2PinchOS() - CA1*SA2*SA3*dAlpha1PinchOS()
		dRR23 = CA2*CA3*dAlpha3PinchOS() - SA2*SA3*dAlpha2PinchOS()
		dRR31 = -CA1*CA2*CA3*dAlpha2PinchOS() + CA3*SA1*dAlpha3PinchOS() + CA3*SA1*SA2*dAlpha1PinchOS() + &
			& CA1*SA3*dAlpha1PinchOS() + CA1*SA2*SA3*dAlpha3PinchOS()
		dRR32 = -CA1*CA3*dAlpha3PinchOS() - CA2*CA3*SA1*dAlpha2PinchOS() - CA1*CA3*SA2*dAlpha1PinchOS() + &
			& SA1*SA3*dAlpha1PinchOS() + SA1*SA2*SA3*dAlpha3PinchOS()
		dRR33 = -CA3*SA2*dAlpha2PinchOS() - CA2*SA3*dAlpha3PinchOS()

		totalAmplitude = CS1S1S1f311*( &
&(0.28125D0*CA1*CA3*EL*m12squared*SA2*dAlpha1PinchOS())/(CB*MW*SW) + (0.84375D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha1PinchOS())/(&
  &CB*MW*SW) - (0.1875D0*CA3*EL*MH12*SA1*SA2*dAlpha1PinchOS())/(CB*MW*SW) - (0.5625D0*CA12*CA3*EL*MH12*SA1*SA2*dAlpha1PinchOS())/&
  &(CB*MW*SW) - (0.5625D0*CA22*CA3*EL*MH12*SA1*SA2*dAlpha1PinchOS())/(CB*MW*SW) - (1.6875D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dAlpha1&
  &PinchOS())/(CB*MW*SW) - (0.09375D0*CA3*EL*MH32*SA1*SA2*dAlpha1PinchOS())/(CB*MW*SW) - (0.28125D0*CA12*CA3*EL*MH32*SA1*SA2*dAlp&
  &ha1PinchOS())/(CB*MW*SW) - (0.28125D0*CA22*CA3*EL*MH32*SA1*SA2*dAlpha1PinchOS())/(CB*MW*SW) - (0.84375D0*CA12*CA22*CA3*EL*MH32&
  &*SA1*SA2*dAlpha1PinchOS())/(CB*MW*SW) - (0.125D0*CA1*EL*MH12*SA3*dAlpha1PinchOS())/(CB*MW*SW) - (0.125D0*CA1*CA22*EL*MH12*SA3*&
  &dAlpha1PinchOS())/(CB*MW*SW) - (0.0625D0*CA1*EL*MH32*SA3*dAlpha1PinchOS())/(CB*MW*SW) - (0.0625D0*CA1*CA22*EL*MH32*SA3*dAlpha1&
  &PinchOS())/(CB*MW*SW) - (0.1875D0*EL*m12squared*SA1*SA3*dAlpha1PinchOS())/(CB*MW*SW) - (0.1875D0*CA22*EL*m12squared*SA1*SA3*dA&
  &lpha1PinchOS())/(CB*MW*SW) + (1.125D0*CA1*EL*MH12*SA12*SA3*dAlpha1PinchOS())/(CB*MW*SW) + (1.125D0*CA1*CA22*EL*MH12*SA12*SA3*d&
  &Alpha1PinchOS())/(CB*MW*SW) + (0.5625D0*CA1*EL*MH32*SA12*SA3*dAlpha1PinchOS())/(CB*MW*SW) + (0.5625D0*CA1*CA22*EL*MH32*SA12*SA&
  &3*dAlpha1PinchOS())/(CB*MW*SW) + (0.125D0*CA1*EL*MH12*SA22*SA3*dAlpha1PinchOS())/(CB*MW*SW) + (0.0625D0*CA1*EL*MH32*SA22*SA3*d&
  &Alpha1PinchOS())/(CB*MW*SW) + (0.1875D0*EL*m12squared*SA1*SA22*SA3*dAlpha1PinchOS())/(CB*MW*SW) - (1.125D0*CA1*EL*MH12*SA12*SA&
  &22*SA3*dAlpha1PinchOS())/(CB*MW*SW) - (0.5625D0*CA1*EL*MH32*SA12*SA22*SA3*dAlpha1PinchOS())/(CB*MW*SW) + (0.1875D0*CA1*CA3*EL*&
  &MH12*SA2*dAlpha1PinchOS())/(MW*SB*SW) + (0.5625D0*CA1*CA22*CA3*EL*MH12*SA2*dAlpha1PinchOS())/(MW*SB*SW) + (0.09375D0*CA1*CA3*E&
  &L*MH32*SA2*dAlpha1PinchOS())/(MW*SB*SW) + (0.28125D0*CA1*CA22*CA3*EL*MH32*SA2*dAlpha1PinchOS())/(MW*SB*SW) - (0.28125D0*CA3*EL&
  &*m12squared*SA1*SA2*dAlpha1PinchOS())/(MW*SB*SW) - (0.84375D0*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha1PinchOS())/(MW*SB*SW) + (0&
  &.1875D0*CA3*EL*m12squared*SA1*SA2*dAlpha1PinchOS())/(CB2*MW*SB*SW) + (0.84375D0*CA12*CA3*EL*m12squared*SA1*SA2*dAlpha1PinchOS(&
  &))/(CB2*MW*SB*SW) + (0.5625D0*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha1PinchOS())/(CB2*MW*SB*SW) + (2.53125D0*CA12*CA22*CA3*EL*m1&
  &2squared*SA1*SA2*dAlpha1PinchOS())/(CB2*MW*SB*SW) + (0.5625D0*CA1*CA3*EL*MH12*SA12*SA2*dAlpha1PinchOS())/(MW*SB*SW) + (1.6875D&
  &0*CA1*CA22*CA3*EL*MH12*SA12*SA2*dAlpha1PinchOS())/(MW*SB*SW) + (0.28125D0*CA1*CA3*EL*MH32*SA12*SA2*dAlpha1PinchOS())/(MW*SB*SW&
  &) + (0.84375D0*CA1*CA22*CA3*EL*MH32*SA12*SA2*dAlpha1PinchOS())/(MW*SB*SW) - (0.1875D0*CA1*EL*m12squared*SA3*dAlpha1PinchOS())/&
  &(MW*SB*SW) - (0.1875D0*CA1*CA22*EL*m12squared*SA3*dAlpha1PinchOS())/(MW*SB*SW) + (0.125D0*CA1*EL*m12squared*SA3*dAlpha1PinchOS&
  &())/(CB2*MW*SB*SW) + (0.125D0*CA1*CA22*EL*m12squared*SA3*dAlpha1PinchOS())/(CB2*MW*SB*SW) - (0.125D0*EL*MH12*SA1*SA3*dAlpha1Pi&
  &nchOS())/(MW*SB*SW) + (1.125D0*CA12*EL*MH12*SA1*SA3*dAlpha1PinchOS())/(MW*SB*SW) - (0.125D0*CA22*EL*MH12*SA1*SA3*dAlpha1PinchO&
  &S())/(MW*SB*SW) + (1.125D0*CA12*CA22*EL*MH12*SA1*SA3*dAlpha1PinchOS())/(MW*SB*SW) - (0.0625D0*EL*MH32*SA1*SA3*dAlpha1PinchOS()&
  &)/(MW*SB*SW) + (0.5625D0*CA12*EL*MH32*SA1*SA3*dAlpha1PinchOS())/(MW*SB*SW) - (0.0625D0*CA22*EL*MH32*SA1*SA3*dAlpha1PinchOS())/&
  &(MW*SB*SW) + (0.5625D0*CA12*CA22*EL*MH32*SA1*SA3*dAlpha1PinchOS())/(MW*SB*SW) - (1.6875D0*CA1*EL*m12squared*SA12*SA3*dAlpha1Pi&
  &nchOS())/(CB2*MW*SB*SW) - (1.6875D0*CA1*CA22*EL*m12squared*SA12*SA3*dAlpha1PinchOS())/(CB2*MW*SB*SW) + (0.1875D0*CA1*EL*m12squ&
  &ared*SA22*SA3*dAlpha1PinchOS())/(MW*SB*SW) - (0.125D0*CA1*EL*m12squared*SA22*SA3*dAlpha1PinchOS())/(CB2*MW*SB*SW) + (0.125D0*E&
  &L*MH12*SA1*SA22*SA3*dAlpha1PinchOS())/(MW*SB*SW) - (1.125D0*CA12*EL*MH12*SA1*SA22*SA3*dAlpha1PinchOS())/(MW*SB*SW) + (0.0625D0&
  &*EL*MH32*SA1*SA22*SA3*dAlpha1PinchOS())/(MW*SB*SW) - (0.5625D0*CA12*EL*MH32*SA1*SA22*SA3*dAlpha1PinchOS())/(MW*SB*SW) + (1.687&
  &5D0*CA1*EL*m12squared*SA12*SA22*SA3*dAlpha1PinchOS())/(CB2*MW*SB*SW) - (0.1875D0*CA1*CA3*EL*m12squared*SA2*dAlpha1PinchOS())/(&
  &CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha1PinchOS())/(CB*MW*SB2*SW) - (0.84375D0*CA1*CA3*EL*m12squared*S&
  &A12*SA2*dAlpha1PinchOS())/(CB*MW*SB2*SW) - (2.53125D0*CA1*CA22*CA3*EL*m12squared*SA12*SA2*dAlpha1PinchOS())/(CB*MW*SB2*SW) + (&
  &0.125D0*EL*m12squared*SA1*SA3*dAlpha1PinchOS())/(CB*MW*SB2*SW) - (1.6875D0*CA12*EL*m12squared*SA1*SA3*dAlpha1PinchOS())/(CB*MW&
  &*SB2*SW) + (0.125D0*CA22*EL*m12squared*SA1*SA3*dAlpha1PinchOS())/(CB*MW*SB2*SW) - (1.6875D0*CA12*CA22*EL*m12squared*SA1*SA3*dA&
  &lpha1PinchOS())/(CB*MW*SB2*SW) - (0.125D0*EL*m12squared*SA1*SA22*SA3*dAlpha1PinchOS())/(CB*MW*SB2*SW) + (1.6875D0*CA12*EL*m12s&
  &quared*SA1*SA22*SA3*dAlpha1PinchOS())/(CB*MW*SB2*SW) - (0.09375D0*CA1*CA3*EL*m12squared*SA2*dAlpha1PinchOS())/(MW*SB*SW*TB) - &
  &(0.28125D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha1PinchOS())/(MW*SB*SW*TB) + (0.0625D0*EL*m12squared*SA1*SA3*dAlpha1PinchOS())/&
  &(MW*SB*SW*TB) + (0.0625D0*CA22*EL*m12squared*SA1*SA3*dAlpha1PinchOS())/(MW*SB*SW*TB) - (0.0625D0*EL*m12squared*SA1*SA22*SA3*dA&
  &lpha1PinchOS())/(MW*SB*SW*TB) + (0.09375D0*CA3*EL*m12squared*SA1*SA2*TB*dAlpha1PinchOS())/(CB*MW*SW) + (0.28125D0*CA22*CA3*EL*&
  &m12squared*SA1*SA2*TB*dAlpha1PinchOS())/(CB*MW*SW) + (0.0625D0*CA1*EL*m12squared*SA3*TB*dAlpha1PinchOS())/(CB*MW*SW) + (0.0625&
  &D0*CA1*CA22*EL*m12squared*SA3*TB*dAlpha1PinchOS())/(CB*MW*SW) - (0.0625D0*CA1*EL*m12squared*SA22*SA3*TB*dAlpha1PinchOS())/(CB*&
  &MW*SW) + (0.1875D0*CA1*CA2*CA3*EL*MH12*dAlpha2PinchOS())/(CB*MW*SW) + (0.09375D0*CA1*CA2*CA3*EL*MH32*dAlpha2PinchOS())/(CB*MW*&
  &SW) + (0.28125D0*CA2*CA3*EL*m12squared*SA1*dAlpha2PinchOS())/(CB*MW*SW) - (0.1875D0*CA1*CA2*CA3*EL*MH12*SA12*dAlpha2PinchOS())&
  &/(CB*MW*SW) - (0.09375D0*CA1*CA2*CA3*EL*MH32*SA12*dAlpha2PinchOS())/(CB*MW*SW) - (1.6875D0*CA1*CA2*CA3*EL*MH12*SA22*dAlpha2Pin&
  &chOS())/(CB*MW*SW) - (0.84375D0*CA1*CA2*CA3*EL*MH32*SA22*dAlpha2PinchOS())/(CB*MW*SW) - (2.53125D0*CA2*CA3*EL*m12squared*SA1*S&
  &A22*dAlpha2PinchOS())/(CB*MW*SW) + (1.6875D0*CA1*CA2*CA3*EL*MH12*SA12*SA22*dAlpha2PinchOS())/(CB*MW*SW) + (0.84375D0*CA1*CA2*C&
  &A3*EL*MH32*SA12*SA22*dAlpha2PinchOS())/(CB*MW*SW) - (0.75D0*CA1*CA2*EL*m12squared*SA2*SA3*dAlpha2PinchOS())/(CB*MW*SW) + (0.5D&
  &0*CA2*EL*MH12*SA1*SA2*SA3*dAlpha2PinchOS())/(CB*MW*SW) + (1.5D0*CA12*CA2*EL*MH12*SA1*SA2*SA3*dAlpha2PinchOS())/(CB*MW*SW) + (0&
  &.25D0*CA2*EL*MH32*SA1*SA2*SA3*dAlpha2PinchOS())/(CB*MW*SW) + (0.75D0*CA12*CA2*EL*MH32*SA1*SA2*SA3*dAlpha2PinchOS())/(CB*MW*SW)&
  & + (0.28125D0*CA1*CA2*CA3*EL*m12squared*dAlpha2PinchOS())/(MW*SB*SW) - (0.1875D0*CA1*CA2*CA3*EL*m12squared*dAlpha2PinchOS())/(&
  &CB2*MW*SB*SW) + (0.1875D0*CA2*CA3*EL*MH12*SA1*dAlpha2PinchOS())/(MW*SB*SW) - (0.1875D0*CA12*CA2*CA3*EL*MH12*SA1*dAlpha2PinchOS&
  &())/(MW*SB*SW) + (0.09375D0*CA2*CA3*EL*MH32*SA1*dAlpha2PinchOS())/(MW*SB*SW) - (0.09375D0*CA12*CA2*CA3*EL*MH32*SA1*dAlpha2Pinc&
  &hOS())/(MW*SB*SW) + (0.28125D0*CA1*CA2*CA3*EL*m12squared*SA12*dAlpha2PinchOS())/(CB2*MW*SB*SW) - (2.53125D0*CA1*CA2*CA3*EL*m12&
  &squared*SA22*dAlpha2PinchOS())/(MW*SB*SW) + (1.6875D0*CA1*CA2*CA3*EL*m12squared*SA22*dAlpha2PinchOS())/(CB2*MW*SB*SW) - (1.687&
  &5D0*CA2*CA3*EL*MH12*SA1*SA22*dAlpha2PinchOS())/(MW*SB*SW) + (1.6875D0*CA12*CA2*CA3*EL*MH12*SA1*SA22*dAlpha2PinchOS())/(MW*SB*S&
  &W) - (0.84375D0*CA2*CA3*EL*MH32*SA1*SA22*dAlpha2PinchOS())/(MW*SB*SW) + (0.84375D0*CA12*CA2*CA3*EL*MH32*SA1*SA22*dAlpha2PinchO&
  &S())/(MW*SB*SW) - (2.53125D0*CA1*CA2*CA3*EL*m12squared*SA12*SA22*dAlpha2PinchOS())/(CB2*MW*SB*SW) - (0.5D0*CA1*CA2*EL*MH12*SA2&
  &*SA3*dAlpha2PinchOS())/(MW*SB*SW) - (0.25D0*CA1*CA2*EL*MH32*SA2*SA3*dAlpha2PinchOS())/(MW*SB*SW) + (0.75D0*CA2*EL*m12squared*S&
  &A1*SA2*SA3*dAlpha2PinchOS())/(MW*SB*SW) - (0.5D0*CA2*EL*m12squared*SA1*SA2*SA3*dAlpha2PinchOS())/(CB2*MW*SB*SW) - (2.25D0*CA12&
  &*CA2*EL*m12squared*SA1*SA2*SA3*dAlpha2PinchOS())/(CB2*MW*SB*SW) - (1.5D0*CA1*CA2*EL*MH12*SA12*SA2*SA3*dAlpha2PinchOS())/(MW*SB&
  &*SW) - (0.75D0*CA1*CA2*EL*MH32*SA12*SA2*SA3*dAlpha2PinchOS())/(MW*SB*SW) - (0.1875D0*CA2*CA3*EL*m12squared*SA1*dAlpha2PinchOS(&
  &))/(CB*MW*SB2*SW) + (0.28125D0*CA12*CA2*CA3*EL*m12squared*SA1*dAlpha2PinchOS())/(CB*MW*SB2*SW) + (1.6875D0*CA2*CA3*EL*m12squar&
  &ed*SA1*SA22*dAlpha2PinchOS())/(CB*MW*SB2*SW) - (2.53125D0*CA12*CA2*CA3*EL*m12squared*SA1*SA22*dAlpha2PinchOS())/(CB*MW*SB2*SW)&
  & + (0.5D0*CA1*CA2*EL*m12squared*SA2*SA3*dAlpha2PinchOS())/(CB*MW*SB2*SW) + (2.25D0*CA1*CA2*EL*m12squared*SA12*SA2*SA3*dAlpha2P&
  &inchOS())/(CB*MW*SB2*SW) - (0.09375D0*CA2*CA3*EL*m12squared*SA1*dAlpha2PinchOS())/(MW*SB*SW*TB) + (0.84375D0*CA2*CA3*EL*m12squ&
  &ared*SA1*SA22*dAlpha2PinchOS())/(MW*SB*SW*TB) + (0.25D0*CA1*CA2*EL*m12squared*SA2*SA3*dAlpha2PinchOS())/(MW*SB*SW*TB) - (0.093&
  &75D0*CA1*CA2*CA3*EL*m12squared*TB*dAlpha2PinchOS())/(CB*MW*SW) + (0.84375D0*CA1*CA2*CA3*EL*m12squared*SA22*TB*dAlpha2PinchOS()&
  &)/(CB*MW*SW) - (0.25D0*CA2*EL*m12squared*SA1*SA2*SA3*TB*dAlpha2PinchOS())/(CB*MW*SW) + (0.5D0*CA3*MH12*SA2*dAlpha2PinchOS())/v&
  &S - (4.5D0*CA22*CA3*MH12*SA2*dAlpha2PinchOS())/vS + (0.25D0*CA3*MH32*SA2*dAlpha2PinchOS())/vS - (2.25D0*CA22*CA3*MH32*SA2*dAlp&
  &ha2PinchOS())/vS + (0.1875D0*CA1*CA3*EL*m12squared*dAlpha3PinchOS())/(CB*MW*SW) + (0.1875D0*CA1*CA22*CA3*EL*m12squared*dAlpha3&
  &PinchOS())/(CB*MW*SW) - (0.125D0*CA3*EL*MH12*SA1*dAlpha3PinchOS())/(CB*MW*SW) - (0.375D0*CA12*CA3*EL*MH12*SA1*dAlpha3PinchOS()&
  &)/(CB*MW*SW) - (0.125D0*CA22*CA3*EL*MH12*SA1*dAlpha3PinchOS())/(CB*MW*SW) - (0.375D0*CA12*CA22*CA3*EL*MH12*SA1*dAlpha3PinchOS(&
  &))/(CB*MW*SW) - (0.0625D0*CA3*EL*MH32*SA1*dAlpha3PinchOS())/(CB*MW*SW) - (0.1875D0*CA12*CA3*EL*MH32*SA1*dAlpha3PinchOS())/(CB*&
  &MW*SW) - (0.0625D0*CA22*CA3*EL*MH32*SA1*dAlpha3PinchOS())/(CB*MW*SW) - (0.1875D0*CA12*CA22*CA3*EL*MH32*SA1*dAlpha3PinchOS())/(&
  &CB*MW*SW) - (0.1875D0*CA1*CA3*EL*m12squared*SA22*dAlpha3PinchOS())/(CB*MW*SW) + (0.125D0*CA3*EL*MH12*SA1*SA22*dAlpha3PinchOS()&
  &)/(CB*MW*SW) + (0.375D0*CA12*CA3*EL*MH12*SA1*SA22*dAlpha3PinchOS())/(CB*MW*SW) + (0.0625D0*CA3*EL*MH32*SA1*SA22*dAlpha3PinchOS&
  &())/(CB*MW*SW) + (0.1875D0*CA12*CA3*EL*MH32*SA1*SA22*dAlpha3PinchOS())/(CB*MW*SW) - (0.1875D0*CA1*EL*MH12*SA2*SA3*dAlpha3Pinch&
  &OS())/(CB*MW*SW) - (0.5625D0*CA1*CA22*EL*MH12*SA2*SA3*dAlpha3PinchOS())/(CB*MW*SW) - (0.09375D0*CA1*EL*MH32*SA2*SA3*dAlpha3Pin&
  &chOS())/(CB*MW*SW) - (0.28125D0*CA1*CA22*EL*MH32*SA2*SA3*dAlpha3PinchOS())/(CB*MW*SW) - (0.28125D0*EL*m12squared*SA1*SA2*SA3*d&
  &Alpha3PinchOS())/(CB*MW*SW) - (0.84375D0*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha3PinchOS())/(CB*MW*SW) + (0.1875D0*CA1*EL*MH12*S&
  &A12*SA2*SA3*dAlpha3PinchOS())/(CB*MW*SW) + (0.5625D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dAlpha3PinchOS())/(CB*MW*SW) + (0.09375D0*C&
  &A1*EL*MH32*SA12*SA2*SA3*dAlpha3PinchOS())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*MH32*SA12*SA2*SA3*dAlpha3PinchOS())/(CB*MW*SW) +&
  & (0.125D0*CA1*CA3*EL*MH12*dAlpha3PinchOS())/(MW*SB*SW) + (0.125D0*CA1*CA22*CA3*EL*MH12*dAlpha3PinchOS())/(MW*SB*SW) + (0.0625D&
  &0*CA1*CA3*EL*MH32*dAlpha3PinchOS())/(MW*SB*SW) + (0.0625D0*CA1*CA22*CA3*EL*MH32*dAlpha3PinchOS())/(MW*SB*SW) - (0.1875D0*CA3*E&
  &L*m12squared*SA1*dAlpha3PinchOS())/(MW*SB*SW) - (0.1875D0*CA22*CA3*EL*m12squared*SA1*dAlpha3PinchOS())/(MW*SB*SW) + (0.125D0*C&
  &A3*EL*m12squared*SA1*dAlpha3PinchOS())/(CB2*MW*SB*SW) + (0.5625D0*CA12*CA3*EL*m12squared*SA1*dAlpha3PinchOS())/(CB2*MW*SB*SW) &
  &+ (0.125D0*CA22*CA3*EL*m12squared*SA1*dAlpha3PinchOS())/(CB2*MW*SB*SW) + (0.5625D0*CA12*CA22*CA3*EL*m12squared*SA1*dAlpha3Pinc&
  &hOS())/(CB2*MW*SB*SW) + (0.375D0*CA1*CA3*EL*MH12*SA12*dAlpha3PinchOS())/(MW*SB*SW) + (0.375D0*CA1*CA22*CA3*EL*MH12*SA12*dAlpha&
  &3PinchOS())/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*MH32*SA12*dAlpha3PinchOS())/(MW*SB*SW) + (0.1875D0*CA1*CA22*CA3*EL*MH32*SA12*dAl&
  &pha3PinchOS())/(MW*SB*SW) - (0.125D0*CA1*CA3*EL*MH12*SA22*dAlpha3PinchOS())/(MW*SB*SW) - (0.0625D0*CA1*CA3*EL*MH32*SA22*dAlpha&
  &3PinchOS())/(MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA1*SA22*dAlpha3PinchOS())/(MW*SB*SW) - (0.125D0*CA3*EL*m12squared*SA1*SA&
  &22*dAlpha3PinchOS())/(CB2*MW*SB*SW) - (0.5625D0*CA12*CA3*EL*m12squared*SA1*SA22*dAlpha3PinchOS())/(CB2*MW*SB*SW) - (0.375D0*CA&
  &1*CA3*EL*MH12*SA12*SA22*dAlpha3PinchOS())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*MH32*SA12*SA22*dAlpha3PinchOS())/(MW*SB*SW) - (0.2&
  &8125D0*CA1*EL*m12squared*SA2*SA3*dAlpha3PinchOS())/(MW*SB*SW) - (0.84375D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha3PinchOS())/(M&
  &W*SB*SW) + (0.1875D0*CA1*EL*m12squared*SA2*SA3*dAlpha3PinchOS())/(CB2*MW*SB*SW) + (0.5625D0*CA1*CA22*EL*m12squared*SA2*SA3*dAl&
  &pha3PinchOS())/(CB2*MW*SB*SW) - (0.1875D0*EL*MH12*SA1*SA2*SA3*dAlpha3PinchOS())/(MW*SB*SW) + (0.1875D0*CA12*EL*MH12*SA1*SA2*SA&
  &3*dAlpha3PinchOS())/(MW*SB*SW) - (0.5625D0*CA22*EL*MH12*SA1*SA2*SA3*dAlpha3PinchOS())/(MW*SB*SW) + (0.5625D0*CA12*CA22*EL*MH12&
  &*SA1*SA2*SA3*dAlpha3PinchOS())/(MW*SB*SW) - (0.09375D0*EL*MH32*SA1*SA2*SA3*dAlpha3PinchOS())/(MW*SB*SW) + (0.09375D0*CA12*EL*M&
  &H32*SA1*SA2*SA3*dAlpha3PinchOS())/(MW*SB*SW) - (0.28125D0*CA22*EL*MH32*SA1*SA2*SA3*dAlpha3PinchOS())/(MW*SB*SW) + (0.28125D0*C&
  &A12*CA22*EL*MH32*SA1*SA2*SA3*dAlpha3PinchOS())/(MW*SB*SW) - (0.28125D0*CA1*EL*m12squared*SA12*SA2*SA3*dAlpha3PinchOS())/(CB2*M&
  &W*SB*SW) - (0.84375D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*dAlpha3PinchOS())/(CB2*MW*SB*SW) - (0.125D0*CA1*CA3*EL*m12squared*dA&
  &lpha3PinchOS())/(CB*MW*SB2*SW) - (0.125D0*CA1*CA22*CA3*EL*m12squared*dAlpha3PinchOS())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA3*EL*m&
  &12squared*SA12*dAlpha3PinchOS())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA12*dAlpha3PinchOS())/(CB*MW*SB2*SW) +&
  & (0.125D0*CA1*CA3*EL*m12squared*SA22*dAlpha3PinchOS())/(CB*MW*SB2*SW) + (0.5625D0*CA1*CA3*EL*m12squared*SA12*SA22*dAlpha3Pinch&
  &OS())/(CB*MW*SB2*SW) + (0.1875D0*EL*m12squared*SA1*SA2*SA3*dAlpha3PinchOS())/(CB*MW*SB2*SW) - (0.28125D0*CA12*EL*m12squared*SA&
  &1*SA2*SA3*dAlpha3PinchOS())/(CB*MW*SB2*SW) + (0.5625D0*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha3PinchOS())/(CB*MW*SB2*SW) - (0.84&
  &375D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha3PinchOS())/(CB*MW*SB2*SW) - (0.0625D0*CA1*CA3*EL*m12squared*dAlpha3PinchOS())&
  &/(MW*SB*SW*TB) - (0.0625D0*CA1*CA22*CA3*EL*m12squared*dAlpha3PinchOS())/(MW*SB*SW*TB) + (0.0625D0*CA1*CA3*EL*m12squared*SA22*d&
  &Alpha3PinchOS())/(MW*SB*SW*TB) + (0.09375D0*EL*m12squared*SA1*SA2*SA3*dAlpha3PinchOS())/(MW*SB*SW*TB) + (0.28125D0*CA22*EL*m12&
  &squared*SA1*SA2*SA3*dAlpha3PinchOS())/(MW*SB*SW*TB) + (0.0625D0*CA3*EL*m12squared*SA1*TB*dAlpha3PinchOS())/(CB*MW*SW) + (0.062&
  &5D0*CA22*CA3*EL*m12squared*SA1*TB*dAlpha3PinchOS())/(CB*MW*SW) - (0.0625D0*CA3*EL*m12squared*SA1*SA22*TB*dAlpha3PinchOS())/(CB&
  &*MW*SW) + (0.09375D0*CA1*EL*m12squared*SA2*SA3*TB*dAlpha3PinchOS())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*m12squared*SA2*SA3*TB*&
  &dAlpha3PinchOS())/(CB*MW*SW) + (0.5D0*CA2*MH12*SA3*dAlpha3PinchOS())/vS + (0.25D0*CA2*MH32*SA3*dAlpha3PinchOS())/vS + (1.5D0*C&
  &A2*MH12*SA22*SA3*dAlpha3PinchOS())/vS + (0.75D0*CA2*MH32*SA22*SA3*dAlpha3PinchOS())/vS + (0.09375D0*CA3*EL*MH12*SA1*SA2*dBeta1&
  &PinchOS())/(CB*MW*SW) - (0.09375D0*CA12*CA3*EL*MH12*SA1*SA2*dBeta1PinchOS())/(CB*MW*SW) + (0.28125D0*CA22*CA3*EL*MH12*SA1*SA2*&
  &dBeta1PinchOS())/(CB*MW*SW) - (0.28125D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dBeta1PinchOS())/(CB*MW*SW) + (0.046875D0*CA3*EL*MH32*S&
  &A1*SA2*dBeta1PinchOS())/(CB*MW*SW) - (0.046875D0*CA12*CA3*EL*MH32*SA1*SA2*dBeta1PinchOS())/(CB*MW*SW) + (0.140625D0*CA22*CA3*E&
  &L*MH32*SA1*SA2*dBeta1PinchOS())/(CB*MW*SW) - (0.140625D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*dBeta1PinchOS())/(CB*MW*SW) + (0.0625D0&
  &*CA1*EL*MH12*SA3*dBeta1PinchOS())/(CB*MW*SW) + (0.0625D0*CA1*CA22*EL*MH12*SA3*dBeta1PinchOS())/(CB*MW*SW) + (0.03125D0*CA1*EL*&
  &MH32*SA3*dBeta1PinchOS())/(CB*MW*SW) + (0.03125D0*CA1*CA22*EL*MH32*SA3*dBeta1PinchOS())/(CB*MW*SW) + (0.1875D0*CA1*EL*MH12*SA1&
  &2*SA3*dBeta1PinchOS())/(CB*MW*SW) + (0.1875D0*CA1*CA22*EL*MH12*SA12*SA3*dBeta1PinchOS())/(CB*MW*SW) + (0.09375D0*CA1*EL*MH32*S&
  &A12*SA3*dBeta1PinchOS())/(CB*MW*SW) + (0.09375D0*CA1*CA22*EL*MH32*SA12*SA3*dBeta1PinchOS())/(CB*MW*SW) - (0.0625D0*CA1*EL*MH12&
  &*SA22*SA3*dBeta1PinchOS())/(CB*MW*SW) - (0.03125D0*CA1*EL*MH32*SA22*SA3*dBeta1PinchOS())/(CB*MW*SW) - (0.1875D0*CA1*EL*MH12*SA&
  &12*SA22*SA3*dBeta1PinchOS())/(CB*MW*SW) - (0.09375D0*CA1*EL*MH32*SA12*SA22*SA3*dBeta1PinchOS())/(CB*MW*SW) + (0.09375D0*CA3*EL&
  &*m12squared*SA1*SA2*dBeta1PinchOS())/(MW*SB*SW) + (0.28125D0*CA22*CA3*EL*m12squared*SA1*SA2*dBeta1PinchOS())/(MW*SB*SW) - (0.3&
  &28125D0*CA3*EL*m12squared*SA1*SA2*dBeta1PinchOS())/(CB2*MW*SB*SW) + (0.421875D0*CA12*CA3*EL*m12squared*SA1*SA2*dBeta1PinchOS()&
  &)/(CB2*MW*SB*SW) - (0.984375D0*CA22*CA3*EL*m12squared*SA1*SA2*dBeta1PinchOS())/(CB2*MW*SB*SW) + (1.265625D0*CA12*CA22*CA3*EL*m&
  &12squared*SA1*SA2*dBeta1PinchOS())/(CB2*MW*SB*SW) + (0.0625D0*CA1*EL*m12squared*SA3*dBeta1PinchOS())/(MW*SB*SW) + (0.0625D0*CA&
  &1*CA22*EL*m12squared*SA3*dBeta1PinchOS())/(MW*SB*SW) - (0.21875D0*CA1*EL*m12squared*SA3*dBeta1PinchOS())/(CB2*MW*SB*SW) - (0.2&
  &1875D0*CA1*CA22*EL*m12squared*SA3*dBeta1PinchOS())/(CB2*MW*SB*SW) - (0.84375D0*CA1*EL*m12squared*SA12*SA3*dBeta1PinchOS())/(CB&
  &2*MW*SB*SW) - (0.84375D0*CA1*CA22*EL*m12squared*SA12*SA3*dBeta1PinchOS())/(CB2*MW*SB*SW) - (0.0625D0*CA1*EL*m12squared*SA22*SA&
  &3*dBeta1PinchOS())/(MW*SB*SW) + (0.21875D0*CA1*EL*m12squared*SA22*SA3*dBeta1PinchOS())/(CB2*MW*SB*SW) + (0.84375D0*CA1*EL*m12s&
  &quared*SA12*SA22*SA3*dBeta1PinchOS())/(CB2*MW*SB*SW) + (0.09375D0*CA1*CA3*EL*m12squared*SA2*dBeta1PinchOS())/(CB*MW*SB2*SW) + &
  &(0.28125D0*CA1*CA22*CA3*EL*m12squared*SA2*dBeta1PinchOS())/(CB*MW*SB2*SW) - (0.09375D0*CA3*EL*MH12*SA1*SA2*dBeta1PinchOS())/(C&
  &B*MW*SB2*SW) + (0.09375D0*CA12*CA3*EL*MH12*SA1*SA2*dBeta1PinchOS())/(CB*MW*SB2*SW) - (0.28125D0*CA22*CA3*EL*MH12*SA1*SA2*dBeta&
  &1PinchOS())/(CB*MW*SB2*SW) + (0.28125D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dBeta1PinchOS())/(CB*MW*SB2*SW) - (0.046875D0*CA3*EL*MH3&
  &2*SA1*SA2*dBeta1PinchOS())/(CB*MW*SB2*SW) + (0.046875D0*CA12*CA3*EL*MH32*SA1*SA2*dBeta1PinchOS())/(CB*MW*SB2*SW) - (0.140625D0&
  &*CA22*CA3*EL*MH32*SA1*SA2*dBeta1PinchOS())/(CB*MW*SB2*SW) + (0.140625D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*dBeta1PinchOS())/(CB*MW*&
  &SB2*SW) - (0.28125D0*CA1*CA3*EL*m12squared*SA12*SA2*dBeta1PinchOS())/(CB*MW*SB2*SW) - (0.84375D0*CA1*CA22*CA3*EL*m12squared*SA&
  &12*SA2*dBeta1PinchOS())/(CB*MW*SB2*SW) - (0.0625D0*CA1*EL*MH12*SA3*dBeta1PinchOS())/(CB*MW*SB2*SW) - (0.0625D0*CA1*CA22*EL*MH1&
  &2*SA3*dBeta1PinchOS())/(CB*MW*SB2*SW) - (0.03125D0*CA1*EL*MH32*SA3*dBeta1PinchOS())/(CB*MW*SB2*SW) - (0.03125D0*CA1*CA22*EL*MH&
  &32*SA3*dBeta1PinchOS())/(CB*MW*SB2*SW) - (0.0625D0*EL*m12squared*SA1*SA3*dBeta1PinchOS())/(CB*MW*SB2*SW) - (0.5625D0*CA12*EL*m&
  &12squared*SA1*SA3*dBeta1PinchOS())/(CB*MW*SB2*SW) - (0.0625D0*CA22*EL*m12squared*SA1*SA3*dBeta1PinchOS())/(CB*MW*SB2*SW) - (0.&
  &5625D0*CA12*CA22*EL*m12squared*SA1*SA3*dBeta1PinchOS())/(CB*MW*SB2*SW) - (0.1875D0*CA1*EL*MH12*SA12*SA3*dBeta1PinchOS())/(CB*M&
  &W*SB2*SW) - (0.1875D0*CA1*CA22*EL*MH12*SA12*SA3*dBeta1PinchOS())/(CB*MW*SB2*SW) - (0.09375D0*CA1*EL*MH32*SA12*SA3*dBeta1PinchO&
  &S())/(CB*MW*SB2*SW) - (0.09375D0*CA1*CA22*EL*MH32*SA12*SA3*dBeta1PinchOS())/(CB*MW*SB2*SW) + (0.0625D0*CA1*EL*MH12*SA22*SA3*dB&
  &eta1PinchOS())/(CB*MW*SB2*SW) + (0.03125D0*CA1*EL*MH32*SA22*SA3*dBeta1PinchOS())/(CB*MW*SB2*SW) + (0.0625D0*EL*m12squared*SA1*&
  &SA22*SA3*dBeta1PinchOS())/(CB*MW*SB2*SW) + (0.5625D0*CA12*EL*m12squared*SA1*SA22*SA3*dBeta1PinchOS())/(CB*MW*SB2*SW) + (0.1875&
  &D0*CA1*EL*MH12*SA12*SA22*SA3*dBeta1PinchOS())/(CB*MW*SB2*SW) + (0.09375D0*CA1*EL*MH32*SA12*SA22*SA3*dBeta1PinchOS())/(CB*MW*SB&
  &2*SW) - (0.1875D0*CA1*CA3*EL*m12squared*SA2*dBeta1PinchOS())/(MW*SB*SW*TB) - (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA2*dBeta1Pi&
  &nchOS())/(MW*SB*SW*TB) - (0.09375D0*CA3*EL*MH12*SA1*SA2*dBeta1PinchOS())/(MW*SB*SW*TB) + (0.09375D0*CA12*CA3*EL*MH12*SA1*SA2*d&
  &Beta1PinchOS())/(MW*SB*SW*TB) - (0.28125D0*CA22*CA3*EL*MH12*SA1*SA2*dBeta1PinchOS())/(MW*SB*SW*TB) + (0.28125D0*CA12*CA22*CA3*&
  &EL*MH12*SA1*SA2*dBeta1PinchOS())/(MW*SB*SW*TB) - (0.046875D0*CA3*EL*MH32*SA1*SA2*dBeta1PinchOS())/(MW*SB*SW*TB) + (0.046875D0*&
  &CA12*CA3*EL*MH32*SA1*SA2*dBeta1PinchOS())/(MW*SB*SW*TB) - (0.140625D0*CA22*CA3*EL*MH32*SA1*SA2*dBeta1PinchOS())/(MW*SB*SW*TB) &
  &+ (0.140625D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*dBeta1PinchOS())/(MW*SB*SW*TB) - (0.0625D0*CA1*EL*MH12*SA3*dBeta1PinchOS())/(MW*SB&
  &*SW*TB) - (0.0625D0*CA1*CA22*EL*MH12*SA3*dBeta1PinchOS())/(MW*SB*SW*TB) - (0.03125D0*CA1*EL*MH32*SA3*dBeta1PinchOS())/(MW*SB*S&
  &W*TB) - (0.03125D0*CA1*CA22*EL*MH32*SA3*dBeta1PinchOS())/(MW*SB*SW*TB) + (0.125D0*EL*m12squared*SA1*SA3*dBeta1PinchOS())/(MW*S&
  &B*SW*TB) + (0.125D0*CA22*EL*m12squared*SA1*SA3*dBeta1PinchOS())/(MW*SB*SW*TB) - (0.1875D0*CA1*EL*MH12*SA12*SA3*dBeta1PinchOS()&
  &)/(MW*SB*SW*TB) - (0.1875D0*CA1*CA22*EL*MH12*SA12*SA3*dBeta1PinchOS())/(MW*SB*SW*TB) - (0.09375D0*CA1*EL*MH32*SA12*SA3*dBeta1P&
  &inchOS())/(MW*SB*SW*TB) - (0.09375D0*CA1*CA22*EL*MH32*SA12*SA3*dBeta1PinchOS())/(MW*SB*SW*TB) + (0.0625D0*CA1*EL*MH12*SA22*SA3&
  &*dBeta1PinchOS())/(MW*SB*SW*TB) + (0.03125D0*CA1*EL*MH32*SA22*SA3*dBeta1PinchOS())/(MW*SB*SW*TB) - (0.125D0*EL*m12squared*SA1*&
  &SA22*SA3*dBeta1PinchOS())/(MW*SB*SW*TB) + (0.1875D0*CA1*EL*MH12*SA12*SA22*SA3*dBeta1PinchOS())/(MW*SB*SW*TB) + (0.09375D0*CA1*&
  &EL*MH32*SA12*SA22*SA3*dBeta1PinchOS())/(MW*SB*SW*TB) + (0.1875D0*CA1*CA3*EL*MH12*SA2*TB*dBeta1PinchOS())/(CB*MW*SW) + (0.5625D&
  &0*CA1*CA22*CA3*EL*MH12*SA2*TB*dBeta1PinchOS())/(CB*MW*SW) + (0.09375D0*CA1*CA3*EL*MH32*SA2*TB*dBeta1PinchOS())/(CB*MW*SW) + (0&
  &.28125D0*CA1*CA22*CA3*EL*MH32*SA2*TB*dBeta1PinchOS())/(CB*MW*SW) + (0.328125D0*CA3*EL*m12squared*SA1*SA2*TB*dBeta1PinchOS())/(&
  &CB*MW*SW) + (0.984375D0*CA22*CA3*EL*m12squared*SA1*SA2*TB*dBeta1PinchOS())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*MH12*SA12*SA2*TB*&
  &dBeta1PinchOS())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*EL*MH12*SA12*SA2*TB*dBeta1PinchOS())/(CB*MW*SW) - (0.09375D0*CA1*CA3*EL*M&
  &H32*SA12*SA2*TB*dBeta1PinchOS())/(CB*MW*SW) - (0.28125D0*CA1*CA22*CA3*EL*MH32*SA12*SA2*TB*dBeta1PinchOS())/(CB*MW*SW) + (0.218&
  &75D0*CA1*EL*m12squared*SA3*TB*dBeta1PinchOS())/(CB*MW*SW) + (0.21875D0*CA1*CA22*EL*m12squared*SA3*TB*dBeta1PinchOS())/(CB*MW*S&
  &W) - (0.125D0*EL*MH12*SA1*SA3*TB*dBeta1PinchOS())/(CB*MW*SW) - (0.375D0*CA12*EL*MH12*SA1*SA3*TB*dBeta1PinchOS())/(CB*MW*SW) - &
  &(0.125D0*CA22*EL*MH12*SA1*SA3*TB*dBeta1PinchOS())/(CB*MW*SW) - (0.375D0*CA12*CA22*EL*MH12*SA1*SA3*TB*dBeta1PinchOS())/(CB*MW*S&
  &W) - (0.0625D0*EL*MH32*SA1*SA3*TB*dBeta1PinchOS())/(CB*MW*SW) - (0.1875D0*CA12*EL*MH32*SA1*SA3*TB*dBeta1PinchOS())/(CB*MW*SW) &
  &- (0.0625D0*CA22*EL*MH32*SA1*SA3*TB*dBeta1PinchOS())/(CB*MW*SW) - (0.1875D0*CA12*CA22*EL*MH32*SA1*SA3*TB*dBeta1PinchOS())/(CB*&
  &MW*SW) - (0.21875D0*CA1*EL*m12squared*SA22*SA3*TB*dBeta1PinchOS())/(CB*MW*SW) + (0.125D0*EL*MH12*SA1*SA22*SA3*TB*dBeta1PinchOS&
  &())/(CB*MW*SW) + (0.375D0*CA12*EL*MH12*SA1*SA22*SA3*TB*dBeta1PinchOS())/(CB*MW*SW) + (0.0625D0*EL*MH32*SA1*SA22*SA3*TB*dBeta1P&
  &inchOS())/(CB*MW*SW) + (0.1875D0*CA12*EL*MH32*SA1*SA22*SA3*TB*dBeta1PinchOS())/(CB*MW*SW) + (0.140625D0*CA3*EL*m12squared*SA1*&
  &SA2*dBeta1PinchOS())/(MW*SB*SW*TB2) + (0.421875D0*CA22*CA3*EL*m12squared*SA1*SA2*dBeta1PinchOS())/(MW*SB*SW*TB2) + (0.09375D0*&
  &CA1*EL*m12squared*SA3*dBeta1PinchOS())/(MW*SB*SW*TB2) + (0.09375D0*CA1*CA22*EL*m12squared*SA3*dBeta1PinchOS())/(MW*SB*SW*TB2) &
  &- (0.09375D0*CA1*EL*m12squared*SA22*SA3*dBeta1PinchOS())/(MW*SB*SW*TB2) - (0.1875D0*CA1*CA3*EL*m12squared*SA2*TB2*dBeta1PinchO&
  &S())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA2*TB2*dBeta1PinchOS())/(CB*MW*SW) + (0.125D0*EL*m12squared*SA1*SA3*TB&
  &2*dBeta1PinchOS())/(CB*MW*SW) + (0.125D0*CA22*EL*m12squared*SA1*SA3*TB2*dBeta1PinchOS())/(CB*MW*SW) - (0.125D0*EL*m12squared*S&
  &A1*SA22*SA3*TB2*dBeta1PinchOS())/(CB*MW*SW) + (0.125D0*CA2*CA3*MH12*dBeta1PinchOS())/(CB*SB*vS) + (0.0625D0*CA2*CA3*MH32*dBeta&
  &1PinchOS())/(CB*SB*vS) + (0.375D0*CA2*CA3*MH12*SA22*dBeta1PinchOS())/(CB*SB*vS) + (0.1875D0*CA2*CA3*MH32*SA22*dBeta1PinchOS())&
  &/(CB*SB*vS) - (0.125D0*CA2*CA3*MH12*dBeta1PinchOS())/(TB*vS) - (0.0625D0*CA2*CA3*MH32*dBeta1PinchOS())/(TB*vS) - (0.375D0*CA2*&
  &CA3*MH12*SA22*dBeta1PinchOS())/(TB*vS) - (0.1875D0*CA2*CA3*MH32*SA22*dBeta1PinchOS())/(TB*vS) - (0.125D0*CA2*CA3*MH12*TB*dBeta&
  &1PinchOS())/vS - (0.0625D0*CA2*CA3*MH32*TB*dBeta1PinchOS())/vS - (0.375D0*CA2*CA3*MH12*SA22*TB*dBeta1PinchOS())/vS - (0.1875D0&
  &*CA2*CA3*MH32*SA22*TB*dBeta1PinchOS())/vS - (0.375D0*EL*MH12*SA3*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.375D0*&
  &CA22*EL*MH12*SA3*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*EL*MH32*SA3*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0&
  &)))/(CB*MW*SW) - (0.1875D0*CA22*EL*MH32*SA3*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.375D0*EL*MH12*SA22*SA3*dAlp&
  &ha1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*EL*MH32*SA22*SA3*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) -&
  & (0.1875D0*CA3*EL*MH12*SA2*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA22*CA3*EL*MH12*SA2*dAlpha1PinchOS()&
  &*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA3*EL*MH32*SA2*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*&
  &CA22*CA3*EL*MH32*SA2*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.5625D0*EL*m12squared*SA3*dAlpha1PinchOS()*DBLE(CA1&
  &**INT(3.D0)))/(CB2*MW*SB*SW) + (0.5625D0*CA22*EL*m12squared*SA3*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.562&
  &5D0*EL*m12squared*SA22*SA3*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.28125D0*CA3*EL*m12squared*SA2*dAlpha1Pin&
  &chOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA22*CA3*EL*m12squared*SA2*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB&
  &*MW*SB2*SW) + (0.0625D0*CA2*CA3*EL*MH12*dAlpha2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA2*CA3*EL*MH32*dAlpha2&
  &PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.5625D0*CA2*CA3*EL*MH12*SA22*dAlpha2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) &
  &- (0.28125D0*CA2*CA3*EL*MH32*SA22*dAlpha2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA2*CA3*EL*m12squared*dAlpha2&
  &PinchOS()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.84375D0*CA2*CA3*EL*m12squared*SA22*dAlpha2PinchOS()*DBLE(CA1**INT(3.D0)))/&
  &(CB2*MW*SB*SW) + (0.5D0*CA2*EL*MH12*SA2*SA3*dAlpha2PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.25D0*CA2*EL*MH32*SA2*SA3*dA&
  &lpha2PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.75D0*CA2*EL*m12squared*SA2*SA3*dAlpha2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB&
  &*MW*SB2*SW) - (0.0625D0*EL*MH12*SA2*SA3*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA22*EL*MH12*SA2*SA3*dAl&
  &pha3PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*EL*MH32*SA2*SA3*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) &
  &- (0.09375D0*CA22*EL*MH32*SA2*SA3*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.125D0*CA3*EL*MH12*dAlpha3PinchOS()*DB&
  &LE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.125D0*CA22*CA3*EL*MH12*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.0625D0*CA3*E&
  &L*MH32*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.0625D0*CA22*CA3*EL*MH32*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(&
  &MW*SB*SW) + (0.125D0*CA3*EL*MH12*SA22*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*CA3*EL*MH32*SA22*dAlpha3Pi&
  &nchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*EL*m12squared*SA2*SA3*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*&
  &SW) + (0.28125D0*CA22*EL*m12squared*SA2*SA3*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*m12square&
  &d*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*CA22*CA3*EL*m12squared*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0&
  &)))/(CB*MW*SB2*SW) - (0.1875D0*CA3*EL*m12squared*SA22*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.0625D0*EL*MH1&
  &2*SA3*dBeta1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.0625D0*CA22*EL*MH12*SA3*dBeta1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*&
  &MW*SW) - (0.03125D0*EL*MH32*SA3*dBeta1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*CA22*EL*MH32*SA3*dBeta1PinchOS()&
  &*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*EL*MH12*SA22*SA3*dBeta1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*E&
  &L*MH32*SA22*SA3*dBeta1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*EL*m12squared*SA3*dBeta1PinchOS()*DBLE(CA1**INT(&
  &3.D0)))/(CB2*MW*SB*SW) + (0.28125D0*CA22*EL*m12squared*SA3*dBeta1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*E&
  &L*m12squared*SA22*SA3*dBeta1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.09375D0*CA3*EL*m12squared*SA2*dBeta1PinchOS()*&
  &DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA22*CA3*EL*m12squared*SA2*dBeta1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*&
  &SW) + (0.0625D0*EL*MH12*SA3*dBeta1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*CA22*EL*MH12*SA3*dBeta1PinchOS()*&
  &DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.03125D0*EL*MH32*SA3*dBeta1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.03125D&
  &0*CA22*EL*MH32*SA3*dBeta1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.0625D0*EL*MH12*SA22*SA3*dBeta1PinchOS()*DBLE(CA1*&
  &*INT(3.D0)))/(CB*MW*SB2*SW) - (0.03125D0*EL*MH32*SA22*SA3*dBeta1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*EL*&
  &MH12*SA3*dBeta1PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.0625D0*CA22*EL*MH12*SA3*dBeta1PinchOS()*DBLE(CA1**INT(3.D0))&
  &)/(MW*SB*SW*TB) + (0.03125D0*EL*MH32*SA3*dBeta1PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.03125D0*CA22*EL*MH32*SA3*dBe&
  &ta1PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.0625D0*EL*MH12*SA22*SA3*dBeta1PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*&
  &TB) - (0.03125D0*EL*MH32*SA22*SA3*dBeta1PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.0625D0*CA3*EL*MH12*SA2*TB*dBeta1Pin&
  &chOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA22*CA3*EL*MH12*SA2*TB*dBeta1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) +&
  & (0.03125D0*CA3*EL*MH32*SA2*TB*dBeta1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA22*CA3*EL*MH32*SA2*TB*dBeta1Pin&
  &chOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA1*CA3*EL*MH12*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.281&
  &25D0*CA1*CA3*EL*MH32*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.84375D0*CA3*EL*m12squared*SA1*dAlpha2PinchOS()*DBL&
  &E(CA2**INT(3.D0)))/(CB*MW*SW) - (0.5625D0*CA1*CA3*EL*MH12*SA12*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*&
  &CA1*CA3*EL*MH32*SA12*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.84375D0*CA1*CA3*EL*m12squared*dAlpha2PinchOS()*DBL&
  &E(CA2**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA1*CA3*EL*m12squared*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.562&
  &5D0*CA3*EL*MH12*SA1*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA12*CA3*EL*MH12*SA1*dAlpha2PinchOS()*DBLE(C&
  &A2**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*CA3*EL*MH32*SA1*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA12*CA&
  &3*EL*MH32*SA1*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) + (0.84375D0*CA1*CA3*EL*m12squared*SA12*dAlpha2PinchOS()*DBLE(&
  &CA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA3*EL*m12squared*SA1*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.8&
  &4375D0*CA12*CA3*EL*m12squared*SA1*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125D0*CA3*EL*m12squared*SA1*dAl&
  &pha2PinchOS()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.28125D0*CA1*CA3*EL*m12squared*TB*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))&
  &/(CB*MW*SW) - (0.5D0*MH12*SA3*dAlpha3PinchOS()*DBLE(CA2**INT(3.D0)))/vS - (0.25D0*MH32*SA3*dAlpha3PinchOS()*DBLE(CA2**INT(3.D0&
  &)))/vS - (0.125D0*CA3*MH12*dBeta1PinchOS()*DBLE(CA2**INT(3.D0)))/(CB*SB*vS) - (0.0625D0*CA3*MH32*dBeta1PinchOS()*DBLE(CA2**INT&
  &(3.D0)))/(CB*SB*vS) + (0.125D0*CA3*MH12*dBeta1PinchOS()*DBLE(CA2**INT(3.D0)))/(TB*vS) + (0.0625D0*CA3*MH32*dBeta1PinchOS()*DBL&
  &E(CA2**INT(3.D0)))/(TB*vS) + (0.125D0*CA3*MH12*TB*dBeta1PinchOS()*DBLE(CA2**INT(3.D0)))/vS + (0.0625D0*CA3*MH32*TB*dBeta1Pinch&
  &OS()*DBLE(CA2**INT(3.D0)))/vS + (0.1875D0*CA3*EL*MH12*dAlpha2PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) +&
  & (0.09375D0*CA3*EL*MH32*dAlpha2PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA3*EL*m12squared*&
  &dAlpha2PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.375D0*CA1*CA3*EL*m12squared*SA2*dBeta1PinchOS(&
  &)*DBLE(CB**INT(-3.D0)))/(MW*SW) - (1.125D0*CA1*CA22*CA3*EL*m12squared*SA2*dBeta1PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.5&
  &625D0*CA1*CA3*EL*m12squared*SA12*SA2*dBeta1PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (1.6875D0*CA1*CA22*CA3*EL*m12squared*SA12&
  &*SA2*dBeta1PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.25D0*EL*m12squared*SA1*SA3*dBeta1PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*S&
  &W) + (1.125D0*CA12*EL*m12squared*SA1*SA3*dBeta1PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.25D0*CA22*EL*m12squared*SA1*SA3*dB&
  &eta1PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (1.125D0*CA12*CA22*EL*m12squared*SA1*SA3*dBeta1PinchOS()*DBLE(CB**INT(-3.D0)))/(&
  &MW*SW) - (0.25D0*EL*m12squared*SA1*SA22*SA3*dBeta1PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (1.125D0*CA12*EL*m12squared*SA1*SA&
  &22*SA3*dBeta1PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.1875D0*CA3*EL*m12squared*SA2*dBeta1PinchOS()*DBLE(CA1**INT(3.D0))*DB&
  &LE(CB**INT(-3.D0)))/(MW*SW) - (0.5625D0*CA22*CA3*EL*m12squared*SA2*dBeta1PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/&
  &(MW*SW) + (0.1875D0*CA3*EL*MH12*SA2*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA22*CA3*EL*MH12*SA2*dAlpha1&
  &PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA3*EL*MH32*SA2*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0&
  &.28125D0*CA22*CA3*EL*MH32*SA2*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA3*EL*m12squared*SA2*dAlpha1Pinc&
  &hOS()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.84375D0*CA22*CA3*EL*m12squared*SA2*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB2&
  &*MW*SB*SW) - (0.375D0*EL*MH12*SA3*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.375D0*CA22*EL*MH12*SA3*dAlpha1PinchOS&
  &()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*EL*MH32*SA3*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA22&
  &*EL*MH32*SA3*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.375D0*EL*MH12*SA22*SA3*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0&
  &)))/(MW*SB*SW) + (0.1875D0*EL*MH32*SA22*SA3*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.5625D0*EL*m12squared*SA3*dA&
  &lpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.5625D0*CA22*EL*m12squared*SA3*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/&
  &(CB*MW*SB2*SW) - (0.5625D0*EL*m12squared*SA22*SA3*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.5D0*CA2*EL*MH12*S&
  &A2*SA3*dAlpha2PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.25D0*CA2*EL*MH32*SA2*SA3*dAlpha2PinchOS()*DBLE(SA1**INT(3.D0)))/&
  &(CB*MW*SW) + (0.0625D0*CA2*CA3*EL*MH12*dAlpha2PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA2*CA3*EL*MH32*dAlpha2P&
  &inchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA2*CA3*EL*MH12*SA22*dAlpha2PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) -&
  & (0.28125D0*CA2*CA3*EL*MH32*SA22*dAlpha2PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.75D0*CA2*EL*m12squared*SA2*SA3*dAlpha2&
  &PinchOS()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA2*CA3*EL*m12squared*dAlpha2PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*M&
  &W*SB2*SW) + (0.84375D0*CA2*CA3*EL*m12squared*SA22*dAlpha2PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.125D0*CA3*EL*MH12&
  &*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.125D0*CA22*CA3*EL*MH12*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*S&
  &W) + (0.0625D0*CA3*EL*MH32*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA22*CA3*EL*MH32*dAlpha3PinchOS()*DBL&
  &E(SA1**INT(3.D0)))/(CB*MW*SW) - (0.125D0*CA3*EL*MH12*SA22*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.0625D0*CA3*EL&
  &*MH32*SA22*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA3*EL*m12squared*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0&
  &)))/(CB2*MW*SB*SW) - (0.1875D0*CA22*CA3*EL*m12squared*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL&
  &*m12squared*SA22*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D0*EL*MH12*SA2*SA3*dAlpha3PinchOS()*DBLE(SA1**&
  &INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA22*EL*MH12*SA2*SA3*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.03125D0*EL*MH32&
  &*SA2*SA3*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA22*EL*MH32*SA2*SA3*dAlpha3PinchOS()*DBLE(SA1**INT(3.&
  &D0)))/(MW*SB*SW) + (0.09375D0*EL*m12squared*SA2*SA3*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA22*EL&
  &*m12squared*SA2*SA3*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.03125D0*CA3*EL*MH12*SA2*dBeta1PinchOS()*DBLE(SA&
  &1**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA22*CA3*EL*MH12*SA2*dBeta1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.015625D0*CA3&
  &*EL*MH32*SA2*dBeta1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.046875D0*CA22*CA3*EL*MH32*SA2*dBeta1PinchOS()*DBLE(SA1**INT&
  &(3.D0)))/(CB*MW*SW) - (0.140625D0*CA3*EL*m12squared*SA2*dBeta1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.421875D0*CA2&
  &2*CA3*EL*m12squared*SA2*dBeta1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.03125D0*CA3*EL*MH12*SA2*dBeta1PinchOS()*DBLE&
  &(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.09375D0*CA22*CA3*EL*MH12*SA2*dBeta1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.0&
  &15625D0*CA3*EL*MH32*SA2*dBeta1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.046875D0*CA22*CA3*EL*MH32*SA2*dBeta1PinchOS(&
  &)*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*EL*m12squared*SA3*dBeta1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0&
  &.1875D0*CA22*EL*m12squared*SA3*dBeta1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.1875D0*EL*m12squared*SA22*SA3*dBeta1P&
  &inchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.03125D0*CA3*EL*MH12*SA2*dBeta1PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB)&
  & - (0.09375D0*CA22*CA3*EL*MH12*SA2*dBeta1PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.015625D0*CA3*EL*MH32*SA2*dBeta1Pin&
  &chOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.046875D0*CA22*CA3*EL*MH32*SA2*dBeta1PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*&
  &TB) + (0.125D0*EL*MH12*SA3*TB*dBeta1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.125D0*CA22*EL*MH12*SA3*TB*dBeta1PinchOS()*&
  &DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*EL*MH32*SA3*TB*dBeta1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA22*&
  &EL*MH32*SA3*TB*dBeta1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.125D0*EL*MH12*SA22*SA3*TB*dBeta1PinchOS()*DBLE(SA1**INT(3&
  &.D0)))/(CB*MW*SW) - (0.0625D0*EL*MH32*SA22*SA3*TB*dBeta1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA3*EL*MH12*dAl&
  &pha2PinchOS()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA3*EL*MH32*dAlpha2PinchOS()*DBLE(CA2**INT(3.&
  &D0))*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)&
  &))/(CB*MW*SB2*SW) - (0.375D0*EL*m12squared*SA3*dBeta1PinchOS()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) - (0.375D0*C&
  &A22*EL*m12squared*SA3*dBeta1PinchOS()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) + (0.375D0*EL*m12squared*SA22*SA3*dBe&
  &ta1PinchOS()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) - (0.28125D0*CA1*CA3*EL*m12squared*dAlpha1PinchOS()*DBLE(SA2**&
  &INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA3*EL*MH12*SA1*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA12*CA3*EL*M&
  &H12*SA1*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA3*EL*MH32*SA1*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/&
  &(CB*MW*SW) + (0.28125D0*CA12*CA3*EL*MH32*SA1*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*MH12*dAl&
  &pha1PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA1*CA3*EL*MH32*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) &
  &+ (0.28125D0*CA3*EL*m12squared*SA1*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA3*EL*m12squared*SA1*dAlpha1&
  &PinchOS()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.84375D0*CA12*CA3*EL*m12squared*SA1*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/&
  &(CB2*MW*SB*SW) - (0.5625D0*CA1*CA3*EL*MH12*SA12*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA1*CA3*EL*MH32&
  &*SA12*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*m12squared*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)&
  &))/(CB*MW*SB2*SW) + (0.84375D0*CA1*CA3*EL*m12squared*SA12*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.09375D0*C&
  &A1*CA3*EL*m12squared*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA3*EL*m12squared*SA1*TB*dAlpha1PinchOS&
  &()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (1.5D0*CA3*MH12*dAlpha2PinchOS()*DBLE(SA2**INT(3.D0)))/vS + (0.75D0*CA3*MH32*dAlpha2Pinc&
  &hOS()*DBLE(SA2**INT(3.D0)))/vS + (0.1875D0*CA1*EL*MH12*SA3*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1*&
  &EL*MH32*SA3*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*EL*m12squared*SA1*SA3*dAlpha3PinchOS()*DBLE(SA2**IN&
  &T(3.D0)))/(CB*MW*SW) - (0.1875D0*CA1*EL*MH12*SA12*SA3*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1*EL*MH&
  &32*SA12*SA3*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA1*EL*m12squared*SA3*dAlpha3PinchOS()*DBLE(SA2**IN&
  &T(3.D0)))/(MW*SB*SW) - (0.1875D0*CA1*EL*m12squared*SA3*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*EL*MH&
  &12*SA1*SA3*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA12*EL*MH12*SA1*SA3*dAlpha3PinchOS()*DBLE(SA2**INT(3&
  &.D0)))/(MW*SB*SW) + (0.09375D0*EL*MH32*SA1*SA3*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA12*EL*MH32*SA1&
  &*SA3*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*CA1*EL*m12squared*SA12*SA3*dAlpha3PinchOS()*DBLE(SA2**INT(&
  &3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*EL*m12squared*SA1*SA3*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA&
  &12*EL*m12squared*SA1*SA3*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.09375D0*EL*m12squared*SA1*SA3*dAlpha3Pinch&
  &OS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA1*EL*m12squared*SA3*TB*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*S&
  &W) - (0.09375D0*CA3*EL*MH12*SA1*dBeta1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA12*CA3*EL*MH12*SA1*dBeta1Pinch&
  &OS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.046875D0*CA3*EL*MH32*SA1*dBeta1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.0468&
  &75D0*CA12*CA3*EL*MH32*SA1*dBeta1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA3*EL*m12squared*SA1*dBeta1PinchOS()*&
  &DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.328125D0*CA3*EL*m12squared*SA1*dBeta1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0&
  &.421875D0*CA12*CA3*EL*m12squared*SA1*dBeta1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA1*CA3*EL*m12squared*d&
  &Beta1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.09375D0*CA3*EL*MH12*SA1*dBeta1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*&
  &SB2*SW) - (0.09375D0*CA12*CA3*EL*MH12*SA1*dBeta1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.046875D0*CA3*EL*MH32*SA1*d&
  &Beta1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.046875D0*CA12*CA3*EL*MH32*SA1*dBeta1PinchOS()*DBLE(SA2**INT(3.D0)))/(&
  &CB*MW*SB2*SW) + (0.28125D0*CA1*CA3*EL*m12squared*SA12*dBeta1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*CA1*CA3&
  &*EL*m12squared*dBeta1PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.09375D0*CA3*EL*MH12*SA1*dBeta1PinchOS()*DBLE(SA2**INT(&
  &3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA12*CA3*EL*MH12*SA1*dBeta1PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.046875D0*CA3*&
  &EL*MH32*SA1*dBeta1PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.046875D0*CA12*CA3*EL*MH32*SA1*dBeta1PinchOS()*DBLE(SA2**I&
  &NT(3.D0)))/(MW*SB*SW*TB) - (0.1875D0*CA1*CA3*EL*MH12*TB*dBeta1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1*CA3*&
  &EL*MH32*TB*dBeta1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.328125D0*CA3*EL*m12squared*SA1*TB*dBeta1PinchOS()*DBLE(SA2**I&
  &NT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA1*CA3*EL*MH12*SA12*TB*dBeta1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1*CA&
  &3*EL*MH32*SA12*TB*dBeta1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.140625D0*CA3*EL*m12squared*SA1*dBeta1PinchOS()*DBLE(SA&
  &2**INT(3.D0)))/(MW*SB*SW*TB2) + (0.1875D0*CA1*CA3*EL*m12squared*TB2*dBeta1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875&
  &D0*CA3*EL*MH12*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA3*EL*MH32*dAlpha1PinchOS(&
  &)*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0))*D&
  &BLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*EL*MH12*SA3*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW&
  &*SW) + (0.03125D0*EL*MH32*SA3*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*EL*m12square&
  &d*SA3*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA3*EL*m12squared*dBeta1PinchOS(&
  &)*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.0625D0*CA3*EL*MH12*TB*dBeta1PinchOS()*DBLE(CA1**INT(3.D0))*DB&
  &LE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*CA3*EL*MH32*TB*dBeta1PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*S&
  &W) + (0.375D0*CA1*CA3*EL*m12squared*dBeta1PinchOS()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) - (0.5625D0*CA1*CA3*EL*&
  &m12squared*SA12*dBeta1PinchOS()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) + (0.1875D0*CA3*EL*m12squared*dBeta1PinchOS&
  &()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) - (0.1875D0*CA3*EL*MH12*dAlpha1PinchOS()*DBLE(SA1**&
  &INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA3*EL*MH32*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)&
  &))/(CB*MW*SW) + (0.28125D0*CA3*EL*m12squared*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.0&
  &625D0*EL*MH12*SA3*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*EL*MH32*SA3*dAlpha3Pinch&
  &OS()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*EL*m12squared*SA3*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)&
  &)*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.03125D0*CA3*EL*MH12*dBeta1PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB&
  &*MW*SW) - (0.015625D0*CA3*EL*MH32*dBeta1PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.140625D0*CA3*EL*m&
  &12squared*dBeta1PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.03125D0*CA3*EL*MH12*dBeta1PinchOS()*D&
  &BLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.015625D0*CA3*EL*MH32*dBeta1PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(S&
  &A2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.03125D0*CA3*EL*MH12*dBeta1PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*T&
  &B) + (0.015625D0*CA3*EL*MH32*dBeta1PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.328125D0*CA3*EL*m12&
  &squared*SA1*SA2*dBeta1PinchOS()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.421875D0*CA12*CA3*EL*m12squared*SA1*SA2*dBeta1PinchOS()*DBL&
  &E(SB**INT(-3.D0)))/(MW*SW) + (0.984375D0*CA22*CA3*EL*m12squared*SA1*SA2*dBeta1PinchOS()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (1.265&
  &625D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*dBeta1PinchOS()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.09375D0*CA3*EL*m12squared*SA1*SA2&
  &*dBeta1PinchOS()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.140625D0*CA12*CA3*EL*m12squared*SA1*SA2*dBeta1PinchOS()*DBLE(SB**INT(-&
  &3.D0)))/(CB2*MW*SW) + (0.28125D0*CA22*CA3*EL*m12squared*SA1*SA2*dBeta1PinchOS()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.421875D&
  &0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*dBeta1PinchOS()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.21875D0*CA1*EL*m12squared*SA3*dBe&
  &ta1PinchOS()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.21875D0*CA1*CA22*EL*m12squared*SA3*dBeta1PinchOS()*DBLE(SB**INT(-3.D0)))/(MW*S&
  &W) + (0.0625D0*CA1*EL*m12squared*SA3*dBeta1PinchOS()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.0625D0*CA1*CA22*EL*m12squared*SA3*&
  &dBeta1PinchOS()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.84375D0*CA1*EL*m12squared*SA12*SA3*dBeta1PinchOS()*DBLE(SB**INT(-3.D0))&
  &)/(MW*SW) + (0.84375D0*CA1*CA22*EL*m12squared*SA12*SA3*dBeta1PinchOS()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.28125D0*CA1*EL*m12sq&
  &uared*SA12*SA3*dBeta1PinchOS()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.28125D0*CA1*CA22*EL*m12squared*SA12*SA3*dBeta1PinchOS()*&
  &DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.21875D0*CA1*EL*m12squared*SA22*SA3*dBeta1PinchOS()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.0&
  &625D0*CA1*EL*m12squared*SA22*SA3*dBeta1PinchOS()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.84375D0*CA1*EL*m12squared*SA12*SA22*SA&
  &3*dBeta1PinchOS()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.28125D0*CA1*EL*m12squared*SA12*SA22*SA3*dBeta1PinchOS()*DBLE(SB**INT(-3.D&
  &0)))/(CB2*MW*SW) - (0.28125D0*EL*m12squared*SA3*dBeta1PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.28125D&
  &0*CA22*EL*m12squared*SA3*dBeta1PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.09375D0*EL*m12squared*SA3*dBe&
  &ta1PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.09375D0*CA22*EL*m12squared*SA3*dBeta1PinchOS()*DBLE(C&
  &A1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.28125D0*EL*m12squared*SA22*SA3*dBeta1PinchOS()*DBLE(CA1**INT(3.D0))*DBLE&
  &(SB**INT(-3.D0)))/(MW*SW) + (0.09375D0*EL*m12squared*SA22*SA3*dBeta1PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*&
  &MW*SW) + (0.140625D0*CA3*EL*m12squared*SA2*dBeta1PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.421875D0*CA&
  &22*CA3*EL*m12squared*SA2*dBeta1PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.046875D0*CA3*EL*m12squared*SA&
  &2*dBeta1PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.140625D0*CA22*CA3*EL*m12squared*SA2*dBeta1PinchO&
  &S()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.328125D0*CA3*EL*m12squared*SA1*dBeta1PinchOS()*DBLE(SA2**INT(3&
  &.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.421875D0*CA12*CA3*EL*m12squared*SA1*dBeta1PinchOS()*DBLE(SA2**INT(3.D0))*DBLE(SB**INT&
  &(-3.D0)))/(MW*SW) - (0.09375D0*CA3*EL*m12squared*SA1*dBeta1PinchOS()*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + &
  &(0.140625D0*CA12*CA3*EL*m12squared*SA1*dBeta1PinchOS()*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.140625D0*CA&
  &3*EL*m12squared*dBeta1PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.046875D0*CA3*EL*m&
  &12squared*dBeta1PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.1875D0*CA1*CA3*MH12&
  &*SA2*dgAtMZ())/(CB*MW) + (0.5625D0*CA1*CA22*CA3*MH12*SA2*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*CA3*MH32*SA2*dgAtMZ())/(CB*MW) + (&
  &0.28125D0*CA1*CA22*CA3*MH32*SA2*dgAtMZ())/(CB*MW) + (0.28125D0*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*MW) + (0.84375D0*CA22*CA3*&
  &m12squared*SA1*SA2*dgAtMZ())/(CB*MW) - (0.1875D0*CA1*CA3*MH12*SA12*SA2*dgAtMZ())/(CB*MW) - (0.5625D0*CA1*CA22*CA3*MH12*SA12*SA&
  &2*dgAtMZ())/(CB*MW) - (0.09375D0*CA1*CA3*MH32*SA12*SA2*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*CA3*MH32*SA12*SA2*dgAtMZ())/(CB&
  &*MW) + (0.1875D0*CA1*m12squared*SA3*dgAtMZ())/(CB*MW) + (0.1875D0*CA1*CA22*m12squared*SA3*dgAtMZ())/(CB*MW) - (0.125D0*MH12*SA&
  &1*SA3*dgAtMZ())/(CB*MW) - (0.375D0*CA12*MH12*SA1*SA3*dgAtMZ())/(CB*MW) - (0.125D0*CA22*MH12*SA1*SA3*dgAtMZ())/(CB*MW) - (0.375&
  &D0*CA12*CA22*MH12*SA1*SA3*dgAtMZ())/(CB*MW) - (0.0625D0*MH32*SA1*SA3*dgAtMZ())/(CB*MW) - (0.1875D0*CA12*MH32*SA1*SA3*dgAtMZ())&
  &/(CB*MW) - (0.0625D0*CA22*MH32*SA1*SA3*dgAtMZ())/(CB*MW) - (0.1875D0*CA12*CA22*MH32*SA1*SA3*dgAtMZ())/(CB*MW) - (0.1875D0*CA1*&
  &m12squared*SA22*SA3*dgAtMZ())/(CB*MW) + (0.125D0*MH12*SA1*SA22*SA3*dgAtMZ())/(CB*MW) + (0.375D0*CA12*MH12*SA1*SA22*SA3*dgAtMZ(&
  &))/(CB*MW) + (0.0625D0*MH32*SA1*SA22*SA3*dgAtMZ())/(CB*MW) + (0.1875D0*CA12*MH32*SA1*SA22*SA3*dgAtMZ())/(CB*MW) + (0.28125D0*C&
  &A1*CA3*m12squared*SA2*dgAtMZ())/(MW*SB) + (0.84375D0*CA1*CA22*CA3*m12squared*SA2*dgAtMZ())/(MW*SB) - (0.1875D0*CA1*CA3*m12squa&
  &red*SA2*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA1*CA22*CA3*m12squared*SA2*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*CA3*MH12*SA1*SA2*dgAtM&
  &Z())/(MW*SB) - (0.1875D0*CA12*CA3*MH12*SA1*SA2*dgAtMZ())/(MW*SB) + (0.5625D0*CA22*CA3*MH12*SA1*SA2*dgAtMZ())/(MW*SB) - (0.5625&
  &D0*CA12*CA22*CA3*MH12*SA1*SA2*dgAtMZ())/(MW*SB) + (0.09375D0*CA3*MH32*SA1*SA2*dgAtMZ())/(MW*SB) - (0.09375D0*CA12*CA3*MH32*SA1&
  &*SA2*dgAtMZ())/(MW*SB) + (0.28125D0*CA22*CA3*MH32*SA1*SA2*dgAtMZ())/(MW*SB) - (0.28125D0*CA12*CA22*CA3*MH32*SA1*SA2*dgAtMZ())/&
  &(MW*SB) + (0.28125D0*CA1*CA3*m12squared*SA12*SA2*dgAtMZ())/(CB2*MW*SB) + (0.84375D0*CA1*CA22*CA3*m12squared*SA12*SA2*dgAtMZ())&
  &/(CB2*MW*SB) + (0.125D0*CA1*MH12*SA3*dgAtMZ())/(MW*SB) + (0.125D0*CA1*CA22*MH12*SA3*dgAtMZ())/(MW*SB) + (0.0625D0*CA1*MH32*SA3&
  &*dgAtMZ())/(MW*SB) + (0.0625D0*CA1*CA22*MH32*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*m12squared*SA1*SA3*dgAtMZ())/(MW*SB) - (0.1875D&
  &0*CA22*m12squared*SA1*SA3*dgAtMZ())/(MW*SB) + (0.125D0*m12squared*SA1*SA3*dgAtMZ())/(CB2*MW*SB) + (0.5625D0*CA12*m12squared*SA&
  &1*SA3*dgAtMZ())/(CB2*MW*SB) + (0.125D0*CA22*m12squared*SA1*SA3*dgAtMZ())/(CB2*MW*SB) + (0.5625D0*CA12*CA22*m12squared*SA1*SA3*&
  &dgAtMZ())/(CB2*MW*SB) + (0.375D0*CA1*MH12*SA12*SA3*dgAtMZ())/(MW*SB) + (0.375D0*CA1*CA22*MH12*SA12*SA3*dgAtMZ())/(MW*SB) + (0.&
  &1875D0*CA1*MH32*SA12*SA3*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*CA22*MH32*SA12*SA3*dgAtMZ())/(MW*SB) - (0.125D0*CA1*MH12*SA22*SA3*d&
  &gAtMZ())/(MW*SB) - (0.0625D0*CA1*MH32*SA22*SA3*dgAtMZ())/(MW*SB) + (0.1875D0*m12squared*SA1*SA22*SA3*dgAtMZ())/(MW*SB) - (0.12&
  &5D0*m12squared*SA1*SA22*SA3*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA12*m12squared*SA1*SA22*SA3*dgAtMZ())/(CB2*MW*SB) - (0.375D0*CA&
  &1*MH12*SA12*SA22*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*CA1*MH32*SA12*SA22*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*CA3*m12squared*SA1*SA2&
  &*dgAtMZ())/(CB*MW*SB2) + (0.28125D0*CA12*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA22*CA3*m12squared*SA1*SA2*&
  &dgAtMZ())/(CB*MW*SB2) + (0.84375D0*CA12*CA22*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*MW*SB2) - (0.125D0*CA1*m12squared*SA3*dgAtMZ&
  &())/(CB*MW*SB2) - (0.125D0*CA1*CA22*m12squared*SA3*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA1*m12squared*SA12*SA3*dgAtMZ())/(CB*MW*&
  &SB2) - (0.5625D0*CA1*CA22*m12squared*SA12*SA3*dgAtMZ())/(CB*MW*SB2) + (0.125D0*CA1*m12squared*SA22*SA3*dgAtMZ())/(CB*MW*SB2) +&
  & (0.5625D0*CA1*m12squared*SA12*SA22*SA3*dgAtMZ())/(CB*MW*SB2) - (0.09375D0*CA3*m12squared*SA1*SA2*dgAtMZ())/(MW*SB*TB) - (0.28&
  &125D0*CA22*CA3*m12squared*SA1*SA2*dgAtMZ())/(MW*SB*TB) - (0.0625D0*CA1*m12squared*SA3*dgAtMZ())/(MW*SB*TB) - (0.0625D0*CA1*CA2&
  &2*m12squared*SA3*dgAtMZ())/(MW*SB*TB) + (0.0625D0*CA1*m12squared*SA22*SA3*dgAtMZ())/(MW*SB*TB) - (0.09375D0*CA1*CA3*m12squared&
  &*SA2*TB*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*CA3*m12squared*SA2*TB*dgAtMZ())/(CB*MW) + (0.0625D0*m12squared*SA1*SA3*TB*dgAt&
  &MZ())/(CB*MW) + (0.0625D0*CA22*m12squared*SA1*SA3*TB*dgAtMZ())/(CB*MW) - (0.0625D0*m12squared*SA1*SA22*SA3*TB*dgAtMZ())/(CB*MW&
  &) + (0.0625D0*CA3*MH12*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875D0*CA22*CA3*MH12*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())&
  &/(CB*MW) + (0.03125D0*CA3*MH32*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA22*CA3*MH32*SA2*DBLE(CA1**INT(3.D0))*&
  &dgAtMZ())/(CB*MW) - (0.09375D0*CA3*m12squared*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.28125D0*CA22*CA3*m12squared*&
  &SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.125D0*MH12*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.125D0*CA22*MH12&
  &*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.0625D0*MH32*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.0625D0*CA22*MH32*&
  &SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.125D0*MH12*SA22*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.0625D0*MH32*SA&
  &22*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*m12squared*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.1875D&
  &0*CA22*m12squared*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*m12squared*SA22*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ()&
  &)/(CB*MW*SB2) + (0.125D0*MH12*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.125D0*CA22*MH12*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ(&
  &))/(CB*MW) + (0.0625D0*MH32*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.0625D0*CA22*MH32*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ()&
  &)/(CB*MW) - (0.125D0*MH12*SA22*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*MH32*SA22*SA3*DBLE(SA1**INT(3.D0))*dgAtM&
  &Z())/(CB*MW) + (0.0625D0*CA3*MH12*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA22*CA3*MH12*SA2*DBLE(SA1**INT(3.D0)&
  &)*dgAtMZ())/(MW*SB) + (0.03125D0*CA3*MH32*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA22*CA3*MH32*SA2*DBLE(SA1**&
  &INT(3.D0))*dgAtMZ())/(MW*SB) - (0.1875D0*m12squared*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875D0*CA22*m12squared&
  &*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*m12squared*SA22*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (&
  &0.09375D0*CA3*m12squared*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA22*CA3*m12squared*SA2*DBLE(SA1**INT(3.D&
  &0))*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*CA1*CA3*MH12*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.09375D0*CA1*CA3*MH32*DBLE(SA2**&
  &INT(3.D0))*dgAtMZ())/(CB*MW) - (0.28125D0*CA3*m12squared*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875D0*CA1*CA3*MH12*S&
  &A12*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*CA3*MH32*SA12*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.28125D0*&
  &CA1*CA3*m12squared*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*CA3*m12squared*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*M&
  &W*SB) - (0.1875D0*CA3*MH12*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA12*CA3*MH12*SA1*DBLE(SA2**INT(3.D0))*dgAtM&
  &Z())/(MW*SB) - (0.09375D0*CA3*MH32*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA12*CA3*MH32*SA1*DBLE(SA2**INT(3.D&
  &0))*dgAtMZ())/(MW*SB) - (0.28125D0*CA1*CA3*m12squared*SA12*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*CA3*m12squar&
  &ed*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA12*CA3*m12squared*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*S&
  &B2) + (0.09375D0*CA3*m12squared*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB*TB) + (0.09375D0*CA1*CA3*m12squared*TB*DBLE(SA2**INT&
  &(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*CA3*MH12*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.03125D0*CA3*MH&
  &32*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA3*m12squared*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(&
  &3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.0625D0*CA3*MH12*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.03125D0*CA3&
  &*MH32*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA3*m12squared*DBLE(SA1**INT(3.D0))*DBLE(SA2**I&
  &NT(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.28125D0*CA3*EL*SA1*SA2*dm122MSBarAlter())/(CB*MW*SW) + (0.84375D0*CA22*CA3*EL*SA1*SA2*dm1&
  &22MSBarAlter())/(CB*MW*SW) + (0.1875D0*CA1*EL*SA3*dm122MSBarAlter())/(CB*MW*SW) + (0.1875D0*CA1*CA22*EL*SA3*dm122MSBarAlter())&
  &/(CB*MW*SW) - (0.1875D0*CA1*EL*SA22*SA3*dm122MSBarAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA3*EL*SA2*dm122MSBarAlter())/(MW*SB*SW&
  &) + (0.84375D0*CA1*CA22*CA3*EL*SA2*dm122MSBarAlter())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*SA2*dm122MSBarAlter())/(CB2*MW*SB*SW) &
  &- (0.5625D0*CA1*CA22*CA3*EL*SA2*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.28125D0*CA1*CA3*EL*SA12*SA2*dm122MSBarAlter())/(CB2*MW*&
  &SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*SA12*SA2*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.1875D0*EL*SA1*SA3*dm122MSBarAlter())/(MW*S&
  &B*SW) - (0.1875D0*CA22*EL*SA1*SA3*dm122MSBarAlter())/(MW*SB*SW) + (0.125D0*EL*SA1*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.5&
  &625D0*CA12*EL*SA1*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.125D0*CA22*EL*SA1*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.5625&
  &D0*CA12*CA22*EL*SA1*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.1875D0*EL*SA1*SA22*SA3*dm122MSBarAlter())/(MW*SB*SW) - (0.125D0&
  &*EL*SA1*SA22*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.5625D0*CA12*EL*SA1*SA22*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.187&
  &5D0*CA3*EL*SA1*SA2*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.28125D0*CA12*CA3*EL*SA1*SA2*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.5&
  &625D0*CA22*CA3*EL*SA1*SA2*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA22*CA3*EL*SA1*SA2*dm122MSBarAlter())/(CB*MW*SB&
  &2*SW) - (0.125D0*CA1*EL*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.125D0*CA1*CA22*EL*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) - (&
  &0.5625D0*CA1*EL*SA12*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*EL*SA12*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) &
  &+ (0.125D0*CA1*EL*SA22*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.5625D0*CA1*EL*SA12*SA22*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW&
  &) - (0.09375D0*CA3*EL*SA1*SA2*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.28125D0*CA22*CA3*EL*SA1*SA2*dm122MSBarAlter())/(MW*SB*SW*T&
  &B) - (0.0625D0*CA1*EL*SA3*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.0625D0*CA1*CA22*EL*SA3*dm122MSBarAlter())/(MW*SB*SW*TB) + (0.0&
  &625D0*CA1*EL*SA22*SA3*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.09375D0*CA1*CA3*EL*SA2*TB*dm122MSBarAlter())/(CB*MW*SW) - (0.28125&
  &D0*CA1*CA22*CA3*EL*SA2*TB*dm122MSBarAlter())/(CB*MW*SW) + (0.0625D0*EL*SA1*SA3*TB*dm122MSBarAlter())/(CB*MW*SW) + (0.0625D0*CA&
  &22*EL*SA1*SA3*TB*dm122MSBarAlter())/(CB*MW*SW) - (0.0625D0*EL*SA1*SA22*SA3*TB*dm122MSBarAlter())/(CB*MW*SW) - (0.09375D0*CA3*E&
  &L*SA2*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.28125D0*CA22*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dm122MSBarAlter&
  &())/(CB2*MW*SB*SW) + (0.1875D0*EL*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.1875D0*CA22*EL*SA3*DBLE(CA1*&
  &*INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.1875D0*EL*SA22*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) -&
  & (0.1875D0*EL*SA3*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.1875D0*CA22*EL*SA3*DBLE(SA1**INT(3.D0))*dm122MSB&
  &arAlter())/(CB2*MW*SB*SW) + (0.1875D0*EL*SA22*SA3*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.09375D0*CA3*EL*S&
  &A2*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA22*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())&
  &/(CB*MW*SB2*SW) - (0.28125D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SW) - (0.28125D0*CA1*CA3*EL*DBLE(SA2**I&
  &NT(3.D0))*dm122MSBarAlter())/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.281&
  &25D0*CA1*CA3*EL*SA12*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dm122M&
  &SBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA12*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.09375D0*C&
  &A3*EL*SA1*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(MW*SB*SW*TB) + (0.09375D0*CA1*CA3*EL*TB*DBLE(SA2**INT(3.D0))*dm122MSBarAlte&
  &r())/(CB*MW*SW) + (0.09375D0*CA3*EL*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.09375D0*C&
  &A3*EL*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.1875D0*CA1*CA3*EL*SA2*dMH12OSAlter())/(&
  &CB*MW*SW) + (0.5625D0*CA1*CA22*CA3*EL*SA2*dMH12OSAlter())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*SA12*SA2*dMH12OSAlter())/(CB*MW*SW&
  &) - (0.5625D0*CA1*CA22*CA3*EL*SA12*SA2*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*EL*SA1*SA3*dMH12OSAlter())/(CB*MW*SW) - (0.375D0*&
  &CA12*EL*SA1*SA3*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*CA22*EL*SA1*SA3*dMH12OSAlter())/(CB*MW*SW) - (0.375D0*CA12*CA22*EL*SA1*S&
  &A3*dMH12OSAlter())/(CB*MW*SW) + (0.125D0*EL*SA1*SA22*SA3*dMH12OSAlter())/(CB*MW*SW) + (0.375D0*CA12*EL*SA1*SA22*SA3*dMH12OSAlt&
  &er())/(CB*MW*SW) + (0.1875D0*CA3*EL*SA1*SA2*dMH12OSAlter())/(MW*SB*SW) - (0.1875D0*CA12*CA3*EL*SA1*SA2*dMH12OSAlter())/(MW*SB*&
  &SW) + (0.5625D0*CA22*CA3*EL*SA1*SA2*dMH12OSAlter())/(MW*SB*SW) - (0.5625D0*CA12*CA22*CA3*EL*SA1*SA2*dMH12OSAlter())/(MW*SB*SW)&
  & + (0.125D0*CA1*EL*SA3*dMH12OSAlter())/(MW*SB*SW) + (0.125D0*CA1*CA22*EL*SA3*dMH12OSAlter())/(MW*SB*SW) + (0.375D0*CA1*EL*SA12&
  &*SA3*dMH12OSAlter())/(MW*SB*SW) + (0.375D0*CA1*CA22*EL*SA12*SA3*dMH12OSAlter())/(MW*SB*SW) - (0.125D0*CA1*EL*SA22*SA3*dMH12OSA&
  &lter())/(MW*SB*SW) - (0.375D0*CA1*EL*SA12*SA22*SA3*dMH12OSAlter())/(MW*SB*SW) - (0.5D0*CA2*CA3*dMH12OSAlter())/vS - (1.5D0*CA2&
  &*CA3*SA22*dMH12OSAlter())/vS + (0.0625D0*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) + (0.1875D0*CA22*CA3*EL*SA&
  &2*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*EL*SA3*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) - (0.125D0&
  &*CA22*EL*SA3*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.125D0*EL*SA22*SA3*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(MW*S&
  &B*SW) + (0.5D0*CA3*DBLE(CA2**INT(3.D0))*dMH12OSAlter())/vS + (0.125D0*EL*SA3*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) +&
  & (0.125D0*CA22*EL*SA3*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*EL*SA22*SA3*DBLE(SA1**INT(3.D0))*dMH12OSAlter&
  &())/(CB*MW*SW) + (0.0625D0*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.1875D0*CA22*CA3*EL*SA2*DBLE(SA1**IN&
  &T(3.D0))*dMH12OSAlter())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) + (0.1875D0*CA1*CA3&
  &*EL*SA12*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.1875D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW&
  &) + (0.1875D0*CA12*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) - (0.0625D0*CA3*EL*DBLE(CA1**INT(3.D0))*DBLE(SA2&
  &**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.0625D0*CA3*EL*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW&
  &) + (0.09375D0*CA1*CA3*EL*SA2*dMH32OSAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA22*CA3*EL*SA2*dMH32OSAlter())/(CB*MW*SW) - (0.0937&
  &5D0*CA1*CA3*EL*SA12*SA2*dMH32OSAlter())/(CB*MW*SW) - (0.28125D0*CA1*CA22*CA3*EL*SA12*SA2*dMH32OSAlter())/(CB*MW*SW) - (0.0625D&
  &0*EL*SA1*SA3*dMH32OSAlter())/(CB*MW*SW) - (0.1875D0*CA12*EL*SA1*SA3*dMH32OSAlter())/(CB*MW*SW) - (0.0625D0*CA22*EL*SA1*SA3*dMH&
  &32OSAlter())/(CB*MW*SW) - (0.1875D0*CA12*CA22*EL*SA1*SA3*dMH32OSAlter())/(CB*MW*SW) + (0.0625D0*EL*SA1*SA22*SA3*dMH32OSAlter()&
  &)/(CB*MW*SW) + (0.1875D0*CA12*EL*SA1*SA22*SA3*dMH32OSAlter())/(CB*MW*SW) + (0.09375D0*CA3*EL*SA1*SA2*dMH32OSAlter())/(MW*SB*SW&
  &) - (0.09375D0*CA12*CA3*EL*SA1*SA2*dMH32OSAlter())/(MW*SB*SW) + (0.28125D0*CA22*CA3*EL*SA1*SA2*dMH32OSAlter())/(MW*SB*SW) - (0&
  &.28125D0*CA12*CA22*CA3*EL*SA1*SA2*dMH32OSAlter())/(MW*SB*SW) + (0.0625D0*CA1*EL*SA3*dMH32OSAlter())/(MW*SB*SW) + (0.0625D0*CA1&
  &*CA22*EL*SA3*dMH32OSAlter())/(MW*SB*SW) + (0.1875D0*CA1*EL*SA12*SA3*dMH32OSAlter())/(MW*SB*SW) + (0.1875D0*CA1*CA22*EL*SA12*SA&
  &3*dMH32OSAlter())/(MW*SB*SW) - (0.0625D0*CA1*EL*SA22*SA3*dMH32OSAlter())/(MW*SB*SW) - (0.1875D0*CA1*EL*SA12*SA22*SA3*dMH32OSAl&
  &ter())/(MW*SB*SW) - (0.25D0*CA2*CA3*dMH32OSAlter())/vS - (0.75D0*CA2*CA3*SA22*dMH32OSAlter())/vS + (0.03125D0*CA3*EL*SA2*DBLE(&
  &CA1**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) + (0.09375D0*CA22*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) - (0.0&
  &625D0*EL*SA3*DBLE(CA1**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) - (0.0625D0*CA22*EL*SA3*DBLE(CA1**INT(3.D0))*dMH32OSAlter())/(MW*&
  &SB*SW) + (0.0625D0*EL*SA22*SA3*DBLE(CA1**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) + (0.25D0*CA3*DBLE(CA2**INT(3.D0))*dMH32OSAlter&
  &())/vS + (0.0625D0*EL*SA3*DBLE(SA1**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) + (0.0625D0*CA22*EL*SA3*DBLE(SA1**INT(3.D0))*dMH32OS&
  &Alter())/(CB*MW*SW) - (0.0625D0*EL*SA22*SA3*DBLE(SA1**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) + (0.03125D0*CA3*EL*SA2*DBLE(SA1**&
  &INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) + (0.09375D0*CA22*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) - (0.09375D&
  &0*CA1*CA3*EL*DBLE(SA2**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) + (0.09375D0*CA1*CA3*EL*SA12*DBLE(SA2**INT(3.D0))*dMH32OSAlter())&
  &/(CB*MW*SW) - (0.09375D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) + (0.09375D0*CA12*CA3*EL*SA1*DBLE(SA2**INT&
  &(3.D0))*dMH32OSAlter())/(MW*SB*SW) - (0.03125D0*CA3*EL*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) - &
  &(0.03125D0*CA3*EL*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) - (0.09375D0*CA1*CA3*EL*MH12*SA2*DBLE(M&
  &W**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.28125D0*CA1*CA22*CA3*EL*MH12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.04687&
  &5D0*CA1*CA3*EL*MH32*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.140625D0*CA1*CA22*CA3*EL*MH32*SA2*DBLE(MW**INT(-3.D0))*&
  &dMW2Alter())/(CB*SW) - (0.140625D0*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.421875D0*CA22*CA3*&
  &EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.09375D0*CA1*CA3*EL*MH12*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW&
  &2Alter())/(CB*SW) + (0.28125D0*CA1*CA22*CA3*EL*MH12*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.046875D0*CA1*CA3*E&
  &L*MH32*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*CA1*CA22*CA3*EL*MH32*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW&
  &2Alter())/(CB*SW) - (0.09375D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA1*CA22*EL*m12squ&
  &ared*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0625D0*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.&
  &1875D0*CA12*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0625D0*CA22*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*dM&
  &W2Alter())/(CB*SW) + (0.1875D0*CA12*CA22*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*EL*MH32*SA1*SA&
  &3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.09375D0*CA12*EL*MH32*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0&
  &3125D0*CA22*EL*MH32*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.09375D0*CA12*CA22*EL*MH32*SA1*SA3*DBLE(MW**INT(-3.D&
  &0))*dMW2Alter())/(CB*SW) + (0.09375D0*CA1*EL*m12squared*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.0625D0*EL*MH12&
  &*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.1875D0*CA12*EL*MH12*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter()&
  &)/(CB*SW) - (0.03125D0*EL*MH32*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA12*EL*MH32*SA1*SA22*SA3*D&
  &BLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.140625D0*CA1*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (&
  &0.421875D0*CA1*CA22*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA1*CA3*EL*m12squared*SA2*DBL&
  &E(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.28125D0*CA1*CA22*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*&
  &SB*SW) - (0.09375D0*CA3*EL*MH12*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA12*CA3*EL*MH12*SA1*SA2*DBLE(M&
  &W**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.28125D0*CA22*CA3*EL*MH12*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.28125&
  &D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*CA3*EL*MH32*SA1*SA2*DBLE(MW**INT(-3.D&
  &0))*dMW2Alter())/(SB*SW) + (0.046875D0*CA12*CA3*EL*MH32*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.140625D0*CA22*C&
  &A3*EL*MH32*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.140625D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*DBLE(MW**INT(-3.D0))*&
  &dMW2Alter())/(SB*SW) - (0.140625D0*CA1*CA3*EL*m12squared*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.421875D0*&
  &CA1*CA22*CA3*EL*m12squared*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.0625D0*CA1*EL*MH12*SA3*DBLE(MW**INT(-3.&
  &D0))*dMW2Alter())/(SB*SW) - (0.0625D0*CA1*CA22*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.03125D0*CA1*EL*MH32*&
  &SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.03125D0*CA1*CA22*EL*MH32*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0&
  &.09375D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA22*EL*m12squared*SA1*SA3*DBLE(MW**INT(&
  &-3.D0))*dMW2Alter())/(SB*SW) - (0.0625D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.28125D0*CA12&
  &*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.0625D0*CA22*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0&
  &))*dMW2Alter())/(CB2*SB*SW) - (0.28125D0*CA12*CA22*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.18&
  &75D0*CA1*EL*MH12*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.1875D0*CA1*CA22*EL*MH12*SA12*SA3*DBLE(MW**INT(-3.D0))&
  &*dMW2Alter())/(SB*SW) - (0.09375D0*CA1*EL*MH32*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*CA1*CA22*EL*MH3&
  &2*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.0625D0*CA1*EL*MH12*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW&
  &) + (0.03125D0*CA1*EL*MH32*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*EL*m12squared*SA1*SA22*SA3*DBLE(MW*&
  &*INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.0625D0*EL*m12squared*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.28&
  &125D0*CA12*EL*m12squared*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.1875D0*CA1*EL*MH12*SA12*SA22*SA3*DBLE&
  &(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA1*EL*MH32*SA12*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09&
  &375D0*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.140625D0*CA12*CA3*EL*m12squared*SA1*SA2*DBL&
  &E(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.28125D0*CA22*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*S&
  &B2*SW) - (0.421875D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.0625D0*CA1*EL*m12s&
  &quared*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.0625D0*CA1*CA22*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter&
  &())/(CB*SB2*SW) + (0.28125D0*CA1*EL*m12squared*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.28125D0*CA1*CA22*EL&
  &*m12squared*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.0625D0*CA1*EL*m12squared*SA22*SA3*DBLE(MW**INT(-3.D0))&
  &*dMW2Alter())/(CB*SB2*SW) - (0.28125D0*CA1*EL*m12squared*SA12*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.0468&
  &75D0*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.140625D0*CA22*CA3*EL*m12squared*SA1*SA2*DBLE(&
  &MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.03125D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.0&
  &3125D0*CA1*CA22*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) - (0.03125D0*CA1*EL*m12squared*SA22*SA3*DBLE(MW&
  &**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.046875D0*CA1*CA3*EL*m12squared*SA2*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (&
  &0.140625D0*CA1*CA22*CA3*EL*m12squared*SA2*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*EL*m12squared*SA1*SA3*TB*D&
  &BLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA22*EL*m12squared*SA1*SA3*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) &
  &+ (0.03125D0*EL*m12squared*SA1*SA22*SA3*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA3*EL*MH12*SA2*DBLE(CA1**IN&
  &T(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA22*CA3*EL*MH12*SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))&
  &*dMW2Alter())/(CB*SW) - (0.015625D0*CA3*EL*MH32*SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.046875&
  &D0*CA22*CA3*EL*MH32*SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.046875D0*CA3*EL*m12squared*SA2*DBL&
  &E(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.140625D0*CA22*CA3*EL*m12squared*SA2*DBLE(CA1**INT(3.D0))*&
  &DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.0625D0*EL*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(&
  &SB*SW) + (0.0625D0*CA22*EL*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.03125D0*EL*MH32*SA3*DB&
  &LE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.03125D0*CA22*EL*MH32*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-&
  &3.D0))*dMW2Alter())/(SB*SW) - (0.0625D0*EL*MH12*SA22*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.0&
  &3125D0*EL*MH32*SA22*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*EL*m12squared*SA3*DBLE(CA1&
  &**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.09375D0*CA22*EL*m12squared*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**I&
  &NT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.09375D0*EL*m12squared*SA22*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())&
  &/(CB*SB2*SW) - (0.0625D0*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.0625D0*CA22*EL*MH12*S&
  &A3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*EL*MH32*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(&
  &3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA22*EL*MH32*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.&
  &0625D0*EL*MH12*SA22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*EL*MH32*SA22*SA3*DBLE(MW**&
  &INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA3*EL*MH12*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*d&
  &MW2Alter())/(SB*SW) - (0.09375D0*CA22*CA3*EL*MH12*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.0156&
  &25D0*CA3*EL*MH32*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*CA22*CA3*EL*MH32*SA2*DBLE(MW&
  &**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0&
  &))*dMW2Alter())/(CB2*SB*SW) + (0.09375D0*CA22*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB2*SB&
  &*SW) - (0.09375D0*EL*m12squared*SA22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.046875D0*CA3*&
  &EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.140625D0*CA22*CA3*EL*m12squared*SA2*&
  &DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.09375D0*CA1*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*DBLE(SA2*&
  &*INT(3.D0))*dMW2Alter())/(CB*SW) + (0.046875D0*CA1*CA3*EL*MH32*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) &
  &+ (0.140625D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA1*CA3*EL*MH1&
  &2*SA12*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.046875D0*CA1*CA3*EL*MH32*SA12*DBLE(MW**INT(-3.D0))*&
  &DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Al&
  &ter())/(SB*SW) - (0.09375D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.0937&
  &5D0*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*CA12*CA3*EL*MH12*SA1*DBLE(MW**&
  &INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.046875D0*CA3*EL*MH32*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*&
  &dMW2Alter())/(SB*SW) - (0.046875D0*CA12*CA3*EL*MH32*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.14&
  &0625D0*CA1*CA3*EL*m12squared*SA12*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.09375D0*CA3*EL*m12sq&
  &uared*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.140625D0*CA12*CA3*EL*m12squared*SA1*DBLE(MW*&
  &*INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.046875D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**&
  &INT(3.D0))*dMW2Alter())/(SB*SW*TB) - (0.046875D0*CA1*CA3*EL*m12squared*TB*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter(&
  &))/(CB*SW) + (0.03125D0*CA3*EL*MH12*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.0&
  &15625D0*CA3*EL*MH32*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.046875D0*CA3*EL*m&
  &12squared*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.03125D0*CA3*EL*MH12*DBL&
  &E(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.015625D0*CA3*EL*MH32*DBLE(MW**INT(-3.D0)&
  &)*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*DBLE(SA1&
  &**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + 0.5D0*CA2*CA3*MH12*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() + 0.25D0*&
  &CA2*CA3*MH32*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() + 1.5D0*CA2*CA3*MH12*SA22*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() + 0.75D0*CA2*&
  &CA3*MH32*SA22*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() - 0.5D0*CA3*MH12*DBLE(CA2**INT(3.D0))*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() &
  &- 0.25D0*CA3*MH32*DBLE(CA2**INT(3.D0))*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() &
		& + CS1S1S1f111*dZH1H3OSAlter()/2D0 + CS1S1S1f211*dZH2H3OSAlter()/2D0 + CS1S1S1f311*dZH3H3OS()/2D0 &
		& + CS1S1S1f131*dZH1H1OS()/2D0 + CS1S1S1f231*dZH2H1OSAlter()/2D0 + CS1S1S1f331*dZH3H1OSAlter()/2D0 &
		& + CS1S1S1f131*dZH1H1OS()/2D0 + CS1S1S1f231*dZH2H1OSAlter()/2D0 + CS1S1S1f331*dZH3H1OSAlter()/2D0 &
		& )
	case (8)
		dRR11 = -CA2*SA1*dAlpha1PinchOS() - CA1*SA2*dAlpha2PinchOS()
		dRR12 = CA1*CA2*dAlpha1PinchOS() - SA1*SA2*dAlpha2PinchOS()
		dRR13 = CA2*dAlpha2PinchOS()
		dRR21 = -CA1*CA3*dAlpha1PinchOS() - CA1*CA3*SA2*dAlpha3PinchOS() - CA1*CA2*SA3*dAlpha2PinchOS() + &
			& SA1*SA3*dAlpha3PinchOS() + SA1*SA2*SA3*dAlpha1PinchOS()
		dRR22 = -CA3*SA1*dAlpha1PinchOS() - CA3*SA1*SA2*dAlpha3PinchOS() - CA1*SA3*dAlpha3PinchOS() - &
			& CA2*SA1*SA3*dAlpha2PinchOS() - CA1*SA2*SA3*dAlpha1PinchOS()
		dRR23 = CA2*CA3*dAlpha3PinchOS() - SA2*SA3*dAlpha2PinchOS()
		dRR31 = -CA1*CA2*CA3*dAlpha2PinchOS() + CA3*SA1*dAlpha3PinchOS() + CA3*SA1*SA2*dAlpha1PinchOS() + &
			& CA1*SA3*dAlpha1PinchOS() + CA1*SA2*SA3*dAlpha3PinchOS()
		dRR32 = -CA1*CA3*dAlpha3PinchOS() - CA2*CA3*SA1*dAlpha2PinchOS() - CA1*CA3*SA2*dAlpha1PinchOS() + &
			& SA1*SA3*dAlpha1PinchOS() + SA1*SA2*SA3*dAlpha3PinchOS()
		dRR33 = -CA3*SA2*dAlpha2PinchOS() - CA2*SA3*dAlpha3PinchOS()

		totalAmplitude = CS1S1S1f311*( &
&(0.28125D0*CA1*CA3*EL*m12squared*SA2*dAlpha1PinchOS())/(CB*MW*SW) + (0.84375D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha1PinchOS())/(&
  &CB*MW*SW) - (0.1875D0*CA3*EL*MH12*SA1*SA2*dAlpha1PinchOS())/(CB*MW*SW) - (0.5625D0*CA12*CA3*EL*MH12*SA1*SA2*dAlpha1PinchOS())/&
  &(CB*MW*SW) - (0.5625D0*CA22*CA3*EL*MH12*SA1*SA2*dAlpha1PinchOS())/(CB*MW*SW) - (1.6875D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dAlpha1&
  &PinchOS())/(CB*MW*SW) - (0.09375D0*CA3*EL*MH32*SA1*SA2*dAlpha1PinchOS())/(CB*MW*SW) - (0.28125D0*CA12*CA3*EL*MH32*SA1*SA2*dAlp&
  &ha1PinchOS())/(CB*MW*SW) - (0.28125D0*CA22*CA3*EL*MH32*SA1*SA2*dAlpha1PinchOS())/(CB*MW*SW) - (0.84375D0*CA12*CA22*CA3*EL*MH32&
  &*SA1*SA2*dAlpha1PinchOS())/(CB*MW*SW) - (0.125D0*CA1*EL*MH12*SA3*dAlpha1PinchOS())/(CB*MW*SW) - (0.125D0*CA1*CA22*EL*MH12*SA3*&
  &dAlpha1PinchOS())/(CB*MW*SW) - (0.0625D0*CA1*EL*MH32*SA3*dAlpha1PinchOS())/(CB*MW*SW) - (0.0625D0*CA1*CA22*EL*MH32*SA3*dAlpha1&
  &PinchOS())/(CB*MW*SW) - (0.1875D0*EL*m12squared*SA1*SA3*dAlpha1PinchOS())/(CB*MW*SW) - (0.1875D0*CA22*EL*m12squared*SA1*SA3*dA&
  &lpha1PinchOS())/(CB*MW*SW) + (1.125D0*CA1*EL*MH12*SA12*SA3*dAlpha1PinchOS())/(CB*MW*SW) + (1.125D0*CA1*CA22*EL*MH12*SA12*SA3*d&
  &Alpha1PinchOS())/(CB*MW*SW) + (0.5625D0*CA1*EL*MH32*SA12*SA3*dAlpha1PinchOS())/(CB*MW*SW) + (0.5625D0*CA1*CA22*EL*MH32*SA12*SA&
  &3*dAlpha1PinchOS())/(CB*MW*SW) + (0.125D0*CA1*EL*MH12*SA22*SA3*dAlpha1PinchOS())/(CB*MW*SW) + (0.0625D0*CA1*EL*MH32*SA22*SA3*d&
  &Alpha1PinchOS())/(CB*MW*SW) + (0.1875D0*EL*m12squared*SA1*SA22*SA3*dAlpha1PinchOS())/(CB*MW*SW) - (1.125D0*CA1*EL*MH12*SA12*SA&
  &22*SA3*dAlpha1PinchOS())/(CB*MW*SW) - (0.5625D0*CA1*EL*MH32*SA12*SA22*SA3*dAlpha1PinchOS())/(CB*MW*SW) + (0.1875D0*CA1*CA3*EL*&
  &MH12*SA2*dAlpha1PinchOS())/(MW*SB*SW) + (0.5625D0*CA1*CA22*CA3*EL*MH12*SA2*dAlpha1PinchOS())/(MW*SB*SW) + (0.09375D0*CA1*CA3*E&
  &L*MH32*SA2*dAlpha1PinchOS())/(MW*SB*SW) + (0.28125D0*CA1*CA22*CA3*EL*MH32*SA2*dAlpha1PinchOS())/(MW*SB*SW) - (0.28125D0*CA3*EL&
  &*m12squared*SA1*SA2*dAlpha1PinchOS())/(MW*SB*SW) - (0.84375D0*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha1PinchOS())/(MW*SB*SW) + (0&
  &.1875D0*CA3*EL*m12squared*SA1*SA2*dAlpha1PinchOS())/(CB2*MW*SB*SW) + (0.84375D0*CA12*CA3*EL*m12squared*SA1*SA2*dAlpha1PinchOS(&
  &))/(CB2*MW*SB*SW) + (0.5625D0*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha1PinchOS())/(CB2*MW*SB*SW) + (2.53125D0*CA12*CA22*CA3*EL*m1&
  &2squared*SA1*SA2*dAlpha1PinchOS())/(CB2*MW*SB*SW) + (0.5625D0*CA1*CA3*EL*MH12*SA12*SA2*dAlpha1PinchOS())/(MW*SB*SW) + (1.6875D&
  &0*CA1*CA22*CA3*EL*MH12*SA12*SA2*dAlpha1PinchOS())/(MW*SB*SW) + (0.28125D0*CA1*CA3*EL*MH32*SA12*SA2*dAlpha1PinchOS())/(MW*SB*SW&
  &) + (0.84375D0*CA1*CA22*CA3*EL*MH32*SA12*SA2*dAlpha1PinchOS())/(MW*SB*SW) - (0.1875D0*CA1*EL*m12squared*SA3*dAlpha1PinchOS())/&
  &(MW*SB*SW) - (0.1875D0*CA1*CA22*EL*m12squared*SA3*dAlpha1PinchOS())/(MW*SB*SW) + (0.125D0*CA1*EL*m12squared*SA3*dAlpha1PinchOS&
  &())/(CB2*MW*SB*SW) + (0.125D0*CA1*CA22*EL*m12squared*SA3*dAlpha1PinchOS())/(CB2*MW*SB*SW) - (0.125D0*EL*MH12*SA1*SA3*dAlpha1Pi&
  &nchOS())/(MW*SB*SW) + (1.125D0*CA12*EL*MH12*SA1*SA3*dAlpha1PinchOS())/(MW*SB*SW) - (0.125D0*CA22*EL*MH12*SA1*SA3*dAlpha1PinchO&
  &S())/(MW*SB*SW) + (1.125D0*CA12*CA22*EL*MH12*SA1*SA3*dAlpha1PinchOS())/(MW*SB*SW) - (0.0625D0*EL*MH32*SA1*SA3*dAlpha1PinchOS()&
  &)/(MW*SB*SW) + (0.5625D0*CA12*EL*MH32*SA1*SA3*dAlpha1PinchOS())/(MW*SB*SW) - (0.0625D0*CA22*EL*MH32*SA1*SA3*dAlpha1PinchOS())/&
  &(MW*SB*SW) + (0.5625D0*CA12*CA22*EL*MH32*SA1*SA3*dAlpha1PinchOS())/(MW*SB*SW) - (1.6875D0*CA1*EL*m12squared*SA12*SA3*dAlpha1Pi&
  &nchOS())/(CB2*MW*SB*SW) - (1.6875D0*CA1*CA22*EL*m12squared*SA12*SA3*dAlpha1PinchOS())/(CB2*MW*SB*SW) + (0.1875D0*CA1*EL*m12squ&
  &ared*SA22*SA3*dAlpha1PinchOS())/(MW*SB*SW) - (0.125D0*CA1*EL*m12squared*SA22*SA3*dAlpha1PinchOS())/(CB2*MW*SB*SW) + (0.125D0*E&
  &L*MH12*SA1*SA22*SA3*dAlpha1PinchOS())/(MW*SB*SW) - (1.125D0*CA12*EL*MH12*SA1*SA22*SA3*dAlpha1PinchOS())/(MW*SB*SW) + (0.0625D0&
  &*EL*MH32*SA1*SA22*SA3*dAlpha1PinchOS())/(MW*SB*SW) - (0.5625D0*CA12*EL*MH32*SA1*SA22*SA3*dAlpha1PinchOS())/(MW*SB*SW) + (1.687&
  &5D0*CA1*EL*m12squared*SA12*SA22*SA3*dAlpha1PinchOS())/(CB2*MW*SB*SW) - (0.1875D0*CA1*CA3*EL*m12squared*SA2*dAlpha1PinchOS())/(&
  &CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha1PinchOS())/(CB*MW*SB2*SW) - (0.84375D0*CA1*CA3*EL*m12squared*S&
  &A12*SA2*dAlpha1PinchOS())/(CB*MW*SB2*SW) - (2.53125D0*CA1*CA22*CA3*EL*m12squared*SA12*SA2*dAlpha1PinchOS())/(CB*MW*SB2*SW) + (&
  &0.125D0*EL*m12squared*SA1*SA3*dAlpha1PinchOS())/(CB*MW*SB2*SW) - (1.6875D0*CA12*EL*m12squared*SA1*SA3*dAlpha1PinchOS())/(CB*MW&
  &*SB2*SW) + (0.125D0*CA22*EL*m12squared*SA1*SA3*dAlpha1PinchOS())/(CB*MW*SB2*SW) - (1.6875D0*CA12*CA22*EL*m12squared*SA1*SA3*dA&
  &lpha1PinchOS())/(CB*MW*SB2*SW) - (0.125D0*EL*m12squared*SA1*SA22*SA3*dAlpha1PinchOS())/(CB*MW*SB2*SW) + (1.6875D0*CA12*EL*m12s&
  &quared*SA1*SA22*SA3*dAlpha1PinchOS())/(CB*MW*SB2*SW) - (0.09375D0*CA1*CA3*EL*m12squared*SA2*dAlpha1PinchOS())/(MW*SB*SW*TB) - &
  &(0.28125D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha1PinchOS())/(MW*SB*SW*TB) + (0.0625D0*EL*m12squared*SA1*SA3*dAlpha1PinchOS())/&
  &(MW*SB*SW*TB) + (0.0625D0*CA22*EL*m12squared*SA1*SA3*dAlpha1PinchOS())/(MW*SB*SW*TB) - (0.0625D0*EL*m12squared*SA1*SA22*SA3*dA&
  &lpha1PinchOS())/(MW*SB*SW*TB) + (0.09375D0*CA3*EL*m12squared*SA1*SA2*TB*dAlpha1PinchOS())/(CB*MW*SW) + (0.28125D0*CA22*CA3*EL*&
  &m12squared*SA1*SA2*TB*dAlpha1PinchOS())/(CB*MW*SW) + (0.0625D0*CA1*EL*m12squared*SA3*TB*dAlpha1PinchOS())/(CB*MW*SW) + (0.0625&
  &D0*CA1*CA22*EL*m12squared*SA3*TB*dAlpha1PinchOS())/(CB*MW*SW) - (0.0625D0*CA1*EL*m12squared*SA22*SA3*TB*dAlpha1PinchOS())/(CB*&
  &MW*SW) + (0.1875D0*CA1*CA2*CA3*EL*MH12*dAlpha2PinchOS())/(CB*MW*SW) + (0.09375D0*CA1*CA2*CA3*EL*MH32*dAlpha2PinchOS())/(CB*MW*&
  &SW) + (0.28125D0*CA2*CA3*EL*m12squared*SA1*dAlpha2PinchOS())/(CB*MW*SW) - (0.1875D0*CA1*CA2*CA3*EL*MH12*SA12*dAlpha2PinchOS())&
  &/(CB*MW*SW) - (0.09375D0*CA1*CA2*CA3*EL*MH32*SA12*dAlpha2PinchOS())/(CB*MW*SW) - (1.6875D0*CA1*CA2*CA3*EL*MH12*SA22*dAlpha2Pin&
  &chOS())/(CB*MW*SW) - (0.84375D0*CA1*CA2*CA3*EL*MH32*SA22*dAlpha2PinchOS())/(CB*MW*SW) - (2.53125D0*CA2*CA3*EL*m12squared*SA1*S&
  &A22*dAlpha2PinchOS())/(CB*MW*SW) + (1.6875D0*CA1*CA2*CA3*EL*MH12*SA12*SA22*dAlpha2PinchOS())/(CB*MW*SW) + (0.84375D0*CA1*CA2*C&
  &A3*EL*MH32*SA12*SA22*dAlpha2PinchOS())/(CB*MW*SW) - (0.75D0*CA1*CA2*EL*m12squared*SA2*SA3*dAlpha2PinchOS())/(CB*MW*SW) + (0.5D&
  &0*CA2*EL*MH12*SA1*SA2*SA3*dAlpha2PinchOS())/(CB*MW*SW) + (1.5D0*CA12*CA2*EL*MH12*SA1*SA2*SA3*dAlpha2PinchOS())/(CB*MW*SW) + (0&
  &.25D0*CA2*EL*MH32*SA1*SA2*SA3*dAlpha2PinchOS())/(CB*MW*SW) + (0.75D0*CA12*CA2*EL*MH32*SA1*SA2*SA3*dAlpha2PinchOS())/(CB*MW*SW)&
  & + (0.28125D0*CA1*CA2*CA3*EL*m12squared*dAlpha2PinchOS())/(MW*SB*SW) - (0.1875D0*CA1*CA2*CA3*EL*m12squared*dAlpha2PinchOS())/(&
  &CB2*MW*SB*SW) + (0.1875D0*CA2*CA3*EL*MH12*SA1*dAlpha2PinchOS())/(MW*SB*SW) - (0.1875D0*CA12*CA2*CA3*EL*MH12*SA1*dAlpha2PinchOS&
  &())/(MW*SB*SW) + (0.09375D0*CA2*CA3*EL*MH32*SA1*dAlpha2PinchOS())/(MW*SB*SW) - (0.09375D0*CA12*CA2*CA3*EL*MH32*SA1*dAlpha2Pinc&
  &hOS())/(MW*SB*SW) + (0.28125D0*CA1*CA2*CA3*EL*m12squared*SA12*dAlpha2PinchOS())/(CB2*MW*SB*SW) - (2.53125D0*CA1*CA2*CA3*EL*m12&
  &squared*SA22*dAlpha2PinchOS())/(MW*SB*SW) + (1.6875D0*CA1*CA2*CA3*EL*m12squared*SA22*dAlpha2PinchOS())/(CB2*MW*SB*SW) - (1.687&
  &5D0*CA2*CA3*EL*MH12*SA1*SA22*dAlpha2PinchOS())/(MW*SB*SW) + (1.6875D0*CA12*CA2*CA3*EL*MH12*SA1*SA22*dAlpha2PinchOS())/(MW*SB*S&
  &W) - (0.84375D0*CA2*CA3*EL*MH32*SA1*SA22*dAlpha2PinchOS())/(MW*SB*SW) + (0.84375D0*CA12*CA2*CA3*EL*MH32*SA1*SA22*dAlpha2PinchO&
  &S())/(MW*SB*SW) - (2.53125D0*CA1*CA2*CA3*EL*m12squared*SA12*SA22*dAlpha2PinchOS())/(CB2*MW*SB*SW) - (0.5D0*CA1*CA2*EL*MH12*SA2&
  &*SA3*dAlpha2PinchOS())/(MW*SB*SW) - (0.25D0*CA1*CA2*EL*MH32*SA2*SA3*dAlpha2PinchOS())/(MW*SB*SW) + (0.75D0*CA2*EL*m12squared*S&
  &A1*SA2*SA3*dAlpha2PinchOS())/(MW*SB*SW) - (0.5D0*CA2*EL*m12squared*SA1*SA2*SA3*dAlpha2PinchOS())/(CB2*MW*SB*SW) - (2.25D0*CA12&
  &*CA2*EL*m12squared*SA1*SA2*SA3*dAlpha2PinchOS())/(CB2*MW*SB*SW) - (1.5D0*CA1*CA2*EL*MH12*SA12*SA2*SA3*dAlpha2PinchOS())/(MW*SB&
  &*SW) - (0.75D0*CA1*CA2*EL*MH32*SA12*SA2*SA3*dAlpha2PinchOS())/(MW*SB*SW) - (0.1875D0*CA2*CA3*EL*m12squared*SA1*dAlpha2PinchOS(&
  &))/(CB*MW*SB2*SW) + (0.28125D0*CA12*CA2*CA3*EL*m12squared*SA1*dAlpha2PinchOS())/(CB*MW*SB2*SW) + (1.6875D0*CA2*CA3*EL*m12squar&
  &ed*SA1*SA22*dAlpha2PinchOS())/(CB*MW*SB2*SW) - (2.53125D0*CA12*CA2*CA3*EL*m12squared*SA1*SA22*dAlpha2PinchOS())/(CB*MW*SB2*SW)&
  & + (0.5D0*CA1*CA2*EL*m12squared*SA2*SA3*dAlpha2PinchOS())/(CB*MW*SB2*SW) + (2.25D0*CA1*CA2*EL*m12squared*SA12*SA2*SA3*dAlpha2P&
  &inchOS())/(CB*MW*SB2*SW) - (0.09375D0*CA2*CA3*EL*m12squared*SA1*dAlpha2PinchOS())/(MW*SB*SW*TB) + (0.84375D0*CA2*CA3*EL*m12squ&
  &ared*SA1*SA22*dAlpha2PinchOS())/(MW*SB*SW*TB) + (0.25D0*CA1*CA2*EL*m12squared*SA2*SA3*dAlpha2PinchOS())/(MW*SB*SW*TB) - (0.093&
  &75D0*CA1*CA2*CA3*EL*m12squared*TB*dAlpha2PinchOS())/(CB*MW*SW) + (0.84375D0*CA1*CA2*CA3*EL*m12squared*SA22*TB*dAlpha2PinchOS()&
  &)/(CB*MW*SW) - (0.25D0*CA2*EL*m12squared*SA1*SA2*SA3*TB*dAlpha2PinchOS())/(CB*MW*SW) + (0.5D0*CA3*MH12*SA2*dAlpha2PinchOS())/v&
  &S - (4.5D0*CA22*CA3*MH12*SA2*dAlpha2PinchOS())/vS + (0.25D0*CA3*MH32*SA2*dAlpha2PinchOS())/vS - (2.25D0*CA22*CA3*MH32*SA2*dAlp&
  &ha2PinchOS())/vS + (0.1875D0*CA1*CA3*EL*m12squared*dAlpha3PinchOS())/(CB*MW*SW) + (0.1875D0*CA1*CA22*CA3*EL*m12squared*dAlpha3&
  &PinchOS())/(CB*MW*SW) - (0.125D0*CA3*EL*MH12*SA1*dAlpha3PinchOS())/(CB*MW*SW) - (0.375D0*CA12*CA3*EL*MH12*SA1*dAlpha3PinchOS()&
  &)/(CB*MW*SW) - (0.125D0*CA22*CA3*EL*MH12*SA1*dAlpha3PinchOS())/(CB*MW*SW) - (0.375D0*CA12*CA22*CA3*EL*MH12*SA1*dAlpha3PinchOS(&
  &))/(CB*MW*SW) - (0.0625D0*CA3*EL*MH32*SA1*dAlpha3PinchOS())/(CB*MW*SW) - (0.1875D0*CA12*CA3*EL*MH32*SA1*dAlpha3PinchOS())/(CB*&
  &MW*SW) - (0.0625D0*CA22*CA3*EL*MH32*SA1*dAlpha3PinchOS())/(CB*MW*SW) - (0.1875D0*CA12*CA22*CA3*EL*MH32*SA1*dAlpha3PinchOS())/(&
  &CB*MW*SW) - (0.1875D0*CA1*CA3*EL*m12squared*SA22*dAlpha3PinchOS())/(CB*MW*SW) + (0.125D0*CA3*EL*MH12*SA1*SA22*dAlpha3PinchOS()&
  &)/(CB*MW*SW) + (0.375D0*CA12*CA3*EL*MH12*SA1*SA22*dAlpha3PinchOS())/(CB*MW*SW) + (0.0625D0*CA3*EL*MH32*SA1*SA22*dAlpha3PinchOS&
  &())/(CB*MW*SW) + (0.1875D0*CA12*CA3*EL*MH32*SA1*SA22*dAlpha3PinchOS())/(CB*MW*SW) - (0.1875D0*CA1*EL*MH12*SA2*SA3*dAlpha3Pinch&
  &OS())/(CB*MW*SW) - (0.5625D0*CA1*CA22*EL*MH12*SA2*SA3*dAlpha3PinchOS())/(CB*MW*SW) - (0.09375D0*CA1*EL*MH32*SA2*SA3*dAlpha3Pin&
  &chOS())/(CB*MW*SW) - (0.28125D0*CA1*CA22*EL*MH32*SA2*SA3*dAlpha3PinchOS())/(CB*MW*SW) - (0.28125D0*EL*m12squared*SA1*SA2*SA3*d&
  &Alpha3PinchOS())/(CB*MW*SW) - (0.84375D0*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha3PinchOS())/(CB*MW*SW) + (0.1875D0*CA1*EL*MH12*S&
  &A12*SA2*SA3*dAlpha3PinchOS())/(CB*MW*SW) + (0.5625D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dAlpha3PinchOS())/(CB*MW*SW) + (0.09375D0*C&
  &A1*EL*MH32*SA12*SA2*SA3*dAlpha3PinchOS())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*MH32*SA12*SA2*SA3*dAlpha3PinchOS())/(CB*MW*SW) +&
  & (0.125D0*CA1*CA3*EL*MH12*dAlpha3PinchOS())/(MW*SB*SW) + (0.125D0*CA1*CA22*CA3*EL*MH12*dAlpha3PinchOS())/(MW*SB*SW) + (0.0625D&
  &0*CA1*CA3*EL*MH32*dAlpha3PinchOS())/(MW*SB*SW) + (0.0625D0*CA1*CA22*CA3*EL*MH32*dAlpha3PinchOS())/(MW*SB*SW) - (0.1875D0*CA3*E&
  &L*m12squared*SA1*dAlpha3PinchOS())/(MW*SB*SW) - (0.1875D0*CA22*CA3*EL*m12squared*SA1*dAlpha3PinchOS())/(MW*SB*SW) + (0.125D0*C&
  &A3*EL*m12squared*SA1*dAlpha3PinchOS())/(CB2*MW*SB*SW) + (0.5625D0*CA12*CA3*EL*m12squared*SA1*dAlpha3PinchOS())/(CB2*MW*SB*SW) &
  &+ (0.125D0*CA22*CA3*EL*m12squared*SA1*dAlpha3PinchOS())/(CB2*MW*SB*SW) + (0.5625D0*CA12*CA22*CA3*EL*m12squared*SA1*dAlpha3Pinc&
  &hOS())/(CB2*MW*SB*SW) + (0.375D0*CA1*CA3*EL*MH12*SA12*dAlpha3PinchOS())/(MW*SB*SW) + (0.375D0*CA1*CA22*CA3*EL*MH12*SA12*dAlpha&
  &3PinchOS())/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*MH32*SA12*dAlpha3PinchOS())/(MW*SB*SW) + (0.1875D0*CA1*CA22*CA3*EL*MH32*SA12*dAl&
  &pha3PinchOS())/(MW*SB*SW) - (0.125D0*CA1*CA3*EL*MH12*SA22*dAlpha3PinchOS())/(MW*SB*SW) - (0.0625D0*CA1*CA3*EL*MH32*SA22*dAlpha&
  &3PinchOS())/(MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA1*SA22*dAlpha3PinchOS())/(MW*SB*SW) - (0.125D0*CA3*EL*m12squared*SA1*SA&
  &22*dAlpha3PinchOS())/(CB2*MW*SB*SW) - (0.5625D0*CA12*CA3*EL*m12squared*SA1*SA22*dAlpha3PinchOS())/(CB2*MW*SB*SW) - (0.375D0*CA&
  &1*CA3*EL*MH12*SA12*SA22*dAlpha3PinchOS())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*MH32*SA12*SA22*dAlpha3PinchOS())/(MW*SB*SW) - (0.2&
  &8125D0*CA1*EL*m12squared*SA2*SA3*dAlpha3PinchOS())/(MW*SB*SW) - (0.84375D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha3PinchOS())/(M&
  &W*SB*SW) + (0.1875D0*CA1*EL*m12squared*SA2*SA3*dAlpha3PinchOS())/(CB2*MW*SB*SW) + (0.5625D0*CA1*CA22*EL*m12squared*SA2*SA3*dAl&
  &pha3PinchOS())/(CB2*MW*SB*SW) - (0.1875D0*EL*MH12*SA1*SA2*SA3*dAlpha3PinchOS())/(MW*SB*SW) + (0.1875D0*CA12*EL*MH12*SA1*SA2*SA&
  &3*dAlpha3PinchOS())/(MW*SB*SW) - (0.5625D0*CA22*EL*MH12*SA1*SA2*SA3*dAlpha3PinchOS())/(MW*SB*SW) + (0.5625D0*CA12*CA22*EL*MH12&
  &*SA1*SA2*SA3*dAlpha3PinchOS())/(MW*SB*SW) - (0.09375D0*EL*MH32*SA1*SA2*SA3*dAlpha3PinchOS())/(MW*SB*SW) + (0.09375D0*CA12*EL*M&
  &H32*SA1*SA2*SA3*dAlpha3PinchOS())/(MW*SB*SW) - (0.28125D0*CA22*EL*MH32*SA1*SA2*SA3*dAlpha3PinchOS())/(MW*SB*SW) + (0.28125D0*C&
  &A12*CA22*EL*MH32*SA1*SA2*SA3*dAlpha3PinchOS())/(MW*SB*SW) - (0.28125D0*CA1*EL*m12squared*SA12*SA2*SA3*dAlpha3PinchOS())/(CB2*M&
  &W*SB*SW) - (0.84375D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3*dAlpha3PinchOS())/(CB2*MW*SB*SW) - (0.125D0*CA1*CA3*EL*m12squared*dA&
  &lpha3PinchOS())/(CB*MW*SB2*SW) - (0.125D0*CA1*CA22*CA3*EL*m12squared*dAlpha3PinchOS())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA3*EL*m&
  &12squared*SA12*dAlpha3PinchOS())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA12*dAlpha3PinchOS())/(CB*MW*SB2*SW) +&
  & (0.125D0*CA1*CA3*EL*m12squared*SA22*dAlpha3PinchOS())/(CB*MW*SB2*SW) + (0.5625D0*CA1*CA3*EL*m12squared*SA12*SA22*dAlpha3Pinch&
  &OS())/(CB*MW*SB2*SW) + (0.1875D0*EL*m12squared*SA1*SA2*SA3*dAlpha3PinchOS())/(CB*MW*SB2*SW) - (0.28125D0*CA12*EL*m12squared*SA&
  &1*SA2*SA3*dAlpha3PinchOS())/(CB*MW*SB2*SW) + (0.5625D0*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha3PinchOS())/(CB*MW*SB2*SW) - (0.84&
  &375D0*CA12*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha3PinchOS())/(CB*MW*SB2*SW) - (0.0625D0*CA1*CA3*EL*m12squared*dAlpha3PinchOS())&
  &/(MW*SB*SW*TB) - (0.0625D0*CA1*CA22*CA3*EL*m12squared*dAlpha3PinchOS())/(MW*SB*SW*TB) + (0.0625D0*CA1*CA3*EL*m12squared*SA22*d&
  &Alpha3PinchOS())/(MW*SB*SW*TB) + (0.09375D0*EL*m12squared*SA1*SA2*SA3*dAlpha3PinchOS())/(MW*SB*SW*TB) + (0.28125D0*CA22*EL*m12&
  &squared*SA1*SA2*SA3*dAlpha3PinchOS())/(MW*SB*SW*TB) + (0.0625D0*CA3*EL*m12squared*SA1*TB*dAlpha3PinchOS())/(CB*MW*SW) + (0.062&
  &5D0*CA22*CA3*EL*m12squared*SA1*TB*dAlpha3PinchOS())/(CB*MW*SW) - (0.0625D0*CA3*EL*m12squared*SA1*SA22*TB*dAlpha3PinchOS())/(CB&
  &*MW*SW) + (0.09375D0*CA1*EL*m12squared*SA2*SA3*TB*dAlpha3PinchOS())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*m12squared*SA2*SA3*TB*&
  &dAlpha3PinchOS())/(CB*MW*SW) + (0.5D0*CA2*MH12*SA3*dAlpha3PinchOS())/vS + (0.25D0*CA2*MH32*SA3*dAlpha3PinchOS())/vS + (1.5D0*C&
  &A2*MH12*SA22*SA3*dAlpha3PinchOS())/vS + (0.75D0*CA2*MH32*SA22*SA3*dAlpha3PinchOS())/vS + (0.09375D0*CA3*EL*MH12*SA1*SA2*dBeta2&
  &PinchOS())/(CB*MW*SW) - (0.09375D0*CA12*CA3*EL*MH12*SA1*SA2*dBeta2PinchOS())/(CB*MW*SW) + (0.28125D0*CA22*CA3*EL*MH12*SA1*SA2*&
  &dBeta2PinchOS())/(CB*MW*SW) - (0.28125D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dBeta2PinchOS())/(CB*MW*SW) + (0.046875D0*CA3*EL*MH32*S&
  &A1*SA2*dBeta2PinchOS())/(CB*MW*SW) - (0.046875D0*CA12*CA3*EL*MH32*SA1*SA2*dBeta2PinchOS())/(CB*MW*SW) + (0.140625D0*CA22*CA3*E&
  &L*MH32*SA1*SA2*dBeta2PinchOS())/(CB*MW*SW) - (0.140625D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*dBeta2PinchOS())/(CB*MW*SW) + (0.0625D0&
  &*CA1*EL*MH12*SA3*dBeta2PinchOS())/(CB*MW*SW) + (0.0625D0*CA1*CA22*EL*MH12*SA3*dBeta2PinchOS())/(CB*MW*SW) + (0.03125D0*CA1*EL*&
  &MH32*SA3*dBeta2PinchOS())/(CB*MW*SW) + (0.03125D0*CA1*CA22*EL*MH32*SA3*dBeta2PinchOS())/(CB*MW*SW) + (0.1875D0*CA1*EL*MH12*SA1&
  &2*SA3*dBeta2PinchOS())/(CB*MW*SW) + (0.1875D0*CA1*CA22*EL*MH12*SA12*SA3*dBeta2PinchOS())/(CB*MW*SW) + (0.09375D0*CA1*EL*MH32*S&
  &A12*SA3*dBeta2PinchOS())/(CB*MW*SW) + (0.09375D0*CA1*CA22*EL*MH32*SA12*SA3*dBeta2PinchOS())/(CB*MW*SW) - (0.0625D0*CA1*EL*MH12&
  &*SA22*SA3*dBeta2PinchOS())/(CB*MW*SW) - (0.03125D0*CA1*EL*MH32*SA22*SA3*dBeta2PinchOS())/(CB*MW*SW) - (0.1875D0*CA1*EL*MH12*SA&
  &12*SA22*SA3*dBeta2PinchOS())/(CB*MW*SW) - (0.09375D0*CA1*EL*MH32*SA12*SA22*SA3*dBeta2PinchOS())/(CB*MW*SW) + (0.09375D0*CA3*EL&
  &*m12squared*SA1*SA2*dBeta2PinchOS())/(MW*SB*SW) + (0.28125D0*CA22*CA3*EL*m12squared*SA1*SA2*dBeta2PinchOS())/(MW*SB*SW) - (0.3&
  &28125D0*CA3*EL*m12squared*SA1*SA2*dBeta2PinchOS())/(CB2*MW*SB*SW) + (0.421875D0*CA12*CA3*EL*m12squared*SA1*SA2*dBeta2PinchOS()&
  &)/(CB2*MW*SB*SW) - (0.984375D0*CA22*CA3*EL*m12squared*SA1*SA2*dBeta2PinchOS())/(CB2*MW*SB*SW) + (1.265625D0*CA12*CA22*CA3*EL*m&
  &12squared*SA1*SA2*dBeta2PinchOS())/(CB2*MW*SB*SW) + (0.0625D0*CA1*EL*m12squared*SA3*dBeta2PinchOS())/(MW*SB*SW) + (0.0625D0*CA&
  &1*CA22*EL*m12squared*SA3*dBeta2PinchOS())/(MW*SB*SW) - (0.21875D0*CA1*EL*m12squared*SA3*dBeta2PinchOS())/(CB2*MW*SB*SW) - (0.2&
  &1875D0*CA1*CA22*EL*m12squared*SA3*dBeta2PinchOS())/(CB2*MW*SB*SW) - (0.84375D0*CA1*EL*m12squared*SA12*SA3*dBeta2PinchOS())/(CB&
  &2*MW*SB*SW) - (0.84375D0*CA1*CA22*EL*m12squared*SA12*SA3*dBeta2PinchOS())/(CB2*MW*SB*SW) - (0.0625D0*CA1*EL*m12squared*SA22*SA&
  &3*dBeta2PinchOS())/(MW*SB*SW) + (0.21875D0*CA1*EL*m12squared*SA22*SA3*dBeta2PinchOS())/(CB2*MW*SB*SW) + (0.84375D0*CA1*EL*m12s&
  &quared*SA12*SA22*SA3*dBeta2PinchOS())/(CB2*MW*SB*SW) + (0.09375D0*CA1*CA3*EL*m12squared*SA2*dBeta2PinchOS())/(CB*MW*SB2*SW) + &
  &(0.28125D0*CA1*CA22*CA3*EL*m12squared*SA2*dBeta2PinchOS())/(CB*MW*SB2*SW) - (0.09375D0*CA3*EL*MH12*SA1*SA2*dBeta2PinchOS())/(C&
  &B*MW*SB2*SW) + (0.09375D0*CA12*CA3*EL*MH12*SA1*SA2*dBeta2PinchOS())/(CB*MW*SB2*SW) - (0.28125D0*CA22*CA3*EL*MH12*SA1*SA2*dBeta&
  &2PinchOS())/(CB*MW*SB2*SW) + (0.28125D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dBeta2PinchOS())/(CB*MW*SB2*SW) - (0.046875D0*CA3*EL*MH3&
  &2*SA1*SA2*dBeta2PinchOS())/(CB*MW*SB2*SW) + (0.046875D0*CA12*CA3*EL*MH32*SA1*SA2*dBeta2PinchOS())/(CB*MW*SB2*SW) - (0.140625D0&
  &*CA22*CA3*EL*MH32*SA1*SA2*dBeta2PinchOS())/(CB*MW*SB2*SW) + (0.140625D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*dBeta2PinchOS())/(CB*MW*&
  &SB2*SW) - (0.28125D0*CA1*CA3*EL*m12squared*SA12*SA2*dBeta2PinchOS())/(CB*MW*SB2*SW) - (0.84375D0*CA1*CA22*CA3*EL*m12squared*SA&
  &12*SA2*dBeta2PinchOS())/(CB*MW*SB2*SW) - (0.0625D0*CA1*EL*MH12*SA3*dBeta2PinchOS())/(CB*MW*SB2*SW) - (0.0625D0*CA1*CA22*EL*MH1&
  &2*SA3*dBeta2PinchOS())/(CB*MW*SB2*SW) - (0.03125D0*CA1*EL*MH32*SA3*dBeta2PinchOS())/(CB*MW*SB2*SW) - (0.03125D0*CA1*CA22*EL*MH&
  &32*SA3*dBeta2PinchOS())/(CB*MW*SB2*SW) - (0.0625D0*EL*m12squared*SA1*SA3*dBeta2PinchOS())/(CB*MW*SB2*SW) - (0.5625D0*CA12*EL*m&
  &12squared*SA1*SA3*dBeta2PinchOS())/(CB*MW*SB2*SW) - (0.0625D0*CA22*EL*m12squared*SA1*SA3*dBeta2PinchOS())/(CB*MW*SB2*SW) - (0.&
  &5625D0*CA12*CA22*EL*m12squared*SA1*SA3*dBeta2PinchOS())/(CB*MW*SB2*SW) - (0.1875D0*CA1*EL*MH12*SA12*SA3*dBeta2PinchOS())/(CB*M&
  &W*SB2*SW) - (0.1875D0*CA1*CA22*EL*MH12*SA12*SA3*dBeta2PinchOS())/(CB*MW*SB2*SW) - (0.09375D0*CA1*EL*MH32*SA12*SA3*dBeta2PinchO&
  &S())/(CB*MW*SB2*SW) - (0.09375D0*CA1*CA22*EL*MH32*SA12*SA3*dBeta2PinchOS())/(CB*MW*SB2*SW) + (0.0625D0*CA1*EL*MH12*SA22*SA3*dB&
  &eta2PinchOS())/(CB*MW*SB2*SW) + (0.03125D0*CA1*EL*MH32*SA22*SA3*dBeta2PinchOS())/(CB*MW*SB2*SW) + (0.0625D0*EL*m12squared*SA1*&
  &SA22*SA3*dBeta2PinchOS())/(CB*MW*SB2*SW) + (0.5625D0*CA12*EL*m12squared*SA1*SA22*SA3*dBeta2PinchOS())/(CB*MW*SB2*SW) + (0.1875&
  &D0*CA1*EL*MH12*SA12*SA22*SA3*dBeta2PinchOS())/(CB*MW*SB2*SW) + (0.09375D0*CA1*EL*MH32*SA12*SA22*SA3*dBeta2PinchOS())/(CB*MW*SB&
  &2*SW) - (0.1875D0*CA1*CA3*EL*m12squared*SA2*dBeta2PinchOS())/(MW*SB*SW*TB) - (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA2*dBeta2Pi&
  &nchOS())/(MW*SB*SW*TB) - (0.09375D0*CA3*EL*MH12*SA1*SA2*dBeta2PinchOS())/(MW*SB*SW*TB) + (0.09375D0*CA12*CA3*EL*MH12*SA1*SA2*d&
  &Beta2PinchOS())/(MW*SB*SW*TB) - (0.28125D0*CA22*CA3*EL*MH12*SA1*SA2*dBeta2PinchOS())/(MW*SB*SW*TB) + (0.28125D0*CA12*CA22*CA3*&
  &EL*MH12*SA1*SA2*dBeta2PinchOS())/(MW*SB*SW*TB) - (0.046875D0*CA3*EL*MH32*SA1*SA2*dBeta2PinchOS())/(MW*SB*SW*TB) + (0.046875D0*&
  &CA12*CA3*EL*MH32*SA1*SA2*dBeta2PinchOS())/(MW*SB*SW*TB) - (0.140625D0*CA22*CA3*EL*MH32*SA1*SA2*dBeta2PinchOS())/(MW*SB*SW*TB) &
  &+ (0.140625D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*dBeta2PinchOS())/(MW*SB*SW*TB) - (0.0625D0*CA1*EL*MH12*SA3*dBeta2PinchOS())/(MW*SB&
  &*SW*TB) - (0.0625D0*CA1*CA22*EL*MH12*SA3*dBeta2PinchOS())/(MW*SB*SW*TB) - (0.03125D0*CA1*EL*MH32*SA3*dBeta2PinchOS())/(MW*SB*S&
  &W*TB) - (0.03125D0*CA1*CA22*EL*MH32*SA3*dBeta2PinchOS())/(MW*SB*SW*TB) + (0.125D0*EL*m12squared*SA1*SA3*dBeta2PinchOS())/(MW*S&
  &B*SW*TB) + (0.125D0*CA22*EL*m12squared*SA1*SA3*dBeta2PinchOS())/(MW*SB*SW*TB) - (0.1875D0*CA1*EL*MH12*SA12*SA3*dBeta2PinchOS()&
  &)/(MW*SB*SW*TB) - (0.1875D0*CA1*CA22*EL*MH12*SA12*SA3*dBeta2PinchOS())/(MW*SB*SW*TB) - (0.09375D0*CA1*EL*MH32*SA12*SA3*dBeta2P&
  &inchOS())/(MW*SB*SW*TB) - (0.09375D0*CA1*CA22*EL*MH32*SA12*SA3*dBeta2PinchOS())/(MW*SB*SW*TB) + (0.0625D0*CA1*EL*MH12*SA22*SA3&
  &*dBeta2PinchOS())/(MW*SB*SW*TB) + (0.03125D0*CA1*EL*MH32*SA22*SA3*dBeta2PinchOS())/(MW*SB*SW*TB) - (0.125D0*EL*m12squared*SA1*&
  &SA22*SA3*dBeta2PinchOS())/(MW*SB*SW*TB) + (0.1875D0*CA1*EL*MH12*SA12*SA22*SA3*dBeta2PinchOS())/(MW*SB*SW*TB) + (0.09375D0*CA1*&
  &EL*MH32*SA12*SA22*SA3*dBeta2PinchOS())/(MW*SB*SW*TB) + (0.1875D0*CA1*CA3*EL*MH12*SA2*TB*dBeta2PinchOS())/(CB*MW*SW) + (0.5625D&
  &0*CA1*CA22*CA3*EL*MH12*SA2*TB*dBeta2PinchOS())/(CB*MW*SW) + (0.09375D0*CA1*CA3*EL*MH32*SA2*TB*dBeta2PinchOS())/(CB*MW*SW) + (0&
  &.28125D0*CA1*CA22*CA3*EL*MH32*SA2*TB*dBeta2PinchOS())/(CB*MW*SW) + (0.328125D0*CA3*EL*m12squared*SA1*SA2*TB*dBeta2PinchOS())/(&
  &CB*MW*SW) + (0.984375D0*CA22*CA3*EL*m12squared*SA1*SA2*TB*dBeta2PinchOS())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*MH12*SA12*SA2*TB*&
  &dBeta2PinchOS())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*EL*MH12*SA12*SA2*TB*dBeta2PinchOS())/(CB*MW*SW) - (0.09375D0*CA1*CA3*EL*M&
  &H32*SA12*SA2*TB*dBeta2PinchOS())/(CB*MW*SW) - (0.28125D0*CA1*CA22*CA3*EL*MH32*SA12*SA2*TB*dBeta2PinchOS())/(CB*MW*SW) + (0.218&
  &75D0*CA1*EL*m12squared*SA3*TB*dBeta2PinchOS())/(CB*MW*SW) + (0.21875D0*CA1*CA22*EL*m12squared*SA3*TB*dBeta2PinchOS())/(CB*MW*S&
  &W) - (0.125D0*EL*MH12*SA1*SA3*TB*dBeta2PinchOS())/(CB*MW*SW) - (0.375D0*CA12*EL*MH12*SA1*SA3*TB*dBeta2PinchOS())/(CB*MW*SW) - &
  &(0.125D0*CA22*EL*MH12*SA1*SA3*TB*dBeta2PinchOS())/(CB*MW*SW) - (0.375D0*CA12*CA22*EL*MH12*SA1*SA3*TB*dBeta2PinchOS())/(CB*MW*S&
  &W) - (0.0625D0*EL*MH32*SA1*SA3*TB*dBeta2PinchOS())/(CB*MW*SW) - (0.1875D0*CA12*EL*MH32*SA1*SA3*TB*dBeta2PinchOS())/(CB*MW*SW) &
  &- (0.0625D0*CA22*EL*MH32*SA1*SA3*TB*dBeta2PinchOS())/(CB*MW*SW) - (0.1875D0*CA12*CA22*EL*MH32*SA1*SA3*TB*dBeta2PinchOS())/(CB*&
  &MW*SW) - (0.21875D0*CA1*EL*m12squared*SA22*SA3*TB*dBeta2PinchOS())/(CB*MW*SW) + (0.125D0*EL*MH12*SA1*SA22*SA3*TB*dBeta2PinchOS&
  &())/(CB*MW*SW) + (0.375D0*CA12*EL*MH12*SA1*SA22*SA3*TB*dBeta2PinchOS())/(CB*MW*SW) + (0.0625D0*EL*MH32*SA1*SA22*SA3*TB*dBeta2P&
  &inchOS())/(CB*MW*SW) + (0.1875D0*CA12*EL*MH32*SA1*SA22*SA3*TB*dBeta2PinchOS())/(CB*MW*SW) + (0.140625D0*CA3*EL*m12squared*SA1*&
  &SA2*dBeta2PinchOS())/(MW*SB*SW*TB2) + (0.421875D0*CA22*CA3*EL*m12squared*SA1*SA2*dBeta2PinchOS())/(MW*SB*SW*TB2) + (0.09375D0*&
  &CA1*EL*m12squared*SA3*dBeta2PinchOS())/(MW*SB*SW*TB2) + (0.09375D0*CA1*CA22*EL*m12squared*SA3*dBeta2PinchOS())/(MW*SB*SW*TB2) &
  &- (0.09375D0*CA1*EL*m12squared*SA22*SA3*dBeta2PinchOS())/(MW*SB*SW*TB2) - (0.1875D0*CA1*CA3*EL*m12squared*SA2*TB2*dBeta2PinchO&
  &S())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA2*TB2*dBeta2PinchOS())/(CB*MW*SW) + (0.125D0*EL*m12squared*SA1*SA3*TB&
  &2*dBeta2PinchOS())/(CB*MW*SW) + (0.125D0*CA22*EL*m12squared*SA1*SA3*TB2*dBeta2PinchOS())/(CB*MW*SW) - (0.125D0*EL*m12squared*S&
  &A1*SA22*SA3*TB2*dBeta2PinchOS())/(CB*MW*SW) + (0.125D0*CA2*CA3*MH12*dBeta2PinchOS())/(CB*SB*vS) + (0.0625D0*CA2*CA3*MH32*dBeta&
  &2PinchOS())/(CB*SB*vS) + (0.375D0*CA2*CA3*MH12*SA22*dBeta2PinchOS())/(CB*SB*vS) + (0.1875D0*CA2*CA3*MH32*SA22*dBeta2PinchOS())&
  &/(CB*SB*vS) - (0.125D0*CA2*CA3*MH12*dBeta2PinchOS())/(TB*vS) - (0.0625D0*CA2*CA3*MH32*dBeta2PinchOS())/(TB*vS) - (0.375D0*CA2*&
  &CA3*MH12*SA22*dBeta2PinchOS())/(TB*vS) - (0.1875D0*CA2*CA3*MH32*SA22*dBeta2PinchOS())/(TB*vS) - (0.125D0*CA2*CA3*MH12*TB*dBeta&
  &2PinchOS())/vS - (0.0625D0*CA2*CA3*MH32*TB*dBeta2PinchOS())/vS - (0.375D0*CA2*CA3*MH12*SA22*TB*dBeta2PinchOS())/vS - (0.1875D0&
  &*CA2*CA3*MH32*SA22*TB*dBeta2PinchOS())/vS - (0.375D0*EL*MH12*SA3*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.375D0*&
  &CA22*EL*MH12*SA3*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*EL*MH32*SA3*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0&
  &)))/(CB*MW*SW) - (0.1875D0*CA22*EL*MH32*SA3*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.375D0*EL*MH12*SA22*SA3*dAlp&
  &ha1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*EL*MH32*SA22*SA3*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) -&
  & (0.1875D0*CA3*EL*MH12*SA2*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA22*CA3*EL*MH12*SA2*dAlpha1PinchOS()&
  &*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA3*EL*MH32*SA2*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*&
  &CA22*CA3*EL*MH32*SA2*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.5625D0*EL*m12squared*SA3*dAlpha1PinchOS()*DBLE(CA1&
  &**INT(3.D0)))/(CB2*MW*SB*SW) + (0.5625D0*CA22*EL*m12squared*SA3*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.562&
  &5D0*EL*m12squared*SA22*SA3*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.28125D0*CA3*EL*m12squared*SA2*dAlpha1Pin&
  &chOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA22*CA3*EL*m12squared*SA2*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0)))/(CB&
  &*MW*SB2*SW) + (0.0625D0*CA2*CA3*EL*MH12*dAlpha2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA2*CA3*EL*MH32*dAlpha2&
  &PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.5625D0*CA2*CA3*EL*MH12*SA22*dAlpha2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) &
  &- (0.28125D0*CA2*CA3*EL*MH32*SA22*dAlpha2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA2*CA3*EL*m12squared*dAlpha2&
  &PinchOS()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.84375D0*CA2*CA3*EL*m12squared*SA22*dAlpha2PinchOS()*DBLE(CA1**INT(3.D0)))/&
  &(CB2*MW*SB*SW) + (0.5D0*CA2*EL*MH12*SA2*SA3*dAlpha2PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.25D0*CA2*EL*MH32*SA2*SA3*dA&
  &lpha2PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.75D0*CA2*EL*m12squared*SA2*SA3*dAlpha2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB&
  &*MW*SB2*SW) - (0.0625D0*EL*MH12*SA2*SA3*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA22*EL*MH12*SA2*SA3*dAl&
  &pha3PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*EL*MH32*SA2*SA3*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) &
  &- (0.09375D0*CA22*EL*MH32*SA2*SA3*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.125D0*CA3*EL*MH12*dAlpha3PinchOS()*DB&
  &LE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.125D0*CA22*CA3*EL*MH12*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.0625D0*CA3*E&
  &L*MH32*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.0625D0*CA22*CA3*EL*MH32*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(&
  &MW*SB*SW) + (0.125D0*CA3*EL*MH12*SA22*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*CA3*EL*MH32*SA22*dAlpha3Pi&
  &nchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*EL*m12squared*SA2*SA3*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*&
  &SW) + (0.28125D0*CA22*EL*m12squared*SA2*SA3*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*m12square&
  &d*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*CA22*CA3*EL*m12squared*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0&
  &)))/(CB*MW*SB2*SW) - (0.1875D0*CA3*EL*m12squared*SA22*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.0625D0*EL*MH1&
  &2*SA3*dBeta2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.0625D0*CA22*EL*MH12*SA3*dBeta2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*&
  &MW*SW) - (0.03125D0*EL*MH32*SA3*dBeta2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*CA22*EL*MH32*SA3*dBeta2PinchOS()&
  &*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*EL*MH12*SA22*SA3*dBeta2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*E&
  &L*MH32*SA22*SA3*dBeta2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*EL*m12squared*SA3*dBeta2PinchOS()*DBLE(CA1**INT(&
  &3.D0)))/(CB2*MW*SB*SW) + (0.28125D0*CA22*EL*m12squared*SA3*dBeta2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*E&
  &L*m12squared*SA22*SA3*dBeta2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.09375D0*CA3*EL*m12squared*SA2*dBeta2PinchOS()*&
  &DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA22*CA3*EL*m12squared*SA2*dBeta2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*&
  &SW) + (0.0625D0*EL*MH12*SA3*dBeta2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*CA22*EL*MH12*SA3*dBeta2PinchOS()*&
  &DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.03125D0*EL*MH32*SA3*dBeta2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.03125D&
  &0*CA22*EL*MH32*SA3*dBeta2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.0625D0*EL*MH12*SA22*SA3*dBeta2PinchOS()*DBLE(CA1*&
  &*INT(3.D0)))/(CB*MW*SB2*SW) - (0.03125D0*EL*MH32*SA22*SA3*dBeta2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*EL*&
  &MH12*SA3*dBeta2PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.0625D0*CA22*EL*MH12*SA3*dBeta2PinchOS()*DBLE(CA1**INT(3.D0))&
  &)/(MW*SB*SW*TB) + (0.03125D0*EL*MH32*SA3*dBeta2PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.03125D0*CA22*EL*MH32*SA3*dBe&
  &ta2PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.0625D0*EL*MH12*SA22*SA3*dBeta2PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*&
  &TB) - (0.03125D0*EL*MH32*SA22*SA3*dBeta2PinchOS()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.0625D0*CA3*EL*MH12*SA2*TB*dBeta2Pin&
  &chOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA22*CA3*EL*MH12*SA2*TB*dBeta2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) +&
  & (0.03125D0*CA3*EL*MH32*SA2*TB*dBeta2PinchOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA22*CA3*EL*MH32*SA2*TB*dBeta2Pin&
  &chOS()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA1*CA3*EL*MH12*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.281&
  &25D0*CA1*CA3*EL*MH32*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.84375D0*CA3*EL*m12squared*SA1*dAlpha2PinchOS()*DBL&
  &E(CA2**INT(3.D0)))/(CB*MW*SW) - (0.5625D0*CA1*CA3*EL*MH12*SA12*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*&
  &CA1*CA3*EL*MH32*SA12*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.84375D0*CA1*CA3*EL*m12squared*dAlpha2PinchOS()*DBL&
  &E(CA2**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA1*CA3*EL*m12squared*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.562&
  &5D0*CA3*EL*MH12*SA1*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA12*CA3*EL*MH12*SA1*dAlpha2PinchOS()*DBLE(C&
  &A2**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*CA3*EL*MH32*SA1*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA12*CA&
  &3*EL*MH32*SA1*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) + (0.84375D0*CA1*CA3*EL*m12squared*SA12*dAlpha2PinchOS()*DBLE(&
  &CA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA3*EL*m12squared*SA1*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.8&
  &4375D0*CA12*CA3*EL*m12squared*SA1*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125D0*CA3*EL*m12squared*SA1*dAl&
  &pha2PinchOS()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.28125D0*CA1*CA3*EL*m12squared*TB*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0)))&
  &/(CB*MW*SW) - (0.5D0*MH12*SA3*dAlpha3PinchOS()*DBLE(CA2**INT(3.D0)))/vS - (0.25D0*MH32*SA3*dAlpha3PinchOS()*DBLE(CA2**INT(3.D0&
  &)))/vS - (0.125D0*CA3*MH12*dBeta2PinchOS()*DBLE(CA2**INT(3.D0)))/(CB*SB*vS) - (0.0625D0*CA3*MH32*dBeta2PinchOS()*DBLE(CA2**INT&
  &(3.D0)))/(CB*SB*vS) + (0.125D0*CA3*MH12*dBeta2PinchOS()*DBLE(CA2**INT(3.D0)))/(TB*vS) + (0.0625D0*CA3*MH32*dBeta2PinchOS()*DBL&
  &E(CA2**INT(3.D0)))/(TB*vS) + (0.125D0*CA3*MH12*TB*dBeta2PinchOS()*DBLE(CA2**INT(3.D0)))/vS + (0.0625D0*CA3*MH32*TB*dBeta2Pinch&
  &OS()*DBLE(CA2**INT(3.D0)))/vS + (0.1875D0*CA3*EL*MH12*dAlpha2PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) +&
  & (0.09375D0*CA3*EL*MH32*dAlpha2PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA3*EL*m12squared*&
  &dAlpha2PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.375D0*CA1*CA3*EL*m12squared*SA2*dBeta2PinchOS(&
  &)*DBLE(CB**INT(-3.D0)))/(MW*SW) - (1.125D0*CA1*CA22*CA3*EL*m12squared*SA2*dBeta2PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.5&
  &625D0*CA1*CA3*EL*m12squared*SA12*SA2*dBeta2PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (1.6875D0*CA1*CA22*CA3*EL*m12squared*SA12&
  &*SA2*dBeta2PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.25D0*EL*m12squared*SA1*SA3*dBeta2PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*S&
  &W) + (1.125D0*CA12*EL*m12squared*SA1*SA3*dBeta2PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.25D0*CA22*EL*m12squared*SA1*SA3*dB&
  &eta2PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (1.125D0*CA12*CA22*EL*m12squared*SA1*SA3*dBeta2PinchOS()*DBLE(CB**INT(-3.D0)))/(&
  &MW*SW) - (0.25D0*EL*m12squared*SA1*SA22*SA3*dBeta2PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (1.125D0*CA12*EL*m12squared*SA1*SA&
  &22*SA3*dBeta2PinchOS()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.1875D0*CA3*EL*m12squared*SA2*dBeta2PinchOS()*DBLE(CA1**INT(3.D0))*DB&
  &LE(CB**INT(-3.D0)))/(MW*SW) - (0.5625D0*CA22*CA3*EL*m12squared*SA2*dBeta2PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/&
  &(MW*SW) + (0.1875D0*CA3*EL*MH12*SA2*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA22*CA3*EL*MH12*SA2*dAlpha1&
  &PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA3*EL*MH32*SA2*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0&
  &.28125D0*CA22*CA3*EL*MH32*SA2*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA3*EL*m12squared*SA2*dAlpha1Pinc&
  &hOS()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.84375D0*CA22*CA3*EL*m12squared*SA2*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB2&
  &*MW*SB*SW) - (0.375D0*EL*MH12*SA3*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.375D0*CA22*EL*MH12*SA3*dAlpha1PinchOS&
  &()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*EL*MH32*SA3*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA22&
  &*EL*MH32*SA3*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.375D0*EL*MH12*SA22*SA3*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0&
  &)))/(MW*SB*SW) + (0.1875D0*EL*MH32*SA22*SA3*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.5625D0*EL*m12squared*SA3*dA&
  &lpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.5625D0*CA22*EL*m12squared*SA3*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/&
  &(CB*MW*SB2*SW) - (0.5625D0*EL*m12squared*SA22*SA3*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.5D0*CA2*EL*MH12*S&
  &A2*SA3*dAlpha2PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.25D0*CA2*EL*MH32*SA2*SA3*dAlpha2PinchOS()*DBLE(SA1**INT(3.D0)))/&
  &(CB*MW*SW) + (0.0625D0*CA2*CA3*EL*MH12*dAlpha2PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA2*CA3*EL*MH32*dAlpha2P&
  &inchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA2*CA3*EL*MH12*SA22*dAlpha2PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) -&
  & (0.28125D0*CA2*CA3*EL*MH32*SA22*dAlpha2PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.75D0*CA2*EL*m12squared*SA2*SA3*dAlpha2&
  &PinchOS()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA2*CA3*EL*m12squared*dAlpha2PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*M&
  &W*SB2*SW) + (0.84375D0*CA2*CA3*EL*m12squared*SA22*dAlpha2PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.125D0*CA3*EL*MH12&
  &*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.125D0*CA22*CA3*EL*MH12*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*S&
  &W) + (0.0625D0*CA3*EL*MH32*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA22*CA3*EL*MH32*dAlpha3PinchOS()*DBL&
  &E(SA1**INT(3.D0)))/(CB*MW*SW) - (0.125D0*CA3*EL*MH12*SA22*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.0625D0*CA3*EL&
  &*MH32*SA22*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA3*EL*m12squared*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0&
  &)))/(CB2*MW*SB*SW) - (0.1875D0*CA22*CA3*EL*m12squared*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL&
  &*m12squared*SA22*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D0*EL*MH12*SA2*SA3*dAlpha3PinchOS()*DBLE(SA1**&
  &INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA22*EL*MH12*SA2*SA3*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.03125D0*EL*MH32&
  &*SA2*SA3*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA22*EL*MH32*SA2*SA3*dAlpha3PinchOS()*DBLE(SA1**INT(3.&
  &D0)))/(MW*SB*SW) + (0.09375D0*EL*m12squared*SA2*SA3*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA22*EL&
  &*m12squared*SA2*SA3*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.03125D0*CA3*EL*MH12*SA2*dBeta2PinchOS()*DBLE(SA&
  &1**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA22*CA3*EL*MH12*SA2*dBeta2PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.015625D0*CA3&
  &*EL*MH32*SA2*dBeta2PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.046875D0*CA22*CA3*EL*MH32*SA2*dBeta2PinchOS()*DBLE(SA1**INT&
  &(3.D0)))/(CB*MW*SW) - (0.140625D0*CA3*EL*m12squared*SA2*dBeta2PinchOS()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.421875D0*CA2&
  &2*CA3*EL*m12squared*SA2*dBeta2PinchOS()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.03125D0*CA3*EL*MH12*SA2*dBeta2PinchOS()*DBLE&
  &(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.09375D0*CA22*CA3*EL*MH12*SA2*dBeta2PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.0&
  &15625D0*CA3*EL*MH32*SA2*dBeta2PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.046875D0*CA22*CA3*EL*MH32*SA2*dBeta2PinchOS(&
  &)*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*EL*m12squared*SA3*dBeta2PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0&
  &.1875D0*CA22*EL*m12squared*SA3*dBeta2PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.1875D0*EL*m12squared*SA22*SA3*dBeta2P&
  &inchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.03125D0*CA3*EL*MH12*SA2*dBeta2PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB)&
  & - (0.09375D0*CA22*CA3*EL*MH12*SA2*dBeta2PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.015625D0*CA3*EL*MH32*SA2*dBeta2Pin&
  &chOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.046875D0*CA22*CA3*EL*MH32*SA2*dBeta2PinchOS()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*&
  &TB) + (0.125D0*EL*MH12*SA3*TB*dBeta2PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.125D0*CA22*EL*MH12*SA3*TB*dBeta2PinchOS()*&
  &DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*EL*MH32*SA3*TB*dBeta2PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA22*&
  &EL*MH32*SA3*TB*dBeta2PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.125D0*EL*MH12*SA22*SA3*TB*dBeta2PinchOS()*DBLE(SA1**INT(3&
  &.D0)))/(CB*MW*SW) - (0.0625D0*EL*MH32*SA22*SA3*TB*dBeta2PinchOS()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA3*EL*MH12*dAl&
  &pha2PinchOS()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA3*EL*MH32*dAlpha2PinchOS()*DBLE(CA2**INT(3.&
  &D0))*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*dAlpha2PinchOS()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)&
  &))/(CB*MW*SB2*SW) - (0.375D0*EL*m12squared*SA3*dBeta2PinchOS()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) - (0.375D0*C&
  &A22*EL*m12squared*SA3*dBeta2PinchOS()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) + (0.375D0*EL*m12squared*SA22*SA3*dBe&
  &ta2PinchOS()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) - (0.28125D0*CA1*CA3*EL*m12squared*dAlpha1PinchOS()*DBLE(SA2**&
  &INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA3*EL*MH12*SA1*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA12*CA3*EL*M&
  &H12*SA1*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA3*EL*MH32*SA1*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/&
  &(CB*MW*SW) + (0.28125D0*CA12*CA3*EL*MH32*SA1*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*MH12*dAl&
  &pha1PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA1*CA3*EL*MH32*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) &
  &+ (0.28125D0*CA3*EL*m12squared*SA1*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA3*EL*m12squared*SA1*dAlpha1&
  &PinchOS()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.84375D0*CA12*CA3*EL*m12squared*SA1*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/&
  &(CB2*MW*SB*SW) - (0.5625D0*CA1*CA3*EL*MH12*SA12*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA1*CA3*EL*MH32&
  &*SA12*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*m12squared*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)&
  &))/(CB*MW*SB2*SW) + (0.84375D0*CA1*CA3*EL*m12squared*SA12*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.09375D0*C&
  &A1*CA3*EL*m12squared*dAlpha1PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA3*EL*m12squared*SA1*TB*dAlpha1PinchOS&
  &()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (1.5D0*CA3*MH12*dAlpha2PinchOS()*DBLE(SA2**INT(3.D0)))/vS + (0.75D0*CA3*MH32*dAlpha2Pinc&
  &hOS()*DBLE(SA2**INT(3.D0)))/vS + (0.1875D0*CA1*EL*MH12*SA3*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1*&
  &EL*MH32*SA3*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*EL*m12squared*SA1*SA3*dAlpha3PinchOS()*DBLE(SA2**IN&
  &T(3.D0)))/(CB*MW*SW) - (0.1875D0*CA1*EL*MH12*SA12*SA3*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1*EL*MH&
  &32*SA12*SA3*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA1*EL*m12squared*SA3*dAlpha3PinchOS()*DBLE(SA2**IN&
  &T(3.D0)))/(MW*SB*SW) - (0.1875D0*CA1*EL*m12squared*SA3*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*EL*MH&
  &12*SA1*SA3*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA12*EL*MH12*SA1*SA3*dAlpha3PinchOS()*DBLE(SA2**INT(3&
  &.D0)))/(MW*SB*SW) + (0.09375D0*EL*MH32*SA1*SA3*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA12*EL*MH32*SA1&
  &*SA3*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.28125D0*CA1*EL*m12squared*SA12*SA3*dAlpha3PinchOS()*DBLE(SA2**INT(&
  &3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*EL*m12squared*SA1*SA3*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA&
  &12*EL*m12squared*SA1*SA3*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.09375D0*EL*m12squared*SA1*SA3*dAlpha3Pinch&
  &OS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA1*EL*m12squared*SA3*TB*dAlpha3PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*S&
  &W) - (0.09375D0*CA3*EL*MH12*SA1*dBeta2PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA12*CA3*EL*MH12*SA1*dBeta2Pinch&
  &OS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.046875D0*CA3*EL*MH32*SA1*dBeta2PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.0468&
  &75D0*CA12*CA3*EL*MH32*SA1*dBeta2PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA3*EL*m12squared*SA1*dBeta2PinchOS()*&
  &DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.328125D0*CA3*EL*m12squared*SA1*dBeta2PinchOS()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0&
  &.421875D0*CA12*CA3*EL*m12squared*SA1*dBeta2PinchOS()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA1*CA3*EL*m12squared*d&
  &Beta2PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.09375D0*CA3*EL*MH12*SA1*dBeta2PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*&
  &SB2*SW) - (0.09375D0*CA12*CA3*EL*MH12*SA1*dBeta2PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.046875D0*CA3*EL*MH32*SA1*d&
  &Beta2PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.046875D0*CA12*CA3*EL*MH32*SA1*dBeta2PinchOS()*DBLE(SA2**INT(3.D0)))/(&
  &CB*MW*SB2*SW) + (0.28125D0*CA1*CA3*EL*m12squared*SA12*dBeta2PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*CA1*CA3&
  &*EL*m12squared*dBeta2PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.09375D0*CA3*EL*MH12*SA1*dBeta2PinchOS()*DBLE(SA2**INT(&
  &3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA12*CA3*EL*MH12*SA1*dBeta2PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.046875D0*CA3*&
  &EL*MH32*SA1*dBeta2PinchOS()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.046875D0*CA12*CA3*EL*MH32*SA1*dBeta2PinchOS()*DBLE(SA2**I&
  &NT(3.D0)))/(MW*SB*SW*TB) - (0.1875D0*CA1*CA3*EL*MH12*TB*dBeta2PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1*CA3*&
  &EL*MH32*TB*dBeta2PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.328125D0*CA3*EL*m12squared*SA1*TB*dBeta2PinchOS()*DBLE(SA2**I&
  &NT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA1*CA3*EL*MH12*SA12*TB*dBeta2PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1*CA&
  &3*EL*MH32*SA12*TB*dBeta2PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.140625D0*CA3*EL*m12squared*SA1*dBeta2PinchOS()*DBLE(SA&
  &2**INT(3.D0)))/(MW*SB*SW*TB2) + (0.1875D0*CA1*CA3*EL*m12squared*TB2*dBeta2PinchOS()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.1875&
  &D0*CA3*EL*MH12*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA3*EL*MH32*dAlpha1PinchOS(&
  &)*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*dAlpha1PinchOS()*DBLE(CA1**INT(3.D0))*D&
  &BLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*EL*MH12*SA3*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW&
  &*SW) + (0.03125D0*EL*MH32*SA3*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*EL*m12square&
  &d*SA3*dAlpha3PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA3*EL*m12squared*dBeta2PinchOS(&
  &)*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.0625D0*CA3*EL*MH12*TB*dBeta2PinchOS()*DBLE(CA1**INT(3.D0))*DB&
  &LE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*CA3*EL*MH32*TB*dBeta2PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*S&
  &W) + (0.375D0*CA1*CA3*EL*m12squared*dBeta2PinchOS()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) - (0.5625D0*CA1*CA3*EL*&
  &m12squared*SA12*dBeta2PinchOS()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) + (0.1875D0*CA3*EL*m12squared*dBeta2PinchOS&
  &()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) - (0.1875D0*CA3*EL*MH12*dAlpha1PinchOS()*DBLE(SA1**&
  &INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA3*EL*MH32*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)&
  &))/(CB*MW*SW) + (0.28125D0*CA3*EL*m12squared*dAlpha1PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.0&
  &625D0*EL*MH12*SA3*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*EL*MH32*SA3*dAlpha3Pinch&
  &OS()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*EL*m12squared*SA3*dAlpha3PinchOS()*DBLE(SA1**INT(3.D0)&
  &)*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.03125D0*CA3*EL*MH12*dBeta2PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB&
  &*MW*SW) - (0.015625D0*CA3*EL*MH32*dBeta2PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.140625D0*CA3*EL*m&
  &12squared*dBeta2PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.03125D0*CA3*EL*MH12*dBeta2PinchOS()*D&
  &BLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.015625D0*CA3*EL*MH32*dBeta2PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(S&
  &A2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.03125D0*CA3*EL*MH12*dBeta2PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*T&
  &B) + (0.015625D0*CA3*EL*MH32*dBeta2PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.328125D0*CA3*EL*m12&
  &squared*SA1*SA2*dBeta2PinchOS()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.421875D0*CA12*CA3*EL*m12squared*SA1*SA2*dBeta2PinchOS()*DBL&
  &E(SB**INT(-3.D0)))/(MW*SW) + (0.984375D0*CA22*CA3*EL*m12squared*SA1*SA2*dBeta2PinchOS()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (1.265&
  &625D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*dBeta2PinchOS()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.09375D0*CA3*EL*m12squared*SA1*SA2&
  &*dBeta2PinchOS()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.140625D0*CA12*CA3*EL*m12squared*SA1*SA2*dBeta2PinchOS()*DBLE(SB**INT(-&
  &3.D0)))/(CB2*MW*SW) + (0.28125D0*CA22*CA3*EL*m12squared*SA1*SA2*dBeta2PinchOS()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.421875D&
  &0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*dBeta2PinchOS()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.21875D0*CA1*EL*m12squared*SA3*dBe&
  &ta2PinchOS()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.21875D0*CA1*CA22*EL*m12squared*SA3*dBeta2PinchOS()*DBLE(SB**INT(-3.D0)))/(MW*S&
  &W) + (0.0625D0*CA1*EL*m12squared*SA3*dBeta2PinchOS()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.0625D0*CA1*CA22*EL*m12squared*SA3*&
  &dBeta2PinchOS()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.84375D0*CA1*EL*m12squared*SA12*SA3*dBeta2PinchOS()*DBLE(SB**INT(-3.D0))&
  &)/(MW*SW) + (0.84375D0*CA1*CA22*EL*m12squared*SA12*SA3*dBeta2PinchOS()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.28125D0*CA1*EL*m12sq&
  &uared*SA12*SA3*dBeta2PinchOS()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.28125D0*CA1*CA22*EL*m12squared*SA12*SA3*dBeta2PinchOS()*&
  &DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.21875D0*CA1*EL*m12squared*SA22*SA3*dBeta2PinchOS()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.0&
  &625D0*CA1*EL*m12squared*SA22*SA3*dBeta2PinchOS()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.84375D0*CA1*EL*m12squared*SA12*SA22*SA&
  &3*dBeta2PinchOS()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.28125D0*CA1*EL*m12squared*SA12*SA22*SA3*dBeta2PinchOS()*DBLE(SB**INT(-3.D&
  &0)))/(CB2*MW*SW) - (0.28125D0*EL*m12squared*SA3*dBeta2PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.28125D&
  &0*CA22*EL*m12squared*SA3*dBeta2PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.09375D0*EL*m12squared*SA3*dBe&
  &ta2PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.09375D0*CA22*EL*m12squared*SA3*dBeta2PinchOS()*DBLE(C&
  &A1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.28125D0*EL*m12squared*SA22*SA3*dBeta2PinchOS()*DBLE(CA1**INT(3.D0))*DBLE&
  &(SB**INT(-3.D0)))/(MW*SW) + (0.09375D0*EL*m12squared*SA22*SA3*dBeta2PinchOS()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*&
  &MW*SW) + (0.140625D0*CA3*EL*m12squared*SA2*dBeta2PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.421875D0*CA&
  &22*CA3*EL*m12squared*SA2*dBeta2PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.046875D0*CA3*EL*m12squared*SA&
  &2*dBeta2PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.140625D0*CA22*CA3*EL*m12squared*SA2*dBeta2PinchO&
  &S()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.328125D0*CA3*EL*m12squared*SA1*dBeta2PinchOS()*DBLE(SA2**INT(3&
  &.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.421875D0*CA12*CA3*EL*m12squared*SA1*dBeta2PinchOS()*DBLE(SA2**INT(3.D0))*DBLE(SB**INT&
  &(-3.D0)))/(MW*SW) - (0.09375D0*CA3*EL*m12squared*SA1*dBeta2PinchOS()*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + &
  &(0.140625D0*CA12*CA3*EL*m12squared*SA1*dBeta2PinchOS()*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.140625D0*CA&
  &3*EL*m12squared*dBeta2PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.046875D0*CA3*EL*m&
  &12squared*dBeta2PinchOS()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.1875D0*CA1*CA3*MH12&
  &*SA2*dgAtMZ())/(CB*MW) + (0.5625D0*CA1*CA22*CA3*MH12*SA2*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*CA3*MH32*SA2*dgAtMZ())/(CB*MW) + (&
  &0.28125D0*CA1*CA22*CA3*MH32*SA2*dgAtMZ())/(CB*MW) + (0.28125D0*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*MW) + (0.84375D0*CA22*CA3*&
  &m12squared*SA1*SA2*dgAtMZ())/(CB*MW) - (0.1875D0*CA1*CA3*MH12*SA12*SA2*dgAtMZ())/(CB*MW) - (0.5625D0*CA1*CA22*CA3*MH12*SA12*SA&
  &2*dgAtMZ())/(CB*MW) - (0.09375D0*CA1*CA3*MH32*SA12*SA2*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*CA3*MH32*SA12*SA2*dgAtMZ())/(CB&
  &*MW) + (0.1875D0*CA1*m12squared*SA3*dgAtMZ())/(CB*MW) + (0.1875D0*CA1*CA22*m12squared*SA3*dgAtMZ())/(CB*MW) - (0.125D0*MH12*SA&
  &1*SA3*dgAtMZ())/(CB*MW) - (0.375D0*CA12*MH12*SA1*SA3*dgAtMZ())/(CB*MW) - (0.125D0*CA22*MH12*SA1*SA3*dgAtMZ())/(CB*MW) - (0.375&
  &D0*CA12*CA22*MH12*SA1*SA3*dgAtMZ())/(CB*MW) - (0.0625D0*MH32*SA1*SA3*dgAtMZ())/(CB*MW) - (0.1875D0*CA12*MH32*SA1*SA3*dgAtMZ())&
  &/(CB*MW) - (0.0625D0*CA22*MH32*SA1*SA3*dgAtMZ())/(CB*MW) - (0.1875D0*CA12*CA22*MH32*SA1*SA3*dgAtMZ())/(CB*MW) - (0.1875D0*CA1*&
  &m12squared*SA22*SA3*dgAtMZ())/(CB*MW) + (0.125D0*MH12*SA1*SA22*SA3*dgAtMZ())/(CB*MW) + (0.375D0*CA12*MH12*SA1*SA22*SA3*dgAtMZ(&
  &))/(CB*MW) + (0.0625D0*MH32*SA1*SA22*SA3*dgAtMZ())/(CB*MW) + (0.1875D0*CA12*MH32*SA1*SA22*SA3*dgAtMZ())/(CB*MW) + (0.28125D0*C&
  &A1*CA3*m12squared*SA2*dgAtMZ())/(MW*SB) + (0.84375D0*CA1*CA22*CA3*m12squared*SA2*dgAtMZ())/(MW*SB) - (0.1875D0*CA1*CA3*m12squa&
  &red*SA2*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA1*CA22*CA3*m12squared*SA2*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*CA3*MH12*SA1*SA2*dgAtM&
  &Z())/(MW*SB) - (0.1875D0*CA12*CA3*MH12*SA1*SA2*dgAtMZ())/(MW*SB) + (0.5625D0*CA22*CA3*MH12*SA1*SA2*dgAtMZ())/(MW*SB) - (0.5625&
  &D0*CA12*CA22*CA3*MH12*SA1*SA2*dgAtMZ())/(MW*SB) + (0.09375D0*CA3*MH32*SA1*SA2*dgAtMZ())/(MW*SB) - (0.09375D0*CA12*CA3*MH32*SA1&
  &*SA2*dgAtMZ())/(MW*SB) + (0.28125D0*CA22*CA3*MH32*SA1*SA2*dgAtMZ())/(MW*SB) - (0.28125D0*CA12*CA22*CA3*MH32*SA1*SA2*dgAtMZ())/&
  &(MW*SB) + (0.28125D0*CA1*CA3*m12squared*SA12*SA2*dgAtMZ())/(CB2*MW*SB) + (0.84375D0*CA1*CA22*CA3*m12squared*SA12*SA2*dgAtMZ())&
  &/(CB2*MW*SB) + (0.125D0*CA1*MH12*SA3*dgAtMZ())/(MW*SB) + (0.125D0*CA1*CA22*MH12*SA3*dgAtMZ())/(MW*SB) + (0.0625D0*CA1*MH32*SA3&
  &*dgAtMZ())/(MW*SB) + (0.0625D0*CA1*CA22*MH32*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*m12squared*SA1*SA3*dgAtMZ())/(MW*SB) - (0.1875D&
  &0*CA22*m12squared*SA1*SA3*dgAtMZ())/(MW*SB) + (0.125D0*m12squared*SA1*SA3*dgAtMZ())/(CB2*MW*SB) + (0.5625D0*CA12*m12squared*SA&
  &1*SA3*dgAtMZ())/(CB2*MW*SB) + (0.125D0*CA22*m12squared*SA1*SA3*dgAtMZ())/(CB2*MW*SB) + (0.5625D0*CA12*CA22*m12squared*SA1*SA3*&
  &dgAtMZ())/(CB2*MW*SB) + (0.375D0*CA1*MH12*SA12*SA3*dgAtMZ())/(MW*SB) + (0.375D0*CA1*CA22*MH12*SA12*SA3*dgAtMZ())/(MW*SB) + (0.&
  &1875D0*CA1*MH32*SA12*SA3*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*CA22*MH32*SA12*SA3*dgAtMZ())/(MW*SB) - (0.125D0*CA1*MH12*SA22*SA3*d&
  &gAtMZ())/(MW*SB) - (0.0625D0*CA1*MH32*SA22*SA3*dgAtMZ())/(MW*SB) + (0.1875D0*m12squared*SA1*SA22*SA3*dgAtMZ())/(MW*SB) - (0.12&
  &5D0*m12squared*SA1*SA22*SA3*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA12*m12squared*SA1*SA22*SA3*dgAtMZ())/(CB2*MW*SB) - (0.375D0*CA&
  &1*MH12*SA12*SA22*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*CA1*MH32*SA12*SA22*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*CA3*m12squared*SA1*SA2&
  &*dgAtMZ())/(CB*MW*SB2) + (0.28125D0*CA12*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA22*CA3*m12squared*SA1*SA2*&
  &dgAtMZ())/(CB*MW*SB2) + (0.84375D0*CA12*CA22*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*MW*SB2) - (0.125D0*CA1*m12squared*SA3*dgAtMZ&
  &())/(CB*MW*SB2) - (0.125D0*CA1*CA22*m12squared*SA3*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA1*m12squared*SA12*SA3*dgAtMZ())/(CB*MW*&
  &SB2) - (0.5625D0*CA1*CA22*m12squared*SA12*SA3*dgAtMZ())/(CB*MW*SB2) + (0.125D0*CA1*m12squared*SA22*SA3*dgAtMZ())/(CB*MW*SB2) +&
  & (0.5625D0*CA1*m12squared*SA12*SA22*SA3*dgAtMZ())/(CB*MW*SB2) - (0.09375D0*CA3*m12squared*SA1*SA2*dgAtMZ())/(MW*SB*TB) - (0.28&
  &125D0*CA22*CA3*m12squared*SA1*SA2*dgAtMZ())/(MW*SB*TB) - (0.0625D0*CA1*m12squared*SA3*dgAtMZ())/(MW*SB*TB) - (0.0625D0*CA1*CA2&
  &2*m12squared*SA3*dgAtMZ())/(MW*SB*TB) + (0.0625D0*CA1*m12squared*SA22*SA3*dgAtMZ())/(MW*SB*TB) - (0.09375D0*CA1*CA3*m12squared&
  &*SA2*TB*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*CA3*m12squared*SA2*TB*dgAtMZ())/(CB*MW) + (0.0625D0*m12squared*SA1*SA3*TB*dgAt&
  &MZ())/(CB*MW) + (0.0625D0*CA22*m12squared*SA1*SA3*TB*dgAtMZ())/(CB*MW) - (0.0625D0*m12squared*SA1*SA22*SA3*TB*dgAtMZ())/(CB*MW&
  &) + (0.0625D0*CA3*MH12*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875D0*CA22*CA3*MH12*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())&
  &/(CB*MW) + (0.03125D0*CA3*MH32*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA22*CA3*MH32*SA2*DBLE(CA1**INT(3.D0))*&
  &dgAtMZ())/(CB*MW) - (0.09375D0*CA3*m12squared*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.28125D0*CA22*CA3*m12squared*&
  &SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.125D0*MH12*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.125D0*CA22*MH12&
  &*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.0625D0*MH32*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.0625D0*CA22*MH32*&
  &SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.125D0*MH12*SA22*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.0625D0*MH32*SA&
  &22*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*m12squared*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.1875D&
  &0*CA22*m12squared*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*m12squared*SA22*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ()&
  &)/(CB*MW*SB2) + (0.125D0*MH12*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.125D0*CA22*MH12*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ(&
  &))/(CB*MW) + (0.0625D0*MH32*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.0625D0*CA22*MH32*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ()&
  &)/(CB*MW) - (0.125D0*MH12*SA22*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*MH32*SA22*SA3*DBLE(SA1**INT(3.D0))*dgAtM&
  &Z())/(CB*MW) + (0.0625D0*CA3*MH12*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA22*CA3*MH12*SA2*DBLE(SA1**INT(3.D0)&
  &)*dgAtMZ())/(MW*SB) + (0.03125D0*CA3*MH32*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA22*CA3*MH32*SA2*DBLE(SA1**&
  &INT(3.D0))*dgAtMZ())/(MW*SB) - (0.1875D0*m12squared*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875D0*CA22*m12squared&
  &*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*m12squared*SA22*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (&
  &0.09375D0*CA3*m12squared*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA22*CA3*m12squared*SA2*DBLE(SA1**INT(3.D&
  &0))*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*CA1*CA3*MH12*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.09375D0*CA1*CA3*MH32*DBLE(SA2**&
  &INT(3.D0))*dgAtMZ())/(CB*MW) - (0.28125D0*CA3*m12squared*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875D0*CA1*CA3*MH12*S&
  &A12*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*CA3*MH32*SA12*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.28125D0*&
  &CA1*CA3*m12squared*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*CA3*m12squared*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*M&
  &W*SB) - (0.1875D0*CA3*MH12*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA12*CA3*MH12*SA1*DBLE(SA2**INT(3.D0))*dgAtM&
  &Z())/(MW*SB) - (0.09375D0*CA3*MH32*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA12*CA3*MH32*SA1*DBLE(SA2**INT(3.D&
  &0))*dgAtMZ())/(MW*SB) - (0.28125D0*CA1*CA3*m12squared*SA12*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*CA3*m12squar&
  &ed*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA12*CA3*m12squared*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*S&
  &B2) + (0.09375D0*CA3*m12squared*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB*TB) + (0.09375D0*CA1*CA3*m12squared*TB*DBLE(SA2**INT&
  &(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*CA3*MH12*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.03125D0*CA3*MH&
  &32*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA3*m12squared*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(&
  &3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.0625D0*CA3*MH12*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.03125D0*CA3&
  &*MH32*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA3*m12squared*DBLE(SA1**INT(3.D0))*DBLE(SA2**I&
  &NT(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.28125D0*CA3*EL*SA1*SA2*dm122MSBarAlter())/(CB*MW*SW) + (0.84375D0*CA22*CA3*EL*SA1*SA2*dm1&
  &22MSBarAlter())/(CB*MW*SW) + (0.1875D0*CA1*EL*SA3*dm122MSBarAlter())/(CB*MW*SW) + (0.1875D0*CA1*CA22*EL*SA3*dm122MSBarAlter())&
  &/(CB*MW*SW) - (0.1875D0*CA1*EL*SA22*SA3*dm122MSBarAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA3*EL*SA2*dm122MSBarAlter())/(MW*SB*SW&
  &) + (0.84375D0*CA1*CA22*CA3*EL*SA2*dm122MSBarAlter())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*SA2*dm122MSBarAlter())/(CB2*MW*SB*SW) &
  &- (0.5625D0*CA1*CA22*CA3*EL*SA2*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.28125D0*CA1*CA3*EL*SA12*SA2*dm122MSBarAlter())/(CB2*MW*&
  &SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*SA12*SA2*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.1875D0*EL*SA1*SA3*dm122MSBarAlter())/(MW*S&
  &B*SW) - (0.1875D0*CA22*EL*SA1*SA3*dm122MSBarAlter())/(MW*SB*SW) + (0.125D0*EL*SA1*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.5&
  &625D0*CA12*EL*SA1*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.125D0*CA22*EL*SA1*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.5625&
  &D0*CA12*CA22*EL*SA1*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.1875D0*EL*SA1*SA22*SA3*dm122MSBarAlter())/(MW*SB*SW) - (0.125D0&
  &*EL*SA1*SA22*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.5625D0*CA12*EL*SA1*SA22*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.187&
  &5D0*CA3*EL*SA1*SA2*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.28125D0*CA12*CA3*EL*SA1*SA2*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.5&
  &625D0*CA22*CA3*EL*SA1*SA2*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA22*CA3*EL*SA1*SA2*dm122MSBarAlter())/(CB*MW*SB&
  &2*SW) - (0.125D0*CA1*EL*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.125D0*CA1*CA22*EL*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) - (&
  &0.5625D0*CA1*EL*SA12*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*EL*SA12*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) &
  &+ (0.125D0*CA1*EL*SA22*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.5625D0*CA1*EL*SA12*SA22*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW&
  &) - (0.09375D0*CA3*EL*SA1*SA2*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.28125D0*CA22*CA3*EL*SA1*SA2*dm122MSBarAlter())/(MW*SB*SW*T&
  &B) - (0.0625D0*CA1*EL*SA3*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.0625D0*CA1*CA22*EL*SA3*dm122MSBarAlter())/(MW*SB*SW*TB) + (0.0&
  &625D0*CA1*EL*SA22*SA3*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.09375D0*CA1*CA3*EL*SA2*TB*dm122MSBarAlter())/(CB*MW*SW) - (0.28125&
  &D0*CA1*CA22*CA3*EL*SA2*TB*dm122MSBarAlter())/(CB*MW*SW) + (0.0625D0*EL*SA1*SA3*TB*dm122MSBarAlter())/(CB*MW*SW) + (0.0625D0*CA&
  &22*EL*SA1*SA3*TB*dm122MSBarAlter())/(CB*MW*SW) - (0.0625D0*EL*SA1*SA22*SA3*TB*dm122MSBarAlter())/(CB*MW*SW) - (0.09375D0*CA3*E&
  &L*SA2*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.28125D0*CA22*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dm122MSBarAlter&
  &())/(CB2*MW*SB*SW) + (0.1875D0*EL*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.1875D0*CA22*EL*SA3*DBLE(CA1*&
  &*INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.1875D0*EL*SA22*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) -&
  & (0.1875D0*EL*SA3*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.1875D0*CA22*EL*SA3*DBLE(SA1**INT(3.D0))*dm122MSB&
  &arAlter())/(CB2*MW*SB*SW) + (0.1875D0*EL*SA22*SA3*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.09375D0*CA3*EL*S&
  &A2*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA22*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())&
  &/(CB*MW*SB2*SW) - (0.28125D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SW) - (0.28125D0*CA1*CA3*EL*DBLE(SA2**I&
  &NT(3.D0))*dm122MSBarAlter())/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.281&
  &25D0*CA1*CA3*EL*SA12*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dm122M&
  &SBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA12*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.09375D0*C&
  &A3*EL*SA1*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(MW*SB*SW*TB) + (0.09375D0*CA1*CA3*EL*TB*DBLE(SA2**INT(3.D0))*dm122MSBarAlte&
  &r())/(CB*MW*SW) + (0.09375D0*CA3*EL*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.09375D0*C&
  &A3*EL*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.1875D0*CA1*CA3*EL*SA2*dMH12OSAlter())/(&
  &CB*MW*SW) + (0.5625D0*CA1*CA22*CA3*EL*SA2*dMH12OSAlter())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*SA12*SA2*dMH12OSAlter())/(CB*MW*SW&
  &) - (0.5625D0*CA1*CA22*CA3*EL*SA12*SA2*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*EL*SA1*SA3*dMH12OSAlter())/(CB*MW*SW) - (0.375D0*&
  &CA12*EL*SA1*SA3*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*CA22*EL*SA1*SA3*dMH12OSAlter())/(CB*MW*SW) - (0.375D0*CA12*CA22*EL*SA1*S&
  &A3*dMH12OSAlter())/(CB*MW*SW) + (0.125D0*EL*SA1*SA22*SA3*dMH12OSAlter())/(CB*MW*SW) + (0.375D0*CA12*EL*SA1*SA22*SA3*dMH12OSAlt&
  &er())/(CB*MW*SW) + (0.1875D0*CA3*EL*SA1*SA2*dMH12OSAlter())/(MW*SB*SW) - (0.1875D0*CA12*CA3*EL*SA1*SA2*dMH12OSAlter())/(MW*SB*&
  &SW) + (0.5625D0*CA22*CA3*EL*SA1*SA2*dMH12OSAlter())/(MW*SB*SW) - (0.5625D0*CA12*CA22*CA3*EL*SA1*SA2*dMH12OSAlter())/(MW*SB*SW)&
  & + (0.125D0*CA1*EL*SA3*dMH12OSAlter())/(MW*SB*SW) + (0.125D0*CA1*CA22*EL*SA3*dMH12OSAlter())/(MW*SB*SW) + (0.375D0*CA1*EL*SA12&
  &*SA3*dMH12OSAlter())/(MW*SB*SW) + (0.375D0*CA1*CA22*EL*SA12*SA3*dMH12OSAlter())/(MW*SB*SW) - (0.125D0*CA1*EL*SA22*SA3*dMH12OSA&
  &lter())/(MW*SB*SW) - (0.375D0*CA1*EL*SA12*SA22*SA3*dMH12OSAlter())/(MW*SB*SW) - (0.5D0*CA2*CA3*dMH12OSAlter())/vS - (1.5D0*CA2&
  &*CA3*SA22*dMH12OSAlter())/vS + (0.0625D0*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) + (0.1875D0*CA22*CA3*EL*SA&
  &2*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*EL*SA3*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) - (0.125D0&
  &*CA22*EL*SA3*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.125D0*EL*SA22*SA3*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(MW*S&
  &B*SW) + (0.5D0*CA3*DBLE(CA2**INT(3.D0))*dMH12OSAlter())/vS + (0.125D0*EL*SA3*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) +&
  & (0.125D0*CA22*EL*SA3*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*EL*SA22*SA3*DBLE(SA1**INT(3.D0))*dMH12OSAlter&
  &())/(CB*MW*SW) + (0.0625D0*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.1875D0*CA22*CA3*EL*SA2*DBLE(SA1**IN&
  &T(3.D0))*dMH12OSAlter())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) + (0.1875D0*CA1*CA3&
  &*EL*SA12*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.1875D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW&
  &) + (0.1875D0*CA12*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) - (0.0625D0*CA3*EL*DBLE(CA1**INT(3.D0))*DBLE(SA2&
  &**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.0625D0*CA3*EL*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW&
  &) + (0.09375D0*CA1*CA3*EL*SA2*dMH32OSAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA22*CA3*EL*SA2*dMH32OSAlter())/(CB*MW*SW) - (0.0937&
  &5D0*CA1*CA3*EL*SA12*SA2*dMH32OSAlter())/(CB*MW*SW) - (0.28125D0*CA1*CA22*CA3*EL*SA12*SA2*dMH32OSAlter())/(CB*MW*SW) - (0.0625D&
  &0*EL*SA1*SA3*dMH32OSAlter())/(CB*MW*SW) - (0.1875D0*CA12*EL*SA1*SA3*dMH32OSAlter())/(CB*MW*SW) - (0.0625D0*CA22*EL*SA1*SA3*dMH&
  &32OSAlter())/(CB*MW*SW) - (0.1875D0*CA12*CA22*EL*SA1*SA3*dMH32OSAlter())/(CB*MW*SW) + (0.0625D0*EL*SA1*SA22*SA3*dMH32OSAlter()&
  &)/(CB*MW*SW) + (0.1875D0*CA12*EL*SA1*SA22*SA3*dMH32OSAlter())/(CB*MW*SW) + (0.09375D0*CA3*EL*SA1*SA2*dMH32OSAlter())/(MW*SB*SW&
  &) - (0.09375D0*CA12*CA3*EL*SA1*SA2*dMH32OSAlter())/(MW*SB*SW) + (0.28125D0*CA22*CA3*EL*SA1*SA2*dMH32OSAlter())/(MW*SB*SW) - (0&
  &.28125D0*CA12*CA22*CA3*EL*SA1*SA2*dMH32OSAlter())/(MW*SB*SW) + (0.0625D0*CA1*EL*SA3*dMH32OSAlter())/(MW*SB*SW) + (0.0625D0*CA1&
  &*CA22*EL*SA3*dMH32OSAlter())/(MW*SB*SW) + (0.1875D0*CA1*EL*SA12*SA3*dMH32OSAlter())/(MW*SB*SW) + (0.1875D0*CA1*CA22*EL*SA12*SA&
  &3*dMH32OSAlter())/(MW*SB*SW) - (0.0625D0*CA1*EL*SA22*SA3*dMH32OSAlter())/(MW*SB*SW) - (0.1875D0*CA1*EL*SA12*SA22*SA3*dMH32OSAl&
  &ter())/(MW*SB*SW) - (0.25D0*CA2*CA3*dMH32OSAlter())/vS - (0.75D0*CA2*CA3*SA22*dMH32OSAlter())/vS + (0.03125D0*CA3*EL*SA2*DBLE(&
  &CA1**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) + (0.09375D0*CA22*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) - (0.0&
  &625D0*EL*SA3*DBLE(CA1**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) - (0.0625D0*CA22*EL*SA3*DBLE(CA1**INT(3.D0))*dMH32OSAlter())/(MW*&
  &SB*SW) + (0.0625D0*EL*SA22*SA3*DBLE(CA1**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) + (0.25D0*CA3*DBLE(CA2**INT(3.D0))*dMH32OSAlter&
  &())/vS + (0.0625D0*EL*SA3*DBLE(SA1**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) + (0.0625D0*CA22*EL*SA3*DBLE(SA1**INT(3.D0))*dMH32OS&
  &Alter())/(CB*MW*SW) - (0.0625D0*EL*SA22*SA3*DBLE(SA1**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) + (0.03125D0*CA3*EL*SA2*DBLE(SA1**&
  &INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) + (0.09375D0*CA22*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) - (0.09375D&
  &0*CA1*CA3*EL*DBLE(SA2**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) + (0.09375D0*CA1*CA3*EL*SA12*DBLE(SA2**INT(3.D0))*dMH32OSAlter())&
  &/(CB*MW*SW) - (0.09375D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) + (0.09375D0*CA12*CA3*EL*SA1*DBLE(SA2**INT&
  &(3.D0))*dMH32OSAlter())/(MW*SB*SW) - (0.03125D0*CA3*EL*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) - &
  &(0.03125D0*CA3*EL*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) - (0.09375D0*CA1*CA3*EL*MH12*SA2*DBLE(M&
  &W**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.28125D0*CA1*CA22*CA3*EL*MH12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.04687&
  &5D0*CA1*CA3*EL*MH32*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.140625D0*CA1*CA22*CA3*EL*MH32*SA2*DBLE(MW**INT(-3.D0))*&
  &dMW2Alter())/(CB*SW) - (0.140625D0*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.421875D0*CA22*CA3*&
  &EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.09375D0*CA1*CA3*EL*MH12*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW&
  &2Alter())/(CB*SW) + (0.28125D0*CA1*CA22*CA3*EL*MH12*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.046875D0*CA1*CA3*E&
  &L*MH32*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*CA1*CA22*CA3*EL*MH32*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW&
  &2Alter())/(CB*SW) - (0.09375D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA1*CA22*EL*m12squ&
  &ared*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0625D0*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.&
  &1875D0*CA12*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0625D0*CA22*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*dM&
  &W2Alter())/(CB*SW) + (0.1875D0*CA12*CA22*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*EL*MH32*SA1*SA&
  &3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.09375D0*CA12*EL*MH32*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0&
  &3125D0*CA22*EL*MH32*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.09375D0*CA12*CA22*EL*MH32*SA1*SA3*DBLE(MW**INT(-3.D&
  &0))*dMW2Alter())/(CB*SW) + (0.09375D0*CA1*EL*m12squared*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.0625D0*EL*MH12&
  &*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.1875D0*CA12*EL*MH12*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter()&
  &)/(CB*SW) - (0.03125D0*EL*MH32*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA12*EL*MH32*SA1*SA22*SA3*D&
  &BLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.140625D0*CA1*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (&
  &0.421875D0*CA1*CA22*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA1*CA3*EL*m12squared*SA2*DBL&
  &E(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.28125D0*CA1*CA22*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*&
  &SB*SW) - (0.09375D0*CA3*EL*MH12*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA12*CA3*EL*MH12*SA1*SA2*DBLE(M&
  &W**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.28125D0*CA22*CA3*EL*MH12*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.28125&
  &D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*CA3*EL*MH32*SA1*SA2*DBLE(MW**INT(-3.D&
  &0))*dMW2Alter())/(SB*SW) + (0.046875D0*CA12*CA3*EL*MH32*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.140625D0*CA22*C&
  &A3*EL*MH32*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.140625D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*DBLE(MW**INT(-3.D0))*&
  &dMW2Alter())/(SB*SW) - (0.140625D0*CA1*CA3*EL*m12squared*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.421875D0*&
  &CA1*CA22*CA3*EL*m12squared*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.0625D0*CA1*EL*MH12*SA3*DBLE(MW**INT(-3.&
  &D0))*dMW2Alter())/(SB*SW) - (0.0625D0*CA1*CA22*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.03125D0*CA1*EL*MH32*&
  &SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.03125D0*CA1*CA22*EL*MH32*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0&
  &.09375D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA22*EL*m12squared*SA1*SA3*DBLE(MW**INT(&
  &-3.D0))*dMW2Alter())/(SB*SW) - (0.0625D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.28125D0*CA12&
  &*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.0625D0*CA22*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0&
  &))*dMW2Alter())/(CB2*SB*SW) - (0.28125D0*CA12*CA22*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.18&
  &75D0*CA1*EL*MH12*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.1875D0*CA1*CA22*EL*MH12*SA12*SA3*DBLE(MW**INT(-3.D0))&
  &*dMW2Alter())/(SB*SW) - (0.09375D0*CA1*EL*MH32*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*CA1*CA22*EL*MH3&
  &2*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.0625D0*CA1*EL*MH12*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW&
  &) + (0.03125D0*CA1*EL*MH32*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*EL*m12squared*SA1*SA22*SA3*DBLE(MW*&
  &*INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.0625D0*EL*m12squared*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.28&
  &125D0*CA12*EL*m12squared*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.1875D0*CA1*EL*MH12*SA12*SA22*SA3*DBLE&
  &(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA1*EL*MH32*SA12*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09&
  &375D0*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.140625D0*CA12*CA3*EL*m12squared*SA1*SA2*DBL&
  &E(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.28125D0*CA22*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*S&
  &B2*SW) - (0.421875D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.0625D0*CA1*EL*m12s&
  &quared*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.0625D0*CA1*CA22*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter&
  &())/(CB*SB2*SW) + (0.28125D0*CA1*EL*m12squared*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.28125D0*CA1*CA22*EL&
  &*m12squared*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.0625D0*CA1*EL*m12squared*SA22*SA3*DBLE(MW**INT(-3.D0))&
  &*dMW2Alter())/(CB*SB2*SW) - (0.28125D0*CA1*EL*m12squared*SA12*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.0468&
  &75D0*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.140625D0*CA22*CA3*EL*m12squared*SA1*SA2*DBLE(&
  &MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.03125D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.0&
  &3125D0*CA1*CA22*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) - (0.03125D0*CA1*EL*m12squared*SA22*SA3*DBLE(MW&
  &**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.046875D0*CA1*CA3*EL*m12squared*SA2*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (&
  &0.140625D0*CA1*CA22*CA3*EL*m12squared*SA2*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*EL*m12squared*SA1*SA3*TB*D&
  &BLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA22*EL*m12squared*SA1*SA3*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) &
  &+ (0.03125D0*EL*m12squared*SA1*SA22*SA3*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA3*EL*MH12*SA2*DBLE(CA1**IN&
  &T(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA22*CA3*EL*MH12*SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))&
  &*dMW2Alter())/(CB*SW) - (0.015625D0*CA3*EL*MH32*SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.046875&
  &D0*CA22*CA3*EL*MH32*SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.046875D0*CA3*EL*m12squared*SA2*DBL&
  &E(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.140625D0*CA22*CA3*EL*m12squared*SA2*DBLE(CA1**INT(3.D0))*&
  &DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.0625D0*EL*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(&
  &SB*SW) + (0.0625D0*CA22*EL*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.03125D0*EL*MH32*SA3*DB&
  &LE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.03125D0*CA22*EL*MH32*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-&
  &3.D0))*dMW2Alter())/(SB*SW) - (0.0625D0*EL*MH12*SA22*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.0&
  &3125D0*EL*MH32*SA22*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*EL*m12squared*SA3*DBLE(CA1&
  &**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.09375D0*CA22*EL*m12squared*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**I&
  &NT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.09375D0*EL*m12squared*SA22*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())&
  &/(CB*SB2*SW) - (0.0625D0*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.0625D0*CA22*EL*MH12*S&
  &A3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*EL*MH32*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(&
  &3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA22*EL*MH32*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.&
  &0625D0*EL*MH12*SA22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*EL*MH32*SA22*SA3*DBLE(MW**&
  &INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA3*EL*MH12*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*d&
  &MW2Alter())/(SB*SW) - (0.09375D0*CA22*CA3*EL*MH12*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.0156&
  &25D0*CA3*EL*MH32*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*CA22*CA3*EL*MH32*SA2*DBLE(MW&
  &**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0&
  &))*dMW2Alter())/(CB2*SB*SW) + (0.09375D0*CA22*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB2*SB&
  &*SW) - (0.09375D0*EL*m12squared*SA22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.046875D0*CA3*&
  &EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.140625D0*CA22*CA3*EL*m12squared*SA2*&
  &DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.09375D0*CA1*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*DBLE(SA2*&
  &*INT(3.D0))*dMW2Alter())/(CB*SW) + (0.046875D0*CA1*CA3*EL*MH32*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) &
  &+ (0.140625D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA1*CA3*EL*MH1&
  &2*SA12*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.046875D0*CA1*CA3*EL*MH32*SA12*DBLE(MW**INT(-3.D0))*&
  &DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Al&
  &ter())/(SB*SW) - (0.09375D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.0937&
  &5D0*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*CA12*CA3*EL*MH12*SA1*DBLE(MW**&
  &INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.046875D0*CA3*EL*MH32*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*&
  &dMW2Alter())/(SB*SW) - (0.046875D0*CA12*CA3*EL*MH32*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.14&
  &0625D0*CA1*CA3*EL*m12squared*SA12*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.09375D0*CA3*EL*m12sq&
  &uared*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.140625D0*CA12*CA3*EL*m12squared*SA1*DBLE(MW*&
  &*INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.046875D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**&
  &INT(3.D0))*dMW2Alter())/(SB*SW*TB) - (0.046875D0*CA1*CA3*EL*m12squared*TB*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter(&
  &))/(CB*SW) + (0.03125D0*CA3*EL*MH12*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.0&
  &15625D0*CA3*EL*MH32*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.046875D0*CA3*EL*m&
  &12squared*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.03125D0*CA3*EL*MH12*DBL&
  &E(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.015625D0*CA3*EL*MH32*DBLE(MW**INT(-3.D0)&
  &)*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*DBLE(SA1&
  &**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + 0.5D0*CA2*CA3*MH12*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() + 0.25D0*&
  &CA2*CA3*MH32*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() + 1.5D0*CA2*CA3*MH12*SA22*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() + 0.75D0*CA2*&
  &CA3*MH32*SA22*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() - 0.5D0*CA3*MH12*DBLE(CA2**INT(3.D0))*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() &
  &- 0.25D0*CA3*MH32*DBLE(CA2**INT(3.D0))*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() &
		& + CS1S1S1f111*dZH1H3OSAlter()/2D0 + CS1S1S1f211*dZH2H3OSAlter()/2D0 + CS1S1S1f311*dZH3H3OS()/2D0 &
		& + CS1S1S1f131*dZH1H1OS()/2D0 + CS1S1S1f231*dZH2H1OSAlter()/2D0 + CS1S1S1f331*dZH3H1OSAlter()/2D0 &
		& + CS1S1S1f131*dZH1H1OS()/2D0 + CS1S1S1f231*dZH2H1OSAlter()/2D0 + CS1S1S1f331*dZH3H1OSAlter()/2D0 &
		& )
	case (9)
		dRR11 = -CA2*SA1*dAlpha1MSBarUsual() - CA1*SA2*dAlpha2MSBarUsual()
		dRR12 = CA1*CA2*dAlpha1MSBarUsual() - SA1*SA2*dAlpha2MSBarUsual()
		dRR13 = CA2*dAlpha2MSBarUsual()
		dRR21 = -CA1*CA3*dAlpha1MSBarUsual() - CA1*CA3*SA2*dAlpha3MSBarUsual() - CA1*CA2*SA3*dAlpha2MSBarUsual() + &
			& SA1*SA3*dAlpha3MSBarUsual() + SA1*SA2*SA3*dAlpha1MSBarUsual()
		dRR22 = -CA3*SA1*dAlpha1MSBarUsual() - CA3*SA1*SA2*dAlpha3MSBarUsual() - CA1*SA3*dAlpha3MSBarUsual() - &
			& CA2*SA1*SA3*dAlpha2MSBarUsual() - CA1*SA2*SA3*dAlpha1MSBarUsual()
		dRR23 = CA2*CA3*dAlpha3MSBarUsual() - SA2*SA3*dAlpha2MSBarUsual()
		dRR31 = -CA1*CA2*CA3*dAlpha2MSBarUsual() + CA3*SA1*dAlpha3MSBarUsual() + CA3*SA1*SA2*dAlpha1MSBarUsual() + &
			& CA1*SA3*dAlpha1MSBarUsual() + CA1*SA2*SA3*dAlpha3MSBarUsual()
		dRR32 = -CA1*CA3*dAlpha3MSBarUsual() - CA2*CA3*SA1*dAlpha2MSBarUsual() - CA1*CA3*SA2*dAlpha1MSBarUsual() + &
			& SA1*SA3*dAlpha1MSBarUsual() + SA1*SA2*SA3*dAlpha3MSBarUsual()
		dRR33 = -CA3*SA2*dAlpha2MSBarUsual() - CA2*SA3*dAlpha3MSBarUsual()

		totalAmplitude = CS1S1S1f311*( &
&(0.28125D0*CA1*CA3*EL*m12squared*SA2*dAlpha1MSBarUsual())/(CB*MW*SW) + (0.84375D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha1MSBarUsua&
  &l())/(CB*MW*SW) - (0.1875D0*CA3*EL*MH12*SA1*SA2*dAlpha1MSBarUsual())/(CB*MW*SW) - (0.5625D0*CA12*CA3*EL*MH12*SA1*SA2*dAlpha1MS&
  &BarUsual())/(CB*MW*SW) - (0.5625D0*CA22*CA3*EL*MH12*SA1*SA2*dAlpha1MSBarUsual())/(CB*MW*SW) - (1.6875D0*CA12*CA22*CA3*EL*MH12*&
  &SA1*SA2*dAlpha1MSBarUsual())/(CB*MW*SW) - (0.09375D0*CA3*EL*MH32*SA1*SA2*dAlpha1MSBarUsual())/(CB*MW*SW) - (0.28125D0*CA12*CA3&
  &*EL*MH32*SA1*SA2*dAlpha1MSBarUsual())/(CB*MW*SW) - (0.28125D0*CA22*CA3*EL*MH32*SA1*SA2*dAlpha1MSBarUsual())/(CB*MW*SW) - (0.84&
  &375D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*dAlpha1MSBarUsual())/(CB*MW*SW) - (0.125D0*CA1*EL*MH12*SA3*dAlpha1MSBarUsual())/(CB*MW*SW)&
  & - (0.125D0*CA1*CA22*EL*MH12*SA3*dAlpha1MSBarUsual())/(CB*MW*SW) - (0.0625D0*CA1*EL*MH32*SA3*dAlpha1MSBarUsual())/(CB*MW*SW) -&
  & (0.0625D0*CA1*CA22*EL*MH32*SA3*dAlpha1MSBarUsual())/(CB*MW*SW) - (0.1875D0*EL*m12squared*SA1*SA3*dAlpha1MSBarUsual())/(CB*MW*&
  &SW) - (0.1875D0*CA22*EL*m12squared*SA1*SA3*dAlpha1MSBarUsual())/(CB*MW*SW) + (1.125D0*CA1*EL*MH12*SA12*SA3*dAlpha1MSBarUsual()&
  &)/(CB*MW*SW) + (1.125D0*CA1*CA22*EL*MH12*SA12*SA3*dAlpha1MSBarUsual())/(CB*MW*SW) + (0.5625D0*CA1*EL*MH32*SA12*SA3*dAlpha1MSBa&
  &rUsual())/(CB*MW*SW) + (0.5625D0*CA1*CA22*EL*MH32*SA12*SA3*dAlpha1MSBarUsual())/(CB*MW*SW) + (0.125D0*CA1*EL*MH12*SA22*SA3*dAl&
  &pha1MSBarUsual())/(CB*MW*SW) + (0.0625D0*CA1*EL*MH32*SA22*SA3*dAlpha1MSBarUsual())/(CB*MW*SW) + (0.1875D0*EL*m12squared*SA1*SA&
  &22*SA3*dAlpha1MSBarUsual())/(CB*MW*SW) - (1.125D0*CA1*EL*MH12*SA12*SA22*SA3*dAlpha1MSBarUsual())/(CB*MW*SW) - (0.5625D0*CA1*EL&
  &*MH32*SA12*SA22*SA3*dAlpha1MSBarUsual())/(CB*MW*SW) + (0.1875D0*CA1*CA3*EL*MH12*SA2*dAlpha1MSBarUsual())/(MW*SB*SW) + (0.5625D&
  &0*CA1*CA22*CA3*EL*MH12*SA2*dAlpha1MSBarUsual())/(MW*SB*SW) + (0.09375D0*CA1*CA3*EL*MH32*SA2*dAlpha1MSBarUsual())/(MW*SB*SW) + &
  &(0.28125D0*CA1*CA22*CA3*EL*MH32*SA2*dAlpha1MSBarUsual())/(MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*SA1*SA2*dAlpha1MSBarUsual()&
  &)/(MW*SB*SW) - (0.84375D0*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha1MSBarUsual())/(MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA1*SA2&
  &*dAlpha1MSBarUsual())/(CB2*MW*SB*SW) + (0.84375D0*CA12*CA3*EL*m12squared*SA1*SA2*dAlpha1MSBarUsual())/(CB2*MW*SB*SW) + (0.5625&
  &D0*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha1MSBarUsual())/(CB2*MW*SB*SW) + (2.53125D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha1&
  &MSBarUsual())/(CB2*MW*SB*SW) + (0.5625D0*CA1*CA3*EL*MH12*SA12*SA2*dAlpha1MSBarUsual())/(MW*SB*SW) + (1.6875D0*CA1*CA22*CA3*EL*&
  &MH12*SA12*SA2*dAlpha1MSBarUsual())/(MW*SB*SW) + (0.28125D0*CA1*CA3*EL*MH32*SA12*SA2*dAlpha1MSBarUsual())/(MW*SB*SW) + (0.84375&
  &D0*CA1*CA22*CA3*EL*MH32*SA12*SA2*dAlpha1MSBarUsual())/(MW*SB*SW) - (0.1875D0*CA1*EL*m12squared*SA3*dAlpha1MSBarUsual())/(MW*SB&
  &*SW) - (0.1875D0*CA1*CA22*EL*m12squared*SA3*dAlpha1MSBarUsual())/(MW*SB*SW) + (0.125D0*CA1*EL*m12squared*SA3*dAlpha1MSBarUsual&
  &())/(CB2*MW*SB*SW) + (0.125D0*CA1*CA22*EL*m12squared*SA3*dAlpha1MSBarUsual())/(CB2*MW*SB*SW) - (0.125D0*EL*MH12*SA1*SA3*dAlpha&
  &1MSBarUsual())/(MW*SB*SW) + (1.125D0*CA12*EL*MH12*SA1*SA3*dAlpha1MSBarUsual())/(MW*SB*SW) - (0.125D0*CA22*EL*MH12*SA1*SA3*dAlp&
  &ha1MSBarUsual())/(MW*SB*SW) + (1.125D0*CA12*CA22*EL*MH12*SA1*SA3*dAlpha1MSBarUsual())/(MW*SB*SW) - (0.0625D0*EL*MH32*SA1*SA3*d&
  &Alpha1MSBarUsual())/(MW*SB*SW) + (0.5625D0*CA12*EL*MH32*SA1*SA3*dAlpha1MSBarUsual())/(MW*SB*SW) - (0.0625D0*CA22*EL*MH32*SA1*S&
  &A3*dAlpha1MSBarUsual())/(MW*SB*SW) + (0.5625D0*CA12*CA22*EL*MH32*SA1*SA3*dAlpha1MSBarUsual())/(MW*SB*SW) - (1.6875D0*CA1*EL*m1&
  &2squared*SA12*SA3*dAlpha1MSBarUsual())/(CB2*MW*SB*SW) - (1.6875D0*CA1*CA22*EL*m12squared*SA12*SA3*dAlpha1MSBarUsual())/(CB2*MW&
  &*SB*SW) + (0.1875D0*CA1*EL*m12squared*SA22*SA3*dAlpha1MSBarUsual())/(MW*SB*SW) - (0.125D0*CA1*EL*m12squared*SA22*SA3*dAlpha1MS&
  &BarUsual())/(CB2*MW*SB*SW) + (0.125D0*EL*MH12*SA1*SA22*SA3*dAlpha1MSBarUsual())/(MW*SB*SW) - (1.125D0*CA12*EL*MH12*SA1*SA22*SA&
  &3*dAlpha1MSBarUsual())/(MW*SB*SW) + (0.0625D0*EL*MH32*SA1*SA22*SA3*dAlpha1MSBarUsual())/(MW*SB*SW) - (0.5625D0*CA12*EL*MH32*SA&
  &1*SA22*SA3*dAlpha1MSBarUsual())/(MW*SB*SW) + (1.6875D0*CA1*EL*m12squared*SA12*SA22*SA3*dAlpha1MSBarUsual())/(CB2*MW*SB*SW) - (&
  &0.1875D0*CA1*CA3*EL*m12squared*SA2*dAlpha1MSBarUsual())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha1MSBar&
  &Usual())/(CB*MW*SB2*SW) - (0.84375D0*CA1*CA3*EL*m12squared*SA12*SA2*dAlpha1MSBarUsual())/(CB*MW*SB2*SW) - (2.53125D0*CA1*CA22*&
  &CA3*EL*m12squared*SA12*SA2*dAlpha1MSBarUsual())/(CB*MW*SB2*SW) + (0.125D0*EL*m12squared*SA1*SA3*dAlpha1MSBarUsual())/(CB*MW*SB&
  &2*SW) - (1.6875D0*CA12*EL*m12squared*SA1*SA3*dAlpha1MSBarUsual())/(CB*MW*SB2*SW) + (0.125D0*CA22*EL*m12squared*SA1*SA3*dAlpha1&
  &MSBarUsual())/(CB*MW*SB2*SW) - (1.6875D0*CA12*CA22*EL*m12squared*SA1*SA3*dAlpha1MSBarUsual())/(CB*MW*SB2*SW) - (0.125D0*EL*m12&
  &squared*SA1*SA22*SA3*dAlpha1MSBarUsual())/(CB*MW*SB2*SW) + (1.6875D0*CA12*EL*m12squared*SA1*SA22*SA3*dAlpha1MSBarUsual())/(CB*&
  &MW*SB2*SW) - (0.09375D0*CA1*CA3*EL*m12squared*SA2*dAlpha1MSBarUsual())/(MW*SB*SW*TB) - (0.28125D0*CA1*CA22*CA3*EL*m12squared*S&
  &A2*dAlpha1MSBarUsual())/(MW*SB*SW*TB) + (0.0625D0*EL*m12squared*SA1*SA3*dAlpha1MSBarUsual())/(MW*SB*SW*TB) + (0.0625D0*CA22*EL&
  &*m12squared*SA1*SA3*dAlpha1MSBarUsual())/(MW*SB*SW*TB) - (0.0625D0*EL*m12squared*SA1*SA22*SA3*dAlpha1MSBarUsual())/(MW*SB*SW*T&
  &B) + (0.09375D0*CA3*EL*m12squared*SA1*SA2*TB*dAlpha1MSBarUsual())/(CB*MW*SW) + (0.28125D0*CA22*CA3*EL*m12squared*SA1*SA2*TB*dA&
  &lpha1MSBarUsual())/(CB*MW*SW) + (0.0625D0*CA1*EL*m12squared*SA3*TB*dAlpha1MSBarUsual())/(CB*MW*SW) + (0.0625D0*CA1*CA22*EL*m12&
  &squared*SA3*TB*dAlpha1MSBarUsual())/(CB*MW*SW) - (0.0625D0*CA1*EL*m12squared*SA22*SA3*TB*dAlpha1MSBarUsual())/(CB*MW*SW) + (0.&
  &1875D0*CA1*CA2*CA3*EL*MH12*dAlpha2MSBarUsual())/(CB*MW*SW) + (0.09375D0*CA1*CA2*CA3*EL*MH32*dAlpha2MSBarUsual())/(CB*MW*SW) + &
  &(0.28125D0*CA2*CA3*EL*m12squared*SA1*dAlpha2MSBarUsual())/(CB*MW*SW) - (0.1875D0*CA1*CA2*CA3*EL*MH12*SA12*dAlpha2MSBarUsual())&
  &/(CB*MW*SW) - (0.09375D0*CA1*CA2*CA3*EL*MH32*SA12*dAlpha2MSBarUsual())/(CB*MW*SW) - (1.6875D0*CA1*CA2*CA3*EL*MH12*SA22*dAlpha2&
  &MSBarUsual())/(CB*MW*SW) - (0.84375D0*CA1*CA2*CA3*EL*MH32*SA22*dAlpha2MSBarUsual())/(CB*MW*SW) - (2.53125D0*CA2*CA3*EL*m12squa&
  &red*SA1*SA22*dAlpha2MSBarUsual())/(CB*MW*SW) + (1.6875D0*CA1*CA2*CA3*EL*MH12*SA12*SA22*dAlpha2MSBarUsual())/(CB*MW*SW) + (0.84&
  &375D0*CA1*CA2*CA3*EL*MH32*SA12*SA22*dAlpha2MSBarUsual())/(CB*MW*SW) - (0.75D0*CA1*CA2*EL*m12squared*SA2*SA3*dAlpha2MSBarUsual(&
  &))/(CB*MW*SW) + (0.5D0*CA2*EL*MH12*SA1*SA2*SA3*dAlpha2MSBarUsual())/(CB*MW*SW) + (1.5D0*CA12*CA2*EL*MH12*SA1*SA2*SA3*dAlpha2MS&
  &BarUsual())/(CB*MW*SW) + (0.25D0*CA2*EL*MH32*SA1*SA2*SA3*dAlpha2MSBarUsual())/(CB*MW*SW) + (0.75D0*CA12*CA2*EL*MH32*SA1*SA2*SA&
  &3*dAlpha2MSBarUsual())/(CB*MW*SW) + (0.28125D0*CA1*CA2*CA3*EL*m12squared*dAlpha2MSBarUsual())/(MW*SB*SW) - (0.1875D0*CA1*CA2*C&
  &A3*EL*m12squared*dAlpha2MSBarUsual())/(CB2*MW*SB*SW) + (0.1875D0*CA2*CA3*EL*MH12*SA1*dAlpha2MSBarUsual())/(MW*SB*SW) - (0.1875&
  &D0*CA12*CA2*CA3*EL*MH12*SA1*dAlpha2MSBarUsual())/(MW*SB*SW) + (0.09375D0*CA2*CA3*EL*MH32*SA1*dAlpha2MSBarUsual())/(MW*SB*SW) -&
  & (0.09375D0*CA12*CA2*CA3*EL*MH32*SA1*dAlpha2MSBarUsual())/(MW*SB*SW) + (0.28125D0*CA1*CA2*CA3*EL*m12squared*SA12*dAlpha2MSBarU&
  &sual())/(CB2*MW*SB*SW) - (2.53125D0*CA1*CA2*CA3*EL*m12squared*SA22*dAlpha2MSBarUsual())/(MW*SB*SW) + (1.6875D0*CA1*CA2*CA3*EL*&
  &m12squared*SA22*dAlpha2MSBarUsual())/(CB2*MW*SB*SW) - (1.6875D0*CA2*CA3*EL*MH12*SA1*SA22*dAlpha2MSBarUsual())/(MW*SB*SW) + (1.&
  &6875D0*CA12*CA2*CA3*EL*MH12*SA1*SA22*dAlpha2MSBarUsual())/(MW*SB*SW) - (0.84375D0*CA2*CA3*EL*MH32*SA1*SA22*dAlpha2MSBarUsual()&
  &)/(MW*SB*SW) + (0.84375D0*CA12*CA2*CA3*EL*MH32*SA1*SA22*dAlpha2MSBarUsual())/(MW*SB*SW) - (2.53125D0*CA1*CA2*CA3*EL*m12squared&
  &*SA12*SA22*dAlpha2MSBarUsual())/(CB2*MW*SB*SW) - (0.5D0*CA1*CA2*EL*MH12*SA2*SA3*dAlpha2MSBarUsual())/(MW*SB*SW) - (0.25D0*CA1*&
  &CA2*EL*MH32*SA2*SA3*dAlpha2MSBarUsual())/(MW*SB*SW) + (0.75D0*CA2*EL*m12squared*SA1*SA2*SA3*dAlpha2MSBarUsual())/(MW*SB*SW) - &
  &(0.5D0*CA2*EL*m12squared*SA1*SA2*SA3*dAlpha2MSBarUsual())/(CB2*MW*SB*SW) - (2.25D0*CA12*CA2*EL*m12squared*SA1*SA2*SA3*dAlpha2M&
  &SBarUsual())/(CB2*MW*SB*SW) - (1.5D0*CA1*CA2*EL*MH12*SA12*SA2*SA3*dAlpha2MSBarUsual())/(MW*SB*SW) - (0.75D0*CA1*CA2*EL*MH32*SA&
  &12*SA2*SA3*dAlpha2MSBarUsual())/(MW*SB*SW) - (0.1875D0*CA2*CA3*EL*m12squared*SA1*dAlpha2MSBarUsual())/(CB*MW*SB2*SW) + (0.2812&
  &5D0*CA12*CA2*CA3*EL*m12squared*SA1*dAlpha2MSBarUsual())/(CB*MW*SB2*SW) + (1.6875D0*CA2*CA3*EL*m12squared*SA1*SA22*dAlpha2MSBar&
  &Usual())/(CB*MW*SB2*SW) - (2.53125D0*CA12*CA2*CA3*EL*m12squared*SA1*SA22*dAlpha2MSBarUsual())/(CB*MW*SB2*SW) + (0.5D0*CA1*CA2*&
  &EL*m12squared*SA2*SA3*dAlpha2MSBarUsual())/(CB*MW*SB2*SW) + (2.25D0*CA1*CA2*EL*m12squared*SA12*SA2*SA3*dAlpha2MSBarUsual())/(C&
  &B*MW*SB2*SW) - (0.09375D0*CA2*CA3*EL*m12squared*SA1*dAlpha2MSBarUsual())/(MW*SB*SW*TB) + (0.84375D0*CA2*CA3*EL*m12squared*SA1*&
  &SA22*dAlpha2MSBarUsual())/(MW*SB*SW*TB) + (0.25D0*CA1*CA2*EL*m12squared*SA2*SA3*dAlpha2MSBarUsual())/(MW*SB*SW*TB) - (0.09375D&
  &0*CA1*CA2*CA3*EL*m12squared*TB*dAlpha2MSBarUsual())/(CB*MW*SW) + (0.84375D0*CA1*CA2*CA3*EL*m12squared*SA22*TB*dAlpha2MSBarUsua&
  &l())/(CB*MW*SW) - (0.25D0*CA2*EL*m12squared*SA1*SA2*SA3*TB*dAlpha2MSBarUsual())/(CB*MW*SW) + (0.5D0*CA3*MH12*SA2*dAlpha2MSBarU&
  &sual())/vS - (4.5D0*CA22*CA3*MH12*SA2*dAlpha2MSBarUsual())/vS + (0.25D0*CA3*MH32*SA2*dAlpha2MSBarUsual())/vS - (2.25D0*CA22*CA&
  &3*MH32*SA2*dAlpha2MSBarUsual())/vS + (0.1875D0*CA1*CA3*EL*m12squared*dAlpha3MSBarUsual())/(CB*MW*SW) + (0.1875D0*CA1*CA22*CA3*&
  &EL*m12squared*dAlpha3MSBarUsual())/(CB*MW*SW) - (0.125D0*CA3*EL*MH12*SA1*dAlpha3MSBarUsual())/(CB*MW*SW) - (0.375D0*CA12*CA3*E&
  &L*MH12*SA1*dAlpha3MSBarUsual())/(CB*MW*SW) - (0.125D0*CA22*CA3*EL*MH12*SA1*dAlpha3MSBarUsual())/(CB*MW*SW) - (0.375D0*CA12*CA2&
  &2*CA3*EL*MH12*SA1*dAlpha3MSBarUsual())/(CB*MW*SW) - (0.0625D0*CA3*EL*MH32*SA1*dAlpha3MSBarUsual())/(CB*MW*SW) - (0.1875D0*CA12&
  &*CA3*EL*MH32*SA1*dAlpha3MSBarUsual())/(CB*MW*SW) - (0.0625D0*CA22*CA3*EL*MH32*SA1*dAlpha3MSBarUsual())/(CB*MW*SW) - (0.1875D0*&
  &CA12*CA22*CA3*EL*MH32*SA1*dAlpha3MSBarUsual())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*m12squared*SA22*dAlpha3MSBarUsual())/(CB*MW*S&
  &W) + (0.125D0*CA3*EL*MH12*SA1*SA22*dAlpha3MSBarUsual())/(CB*MW*SW) + (0.375D0*CA12*CA3*EL*MH12*SA1*SA22*dAlpha3MSBarUsual())/(&
  &CB*MW*SW) + (0.0625D0*CA3*EL*MH32*SA1*SA22*dAlpha3MSBarUsual())/(CB*MW*SW) + (0.1875D0*CA12*CA3*EL*MH32*SA1*SA22*dAlpha3MSBarU&
  &sual())/(CB*MW*SW) - (0.1875D0*CA1*EL*MH12*SA2*SA3*dAlpha3MSBarUsual())/(CB*MW*SW) - (0.5625D0*CA1*CA22*EL*MH12*SA2*SA3*dAlpha&
  &3MSBarUsual())/(CB*MW*SW) - (0.09375D0*CA1*EL*MH32*SA2*SA3*dAlpha3MSBarUsual())/(CB*MW*SW) - (0.28125D0*CA1*CA22*EL*MH32*SA2*S&
  &A3*dAlpha3MSBarUsual())/(CB*MW*SW) - (0.28125D0*EL*m12squared*SA1*SA2*SA3*dAlpha3MSBarUsual())/(CB*MW*SW) - (0.84375D0*CA22*EL&
  &*m12squared*SA1*SA2*SA3*dAlpha3MSBarUsual())/(CB*MW*SW) + (0.1875D0*CA1*EL*MH12*SA12*SA2*SA3*dAlpha3MSBarUsual())/(CB*MW*SW) +&
  & (0.5625D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dAlpha3MSBarUsual())/(CB*MW*SW) + (0.09375D0*CA1*EL*MH32*SA12*SA2*SA3*dAlpha3MSBarUsu&
  &al())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*MH32*SA12*SA2*SA3*dAlpha3MSBarUsual())/(CB*MW*SW) + (0.125D0*CA1*CA3*EL*MH12*dAlpha3&
  &MSBarUsual())/(MW*SB*SW) + (0.125D0*CA1*CA22*CA3*EL*MH12*dAlpha3MSBarUsual())/(MW*SB*SW) + (0.0625D0*CA1*CA3*EL*MH32*dAlpha3MS&
  &BarUsual())/(MW*SB*SW) + (0.0625D0*CA1*CA22*CA3*EL*MH32*dAlpha3MSBarUsual())/(MW*SB*SW) - (0.1875D0*CA3*EL*m12squared*SA1*dAlp&
  &ha3MSBarUsual())/(MW*SB*SW) - (0.1875D0*CA22*CA3*EL*m12squared*SA1*dAlpha3MSBarUsual())/(MW*SB*SW) + (0.125D0*CA3*EL*m12square&
  &d*SA1*dAlpha3MSBarUsual())/(CB2*MW*SB*SW) + (0.5625D0*CA12*CA3*EL*m12squared*SA1*dAlpha3MSBarUsual())/(CB2*MW*SB*SW) + (0.125D&
  &0*CA22*CA3*EL*m12squared*SA1*dAlpha3MSBarUsual())/(CB2*MW*SB*SW) + (0.5625D0*CA12*CA22*CA3*EL*m12squared*SA1*dAlpha3MSBarUsual&
  &())/(CB2*MW*SB*SW) + (0.375D0*CA1*CA3*EL*MH12*SA12*dAlpha3MSBarUsual())/(MW*SB*SW) + (0.375D0*CA1*CA22*CA3*EL*MH12*SA12*dAlpha&
  &3MSBarUsual())/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*MH32*SA12*dAlpha3MSBarUsual())/(MW*SB*SW) + (0.1875D0*CA1*CA22*CA3*EL*MH32*SA&
  &12*dAlpha3MSBarUsual())/(MW*SB*SW) - (0.125D0*CA1*CA3*EL*MH12*SA22*dAlpha3MSBarUsual())/(MW*SB*SW) - (0.0625D0*CA1*CA3*EL*MH32&
  &*SA22*dAlpha3MSBarUsual())/(MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA1*SA22*dAlpha3MSBarUsual())/(MW*SB*SW) - (0.125D0*CA3*EL&
  &*m12squared*SA1*SA22*dAlpha3MSBarUsual())/(CB2*MW*SB*SW) - (0.5625D0*CA12*CA3*EL*m12squared*SA1*SA22*dAlpha3MSBarUsual())/(CB2&
  &*MW*SB*SW) - (0.375D0*CA1*CA3*EL*MH12*SA12*SA22*dAlpha3MSBarUsual())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*MH32*SA12*SA22*dAlpha3M&
  &SBarUsual())/(MW*SB*SW) - (0.28125D0*CA1*EL*m12squared*SA2*SA3*dAlpha3MSBarUsual())/(MW*SB*SW) - (0.84375D0*CA1*CA22*EL*m12squ&
  &ared*SA2*SA3*dAlpha3MSBarUsual())/(MW*SB*SW) + (0.1875D0*CA1*EL*m12squared*SA2*SA3*dAlpha3MSBarUsual())/(CB2*MW*SB*SW) + (0.56&
  &25D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha3MSBarUsual())/(CB2*MW*SB*SW) - (0.1875D0*EL*MH12*SA1*SA2*SA3*dAlpha3MSBarUsual())/(&
  &MW*SB*SW) + (0.1875D0*CA12*EL*MH12*SA1*SA2*SA3*dAlpha3MSBarUsual())/(MW*SB*SW) - (0.5625D0*CA22*EL*MH12*SA1*SA2*SA3*dAlpha3MSB&
  &arUsual())/(MW*SB*SW) + (0.5625D0*CA12*CA22*EL*MH12*SA1*SA2*SA3*dAlpha3MSBarUsual())/(MW*SB*SW) - (0.09375D0*EL*MH32*SA1*SA2*S&
  &A3*dAlpha3MSBarUsual())/(MW*SB*SW) + (0.09375D0*CA12*EL*MH32*SA1*SA2*SA3*dAlpha3MSBarUsual())/(MW*SB*SW) - (0.28125D0*CA22*EL*&
  &MH32*SA1*SA2*SA3*dAlpha3MSBarUsual())/(MW*SB*SW) + (0.28125D0*CA12*CA22*EL*MH32*SA1*SA2*SA3*dAlpha3MSBarUsual())/(MW*SB*SW) - &
  &(0.28125D0*CA1*EL*m12squared*SA12*SA2*SA3*dAlpha3MSBarUsual())/(CB2*MW*SB*SW) - (0.84375D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3&
  &*dAlpha3MSBarUsual())/(CB2*MW*SB*SW) - (0.125D0*CA1*CA3*EL*m12squared*dAlpha3MSBarUsual())/(CB*MW*SB2*SW) - (0.125D0*CA1*CA22*&
  &CA3*EL*m12squared*dAlpha3MSBarUsual())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA3*EL*m12squared*SA12*dAlpha3MSBarUsual())/(CB*MW*SB2*S&
  &W) - (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA12*dAlpha3MSBarUsual())/(CB*MW*SB2*SW) + (0.125D0*CA1*CA3*EL*m12squared*SA22*dAlph&
  &a3MSBarUsual())/(CB*MW*SB2*SW) + (0.5625D0*CA1*CA3*EL*m12squared*SA12*SA22*dAlpha3MSBarUsual())/(CB*MW*SB2*SW) + (0.1875D0*EL*&
  &m12squared*SA1*SA2*SA3*dAlpha3MSBarUsual())/(CB*MW*SB2*SW) - (0.28125D0*CA12*EL*m12squared*SA1*SA2*SA3*dAlpha3MSBarUsual())/(C&
  &B*MW*SB2*SW) + (0.5625D0*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha3MSBarUsual())/(CB*MW*SB2*SW) - (0.84375D0*CA12*CA22*EL*m12squar&
  &ed*SA1*SA2*SA3*dAlpha3MSBarUsual())/(CB*MW*SB2*SW) - (0.0625D0*CA1*CA3*EL*m12squared*dAlpha3MSBarUsual())/(MW*SB*SW*TB) - (0.0&
  &625D0*CA1*CA22*CA3*EL*m12squared*dAlpha3MSBarUsual())/(MW*SB*SW*TB) + (0.0625D0*CA1*CA3*EL*m12squared*SA22*dAlpha3MSBarUsual()&
  &)/(MW*SB*SW*TB) + (0.09375D0*EL*m12squared*SA1*SA2*SA3*dAlpha3MSBarUsual())/(MW*SB*SW*TB) + (0.28125D0*CA22*EL*m12squared*SA1*&
  &SA2*SA3*dAlpha3MSBarUsual())/(MW*SB*SW*TB) + (0.0625D0*CA3*EL*m12squared*SA1*TB*dAlpha3MSBarUsual())/(CB*MW*SW) + (0.0625D0*CA&
  &22*CA3*EL*m12squared*SA1*TB*dAlpha3MSBarUsual())/(CB*MW*SW) - (0.0625D0*CA3*EL*m12squared*SA1*SA22*TB*dAlpha3MSBarUsual())/(CB&
  &*MW*SW) + (0.09375D0*CA1*EL*m12squared*SA2*SA3*TB*dAlpha3MSBarUsual())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*m12squared*SA2*SA3*&
  &TB*dAlpha3MSBarUsual())/(CB*MW*SW) + (0.5D0*CA2*MH12*SA3*dAlpha3MSBarUsual())/vS + (0.25D0*CA2*MH32*SA3*dAlpha3MSBarUsual())/v&
  &S + (1.5D0*CA2*MH12*SA22*SA3*dAlpha3MSBarUsual())/vS + (0.75D0*CA2*MH32*SA22*SA3*dAlpha3MSBarUsual())/vS + (0.09375D0*CA3*EL*M&
  &H12*SA1*SA2*dBetaMSBarUsual())/(CB*MW*SW) - (0.09375D0*CA12*CA3*EL*MH12*SA1*SA2*dBetaMSBarUsual())/(CB*MW*SW) + (0.28125D0*CA2&
  &2*CA3*EL*MH12*SA1*SA2*dBetaMSBarUsual())/(CB*MW*SW) - (0.28125D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dBetaMSBarUsual())/(CB*MW*SW) +&
  & (0.046875D0*CA3*EL*MH32*SA1*SA2*dBetaMSBarUsual())/(CB*MW*SW) - (0.046875D0*CA12*CA3*EL*MH32*SA1*SA2*dBetaMSBarUsual())/(CB*M&
  &W*SW) + (0.140625D0*CA22*CA3*EL*MH32*SA1*SA2*dBetaMSBarUsual())/(CB*MW*SW) - (0.140625D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*dBetaMS&
  &BarUsual())/(CB*MW*SW) + (0.0625D0*CA1*EL*MH12*SA3*dBetaMSBarUsual())/(CB*MW*SW) + (0.0625D0*CA1*CA22*EL*MH12*SA3*dBetaMSBarUs&
  &ual())/(CB*MW*SW) + (0.03125D0*CA1*EL*MH32*SA3*dBetaMSBarUsual())/(CB*MW*SW) + (0.03125D0*CA1*CA22*EL*MH32*SA3*dBetaMSBarUsual&
  &())/(CB*MW*SW) + (0.1875D0*CA1*EL*MH12*SA12*SA3*dBetaMSBarUsual())/(CB*MW*SW) + (0.1875D0*CA1*CA22*EL*MH12*SA12*SA3*dBetaMSBar&
  &Usual())/(CB*MW*SW) + (0.09375D0*CA1*EL*MH32*SA12*SA3*dBetaMSBarUsual())/(CB*MW*SW) + (0.09375D0*CA1*CA22*EL*MH32*SA12*SA3*dBe&
  &taMSBarUsual())/(CB*MW*SW) - (0.0625D0*CA1*EL*MH12*SA22*SA3*dBetaMSBarUsual())/(CB*MW*SW) - (0.03125D0*CA1*EL*MH32*SA22*SA3*dB&
  &etaMSBarUsual())/(CB*MW*SW) - (0.1875D0*CA1*EL*MH12*SA12*SA22*SA3*dBetaMSBarUsual())/(CB*MW*SW) - (0.09375D0*CA1*EL*MH32*SA12*&
  &SA22*SA3*dBetaMSBarUsual())/(CB*MW*SW) + (0.09375D0*CA3*EL*m12squared*SA1*SA2*dBetaMSBarUsual())/(MW*SB*SW) + (0.28125D0*CA22*&
  &CA3*EL*m12squared*SA1*SA2*dBetaMSBarUsual())/(MW*SB*SW) - (0.328125D0*CA3*EL*m12squared*SA1*SA2*dBetaMSBarUsual())/(CB2*MW*SB*&
  &SW) + (0.421875D0*CA12*CA3*EL*m12squared*SA1*SA2*dBetaMSBarUsual())/(CB2*MW*SB*SW) - (0.984375D0*CA22*CA3*EL*m12squared*SA1*SA&
  &2*dBetaMSBarUsual())/(CB2*MW*SB*SW) + (1.265625D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*dBetaMSBarUsual())/(CB2*MW*SB*SW) + (0.0&
  &625D0*CA1*EL*m12squared*SA3*dBetaMSBarUsual())/(MW*SB*SW) + (0.0625D0*CA1*CA22*EL*m12squared*SA3*dBetaMSBarUsual())/(MW*SB*SW)&
  & - (0.21875D0*CA1*EL*m12squared*SA3*dBetaMSBarUsual())/(CB2*MW*SB*SW) - (0.21875D0*CA1*CA22*EL*m12squared*SA3*dBetaMSBarUsual(&
  &))/(CB2*MW*SB*SW) - (0.84375D0*CA1*EL*m12squared*SA12*SA3*dBetaMSBarUsual())/(CB2*MW*SB*SW) - (0.84375D0*CA1*CA22*EL*m12square&
  &d*SA12*SA3*dBetaMSBarUsual())/(CB2*MW*SB*SW) - (0.0625D0*CA1*EL*m12squared*SA22*SA3*dBetaMSBarUsual())/(MW*SB*SW) + (0.21875D0&
  &*CA1*EL*m12squared*SA22*SA3*dBetaMSBarUsual())/(CB2*MW*SB*SW) + (0.84375D0*CA1*EL*m12squared*SA12*SA22*SA3*dBetaMSBarUsual())/&
  &(CB2*MW*SB*SW) + (0.09375D0*CA1*CA3*EL*m12squared*SA2*dBetaMSBarUsual())/(CB*MW*SB2*SW) + (0.28125D0*CA1*CA22*CA3*EL*m12square&
  &d*SA2*dBetaMSBarUsual())/(CB*MW*SB2*SW) - (0.09375D0*CA3*EL*MH12*SA1*SA2*dBetaMSBarUsual())/(CB*MW*SB2*SW) + (0.09375D0*CA12*C&
  &A3*EL*MH12*SA1*SA2*dBetaMSBarUsual())/(CB*MW*SB2*SW) - (0.28125D0*CA22*CA3*EL*MH12*SA1*SA2*dBetaMSBarUsual())/(CB*MW*SB2*SW) +&
  & (0.28125D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dBetaMSBarUsual())/(CB*MW*SB2*SW) - (0.046875D0*CA3*EL*MH32*SA1*SA2*dBetaMSBarUsual(&
  &))/(CB*MW*SB2*SW) + (0.046875D0*CA12*CA3*EL*MH32*SA1*SA2*dBetaMSBarUsual())/(CB*MW*SB2*SW) - (0.140625D0*CA22*CA3*EL*MH32*SA1*&
  &SA2*dBetaMSBarUsual())/(CB*MW*SB2*SW) + (0.140625D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*dBetaMSBarUsual())/(CB*MW*SB2*SW) - (0.28125&
  &D0*CA1*CA3*EL*m12squared*SA12*SA2*dBetaMSBarUsual())/(CB*MW*SB2*SW) - (0.84375D0*CA1*CA22*CA3*EL*m12squared*SA12*SA2*dBetaMSBa&
  &rUsual())/(CB*MW*SB2*SW) - (0.0625D0*CA1*EL*MH12*SA3*dBetaMSBarUsual())/(CB*MW*SB2*SW) - (0.0625D0*CA1*CA22*EL*MH12*SA3*dBetaM&
  &SBarUsual())/(CB*MW*SB2*SW) - (0.03125D0*CA1*EL*MH32*SA3*dBetaMSBarUsual())/(CB*MW*SB2*SW) - (0.03125D0*CA1*CA22*EL*MH32*SA3*d&
  &BetaMSBarUsual())/(CB*MW*SB2*SW) - (0.0625D0*EL*m12squared*SA1*SA3*dBetaMSBarUsual())/(CB*MW*SB2*SW) - (0.5625D0*CA12*EL*m12sq&
  &uared*SA1*SA3*dBetaMSBarUsual())/(CB*MW*SB2*SW) - (0.0625D0*CA22*EL*m12squared*SA1*SA3*dBetaMSBarUsual())/(CB*MW*SB2*SW) - (0.&
  &5625D0*CA12*CA22*EL*m12squared*SA1*SA3*dBetaMSBarUsual())/(CB*MW*SB2*SW) - (0.1875D0*CA1*EL*MH12*SA12*SA3*dBetaMSBarUsual())/(&
  &CB*MW*SB2*SW) - (0.1875D0*CA1*CA22*EL*MH12*SA12*SA3*dBetaMSBarUsual())/(CB*MW*SB2*SW) - (0.09375D0*CA1*EL*MH32*SA12*SA3*dBetaM&
  &SBarUsual())/(CB*MW*SB2*SW) - (0.09375D0*CA1*CA22*EL*MH32*SA12*SA3*dBetaMSBarUsual())/(CB*MW*SB2*SW) + (0.0625D0*CA1*EL*MH12*S&
  &A22*SA3*dBetaMSBarUsual())/(CB*MW*SB2*SW) + (0.03125D0*CA1*EL*MH32*SA22*SA3*dBetaMSBarUsual())/(CB*MW*SB2*SW) + (0.0625D0*EL*m&
  &12squared*SA1*SA22*SA3*dBetaMSBarUsual())/(CB*MW*SB2*SW) + (0.5625D0*CA12*EL*m12squared*SA1*SA22*SA3*dBetaMSBarUsual())/(CB*MW&
  &*SB2*SW) + (0.1875D0*CA1*EL*MH12*SA12*SA22*SA3*dBetaMSBarUsual())/(CB*MW*SB2*SW) + (0.09375D0*CA1*EL*MH32*SA12*SA22*SA3*dBetaM&
  &SBarUsual())/(CB*MW*SB2*SW) - (0.1875D0*CA1*CA3*EL*m12squared*SA2*dBetaMSBarUsual())/(MW*SB*SW*TB) - (0.5625D0*CA1*CA22*CA3*EL&
  &*m12squared*SA2*dBetaMSBarUsual())/(MW*SB*SW*TB) - (0.09375D0*CA3*EL*MH12*SA1*SA2*dBetaMSBarUsual())/(MW*SB*SW*TB) + (0.09375D&
  &0*CA12*CA3*EL*MH12*SA1*SA2*dBetaMSBarUsual())/(MW*SB*SW*TB) - (0.28125D0*CA22*CA3*EL*MH12*SA1*SA2*dBetaMSBarUsual())/(MW*SB*SW&
  &*TB) + (0.28125D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dBetaMSBarUsual())/(MW*SB*SW*TB) - (0.046875D0*CA3*EL*MH32*SA1*SA2*dBetaMSBarU&
  &sual())/(MW*SB*SW*TB) + (0.046875D0*CA12*CA3*EL*MH32*SA1*SA2*dBetaMSBarUsual())/(MW*SB*SW*TB) - (0.140625D0*CA22*CA3*EL*MH32*S&
  &A1*SA2*dBetaMSBarUsual())/(MW*SB*SW*TB) + (0.140625D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*dBetaMSBarUsual())/(MW*SB*SW*TB) - (0.0625&
  &D0*CA1*EL*MH12*SA3*dBetaMSBarUsual())/(MW*SB*SW*TB) - (0.0625D0*CA1*CA22*EL*MH12*SA3*dBetaMSBarUsual())/(MW*SB*SW*TB) - (0.031&
  &25D0*CA1*EL*MH32*SA3*dBetaMSBarUsual())/(MW*SB*SW*TB) - (0.03125D0*CA1*CA22*EL*MH32*SA3*dBetaMSBarUsual())/(MW*SB*SW*TB) + (0.&
  &125D0*EL*m12squared*SA1*SA3*dBetaMSBarUsual())/(MW*SB*SW*TB) + (0.125D0*CA22*EL*m12squared*SA1*SA3*dBetaMSBarUsual())/(MW*SB*S&
  &W*TB) - (0.1875D0*CA1*EL*MH12*SA12*SA3*dBetaMSBarUsual())/(MW*SB*SW*TB) - (0.1875D0*CA1*CA22*EL*MH12*SA12*SA3*dBetaMSBarUsual(&
  &))/(MW*SB*SW*TB) - (0.09375D0*CA1*EL*MH32*SA12*SA3*dBetaMSBarUsual())/(MW*SB*SW*TB) - (0.09375D0*CA1*CA22*EL*MH32*SA12*SA3*dBe&
  &taMSBarUsual())/(MW*SB*SW*TB) + (0.0625D0*CA1*EL*MH12*SA22*SA3*dBetaMSBarUsual())/(MW*SB*SW*TB) + (0.03125D0*CA1*EL*MH32*SA22*&
  &SA3*dBetaMSBarUsual())/(MW*SB*SW*TB) - (0.125D0*EL*m12squared*SA1*SA22*SA3*dBetaMSBarUsual())/(MW*SB*SW*TB) + (0.1875D0*CA1*EL&
  &*MH12*SA12*SA22*SA3*dBetaMSBarUsual())/(MW*SB*SW*TB) + (0.09375D0*CA1*EL*MH32*SA12*SA22*SA3*dBetaMSBarUsual())/(MW*SB*SW*TB) +&
  & (0.1875D0*CA1*CA3*EL*MH12*SA2*TB*dBetaMSBarUsual())/(CB*MW*SW) + (0.5625D0*CA1*CA22*CA3*EL*MH12*SA2*TB*dBetaMSBarUsual())/(CB&
  &*MW*SW) + (0.09375D0*CA1*CA3*EL*MH32*SA2*TB*dBetaMSBarUsual())/(CB*MW*SW) + (0.28125D0*CA1*CA22*CA3*EL*MH32*SA2*TB*dBetaMSBarU&
  &sual())/(CB*MW*SW) + (0.328125D0*CA3*EL*m12squared*SA1*SA2*TB*dBetaMSBarUsual())/(CB*MW*SW) + (0.984375D0*CA22*CA3*EL*m12squar&
  &ed*SA1*SA2*TB*dBetaMSBarUsual())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*MH12*SA12*SA2*TB*dBetaMSBarUsual())/(CB*MW*SW) - (0.5625D0*&
  &CA1*CA22*CA3*EL*MH12*SA12*SA2*TB*dBetaMSBarUsual())/(CB*MW*SW) - (0.09375D0*CA1*CA3*EL*MH32*SA12*SA2*TB*dBetaMSBarUsual())/(CB&
  &*MW*SW) - (0.28125D0*CA1*CA22*CA3*EL*MH32*SA12*SA2*TB*dBetaMSBarUsual())/(CB*MW*SW) + (0.21875D0*CA1*EL*m12squared*SA3*TB*dBet&
  &aMSBarUsual())/(CB*MW*SW) + (0.21875D0*CA1*CA22*EL*m12squared*SA3*TB*dBetaMSBarUsual())/(CB*MW*SW) - (0.125D0*EL*MH12*SA1*SA3*&
  &TB*dBetaMSBarUsual())/(CB*MW*SW) - (0.375D0*CA12*EL*MH12*SA1*SA3*TB*dBetaMSBarUsual())/(CB*MW*SW) - (0.125D0*CA22*EL*MH12*SA1*&
  &SA3*TB*dBetaMSBarUsual())/(CB*MW*SW) - (0.375D0*CA12*CA22*EL*MH12*SA1*SA3*TB*dBetaMSBarUsual())/(CB*MW*SW) - (0.0625D0*EL*MH32&
  &*SA1*SA3*TB*dBetaMSBarUsual())/(CB*MW*SW) - (0.1875D0*CA12*EL*MH32*SA1*SA3*TB*dBetaMSBarUsual())/(CB*MW*SW) - (0.0625D0*CA22*E&
  &L*MH32*SA1*SA3*TB*dBetaMSBarUsual())/(CB*MW*SW) - (0.1875D0*CA12*CA22*EL*MH32*SA1*SA3*TB*dBetaMSBarUsual())/(CB*MW*SW) - (0.21&
  &875D0*CA1*EL*m12squared*SA22*SA3*TB*dBetaMSBarUsual())/(CB*MW*SW) + (0.125D0*EL*MH12*SA1*SA22*SA3*TB*dBetaMSBarUsual())/(CB*MW&
  &*SW) + (0.375D0*CA12*EL*MH12*SA1*SA22*SA3*TB*dBetaMSBarUsual())/(CB*MW*SW) + (0.0625D0*EL*MH32*SA1*SA22*SA3*TB*dBetaMSBarUsual&
  &())/(CB*MW*SW) + (0.1875D0*CA12*EL*MH32*SA1*SA22*SA3*TB*dBetaMSBarUsual())/(CB*MW*SW) + (0.140625D0*CA3*EL*m12squared*SA1*SA2*&
  &dBetaMSBarUsual())/(MW*SB*SW*TB2) + (0.421875D0*CA22*CA3*EL*m12squared*SA1*SA2*dBetaMSBarUsual())/(MW*SB*SW*TB2) + (0.09375D0*&
  &CA1*EL*m12squared*SA3*dBetaMSBarUsual())/(MW*SB*SW*TB2) + (0.09375D0*CA1*CA22*EL*m12squared*SA3*dBetaMSBarUsual())/(MW*SB*SW*T&
  &B2) - (0.09375D0*CA1*EL*m12squared*SA22*SA3*dBetaMSBarUsual())/(MW*SB*SW*TB2) - (0.1875D0*CA1*CA3*EL*m12squared*SA2*TB2*dBetaM&
  &SBarUsual())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA2*TB2*dBetaMSBarUsual())/(CB*MW*SW) + (0.125D0*EL*m12squared*&
  &SA1*SA3*TB2*dBetaMSBarUsual())/(CB*MW*SW) + (0.125D0*CA22*EL*m12squared*SA1*SA3*TB2*dBetaMSBarUsual())/(CB*MW*SW) - (0.125D0*E&
  &L*m12squared*SA1*SA22*SA3*TB2*dBetaMSBarUsual())/(CB*MW*SW) + (0.125D0*CA2*CA3*MH12*dBetaMSBarUsual())/(CB*SB*vS) + (0.0625D0*&
  &CA2*CA3*MH32*dBetaMSBarUsual())/(CB*SB*vS) + (0.375D0*CA2*CA3*MH12*SA22*dBetaMSBarUsual())/(CB*SB*vS) + (0.1875D0*CA2*CA3*MH32&
  &*SA22*dBetaMSBarUsual())/(CB*SB*vS) - (0.125D0*CA2*CA3*MH12*dBetaMSBarUsual())/(TB*vS) - (0.0625D0*CA2*CA3*MH32*dBetaMSBarUsua&
  &l())/(TB*vS) - (0.375D0*CA2*CA3*MH12*SA22*dBetaMSBarUsual())/(TB*vS) - (0.1875D0*CA2*CA3*MH32*SA22*dBetaMSBarUsual())/(TB*vS) &
  &- (0.125D0*CA2*CA3*MH12*TB*dBetaMSBarUsual())/vS - (0.0625D0*CA2*CA3*MH32*TB*dBetaMSBarUsual())/vS - (0.375D0*CA2*CA3*MH12*SA2&
  &2*TB*dBetaMSBarUsual())/vS - (0.1875D0*CA2*CA3*MH32*SA22*TB*dBetaMSBarUsual())/vS - (0.375D0*EL*MH12*SA3*dAlpha1MSBarUsual()*D&
  &BLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.375D0*CA22*EL*MH12*SA3*dAlpha1MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*E&
  &L*MH32*SA3*dAlpha1MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA22*EL*MH32*SA3*dAlpha1MSBarUsual()*DBLE(CA1**INT&
  &(3.D0)))/(CB*MW*SW) + (0.375D0*EL*MH12*SA22*SA3*dAlpha1MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*EL*MH32*SA22*&
  &SA3*dAlpha1MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA3*EL*MH12*SA2*dAlpha1MSBarUsual()*DBLE(CA1**INT(3.D0)))&
  &/(MW*SB*SW) - (0.5625D0*CA22*CA3*EL*MH12*SA2*dAlpha1MSBarUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA3*EL*MH32*SA2&
  &*dAlpha1MSBarUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA22*CA3*EL*MH32*SA2*dAlpha1MSBarUsual()*DBLE(CA1**INT(3.D0&
  &)))/(MW*SB*SW) + (0.5625D0*EL*m12squared*SA3*dAlpha1MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.5625D0*CA22*EL*m12s&
  &quared*SA3*dAlpha1MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*EL*m12squared*SA22*SA3*dAlpha1MSBarUsual()*DBL&
  &E(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.28125D0*CA3*EL*m12squared*SA2*dAlpha1MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) &
  &+ (0.84375D0*CA22*CA3*EL*m12squared*SA2*dAlpha1MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*CA2*CA3*EL*MH12*d&
  &Alpha2MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA2*CA3*EL*MH32*dAlpha2MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB&
  &*MW*SW) - (0.5625D0*CA2*CA3*EL*MH12*SA22*dAlpha2MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA2*CA3*EL*MH32*SA2&
  &2*dAlpha2MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA2*CA3*EL*m12squared*dAlpha2MSBarUsual()*DBLE(CA1**INT(3.&
  &D0)))/(CB2*MW*SB*SW) + (0.84375D0*CA2*CA3*EL*m12squared*SA22*dAlpha2MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.5D0&
  &*CA2*EL*MH12*SA2*SA3*dAlpha2MSBarUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.25D0*CA2*EL*MH32*SA2*SA3*dAlpha2MSBarUsual()*DB&
  &LE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.75D0*CA2*EL*m12squared*SA2*SA3*dAlpha2MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - &
  &(0.0625D0*EL*MH12*SA2*SA3*dAlpha3MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA22*EL*MH12*SA2*SA3*dAlpha3MSBarUs&
  &ual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*EL*MH32*SA2*SA3*dAlpha3MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.&
  &09375D0*CA22*EL*MH32*SA2*SA3*dAlpha3MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.125D0*CA3*EL*MH12*dAlpha3MSBarUsual()*D&
  &BLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.125D0*CA22*CA3*EL*MH12*dAlpha3MSBarUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.0625D0*C&
  &A3*EL*MH32*dAlpha3MSBarUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.0625D0*CA22*CA3*EL*MH32*dAlpha3MSBarUsual()*DBLE(CA1**INT&
  &(3.D0)))/(MW*SB*SW) + (0.125D0*CA3*EL*MH12*SA22*dAlpha3MSBarUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*CA3*EL*MH32*S&
  &A22*dAlpha3MSBarUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*EL*m12squared*SA2*SA3*dAlpha3MSBarUsual()*DBLE(CA1**INT(&
  &3.D0)))/(CB2*MW*SB*SW) + (0.28125D0*CA22*EL*m12squared*SA2*SA3*dAlpha3MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1&
  &875D0*CA3*EL*m12squared*dAlpha3MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*CA22*CA3*EL*m12squared*dAlpha3MSB&
  &arUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.1875D0*CA3*EL*m12squared*SA22*dAlpha3MSBarUsual()*DBLE(CA1**INT(3.D0)))/(C&
  &B*MW*SB2*SW) - (0.0625D0*EL*MH12*SA3*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.0625D0*CA22*EL*MH12*SA3*dBetaMSBa&
  &rUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*EL*MH32*SA3*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.031&
  &25D0*CA22*EL*MH32*SA3*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*EL*MH12*SA22*SA3*dBetaMSBarUsual()*DBLE(C&
  &A1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*EL*MH32*SA22*SA3*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*EL*m1&
  &2squared*SA3*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.28125D0*CA22*EL*m12squared*SA3*dBetaMSBarUsual()*DBLE&
  &(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*EL*m12squared*SA22*SA3*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + &
  &(0.09375D0*CA3*EL*m12squared*SA2*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA22*CA3*EL*m12squared*SA&
  &2*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*EL*MH12*SA3*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*M&
  &W*SB2*SW) + (0.0625D0*CA22*EL*MH12*SA3*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.03125D0*EL*MH32*SA3*dBetaMS&
  &BarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.03125D0*CA22*EL*MH32*SA3*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*S&
  &B2*SW) - (0.0625D0*EL*MH12*SA22*SA3*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.03125D0*EL*MH32*SA22*SA3*dBeta&
  &MSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*EL*MH12*SA3*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB&
  &) + (0.0625D0*CA22*EL*MH12*SA3*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.03125D0*EL*MH32*SA3*dBetaMSBarUsual(&
  &)*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.03125D0*CA22*EL*MH32*SA3*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) - (0&
  &.0625D0*EL*MH12*SA22*SA3*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.03125D0*EL*MH32*SA22*SA3*dBetaMSBarUsual()&
  &*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.0625D0*CA3*EL*MH12*SA2*TB*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.18&
  &75D0*CA22*CA3*EL*MH12*SA2*TB*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA3*EL*MH32*SA2*TB*dBetaMSBarUsua&
  &l()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA22*CA3*EL*MH32*SA2*TB*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) +&
  & (0.5625D0*CA1*CA3*EL*MH12*dAlpha2MSBarUsual()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA1*CA3*EL*MH32*dAlpha2MSBarUsual&
  &()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.84375D0*CA3*EL*m12squared*SA1*dAlpha2MSBarUsual()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - &
  &(0.5625D0*CA1*CA3*EL*MH12*SA12*dAlpha2MSBarUsual()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA1*CA3*EL*MH32*SA12*dAlpha2M&
  &SBarUsual()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.84375D0*CA1*CA3*EL*m12squared*dAlpha2MSBarUsual()*DBLE(CA2**INT(3.D0)))/(MW*&
  &SB*SW) - (0.5625D0*CA1*CA3*EL*m12squared*dAlpha2MSBarUsual()*DBLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.5625D0*CA3*EL*MH12*SA1*&
  &dAlpha2MSBarUsual()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA12*CA3*EL*MH12*SA1*dAlpha2MSBarUsual()*DBLE(CA2**INT(3.D0))&
  &)/(MW*SB*SW) + (0.28125D0*CA3*EL*MH32*SA1*dAlpha2MSBarUsual()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA12*CA3*EL*MH32*S&
  &A1*dAlpha2MSBarUsual()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) + (0.84375D0*CA1*CA3*EL*m12squared*SA12*dAlpha2MSBarUsual()*DBLE(CA2**&
  &INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA3*EL*m12squared*SA1*dAlpha2MSBarUsual()*DBLE(CA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.843&
  &75D0*CA12*CA3*EL*m12squared*SA1*dAlpha2MSBarUsual()*DBLE(CA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125D0*CA3*EL*m12squared*SA1*dA&
  &lpha2MSBarUsual()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.28125D0*CA1*CA3*EL*m12squared*TB*dAlpha2MSBarUsual()*DBLE(CA2**INT(&
  &3.D0)))/(CB*MW*SW) - (0.5D0*MH12*SA3*dAlpha3MSBarUsual()*DBLE(CA2**INT(3.D0)))/vS - (0.25D0*MH32*SA3*dAlpha3MSBarUsual()*DBLE(&
  &CA2**INT(3.D0)))/vS - (0.125D0*CA3*MH12*dBetaMSBarUsual()*DBLE(CA2**INT(3.D0)))/(CB*SB*vS) - (0.0625D0*CA3*MH32*dBetaMSBarUsua&
  &l()*DBLE(CA2**INT(3.D0)))/(CB*SB*vS) + (0.125D0*CA3*MH12*dBetaMSBarUsual()*DBLE(CA2**INT(3.D0)))/(TB*vS) + (0.0625D0*CA3*MH32*&
  &dBetaMSBarUsual()*DBLE(CA2**INT(3.D0)))/(TB*vS) + (0.125D0*CA3*MH12*TB*dBetaMSBarUsual()*DBLE(CA2**INT(3.D0)))/vS + (0.0625D0*&
  &CA3*MH32*TB*dBetaMSBarUsual()*DBLE(CA2**INT(3.D0)))/vS + (0.1875D0*CA3*EL*MH12*dAlpha2MSBarUsual()*DBLE(CA1**INT(3.D0))*DBLE(C&
  &A2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA3*EL*MH32*dAlpha2MSBarUsual()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) &
  &- (0.28125D0*CA3*EL*m12squared*dAlpha2MSBarUsual()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.375D0*CA1*CA&
  &3*EL*m12squared*SA2*dBetaMSBarUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (1.125D0*CA1*CA22*CA3*EL*m12squared*SA2*dBetaMSBarUsual(&
  &)*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.5625D0*CA1*CA3*EL*m12squared*SA12*SA2*dBetaMSBarUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (&
  &1.6875D0*CA1*CA22*CA3*EL*m12squared*SA12*SA2*dBetaMSBarUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.25D0*EL*m12squared*SA1*SA3*d&
  &BetaMSBarUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (1.125D0*CA12*EL*m12squared*SA1*SA3*dBetaMSBarUsual()*DBLE(CB**INT(-3.D0)))/(&
  &MW*SW) + (0.25D0*CA22*EL*m12squared*SA1*SA3*dBetaMSBarUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (1.125D0*CA12*CA22*EL*m12squared&
  &*SA1*SA3*dBetaMSBarUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.25D0*EL*m12squared*SA1*SA22*SA3*dBetaMSBarUsual()*DBLE(CB**INT(-&
  &3.D0)))/(MW*SW) - (1.125D0*CA12*EL*m12squared*SA1*SA22*SA3*dBetaMSBarUsual()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.1875D0*CA3*EL*&
  &m12squared*SA2*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.5625D0*CA22*CA3*EL*m12squared*SA2*dBe&
  &taMSBarUsual()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.1875D0*CA3*EL*MH12*SA2*dAlpha1MSBarUsual()*DBLE(SA1**IN&
  &T(3.D0)))/(CB*MW*SW) + (0.5625D0*CA22*CA3*EL*MH12*SA2*dAlpha1MSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA3*EL&
  &*MH32*SA2*dAlpha1MSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA22*CA3*EL*MH32*SA2*dAlpha1MSBarUsual()*DBLE(SA1*&
  &*INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA3*EL*m12squared*SA2*dAlpha1MSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.84375&
  &D0*CA22*CA3*EL*m12squared*SA2*dAlpha1MSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.375D0*EL*MH12*SA3*dAlpha1MSBarUsua&
  &l()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.375D0*CA22*EL*MH12*SA3*dAlpha1MSBarUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.187&
  &5D0*EL*MH32*SA3*dAlpha1MSBarUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA22*EL*MH32*SA3*dAlpha1MSBarUsual()*DBLE(SA1&
  &**INT(3.D0)))/(MW*SB*SW) + (0.375D0*EL*MH12*SA22*SA3*dAlpha1MSBarUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*EL*MH32*&
  &SA22*SA3*dAlpha1MSBarUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.5625D0*EL*m12squared*SA3*dAlpha1MSBarUsual()*DBLE(SA1**INT(&
  &3.D0)))/(CB*MW*SB2*SW) + (0.5625D0*CA22*EL*m12squared*SA3*dAlpha1MSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.5625D0&
  &*EL*m12squared*SA22*SA3*dAlpha1MSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.5D0*CA2*EL*MH12*SA2*SA3*dAlpha2MSBarUsua&
  &l()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.25D0*CA2*EL*MH32*SA2*SA3*dAlpha2MSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0&
  &625D0*CA2*CA3*EL*MH12*dAlpha2MSBarUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA2*CA3*EL*MH32*dAlpha2MSBarUsual()*DB&
  &LE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA2*CA3*EL*MH12*SA22*dAlpha2MSBarUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.2812&
  &5D0*CA2*CA3*EL*MH32*SA22*dAlpha2MSBarUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.75D0*CA2*EL*m12squared*SA2*SA3*dAlpha2MSBar&
  &Usual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA2*CA3*EL*m12squared*dAlpha2MSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*&
  &MW*SB2*SW) + (0.84375D0*CA2*CA3*EL*m12squared*SA22*dAlpha2MSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.125D0*CA3*EL*&
  &MH12*dAlpha3MSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.125D0*CA22*CA3*EL*MH12*dAlpha3MSBarUsual()*DBLE(SA1**INT(3.D0))&
  &)/(CB*MW*SW) + (0.0625D0*CA3*EL*MH32*dAlpha3MSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA22*CA3*EL*MH32*dAlpha3&
  &MSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.125D0*CA3*EL*MH12*SA22*dAlpha3MSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW)&
  & - (0.0625D0*CA3*EL*MH32*SA22*dAlpha3MSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA3*EL*m12squared*dAlpha3MSBarU&
  &sual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*CA22*CA3*EL*m12squared*dAlpha3MSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB2*&
  &MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA22*dAlpha3MSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D0*EL*MH12*SA2*&
  &SA3*dAlpha3MSBarUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA22*EL*MH12*SA2*SA3*dAlpha3MSBarUsual()*DBLE(SA1**INT(3.&
  &D0)))/(MW*SB*SW) - (0.03125D0*EL*MH32*SA2*SA3*dAlpha3MSBarUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA22*EL*MH32*S&
  &A2*SA3*dAlpha3MSBarUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*EL*m12squared*SA2*SA3*dAlpha3MSBarUsual()*DBLE(SA1**I&
  &NT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA22*EL*m12squared*SA2*SA3*dAlpha3MSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (&
  &0.03125D0*CA3*EL*MH12*SA2*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA22*CA3*EL*MH12*SA2*dBetaMSBarUsual&
  &()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.015625D0*CA3*EL*MH32*SA2*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0468&
  &75D0*CA22*CA3*EL*MH32*SA2*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.140625D0*CA3*EL*m12squared*SA2*dBetaMSBarUsu&
  &al()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.421875D0*CA22*CA3*EL*m12squared*SA2*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB&
  &2*MW*SB*SW) - (0.03125D0*CA3*EL*MH12*SA2*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.09375D0*CA22*CA3*EL*MH12*&
  &SA2*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.015625D0*CA3*EL*MH32*SA2*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0)&
  &))/(CB*MW*SB2*SW) - (0.046875D0*CA22*CA3*EL*MH32*SA2*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*EL*m12&
  &squared*SA3*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*CA22*EL*m12squared*SA3*dBetaMSBarUsual()*DBLE(S&
  &A1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.1875D0*EL*m12squared*SA22*SA3*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.&
  &03125D0*CA3*EL*MH12*SA2*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA22*CA3*EL*MH12*SA2*dBetaMSBarUsua&
  &l()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.015625D0*CA3*EL*MH32*SA2*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - &
  &(0.046875D0*CA22*CA3*EL*MH32*SA2*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.125D0*EL*MH12*SA3*TB*dBetaMSBarUsu&
  &al()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.125D0*CA22*EL*MH12*SA3*TB*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0&
  &625D0*EL*MH32*SA3*TB*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA22*EL*MH32*SA3*TB*dBetaMSBarUsual()*DBLE&
  &(SA1**INT(3.D0)))/(CB*MW*SW) - (0.125D0*EL*MH12*SA22*SA3*TB*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.0625D0*EL*&
  &MH32*SA22*SA3*TB*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA3*EL*MH12*dAlpha2MSBarUsual()*DBLE(CA2**INT(&
  &3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA3*EL*MH32*dAlpha2MSBarUsual()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0))&
  &)/(MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*dAlpha2MSBarUsual()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0&
  &.375D0*EL*m12squared*SA3*dBetaMSBarUsual()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) - (0.375D0*CA22*EL*m12squared*SA&
  &3*dBetaMSBarUsual()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) + (0.375D0*EL*m12squared*SA22*SA3*dBetaMSBarUsual()*DBL&
  &E(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) - (0.28125D0*CA1*CA3*EL*m12squared*dAlpha1MSBarUsual()*DBLE(SA2**INT(3.D0)))/(&
  &CB*MW*SW) + (0.1875D0*CA3*EL*MH12*SA1*dAlpha1MSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA12*CA3*EL*MH12*SA1*dA&
  &lpha1MSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA3*EL*MH32*SA1*dAlpha1MSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB*&
  &MW*SW) + (0.28125D0*CA12*CA3*EL*MH32*SA1*dAlpha1MSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*MH12*dAlp&
  &ha1MSBarUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA1*CA3*EL*MH32*dAlpha1MSBarUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB&
  &*SW) + (0.28125D0*CA3*EL*m12squared*SA1*dAlpha1MSBarUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA3*EL*m12squared*SA1&
  &*dAlpha1MSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.84375D0*CA12*CA3*EL*m12squared*SA1*dAlpha1MSBarUsual()*DBLE(SA2&
  &**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA3*EL*MH12*SA12*dAlpha1MSBarUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D&
  &0*CA1*CA3*EL*MH32*SA12*dAlpha1MSBarUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*m12squared*dAlpha1MSBarUsua&
  &l()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA1*CA3*EL*m12squared*SA12*dAlpha1MSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB&
  &*MW*SB2*SW) + (0.09375D0*CA1*CA3*EL*m12squared*dAlpha1MSBarUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA3*EL*m12&
  &squared*SA1*TB*dAlpha1MSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (1.5D0*CA3*MH12*dAlpha2MSBarUsual()*DBLE(SA2**INT(3.D0))&
  &)/vS + (0.75D0*CA3*MH32*dAlpha2MSBarUsual()*DBLE(SA2**INT(3.D0)))/vS + (0.1875D0*CA1*EL*MH12*SA3*dAlpha3MSBarUsual()*DBLE(SA2*&
  &*INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1*EL*MH32*SA3*dAlpha3MSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*EL*m12s&
  &quared*SA1*SA3*dAlpha3MSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA1*EL*MH12*SA12*SA3*dAlpha3MSBarUsual()*DBLE(&
  &SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1*EL*MH32*SA12*SA3*dAlpha3MSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D&
  &0*CA1*EL*m12squared*SA3*dAlpha3MSBarUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA1*EL*m12squared*SA3*dAlpha3MSBarUsu&
  &al()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*EL*MH12*SA1*SA3*dAlpha3MSBarUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (&
  &0.1875D0*CA12*EL*MH12*SA1*SA3*dAlpha3MSBarUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*EL*MH32*SA1*SA3*dAlpha3MSBarUs&
  &ual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA12*EL*MH32*SA1*SA3*dAlpha3MSBarUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) &
  &+ (0.28125D0*CA1*EL*m12squared*SA12*SA3*dAlpha3MSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*EL*m12squared*SA1&
  &*SA3*dAlpha3MSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA12*EL*m12squared*SA1*SA3*dAlpha3MSBarUsual()*DBLE&
  &(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.09375D0*EL*m12squared*SA1*SA3*dAlpha3MSBarUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - &
  &(0.09375D0*CA1*EL*m12squared*SA3*TB*dAlpha3MSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA3*EL*MH12*SA1*dBetaMSB&
  &arUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA12*CA3*EL*MH12*SA1*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW&
  &) - (0.046875D0*CA3*EL*MH32*SA1*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.046875D0*CA12*CA3*EL*MH32*SA1*dBetaMSB&
  &arUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA3*EL*m12squared*SA1*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*S&
  &W) + (0.328125D0*CA3*EL*m12squared*SA1*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.421875D0*CA12*CA3*EL*m12squ&
  &ared*SA1*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA1*CA3*EL*m12squared*dBetaMSBarUsual()*DBLE(SA2*&
  &*INT(3.D0)))/(CB*MW*SB2*SW) + (0.09375D0*CA3*EL*MH12*SA1*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.09375D0*C&
  &A12*CA3*EL*MH12*SA1*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.046875D0*CA3*EL*MH32*SA1*dBetaMSBarUsual()*DBL&
  &E(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.046875D0*CA12*CA3*EL*MH32*SA1*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + &
  &(0.28125D0*CA1*CA3*EL*m12squared*SA12*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*CA1*CA3*EL*m12squared&
  &*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.09375D0*CA3*EL*MH12*SA1*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0)))/(M&
  &W*SB*SW*TB) - (0.09375D0*CA12*CA3*EL*MH12*SA1*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.046875D0*CA3*EL*MH32*&
  &SA1*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.046875D0*CA12*CA3*EL*MH32*SA1*dBetaMSBarUsual()*DBLE(SA2**INT(3&
  &.D0)))/(MW*SB*SW*TB) - (0.1875D0*CA1*CA3*EL*MH12*TB*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1*CA3*EL&
  &*MH32*TB*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.328125D0*CA3*EL*m12squared*SA1*TB*dBetaMSBarUsual()*DBLE(SA2*&
  &*INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA1*CA3*EL*MH12*SA12*TB*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA&
  &1*CA3*EL*MH32*SA12*TB*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.140625D0*CA3*EL*m12squared*SA1*dBetaMSBarUsual()&
  &*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB2) + (0.1875D0*CA1*CA3*EL*m12squared*TB2*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW)&
  & + (0.1875D0*CA3*EL*MH12*dAlpha1MSBarUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA3*EL*MH32*dA&
  &lpha1MSBarUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*dAlpha1MSBarUsual()*DBL&
  &E(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*EL*MH12*SA3*dAlpha3MSBarUsual()*DBLE(CA1**INT(3.D0))*DBLE(S&
  &A2**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*EL*MH32*SA3*dAlpha3MSBarUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) &
  &- (0.09375D0*EL*m12squared*SA3*dAlpha3MSBarUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA3*&
  &EL*m12squared*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.0625D0*CA3*EL*MH12*TB*dBetaMSBa&
  &rUsual()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*CA3*EL*MH32*TB*dBetaMSBarUsual()*DBLE(CA1**INT(3.D&
  &0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.375D0*CA1*CA3*EL*m12squared*dBetaMSBarUsual()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D&
  &0)))/(MW*SW) - (0.5625D0*CA1*CA3*EL*m12squared*SA12*dBetaMSBarUsual()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) + (0.&
  &1875D0*CA3*EL*m12squared*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) - (0.1875D0&
  &*CA3*EL*MH12*dAlpha1MSBarUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA3*EL*MH32*dAlpha1MSBarUs&
  &ual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA3*EL*m12squared*dAlpha1MSBarUsual()*DBLE(SA1**INT(3&
  &.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.0625D0*EL*MH12*SA3*dAlpha3MSBarUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0&
  &)))/(MW*SB*SW) + (0.03125D0*EL*MH32*SA3*dAlpha3MSBarUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0&
  &*EL*m12squared*SA3*dAlpha3MSBarUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.03125D0*CA3*EL*MH12*dBet&
  &aMSBarUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.015625D0*CA3*EL*MH32*dBetaMSBarUsual()*DBLE(SA1**INT(&
  &3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.140625D0*CA3*EL*m12squared*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3&
  &.D0)))/(CB2*MW*SB*SW) + (0.03125D0*CA3*EL*MH12*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (&
  &0.015625D0*CA3*EL*MH32*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.03125D0*CA3*EL*MH12*dB&
  &etaMSBarUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.015625D0*CA3*EL*MH32*dBetaMSBarUsual()*DBLE(SA1*&
  &*INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.328125D0*CA3*EL*m12squared*SA1*SA2*dBetaMSBarUsual()*DBLE(SB**INT(-3.D0))&
  &)/(MW*SW) - (0.421875D0*CA12*CA3*EL*m12squared*SA1*SA2*dBetaMSBarUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.984375D0*CA22*CA3*&
  &EL*m12squared*SA1*SA2*dBetaMSBarUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (1.265625D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*dBetaM&
  &SBarUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.09375D0*CA3*EL*m12squared*SA1*SA2*dBetaMSBarUsual()*DBLE(SB**INT(-3.D0)))/(CB2*&
  &MW*SW) - (0.140625D0*CA12*CA3*EL*m12squared*SA1*SA2*dBetaMSBarUsual()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.28125D0*CA22*CA3*&
  &EL*m12squared*SA1*SA2*dBetaMSBarUsual()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.421875D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*dB&
  &etaMSBarUsual()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.21875D0*CA1*EL*m12squared*SA3*dBetaMSBarUsual()*DBLE(SB**INT(-3.D0)))/(&
  &MW*SW) + (0.21875D0*CA1*CA22*EL*m12squared*SA3*dBetaMSBarUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.0625D0*CA1*EL*m12squared*S&
  &A3*dBetaMSBarUsual()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.0625D0*CA1*CA22*EL*m12squared*SA3*dBetaMSBarUsual()*DBLE(SB**INT(-&
  &3.D0)))/(CB2*MW*SW) + (0.84375D0*CA1*EL*m12squared*SA12*SA3*dBetaMSBarUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.84375D0*CA1*C&
  &A22*EL*m12squared*SA12*SA3*dBetaMSBarUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.28125D0*CA1*EL*m12squared*SA12*SA3*dBetaMSBarU&
  &sual()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.28125D0*CA1*CA22*EL*m12squared*SA12*SA3*dBetaMSBarUsual()*DBLE(SB**INT(-3.D0)))/&
  &(CB2*MW*SW) - (0.21875D0*CA1*EL*m12squared*SA22*SA3*dBetaMSBarUsual()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.0625D0*CA1*EL*m12squa&
  &red*SA22*SA3*dBetaMSBarUsual()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.84375D0*CA1*EL*m12squared*SA12*SA22*SA3*dBetaMSBarUsual(&
  &)*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.28125D0*CA1*EL*m12squared*SA12*SA22*SA3*dBetaMSBarUsual()*DBLE(SB**INT(-3.D0)))/(CB2*MW*S&
  &W) - (0.28125D0*EL*m12squared*SA3*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.28125D0*CA22*EL*m1&
  &2squared*SA3*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.09375D0*EL*m12squared*SA3*dBetaMSBarUsu&
  &al()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.09375D0*CA22*EL*m12squared*SA3*dBetaMSBarUsual()*DBLE(CA1**IN&
  &T(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.28125D0*EL*m12squared*SA22*SA3*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0))*DBLE(SB*&
  &*INT(-3.D0)))/(MW*SW) + (0.09375D0*EL*m12squared*SA22*SA3*dBetaMSBarUsual()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW&
  &*SW) + (0.140625D0*CA3*EL*m12squared*SA2*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.421875D0*CA&
  &22*CA3*EL*m12squared*SA2*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.046875D0*CA3*EL*m12squared*&
  &SA2*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.140625D0*CA22*CA3*EL*m12squared*SA2*dBetaMSB&
  &arUsual()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.328125D0*CA3*EL*m12squared*SA1*dBetaMSBarUsual()*DBLE(SA&
  &2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.421875D0*CA12*CA3*EL*m12squared*SA1*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0))*DB&
  &LE(SB**INT(-3.D0)))/(MW*SW) - (0.09375D0*CA3*EL*m12squared*SA1*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(C&
  &B2*MW*SW) + (0.140625D0*CA12*CA3*EL*m12squared*SA1*dBetaMSBarUsual()*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - &
  &(0.140625D0*CA3*EL*m12squared*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.0&
  &46875D0*CA3*EL*m12squared*dBetaMSBarUsual()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.1&
  &875D0*CA1*CA3*MH12*SA2*dgAtMZ())/(CB*MW) + (0.5625D0*CA1*CA22*CA3*MH12*SA2*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*CA3*MH32*SA2*dgA&
  &tMZ())/(CB*MW) + (0.28125D0*CA1*CA22*CA3*MH32*SA2*dgAtMZ())/(CB*MW) + (0.28125D0*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*MW) + (0&
  &.84375D0*CA22*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*MW) - (0.1875D0*CA1*CA3*MH12*SA12*SA2*dgAtMZ())/(CB*MW) - (0.5625D0*CA1*CA2&
  &2*CA3*MH12*SA12*SA2*dgAtMZ())/(CB*MW) - (0.09375D0*CA1*CA3*MH32*SA12*SA2*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*CA3*MH32*SA12&
  &*SA2*dgAtMZ())/(CB*MW) + (0.1875D0*CA1*m12squared*SA3*dgAtMZ())/(CB*MW) + (0.1875D0*CA1*CA22*m12squared*SA3*dgAtMZ())/(CB*MW) &
  &- (0.125D0*MH12*SA1*SA3*dgAtMZ())/(CB*MW) - (0.375D0*CA12*MH12*SA1*SA3*dgAtMZ())/(CB*MW) - (0.125D0*CA22*MH12*SA1*SA3*dgAtMZ()&
  &)/(CB*MW) - (0.375D0*CA12*CA22*MH12*SA1*SA3*dgAtMZ())/(CB*MW) - (0.0625D0*MH32*SA1*SA3*dgAtMZ())/(CB*MW) - (0.1875D0*CA12*MH32&
  &*SA1*SA3*dgAtMZ())/(CB*MW) - (0.0625D0*CA22*MH32*SA1*SA3*dgAtMZ())/(CB*MW) - (0.1875D0*CA12*CA22*MH32*SA1*SA3*dgAtMZ())/(CB*MW&
  &) - (0.1875D0*CA1*m12squared*SA22*SA3*dgAtMZ())/(CB*MW) + (0.125D0*MH12*SA1*SA22*SA3*dgAtMZ())/(CB*MW) + (0.375D0*CA12*MH12*SA&
  &1*SA22*SA3*dgAtMZ())/(CB*MW) + (0.0625D0*MH32*SA1*SA22*SA3*dgAtMZ())/(CB*MW) + (0.1875D0*CA12*MH32*SA1*SA22*SA3*dgAtMZ())/(CB*&
  &MW) + (0.28125D0*CA1*CA3*m12squared*SA2*dgAtMZ())/(MW*SB) + (0.84375D0*CA1*CA22*CA3*m12squared*SA2*dgAtMZ())/(MW*SB) - (0.1875&
  &D0*CA1*CA3*m12squared*SA2*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA1*CA22*CA3*m12squared*SA2*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*CA3*&
  &MH12*SA1*SA2*dgAtMZ())/(MW*SB) - (0.1875D0*CA12*CA3*MH12*SA1*SA2*dgAtMZ())/(MW*SB) + (0.5625D0*CA22*CA3*MH12*SA1*SA2*dgAtMZ())&
  &/(MW*SB) - (0.5625D0*CA12*CA22*CA3*MH12*SA1*SA2*dgAtMZ())/(MW*SB) + (0.09375D0*CA3*MH32*SA1*SA2*dgAtMZ())/(MW*SB) - (0.09375D0&
  &*CA12*CA3*MH32*SA1*SA2*dgAtMZ())/(MW*SB) + (0.28125D0*CA22*CA3*MH32*SA1*SA2*dgAtMZ())/(MW*SB) - (0.28125D0*CA12*CA22*CA3*MH32*&
  &SA1*SA2*dgAtMZ())/(MW*SB) + (0.28125D0*CA1*CA3*m12squared*SA12*SA2*dgAtMZ())/(CB2*MW*SB) + (0.84375D0*CA1*CA22*CA3*m12squared*&
  &SA12*SA2*dgAtMZ())/(CB2*MW*SB) + (0.125D0*CA1*MH12*SA3*dgAtMZ())/(MW*SB) + (0.125D0*CA1*CA22*MH12*SA3*dgAtMZ())/(MW*SB) + (0.0&
  &625D0*CA1*MH32*SA3*dgAtMZ())/(MW*SB) + (0.0625D0*CA1*CA22*MH32*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*m12squared*SA1*SA3*dgAtMZ())/&
  &(MW*SB) - (0.1875D0*CA22*m12squared*SA1*SA3*dgAtMZ())/(MW*SB) + (0.125D0*m12squared*SA1*SA3*dgAtMZ())/(CB2*MW*SB) + (0.5625D0*&
  &CA12*m12squared*SA1*SA3*dgAtMZ())/(CB2*MW*SB) + (0.125D0*CA22*m12squared*SA1*SA3*dgAtMZ())/(CB2*MW*SB) + (0.5625D0*CA12*CA22*m&
  &12squared*SA1*SA3*dgAtMZ())/(CB2*MW*SB) + (0.375D0*CA1*MH12*SA12*SA3*dgAtMZ())/(MW*SB) + (0.375D0*CA1*CA22*MH12*SA12*SA3*dgAtM&
  &Z())/(MW*SB) + (0.1875D0*CA1*MH32*SA12*SA3*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*CA22*MH32*SA12*SA3*dgAtMZ())/(MW*SB) - (0.125D0*C&
  &A1*MH12*SA22*SA3*dgAtMZ())/(MW*SB) - (0.0625D0*CA1*MH32*SA22*SA3*dgAtMZ())/(MW*SB) + (0.1875D0*m12squared*SA1*SA22*SA3*dgAtMZ(&
  &))/(MW*SB) - (0.125D0*m12squared*SA1*SA22*SA3*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA12*m12squared*SA1*SA22*SA3*dgAtMZ())/(CB2*MW&
  &*SB) - (0.375D0*CA1*MH12*SA12*SA22*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*CA1*MH32*SA12*SA22*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*CA3*&
  &m12squared*SA1*SA2*dgAtMZ())/(CB*MW*SB2) + (0.28125D0*CA12*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA22*CA3*m&
  &12squared*SA1*SA2*dgAtMZ())/(CB*MW*SB2) + (0.84375D0*CA12*CA22*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*MW*SB2) - (0.125D0*CA1*m12&
  &squared*SA3*dgAtMZ())/(CB*MW*SB2) - (0.125D0*CA1*CA22*m12squared*SA3*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA1*m12squared*SA12*SA3&
  &*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA1*CA22*m12squared*SA12*SA3*dgAtMZ())/(CB*MW*SB2) + (0.125D0*CA1*m12squared*SA22*SA3*dgAtM&
  &Z())/(CB*MW*SB2) + (0.5625D0*CA1*m12squared*SA12*SA22*SA3*dgAtMZ())/(CB*MW*SB2) - (0.09375D0*CA3*m12squared*SA1*SA2*dgAtMZ())/&
  &(MW*SB*TB) - (0.28125D0*CA22*CA3*m12squared*SA1*SA2*dgAtMZ())/(MW*SB*TB) - (0.0625D0*CA1*m12squared*SA3*dgAtMZ())/(MW*SB*TB) -&
  & (0.0625D0*CA1*CA22*m12squared*SA3*dgAtMZ())/(MW*SB*TB) + (0.0625D0*CA1*m12squared*SA22*SA3*dgAtMZ())/(MW*SB*TB) - (0.09375D0*&
  &CA1*CA3*m12squared*SA2*TB*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*CA3*m12squared*SA2*TB*dgAtMZ())/(CB*MW) + (0.0625D0*m12squar&
  &ed*SA1*SA3*TB*dgAtMZ())/(CB*MW) + (0.0625D0*CA22*m12squared*SA1*SA3*TB*dgAtMZ())/(CB*MW) - (0.0625D0*m12squared*SA1*SA22*SA3*T&
  &B*dgAtMZ())/(CB*MW) + (0.0625D0*CA3*MH12*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875D0*CA22*CA3*MH12*SA2*DBLE(CA1**IN&
  &T(3.D0))*dgAtMZ())/(CB*MW) + (0.03125D0*CA3*MH32*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA22*CA3*MH32*SA2*DBL&
  &E(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.09375D0*CA3*m12squared*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.28125D0*CA&
  &22*CA3*m12squared*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.125D0*MH12*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - &
  &(0.125D0*CA22*MH12*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.0625D0*MH32*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0&
  &.0625D0*CA22*MH32*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.125D0*MH12*SA22*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) +&
  & (0.0625D0*MH32*SA22*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*m12squared*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*&
  &MW*SB2) + (0.1875D0*CA22*m12squared*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*m12squared*SA22*SA3*DBLE(CA1**I&
  &NT(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.125D0*MH12*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.125D0*CA22*MH12*SA3*DBLE(SA1**&
  &INT(3.D0))*dgAtMZ())/(CB*MW) + (0.0625D0*MH32*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.0625D0*CA22*MH32*SA3*DBLE(SA1**I&
  &NT(3.D0))*dgAtMZ())/(CB*MW) - (0.125D0*MH12*SA22*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*MH32*SA22*SA3*DBLE(SA1&
  &**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.0625D0*CA3*MH12*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA22*CA3*MH12*SA2*D&
  &BLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.03125D0*CA3*MH32*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA22*CA3*M&
  &H32*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.1875D0*m12squared*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875&
  &D0*CA22*m12squared*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*m12squared*SA22*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ(&
  &))/(CB2*MW*SB) - (0.09375D0*CA3*m12squared*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA22*CA3*m12squared*SA2&
  &*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*CA1*CA3*MH12*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.09375D0*CA1*C&
  &A3*MH32*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.28125D0*CA3*m12squared*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.187&
  &5D0*CA1*CA3*MH12*SA12*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*CA3*MH32*SA12*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB&
  &*MW) - (0.28125D0*CA1*CA3*m12squared*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*CA3*m12squared*DBLE(SA2**INT(3.D0)&
  &)*dgAtMZ())/(CB2*MW*SB) - (0.1875D0*CA3*MH12*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA12*CA3*MH12*SA1*DBLE(SA2&
  &**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.09375D0*CA3*MH32*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA12*CA3*MH32*SA1&
  &*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.28125D0*CA1*CA3*m12squared*SA12*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) + (0.1&
  &875D0*CA3*m12squared*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA12*CA3*m12squared*SA1*DBLE(SA2**INT(3.D0))*&
  &dgAtMZ())/(CB*MW*SB2) + (0.09375D0*CA3*m12squared*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB*TB) + (0.09375D0*CA1*CA3*m12square&
  &d*TB*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*CA3*MH12*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) -&
  & (0.03125D0*CA3*MH32*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA3*m12squared*DBLE(CA1**INT(3.D&
  &0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.0625D0*CA3*MH12*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB&
  &) - (0.03125D0*CA3*MH32*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA3*m12squared*DBLE(SA1**INT(&
  &3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.28125D0*CA3*EL*SA1*SA2*dm122MSBarUsual())/(CB*MW*SW) + (0.84375D0*CA22*&
  &CA3*EL*SA1*SA2*dm122MSBarUsual())/(CB*MW*SW) + (0.1875D0*CA1*EL*SA3*dm122MSBarUsual())/(CB*MW*SW) + (0.1875D0*CA1*CA22*EL*SA3*&
  &dm122MSBarUsual())/(CB*MW*SW) - (0.1875D0*CA1*EL*SA22*SA3*dm122MSBarUsual())/(CB*MW*SW) + (0.28125D0*CA1*CA3*EL*SA2*dm122MSBar&
  &Usual())/(MW*SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*SA2*dm122MSBarUsual())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*SA2*dm122MSBarUsual(&
  &))/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA22*CA3*EL*SA2*dm122MSBarUsual())/(CB2*MW*SB*SW) + (0.28125D0*CA1*CA3*EL*SA12*SA2*dm122MSBa&
  &rUsual())/(CB2*MW*SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*SA12*SA2*dm122MSBarUsual())/(CB2*MW*SB*SW) - (0.1875D0*EL*SA1*SA3*dm122M&
  &SBarUsual())/(MW*SB*SW) - (0.1875D0*CA22*EL*SA1*SA3*dm122MSBarUsual())/(MW*SB*SW) + (0.125D0*EL*SA1*SA3*dm122MSBarUsual())/(CB&
  &2*MW*SB*SW) + (0.5625D0*CA12*EL*SA1*SA3*dm122MSBarUsual())/(CB2*MW*SB*SW) + (0.125D0*CA22*EL*SA1*SA3*dm122MSBarUsual())/(CB2*M&
  &W*SB*SW) + (0.5625D0*CA12*CA22*EL*SA1*SA3*dm122MSBarUsual())/(CB2*MW*SB*SW) + (0.1875D0*EL*SA1*SA22*SA3*dm122MSBarUsual())/(MW&
  &*SB*SW) - (0.125D0*EL*SA1*SA22*SA3*dm122MSBarUsual())/(CB2*MW*SB*SW) - (0.5625D0*CA12*EL*SA1*SA22*SA3*dm122MSBarUsual())/(CB2*&
  &MW*SB*SW) - (0.1875D0*CA3*EL*SA1*SA2*dm122MSBarUsual())/(CB*MW*SB2*SW) + (0.28125D0*CA12*CA3*EL*SA1*SA2*dm122MSBarUsual())/(CB&
  &*MW*SB2*SW) - (0.5625D0*CA22*CA3*EL*SA1*SA2*dm122MSBarUsual())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA22*CA3*EL*SA1*SA2*dm122MSBar&
  &Usual())/(CB*MW*SB2*SW) - (0.125D0*CA1*EL*SA3*dm122MSBarUsual())/(CB*MW*SB2*SW) - (0.125D0*CA1*CA22*EL*SA3*dm122MSBarUsual())/&
  &(CB*MW*SB2*SW) - (0.5625D0*CA1*EL*SA12*SA3*dm122MSBarUsual())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*EL*SA12*SA3*dm122MSBarUsual(&
  &))/(CB*MW*SB2*SW) + (0.125D0*CA1*EL*SA22*SA3*dm122MSBarUsual())/(CB*MW*SB2*SW) + (0.5625D0*CA1*EL*SA12*SA22*SA3*dm122MSBarUsua&
  &l())/(CB*MW*SB2*SW) - (0.09375D0*CA3*EL*SA1*SA2*dm122MSBarUsual())/(MW*SB*SW*TB) - (0.28125D0*CA22*CA3*EL*SA1*SA2*dm122MSBarUs&
  &ual())/(MW*SB*SW*TB) - (0.0625D0*CA1*EL*SA3*dm122MSBarUsual())/(MW*SB*SW*TB) - (0.0625D0*CA1*CA22*EL*SA3*dm122MSBarUsual())/(M&
  &W*SB*SW*TB) + (0.0625D0*CA1*EL*SA22*SA3*dm122MSBarUsual())/(MW*SB*SW*TB) - (0.09375D0*CA1*CA3*EL*SA2*TB*dm122MSBarUsual())/(CB&
  &*MW*SW) - (0.28125D0*CA1*CA22*CA3*EL*SA2*TB*dm122MSBarUsual())/(CB*MW*SW) + (0.0625D0*EL*SA1*SA3*TB*dm122MSBarUsual())/(CB*MW*&
  &SW) + (0.0625D0*CA22*EL*SA1*SA3*TB*dm122MSBarUsual())/(CB*MW*SW) - (0.0625D0*EL*SA1*SA22*SA3*TB*dm122MSBarUsual())/(CB*MW*SW) &
  &- (0.09375D0*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dm122MSBarUsual())/(CB2*MW*SB*SW) - (0.28125D0*CA22*CA3*EL*SA2*DBLE(CA1**INT(3.D0&
  &))*dm122MSBarUsual())/(CB2*MW*SB*SW) + (0.1875D0*EL*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SB2*SW) + (0.1875D0*CA2&
  &2*EL*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SB2*SW) - (0.1875D0*EL*SA22*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarUsual()&
  &)/(CB*MW*SB2*SW) - (0.1875D0*EL*SA3*DBLE(SA1**INT(3.D0))*dm122MSBarUsual())/(CB2*MW*SB*SW) - (0.1875D0*CA22*EL*SA3*DBLE(SA1**I&
  &NT(3.D0))*dm122MSBarUsual())/(CB2*MW*SB*SW) + (0.1875D0*EL*SA22*SA3*DBLE(SA1**INT(3.D0))*dm122MSBarUsual())/(CB2*MW*SB*SW) - (&
  &0.09375D0*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SB2*SW) - (0.28125D0*CA22*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*&
  &dm122MSBarUsual())/(CB*MW*SB2*SW) - (0.28125D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SW) - (0.28125D0*CA1*&
  &CA3*EL*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(CB2*&
  &MW*SB*SW) - (0.28125D0*CA1*CA3*EL*SA12*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*SA1*DBLE(SA2*&
  &*INT(3.D0))*dm122MSBarUsual())/(CB*MW*SB2*SW) - (0.28125D0*CA12*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SB2*&
  &SW) + (0.09375D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(MW*SB*SW*TB) + (0.09375D0*CA1*CA3*EL*TB*DBLE(SA2**INT(3.D&
  &0))*dm122MSBarUsual())/(CB*MW*SW) + (0.09375D0*CA3*EL*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(CB2*MW*SB*&
  &SW) + (0.09375D0*CA3*EL*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm122MSBarUsual())/(CB*MW*SB2*SW) + (0.1875D0*CA1*CA3*EL*SA2&
  &*dMH12OSUsual())/(CB*MW*SW) + (0.5625D0*CA1*CA22*CA3*EL*SA2*dMH12OSUsual())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*SA12*SA2*dMH12OS&
  &Usual())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*EL*SA12*SA2*dMH12OSUsual())/(CB*MW*SW) - (0.125D0*EL*SA1*SA3*dMH12OSUsual())/(CB*&
  &MW*SW) - (0.375D0*CA12*EL*SA1*SA3*dMH12OSUsual())/(CB*MW*SW) - (0.125D0*CA22*EL*SA1*SA3*dMH12OSUsual())/(CB*MW*SW) - (0.375D0*&
  &CA12*CA22*EL*SA1*SA3*dMH12OSUsual())/(CB*MW*SW) + (0.125D0*EL*SA1*SA22*SA3*dMH12OSUsual())/(CB*MW*SW) + (0.375D0*CA12*EL*SA1*S&
  &A22*SA3*dMH12OSUsual())/(CB*MW*SW) + (0.1875D0*CA3*EL*SA1*SA2*dMH12OSUsual())/(MW*SB*SW) - (0.1875D0*CA12*CA3*EL*SA1*SA2*dMH12&
  &OSUsual())/(MW*SB*SW) + (0.5625D0*CA22*CA3*EL*SA1*SA2*dMH12OSUsual())/(MW*SB*SW) - (0.5625D0*CA12*CA22*CA3*EL*SA1*SA2*dMH12OSU&
  &sual())/(MW*SB*SW) + (0.125D0*CA1*EL*SA3*dMH12OSUsual())/(MW*SB*SW) + (0.125D0*CA1*CA22*EL*SA3*dMH12OSUsual())/(MW*SB*SW) + (0&
  &.375D0*CA1*EL*SA12*SA3*dMH12OSUsual())/(MW*SB*SW) + (0.375D0*CA1*CA22*EL*SA12*SA3*dMH12OSUsual())/(MW*SB*SW) - (0.125D0*CA1*EL&
  &*SA22*SA3*dMH12OSUsual())/(MW*SB*SW) - (0.375D0*CA1*EL*SA12*SA22*SA3*dMH12OSUsual())/(MW*SB*SW) - (0.5D0*CA2*CA3*dMH12OSUsual(&
  &))/vS - (1.5D0*CA2*CA3*SA22*dMH12OSUsual())/vS + (0.0625D0*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dMH12OSUsual())/(CB*MW*SW) + (0.187&
  &5D0*CA22*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dMH12OSUsual())/(CB*MW*SW) - (0.125D0*EL*SA3*DBLE(CA1**INT(3.D0))*dMH12OSUsual())/(MW&
  &*SB*SW) - (0.125D0*CA22*EL*SA3*DBLE(CA1**INT(3.D0))*dMH12OSUsual())/(MW*SB*SW) + (0.125D0*EL*SA22*SA3*DBLE(CA1**INT(3.D0))*dMH&
  &12OSUsual())/(MW*SB*SW) + (0.5D0*CA3*DBLE(CA2**INT(3.D0))*dMH12OSUsual())/vS + (0.125D0*EL*SA3*DBLE(SA1**INT(3.D0))*dMH12OSUsu&
  &al())/(CB*MW*SW) + (0.125D0*CA22*EL*SA3*DBLE(SA1**INT(3.D0))*dMH12OSUsual())/(CB*MW*SW) - (0.125D0*EL*SA22*SA3*DBLE(SA1**INT(3&
  &.D0))*dMH12OSUsual())/(CB*MW*SW) + (0.0625D0*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*dMH12OSUsual())/(MW*SB*SW) + (0.1875D0*CA22*CA3*E&
  &L*SA2*DBLE(SA1**INT(3.D0))*dMH12OSUsual())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*DBLE(SA2**INT(3.D0))*dMH12OSUsual())/(CB*MW*SW) +&
  & (0.1875D0*CA1*CA3*EL*SA12*DBLE(SA2**INT(3.D0))*dMH12OSUsual())/(CB*MW*SW) - (0.1875D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dMH12OS&
  &Usual())/(MW*SB*SW) + (0.1875D0*CA12*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dMH12OSUsual())/(MW*SB*SW) - (0.0625D0*CA3*EL*DBLE(CA1**I&
  &NT(3.D0))*DBLE(SA2**INT(3.D0))*dMH12OSUsual())/(CB*MW*SW) - (0.0625D0*CA3*EL*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH12OS&
  &Usual())/(MW*SB*SW) + (0.09375D0*CA1*CA3*EL*SA2*dMH32OSUsual())/(CB*MW*SW) + (0.28125D0*CA1*CA22*CA3*EL*SA2*dMH32OSUsual())/(C&
  &B*MW*SW) - (0.09375D0*CA1*CA3*EL*SA12*SA2*dMH32OSUsual())/(CB*MW*SW) - (0.28125D0*CA1*CA22*CA3*EL*SA12*SA2*dMH32OSUsual())/(CB&
  &*MW*SW) - (0.0625D0*EL*SA1*SA3*dMH32OSUsual())/(CB*MW*SW) - (0.1875D0*CA12*EL*SA1*SA3*dMH32OSUsual())/(CB*MW*SW) - (0.0625D0*C&
  &A22*EL*SA1*SA3*dMH32OSUsual())/(CB*MW*SW) - (0.1875D0*CA12*CA22*EL*SA1*SA3*dMH32OSUsual())/(CB*MW*SW) + (0.0625D0*EL*SA1*SA22*&
  &SA3*dMH32OSUsual())/(CB*MW*SW) + (0.1875D0*CA12*EL*SA1*SA22*SA3*dMH32OSUsual())/(CB*MW*SW) + (0.09375D0*CA3*EL*SA1*SA2*dMH32OS&
  &Usual())/(MW*SB*SW) - (0.09375D0*CA12*CA3*EL*SA1*SA2*dMH32OSUsual())/(MW*SB*SW) + (0.28125D0*CA22*CA3*EL*SA1*SA2*dMH32OSUsual(&
  &))/(MW*SB*SW) - (0.28125D0*CA12*CA22*CA3*EL*SA1*SA2*dMH32OSUsual())/(MW*SB*SW) + (0.0625D0*CA1*EL*SA3*dMH32OSUsual())/(MW*SB*S&
  &W) + (0.0625D0*CA1*CA22*EL*SA3*dMH32OSUsual())/(MW*SB*SW) + (0.1875D0*CA1*EL*SA12*SA3*dMH32OSUsual())/(MW*SB*SW) + (0.1875D0*C&
  &A1*CA22*EL*SA12*SA3*dMH32OSUsual())/(MW*SB*SW) - (0.0625D0*CA1*EL*SA22*SA3*dMH32OSUsual())/(MW*SB*SW) - (0.1875D0*CA1*EL*SA12*&
  &SA22*SA3*dMH32OSUsual())/(MW*SB*SW) - (0.25D0*CA2*CA3*dMH32OSUsual())/vS - (0.75D0*CA2*CA3*SA22*dMH32OSUsual())/vS + (0.03125D&
  &0*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dMH32OSUsual())/(CB*MW*SW) + (0.09375D0*CA22*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dMH32OSUsual())&
  &/(CB*MW*SW) - (0.0625D0*EL*SA3*DBLE(CA1**INT(3.D0))*dMH32OSUsual())/(MW*SB*SW) - (0.0625D0*CA22*EL*SA3*DBLE(CA1**INT(3.D0))*dM&
  &H32OSUsual())/(MW*SB*SW) + (0.0625D0*EL*SA22*SA3*DBLE(CA1**INT(3.D0))*dMH32OSUsual())/(MW*SB*SW) + (0.25D0*CA3*DBLE(CA2**INT(3&
  &.D0))*dMH32OSUsual())/vS + (0.0625D0*EL*SA3*DBLE(SA1**INT(3.D0))*dMH32OSUsual())/(CB*MW*SW) + (0.0625D0*CA22*EL*SA3*DBLE(SA1**&
  &INT(3.D0))*dMH32OSUsual())/(CB*MW*SW) - (0.0625D0*EL*SA22*SA3*DBLE(SA1**INT(3.D0))*dMH32OSUsual())/(CB*MW*SW) + (0.03125D0*CA3&
  &*EL*SA2*DBLE(SA1**INT(3.D0))*dMH32OSUsual())/(MW*SB*SW) + (0.09375D0*CA22*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*dMH32OSUsual())/(MW*&
  &SB*SW) - (0.09375D0*CA1*CA3*EL*DBLE(SA2**INT(3.D0))*dMH32OSUsual())/(CB*MW*SW) + (0.09375D0*CA1*CA3*EL*SA12*DBLE(SA2**INT(3.D0&
  &))*dMH32OSUsual())/(CB*MW*SW) - (0.09375D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dMH32OSUsual())/(MW*SB*SW) + (0.09375D0*CA12*CA3*EL&
  &*SA1*DBLE(SA2**INT(3.D0))*dMH32OSUsual())/(MW*SB*SW) - (0.03125D0*CA3*EL*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH32OSUsua&
  &l())/(CB*MW*SW) - (0.03125D0*CA3*EL*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH32OSUsual())/(MW*SB*SW) - (0.09375D0*CA1*CA3*&
  &EL*MH12*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.28125D0*CA1*CA22*CA3*EL*MH12*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/&
  &(CB*SW) - (0.046875D0*CA1*CA3*EL*MH32*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.140625D0*CA1*CA22*CA3*EL*MH32*SA2*DBL&
  &E(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.140625D0*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.&
  &421875D0*CA22*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.09375D0*CA1*CA3*EL*MH12*SA12*SA2*DBLE(M&
  &W**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.28125D0*CA1*CA22*CA3*EL*MH12*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.&
  &046875D0*CA1*CA3*EL*MH32*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.140625D0*CA1*CA22*CA3*EL*MH32*SA12*SA2*DBLE(M&
  &W**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.09375D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.09375D0*&
  &CA1*CA22*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.0625D0*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Usua&
  &l())/(CB*SW) + (0.1875D0*CA12*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.0625D0*CA22*EL*MH12*SA1*SA3*DBLE(&
  &MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.1875D0*CA12*CA22*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.0312&
  &5D0*EL*MH32*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.09375D0*CA12*EL*MH32*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual&
  &())/(CB*SW) + (0.03125D0*CA22*EL*MH32*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.09375D0*CA12*CA22*EL*MH32*SA1*SA3&
  &*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.09375D0*CA1*EL*m12squared*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) -&
  & (0.0625D0*EL*MH12*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.1875D0*CA12*EL*MH12*SA1*SA22*SA3*DBLE(MW**INT(-&
  &3.D0))*dMW2Usual())/(CB*SW) - (0.03125D0*EL*MH32*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.09375D0*CA12*EL*M&
  &H32*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.140625D0*CA1*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*dMW2Us&
  &ual())/(SB*SW) - (0.421875D0*CA1*CA22*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.09375D0*CA1*CA3*EL*&
  &m12squared*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) + (0.28125D0*CA1*CA22*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*&
  &dMW2Usual())/(CB2*SB*SW) - (0.09375D0*CA3*EL*MH12*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.09375D0*CA12*CA3*EL*M&
  &H12*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.28125D0*CA22*CA3*EL*MH12*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/&
  &(SB*SW) + (0.28125D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.046875D0*CA3*EL*MH32*SA1*SA2&
  &*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.046875D0*CA12*CA3*EL*MH32*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - &
  &(0.140625D0*CA22*CA3*EL*MH32*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.140625D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*DBL&
  &E(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.140625D0*CA1*CA3*EL*m12squared*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*&
  &SW) - (0.421875D0*CA1*CA22*CA3*EL*m12squared*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) - (0.0625D0*CA1*EL*MH12*SA&
  &3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.0625D0*CA1*CA22*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.03&
  &125D0*CA1*EL*MH32*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.03125D0*CA1*CA22*EL*MH32*SA3*DBLE(MW**INT(-3.D0))*dMW2Usu&
  &al())/(SB*SW) + (0.09375D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.09375D0*CA22*EL*m12squared*SA1&
  &*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.0625D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW)&
  & - (0.28125D0*CA12*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) - (0.0625D0*CA22*EL*m12squared*SA1*SA3*&
  &DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) - (0.28125D0*CA12*CA22*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(&
  &CB2*SB*SW) - (0.1875D0*CA1*EL*MH12*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.1875D0*CA1*CA22*EL*MH12*SA12*SA3*DB&
  &LE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.09375D0*CA1*EL*MH32*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.09375&
  &D0*CA1*CA22*EL*MH32*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.0625D0*CA1*EL*MH12*SA22*SA3*DBLE(MW**INT(-3.D0))*d&
  &MW2Usual())/(SB*SW) + (0.03125D0*CA1*EL*MH32*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.09375D0*EL*m12squared*SA1&
  &*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.0625D0*EL*m12squared*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(&
  &CB2*SB*SW) + (0.28125D0*CA12*EL*m12squared*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) + (0.1875D0*CA1*EL*MH12*&
  &SA12*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.09375D0*CA1*EL*MH32*SA12*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual(&
  &))/(SB*SW) + (0.09375D0*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) - (0.140625D0*CA12*CA3*EL*m12s&
  &quared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.28125D0*CA22*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*&
  &dMW2Usual())/(CB*SB2*SW) - (0.421875D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.&
  &0625D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.0625D0*CA1*CA22*EL*m12squared*SA3*DBLE(MW**INT&
  &(-3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.28125D0*CA1*EL*m12squared*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.2&
  &8125D0*CA1*CA22*EL*m12squared*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) - (0.0625D0*CA1*EL*m12squared*SA22*SA3*DB&
  &LE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) - (0.28125D0*CA1*EL*m12squared*SA12*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB&
  &*SB2*SW) + (0.046875D0*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW*TB) + (0.140625D0*CA22*CA3*EL*m12squ&
  &ared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW*TB) + (0.03125D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())&
  &/(SB*SW*TB) + (0.03125D0*CA1*CA22*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW*TB) - (0.03125D0*CA1*EL*m12square&
  &d*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW*TB) + (0.046875D0*CA1*CA3*EL*m12squared*SA2*TB*DBLE(MW**INT(-3.D0))*dMW2Us&
  &ual())/(CB*SW) + (0.140625D0*CA1*CA22*CA3*EL*m12squared*SA2*TB*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.03125D0*EL*m12sq&
  &uared*SA1*SA3*TB*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.03125D0*CA22*EL*m12squared*SA1*SA3*TB*DBLE(MW**INT(-3.D0))*dMW&
  &2Usual())/(CB*SW) + (0.03125D0*EL*m12squared*SA1*SA22*SA3*TB*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.03125D0*CA3*EL*MH1&
  &2*SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.09375D0*CA22*CA3*EL*MH12*SA2*DBLE(CA1**INT(3.D0))*DB&
  &LE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) - (0.015625D0*CA3*EL*MH32*SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(&
  &CB*SW) - (0.046875D0*CA22*CA3*EL*MH32*SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SW) + (0.046875D0*CA3*EL*&
  &m12squared*SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) + (0.140625D0*CA22*CA3*EL*m12squared*SA2*DBL&
  &E(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB2*SB*SW) + (0.0625D0*EL*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D&
  &0))*dMW2Usual())/(SB*SW) + (0.0625D0*CA22*EL*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.0312&
  &5D0*EL*MH32*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) + (0.03125D0*CA22*EL*MH32*SA3*DBLE(CA1**INT(3.D&
  &0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.0625D0*EL*MH12*SA22*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual&
  &())/(SB*SW) - (0.03125D0*EL*MH32*SA22*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(SB*SW) - (0.09375D0*EL*m12sq&
  &uared*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) - (0.09375D0*CA22*EL*m12squared*SA3*DBLE(CA1**INT&
  &(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.09375D0*EL*m12squared*SA22*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3&
  &.D0))*dMW2Usual())/(CB*SB2*SW) - (0.0625D0*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB*SW) - (0.062&
  &5D0*CA22*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB*SW) - (0.03125D0*EL*MH32*SA3*DBLE(MW**INT(-3.D&
  &0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB*SW) - (0.03125D0*CA22*EL*MH32*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usua&
  &l())/(CB*SW) + (0.0625D0*EL*MH12*SA22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB*SW) + (0.03125D0*EL*MH32*&
  &SA22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB*SW) - (0.03125D0*CA3*EL*MH12*SA2*DBLE(MW**INT(-3.D0))*DBLE&
  &(SA1**INT(3.D0))*dMW2Usual())/(SB*SW) - (0.09375D0*CA22*CA3*EL*MH12*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())&
  &/(SB*SW) - (0.015625D0*CA3*EL*MH32*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(SB*SW) - (0.046875D0*CA22*CA3*E&
  &L*MH32*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(SB*SW) + (0.09375D0*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*&
  &DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB2*SB*SW) + (0.09375D0*CA22*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dM&
  &W2Usual())/(CB2*SB*SW) - (0.09375D0*EL*m12squared*SA22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB2*SB*SW) &
  &+ (0.046875D0*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.140625D0*CA22*CA3*&
  &EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.09375D0*CA1*CA3*EL*MH12*DBLE(MW**INT&
  &(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SW) + (0.046875D0*CA1*CA3*EL*MH32*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW&
  &2Usual())/(CB*SW) + (0.140625D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SW) - (0.0937&
  &5D0*CA1*CA3*EL*MH12*SA12*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SW) - (0.046875D0*CA1*CA3*EL*MH32*SA12*DBL&
  &E(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SW) + (0.140625D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*DBLE(SA2*&
  &*INT(3.D0))*dMW2Usual())/(SB*SW) - (0.09375D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB&
  &2*SB*SW) + (0.09375D0*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(SB*SW) - (0.09375D0*CA12*CA3*EL*&
  &MH12*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(SB*SW) + (0.046875D0*CA3*EL*MH32*SA1*DBLE(MW**INT(-3.D0))*DBL&
  &E(SA2**INT(3.D0))*dMW2Usual())/(SB*SW) - (0.046875D0*CA12*CA3*EL*MH32*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual(&
  &))/(SB*SW) + (0.140625D0*CA1*CA3*EL*m12squared*SA12*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB2*SB*SW) - (0.09&
  &375D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SB2*SW) + (0.140625D0*CA12*CA3*EL*m12sq&
  &uared*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SB2*SW) - (0.046875D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(&
  &-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(SB*SW*TB) - (0.046875D0*CA1*CA3*EL*m12squared*TB*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT&
  &(3.D0))*dMW2Usual())/(CB*SW) + (0.03125D0*CA3*EL*MH12*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual&
  &())/(CB*SW) + (0.015625D0*CA3*EL*MH32*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SW) - (0&
  &.046875D0*CA3*EL*m12squared*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB2*SB*SW) + (0.03125&
  &D0*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(SB*SW) + (0.015625D0*CA3*EL*MH32*D&
  &BLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(SB*SW) - (0.046875D0*CA3*EL*m12squared*DBLE(MW**IN&
  &T(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Usual())/(CB*SB2*SW) + 0.5D0*CA2*CA3*MH12*DBLE(vS**INT(-2.D0))*dvSMSBa&
  &rUsual() + 0.25D0*CA2*CA3*MH32*DBLE(vS**INT(-2.D0))*dvSMSBarUsual() + 1.5D0*CA2*CA3*MH12*SA22*DBLE(vS**INT(-2.D0))*dvSMSBarUsu&
  &al() + 0.75D0*CA2*CA3*MH32*SA22*DBLE(vS**INT(-2.D0))*dvSMSBarUsual() - 0.5D0*CA3*MH12*DBLE(CA2**INT(3.D0))*DBLE(vS**INT(-2.D0)&
  &)*dvSMSBarUsual() - 0.25D0*CA3*MH32*DBLE(CA2**INT(3.D0))*DBLE(vS**INT(-2.D0))*dvSMSBarUsual() &
		& + CS1S1S1f111*dZH1H3OSUsual()/2D0 + CS1S1S1f211*dZH2H3OSUsual()/2D0 + CS1S1S1f311*dZH3H3OS()/2D0 &
		& + CS1S1S1f131*dZH1H1OS()/2D0 + CS1S1S1f231*dZH2H1OSUsual()/2D0 + CS1S1S1f331*dZH3H1OSUsual()/2D0 &
		& + CS1S1S1f131*dZH1H1OS()/2D0 + CS1S1S1f231*dZH2H1OSUsual()/2D0 + CS1S1S1f331*dZH3H1OSUsual()/2D0 &
		& )
	case (10)
		dRR11 = -CA2*SA1*dAlpha1MSBarAlter() - CA1*SA2*dAlpha2MSBarAlter()
		dRR12 = CA1*CA2*dAlpha1MSBarAlter() - SA1*SA2*dAlpha2MSBarAlter()
		dRR13 = CA2*dAlpha2MSBarAlter()
		dRR21 = -CA1*CA3*dAlpha1MSBarAlter() - CA1*CA3*SA2*dAlpha3MSBarAlter() - CA1*CA2*SA3*dAlpha2MSBarAlter() + &
			& SA1*SA3*dAlpha3MSBarAlter() + SA1*SA2*SA3*dAlpha1MSBarAlter()
		dRR22 = -CA3*SA1*dAlpha1MSBarAlter() - CA3*SA1*SA2*dAlpha3MSBarAlter() - CA1*SA3*dAlpha3MSBarAlter() - &
			& CA2*SA1*SA3*dAlpha2MSBarAlter() - CA1*SA2*SA3*dAlpha1MSBarAlter()
		dRR23 = CA2*CA3*dAlpha3MSBarAlter() - SA2*SA3*dAlpha2MSBarAlter()
		dRR31 = -CA1*CA2*CA3*dAlpha2MSBarAlter() + CA3*SA1*dAlpha3MSBarAlter() + CA3*SA1*SA2*dAlpha1MSBarAlter() + &
			& CA1*SA3*dAlpha1MSBarAlter() + CA1*SA2*SA3*dAlpha3MSBarAlter()
		dRR32 = -CA1*CA3*dAlpha3MSBarAlter() - CA2*CA3*SA1*dAlpha2MSBarAlter() - CA1*CA3*SA2*dAlpha1MSBarAlter() + &
			& SA1*SA3*dAlpha1MSBarAlter() + SA1*SA2*SA3*dAlpha3MSBarAlter()
		dRR33 = -CA3*SA2*dAlpha2MSBarAlter() - CA2*SA3*dAlpha3MSBarAlter()

		totalAmplitude = CS1S1S1f311*( &
&(0.28125D0*CA1*CA3*EL*m12squared*SA2*dAlpha1MSBarAlter())/(CB*MW*SW) + (0.84375D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha1MSBarAlte&
  &r())/(CB*MW*SW) - (0.1875D0*CA3*EL*MH12*SA1*SA2*dAlpha1MSBarAlter())/(CB*MW*SW) - (0.5625D0*CA12*CA3*EL*MH12*SA1*SA2*dAlpha1MS&
  &BarAlter())/(CB*MW*SW) - (0.5625D0*CA22*CA3*EL*MH12*SA1*SA2*dAlpha1MSBarAlter())/(CB*MW*SW) - (1.6875D0*CA12*CA22*CA3*EL*MH12*&
  &SA1*SA2*dAlpha1MSBarAlter())/(CB*MW*SW) - (0.09375D0*CA3*EL*MH32*SA1*SA2*dAlpha1MSBarAlter())/(CB*MW*SW) - (0.28125D0*CA12*CA3&
  &*EL*MH32*SA1*SA2*dAlpha1MSBarAlter())/(CB*MW*SW) - (0.28125D0*CA22*CA3*EL*MH32*SA1*SA2*dAlpha1MSBarAlter())/(CB*MW*SW) - (0.84&
  &375D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*dAlpha1MSBarAlter())/(CB*MW*SW) - (0.125D0*CA1*EL*MH12*SA3*dAlpha1MSBarAlter())/(CB*MW*SW)&
  & - (0.125D0*CA1*CA22*EL*MH12*SA3*dAlpha1MSBarAlter())/(CB*MW*SW) - (0.0625D0*CA1*EL*MH32*SA3*dAlpha1MSBarAlter())/(CB*MW*SW) -&
  & (0.0625D0*CA1*CA22*EL*MH32*SA3*dAlpha1MSBarAlter())/(CB*MW*SW) - (0.1875D0*EL*m12squared*SA1*SA3*dAlpha1MSBarAlter())/(CB*MW*&
  &SW) - (0.1875D0*CA22*EL*m12squared*SA1*SA3*dAlpha1MSBarAlter())/(CB*MW*SW) + (1.125D0*CA1*EL*MH12*SA12*SA3*dAlpha1MSBarAlter()&
  &)/(CB*MW*SW) + (1.125D0*CA1*CA22*EL*MH12*SA12*SA3*dAlpha1MSBarAlter())/(CB*MW*SW) + (0.5625D0*CA1*EL*MH32*SA12*SA3*dAlpha1MSBa&
  &rAlter())/(CB*MW*SW) + (0.5625D0*CA1*CA22*EL*MH32*SA12*SA3*dAlpha1MSBarAlter())/(CB*MW*SW) + (0.125D0*CA1*EL*MH12*SA22*SA3*dAl&
  &pha1MSBarAlter())/(CB*MW*SW) + (0.0625D0*CA1*EL*MH32*SA22*SA3*dAlpha1MSBarAlter())/(CB*MW*SW) + (0.1875D0*EL*m12squared*SA1*SA&
  &22*SA3*dAlpha1MSBarAlter())/(CB*MW*SW) - (1.125D0*CA1*EL*MH12*SA12*SA22*SA3*dAlpha1MSBarAlter())/(CB*MW*SW) - (0.5625D0*CA1*EL&
  &*MH32*SA12*SA22*SA3*dAlpha1MSBarAlter())/(CB*MW*SW) + (0.1875D0*CA1*CA3*EL*MH12*SA2*dAlpha1MSBarAlter())/(MW*SB*SW) + (0.5625D&
  &0*CA1*CA22*CA3*EL*MH12*SA2*dAlpha1MSBarAlter())/(MW*SB*SW) + (0.09375D0*CA1*CA3*EL*MH32*SA2*dAlpha1MSBarAlter())/(MW*SB*SW) + &
  &(0.28125D0*CA1*CA22*CA3*EL*MH32*SA2*dAlpha1MSBarAlter())/(MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*SA1*SA2*dAlpha1MSBarAlter()&
  &)/(MW*SB*SW) - (0.84375D0*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha1MSBarAlter())/(MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA1*SA2&
  &*dAlpha1MSBarAlter())/(CB2*MW*SB*SW) + (0.84375D0*CA12*CA3*EL*m12squared*SA1*SA2*dAlpha1MSBarAlter())/(CB2*MW*SB*SW) + (0.5625&
  &D0*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha1MSBarAlter())/(CB2*MW*SB*SW) + (2.53125D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*dAlpha1&
  &MSBarAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA1*CA3*EL*MH12*SA12*SA2*dAlpha1MSBarAlter())/(MW*SB*SW) + (1.6875D0*CA1*CA22*CA3*EL*&
  &MH12*SA12*SA2*dAlpha1MSBarAlter())/(MW*SB*SW) + (0.28125D0*CA1*CA3*EL*MH32*SA12*SA2*dAlpha1MSBarAlter())/(MW*SB*SW) + (0.84375&
  &D0*CA1*CA22*CA3*EL*MH32*SA12*SA2*dAlpha1MSBarAlter())/(MW*SB*SW) - (0.1875D0*CA1*EL*m12squared*SA3*dAlpha1MSBarAlter())/(MW*SB&
  &*SW) - (0.1875D0*CA1*CA22*EL*m12squared*SA3*dAlpha1MSBarAlter())/(MW*SB*SW) + (0.125D0*CA1*EL*m12squared*SA3*dAlpha1MSBarAlter&
  &())/(CB2*MW*SB*SW) + (0.125D0*CA1*CA22*EL*m12squared*SA3*dAlpha1MSBarAlter())/(CB2*MW*SB*SW) - (0.125D0*EL*MH12*SA1*SA3*dAlpha&
  &1MSBarAlter())/(MW*SB*SW) + (1.125D0*CA12*EL*MH12*SA1*SA3*dAlpha1MSBarAlter())/(MW*SB*SW) - (0.125D0*CA22*EL*MH12*SA1*SA3*dAlp&
  &ha1MSBarAlter())/(MW*SB*SW) + (1.125D0*CA12*CA22*EL*MH12*SA1*SA3*dAlpha1MSBarAlter())/(MW*SB*SW) - (0.0625D0*EL*MH32*SA1*SA3*d&
  &Alpha1MSBarAlter())/(MW*SB*SW) + (0.5625D0*CA12*EL*MH32*SA1*SA3*dAlpha1MSBarAlter())/(MW*SB*SW) - (0.0625D0*CA22*EL*MH32*SA1*S&
  &A3*dAlpha1MSBarAlter())/(MW*SB*SW) + (0.5625D0*CA12*CA22*EL*MH32*SA1*SA3*dAlpha1MSBarAlter())/(MW*SB*SW) - (1.6875D0*CA1*EL*m1&
  &2squared*SA12*SA3*dAlpha1MSBarAlter())/(CB2*MW*SB*SW) - (1.6875D0*CA1*CA22*EL*m12squared*SA12*SA3*dAlpha1MSBarAlter())/(CB2*MW&
  &*SB*SW) + (0.1875D0*CA1*EL*m12squared*SA22*SA3*dAlpha1MSBarAlter())/(MW*SB*SW) - (0.125D0*CA1*EL*m12squared*SA22*SA3*dAlpha1MS&
  &BarAlter())/(CB2*MW*SB*SW) + (0.125D0*EL*MH12*SA1*SA22*SA3*dAlpha1MSBarAlter())/(MW*SB*SW) - (1.125D0*CA12*EL*MH12*SA1*SA22*SA&
  &3*dAlpha1MSBarAlter())/(MW*SB*SW) + (0.0625D0*EL*MH32*SA1*SA22*SA3*dAlpha1MSBarAlter())/(MW*SB*SW) - (0.5625D0*CA12*EL*MH32*SA&
  &1*SA22*SA3*dAlpha1MSBarAlter())/(MW*SB*SW) + (1.6875D0*CA1*EL*m12squared*SA12*SA22*SA3*dAlpha1MSBarAlter())/(CB2*MW*SB*SW) - (&
  &0.1875D0*CA1*CA3*EL*m12squared*SA2*dAlpha1MSBarAlter())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA2*dAlpha1MSBar&
  &Alter())/(CB*MW*SB2*SW) - (0.84375D0*CA1*CA3*EL*m12squared*SA12*SA2*dAlpha1MSBarAlter())/(CB*MW*SB2*SW) - (2.53125D0*CA1*CA22*&
  &CA3*EL*m12squared*SA12*SA2*dAlpha1MSBarAlter())/(CB*MW*SB2*SW) + (0.125D0*EL*m12squared*SA1*SA3*dAlpha1MSBarAlter())/(CB*MW*SB&
  &2*SW) - (1.6875D0*CA12*EL*m12squared*SA1*SA3*dAlpha1MSBarAlter())/(CB*MW*SB2*SW) + (0.125D0*CA22*EL*m12squared*SA1*SA3*dAlpha1&
  &MSBarAlter())/(CB*MW*SB2*SW) - (1.6875D0*CA12*CA22*EL*m12squared*SA1*SA3*dAlpha1MSBarAlter())/(CB*MW*SB2*SW) - (0.125D0*EL*m12&
  &squared*SA1*SA22*SA3*dAlpha1MSBarAlter())/(CB*MW*SB2*SW) + (1.6875D0*CA12*EL*m12squared*SA1*SA22*SA3*dAlpha1MSBarAlter())/(CB*&
  &MW*SB2*SW) - (0.09375D0*CA1*CA3*EL*m12squared*SA2*dAlpha1MSBarAlter())/(MW*SB*SW*TB) - (0.28125D0*CA1*CA22*CA3*EL*m12squared*S&
  &A2*dAlpha1MSBarAlter())/(MW*SB*SW*TB) + (0.0625D0*EL*m12squared*SA1*SA3*dAlpha1MSBarAlter())/(MW*SB*SW*TB) + (0.0625D0*CA22*EL&
  &*m12squared*SA1*SA3*dAlpha1MSBarAlter())/(MW*SB*SW*TB) - (0.0625D0*EL*m12squared*SA1*SA22*SA3*dAlpha1MSBarAlter())/(MW*SB*SW*T&
  &B) + (0.09375D0*CA3*EL*m12squared*SA1*SA2*TB*dAlpha1MSBarAlter())/(CB*MW*SW) + (0.28125D0*CA22*CA3*EL*m12squared*SA1*SA2*TB*dA&
  &lpha1MSBarAlter())/(CB*MW*SW) + (0.0625D0*CA1*EL*m12squared*SA3*TB*dAlpha1MSBarAlter())/(CB*MW*SW) + (0.0625D0*CA1*CA22*EL*m12&
  &squared*SA3*TB*dAlpha1MSBarAlter())/(CB*MW*SW) - (0.0625D0*CA1*EL*m12squared*SA22*SA3*TB*dAlpha1MSBarAlter())/(CB*MW*SW) + (0.&
  &1875D0*CA1*CA2*CA3*EL*MH12*dAlpha2MSBarAlter())/(CB*MW*SW) + (0.09375D0*CA1*CA2*CA3*EL*MH32*dAlpha2MSBarAlter())/(CB*MW*SW) + &
  &(0.28125D0*CA2*CA3*EL*m12squared*SA1*dAlpha2MSBarAlter())/(CB*MW*SW) - (0.1875D0*CA1*CA2*CA3*EL*MH12*SA12*dAlpha2MSBarAlter())&
  &/(CB*MW*SW) - (0.09375D0*CA1*CA2*CA3*EL*MH32*SA12*dAlpha2MSBarAlter())/(CB*MW*SW) - (1.6875D0*CA1*CA2*CA3*EL*MH12*SA22*dAlpha2&
  &MSBarAlter())/(CB*MW*SW) - (0.84375D0*CA1*CA2*CA3*EL*MH32*SA22*dAlpha2MSBarAlter())/(CB*MW*SW) - (2.53125D0*CA2*CA3*EL*m12squa&
  &red*SA1*SA22*dAlpha2MSBarAlter())/(CB*MW*SW) + (1.6875D0*CA1*CA2*CA3*EL*MH12*SA12*SA22*dAlpha2MSBarAlter())/(CB*MW*SW) + (0.84&
  &375D0*CA1*CA2*CA3*EL*MH32*SA12*SA22*dAlpha2MSBarAlter())/(CB*MW*SW) - (0.75D0*CA1*CA2*EL*m12squared*SA2*SA3*dAlpha2MSBarAlter(&
  &))/(CB*MW*SW) + (0.5D0*CA2*EL*MH12*SA1*SA2*SA3*dAlpha2MSBarAlter())/(CB*MW*SW) + (1.5D0*CA12*CA2*EL*MH12*SA1*SA2*SA3*dAlpha2MS&
  &BarAlter())/(CB*MW*SW) + (0.25D0*CA2*EL*MH32*SA1*SA2*SA3*dAlpha2MSBarAlter())/(CB*MW*SW) + (0.75D0*CA12*CA2*EL*MH32*SA1*SA2*SA&
  &3*dAlpha2MSBarAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA2*CA3*EL*m12squared*dAlpha2MSBarAlter())/(MW*SB*SW) - (0.1875D0*CA1*CA2*C&
  &A3*EL*m12squared*dAlpha2MSBarAlter())/(CB2*MW*SB*SW) + (0.1875D0*CA2*CA3*EL*MH12*SA1*dAlpha2MSBarAlter())/(MW*SB*SW) - (0.1875&
  &D0*CA12*CA2*CA3*EL*MH12*SA1*dAlpha2MSBarAlter())/(MW*SB*SW) + (0.09375D0*CA2*CA3*EL*MH32*SA1*dAlpha2MSBarAlter())/(MW*SB*SW) -&
  & (0.09375D0*CA12*CA2*CA3*EL*MH32*SA1*dAlpha2MSBarAlter())/(MW*SB*SW) + (0.28125D0*CA1*CA2*CA3*EL*m12squared*SA12*dAlpha2MSBarA&
  &lter())/(CB2*MW*SB*SW) - (2.53125D0*CA1*CA2*CA3*EL*m12squared*SA22*dAlpha2MSBarAlter())/(MW*SB*SW) + (1.6875D0*CA1*CA2*CA3*EL*&
  &m12squared*SA22*dAlpha2MSBarAlter())/(CB2*MW*SB*SW) - (1.6875D0*CA2*CA3*EL*MH12*SA1*SA22*dAlpha2MSBarAlter())/(MW*SB*SW) + (1.&
  &6875D0*CA12*CA2*CA3*EL*MH12*SA1*SA22*dAlpha2MSBarAlter())/(MW*SB*SW) - (0.84375D0*CA2*CA3*EL*MH32*SA1*SA22*dAlpha2MSBarAlter()&
  &)/(MW*SB*SW) + (0.84375D0*CA12*CA2*CA3*EL*MH32*SA1*SA22*dAlpha2MSBarAlter())/(MW*SB*SW) - (2.53125D0*CA1*CA2*CA3*EL*m12squared&
  &*SA12*SA22*dAlpha2MSBarAlter())/(CB2*MW*SB*SW) - (0.5D0*CA1*CA2*EL*MH12*SA2*SA3*dAlpha2MSBarAlter())/(MW*SB*SW) - (0.25D0*CA1*&
  &CA2*EL*MH32*SA2*SA3*dAlpha2MSBarAlter())/(MW*SB*SW) + (0.75D0*CA2*EL*m12squared*SA1*SA2*SA3*dAlpha2MSBarAlter())/(MW*SB*SW) - &
  &(0.5D0*CA2*EL*m12squared*SA1*SA2*SA3*dAlpha2MSBarAlter())/(CB2*MW*SB*SW) - (2.25D0*CA12*CA2*EL*m12squared*SA1*SA2*SA3*dAlpha2M&
  &SBarAlter())/(CB2*MW*SB*SW) - (1.5D0*CA1*CA2*EL*MH12*SA12*SA2*SA3*dAlpha2MSBarAlter())/(MW*SB*SW) - (0.75D0*CA1*CA2*EL*MH32*SA&
  &12*SA2*SA3*dAlpha2MSBarAlter())/(MW*SB*SW) - (0.1875D0*CA2*CA3*EL*m12squared*SA1*dAlpha2MSBarAlter())/(CB*MW*SB2*SW) + (0.2812&
  &5D0*CA12*CA2*CA3*EL*m12squared*SA1*dAlpha2MSBarAlter())/(CB*MW*SB2*SW) + (1.6875D0*CA2*CA3*EL*m12squared*SA1*SA22*dAlpha2MSBar&
  &Alter())/(CB*MW*SB2*SW) - (2.53125D0*CA12*CA2*CA3*EL*m12squared*SA1*SA22*dAlpha2MSBarAlter())/(CB*MW*SB2*SW) + (0.5D0*CA1*CA2*&
  &EL*m12squared*SA2*SA3*dAlpha2MSBarAlter())/(CB*MW*SB2*SW) + (2.25D0*CA1*CA2*EL*m12squared*SA12*SA2*SA3*dAlpha2MSBarAlter())/(C&
  &B*MW*SB2*SW) - (0.09375D0*CA2*CA3*EL*m12squared*SA1*dAlpha2MSBarAlter())/(MW*SB*SW*TB) + (0.84375D0*CA2*CA3*EL*m12squared*SA1*&
  &SA22*dAlpha2MSBarAlter())/(MW*SB*SW*TB) + (0.25D0*CA1*CA2*EL*m12squared*SA2*SA3*dAlpha2MSBarAlter())/(MW*SB*SW*TB) - (0.09375D&
  &0*CA1*CA2*CA3*EL*m12squared*TB*dAlpha2MSBarAlter())/(CB*MW*SW) + (0.84375D0*CA1*CA2*CA3*EL*m12squared*SA22*TB*dAlpha2MSBarAlte&
  &r())/(CB*MW*SW) - (0.25D0*CA2*EL*m12squared*SA1*SA2*SA3*TB*dAlpha2MSBarAlter())/(CB*MW*SW) + (0.5D0*CA3*MH12*SA2*dAlpha2MSBarA&
  &lter())/vS - (4.5D0*CA22*CA3*MH12*SA2*dAlpha2MSBarAlter())/vS + (0.25D0*CA3*MH32*SA2*dAlpha2MSBarAlter())/vS - (2.25D0*CA22*CA&
  &3*MH32*SA2*dAlpha2MSBarAlter())/vS + (0.1875D0*CA1*CA3*EL*m12squared*dAlpha3MSBarAlter())/(CB*MW*SW) + (0.1875D0*CA1*CA22*CA3*&
  &EL*m12squared*dAlpha3MSBarAlter())/(CB*MW*SW) - (0.125D0*CA3*EL*MH12*SA1*dAlpha3MSBarAlter())/(CB*MW*SW) - (0.375D0*CA12*CA3*E&
  &L*MH12*SA1*dAlpha3MSBarAlter())/(CB*MW*SW) - (0.125D0*CA22*CA3*EL*MH12*SA1*dAlpha3MSBarAlter())/(CB*MW*SW) - (0.375D0*CA12*CA2&
  &2*CA3*EL*MH12*SA1*dAlpha3MSBarAlter())/(CB*MW*SW) - (0.0625D0*CA3*EL*MH32*SA1*dAlpha3MSBarAlter())/(CB*MW*SW) - (0.1875D0*CA12&
  &*CA3*EL*MH32*SA1*dAlpha3MSBarAlter())/(CB*MW*SW) - (0.0625D0*CA22*CA3*EL*MH32*SA1*dAlpha3MSBarAlter())/(CB*MW*SW) - (0.1875D0*&
  &CA12*CA22*CA3*EL*MH32*SA1*dAlpha3MSBarAlter())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*m12squared*SA22*dAlpha3MSBarAlter())/(CB*MW*S&
  &W) + (0.125D0*CA3*EL*MH12*SA1*SA22*dAlpha3MSBarAlter())/(CB*MW*SW) + (0.375D0*CA12*CA3*EL*MH12*SA1*SA22*dAlpha3MSBarAlter())/(&
  &CB*MW*SW) + (0.0625D0*CA3*EL*MH32*SA1*SA22*dAlpha3MSBarAlter())/(CB*MW*SW) + (0.1875D0*CA12*CA3*EL*MH32*SA1*SA22*dAlpha3MSBarA&
  &lter())/(CB*MW*SW) - (0.1875D0*CA1*EL*MH12*SA2*SA3*dAlpha3MSBarAlter())/(CB*MW*SW) - (0.5625D0*CA1*CA22*EL*MH12*SA2*SA3*dAlpha&
  &3MSBarAlter())/(CB*MW*SW) - (0.09375D0*CA1*EL*MH32*SA2*SA3*dAlpha3MSBarAlter())/(CB*MW*SW) - (0.28125D0*CA1*CA22*EL*MH32*SA2*S&
  &A3*dAlpha3MSBarAlter())/(CB*MW*SW) - (0.28125D0*EL*m12squared*SA1*SA2*SA3*dAlpha3MSBarAlter())/(CB*MW*SW) - (0.84375D0*CA22*EL&
  &*m12squared*SA1*SA2*SA3*dAlpha3MSBarAlter())/(CB*MW*SW) + (0.1875D0*CA1*EL*MH12*SA12*SA2*SA3*dAlpha3MSBarAlter())/(CB*MW*SW) +&
  & (0.5625D0*CA1*CA22*EL*MH12*SA12*SA2*SA3*dAlpha3MSBarAlter())/(CB*MW*SW) + (0.09375D0*CA1*EL*MH32*SA12*SA2*SA3*dAlpha3MSBarAlt&
  &er())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*MH32*SA12*SA2*SA3*dAlpha3MSBarAlter())/(CB*MW*SW) + (0.125D0*CA1*CA3*EL*MH12*dAlpha3&
  &MSBarAlter())/(MW*SB*SW) + (0.125D0*CA1*CA22*CA3*EL*MH12*dAlpha3MSBarAlter())/(MW*SB*SW) + (0.0625D0*CA1*CA3*EL*MH32*dAlpha3MS&
  &BarAlter())/(MW*SB*SW) + (0.0625D0*CA1*CA22*CA3*EL*MH32*dAlpha3MSBarAlter())/(MW*SB*SW) - (0.1875D0*CA3*EL*m12squared*SA1*dAlp&
  &ha3MSBarAlter())/(MW*SB*SW) - (0.1875D0*CA22*CA3*EL*m12squared*SA1*dAlpha3MSBarAlter())/(MW*SB*SW) + (0.125D0*CA3*EL*m12square&
  &d*SA1*dAlpha3MSBarAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA12*CA3*EL*m12squared*SA1*dAlpha3MSBarAlter())/(CB2*MW*SB*SW) + (0.125D&
  &0*CA22*CA3*EL*m12squared*SA1*dAlpha3MSBarAlter())/(CB2*MW*SB*SW) + (0.5625D0*CA12*CA22*CA3*EL*m12squared*SA1*dAlpha3MSBarAlter&
  &())/(CB2*MW*SB*SW) + (0.375D0*CA1*CA3*EL*MH12*SA12*dAlpha3MSBarAlter())/(MW*SB*SW) + (0.375D0*CA1*CA22*CA3*EL*MH12*SA12*dAlpha&
  &3MSBarAlter())/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*MH32*SA12*dAlpha3MSBarAlter())/(MW*SB*SW) + (0.1875D0*CA1*CA22*CA3*EL*MH32*SA&
  &12*dAlpha3MSBarAlter())/(MW*SB*SW) - (0.125D0*CA1*CA3*EL*MH12*SA22*dAlpha3MSBarAlter())/(MW*SB*SW) - (0.0625D0*CA1*CA3*EL*MH32&
  &*SA22*dAlpha3MSBarAlter())/(MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA1*SA22*dAlpha3MSBarAlter())/(MW*SB*SW) - (0.125D0*CA3*EL&
  &*m12squared*SA1*SA22*dAlpha3MSBarAlter())/(CB2*MW*SB*SW) - (0.5625D0*CA12*CA3*EL*m12squared*SA1*SA22*dAlpha3MSBarAlter())/(CB2&
  &*MW*SB*SW) - (0.375D0*CA1*CA3*EL*MH12*SA12*SA22*dAlpha3MSBarAlter())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*MH32*SA12*SA22*dAlpha3M&
  &SBarAlter())/(MW*SB*SW) - (0.28125D0*CA1*EL*m12squared*SA2*SA3*dAlpha3MSBarAlter())/(MW*SB*SW) - (0.84375D0*CA1*CA22*EL*m12squ&
  &ared*SA2*SA3*dAlpha3MSBarAlter())/(MW*SB*SW) + (0.1875D0*CA1*EL*m12squared*SA2*SA3*dAlpha3MSBarAlter())/(CB2*MW*SB*SW) + (0.56&
  &25D0*CA1*CA22*EL*m12squared*SA2*SA3*dAlpha3MSBarAlter())/(CB2*MW*SB*SW) - (0.1875D0*EL*MH12*SA1*SA2*SA3*dAlpha3MSBarAlter())/(&
  &MW*SB*SW) + (0.1875D0*CA12*EL*MH12*SA1*SA2*SA3*dAlpha3MSBarAlter())/(MW*SB*SW) - (0.5625D0*CA22*EL*MH12*SA1*SA2*SA3*dAlpha3MSB&
  &arAlter())/(MW*SB*SW) + (0.5625D0*CA12*CA22*EL*MH12*SA1*SA2*SA3*dAlpha3MSBarAlter())/(MW*SB*SW) - (0.09375D0*EL*MH32*SA1*SA2*S&
  &A3*dAlpha3MSBarAlter())/(MW*SB*SW) + (0.09375D0*CA12*EL*MH32*SA1*SA2*SA3*dAlpha3MSBarAlter())/(MW*SB*SW) - (0.28125D0*CA22*EL*&
  &MH32*SA1*SA2*SA3*dAlpha3MSBarAlter())/(MW*SB*SW) + (0.28125D0*CA12*CA22*EL*MH32*SA1*SA2*SA3*dAlpha3MSBarAlter())/(MW*SB*SW) - &
  &(0.28125D0*CA1*EL*m12squared*SA12*SA2*SA3*dAlpha3MSBarAlter())/(CB2*MW*SB*SW) - (0.84375D0*CA1*CA22*EL*m12squared*SA12*SA2*SA3&
  &*dAlpha3MSBarAlter())/(CB2*MW*SB*SW) - (0.125D0*CA1*CA3*EL*m12squared*dAlpha3MSBarAlter())/(CB*MW*SB2*SW) - (0.125D0*CA1*CA22*&
  &CA3*EL*m12squared*dAlpha3MSBarAlter())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA3*EL*m12squared*SA12*dAlpha3MSBarAlter())/(CB*MW*SB2*S&
  &W) - (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA12*dAlpha3MSBarAlter())/(CB*MW*SB2*SW) + (0.125D0*CA1*CA3*EL*m12squared*SA22*dAlph&
  &a3MSBarAlter())/(CB*MW*SB2*SW) + (0.5625D0*CA1*CA3*EL*m12squared*SA12*SA22*dAlpha3MSBarAlter())/(CB*MW*SB2*SW) + (0.1875D0*EL*&
  &m12squared*SA1*SA2*SA3*dAlpha3MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA12*EL*m12squared*SA1*SA2*SA3*dAlpha3MSBarAlter())/(C&
  &B*MW*SB2*SW) + (0.5625D0*CA22*EL*m12squared*SA1*SA2*SA3*dAlpha3MSBarAlter())/(CB*MW*SB2*SW) - (0.84375D0*CA12*CA22*EL*m12squar&
  &ed*SA1*SA2*SA3*dAlpha3MSBarAlter())/(CB*MW*SB2*SW) - (0.0625D0*CA1*CA3*EL*m12squared*dAlpha3MSBarAlter())/(MW*SB*SW*TB) - (0.0&
  &625D0*CA1*CA22*CA3*EL*m12squared*dAlpha3MSBarAlter())/(MW*SB*SW*TB) + (0.0625D0*CA1*CA3*EL*m12squared*SA22*dAlpha3MSBarAlter()&
  &)/(MW*SB*SW*TB) + (0.09375D0*EL*m12squared*SA1*SA2*SA3*dAlpha3MSBarAlter())/(MW*SB*SW*TB) + (0.28125D0*CA22*EL*m12squared*SA1*&
  &SA2*SA3*dAlpha3MSBarAlter())/(MW*SB*SW*TB) + (0.0625D0*CA3*EL*m12squared*SA1*TB*dAlpha3MSBarAlter())/(CB*MW*SW) + (0.0625D0*CA&
  &22*CA3*EL*m12squared*SA1*TB*dAlpha3MSBarAlter())/(CB*MW*SW) - (0.0625D0*CA3*EL*m12squared*SA1*SA22*TB*dAlpha3MSBarAlter())/(CB&
  &*MW*SW) + (0.09375D0*CA1*EL*m12squared*SA2*SA3*TB*dAlpha3MSBarAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA22*EL*m12squared*SA2*SA3*&
  &TB*dAlpha3MSBarAlter())/(CB*MW*SW) + (0.5D0*CA2*MH12*SA3*dAlpha3MSBarAlter())/vS + (0.25D0*CA2*MH32*SA3*dAlpha3MSBarAlter())/v&
  &S + (1.5D0*CA2*MH12*SA22*SA3*dAlpha3MSBarAlter())/vS + (0.75D0*CA2*MH32*SA22*SA3*dAlpha3MSBarAlter())/vS + (0.09375D0*CA3*EL*M&
  &H12*SA1*SA2*dBetaMSBarAlter())/(CB*MW*SW) - (0.09375D0*CA12*CA3*EL*MH12*SA1*SA2*dBetaMSBarAlter())/(CB*MW*SW) + (0.28125D0*CA2&
  &2*CA3*EL*MH12*SA1*SA2*dBetaMSBarAlter())/(CB*MW*SW) - (0.28125D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dBetaMSBarAlter())/(CB*MW*SW) +&
  & (0.046875D0*CA3*EL*MH32*SA1*SA2*dBetaMSBarAlter())/(CB*MW*SW) - (0.046875D0*CA12*CA3*EL*MH32*SA1*SA2*dBetaMSBarAlter())/(CB*M&
  &W*SW) + (0.140625D0*CA22*CA3*EL*MH32*SA1*SA2*dBetaMSBarAlter())/(CB*MW*SW) - (0.140625D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*dBetaMS&
  &BarAlter())/(CB*MW*SW) + (0.0625D0*CA1*EL*MH12*SA3*dBetaMSBarAlter())/(CB*MW*SW) + (0.0625D0*CA1*CA22*EL*MH12*SA3*dBetaMSBarAl&
  &ter())/(CB*MW*SW) + (0.03125D0*CA1*EL*MH32*SA3*dBetaMSBarAlter())/(CB*MW*SW) + (0.03125D0*CA1*CA22*EL*MH32*SA3*dBetaMSBarAlter&
  &())/(CB*MW*SW) + (0.1875D0*CA1*EL*MH12*SA12*SA3*dBetaMSBarAlter())/(CB*MW*SW) + (0.1875D0*CA1*CA22*EL*MH12*SA12*SA3*dBetaMSBar&
  &Alter())/(CB*MW*SW) + (0.09375D0*CA1*EL*MH32*SA12*SA3*dBetaMSBarAlter())/(CB*MW*SW) + (0.09375D0*CA1*CA22*EL*MH32*SA12*SA3*dBe&
  &taMSBarAlter())/(CB*MW*SW) - (0.0625D0*CA1*EL*MH12*SA22*SA3*dBetaMSBarAlter())/(CB*MW*SW) - (0.03125D0*CA1*EL*MH32*SA22*SA3*dB&
  &etaMSBarAlter())/(CB*MW*SW) - (0.1875D0*CA1*EL*MH12*SA12*SA22*SA3*dBetaMSBarAlter())/(CB*MW*SW) - (0.09375D0*CA1*EL*MH32*SA12*&
  &SA22*SA3*dBetaMSBarAlter())/(CB*MW*SW) + (0.09375D0*CA3*EL*m12squared*SA1*SA2*dBetaMSBarAlter())/(MW*SB*SW) + (0.28125D0*CA22*&
  &CA3*EL*m12squared*SA1*SA2*dBetaMSBarAlter())/(MW*SB*SW) - (0.328125D0*CA3*EL*m12squared*SA1*SA2*dBetaMSBarAlter())/(CB2*MW*SB*&
  &SW) + (0.421875D0*CA12*CA3*EL*m12squared*SA1*SA2*dBetaMSBarAlter())/(CB2*MW*SB*SW) - (0.984375D0*CA22*CA3*EL*m12squared*SA1*SA&
  &2*dBetaMSBarAlter())/(CB2*MW*SB*SW) + (1.265625D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*dBetaMSBarAlter())/(CB2*MW*SB*SW) + (0.0&
  &625D0*CA1*EL*m12squared*SA3*dBetaMSBarAlter())/(MW*SB*SW) + (0.0625D0*CA1*CA22*EL*m12squared*SA3*dBetaMSBarAlter())/(MW*SB*SW)&
  & - (0.21875D0*CA1*EL*m12squared*SA3*dBetaMSBarAlter())/(CB2*MW*SB*SW) - (0.21875D0*CA1*CA22*EL*m12squared*SA3*dBetaMSBarAlter(&
  &))/(CB2*MW*SB*SW) - (0.84375D0*CA1*EL*m12squared*SA12*SA3*dBetaMSBarAlter())/(CB2*MW*SB*SW) - (0.84375D0*CA1*CA22*EL*m12square&
  &d*SA12*SA3*dBetaMSBarAlter())/(CB2*MW*SB*SW) - (0.0625D0*CA1*EL*m12squared*SA22*SA3*dBetaMSBarAlter())/(MW*SB*SW) + (0.21875D0&
  &*CA1*EL*m12squared*SA22*SA3*dBetaMSBarAlter())/(CB2*MW*SB*SW) + (0.84375D0*CA1*EL*m12squared*SA12*SA22*SA3*dBetaMSBarAlter())/&
  &(CB2*MW*SB*SW) + (0.09375D0*CA1*CA3*EL*m12squared*SA2*dBetaMSBarAlter())/(CB*MW*SB2*SW) + (0.28125D0*CA1*CA22*CA3*EL*m12square&
  &d*SA2*dBetaMSBarAlter())/(CB*MW*SB2*SW) - (0.09375D0*CA3*EL*MH12*SA1*SA2*dBetaMSBarAlter())/(CB*MW*SB2*SW) + (0.09375D0*CA12*C&
  &A3*EL*MH12*SA1*SA2*dBetaMSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA22*CA3*EL*MH12*SA1*SA2*dBetaMSBarAlter())/(CB*MW*SB2*SW) +&
  & (0.28125D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dBetaMSBarAlter())/(CB*MW*SB2*SW) - (0.046875D0*CA3*EL*MH32*SA1*SA2*dBetaMSBarAlter(&
  &))/(CB*MW*SB2*SW) + (0.046875D0*CA12*CA3*EL*MH32*SA1*SA2*dBetaMSBarAlter())/(CB*MW*SB2*SW) - (0.140625D0*CA22*CA3*EL*MH32*SA1*&
  &SA2*dBetaMSBarAlter())/(CB*MW*SB2*SW) + (0.140625D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*dBetaMSBarAlter())/(CB*MW*SB2*SW) - (0.28125&
  &D0*CA1*CA3*EL*m12squared*SA12*SA2*dBetaMSBarAlter())/(CB*MW*SB2*SW) - (0.84375D0*CA1*CA22*CA3*EL*m12squared*SA12*SA2*dBetaMSBa&
  &rAlter())/(CB*MW*SB2*SW) - (0.0625D0*CA1*EL*MH12*SA3*dBetaMSBarAlter())/(CB*MW*SB2*SW) - (0.0625D0*CA1*CA22*EL*MH12*SA3*dBetaM&
  &SBarAlter())/(CB*MW*SB2*SW) - (0.03125D0*CA1*EL*MH32*SA3*dBetaMSBarAlter())/(CB*MW*SB2*SW) - (0.03125D0*CA1*CA22*EL*MH32*SA3*d&
  &BetaMSBarAlter())/(CB*MW*SB2*SW) - (0.0625D0*EL*m12squared*SA1*SA3*dBetaMSBarAlter())/(CB*MW*SB2*SW) - (0.5625D0*CA12*EL*m12sq&
  &uared*SA1*SA3*dBetaMSBarAlter())/(CB*MW*SB2*SW) - (0.0625D0*CA22*EL*m12squared*SA1*SA3*dBetaMSBarAlter())/(CB*MW*SB2*SW) - (0.&
  &5625D0*CA12*CA22*EL*m12squared*SA1*SA3*dBetaMSBarAlter())/(CB*MW*SB2*SW) - (0.1875D0*CA1*EL*MH12*SA12*SA3*dBetaMSBarAlter())/(&
  &CB*MW*SB2*SW) - (0.1875D0*CA1*CA22*EL*MH12*SA12*SA3*dBetaMSBarAlter())/(CB*MW*SB2*SW) - (0.09375D0*CA1*EL*MH32*SA12*SA3*dBetaM&
  &SBarAlter())/(CB*MW*SB2*SW) - (0.09375D0*CA1*CA22*EL*MH32*SA12*SA3*dBetaMSBarAlter())/(CB*MW*SB2*SW) + (0.0625D0*CA1*EL*MH12*S&
  &A22*SA3*dBetaMSBarAlter())/(CB*MW*SB2*SW) + (0.03125D0*CA1*EL*MH32*SA22*SA3*dBetaMSBarAlter())/(CB*MW*SB2*SW) + (0.0625D0*EL*m&
  &12squared*SA1*SA22*SA3*dBetaMSBarAlter())/(CB*MW*SB2*SW) + (0.5625D0*CA12*EL*m12squared*SA1*SA22*SA3*dBetaMSBarAlter())/(CB*MW&
  &*SB2*SW) + (0.1875D0*CA1*EL*MH12*SA12*SA22*SA3*dBetaMSBarAlter())/(CB*MW*SB2*SW) + (0.09375D0*CA1*EL*MH32*SA12*SA22*SA3*dBetaM&
  &SBarAlter())/(CB*MW*SB2*SW) - (0.1875D0*CA1*CA3*EL*m12squared*SA2*dBetaMSBarAlter())/(MW*SB*SW*TB) - (0.5625D0*CA1*CA22*CA3*EL&
  &*m12squared*SA2*dBetaMSBarAlter())/(MW*SB*SW*TB) - (0.09375D0*CA3*EL*MH12*SA1*SA2*dBetaMSBarAlter())/(MW*SB*SW*TB) + (0.09375D&
  &0*CA12*CA3*EL*MH12*SA1*SA2*dBetaMSBarAlter())/(MW*SB*SW*TB) - (0.28125D0*CA22*CA3*EL*MH12*SA1*SA2*dBetaMSBarAlter())/(MW*SB*SW&
  &*TB) + (0.28125D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*dBetaMSBarAlter())/(MW*SB*SW*TB) - (0.046875D0*CA3*EL*MH32*SA1*SA2*dBetaMSBarA&
  &lter())/(MW*SB*SW*TB) + (0.046875D0*CA12*CA3*EL*MH32*SA1*SA2*dBetaMSBarAlter())/(MW*SB*SW*TB) - (0.140625D0*CA22*CA3*EL*MH32*S&
  &A1*SA2*dBetaMSBarAlter())/(MW*SB*SW*TB) + (0.140625D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*dBetaMSBarAlter())/(MW*SB*SW*TB) - (0.0625&
  &D0*CA1*EL*MH12*SA3*dBetaMSBarAlter())/(MW*SB*SW*TB) - (0.0625D0*CA1*CA22*EL*MH12*SA3*dBetaMSBarAlter())/(MW*SB*SW*TB) - (0.031&
  &25D0*CA1*EL*MH32*SA3*dBetaMSBarAlter())/(MW*SB*SW*TB) - (0.03125D0*CA1*CA22*EL*MH32*SA3*dBetaMSBarAlter())/(MW*SB*SW*TB) + (0.&
  &125D0*EL*m12squared*SA1*SA3*dBetaMSBarAlter())/(MW*SB*SW*TB) + (0.125D0*CA22*EL*m12squared*SA1*SA3*dBetaMSBarAlter())/(MW*SB*S&
  &W*TB) - (0.1875D0*CA1*EL*MH12*SA12*SA3*dBetaMSBarAlter())/(MW*SB*SW*TB) - (0.1875D0*CA1*CA22*EL*MH12*SA12*SA3*dBetaMSBarAlter(&
  &))/(MW*SB*SW*TB) - (0.09375D0*CA1*EL*MH32*SA12*SA3*dBetaMSBarAlter())/(MW*SB*SW*TB) - (0.09375D0*CA1*CA22*EL*MH32*SA12*SA3*dBe&
  &taMSBarAlter())/(MW*SB*SW*TB) + (0.0625D0*CA1*EL*MH12*SA22*SA3*dBetaMSBarAlter())/(MW*SB*SW*TB) + (0.03125D0*CA1*EL*MH32*SA22*&
  &SA3*dBetaMSBarAlter())/(MW*SB*SW*TB) - (0.125D0*EL*m12squared*SA1*SA22*SA3*dBetaMSBarAlter())/(MW*SB*SW*TB) + (0.1875D0*CA1*EL&
  &*MH12*SA12*SA22*SA3*dBetaMSBarAlter())/(MW*SB*SW*TB) + (0.09375D0*CA1*EL*MH32*SA12*SA22*SA3*dBetaMSBarAlter())/(MW*SB*SW*TB) +&
  & (0.1875D0*CA1*CA3*EL*MH12*SA2*TB*dBetaMSBarAlter())/(CB*MW*SW) + (0.5625D0*CA1*CA22*CA3*EL*MH12*SA2*TB*dBetaMSBarAlter())/(CB&
  &*MW*SW) + (0.09375D0*CA1*CA3*EL*MH32*SA2*TB*dBetaMSBarAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA22*CA3*EL*MH32*SA2*TB*dBetaMSBarA&
  &lter())/(CB*MW*SW) + (0.328125D0*CA3*EL*m12squared*SA1*SA2*TB*dBetaMSBarAlter())/(CB*MW*SW) + (0.984375D0*CA22*CA3*EL*m12squar&
  &ed*SA1*SA2*TB*dBetaMSBarAlter())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*MH12*SA12*SA2*TB*dBetaMSBarAlter())/(CB*MW*SW) - (0.5625D0*&
  &CA1*CA22*CA3*EL*MH12*SA12*SA2*TB*dBetaMSBarAlter())/(CB*MW*SW) - (0.09375D0*CA1*CA3*EL*MH32*SA12*SA2*TB*dBetaMSBarAlter())/(CB&
  &*MW*SW) - (0.28125D0*CA1*CA22*CA3*EL*MH32*SA12*SA2*TB*dBetaMSBarAlter())/(CB*MW*SW) + (0.21875D0*CA1*EL*m12squared*SA3*TB*dBet&
  &aMSBarAlter())/(CB*MW*SW) + (0.21875D0*CA1*CA22*EL*m12squared*SA3*TB*dBetaMSBarAlter())/(CB*MW*SW) - (0.125D0*EL*MH12*SA1*SA3*&
  &TB*dBetaMSBarAlter())/(CB*MW*SW) - (0.375D0*CA12*EL*MH12*SA1*SA3*TB*dBetaMSBarAlter())/(CB*MW*SW) - (0.125D0*CA22*EL*MH12*SA1*&
  &SA3*TB*dBetaMSBarAlter())/(CB*MW*SW) - (0.375D0*CA12*CA22*EL*MH12*SA1*SA3*TB*dBetaMSBarAlter())/(CB*MW*SW) - (0.0625D0*EL*MH32&
  &*SA1*SA3*TB*dBetaMSBarAlter())/(CB*MW*SW) - (0.1875D0*CA12*EL*MH32*SA1*SA3*TB*dBetaMSBarAlter())/(CB*MW*SW) - (0.0625D0*CA22*E&
  &L*MH32*SA1*SA3*TB*dBetaMSBarAlter())/(CB*MW*SW) - (0.1875D0*CA12*CA22*EL*MH32*SA1*SA3*TB*dBetaMSBarAlter())/(CB*MW*SW) - (0.21&
  &875D0*CA1*EL*m12squared*SA22*SA3*TB*dBetaMSBarAlter())/(CB*MW*SW) + (0.125D0*EL*MH12*SA1*SA22*SA3*TB*dBetaMSBarAlter())/(CB*MW&
  &*SW) + (0.375D0*CA12*EL*MH12*SA1*SA22*SA3*TB*dBetaMSBarAlter())/(CB*MW*SW) + (0.0625D0*EL*MH32*SA1*SA22*SA3*TB*dBetaMSBarAlter&
  &())/(CB*MW*SW) + (0.1875D0*CA12*EL*MH32*SA1*SA22*SA3*TB*dBetaMSBarAlter())/(CB*MW*SW) + (0.140625D0*CA3*EL*m12squared*SA1*SA2*&
  &dBetaMSBarAlter())/(MW*SB*SW*TB2) + (0.421875D0*CA22*CA3*EL*m12squared*SA1*SA2*dBetaMSBarAlter())/(MW*SB*SW*TB2) + (0.09375D0*&
  &CA1*EL*m12squared*SA3*dBetaMSBarAlter())/(MW*SB*SW*TB2) + (0.09375D0*CA1*CA22*EL*m12squared*SA3*dBetaMSBarAlter())/(MW*SB*SW*T&
  &B2) - (0.09375D0*CA1*EL*m12squared*SA22*SA3*dBetaMSBarAlter())/(MW*SB*SW*TB2) - (0.1875D0*CA1*CA3*EL*m12squared*SA2*TB2*dBetaM&
  &SBarAlter())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*EL*m12squared*SA2*TB2*dBetaMSBarAlter())/(CB*MW*SW) + (0.125D0*EL*m12squared*&
  &SA1*SA3*TB2*dBetaMSBarAlter())/(CB*MW*SW) + (0.125D0*CA22*EL*m12squared*SA1*SA3*TB2*dBetaMSBarAlter())/(CB*MW*SW) - (0.125D0*E&
  &L*m12squared*SA1*SA22*SA3*TB2*dBetaMSBarAlter())/(CB*MW*SW) + (0.125D0*CA2*CA3*MH12*dBetaMSBarAlter())/(CB*SB*vS) + (0.0625D0*&
  &CA2*CA3*MH32*dBetaMSBarAlter())/(CB*SB*vS) + (0.375D0*CA2*CA3*MH12*SA22*dBetaMSBarAlter())/(CB*SB*vS) + (0.1875D0*CA2*CA3*MH32&
  &*SA22*dBetaMSBarAlter())/(CB*SB*vS) - (0.125D0*CA2*CA3*MH12*dBetaMSBarAlter())/(TB*vS) - (0.0625D0*CA2*CA3*MH32*dBetaMSBarAlte&
  &r())/(TB*vS) - (0.375D0*CA2*CA3*MH12*SA22*dBetaMSBarAlter())/(TB*vS) - (0.1875D0*CA2*CA3*MH32*SA22*dBetaMSBarAlter())/(TB*vS) &
  &- (0.125D0*CA2*CA3*MH12*TB*dBetaMSBarAlter())/vS - (0.0625D0*CA2*CA3*MH32*TB*dBetaMSBarAlter())/vS - (0.375D0*CA2*CA3*MH12*SA2&
  &2*TB*dBetaMSBarAlter())/vS - (0.1875D0*CA2*CA3*MH32*SA22*TB*dBetaMSBarAlter())/vS - (0.375D0*EL*MH12*SA3*dAlpha1MSBarAlter()*D&
  &BLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.375D0*CA22*EL*MH12*SA3*dAlpha1MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*E&
  &L*MH32*SA3*dAlpha1MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA22*EL*MH32*SA3*dAlpha1MSBarAlter()*DBLE(CA1**INT&
  &(3.D0)))/(CB*MW*SW) + (0.375D0*EL*MH12*SA22*SA3*dAlpha1MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*EL*MH32*SA22*&
  &SA3*dAlpha1MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA3*EL*MH12*SA2*dAlpha1MSBarAlter()*DBLE(CA1**INT(3.D0)))&
  &/(MW*SB*SW) - (0.5625D0*CA22*CA3*EL*MH12*SA2*dAlpha1MSBarAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA3*EL*MH32*SA2&
  &*dAlpha1MSBarAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA22*CA3*EL*MH32*SA2*dAlpha1MSBarAlter()*DBLE(CA1**INT(3.D0&
  &)))/(MW*SB*SW) + (0.5625D0*EL*m12squared*SA3*dAlpha1MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.5625D0*CA22*EL*m12s&
  &quared*SA3*dAlpha1MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*EL*m12squared*SA22*SA3*dAlpha1MSBarAlter()*DBL&
  &E(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.28125D0*CA3*EL*m12squared*SA2*dAlpha1MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) &
  &+ (0.84375D0*CA22*CA3*EL*m12squared*SA2*dAlpha1MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*CA2*CA3*EL*MH12*d&
  &Alpha2MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA2*CA3*EL*MH32*dAlpha2MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB&
  &*MW*SW) - (0.5625D0*CA2*CA3*EL*MH12*SA22*dAlpha2MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA2*CA3*EL*MH32*SA2&
  &2*dAlpha2MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA2*CA3*EL*m12squared*dAlpha2MSBarAlter()*DBLE(CA1**INT(3.&
  &D0)))/(CB2*MW*SB*SW) + (0.84375D0*CA2*CA3*EL*m12squared*SA22*dAlpha2MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.5D0&
  &*CA2*EL*MH12*SA2*SA3*dAlpha2MSBarAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.25D0*CA2*EL*MH32*SA2*SA3*dAlpha2MSBarAlter()*DB&
  &LE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.75D0*CA2*EL*m12squared*SA2*SA3*dAlpha2MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - &
  &(0.0625D0*EL*MH12*SA2*SA3*dAlpha3MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA22*EL*MH12*SA2*SA3*dAlpha3MSBarAl&
  &ter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*EL*MH32*SA2*SA3*dAlpha3MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.&
  &09375D0*CA22*EL*MH32*SA2*SA3*dAlpha3MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.125D0*CA3*EL*MH12*dAlpha3MSBarAlter()*D&
  &BLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.125D0*CA22*CA3*EL*MH12*dAlpha3MSBarAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.0625D0*C&
  &A3*EL*MH32*dAlpha3MSBarAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) - (0.0625D0*CA22*CA3*EL*MH32*dAlpha3MSBarAlter()*DBLE(CA1**INT&
  &(3.D0)))/(MW*SB*SW) + (0.125D0*CA3*EL*MH12*SA22*dAlpha3MSBarAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.0625D0*CA3*EL*MH32*S&
  &A22*dAlpha3MSBarAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*EL*m12squared*SA2*SA3*dAlpha3MSBarAlter()*DBLE(CA1**INT(&
  &3.D0)))/(CB2*MW*SB*SW) + (0.28125D0*CA22*EL*m12squared*SA2*SA3*dAlpha3MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1&
  &875D0*CA3*EL*m12squared*dAlpha3MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*CA22*CA3*EL*m12squared*dAlpha3MSB&
  &arAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.1875D0*CA3*EL*m12squared*SA22*dAlpha3MSBarAlter()*DBLE(CA1**INT(3.D0)))/(C&
  &B*MW*SB2*SW) - (0.0625D0*EL*MH12*SA3*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.0625D0*CA22*EL*MH12*SA3*dBetaMSBa&
  &rAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*EL*MH32*SA3*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) - (0.031&
  &25D0*CA22*EL*MH32*SA3*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*EL*MH12*SA22*SA3*dBetaMSBarAlter()*DBLE(C&
  &A1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*EL*MH32*SA22*SA3*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*EL*m1&
  &2squared*SA3*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + (0.28125D0*CA22*EL*m12squared*SA3*dBetaMSBarAlter()*DBLE&
  &(CA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.28125D0*EL*m12squared*SA22*SA3*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB2*MW*SB*SW) + &
  &(0.09375D0*CA3*EL*m12squared*SA2*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA22*CA3*EL*m12squared*SA&
  &2*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*EL*MH12*SA3*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*M&
  &W*SB2*SW) + (0.0625D0*CA22*EL*MH12*SA3*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.03125D0*EL*MH32*SA3*dBetaMS&
  &BarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.03125D0*CA22*EL*MH32*SA3*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*S&
  &B2*SW) - (0.0625D0*EL*MH12*SA22*SA3*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.03125D0*EL*MH32*SA22*SA3*dBeta&
  &MSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*EL*MH12*SA3*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB&
  &) + (0.0625D0*CA22*EL*MH12*SA3*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.03125D0*EL*MH32*SA3*dBetaMSBarAlter(&
  &)*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.03125D0*CA22*EL*MH32*SA3*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) - (0&
  &.0625D0*EL*MH12*SA22*SA3*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.03125D0*EL*MH32*SA22*SA3*dBetaMSBarAlter()&
  &*DBLE(CA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.0625D0*CA3*EL*MH12*SA2*TB*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.18&
  &75D0*CA22*CA3*EL*MH12*SA2*TB*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*CA3*EL*MH32*SA2*TB*dBetaMSBarAlte&
  &r()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA22*CA3*EL*MH32*SA2*TB*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0)))/(CB*MW*SW) +&
  & (0.5625D0*CA1*CA3*EL*MH12*dAlpha2MSBarAlter()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA1*CA3*EL*MH32*dAlpha2MSBarAlter&
  &()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.84375D0*CA3*EL*m12squared*SA1*dAlpha2MSBarAlter()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - &
  &(0.5625D0*CA1*CA3*EL*MH12*SA12*dAlpha2MSBarAlter()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA1*CA3*EL*MH32*SA12*dAlpha2M&
  &SBarAlter()*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) + (0.84375D0*CA1*CA3*EL*m12squared*dAlpha2MSBarAlter()*DBLE(CA2**INT(3.D0)))/(MW*&
  &SB*SW) - (0.5625D0*CA1*CA3*EL*m12squared*dAlpha2MSBarAlter()*DBLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.5625D0*CA3*EL*MH12*SA1*&
  &dAlpha2MSBarAlter()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA12*CA3*EL*MH12*SA1*dAlpha2MSBarAlter()*DBLE(CA2**INT(3.D0))&
  &)/(MW*SB*SW) + (0.28125D0*CA3*EL*MH32*SA1*dAlpha2MSBarAlter()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA12*CA3*EL*MH32*S&
  &A1*dAlpha2MSBarAlter()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW) + (0.84375D0*CA1*CA3*EL*m12squared*SA12*dAlpha2MSBarAlter()*DBLE(CA2**&
  &INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA3*EL*m12squared*SA1*dAlpha2MSBarAlter()*DBLE(CA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.843&
  &75D0*CA12*CA3*EL*m12squared*SA1*dAlpha2MSBarAlter()*DBLE(CA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.28125D0*CA3*EL*m12squared*SA1*dA&
  &lpha2MSBarAlter()*DBLE(CA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.28125D0*CA1*CA3*EL*m12squared*TB*dAlpha2MSBarAlter()*DBLE(CA2**INT(&
  &3.D0)))/(CB*MW*SW) - (0.5D0*MH12*SA3*dAlpha3MSBarAlter()*DBLE(CA2**INT(3.D0)))/vS - (0.25D0*MH32*SA3*dAlpha3MSBarAlter()*DBLE(&
  &CA2**INT(3.D0)))/vS - (0.125D0*CA3*MH12*dBetaMSBarAlter()*DBLE(CA2**INT(3.D0)))/(CB*SB*vS) - (0.0625D0*CA3*MH32*dBetaMSBarAlte&
  &r()*DBLE(CA2**INT(3.D0)))/(CB*SB*vS) + (0.125D0*CA3*MH12*dBetaMSBarAlter()*DBLE(CA2**INT(3.D0)))/(TB*vS) + (0.0625D0*CA3*MH32*&
  &dBetaMSBarAlter()*DBLE(CA2**INT(3.D0)))/(TB*vS) + (0.125D0*CA3*MH12*TB*dBetaMSBarAlter()*DBLE(CA2**INT(3.D0)))/vS + (0.0625D0*&
  &CA3*MH32*TB*dBetaMSBarAlter()*DBLE(CA2**INT(3.D0)))/vS + (0.1875D0*CA3*EL*MH12*dAlpha2MSBarAlter()*DBLE(CA1**INT(3.D0))*DBLE(C&
  &A2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA3*EL*MH32*dAlpha2MSBarAlter()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB*MW*SW) &
  &- (0.28125D0*CA3*EL*m12squared*dAlpha2MSBarAlter()*DBLE(CA1**INT(3.D0))*DBLE(CA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.375D0*CA1*CA&
  &3*EL*m12squared*SA2*dBetaMSBarAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (1.125D0*CA1*CA22*CA3*EL*m12squared*SA2*dBetaMSBarAlter(&
  &)*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.5625D0*CA1*CA3*EL*m12squared*SA12*SA2*dBetaMSBarAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (&
  &1.6875D0*CA1*CA22*CA3*EL*m12squared*SA12*SA2*dBetaMSBarAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.25D0*EL*m12squared*SA1*SA3*d&
  &BetaMSBarAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (1.125D0*CA12*EL*m12squared*SA1*SA3*dBetaMSBarAlter()*DBLE(CB**INT(-3.D0)))/(&
  &MW*SW) + (0.25D0*CA22*EL*m12squared*SA1*SA3*dBetaMSBarAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) + (1.125D0*CA12*CA22*EL*m12squared&
  &*SA1*SA3*dBetaMSBarAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.25D0*EL*m12squared*SA1*SA22*SA3*dBetaMSBarAlter()*DBLE(CB**INT(-&
  &3.D0)))/(MW*SW) - (1.125D0*CA12*EL*m12squared*SA1*SA22*SA3*dBetaMSBarAlter()*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.1875D0*CA3*EL*&
  &m12squared*SA2*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SW) - (0.5625D0*CA22*CA3*EL*m12squared*SA2*dBe&
  &taMSBarAlter()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0)))/(MW*SW) + (0.1875D0*CA3*EL*MH12*SA2*dAlpha1MSBarAlter()*DBLE(SA1**IN&
  &T(3.D0)))/(CB*MW*SW) + (0.5625D0*CA22*CA3*EL*MH12*SA2*dAlpha1MSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA3*EL&
  &*MH32*SA2*dAlpha1MSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA22*CA3*EL*MH32*SA2*dAlpha1MSBarAlter()*DBLE(SA1*&
  &*INT(3.D0)))/(CB*MW*SW) - (0.28125D0*CA3*EL*m12squared*SA2*dAlpha1MSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.84375&
  &D0*CA22*CA3*EL*m12squared*SA2*dAlpha1MSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.375D0*EL*MH12*SA3*dAlpha1MSBarAlte&
  &r()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.375D0*CA22*EL*MH12*SA3*dAlpha1MSBarAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.187&
  &5D0*EL*MH32*SA3*dAlpha1MSBarAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA22*EL*MH32*SA3*dAlpha1MSBarAlter()*DBLE(SA1&
  &**INT(3.D0)))/(MW*SB*SW) + (0.375D0*EL*MH12*SA22*SA3*dAlpha1MSBarAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*EL*MH32*&
  &SA22*SA3*dAlpha1MSBarAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.5625D0*EL*m12squared*SA3*dAlpha1MSBarAlter()*DBLE(SA1**INT(&
  &3.D0)))/(CB*MW*SB2*SW) + (0.5625D0*CA22*EL*m12squared*SA3*dAlpha1MSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.5625D0&
  &*EL*m12squared*SA22*SA3*dAlpha1MSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.5D0*CA2*EL*MH12*SA2*SA3*dAlpha2MSBarAlte&
  &r()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.25D0*CA2*EL*MH32*SA2*SA3*dAlpha2MSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0&
  &625D0*CA2*CA3*EL*MH12*dAlpha2MSBarAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.03125D0*CA2*CA3*EL*MH32*dAlpha2MSBarAlter()*DB&
  &LE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.5625D0*CA2*CA3*EL*MH12*SA22*dAlpha2MSBarAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.2812&
  &5D0*CA2*CA3*EL*MH32*SA22*dAlpha2MSBarAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.75D0*CA2*EL*m12squared*SA2*SA3*dAlpha2MSBar&
  &Alter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA2*CA3*EL*m12squared*dAlpha2MSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*&
  &MW*SB2*SW) + (0.84375D0*CA2*CA3*EL*m12squared*SA22*dAlpha2MSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.125D0*CA3*EL*&
  &MH12*dAlpha3MSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.125D0*CA22*CA3*EL*MH12*dAlpha3MSBarAlter()*DBLE(SA1**INT(3.D0))&
  &)/(CB*MW*SW) + (0.0625D0*CA3*EL*MH32*dAlpha3MSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA22*CA3*EL*MH32*dAlpha3&
  &MSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.125D0*CA3*EL*MH12*SA22*dAlpha3MSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW)&
  & - (0.0625D0*CA3*EL*MH32*SA22*dAlpha3MSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA3*EL*m12squared*dAlpha3MSBarA&
  &lter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*CA22*CA3*EL*m12squared*dAlpha3MSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB2*&
  &MW*SB*SW) + (0.1875D0*CA3*EL*m12squared*SA22*dAlpha3MSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.0625D0*EL*MH12*SA2*&
  &SA3*dAlpha3MSBarAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA22*EL*MH12*SA2*SA3*dAlpha3MSBarAlter()*DBLE(SA1**INT(3.&
  &D0)))/(MW*SB*SW) - (0.03125D0*EL*MH32*SA2*SA3*dAlpha3MSBarAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA22*EL*MH32*S&
  &A2*SA3*dAlpha3MSBarAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*EL*m12squared*SA2*SA3*dAlpha3MSBarAlter()*DBLE(SA1**I&
  &NT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA22*EL*m12squared*SA2*SA3*dAlpha3MSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (&
  &0.03125D0*CA3*EL*MH12*SA2*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA22*CA3*EL*MH12*SA2*dBetaMSBarAlter&
  &()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.015625D0*CA3*EL*MH32*SA2*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0468&
  &75D0*CA22*CA3*EL*MH32*SA2*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.140625D0*CA3*EL*m12squared*SA2*dBetaMSBarAlt&
  &er()*DBLE(SA1**INT(3.D0)))/(CB2*MW*SB*SW) - (0.421875D0*CA22*CA3*EL*m12squared*SA2*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB&
  &2*MW*SB*SW) - (0.03125D0*CA3*EL*MH12*SA2*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.09375D0*CA22*CA3*EL*MH12*&
  &SA2*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.015625D0*CA3*EL*MH32*SA2*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0)&
  &))/(CB*MW*SB2*SW) - (0.046875D0*CA22*CA3*EL*MH32*SA2*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*EL*m12&
  &squared*SA3*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*CA22*EL*m12squared*SA3*dBetaMSBarAlter()*DBLE(S&
  &A1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.1875D0*EL*m12squared*SA22*SA3*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0.&
  &03125D0*CA3*EL*MH12*SA2*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA22*CA3*EL*MH12*SA2*dBetaMSBarAlte&
  &r()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - (0.015625D0*CA3*EL*MH32*SA2*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) - &
  &(0.046875D0*CA22*CA3*EL*MH32*SA2*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0)))/(MW*SB*SW*TB) + (0.125D0*EL*MH12*SA3*TB*dBetaMSBarAlt&
  &er()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.125D0*CA22*EL*MH12*SA3*TB*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0&
  &625D0*EL*MH32*SA3*TB*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.0625D0*CA22*EL*MH32*SA3*TB*dBetaMSBarAlter()*DBLE&
  &(SA1**INT(3.D0)))/(CB*MW*SW) - (0.125D0*EL*MH12*SA22*SA3*TB*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) - (0.0625D0*EL*&
  &MH32*SA22*SA3*TB*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA3*EL*MH12*dAlpha2MSBarAlter()*DBLE(CA2**INT(&
  &3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA3*EL*MH32*dAlpha2MSBarAlter()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0))&
  &)/(MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*dAlpha2MSBarAlter()*DBLE(CA2**INT(3.D0))*DBLE(SA1**INT(3.D0)))/(CB*MW*SB2*SW) - (0&
  &.375D0*EL*m12squared*SA3*dBetaMSBarAlter()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) - (0.375D0*CA22*EL*m12squared*SA&
  &3*dBetaMSBarAlter()*DBLE(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) + (0.375D0*EL*m12squared*SA22*SA3*dBetaMSBarAlter()*DBL&
  &E(CB**INT(-3.D0))*DBLE(SA1**INT(3.D0)))/(MW*SW) - (0.28125D0*CA1*CA3*EL*m12squared*dAlpha1MSBarAlter()*DBLE(SA2**INT(3.D0)))/(&
  &CB*MW*SW) + (0.1875D0*CA3*EL*MH12*SA1*dAlpha1MSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.5625D0*CA12*CA3*EL*MH12*SA1*dA&
  &lpha1MSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA3*EL*MH32*SA1*dAlpha1MSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB*&
  &MW*SW) + (0.28125D0*CA12*CA3*EL*MH32*SA1*dAlpha1MSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*MH12*dAlp&
  &ha1MSBarAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA1*CA3*EL*MH32*dAlpha1MSBarAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB&
  &*SW) + (0.28125D0*CA3*EL*m12squared*SA1*dAlpha1MSBarAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA3*EL*m12squared*SA1&
  &*dAlpha1MSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.84375D0*CA12*CA3*EL*m12squared*SA1*dAlpha1MSBarAlter()*DBLE(SA2&
  &**INT(3.D0)))/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA3*EL*MH12*SA12*dAlpha1MSBarAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D&
  &0*CA1*CA3*EL*MH32*SA12*dAlpha1MSBarAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*m12squared*dAlpha1MSBarAlte&
  &r()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.84375D0*CA1*CA3*EL*m12squared*SA12*dAlpha1MSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB&
  &*MW*SB2*SW) + (0.09375D0*CA1*CA3*EL*m12squared*dAlpha1MSBarAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.09375D0*CA3*EL*m12&
  &squared*SA1*TB*dAlpha1MSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (1.5D0*CA3*MH12*dAlpha2MSBarAlter()*DBLE(SA2**INT(3.D0))&
  &)/vS + (0.75D0*CA3*MH32*dAlpha2MSBarAlter()*DBLE(SA2**INT(3.D0)))/vS + (0.1875D0*CA1*EL*MH12*SA3*dAlpha3MSBarAlter()*DBLE(SA2*&
  &*INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA1*EL*MH32*SA3*dAlpha3MSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*EL*m12s&
  &quared*SA1*SA3*dAlpha3MSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.1875D0*CA1*EL*MH12*SA12*SA3*dAlpha3MSBarAlter()*DBLE(&
  &SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1*EL*MH32*SA12*SA3*dAlpha3MSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D&
  &0*CA1*EL*m12squared*SA3*dAlpha3MSBarAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.1875D0*CA1*EL*m12squared*SA3*dAlpha3MSBarAlt&
  &er()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.1875D0*EL*MH12*SA1*SA3*dAlpha3MSBarAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (&
  &0.1875D0*CA12*EL*MH12*SA1*SA3*dAlpha3MSBarAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*EL*MH32*SA1*SA3*dAlpha3MSBarAl&
  &ter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0*CA12*EL*MH32*SA1*SA3*dAlpha3MSBarAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) &
  &+ (0.28125D0*CA1*EL*m12squared*SA12*SA3*dAlpha3MSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.1875D0*EL*m12squared*SA1&
  &*SA3*dAlpha3MSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.28125D0*CA12*EL*m12squared*SA1*SA3*dAlpha3MSBarAlter()*DBLE&
  &(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.09375D0*EL*m12squared*SA1*SA3*dAlpha3MSBarAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - &
  &(0.09375D0*CA1*EL*m12squared*SA3*TB*dAlpha3MSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA3*EL*MH12*SA1*dBetaMSB&
  &arAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA12*CA3*EL*MH12*SA1*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW&
  &) - (0.046875D0*CA3*EL*MH32*SA1*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.046875D0*CA12*CA3*EL*MH32*SA1*dBetaMSB&
  &arAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA3*EL*m12squared*SA1*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*S&
  &W) + (0.328125D0*CA3*EL*m12squared*SA1*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.421875D0*CA12*CA3*EL*m12squ&
  &ared*SA1*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA1*CA3*EL*m12squared*dBetaMSBarAlter()*DBLE(SA2*&
  &*INT(3.D0)))/(CB*MW*SB2*SW) + (0.09375D0*CA3*EL*MH12*SA1*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.09375D0*C&
  &A12*CA3*EL*MH12*SA1*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.046875D0*CA3*EL*MH32*SA1*dBetaMSBarAlter()*DBL&
  &E(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.046875D0*CA12*CA3*EL*MH32*SA1*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + &
  &(0.28125D0*CA1*CA3*EL*m12squared*SA12*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.1875D0*CA1*CA3*EL*m12squared&
  &*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.09375D0*CA3*EL*MH12*SA1*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0)))/(M&
  &W*SB*SW*TB) - (0.09375D0*CA12*CA3*EL*MH12*SA1*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.046875D0*CA3*EL*MH32*&
  &SA1*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) - (0.046875D0*CA12*CA3*EL*MH32*SA1*dBetaMSBarAlter()*DBLE(SA2**INT(3&
  &.D0)))/(MW*SB*SW*TB) - (0.1875D0*CA1*CA3*EL*MH12*TB*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA1*CA3*EL&
  &*MH32*TB*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.328125D0*CA3*EL*m12squared*SA1*TB*dBetaMSBarAlter()*DBLE(SA2*&
  &*INT(3.D0)))/(CB*MW*SW) + (0.1875D0*CA1*CA3*EL*MH12*SA12*TB*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.09375D0*CA&
  &1*CA3*EL*MH32*SA12*TB*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.140625D0*CA3*EL*m12squared*SA1*dBetaMSBarAlter()&
  &*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB2) + (0.1875D0*CA1*CA3*EL*m12squared*TB2*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0)))/(CB*MW*SW)&
  & + (0.1875D0*CA3*EL*MH12*dAlpha1MSBarAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) + (0.09375D0*CA3*EL*MH32*dA&
  &lpha1MSBarAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.28125D0*CA3*EL*m12squared*dAlpha1MSBarAlter()*DBL&
  &E(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.0625D0*EL*MH12*SA3*dAlpha3MSBarAlter()*DBLE(CA1**INT(3.D0))*DBLE(S&
  &A2**INT(3.D0)))/(CB*MW*SW) + (0.03125D0*EL*MH32*SA3*dAlpha3MSBarAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) &
  &- (0.09375D0*EL*m12squared*SA3*dAlpha3MSBarAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) - (0.09375D0*CA3*&
  &EL*m12squared*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.0625D0*CA3*EL*MH12*TB*dBetaMSBa&
  &rAlter()*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.03125D0*CA3*EL*MH32*TB*dBetaMSBarAlter()*DBLE(CA1**INT(3.D&
  &0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.375D0*CA1*CA3*EL*m12squared*dBetaMSBarAlter()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D&
  &0)))/(MW*SW) - (0.5625D0*CA1*CA3*EL*m12squared*SA12*dBetaMSBarAlter()*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) + (0.&
  &1875D0*CA3*EL*m12squared*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0))*DBLE(CB**INT(-3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SW) - (0.1875D0&
  &*CA3*EL*MH12*dAlpha1MSBarAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.09375D0*CA3*EL*MH32*dAlpha1MSBarAl&
  &ter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.28125D0*CA3*EL*m12squared*dAlpha1MSBarAlter()*DBLE(SA1**INT(3&
  &.D0))*DBLE(SA2**INT(3.D0)))/(CB2*MW*SB*SW) + (0.0625D0*EL*MH12*SA3*dAlpha3MSBarAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0&
  &)))/(MW*SB*SW) + (0.03125D0*EL*MH32*SA3*dAlpha3MSBarAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW) - (0.09375D0&
  &*EL*m12squared*SA3*dAlpha3MSBarAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) - (0.03125D0*CA3*EL*MH12*dBet&
  &aMSBarAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) - (0.015625D0*CA3*EL*MH32*dBetaMSBarAlter()*DBLE(SA1**INT(&
  &3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SW) + (0.140625D0*CA3*EL*m12squared*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3&
  &.D0)))/(CB2*MW*SB*SW) + (0.03125D0*CA3*EL*MH12*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (&
  &0.015625D0*CA3*EL*MH32*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(CB*MW*SB2*SW) + (0.03125D0*CA3*EL*MH12*dB&
  &etaMSBarAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.015625D0*CA3*EL*MH32*dBetaMSBarAlter()*DBLE(SA1*&
  &*INT(3.D0))*DBLE(SA2**INT(3.D0)))/(MW*SB*SW*TB) + (0.328125D0*CA3*EL*m12squared*SA1*SA2*dBetaMSBarAlter()*DBLE(SB**INT(-3.D0))&
  &)/(MW*SW) - (0.421875D0*CA12*CA3*EL*m12squared*SA1*SA2*dBetaMSBarAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.984375D0*CA22*CA3*&
  &EL*m12squared*SA1*SA2*dBetaMSBarAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (1.265625D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*dBetaM&
  &SBarAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.09375D0*CA3*EL*m12squared*SA1*SA2*dBetaMSBarAlter()*DBLE(SB**INT(-3.D0)))/(CB2*&
  &MW*SW) - (0.140625D0*CA12*CA3*EL*m12squared*SA1*SA2*dBetaMSBarAlter()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.28125D0*CA22*CA3*&
  &EL*m12squared*SA1*SA2*dBetaMSBarAlter()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.421875D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*dB&
  &etaMSBarAlter()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.21875D0*CA1*EL*m12squared*SA3*dBetaMSBarAlter()*DBLE(SB**INT(-3.D0)))/(&
  &MW*SW) + (0.21875D0*CA1*CA22*EL*m12squared*SA3*dBetaMSBarAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.0625D0*CA1*EL*m12squared*S&
  &A3*dBetaMSBarAlter()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.0625D0*CA1*CA22*EL*m12squared*SA3*dBetaMSBarAlter()*DBLE(SB**INT(-&
  &3.D0)))/(CB2*MW*SW) + (0.84375D0*CA1*EL*m12squared*SA12*SA3*dBetaMSBarAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.84375D0*CA1*C&
  &A22*EL*m12squared*SA12*SA3*dBetaMSBarAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.28125D0*CA1*EL*m12squared*SA12*SA3*dBetaMSBarA&
  &lter()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.28125D0*CA1*CA22*EL*m12squared*SA12*SA3*dBetaMSBarAlter()*DBLE(SB**INT(-3.D0)))/&
  &(CB2*MW*SW) - (0.21875D0*CA1*EL*m12squared*SA22*SA3*dBetaMSBarAlter()*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.0625D0*CA1*EL*m12squa&
  &red*SA22*SA3*dBetaMSBarAlter()*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.84375D0*CA1*EL*m12squared*SA12*SA22*SA3*dBetaMSBarAlter(&
  &)*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.28125D0*CA1*EL*m12squared*SA12*SA22*SA3*dBetaMSBarAlter()*DBLE(SB**INT(-3.D0)))/(CB2*MW*S&
  &W) - (0.28125D0*EL*m12squared*SA3*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.28125D0*CA22*EL*m1&
  &2squared*SA3*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.09375D0*EL*m12squared*SA3*dBetaMSBarAlt&
  &er()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.09375D0*CA22*EL*m12squared*SA3*dBetaMSBarAlter()*DBLE(CA1**IN&
  &T(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.28125D0*EL*m12squared*SA22*SA3*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0))*DBLE(SB*&
  &*INT(-3.D0)))/(MW*SW) + (0.09375D0*EL*m12squared*SA22*SA3*dBetaMSBarAlter()*DBLE(CA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW&
  &*SW) + (0.140625D0*CA3*EL*m12squared*SA2*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.421875D0*CA&
  &22*CA3*EL*m12squared*SA2*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.046875D0*CA3*EL*m12squared*&
  &SA2*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.140625D0*CA22*CA3*EL*m12squared*SA2*dBetaMSB&
  &arAlter()*DBLE(SA1**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - (0.328125D0*CA3*EL*m12squared*SA1*dBetaMSBarAlter()*DBLE(SA&
  &2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) + (0.421875D0*CA12*CA3*EL*m12squared*SA1*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0))*DB&
  &LE(SB**INT(-3.D0)))/(MW*SW) - (0.09375D0*CA3*EL*m12squared*SA1*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(C&
  &B2*MW*SW) + (0.140625D0*CA12*CA3*EL*m12squared*SA1*dBetaMSBarAlter()*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) - &
  &(0.140625D0*CA3*EL*m12squared*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(MW*SW) - (0.0&
  &46875D0*CA3*EL*m12squared*dBetaMSBarAlter()*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*DBLE(SB**INT(-3.D0)))/(CB2*MW*SW) + (0.1&
  &875D0*CA1*CA3*MH12*SA2*dgAtMZ())/(CB*MW) + (0.5625D0*CA1*CA22*CA3*MH12*SA2*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*CA3*MH32*SA2*dgA&
  &tMZ())/(CB*MW) + (0.28125D0*CA1*CA22*CA3*MH32*SA2*dgAtMZ())/(CB*MW) + (0.28125D0*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*MW) + (0&
  &.84375D0*CA22*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*MW) - (0.1875D0*CA1*CA3*MH12*SA12*SA2*dgAtMZ())/(CB*MW) - (0.5625D0*CA1*CA2&
  &2*CA3*MH12*SA12*SA2*dgAtMZ())/(CB*MW) - (0.09375D0*CA1*CA3*MH32*SA12*SA2*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*CA3*MH32*SA12&
  &*SA2*dgAtMZ())/(CB*MW) + (0.1875D0*CA1*m12squared*SA3*dgAtMZ())/(CB*MW) + (0.1875D0*CA1*CA22*m12squared*SA3*dgAtMZ())/(CB*MW) &
  &- (0.125D0*MH12*SA1*SA3*dgAtMZ())/(CB*MW) - (0.375D0*CA12*MH12*SA1*SA3*dgAtMZ())/(CB*MW) - (0.125D0*CA22*MH12*SA1*SA3*dgAtMZ()&
  &)/(CB*MW) - (0.375D0*CA12*CA22*MH12*SA1*SA3*dgAtMZ())/(CB*MW) - (0.0625D0*MH32*SA1*SA3*dgAtMZ())/(CB*MW) - (0.1875D0*CA12*MH32&
  &*SA1*SA3*dgAtMZ())/(CB*MW) - (0.0625D0*CA22*MH32*SA1*SA3*dgAtMZ())/(CB*MW) - (0.1875D0*CA12*CA22*MH32*SA1*SA3*dgAtMZ())/(CB*MW&
  &) - (0.1875D0*CA1*m12squared*SA22*SA3*dgAtMZ())/(CB*MW) + (0.125D0*MH12*SA1*SA22*SA3*dgAtMZ())/(CB*MW) + (0.375D0*CA12*MH12*SA&
  &1*SA22*SA3*dgAtMZ())/(CB*MW) + (0.0625D0*MH32*SA1*SA22*SA3*dgAtMZ())/(CB*MW) + (0.1875D0*CA12*MH32*SA1*SA22*SA3*dgAtMZ())/(CB*&
  &MW) + (0.28125D0*CA1*CA3*m12squared*SA2*dgAtMZ())/(MW*SB) + (0.84375D0*CA1*CA22*CA3*m12squared*SA2*dgAtMZ())/(MW*SB) - (0.1875&
  &D0*CA1*CA3*m12squared*SA2*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA1*CA22*CA3*m12squared*SA2*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*CA3*&
  &MH12*SA1*SA2*dgAtMZ())/(MW*SB) - (0.1875D0*CA12*CA3*MH12*SA1*SA2*dgAtMZ())/(MW*SB) + (0.5625D0*CA22*CA3*MH12*SA1*SA2*dgAtMZ())&
  &/(MW*SB) - (0.5625D0*CA12*CA22*CA3*MH12*SA1*SA2*dgAtMZ())/(MW*SB) + (0.09375D0*CA3*MH32*SA1*SA2*dgAtMZ())/(MW*SB) - (0.09375D0&
  &*CA12*CA3*MH32*SA1*SA2*dgAtMZ())/(MW*SB) + (0.28125D0*CA22*CA3*MH32*SA1*SA2*dgAtMZ())/(MW*SB) - (0.28125D0*CA12*CA22*CA3*MH32*&
  &SA1*SA2*dgAtMZ())/(MW*SB) + (0.28125D0*CA1*CA3*m12squared*SA12*SA2*dgAtMZ())/(CB2*MW*SB) + (0.84375D0*CA1*CA22*CA3*m12squared*&
  &SA12*SA2*dgAtMZ())/(CB2*MW*SB) + (0.125D0*CA1*MH12*SA3*dgAtMZ())/(MW*SB) + (0.125D0*CA1*CA22*MH12*SA3*dgAtMZ())/(MW*SB) + (0.0&
  &625D0*CA1*MH32*SA3*dgAtMZ())/(MW*SB) + (0.0625D0*CA1*CA22*MH32*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*m12squared*SA1*SA3*dgAtMZ())/&
  &(MW*SB) - (0.1875D0*CA22*m12squared*SA1*SA3*dgAtMZ())/(MW*SB) + (0.125D0*m12squared*SA1*SA3*dgAtMZ())/(CB2*MW*SB) + (0.5625D0*&
  &CA12*m12squared*SA1*SA3*dgAtMZ())/(CB2*MW*SB) + (0.125D0*CA22*m12squared*SA1*SA3*dgAtMZ())/(CB2*MW*SB) + (0.5625D0*CA12*CA22*m&
  &12squared*SA1*SA3*dgAtMZ())/(CB2*MW*SB) + (0.375D0*CA1*MH12*SA12*SA3*dgAtMZ())/(MW*SB) + (0.375D0*CA1*CA22*MH12*SA12*SA3*dgAtM&
  &Z())/(MW*SB) + (0.1875D0*CA1*MH32*SA12*SA3*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*CA22*MH32*SA12*SA3*dgAtMZ())/(MW*SB) - (0.125D0*C&
  &A1*MH12*SA22*SA3*dgAtMZ())/(MW*SB) - (0.0625D0*CA1*MH32*SA22*SA3*dgAtMZ())/(MW*SB) + (0.1875D0*m12squared*SA1*SA22*SA3*dgAtMZ(&
  &))/(MW*SB) - (0.125D0*m12squared*SA1*SA22*SA3*dgAtMZ())/(CB2*MW*SB) - (0.5625D0*CA12*m12squared*SA1*SA22*SA3*dgAtMZ())/(CB2*MW&
  &*SB) - (0.375D0*CA1*MH12*SA12*SA22*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*CA1*MH32*SA12*SA22*SA3*dgAtMZ())/(MW*SB) - (0.1875D0*CA3*&
  &m12squared*SA1*SA2*dgAtMZ())/(CB*MW*SB2) + (0.28125D0*CA12*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA22*CA3*m&
  &12squared*SA1*SA2*dgAtMZ())/(CB*MW*SB2) + (0.84375D0*CA12*CA22*CA3*m12squared*SA1*SA2*dgAtMZ())/(CB*MW*SB2) - (0.125D0*CA1*m12&
  &squared*SA3*dgAtMZ())/(CB*MW*SB2) - (0.125D0*CA1*CA22*m12squared*SA3*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA1*m12squared*SA12*SA3&
  &*dgAtMZ())/(CB*MW*SB2) - (0.5625D0*CA1*CA22*m12squared*SA12*SA3*dgAtMZ())/(CB*MW*SB2) + (0.125D0*CA1*m12squared*SA22*SA3*dgAtM&
  &Z())/(CB*MW*SB2) + (0.5625D0*CA1*m12squared*SA12*SA22*SA3*dgAtMZ())/(CB*MW*SB2) - (0.09375D0*CA3*m12squared*SA1*SA2*dgAtMZ())/&
  &(MW*SB*TB) - (0.28125D0*CA22*CA3*m12squared*SA1*SA2*dgAtMZ())/(MW*SB*TB) - (0.0625D0*CA1*m12squared*SA3*dgAtMZ())/(MW*SB*TB) -&
  & (0.0625D0*CA1*CA22*m12squared*SA3*dgAtMZ())/(MW*SB*TB) + (0.0625D0*CA1*m12squared*SA22*SA3*dgAtMZ())/(MW*SB*TB) - (0.09375D0*&
  &CA1*CA3*m12squared*SA2*TB*dgAtMZ())/(CB*MW) - (0.28125D0*CA1*CA22*CA3*m12squared*SA2*TB*dgAtMZ())/(CB*MW) + (0.0625D0*m12squar&
  &ed*SA1*SA3*TB*dgAtMZ())/(CB*MW) + (0.0625D0*CA22*m12squared*SA1*SA3*TB*dgAtMZ())/(CB*MW) - (0.0625D0*m12squared*SA1*SA22*SA3*T&
  &B*dgAtMZ())/(CB*MW) + (0.0625D0*CA3*MH12*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.1875D0*CA22*CA3*MH12*SA2*DBLE(CA1**IN&
  &T(3.D0))*dgAtMZ())/(CB*MW) + (0.03125D0*CA3*MH32*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA22*CA3*MH32*SA2*DBL&
  &E(CA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.09375D0*CA3*m12squared*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.28125D0*CA&
  &22*CA3*m12squared*SA2*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.125D0*MH12*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - &
  &(0.125D0*CA22*MH12*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.0625D0*MH32*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0&
  &.0625D0*CA22*MH32*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.125D0*MH12*SA22*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) +&
  & (0.0625D0*MH32*SA22*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*m12squared*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*&
  &MW*SB2) + (0.1875D0*CA22*m12squared*SA3*DBLE(CA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*m12squared*SA22*SA3*DBLE(CA1**I&
  &NT(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.125D0*MH12*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.125D0*CA22*MH12*SA3*DBLE(SA1**&
  &INT(3.D0))*dgAtMZ())/(CB*MW) + (0.0625D0*MH32*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.0625D0*CA22*MH32*SA3*DBLE(SA1**I&
  &NT(3.D0))*dgAtMZ())/(CB*MW) - (0.125D0*MH12*SA22*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*MH32*SA22*SA3*DBLE(SA1&
  &**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.0625D0*CA3*MH12*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA22*CA3*MH12*SA2*D&
  &BLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.03125D0*CA3*MH32*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA22*CA3*M&
  &H32*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.1875D0*m12squared*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.1875&
  &D0*CA22*m12squared*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) + (0.1875D0*m12squared*SA22*SA3*DBLE(SA1**INT(3.D0))*dgAtMZ(&
  &))/(CB2*MW*SB) - (0.09375D0*CA3*m12squared*SA2*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA22*CA3*m12squared*SA2&
  &*DBLE(SA1**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.1875D0*CA1*CA3*MH12*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.09375D0*CA1*C&
  &A3*MH32*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.28125D0*CA3*m12squared*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.187&
  &5D0*CA1*CA3*MH12*SA12*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA1*CA3*MH32*SA12*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB&
  &*MW) - (0.28125D0*CA1*CA3*m12squared*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA1*CA3*m12squared*DBLE(SA2**INT(3.D0)&
  &)*dgAtMZ())/(CB2*MW*SB) - (0.1875D0*CA3*MH12*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.1875D0*CA12*CA3*MH12*SA1*DBLE(SA2&
  &**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.09375D0*CA3*MH32*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA12*CA3*MH32*SA1&
  &*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) - (0.28125D0*CA1*CA3*m12squared*SA12*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) + (0.1&
  &875D0*CA3*m12squared*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) - (0.28125D0*CA12*CA3*m12squared*SA1*DBLE(SA2**INT(3.D0))*&
  &dgAtMZ())/(CB*MW*SB2) + (0.09375D0*CA3*m12squared*SA1*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB*TB) + (0.09375D0*CA1*CA3*m12square&
  &d*TB*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) - (0.0625D0*CA3*MH12*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) -&
  & (0.03125D0*CA3*MH32*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW) + (0.09375D0*CA3*m12squared*DBLE(CA1**INT(3.D&
  &0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB2*MW*SB) - (0.0625D0*CA3*MH12*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB&
  &) - (0.03125D0*CA3*MH32*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(MW*SB) + (0.09375D0*CA3*m12squared*DBLE(SA1**INT(&
  &3.D0))*DBLE(SA2**INT(3.D0))*dgAtMZ())/(CB*MW*SB2) + (0.28125D0*CA3*EL*SA1*SA2*dm122MSBarAlter())/(CB*MW*SW) + (0.84375D0*CA22*&
  &CA3*EL*SA1*SA2*dm122MSBarAlter())/(CB*MW*SW) + (0.1875D0*CA1*EL*SA3*dm122MSBarAlter())/(CB*MW*SW) + (0.1875D0*CA1*CA22*EL*SA3*&
  &dm122MSBarAlter())/(CB*MW*SW) - (0.1875D0*CA1*EL*SA22*SA3*dm122MSBarAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA3*EL*SA2*dm122MSBar&
  &Alter())/(MW*SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*SA2*dm122MSBarAlter())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*SA2*dm122MSBarAlter(&
  &))/(CB2*MW*SB*SW) - (0.5625D0*CA1*CA22*CA3*EL*SA2*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.28125D0*CA1*CA3*EL*SA12*SA2*dm122MSBa&
  &rAlter())/(CB2*MW*SB*SW) + (0.84375D0*CA1*CA22*CA3*EL*SA12*SA2*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.1875D0*EL*SA1*SA3*dm122M&
  &SBarAlter())/(MW*SB*SW) - (0.1875D0*CA22*EL*SA1*SA3*dm122MSBarAlter())/(MW*SB*SW) + (0.125D0*EL*SA1*SA3*dm122MSBarAlter())/(CB&
  &2*MW*SB*SW) + (0.5625D0*CA12*EL*SA1*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.125D0*CA22*EL*SA1*SA3*dm122MSBarAlter())/(CB2*M&
  &W*SB*SW) + (0.5625D0*CA12*CA22*EL*SA1*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.1875D0*EL*SA1*SA22*SA3*dm122MSBarAlter())/(MW&
  &*SB*SW) - (0.125D0*EL*SA1*SA22*SA3*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.5625D0*CA12*EL*SA1*SA22*SA3*dm122MSBarAlter())/(CB2*&
  &MW*SB*SW) - (0.1875D0*CA3*EL*SA1*SA2*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.28125D0*CA12*CA3*EL*SA1*SA2*dm122MSBarAlter())/(CB&
  &*MW*SB2*SW) - (0.5625D0*CA22*CA3*EL*SA1*SA2*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.84375D0*CA12*CA22*CA3*EL*SA1*SA2*dm122MSBar&
  &Alter())/(CB*MW*SB2*SW) - (0.125D0*CA1*EL*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.125D0*CA1*CA22*EL*SA3*dm122MSBarAlter())/&
  &(CB*MW*SB2*SW) - (0.5625D0*CA1*EL*SA12*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.5625D0*CA1*CA22*EL*SA12*SA3*dm122MSBarAlter(&
  &))/(CB*MW*SB2*SW) + (0.125D0*CA1*EL*SA22*SA3*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.5625D0*CA1*EL*SA12*SA22*SA3*dm122MSBarAlte&
  &r())/(CB*MW*SB2*SW) - (0.09375D0*CA3*EL*SA1*SA2*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.28125D0*CA22*CA3*EL*SA1*SA2*dm122MSBarAl&
  &ter())/(MW*SB*SW*TB) - (0.0625D0*CA1*EL*SA3*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.0625D0*CA1*CA22*EL*SA3*dm122MSBarAlter())/(M&
  &W*SB*SW*TB) + (0.0625D0*CA1*EL*SA22*SA3*dm122MSBarAlter())/(MW*SB*SW*TB) - (0.09375D0*CA1*CA3*EL*SA2*TB*dm122MSBarAlter())/(CB&
  &*MW*SW) - (0.28125D0*CA1*CA22*CA3*EL*SA2*TB*dm122MSBarAlter())/(CB*MW*SW) + (0.0625D0*EL*SA1*SA3*TB*dm122MSBarAlter())/(CB*MW*&
  &SW) + (0.0625D0*CA22*EL*SA1*SA3*TB*dm122MSBarAlter())/(CB*MW*SW) - (0.0625D0*EL*SA1*SA22*SA3*TB*dm122MSBarAlter())/(CB*MW*SW) &
  &- (0.09375D0*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.28125D0*CA22*CA3*EL*SA2*DBLE(CA1**INT(3.D0&
  &))*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.1875D0*EL*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.1875D0*CA2&
  &2*EL*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.1875D0*EL*SA22*SA3*DBLE(CA1**INT(3.D0))*dm122MSBarAlter()&
  &)/(CB*MW*SB2*SW) - (0.1875D0*EL*SA3*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (0.1875D0*CA22*EL*SA3*DBLE(SA1**I&
  &NT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.1875D0*EL*SA22*SA3*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) - (&
  &0.09375D0*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA22*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*&
  &dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SW) - (0.28125D0*CA1*&
  &CA3*EL*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(MW*SB*SW) + (0.1875D0*CA1*CA3*EL*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB2*&
  &MW*SB*SW) - (0.28125D0*CA1*CA3*EL*SA12*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*SW) + (0.1875D0*CA3*EL*SA1*DBLE(SA2*&
  &*INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) - (0.28125D0*CA12*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*&
  &SW) + (0.09375D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(MW*SB*SW*TB) + (0.09375D0*CA1*CA3*EL*TB*DBLE(SA2**INT(3.D&
  &0))*dm122MSBarAlter())/(CB*MW*SW) + (0.09375D0*CA3*EL*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB2*MW*SB*&
  &SW) + (0.09375D0*CA3*EL*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dm122MSBarAlter())/(CB*MW*SB2*SW) + (0.1875D0*CA1*CA3*EL*SA2&
  &*dMH12OSAlter())/(CB*MW*SW) + (0.5625D0*CA1*CA22*CA3*EL*SA2*dMH12OSAlter())/(CB*MW*SW) - (0.1875D0*CA1*CA3*EL*SA12*SA2*dMH12OS&
  &Alter())/(CB*MW*SW) - (0.5625D0*CA1*CA22*CA3*EL*SA12*SA2*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*EL*SA1*SA3*dMH12OSAlter())/(CB*&
  &MW*SW) - (0.375D0*CA12*EL*SA1*SA3*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*CA22*EL*SA1*SA3*dMH12OSAlter())/(CB*MW*SW) - (0.375D0*&
  &CA12*CA22*EL*SA1*SA3*dMH12OSAlter())/(CB*MW*SW) + (0.125D0*EL*SA1*SA22*SA3*dMH12OSAlter())/(CB*MW*SW) + (0.375D0*CA12*EL*SA1*S&
  &A22*SA3*dMH12OSAlter())/(CB*MW*SW) + (0.1875D0*CA3*EL*SA1*SA2*dMH12OSAlter())/(MW*SB*SW) - (0.1875D0*CA12*CA3*EL*SA1*SA2*dMH12&
  &OSAlter())/(MW*SB*SW) + (0.5625D0*CA22*CA3*EL*SA1*SA2*dMH12OSAlter())/(MW*SB*SW) - (0.5625D0*CA12*CA22*CA3*EL*SA1*SA2*dMH12OSA&
  &lter())/(MW*SB*SW) + (0.125D0*CA1*EL*SA3*dMH12OSAlter())/(MW*SB*SW) + (0.125D0*CA1*CA22*EL*SA3*dMH12OSAlter())/(MW*SB*SW) + (0&
  &.375D0*CA1*EL*SA12*SA3*dMH12OSAlter())/(MW*SB*SW) + (0.375D0*CA1*CA22*EL*SA12*SA3*dMH12OSAlter())/(MW*SB*SW) - (0.125D0*CA1*EL&
  &*SA22*SA3*dMH12OSAlter())/(MW*SB*SW) - (0.375D0*CA1*EL*SA12*SA22*SA3*dMH12OSAlter())/(MW*SB*SW) - (0.5D0*CA2*CA3*dMH12OSAlter(&
  &))/vS - (1.5D0*CA2*CA3*SA22*dMH12OSAlter())/vS + (0.0625D0*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) + (0.187&
  &5D0*CA22*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*EL*SA3*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(MW&
  &*SB*SW) - (0.125D0*CA22*EL*SA3*DBLE(CA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.125D0*EL*SA22*SA3*DBLE(CA1**INT(3.D0))*dMH&
  &12OSAlter())/(MW*SB*SW) + (0.5D0*CA3*DBLE(CA2**INT(3.D0))*dMH12OSAlter())/vS + (0.125D0*EL*SA3*DBLE(SA1**INT(3.D0))*dMH12OSAlt&
  &er())/(CB*MW*SW) + (0.125D0*CA22*EL*SA3*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.125D0*EL*SA22*SA3*DBLE(SA1**INT(3&
  &.D0))*dMH12OSAlter())/(CB*MW*SW) + (0.0625D0*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) + (0.1875D0*CA22*CA3*E&
  &L*SA2*DBLE(SA1**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) - (0.1875D0*CA1*CA3*EL*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) +&
  & (0.1875D0*CA1*CA3*EL*SA12*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.1875D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dMH12OS&
  &Alter())/(MW*SB*SW) + (0.1875D0*CA12*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(MW*SB*SW) - (0.0625D0*CA3*EL*DBLE(CA1**I&
  &NT(3.D0))*DBLE(SA2**INT(3.D0))*dMH12OSAlter())/(CB*MW*SW) - (0.0625D0*CA3*EL*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH12OS&
  &Alter())/(MW*SB*SW) + (0.09375D0*CA1*CA3*EL*SA2*dMH32OSAlter())/(CB*MW*SW) + (0.28125D0*CA1*CA22*CA3*EL*SA2*dMH32OSAlter())/(C&
  &B*MW*SW) - (0.09375D0*CA1*CA3*EL*SA12*SA2*dMH32OSAlter())/(CB*MW*SW) - (0.28125D0*CA1*CA22*CA3*EL*SA12*SA2*dMH32OSAlter())/(CB&
  &*MW*SW) - (0.0625D0*EL*SA1*SA3*dMH32OSAlter())/(CB*MW*SW) - (0.1875D0*CA12*EL*SA1*SA3*dMH32OSAlter())/(CB*MW*SW) - (0.0625D0*C&
  &A22*EL*SA1*SA3*dMH32OSAlter())/(CB*MW*SW) - (0.1875D0*CA12*CA22*EL*SA1*SA3*dMH32OSAlter())/(CB*MW*SW) + (0.0625D0*EL*SA1*SA22*&
  &SA3*dMH32OSAlter())/(CB*MW*SW) + (0.1875D0*CA12*EL*SA1*SA22*SA3*dMH32OSAlter())/(CB*MW*SW) + (0.09375D0*CA3*EL*SA1*SA2*dMH32OS&
  &Alter())/(MW*SB*SW) - (0.09375D0*CA12*CA3*EL*SA1*SA2*dMH32OSAlter())/(MW*SB*SW) + (0.28125D0*CA22*CA3*EL*SA1*SA2*dMH32OSAlter(&
  &))/(MW*SB*SW) - (0.28125D0*CA12*CA22*CA3*EL*SA1*SA2*dMH32OSAlter())/(MW*SB*SW) + (0.0625D0*CA1*EL*SA3*dMH32OSAlter())/(MW*SB*S&
  &W) + (0.0625D0*CA1*CA22*EL*SA3*dMH32OSAlter())/(MW*SB*SW) + (0.1875D0*CA1*EL*SA12*SA3*dMH32OSAlter())/(MW*SB*SW) + (0.1875D0*C&
  &A1*CA22*EL*SA12*SA3*dMH32OSAlter())/(MW*SB*SW) - (0.0625D0*CA1*EL*SA22*SA3*dMH32OSAlter())/(MW*SB*SW) - (0.1875D0*CA1*EL*SA12*&
  &SA22*SA3*dMH32OSAlter())/(MW*SB*SW) - (0.25D0*CA2*CA3*dMH32OSAlter())/vS - (0.75D0*CA2*CA3*SA22*dMH32OSAlter())/vS + (0.03125D&
  &0*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) + (0.09375D0*CA22*CA3*EL*SA2*DBLE(CA1**INT(3.D0))*dMH32OSAlter())&
  &/(CB*MW*SW) - (0.0625D0*EL*SA3*DBLE(CA1**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) - (0.0625D0*CA22*EL*SA3*DBLE(CA1**INT(3.D0))*dM&
  &H32OSAlter())/(MW*SB*SW) + (0.0625D0*EL*SA22*SA3*DBLE(CA1**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) + (0.25D0*CA3*DBLE(CA2**INT(3&
  &.D0))*dMH32OSAlter())/vS + (0.0625D0*EL*SA3*DBLE(SA1**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) + (0.0625D0*CA22*EL*SA3*DBLE(SA1**&
  &INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) - (0.0625D0*EL*SA22*SA3*DBLE(SA1**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) + (0.03125D0*CA3&
  &*EL*SA2*DBLE(SA1**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) + (0.09375D0*CA22*CA3*EL*SA2*DBLE(SA1**INT(3.D0))*dMH32OSAlter())/(MW*&
  &SB*SW) - (0.09375D0*CA1*CA3*EL*DBLE(SA2**INT(3.D0))*dMH32OSAlter())/(CB*MW*SW) + (0.09375D0*CA1*CA3*EL*SA12*DBLE(SA2**INT(3.D0&
  &))*dMH32OSAlter())/(CB*MW*SW) - (0.09375D0*CA3*EL*SA1*DBLE(SA2**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) + (0.09375D0*CA12*CA3*EL&
  &*SA1*DBLE(SA2**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) - (0.03125D0*CA3*EL*DBLE(CA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH32OSAlte&
  &r())/(CB*MW*SW) - (0.03125D0*CA3*EL*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMH32OSAlter())/(MW*SB*SW) - (0.09375D0*CA1*CA3*&
  &EL*MH12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.28125D0*CA1*CA22*CA3*EL*MH12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/&
  &(CB*SW) - (0.046875D0*CA1*CA3*EL*MH32*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.140625D0*CA1*CA22*CA3*EL*MH32*SA2*DBL&
  &E(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.140625D0*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.&
  &421875D0*CA22*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.09375D0*CA1*CA3*EL*MH12*SA12*SA2*DBLE(M&
  &W**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.28125D0*CA1*CA22*CA3*EL*MH12*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.&
  &046875D0*CA1*CA3*EL*MH32*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*CA1*CA22*CA3*EL*MH32*SA12*SA2*DBLE(M&
  &W**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*&
  &CA1*CA22*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0625D0*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alte&
  &r())/(CB*SW) + (0.1875D0*CA12*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0625D0*CA22*EL*MH12*SA1*SA3*DBLE(&
  &MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.1875D0*CA12*CA22*EL*MH12*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.0312&
  &5D0*EL*MH32*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.09375D0*CA12*EL*MH32*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter&
  &())/(CB*SW) + (0.03125D0*CA22*EL*MH32*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.09375D0*CA12*CA22*EL*MH32*SA1*SA3&
  &*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.09375D0*CA1*EL*m12squared*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) -&
  & (0.0625D0*EL*MH12*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.1875D0*CA12*EL*MH12*SA1*SA22*SA3*DBLE(MW**INT(-&
  &3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*EL*MH32*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA12*EL*M&
  &H32*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.140625D0*CA1*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*dMW2Al&
  &ter())/(SB*SW) - (0.421875D0*CA1*CA22*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA1*CA3*EL*&
  &m12squared*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.28125D0*CA1*CA22*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*&
  &dMW2Alter())/(CB2*SB*SW) - (0.09375D0*CA3*EL*MH12*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA12*CA3*EL*M&
  &H12*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.28125D0*CA22*CA3*EL*MH12*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/&
  &(SB*SW) + (0.28125D0*CA12*CA22*CA3*EL*MH12*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*CA3*EL*MH32*SA1*SA2&
  &*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.046875D0*CA12*CA3*EL*MH32*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - &
  &(0.140625D0*CA22*CA3*EL*MH32*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.140625D0*CA12*CA22*CA3*EL*MH32*SA1*SA2*DBL&
  &E(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.140625D0*CA1*CA3*EL*m12squared*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*&
  &SW) - (0.421875D0*CA1*CA22*CA3*EL*m12squared*SA12*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.0625D0*CA1*EL*MH12*SA&
  &3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.0625D0*CA1*CA22*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.03&
  &125D0*CA1*EL*MH32*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.03125D0*CA1*CA22*EL*MH32*SA3*DBLE(MW**INT(-3.D0))*dMW2Alt&
  &er())/(SB*SW) + (0.09375D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA22*EL*m12squared*SA1&
  &*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.0625D0*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW)&
  & - (0.28125D0*CA12*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.0625D0*CA22*EL*m12squared*SA1*SA3*&
  &DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.28125D0*CA12*CA22*EL*m12squared*SA1*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(&
  &CB2*SB*SW) - (0.1875D0*CA1*EL*MH12*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.1875D0*CA1*CA22*EL*MH12*SA12*SA3*DB&
  &LE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*CA1*EL*MH32*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.09375&
  &D0*CA1*CA22*EL*MH32*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.0625D0*CA1*EL*MH12*SA22*SA3*DBLE(MW**INT(-3.D0))*d&
  &MW2Alter())/(SB*SW) + (0.03125D0*CA1*EL*MH32*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*EL*m12squared*SA1&
  &*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.0625D0*EL*m12squared*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(&
  &CB2*SB*SW) + (0.28125D0*CA12*EL*m12squared*SA1*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.1875D0*CA1*EL*MH12*&
  &SA12*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*CA1*EL*MH32*SA12*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter(&
  &))/(SB*SW) + (0.09375D0*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.140625D0*CA12*CA3*EL*m12s&
  &quared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.28125D0*CA22*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*&
  &dMW2Alter())/(CB*SB2*SW) - (0.421875D0*CA12*CA22*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.&
  &0625D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.0625D0*CA1*CA22*EL*m12squared*SA3*DBLE(MW**INT&
  &(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.28125D0*CA1*EL*m12squared*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.2&
  &8125D0*CA1*CA22*EL*m12squared*SA12*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.0625D0*CA1*EL*m12squared*SA22*SA3*DB&
  &LE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.28125D0*CA1*EL*m12squared*SA12*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB&
  &*SB2*SW) + (0.046875D0*CA3*EL*m12squared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.140625D0*CA22*CA3*EL*m12squ&
  &ared*SA1*SA2*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.03125D0*CA1*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())&
  &/(SB*SW*TB) + (0.03125D0*CA1*CA22*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) - (0.03125D0*CA1*EL*m12square&
  &d*SA22*SA3*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW*TB) + (0.046875D0*CA1*CA3*EL*m12squared*SA2*TB*DBLE(MW**INT(-3.D0))*dMW2Al&
  &ter())/(CB*SW) + (0.140625D0*CA1*CA22*CA3*EL*m12squared*SA2*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*EL*m12sq&
  &uared*SA1*SA3*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA22*EL*m12squared*SA1*SA3*TB*DBLE(MW**INT(-3.D0))*dMW&
  &2Alter())/(CB*SW) + (0.03125D0*EL*m12squared*SA1*SA22*SA3*TB*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA3*EL*MH1&
  &2*SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.09375D0*CA22*CA3*EL*MH12*SA2*DBLE(CA1**INT(3.D0))*DB&
  &LE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) - (0.015625D0*CA3*EL*MH32*SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(&
  &CB*SW) - (0.046875D0*CA22*CA3*EL*MH32*SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SW) + (0.046875D0*CA3*EL*&
  &m12squared*SA2*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.140625D0*CA22*CA3*EL*m12squared*SA2*DBL&
  &E(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.0625D0*EL*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D&
  &0))*dMW2Alter())/(SB*SW) + (0.0625D0*CA22*EL*MH12*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.0312&
  &5D0*EL*MH32*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) + (0.03125D0*CA22*EL*MH32*SA3*DBLE(CA1**INT(3.D&
  &0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.0625D0*EL*MH12*SA22*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter&
  &())/(SB*SW) - (0.03125D0*EL*MH32*SA22*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*EL*m12sq&
  &uared*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.09375D0*CA22*EL*m12squared*SA3*DBLE(CA1**INT&
  &(3.D0))*DBLE(MW**INT(-3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.09375D0*EL*m12squared*SA22*SA3*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3&
  &.D0))*dMW2Alter())/(CB*SB2*SW) - (0.0625D0*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.062&
  &5D0*CA22*EL*MH12*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*EL*MH32*SA3*DBLE(MW**INT(-3.D&
  &0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA22*EL*MH32*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alte&
  &r())/(CB*SW) + (0.0625D0*EL*MH12*SA22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*EL*MH32*&
  &SA22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.03125D0*CA3*EL*MH12*SA2*DBLE(MW**INT(-3.D0))*DBLE&
  &(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*CA22*CA3*EL*MH12*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())&
  &/(SB*SW) - (0.015625D0*CA3*EL*MH32*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*CA22*CA3*E&
  &L*MH32*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.09375D0*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*&
  &DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.09375D0*CA22*EL*m12squared*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dM&
  &W2Alter())/(CB2*SB*SW) - (0.09375D0*EL*m12squared*SA22*SA3*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) &
  &+ (0.046875D0*CA3*EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.140625D0*CA22*CA3*&
  &EL*m12squared*SA2*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.09375D0*CA1*CA3*EL*MH12*DBLE(MW**INT&
  &(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.046875D0*CA1*CA3*EL*MH32*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW&
  &2Alter())/(CB*SW) + (0.140625D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.0937&
  &5D0*CA1*CA3*EL*MH12*SA12*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) - (0.046875D0*CA1*CA3*EL*MH32*SA12*DBL&
  &E(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) + (0.140625D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*DBLE(SA2*&
  &*INT(3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*CA1*CA3*EL*m12squared*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB&
  &2*SB*SW) + (0.09375D0*CA3*EL*MH12*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.09375D0*CA12*CA3*EL*&
  &MH12*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.046875D0*CA3*EL*MH32*SA1*DBLE(MW**INT(-3.D0))*DBL&
  &E(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*CA12*CA3*EL*MH32*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter(&
  &))/(SB*SW) + (0.140625D0*CA1*CA3*EL*m12squared*SA12*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) - (0.09&
  &375D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + (0.140625D0*CA12*CA3*EL*m12sq&
  &uared*SA1*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) - (0.046875D0*CA3*EL*m12squared*SA1*DBLE(MW**INT(&
  &-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW*TB) - (0.046875D0*CA1*CA3*EL*m12squared*TB*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT&
  &(3.D0))*dMW2Alter())/(CB*SW) + (0.03125D0*CA3*EL*MH12*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter&
  &())/(CB*SW) + (0.015625D0*CA3*EL*MH32*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SW) - (0&
  &.046875D0*CA3*EL*m12squared*DBLE(CA1**INT(3.D0))*DBLE(MW**INT(-3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB2*SB*SW) + (0.03125&
  &D0*CA3*EL*MH12*DBLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) + (0.015625D0*CA3*EL*MH32*D&
  &BLE(MW**INT(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(SB*SW) - (0.046875D0*CA3*EL*m12squared*DBLE(MW**IN&
  &T(-3.D0))*DBLE(SA1**INT(3.D0))*DBLE(SA2**INT(3.D0))*dMW2Alter())/(CB*SB2*SW) + 0.5D0*CA2*CA3*MH12*DBLE(vS**INT(-2.D0))*dvSMSBa&
  &rAlter() + 0.25D0*CA2*CA3*MH32*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() + 1.5D0*CA2*CA3*MH12*SA22*DBLE(vS**INT(-2.D0))*dvSMSBarAlt&
  &er() + 0.75D0*CA2*CA3*MH32*SA22*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() - 0.5D0*CA3*MH12*DBLE(CA2**INT(3.D0))*DBLE(vS**INT(-2.D0)&
  &)*dvSMSBarAlter() - 0.25D0*CA3*MH32*DBLE(CA2**INT(3.D0))*DBLE(vS**INT(-2.D0))*dvSMSBarAlter() &
		& + CS1S1S1f111*dZH1H3OSAlter()/2D0 + CS1S1S1f211*dZH2H3OSAlter()/2D0 + CS1S1S1f311*dZH3H3OS()/2D0 &
		& + CS1S1S1f131*dZH1H1OS()/2D0 + CS1S1S1f231*dZH2H1OSAlter()/2D0 + CS1S1S1f331*dZH3H1OSAlter()/2D0 &
		& + CS1S1S1f131*dZH1H1OS()/2D0 + CS1S1S1f231*dZH2H1OSAlter()/2D0 + CS1S1S1f331*dZH3H1OSAlter()/2D0 &
		& )
	case default
		totalAmplitude = 0D0
 end select

 H3toH1H1CT = totalAmplitude
end function H3toH1H1CT
