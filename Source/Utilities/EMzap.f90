! ###################################################################
! Copyright (c) 2013-2025, Marc De Graef Research Group/Carnegie Mellon University
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without modification, are 
! permitted provided that the following conditions are met:
!
!     - Redistributions of source code must retain the above copyright notice, this list 
!        of conditions and the following disclaimer.
!     - Redistributions in binary form must reproduce the above copyright notice, this 
!        list of conditions and the following disclaimer in the documentation and/or 
!        other materials provided with the distribution.
!     - Neither the names of Marc De Graef, Carnegie Mellon University nor the names 
!        of its contributors may be used to endorse or promote products derived from 
!        this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
! ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE 
! LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER 
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, 
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE 
! USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
! ###################################################################

program EMzap
  !! author: MDG
  !! version: 1.0 
  !! date: 01/28/20
  !!
  !! PostScript output of kinematical zone axis diffraction patterns

use mod_kinds
use mod_global
use mod_EMsoft
use mod_crystallography
use mod_symmetry
use mod_io
use mod_postscript
use mod_diffraction
use mod_HDFsupport

character(fnlen)        :: progname = 'EMzap.f90'
character(fnlen)        :: progdesc = 'Kinematical Zone Axis Diffraction Patterns'

type(EMsoft_T)          :: EMsoft 
type(IO_T)              :: Message 
type(Cell_T)            :: cell
type(SpaceGroup_T)      :: SG 
type(Diffraction_T)     :: Diff
type(PostScript_T)      :: PS 

real(kind=sgl)          :: io_real(1), camlen
character(fnlen)        :: xtalname
integer(kind=irg)       :: imanum

! progran header and command line argument handling 
EMsoft = EMsoft_T(progname, progdesc, tpl = (/ 928 /) )

call openFortranHDFInterface()

! ask for the crystal structure file
call Message%ReadValue(' Enter xtal file name : ', xtalname,"(A)")
call cell%getCrystalData(xtalname, SG, EMsoft, verbose=.TRUE.)

call Diff%setrlpmethod('WK')
call Diff%getVoltage(cell, verbose=.TRUE.) 

call Message%ReadValue(' Camera Length [mm, R] : ', io_real, 1)
camlen = io_real(1)

! open PostScript file
imanum = 1
PS = PostScript_T(progdesc, EMsoft, imanum)
call PS%setpspage(0)
call PS%DiffPage(cell, SG, Diff, camlen)

! close Postscript file
call PS%closefile()

end program EMzap
