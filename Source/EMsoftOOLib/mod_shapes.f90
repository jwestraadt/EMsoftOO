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

module mod_shapes
  !! author: MDG 
  !! version: 1.0 
  !! date: 01/22/20
  !!
  !! class definition for the EMshape program

use mod_kinds
use mod_global

IMPLICIT NONE 

! namelist for the EMshape program
type, public :: shapeNameListType
end type shapeNameListType

! class definition
type, public :: shape_T
private 
  character(fnlen)       :: nmldeffile = 'EMshape.nml'
  type(shapeNameListType)  :: nml 

contains
private 
  procedure, pass(self) :: readNameList_
  procedure, pass(self) :: writeHDFNameList_
  procedure, pass(self) :: getNameList_
  procedure, pass(self) :: shape_

  generic, public :: getNameList => getNameList_
  generic, public :: writeHDFNameList => writeHDFNameList_
  generic, public :: readNameList => readNameList_
  generic, public :: shape => shape_

end type shape_T

! the constructor routine for this class 
interface shape_T
  module procedure shape_constructor
end interface shape_T

contains

!--------------------------------------------------------------------------
type(shape_T) function shape_constructor( nmlfile ) result(shape)
!! author: MDG 
!! version: 1.0 
!! date: 01/22/20
!!
!! constructor for the shape_T Class; reads the name list 
 
IMPLICIT NONE

character(fnlen), OPTIONAL   :: nmlfile 

call shape%readNameList(nmlfile)

end function shape_constructor

!--------------------------------------------------------------------------
subroutine shape_destructor(self) 
!! author: MDG 
!! version: 1.0 
!! date: 01/22/20
!!
!! destructor for the shape_T Class
 
IMPLICIT NONE

type(shape_T), INTENT(INOUT)  :: self 

call reportDestructor('shape_T')

end subroutine shape_destructor

!--------------------------------------------------------------------------
subroutine readNameList_(self, nmlfile, initonly)
!! author: MDG 
!! version: 1.0 
!! date: 01/22/20
!!
!! read the namelist from an nml file for the shape_T Class 

use mod_io 
use mod_EMsoft

IMPLICIT NONE 

class(shape_T), INTENT(INOUT)          :: self
character(fnlen),INTENT(IN)          :: nmlfile
 !! full path to namelist file 
logical,OPTIONAL,INTENT(IN)          :: initonly
 !! fill in the default values only; do not read the file

type(EMsoft_T)                       :: EMsoft 
type(IO_T)                           :: Message       
logical                              :: skipread = .FALSE.



end subroutine readNameList_

!--------------------------------------------------------------------------
function getNameList_(self) result(nml)
!! author: MDG 
!! version: 1.0 
!! date: 01/22/20
!!
!! pass the namelist for the shape_T Class to the calling program

IMPLICIT NONE 

class(shape_T), INTENT(INOUT)          :: self
type(shapeNameListType)                :: nml

nml = self%nml

end function getNameList_

!--------------------------------------------------------------------------
recursive subroutine writeHDFNameList_(self, HDF, HDFnames)
!! author: MDG 
!! version: 1.0 
!! date: 01/22/20
!!
!! write namelist to HDF file

use mod_HDFsupport
use mod_HDFnames
use stringconstants 

use ISO_C_BINDING

IMPLICIT NONE

class(shape_T), INTENT(INOUT)        :: self 
type(HDF_T), INTENT(INOUT)              :: HDF
type(HDFnames_T), INTENT(INOUT)         :: HDFnames

integer(kind=irg),parameter             :: n_int = 11, n_real = 9
integer(kind=irg)                       :: hdferr,  io_int(n_int)
real(kind=sgl)                          :: io_real(n_real)
character(20)                           :: intlist(n_int), reallist(n_real)
character(fnlen)                        :: dataset, sval(1),groupname
character(fnlen,kind=c_char)            :: line2(1)

associate( mcnl => self%nml )

end associate

end subroutine writeHDFNameList_

!--------------------------------------------------------------------------
subroutine shape_(self, EMsoft, progname)
!! author: MDG 
!! version: 1.0 
!! date: 01/22/20
!!
!! perform the computations

use mod_EMsoft

IMPLICIT NONE 

class(shape_T), INTENT(INOUT)       :: self
type(EMsoft_T), INTENT(INOUT)           :: EMsoft
character(fnlen), INTENT(INOUT)         :: progname 


end subroutine shape_



end module mod_shapes
