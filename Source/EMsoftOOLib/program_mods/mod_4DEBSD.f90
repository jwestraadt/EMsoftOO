! ###################################################################
! Copyright (c) 2013-2024, Marc De Graef Research Group/Carnegie Mellon University
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

module mod_4DEBSD
  !! author: MDG 
  !! version: 1.0 
  !! date: 04/01/24
  !!
  !! class definition for the EM4DEBSD program

use mod_kinds
use mod_global

IMPLICIT NONE 

! namelist for the EMEBSD4D program
type, public :: EBSD4DNameListType
 integer(kind=irg)  :: ipf_ht
 integer(kind=irg)  :: ipf_wd
 integer(kind=irg)  :: ROI(4)
 integer(kind=irg)  :: numsx
 integer(kind=irg)  :: numsy
 integer(kind=irg)  :: nthreads
 integer(kind=irg)  :: VDwidth
 integer(kind=irg)  :: VDheight
 character(fnlen)   :: exptfile
 character(fnlen)   :: masterfile
 character(fnlen)   :: dotproductfile
 character(fnlen)   :: inputtype
 character(fnlen)   :: HDFstrings(10)
 character(fnlen)   :: virtualimagefile
 character(4)       :: VDtype
 character(4)       :: VDreference
 real(kind=sgl)     :: VDlocx
 real(kind=sgl)     :: VDlocy
 real(kind=sgl)     :: VDSD
 real(kind=sgl)     :: VDHannAlpha
end type EBSD4DNameListType

! class definition
type, public :: EBSD4D_T
private 
  character(fnlen)          :: nmldeffile = 'EM4DEBSD.nml'
  type(EBSD4DNameListType)  :: nml 

contains
private 
  procedure, pass(self) :: readNameList_
  procedure, pass(self) :: getNameList_
  procedure, pass(self) :: EBSD4D_
  procedure, pass(self) :: setipf_ht_
  procedure, pass(self) :: getipf_ht_
  procedure, pass(self) :: setipf_wd_
  procedure, pass(self) :: getipf_wd_
  procedure, pass(self) :: setROI_
  procedure, pass(self) :: getROI_
  procedure, pass(self) :: setnumsx_
  procedure, pass(self) :: getnumsx_
  procedure, pass(self) :: setnumsy_
  procedure, pass(self) :: getnumsy_
  procedure, pass(self) :: setnthreads_
  procedure, pass(self) :: getnthreads_
  procedure, pass(self) :: setexptfile_
  procedure, pass(self) :: getexptfile_
  procedure, pass(self) :: setmasterfile_
  procedure, pass(self) :: getmasterfile_
  procedure, pass(self) :: setdotproductfile_
  procedure, pass(self) :: getdotproductfile_
  procedure, pass(self) :: setinputtype_
  procedure, pass(self) :: getinputtype_
  procedure, pass(self) :: setHDFstrings_
  procedure, pass(self) :: getHDFstrings_
  procedure, pass(self) :: setvirtualimagefile_
  procedure, pass(self) :: getvirtualimagefile_
  procedure, pass(self) :: setVDtype_
  procedure, pass(self) :: getVDtype_
  procedure, pass(self) :: setVDreference_
  procedure, pass(self) :: getVDreference_
  procedure, pass(self) :: setVDlocx_
  procedure, pass(self) :: getVDlocx_
  procedure, pass(self) :: setVDlocy_
  procedure, pass(self) :: getVDlocy_
  procedure, pass(self) :: setVDSD_
  procedure, pass(self) :: getVDSD_
  procedure, pass(self) :: setVDHannAlpha_
  procedure, pass(self) :: getVDHannAlpha_
  procedure, pass(self) :: setVDwidth_
  procedure, pass(self) :: getVDwidth_
  procedure, pass(self) :: setVDheight_
  procedure, pass(self) :: getVDheight_

  generic, public :: getNameList => getNameList_
  generic, public :: readNameList => readNameList_
  generic, public :: EBSD4D => EBSD4D_
  generic, public :: setipf_ht => setipf_ht_
  generic, public :: getipf_ht => getipf_ht_
  generic, public :: setipf_wd => setipf_wd_
  generic, public :: getipf_wd => getipf_wd_
  generic, public :: setROI => setROI_
  generic, public :: getROI => getROI_
  generic, public :: setnumsx => setnumsx_
  generic, public :: getnumsx => getnumsx_
  generic, public :: setnumsy => setnumsy_
  generic, public :: getnumsy => getnumsy_
  generic, public :: setnthreads => setnthreads_
  generic, public :: getnthreads => getnthreads_
  generic, public :: setexptfile => setexptfile_
  generic, public :: getexptfile => getexptfile_
  generic, public :: setdotproductfile => setdotproductfile_
  generic, public :: getdotproductfile => getdotproductfile_
  generic, public :: setmasterfile => setmasterfile_
  generic, public :: getmasterfile => getmasterfile_
  generic, public :: setinputtype => setinputtype_
  generic, public :: getinputtype => getinputtype_
  generic, public :: setHDFstrings => setHDFstrings_
  generic, public :: getHDFstrings => getHDFstrings_
  generic, public :: setvirtualimagefile => setvirtualimagefile_
  generic, public :: getvirtualimagefile => getvirtualimagefile_
  generic, public :: setVDtype => setVDtype_
  generic, public :: getVDtype => getVDtype_
  generic, public :: setVDreference => setVDreference_
  generic, public :: getVDreference => getVDreference_
  generic, public :: setVDlocx => setVDlocx_
  generic, public :: getVDlocx => getVDlocx_
  generic, public :: setVDlocy => setVDlocy_
  generic, public :: getVDlocy => getVDlocy_
  generic, public :: setVDSD => setVDSD_
  generic, public :: getVDSD => getVDSD_
  generic, public :: setVDHannAlpha => setVDHannAlpha_
  generic, public :: getVDHannAlpha => getVDHannAlpha_
  generic, public :: setVDwidth => setVDwidth_
  generic, public :: getVDwidth => getVDwidth_
  generic, public :: setVDheight => setVDheight_
  generic, public :: getVDheight => getVDheight_
end type EBSD4D_T

! the constructor routine for this class 
interface EBSD4D_T
  module procedure EBSD4D_constructor
end interface EBSD4D_T

contains

!--------------------------------------------------------------------------
type(EBSD4D_T) function EBSD4D_constructor( nmlfile ) result(EBSD4D)
!! author: MDG 
!! version: 1.0 
!! date: 04/01/24
!!
!! constructor for the EBSD4D_T Class; reads the name list 
 
IMPLICIT NONE

character(fnlen), OPTIONAL   :: nmlfile 

call EBSD4D%readNameList(nmlfile)

end function EBSD4D_constructor

!--------------------------------------------------------------------------
subroutine EBSD4D_destructor(self) 
!! author: MDG 
!! version: 1.0 
!! date: 04/01/24
!!
!! destructor for the EBSD4D_T Class
 
IMPLICIT NONE

type(EBSD4D_T), INTENT(INOUT)  :: self 

call reportDestructor('EBSD4D_T')

end subroutine EBSD4D_destructor

!--------------------------------------------------------------------------
subroutine readNameList_(self, nmlfile, initonly)
!DEC$ ATTRIBUTES DLLEXPORT :: readNameList_
!! author: MDG 
!! version: 1.0 
!! date: 04/01/24
!!
!! read the namelist from an nml file for the EBSD4D_T Class 

use mod_io 
use mod_EMsoft

IMPLICIT NONE 

class(EBSD4D_T), INTENT(INOUT)      :: self
character(fnlen),INTENT(IN)         :: nmlfile
 !! full path to namelist file 
logical,OPTIONAL,INTENT(IN)         :: initonly
 !! fill in the default values only; do not read the file

type(EMsoft_T)                      :: EMsoft 
type(IO_T)                          :: Message       
logical                             :: skipread = .FALSE.

integer(kind=irg)                   :: ipf_ht
integer(kind=irg)                   :: ipf_wd
integer(kind=irg)                   :: ROI(4)
integer(kind=irg)                   :: numsx
integer(kind=irg)                   :: numsy
integer(kind=irg)                   :: nthreads
character(fnlen)                    :: exptfile
character(fnlen)                    :: masterfile
character(fnlen)                    :: dotproductfile
character(fnlen)                    :: inputtype
character(fnlen)                    :: HDFstrings(10)
character(fnlen)                    :: virtualimagefile
integer(kind=irg)                   :: VDwidth
integer(kind=irg)                   :: VDheight
character(4)                        :: VDtype
character(4)                        :: VDreference
real(kind=sgl)                      :: VDlocx
real(kind=sgl)                      :: VDlocy
real(kind=sgl)                      :: VDSD
real(kind=sgl)                      :: VDHannAlpha

namelist / EBSD4Ddata / ipf_ht, ipf_wd, ROI, numsx, numsy, nthreads, exptfile, inputtype, HDFstrings, virtualimagefile, &
                        VDwidth, VDheight, VDtype, VDlocx, VDlocy, VDSD, VDHannAlpha, VDreference, masterfile, dotproductfile

ipf_ht = 100
ipf_wd = 100
ROI = (/ 0, 0, 0, 0 /)
numsx = 0
numsy = 0
nthreads = 1
exptfile = 'undefined'
masterfile = 'undefined'
dotproductfile = 'undefined'
inputtype = 'Binary'
HDFstrings = (/ '', '', '', '', '', '', '', '', '', '' /)
virtualimagefile = 'undefined'
VDtype = 'Rect'
VDreference = 'EBSP'
VDlocx = 0.0 
VDlocy = 0.0
VDwidth = 5
VDheight = 5
VDSD = 0.5
VDHannAlpha = 0.5 

if (present(initonly)) then
  if (initonly) skipread = .TRUE.
end if

if (.not.skipread) then
! read the namelist file
 open(UNIT=dataunit,FILE=trim(nmlfile),DELIM='apostrophe',STATUS='old')
 read(UNIT=dataunit,NML=EBSD4Ddata)
 close(UNIT=dataunit,STATUS='keep')

! check for required entries
 if (trim(exptfile).eq.'undefined') then
  call Message%printError('readNameList:',' exptfile file name is undefined in '//nmlfile)
 end if

 if (trim(virtualimagefile).eq.'undefined') then
  call Message%printError('readNameList:',' virtualimagefile file name is undefined in '//nmlfile)
 end if

 if (numsx.eq.0) then
  call Message%printError('readNameList:',' pattern size numsx is zero '//nmlfile)
 end if

 if (numsy.eq.0) then
  call Message%printError('readNameList:',' pattern size numsy is zero '//nmlfile)
 end if

 if (VDreference.eq.'MPat') then 
   if (trim(masterfile).eq.'undefined') then
    call Message%printError('readNameList:',' masterfile file name is undefined in '//nmlfile)
   end if
   if (trim(dotproductfile).eq.'undefined') then
    call Message%printError('readNameList:',' dotproductfile file name is undefined in '//nmlfile)
   end if
 end if
end if

self%nml%ipf_ht = ipf_ht
self%nml%ipf_wd = ipf_wd
self%nml%ROI = ROI
self%nml%numsx = numsx
self%nml%numsy = numsy
self%nml%nthreads = nthreads
self%nml%exptfile = exptfile
self%nml%masterfile = masterfile
self%nml%dotproductfile = dotproductfile
self%nml%inputtype = inputtype
self%nml%HDFstrings = HDFstrings
self%nml%virtualimagefile = virtualimagefile
self%nml%VDwidth = VDwidth
self%nml%VDheight = VDheight
self%nml%VDtype = VDtype
self%nml%VDreference = VDreference
self%nml%VDlocx = VDlocx
self%nml%VDlocy = VDlocy
self%nml%VDSD = VDSD 
self%nml%VDHannAlpha = VDHannAlpha

end subroutine readNameList_

!--------------------------------------------------------------------------
function getNameList_(self) result(nml)
!DEC$ ATTRIBUTES DLLEXPORT :: getNameList_
!! author: MDG 
!! version: 1.0 
!! date: 04/01/24
!!
!! pass the namelist for the EBSD4D_T Class to the calling program

IMPLICIT NONE 

class(EBSD4D_T), INTENT(INOUT)   :: self
type(EBSD4DNameListType)         :: nml

nml = self%nml

end function getNameList_

!--------------------------------------------------------------------------
subroutine setipf_ht_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setipf_ht_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! set ipf_ht in the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
integer(kind=irg), INTENT(IN)       :: inp

self%nml%ipf_ht = inp

end subroutine setipf_ht_

!--------------------------------------------------------------------------
function getipf_ht_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getipf_ht_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! get ipf_ht from the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
integer(kind=irg)                   :: out

out = self%nml%ipf_ht

end function getipf_ht_

!--------------------------------------------------------------------------
subroutine setipf_wd_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setipf_wd_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! set ipf_wd in the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
integer(kind=irg), INTENT(IN)       :: inp

self%nml%ipf_wd = inp

end subroutine setipf_wd_

!--------------------------------------------------------------------------
function getipf_wd_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getipf_wd_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! get ipf_wd from the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
integer(kind=irg)                   :: out

out = self%nml%ipf_wd

end function getipf_wd_

!--------------------------------------------------------------------------
subroutine setROI_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setROI_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! set ROI in the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
integer(kind=irg), INTENT(IN)      :: inp(4)

self%nml%ROI = inp

end subroutine setROI_

!--------------------------------------------------------------------------
function getROI_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getROI_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! get ROI from the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
integer(kind=irg)                  :: out(4)

out = self%nml%ROI

end function getROI_

!--------------------------------------------------------------------------
subroutine setnumsx_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setnumsx_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! set numsx in the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
integer(kind=irg), INTENT(IN)       :: inp

self%nml%numsx = inp

end subroutine setnumsx_

!--------------------------------------------------------------------------
function getnumsx_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getnumsx_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! get numsx from the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
integer(kind=irg)                   :: out

out = self%nml%numsx

end function getnumsx_

!--------------------------------------------------------------------------
subroutine setnumsy_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setnumsy_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! set numsy in the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
integer(kind=irg), INTENT(IN)       :: inp

self%nml%numsy = inp

end subroutine setnumsy_

!--------------------------------------------------------------------------
function getnumsy_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getnumsy_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! get numsy from the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
integer(kind=irg)                   :: out

out = self%nml%numsy

end function getnumsy_

!--------------------------------------------------------------------------
subroutine setnthreads_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setnthreads_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! set nthreads in the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
integer(kind=irg), INTENT(IN)       :: inp

self%nml%nthreads = inp

end subroutine setnthreads_

!--------------------------------------------------------------------------
function getnthreads_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getnthreads_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! get nthreads from the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
integer(kind=irg)                   :: out

out = self%nml%nthreads

end function getnthreads_

!--------------------------------------------------------------------------
subroutine setexptfile_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setexptfile_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! set exptfile in the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
character(fnlen), INTENT(IN)       :: inp

self%nml%exptfile = trim(inp)

end subroutine setexptfile_

!--------------------------------------------------------------------------
function getexptfile_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getexptfile_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! get exptfile from the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
character(fnlen)                   :: out

out = trim(self%nml%exptfile)

end function getexptfile_

!--------------------------------------------------------------------------
subroutine setmasterfile_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setmasterfile_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! set masterfile in the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
character(fnlen), INTENT(IN)       :: inp

self%nml%masterfile = trim(inp)

end subroutine setmasterfile_

!--------------------------------------------------------------------------
function getmasterfile_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getmasterfile_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! get masterfile from the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
character(fnlen)                   :: out

out = trim(self%nml%masterfile)

end function getmasterfile_

!--------------------------------------------------------------------------
subroutine setdotproductfile_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setdotproductfile_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! set dotproductfile in the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
character(fnlen), INTENT(IN)       :: inp

self%nml%dotproductfile = trim(inp)

end subroutine setdotproductfile_

!--------------------------------------------------------------------------
function getdotproductfile_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getdotproductfile_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! get dotproductfile from the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
character(fnlen)                   :: out

out = trim(self%nml%dotproductfile)

end function getdotproductfile_

!--------------------------------------------------------------------------
subroutine setinputtype_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setinputtype_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! set inputtype in the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
character(fnlen), INTENT(IN)       :: inp

self%nml%inputtype = trim(inp)

end subroutine setinputtype_

!--------------------------------------------------------------------------
function getinputtype_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getinputtype_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! get inputtype from the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
character(fnlen)                   :: out

out = trim(self%nml%inputtype)

end function getinputtype_

!--------------------------------------------------------------------------
subroutine setHDFstrings_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setHDFstrings_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! set HDFstrings in the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
character(fnlen), INTENT(IN)       :: inp(10)

integer(kind=irg)                  :: i 

do i=1,10 
  self%nml%HDFstrings(i) = trim(inp(i))
end do 

end subroutine setHDFstrings_

!--------------------------------------------------------------------------
function getHDFstrings_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getHDFstrings_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! get HDFstrings from the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
character(fnlen)                   :: out(10)

integer(kind=irg)                  :: i 

do i=1,10 
  out(i) = trim(self%nml%HDFstrings(i))
end do 

end function getHDFstrings_

!--------------------------------------------------------------------------
subroutine setvirtualimagefile_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setvirtualimagefile_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! set virtualimagefile in the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
character(fnlen), INTENT(IN)       :: inp

self%nml%virtualimagefile = trim(inp)

end subroutine setvirtualimagefile_

!--------------------------------------------------------------------------
function getvirtualimagefile_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getvirtualimagefile_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! get virtualimagefile from the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
character(fnlen)                   :: out

out = trim(self%nml%virtualimagefile)

end function getvirtualimagefile_

!--------------------------------------------------------------------------
subroutine setVDtype_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setVDtype_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! set VDtype in the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
character(4), INTENT(IN)       :: inp

self%nml%VDtype = trim(inp)

end subroutine setVDtype_

!--------------------------------------------------------------------------
function getVDtype_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getVDtype_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! get VDtype from the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
character(4)                   :: out

out = trim(self%nml%VDtype)

end function getVDtype_

!--------------------------------------------------------------------------
subroutine setVDreference_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setVDreference_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! set VDreference in the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
character(4), INTENT(IN)       :: inp

self%nml%VDreference = trim(inp)

end subroutine setVDreference_

!--------------------------------------------------------------------------
function getVDreference_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getVDreference_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! get VDreference from the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
character(4)                   :: out

out = trim(self%nml%VDreference)

end function getVDreference_

!--------------------------------------------------------------------------
subroutine setVDlocx_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setVDlocx_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! set VDlocx in the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
real(kind=sgl), INTENT(IN)       :: inp

self%nml%VDlocx = inp

end subroutine setVDlocx_

!--------------------------------------------------------------------------
function getVDlocx_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getVDlocx_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! get VDlocx from the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
real(kind=sgl)                   :: out

out = self%nml%VDlocx

end function getVDlocx_

!--------------------------------------------------------------------------
subroutine setVDlocy_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setVDlocy_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! set VDlocy in the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
real(kind=sgl), INTENT(IN)       :: inp

self%nml%VDlocy = inp

end subroutine setVDlocy_

!--------------------------------------------------------------------------
function getVDlocy_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getVDlocy_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! get VDlocy from the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
real(kind=sgl)                   :: out

out = self%nml%VDlocy

end function getVDlocy_

!--------------------------------------------------------------------------
subroutine setVDSD_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setVDSD_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! set VDSD in the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
real(kind=sgl), INTENT(IN)       :: inp

self%nml%VDSD = inp

end subroutine setVDSD_

!--------------------------------------------------------------------------
function getVDSD_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getVDSD_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! get VDSD from the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
real(kind=sgl)                   :: out

out = self%nml%VDSD

end function getVDSD_

!--------------------------------------------------------------------------
subroutine setVDHannAlpha_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setVDHannAlpha_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! set VDHannAlpha in the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
real(kind=sgl), INTENT(IN)       :: inp

self%nml%VDHannAlpha = inp

end subroutine setVDHannAlpha_

!--------------------------------------------------------------------------
function getVDHannAlpha_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getVDHannAlpha_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! get VDHannAlpha from the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
real(kind=sgl)                   :: out

out = self%nml%VDHannAlpha

end function getVDHannAlpha_

!--------------------------------------------------------------------------
subroutine setVDwidth_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setVDwidth_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! set VDwidth in the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
integer(kind=irg), INTENT(IN)       :: inp

self%nml%VDwidth = inp

end subroutine setVDwidth_

!--------------------------------------------------------------------------
function getVDwidth_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getVDwidth_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! get VDwidth from the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
integer(kind=irg)                   :: out

out = self%nml%VDwidth

end function getVDwidth_

!--------------------------------------------------------------------------
subroutine setVDheight_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setVDheight_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! set VDheight in the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
integer(kind=irg), INTENT(IN)       :: inp

self%nml%VDheight = inp

end subroutine setVDheight_

!--------------------------------------------------------------------------
function getVDheight_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getVDheight_
!! author: MDG
!! version: 1.0
!! date: 04/01/24
!!
!! get VDheight from the EBSD4D_T class

IMPLICIT NONE

class(EBSD4D_T), INTENT(INOUT)     :: self
integer(kind=irg)                   :: out

out = self%nml%VDheight

end function getVDheight_

!--------------------------------------------------------------------------
subroutine EBSD4D_(self, EMsoft, progname, HDFnames)
!DEC$ ATTRIBUTES DLLEXPORT :: EBSD4D_
!! author: MDG 
!! version: 1.0 
!! date: 04/01/24
!!
!! perform the computations

use mod_EMsoft
use mod_HDFnames
use mod_patterns
use mod_vendors
use mod_filters
use mod_io
use mod_math
use mod_timing
use omp_lib
use mod_OMPsupport
use HDF5
use h5im
use h5lt
use mod_HDFsupport
use ISO_C_BINDING
use mod_DIfiles
use mod_image
use mod_NLPAR
use mod_memory

use, intrinsic :: iso_fortran_env

IMPLICIT NONE 

class(EBSD4D_T), INTENT(INOUT)          :: self
type(EMsoft_T), INTENT(INOUT)           :: EMsoft
character(fnlen), INTENT(INOUT)         :: progname 
type(HDFnames_T), INTENT(INOUT)         :: HDFnames

type(IO_T)                              :: Message
type(HDF_T)                             :: HDF
type(timing_T)                          :: timer
type(NLPAR_T)                           :: NLPAR 
type(memory_T)                          :: mem
type(Vendor_T)                          :: VT

integer(kind=irg)                       :: L,totnumexpt,imght,imgwd, recordsize, hdferr, TID, iii, VDposx, VDposy, &
                                           TIFF_nx, TIFF_ny, itype, istat, iiistart, iiiend, jjstart, jjend, binx, biny, &
                                           correctsize, dims(2), i, j, ii, jj, jjj, kk, patsz
logical                                 :: ROIselected
real(kind=sgl),allocatable              :: VDimage(:,:), exppatarray(:), VDmask(:,:), mask(:,:), Pat(:,:), window(:,:)
real(kind=sgl)                          :: mi, ma
integer(HSIZE_T)                        :: dims3(3), offset3(3)
character(fnlen)                        :: fname, TIFF_filename 

! declare variables for use in object oriented image module
integer                                 :: iostat
character(len=128)                      :: iomsg
logical                                 :: isInteger
type(image_t)                           :: im
integer(int8)                           :: i8 (3,4)
integer(int8), allocatable              :: TIFF_image(:,:)

! this program applies a virtual detector (for now a square detector) to 
! an EBSD data set and returns the virtual image.  The code is somewhat 
! similar to the mod_ADP module in that it reads blocks of pattern rows
! to be more memory efficient ... 

call openFortranHDFInterface()

associate( nml=>self%nml )

! memory class 
mem = memory_T()

! copy various constants from the namelist
L = nml%numsx*nml%numsy
totnumexpt = nml%ipf_wd*nml%ipf_ht
imght = nml%numsx
imgwd = nml%numsy
recordsize = L*4
dims = (/ imght, imgwd /)
binx = nml%numsx
biny = nml%numsy

! make sure that correctsize is a multiple of 16; if not, make it so
! this is not really relevant for this program, but several routines 
! rely on this being the case so we impose it here
if (mod(L,16) .ne. 0) then
    correctsize = 16*ceiling(float(L)/16.0)
else
    correctsize = L
end if
patsz = correctsize

ROIselected = .FALSE.
if (sum(nml%ROI).ne.0) ROIselected = .TRUE.

!===================================================================================
! set the vendor inputtype for the pattern file
VT = Vendor_T( nml%inputtype )
itype = VT%get_itype()
call VT%set_filename( nml%exptfile )

!===================================================================================
! open the file with experimental patterns; depending on the inputtype parameter, this
! can be a regular binary file, as produced by a MatLab or IDL script (default); a
! pattern file produced by EMEBSD.f90 etc.; or a vendor binary or HDF5 file... in each case we need to
! open the file and leave it open, then use the getExpPatternRow() routine to read a row
! of patterns into the exppatarray variable ...  at the end, we use closeExpPatternFile() to
! properly close the experimental pattern file
if ( (itype.eq.4) .or. (itype.eq.6) .or. (itype.eq.7) .or. (itype.eq.8) ) then
  HDF = HDF_T()
  istat = VT%openExpPatternFile(EMsoft, nml%ipf_wd, L, recordsize, nml%HDFstrings, HDF)
else
  istat = VT%openExpPatternFile(EMsoft, nml%ipf_wd, L, recordsize)
end if

if (istat.ne.0) then
    call Message%printError("PreProcessPatterns:", "Fatal error handling experimental pattern file")
end if

if (sum(nml%ROI).ne.0) then
  ROIselected = .TRUE.
  iiistart = nml%ROI(2)
  iiiend = nml%ROI(2)+nml%ROI(4)-1
  jjstart = nml%ROI(1)
  jjend = nml%ROI(1)+nml%ROI(3)-1
else
  ROIselected = .FALSE.
  iiistart = 1
  iiiend = nml%ipf_ht
  jjstart = 1
  jjend = nml%ipf_wd
end if

! allocate the output image array
if (ROIselected.eqv..TRUE.) then
  call mem%alloc(VDimage, (/ nml%ROI(3), nml%ROI(4) /), 'VDimage')
else
  call mem%alloc(VDimage, (/ nml%ipf_wd, nml%ipf_ht /), 'VDimage')
end if
call mem%alloc(VDmask, (/ nml%VDwidth, nml%VDheight /), 'VDmask')
call mem%alloc(exppatarray, (/ patsz * nml%ipf_wd /), 'exppatarray')
call mem%alloc(mask, (/ nml%VDwidth, nml%VDheight /), 'mask', initval=0.0)

! make the mask according to the virtual detector type 
if ( nml%VDtype.eq.'Rect' ) then 
  VDmask = 1.0
else
  call Message%printError('EBSD4D','virtual detector type not yet implemented')
end if 

! get the lower left corner of the virtual detector
VDposx = nint(nml%VDlocx) - (nml%VDwidth-1)/2
VDposy = nint(nml%VDlocy) - (nml%VDheight-1)/2

! this next part is done with OpenMP, with only thread 0 doing the reading;
! Thread 0 reads one line worth of patterns from the input file, then all threads do
! the work; repeat until all patterns have been processed.
call OMP_setNThreads(nml%nthreads)
dims3 = (/ binx, biny, nml%ipf_wd /)

do iii = iiistart,iiiend

! start the OpenMP portion
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(TID, jj, kk, Pat, window)

! set the thread ID
   TID = OMP_GET_THREAD_NUM()

   allocate(Pat(binx,biny), window(nml%VDwidth, nml%VDheight))

! thread 0 reads the next row of patterns from the input file
! we have to allow for all the different types of input files here...
    if (TID.eq.0) then
        offset3 = (/ 0, 0, (iii-1)*nml%ipf_wd /)
        if (ROIselected.eqv..TRUE.) then
          if ( (itype.eq.4) .or. (itype.eq.6) .or. (itype.eq.7) .or. (itype.eq.8) ) then
            call VT%getExpPatternRow(iii, nml%ipf_wd, patsz, L, dims3, offset3, exppatarray, nml%ROI, &
                                     HDFstrings=nml%HDFstrings, HDF=HDF)
          else
            call VT%getExpPatternRow(iii, nml%ipf_wd, patsz, L, dims3, offset3, exppatarray, nml%ROI)
          end if
        else
         if ( (itype.eq.4) .or. (itype.eq.6) .or. (itype.eq.7) .or. (itype.eq.8) ) then
            call VT%getExpPatternRow(iii, nml%ipf_wd, patsz, L, dims3, offset3, exppatarray, &
                                     HDFstrings=nml%HDFstrings, HDF=HDF)
          else
            call VT%getExpPatternRow(iii, nml%ipf_wd, patsz, L, dims3, offset3, exppatarray)
          end if
        end if
    end if

! other threads must wait until T0 is ready
!$OMP BARRIER    

! then loop in parallel over all patterns to perform the preprocessing steps
!$OMP DO SCHEDULE(DYNAMIC)
    do jj=jjstart,jjend
! convert imageexpt to 2D EBSD Pattern array
        jjj = jj-jjstart+1
        do kk=1,biny
          Pat(1:binx,kk) = exppatarray((jjj-1)*patsz+(kk-1)*binx+1:(jjj-1)*patsz+kk*binx)
        end do
        window = Pat( VDposx:VDposx+nml%VDwidth-1, VDposy:VDposy+nml%VDheight-1 )
        VDimage(jj-jjstart+1,iii-iiistart+1) = sum( window*VDmask )
    end do
!$OMP END DO

!$OMP BARRIER
deallocate(Pat, window)
!$OMP END PARALLEL

end do


! output the ADP map as a tiff file
fname = trim(EMsoft%generateFilePath('EMdatapathname'))//trim(nml%virtualimagefile)
TIFF_filename = trim(fname)
if (ROIselected.eqv..TRUE.) then
  TIFF_nx = nml%ROI(3)
  TIFF_ny = nml%ROI(4)
else
  TIFF_nx = nml%ipf_wd
  TIFF_ny = nml%ipf_ht
end if

! allocate memory for image
allocate(TIFF_image(TIFF_nx,TIFF_ny))

! fill the image with whatever data you have (between 0 and 255)
ma = maxval(VDimage)
mi = minval(VDimage)

write (*,*) ' image intensity range ',mi,ma 

do j=1,TIFF_ny
 do i=1,TIFF_nx
  ! TIFF_image(i,j) = int(255 * (VDimage(i,TIFF_ny-j+1)-mi)/(ma-mi))
  TIFF_image(i,j) = int(255 * (VDimage(i,j)-mi)/(ma-mi))
 end do
end do

! set up the image_t structure
im = image_t(TIFF_image)
if(im%empty()) call Message%printMessage("EBSD4D_","failed to convert array to image")

! create the file
call im%write(trim(TIFF_filename), iostat, iomsg) ! format automatically detected from extension
if(0.ne.iostat) then
  call Message%printMessage("failed to write image to file : "//iomsg)
else
  call Message%printMessage('Virtual detector map written to '//trim(TIFF_filename))
end if

call closeFortranHDFInterface()

end associate

end subroutine EBSD4D_



end module mod_4DEBSD