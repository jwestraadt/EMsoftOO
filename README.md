# EMsoftOO (version 6.0 of EMsoft)

This repository contains the source code for version 6.0 of the EMsoft package; the main difference between EMsoft 5.0 (https://github.com/EMsoft-org/EMsoft) and the present repository is the fact that EMsoftOO is entirely written in Object Oriented fortran-2018 (hence the letters "OO" at the end of EMsoftOO).  This is a complete rewrite of the original package and is mostly backwards compatible with version 5.0 in terms of the data files that are generated by various programs.  Most of the changes are "under the hood" and not really visible to the ordinary user (except perhaps that most of the programs appear to execute more quickly than those in version 5.0).  

EMsoftOO Version 6.0 should be considered a beta version; the code is still very much under development, and not all of the version 5.0 programs have been ported yet. Nevertheless, most of the SEM-related code has been converted and tested.  We are in the process of updating the wiki pages. 

### **This release of EMsoftOO requires an updated Software Developer Toolkit (SDK); please go to [this page](https://github.com/EMsoft-org/EMsoftSuperbuild) and follow the instructions to install a new SDK before you attempt to build the present release.  As of 11/04/2022, you should use the DevelopOO branch of this respository!!**

## Financial Support 
EBSD/ECP/EKP development of this package, including dictionary indexing for EBSD/ECP, was started with support from an AFOSR/MURI grant, FA9550-12-1-0458; the original EBSD code from CTEMsoft 2.0 was developed with support from an ONR grant, N00014-12-1-0075.  All recent development of EMsoft was performed with support from an ONR Vannevar Bush Faculty Fellowship, N00014-­16-­1-­2821, and an NSF research program DMR \#1904629. Current development is carried out with support from NSF grant DMR-2203378.

## Current and Past Contributors
EMsoft started as a source code base used for the creation of all figures in the *Introduction to Conventional Transmission Electron Microscopy* text book (Cambridge University Press, 2003, ISBN 0521629950) by M. De Graef.  It has since grown into an open source project that has had many contributors and testers over the past nearly 20 years (in no particular order):

- Patrick Callahan
- Saransh Singh
- Stuart Wright
- Elena Pascal
- Will Lenthe
- Chaoyi Zhu
- Clément Lafond
- Joseph Tessmer
- Ke-Wei Jin
- Michael Atkinson
- Joao Fonseca
- Michael Jackson
- Joey Kleingers
- Håkon Wiik Ånes
- McLean Echlin
- Zachary Varley

## Installation 

If you want to build EMsoftOO yourself, it would make sense to first get a GitHub account, and fork this repository into your account. Then clone the repo *from your account* onto your local computer. Before you can compile things, you need to first build the Software Developer Kit (EMsoftOO_SDK), which you can find [here](https://github.com/EMsoft-org/EMsoftSuperbuild); follow the instructions for your platform, starting from the DevelopOO branch. In addition, you will need to clone the *EMsoftData* repository, also from *EMsoft-org*, in a folder at the same level as the main repository folder. 

Then, starting in the top folder where you have cloned the EMsoftOO repository, carry out the following commands (for UNIX-type builds; on Windows, use nmake instead of make):

```fortran
  mkdir EMsoftOOBuild
  cd EMsoftOOBuild
  mkdir Release
  cd Release
  cmake -DCMAKE_BUILD_TYPE=Release -DEMsoftOO_SDK=/somepath/EMsoftOO_SDK ../../EMsoftOO
  make -j
  cd ../
  mkdir Debug
  cd Debug
  cmake -DCMAKE_BUILD_TYPE=Debug -DEMsoftOO_SDK=/somepath/EMsoftOO_SDK ../../EMsoftOO
  make -j

```
Note that *somepath* should be replaced with wherever you installed the SDK.  These commands should compile both a Release and a Debug version of EMsoftOO. You can then add the path to the EMsoftOOBuild/Release/Bin folder to your shell path and start using the programs.  Note that the Debug version of the executables will run much more slowly than the Release version, but, if something goes wrong during the run, the error message of the Debug version will nearly always be more informative than for the Release version.

To always maintain an up-to-date version of the package, you may want to create a little script that will help you synchronize the repositories and compile in one step.  Here is an example shell script for UNIX-flavored systems; the assumptions are that the EMsoftOO repository has been cloned into the folder EMsoftOOPublic, and the EMsoftData repository into EMsoftData:

```fortran
cd EMsoftData
git pull --rebase origin develop
cd ../EMsoftOOPublic
git pull --rebase origin develop
cd ../EMsoftOOBuild
make -j

```

If you do not need the complete EMsoftOO package, you can compile sections of the package (e.g., SEM modalities only) by setting CMake switches using the ccmake GUI program, as described in the ccmake-options.md file. 

## Licenses ##

The BSD-3 license text below can be found at the start of every source file in the EMsoftOO package.

	!###################################################################
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

