## R CMD check results 

0 errors | 0 warnings | 4 note



Build ID:	tnl.Test_0.1.0.tar.gz-5eb33ab2117142fa8ba158c60d0f22a6
Platform:	Windows Server 2022, R-devel, 64 bit
Submitted:	9 minutes 35.4 seconds ago
Build time:	9 minutes 34.2 seconds
NOTES:
* checking CRAN incoming feasibility ... [11s] NOTE
Maintainer: 'Ihab Abusaif <censtat@gmail.com>'

New submission
* checking HTML version of manual ... NOTE
Found the following HTML validation problems:
tnl.test.html:32:20 (tnl.test.Rd:124): Warning: unescaped & or unknown entity "&#8467"
tnl.test.html:28:20 (tnl.test.Rd:120): Warning: unescaped & or unknown entity "&#8467"
tnl.test.html:36:20 (tnl.test.Rd:128): Warning: unescaped & or unknown entity "&#8467"
tnl.test.html:43:22 (tnl.test.Rd:136): Warning: unescaped & or unknown entity "&#8467"
tnl.test.html:40:20 (tnl.test.Rd:132): Warning: unescaped & or unknown entity "&#8467"
tnl.test.html:46:20 (tnl.test.Rd:139): Warning: unescaped & or unknown entity "&#8467"
tnl.test.html:50:20 (tnl.test.Rd:143): Warning: unescaped & or unknown entity "&#8467"
tnl.test.html:54:20 (tnl.test.Rd:147): Warning: unescaped & or unknown entity "&#8467"
tnl.test.html:58:20 (tnl.test.Rd:151): Warning: unescaped & or unknown entity "&#8467"
tnl.test.html:101:20 (tnl.test.Rd:43): Warning: unescaped & or unknown entity "&#8467"
tnl.test.html:148:17 (tnl.test.Rd:160): Warning: unescaped & or unknown entity "&#8800"
tnl.test.html:151:20 (tnl.test.Rd:163): Warning: unescaped & or unknown entity "&#8467"
tnl.test.html:157:20 (tnl.test.Rd:169): Warning: unescaped & or unknown entity "&#8467"
tnl.test.html:169:20 (tnl.test.Rd:181): Warning: unescaped & or unknown entity "&#8467"
tnl.test.html:174:20 (tnl.test.Rd:186): Warning: unescaped & or unknown entity "&#8467"
tnl.test.html:177:20 (tnl.test.Rd:189): Warning: unescaped & or unknown entity "&#8467"
tnl.test.html:203:20 (tnl.test.Rd:77): Warning: unescaped & or unknown entity "&#8467"
tnl.test.html:216:20 (tnl.test.Rd:86): Warning: unescaped & or unknown entity "&#8467"
tnl.test.html:236:20 (tnl.test.Rd:101): Warning: unescaped & or unknown entity "&#8467"
Skipping checking math rendering: package 'V8' unavailable
* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ''NULL''
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
  
  
## R CMD check results (second)  
Build ID:	tnl.Test_0.1.0.tar.gz-5c63f1dcbfe64386a38245dd926e6cc2
Platform:	Fedora Linux, R-devel, clang, gfortran
Submitted:	24 minutes 8.1 seconds ago
Build time:	24 minutes 2.3 seconds
NOTES:
* checking CRAN incoming feasibility ... [7s/22s] NOTE
Maintainer: ‘Ihab Abusaif <censtat@gmail.com>’

New submission
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
Skipping checking math rendering: package 'V8' unavailable




## R CMD check results (third)   
Build ID:	tnl.Test_0.1.0.tar.gz-1a6e4547f1b341d4b561ba8fb248485b
Platform:	Ubuntu Linux 20.04.1 LTS, R-release, GCC
Submitted:	24 minutes 30 seconds ago
Build time:	24 minutes 26.7 seconds
NOTES:
* checking CRAN incoming feasibility ... [6s/16s] NOTE
Maintainer: ‘Ihab Abusaif <censtat@gmail.com>’

New submission
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
Skipping checking math rendering: package 'V8' unavailable



I encountered this even though I installed the latest version of the "tidy" file. Despite these notes, my package works without any problems.
