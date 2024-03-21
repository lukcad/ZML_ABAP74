# ABAP 7.40 examples

## Overview

Examples of programs to demonstrate advantages of ABAP 7.40 against previous versions.

## How to use

* Grab code:

  Use program zabapgit_standalone program by SE38 into your on-premise SAP instance.

  create package ZML_ABAP74 and use this git url: https://github.com/lukcad/ZML_ABAP74.git

  pull all from git to your newly created package ZML_ABAP74

* Run and analyse code:

  using Eclipse ADT you can open package and find programs that you can run by F8.

  Running each progam you should close each view window of demo step which shows results.

  Use comments and debugger to check specific of each demo example piece of code.

## Troubleshgooting connection to gitHub:

If you have SSL error, you should import by STRUST transaction all necessary 3 certificates for Anonymous client:

* github.com
* Sectigo ECC Domain Validation Secure Server CA
* USERTrust ECC Certification Authority.crt

Notice how you can get certificates of github:

	1. Using Google Chrome to go to https://github.com
	2. Click on the lock icon near the address bar, then click on "Connection is secure"
	3. On the Security tab, click on "Certificate is valid"
	4. Go to the "Details" tab and select "Export..." to download the certificate for each certification hierarhy ( it should be at least 3 hierarhy nodes ).


